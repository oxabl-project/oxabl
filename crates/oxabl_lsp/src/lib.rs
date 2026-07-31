//! `oxabl lsp` — a stdio LSP server that surfaces oxabl's parse + lint +
//! loud-preprocessor diagnostics live in an editor (Track A).
//!
//! v1 is a diagnostics-and-formatting server: it completes the LSP handshake,
//! syncs open buffers incrementally, pushes `publishDiagnostics`, and answers
//! `textDocument/formatting`. It advertises nothing else (R2).
//!
//! **This crate orchestrates nothing.** Both the analysis and the formatting are
//! [`oxabl_pipeline`]'s, driven through handles this server holds: the lint
//! pipeline's two phases run inside the salsa queries in [`db`], the format
//! pipeline runs inline in [`formatting`], and one [`PipelineConfig`] resolved
//! from a single read of `oxabl.toml` feeds both. What is left here is genuinely
//! server plumbing: *when* to run, *which* buffers to invalidate, and how to
//! render a byte span as a `Range` under the negotiated encoding.
//!
//! Threading discipline (KTD7): input mutations run on the main loop; each
//! debounced diagnostics computation runs on a cloned salsa snapshot on a
//! worker thread. See [`db`] and [`debounce`].

pub mod capabilities;
pub mod db;
pub mod debounce;
pub mod diagnostics;
pub mod document;
pub mod formatting;
pub mod position;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument, DidOpenTextDocument,
    DidSaveTextDocument, Notification as _, PublishDiagnostics,
};
use lsp_types::request::{Formatting, Request as _};
use lsp_types::{
    DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    InitializeParams, InitializeResult, PositionEncodingKind, PublishDiagnosticsParams, ServerInfo,
    TextEdit, Uri,
};
use oxabl_common::catch_panic;
use oxabl_pipeline::{ConfigOverrides, ConfigWarning, FormatPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;
use salsa::Setter;

use crate::capabilities::{negotiate_position_encoding, server_capabilities};
use crate::db::{
    AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, buffer_dependencies,
    compute_diagnostics,
};
use crate::diagnostics::to_lsp_diagnostics;
use crate::document::DocumentStore;

/// Entry point invoked by the `oxabl lsp` subcommand. Owns the stdio
/// connection, runs the handshake and main loop, and joins the I/O threads.
///
/// Returns `Err` when the client sent `exit` without a prior `shutdown` (the
/// LSP-mandated non-zero exit), so the binary can map it to a failing
/// [`ExitCode`](std::process::ExitCode).
pub fn run() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    let clean = serve(&connection)?;
    io_threads.join().context("joining LSP I/O threads")?;
    if clean {
        Ok(())
    } else {
        anyhow::bail!("`exit` received without a prior `shutdown`");
    }
}

/// Run the LSP handshake and main loop over an arbitrary connection (stdio in
/// production, an in-memory pair in tests). Returns `true` for a clean
/// shutdown (a `shutdown` request preceded `exit`), `false` otherwise.
pub fn serve(connection: &Connection) -> Result<bool> {
    serve_with(connection, crate::debounce::DEFAULT_WINDOW)
}

/// [`serve`] with an explicit debounce window (used by tests to run fast).
pub fn serve_with(connection: &Connection, debounce_window: std::time::Duration) -> Result<bool> {
    let (encoding, workspace_root) = handshake(connection)?;
    let mut server = Server::new(connection, encoding, debounce_window, workspace_root);
    server.main_loop()
}

/// Perform the initialize handshake, negotiating the position encoding from the
/// client's advertised `general.positionEncodings` before replying with the v1
/// server capabilities (R2, R3).
///
/// Also returns the client's declared **workspace root**, when it declared one.
/// That is what lets the server's watch and its configuration cover a *project*
/// rather than whichever directory happened to hold the first opened document.
fn handshake(connection: &Connection) -> Result<(PositionEncodingKind, Option<PathBuf>)> {
    let (id, params) = connection
        .initialize_start()
        .context("LSP initialize_start")?;
    let init: InitializeParams =
        serde_json::from_value(params).context("deserializing InitializeParams")?;

    let encoding = negotiate_position_encoding(&init.capabilities);
    let workspace_root = declared_workspace_root(&init);
    let result = InitializeResult {
        capabilities: server_capabilities(encoding.clone()),
        server_info: Some(ServerInfo {
            name: "oxabl-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    connection
        .initialize_finish(id, serde_json::to_value(result)?)
        .context("LSP initialize_finish")?;
    Ok((encoding, workspace_root))
}

/// The workspace root the client declared, if any.
///
/// `workspaceFolders` first, because it is the live field and the only one that
/// can carry more than one root; `rootUri` second, because plenty of clients still
/// send only that. Both are best-effort: a client that declares neither, or
/// declares a non-`file:` URI, leaves the server on its previous behavior of
/// anchoring to the first opened document.
///
/// Only the **first** folder is taken. A multi-root session gets one anchor, which
/// is the same single `oxabl.toml` resolution the server has always performed;
/// resolving per folder would mean more than one configuration alive at once, and
/// nothing in the server can hold two yet.
fn declared_workspace_root(init: &InitializeParams) -> Option<PathBuf> {
    #[allow(deprecated)] // `root_uri` is how a large share of clients still speak.
    init.workspace_folders
        .as_ref()
        .and_then(|folders| folders.first().map(|folder| folder.uri.clone()))
        .or_else(|| init.root_uri.clone())
        .as_ref()
        .and_then(uri_to_path)
}

/// The running LSP server: negotiated session state, the open-buffer store, the
/// coarse salsa substrate, per-URI debounce timers, and the watcher's schema /
/// include dependency bookkeeping.
struct Server<'c> {
    connection: &'c Connection,
    /// Negotiated position encoding, applied at every span→Range mapping (KTD4).
    encoding: PositionEncodingKind,
    /// Open buffers, keyed by URI (R4).
    documents: DocumentStore,
    /// Coarse salsa substrate (write-on-main).
    db: AnalysisDatabase,
    /// Per-URI salsa input handles for open buffers.
    buffers: HashMap<Uri, Buffer>,
    /// Schema revision handle (bumped on `.df` change, R16).
    schema: SchemaHandle,
    /// Whether the db configuration (include paths, schema) has been resolved
    /// from a workspace yet. Resolved lazily from the first opened document.
    config_resolved: bool,
    /// The path the workspace configuration was resolved from — the declared
    /// workspace root when the client sent one, else the first opened document —
    /// used to re-resolve on `oxabl.toml` / `.df` changes (R16/R17).
    workspace_anchor: Option<PathBuf>,
    /// The root the client declared at `initialize`, if any. Preferred over the
    /// first-opened-document anchor, so configuration and the file watch describe
    /// the project rather than one directory inside it.
    workspace_root: Option<PathBuf>,
    /// The format pipeline, rebuilt whenever configuration is resolved so a
    /// formatting request costs no style resolution and no `StyleGuide` clone.
    formatter: FormatPipeline,
    /// Per-URI include dependency paths (for watcher `*.i` matching, R17).
    dependencies: HashMap<Uri, Vec<PathBuf>>,
    /// Per-URI debounce timers (R13).
    debouncer: crate::debounce::Debouncer,
    /// Bumped every time a configuration is installed. Workers carry the
    /// generation they read so a result computed under a configuration the user
    /// has since replaced is not published (see [`Server::handle_result`]).
    config_generation: u64,
}

/// A completed background diagnostics computation, tagged with the buffer
/// version it read so a superseded result can be discarded (KTD7).
struct ComputeResult {
    uri: Uri,
    version: i32,
    diagnostics: Option<oxabl_analyze::CollectedDiagnostics>,
    /// Include dependency paths observed during this computation (R17), or
    /// `None` when the computation produced no trustworthy set (cancelled, or a
    /// contained panic) — see [`analyze_guarded`].
    dependencies: Option<Vec<PathBuf>>,
    /// Whether the absent halves above come from a **contained panic** rather
    /// than a cancellation. The two are otherwise indistinguishable at the
    /// receiving end, and they want opposite handling: a cancellation is a race
    /// worth retrying, a panic is deterministic in the buffer's text and
    /// retrying it would spin forever (see [`Server::handle_result`]).
    panicked: bool,
    /// The [`Server::config_generation`] the worker read. A result whose
    /// generation has since moved was computed under a configuration the user has
    /// replaced, and publishing it would show one round of diagnostics under the
    /// old severities.
    config_generation: u64,
}

impl<'c> Server<'c> {
    fn new(
        connection: &'c Connection,
        encoding: PositionEncodingKind,
        debounce_window: std::time::Duration,
        workspace_root: Option<PathBuf>,
    ) -> Self {
        let config = AnalysisConfig::default();
        let formatter = FormatPipeline::new(config.pipeline.style.clone());
        let db = AnalysisDatabase::new(config);
        let schema = SchemaHandle::new(&db, 0);
        Server {
            connection,
            encoding,
            documents: DocumentStore::new(),
            db,
            buffers: HashMap::new(),
            schema,
            config_resolved: false,
            workspace_anchor: None,
            workspace_root,
            formatter,
            dependencies: HashMap::new(),
            debouncer: crate::debounce::Debouncer::new(debounce_window),
            config_generation: 0,
        }
    }

    /// Dispatch loop. Multiplexes the client connection, completed background
    /// computations, and debounce-timer expirations via `select!`. Returns
    /// whether shutdown was clean.
    fn main_loop(&mut self) -> Result<bool> {
        use crossbeam_channel::{after, never, select};

        // A declared root is enough to resolve configuration and start watching
        // *before* any document is opened — which is what makes the watch cover a
        // project. Without one, both still happen, on the first `didOpen`.
        self.ensure_config_from_root();

        // Clone the connection receiver so the `select!` doesn't borrow `self`
        // (the arms need `&mut self`).
        let conn_rx = self.connection.receiver.clone();
        let (result_tx, result_rx) = crossbeam_channel::unbounded::<ComputeResult>();

        let mut shutdown_received = false;
        loop {
            // Size the timeout to the nearest pending debounce deadline.
            let timer = match self.debouncer.next_deadline() {
                Some(deadline) => {
                    after(deadline.saturating_duration_since(std::time::Instant::now()))
                }
                None => never::<std::time::Instant>(),
            };

            select! {
                recv(conn_rx) -> msg => {
                    let Ok(msg) = msg else { break; };
                    match msg {
                        Message::Request(req) => {
                            if req.method == "shutdown" {
                                shutdown_received = true;
                                self.respond(Response::new_ok(req.id, serde_json::Value::Null))?;
                            } else if req.method == Formatting::METHOD {
                                self.handle_formatting(req)?;
                            } else {
                                self.respond(Response::new_err(
                                    req.id,
                                    ErrorCode::MethodNotFound as i32,
                                    format!("oxabl-lsp: unsupported request `{}`", req.method),
                                ))?;
                            }
                        }
                        Message::Notification(not) => {
                            if not.method == "exit" {
                                return Ok(shutdown_received);
                            }
                            self.handle_notification(not);
                        }
                        Message::Response(_) => {}
                    }
                }
                recv(result_rx) -> res => {
                    if let Ok(res) = res {
                        self.handle_result(res);
                    }
                }
                recv(timer) -> _ => {
                    self.fire_due_debounces(&result_tx);
                }
            }
        }
        Ok(shutdown_received)
    }

    /// Spawn a worker per due buffer: clone a snapshot on the main thread, then
    /// compute diagnostics off-thread. The result is tagged with the buffer
    /// version it read so a superseded computation is dropped on completion.
    fn fire_due_debounces(&mut self, result_tx: &crossbeam_channel::Sender<ComputeResult>) {
        let now = std::time::Instant::now();
        for uri in self.debouncer.take_due(now) {
            let (Some(&buffer), Some(doc)) = (self.buffers.get(&uri), self.documents.get(&uri))
            else {
                continue;
            };
            let version = doc.version;
            let snapshot = self.db.clone();
            let schema = self.schema;
            let config_generation = self.config_generation;
            let tx = result_tx.clone();
            std::thread::spawn(move || {
                // The guard is inside `analyze_guarded`, so the send below is
                // reached on every path — a panic must not leave this buffer
                // waiting forever on a result that never arrives (R8).
                let analysis = analyze_guarded(&snapshot, buffer, schema, uri.as_str());
                let _ = tx.send(ComputeResult {
                    uri,
                    version,
                    diagnostics: analysis.diagnostics,
                    dependencies: analysis.dependencies,
                    panicked: analysis.panicked,
                    config_generation,
                });
            });
        }
    }

    /// Publish a completed computation, unless it was cancelled (`None`) or its
    /// buffer version has since been superseded by a newer edit (KTD7).
    ///
    /// A **cancelled** result for a buffer that is still open at the version the
    /// worker read is the one case that must not simply be dropped.
    /// [`salsa::Cancelled`] is global — a write to *any* buffer's input flags
    /// every live snapshot — so editing file A cancels file B's in-flight
    /// computation even though B never changed. B's timer was consumed when its
    /// worker was spawned and B's own version is unchanged, so nothing would ever
    /// re-fire it: B would keep displaying pre-edit diagnostics until the user
    /// touched it again. Re-arming the debounce here is what makes cancellation
    /// the optimization it is documented to be rather than lost work.
    ///
    /// A *contained panic* arrives in the same shape (absent diagnostics) and
    /// must **not** be retried: it is deterministic in the buffer's text, so a
    /// reschedule would spin the file at the debounce interval forever.
    ///
    /// A result computed under a **superseded configuration** is dropped the same
    /// way and retried: a worker that finished microseconds before an
    /// `oxabl.toml` change would otherwise publish one round of diagnostics under
    /// the severities the user just replaced. The buffer version cannot catch this
    /// — the text did not change, only the configuration did. The
    /// never-retry-a-panic rule applies to that reschedule too, since the
    /// generation gate is reached before the absent-diagnostics one.
    fn handle_result(&mut self, res: ComputeResult) {
        match self.documents.get(&res.uri) {
            None => return,                                    // buffer closed
            Some(doc) if doc.version != res.version => return, // superseded by a newer edit
            Some(_) => {}
        }
        if res.config_generation != self.config_generation {
            // Computed under a configuration that is no longer current. The
            // watcher path already re-triggers every buffer, but this arrives on
            // the `ensure_config` path too (a first anchor resolved while an
            // earlier buffer's worker was in flight), where nothing else would.
            //
            // A panic is never rescheduled, here as below: this gate runs first, so
            // without the check a contained panic that happened to land under a
            // superseded generation would get the one retry `panicked` exists to
            // prevent.
            if !res.panicked {
                self.debouncer.schedule(res.uri, std::time::Instant::now());
            }
            return;
        }
        self.record_dependencies(&res.uri, res.dependencies);
        let Some(collected) = res.diagnostics else {
            // Cancelled snapshot read: recompute, or this buffer goes stale.
            if !res.panicked {
                self.debouncer.schedule(res.uri, std::time::Instant::now());
            }
            return;
        };
        let Some(doc) = self.documents.get(&res.uri) else {
            return;
        };
        let lsp_diags = to_lsp_diagnostics(&collected, &doc.rope, &self.encoding);
        self.publish(res.uri.clone(), lsp_diags, Some(res.version));
    }

    /// Dispatch a `textDocument/*` notification. Opens publish immediately (for
    /// instant feedback); edits mirror into the salsa input on the main thread
    /// and schedule a debounced recompute (R13). Unknown notifications are
    /// ignored (per LSP, servers must not error on them).
    fn handle_notification(&mut self, not: Notification) {
        match not.method.as_str() {
            DidOpenTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidOpenTextDocumentParams>(not.params)
                {
                    let doc = params.text_document;
                    self.ensure_config(&doc.uri);
                    self.documents.open(doc.uri.clone(), doc.version, &doc.text);
                    // The path travels with the buffer because the *query* needs
                    // it: cross-file resolution must exclude this file from its
                    // own lookups, since the buffer is the unsaved text while
                    // the index answers from disk.
                    let path = uri_to_path(&doc.uri);
                    let buffer = Buffer::new(&self.db, doc.text, path);
                    self.buffers.insert(doc.uri.clone(), buffer);
                    // Open shows diagnostics right away (no debounce).
                    self.compute_and_publish(&doc.uri);
                }
            }
            DidChangeTextDocument::METHOD => {
                if let Ok(params) =
                    serde_json::from_value::<DidChangeTextDocumentParams>(not.params)
                {
                    let uri = params.text_document.uri;
                    let encoding = self.encoding.clone();
                    self.documents.change(
                        &uri,
                        params.text_document.version,
                        &params.content_changes,
                        &encoding,
                    );
                    // Mirror the new text into the salsa input (write on main).
                    // This cancels any in-flight snapshot read for a superseded
                    // version via salsa's `Cancelled` unwind (KTD7).
                    if let (Some(&buffer), Some(doc)) =
                        (self.buffers.get(&uri), self.documents.get(&uri))
                    {
                        buffer.set_text(&mut self.db).to(doc.text());
                    }
                    // Collapse the edit burst to a single debounced recompute.
                    self.debouncer.schedule(uri, std::time::Instant::now());
                }
            }
            DidSaveTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidSaveTextDocumentParams>(not.params)
                {
                    // Recompute on save so on-disk include changes are picked up.
                    self.debouncer
                        .schedule(params.text_document.uri, std::time::Instant::now());
                }
            }
            DidChangeWatchedFiles::METHOD => {
                if let Ok(params) =
                    serde_json::from_value::<DidChangeWatchedFilesParams>(not.params)
                {
                    self.handle_watched_files(params);
                }
            }
            DidCloseTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidCloseTextDocumentParams>(not.params)
                {
                    let uri = params.text_document.uri;
                    self.documents.close(&uri);
                    self.buffers.remove(&uri);
                    self.dependencies.remove(&uri);
                    self.debouncer.cancel(&uri);
                    // Clear the client's diagnostics for the closed buffer.
                    self.publish(uri, Vec::new(), None);
                }
            }
            _ => {}
        }
    }

    /// Resolve configuration from the first opened document **that has a
    /// filesystem path** (once), establishing include paths, the `[lint]`
    /// severity surface, the style, and the schema so include-resident symbols
    /// resolve (R9) and schema-gated rules go live (R10).
    ///
    /// One resolution feeds every surface: diagnostics *and* formatting read the
    /// same [`PipelineConfig`], so the server no longer has a second config path
    /// that could disagree with the first (KTD3).
    ///
    /// **The one-shot is spent only when an anchor is actually derived.** A
    /// document with no path — a scratch `untitled:` buffer, a VS Code `git:`
    /// diff view — is not a workspace, and marking the resolution done for one
    /// used to leave the whole session on defaults: no severities, no include
    /// paths, and no configured formatter style, with nothing able to recover it
    /// (`handle_watched_files` needs the anchor this would never set). Leaving the
    /// flag unset costs one `uri_to_path` per subsequent open until a real file
    /// arrives, and that file then anchors the session normally.
    fn ensure_config(&mut self, uri: &Uri) {
        if self.config_resolved {
            return;
        }
        let Some(path) = uri_to_path(uri) else {
            return;
        };
        self.config_resolved = true;
        // The declared root wins over the document's own directory: an
        // `oxabl.toml` at the project root governs a file opened three levels down,
        // and anchoring at the file would still find it by walking up — but the
        // *root* is also what a client's watch registration is relative to, so the
        // two must agree on what the workspace is.
        let anchor = self.workspace_root.clone().unwrap_or(path);
        self.workspace_anchor = Some(anchor.clone());
        // Installs both halves of the resolution: the db config the queries read
        // and the rebuilt format pipeline (`install_config`).
        self.resolve_config_from(&anchor);
        self.register_file_watchers();
    }

    /// Resolve from the client's declared workspace root, if it declared one.
    ///
    /// Runs before the first document arrives, which matters for more than
    /// tidiness: the cross-file index searches the *resolved* include paths, so a
    /// buffer opened before configuration existed would resolve nothing until
    /// something re-triggered it.
    fn ensure_config_from_root(&mut self) {
        if self.config_resolved {
            return;
        }
        let Some(root) = self.workspace_root.clone() else {
            return;
        };
        self.config_resolved = true;
        self.workspace_anchor = Some(root.clone());
        self.resolve_config_from(&root);
        self.register_file_watchers();
    }

    /// Best-effort dynamic registration of file watchers for `*.i`, `*.df`,
    /// `oxabl.toml`, and **every root source extension**, so a real editor forwards
    /// their changes as `workspace/didChangeWatchedFiles` (R16/R17, and R10 for the
    /// last group). Clients without dynamic registration simply ignore it; the
    /// server needs no response.
    ///
    /// The root extensions are the reason a dependency edit on disk reaches the
    /// buffers that resolved against it: a `.cls` saved in another editor, or
    /// changed by a branch switch, is not a `didChange` on any open document, so
    /// without this glob the server would never hear about it.
    ///
    /// The list comes from [`oxabl_workspace::ROOT_EXTENSIONS`] rather than being
    /// spelled out here, so the watch cannot drift from the one root-file policy
    /// the discovery walk and the index's own search both use.
    fn register_file_watchers(&self) {
        let mut watchers = vec![
            serde_json::json!({ "globPattern": "**/*.i" }),
            serde_json::json!({ "globPattern": "**/*.df" }),
            serde_json::json!({ "globPattern": "**/oxabl.toml" }),
        ];
        watchers.extend(
            oxabl_workspace::ROOT_EXTENSIONS
                .iter()
                .map(|ext| serde_json::json!({ "globPattern": format!("**/*.{ext}") })),
        );
        let params = serde_json::json!({
            "registrations": [{
                "id": "oxabl-file-watchers",
                "method": "workspace/didChangeWatchedFiles",
                "registerOptions": {
                    "watchers": watchers
                }
            }]
        });
        let request = Request {
            id: RequestId::from("oxabl-file-watchers".to_string()),
            method: "client/registerCapability".to_string(),
            params,
        };
        let _ = self.connection.sender.send(Message::Request(request));
    }

    /// Resolve the one [`PipelineConfig`] for a document `path` and install it in
    /// both places it is read: the db (for the two query phases) and the format
    /// pipeline (for `textDocument/formatting`).
    ///
    /// The server has no CLI flags, so there are no overrides — but the whole
    /// resolution still goes through the shared entry point, which is what makes
    /// "the editor and `oxabl check` agree" a property of one code path rather
    /// than of two implementations kept in step by hand (KTD3).
    ///
    /// Non-fatal problems come back as data and are logged to the client (R7).
    /// Before this, the server called three `resolved_*` helpers and dropped
    /// every error slot on the floor, so a malformed `oxabl.toml` degraded the
    /// editor's diagnostics *silently* while the CLI printed a `warning:` line
    /// for the same file.
    fn resolve_config_from(&mut self, path: &Path) {
        let (resolved, warnings) = PipelineConfig::resolve(path, &ConfigOverrides::default());
        self.report_config_warnings(&warnings);
        self.install_config(resolved);
    }

    /// Install a freshly resolved configuration in the db and rebuild the format
    /// pipeline from its style. Callers must re-trigger affected buffers
    /// afterwards (the db configuration is not a salsa input, R17).
    ///
    /// **The cross-file index handle is carried forward, not rebuilt.** It is the
    /// only route back to the per-file salsa inputs, which live in the database
    /// and outlive any one configuration; minting a fresh registry here would
    /// orphan every one of them, so a later disk change would find nothing to bump
    /// and every dependent buffer would keep a stale answer indefinitely. What a
    /// re-resolution legitimately changes is the *search paths*, and those are read
    /// per lookup rather than memoized — see `db::SnapshotIndex`.
    fn install_config(&mut self, resolved: PipelineConfig) {
        // Every install moves the generation, so any worker still running against
        // the previous configuration is identifiable when its result lands.
        self.config_generation = self.config_generation.wrapping_add(1);
        self.formatter = FormatPipeline::new(resolved.style.clone());
        let index = std::sync::Arc::clone(&self.db.config().index);
        self.db.set_config(AnalysisConfig {
            fs: std::sync::Arc::new(RealFileSystem),
            pipeline: std::sync::Arc::new(resolved),
            preprocess: true,
            index,
        });
    }

    /// Surface configuration warnings on the client's log (R7).
    ///
    /// `window/logMessage` rather than a published diagnostic: these problems
    /// belong to `oxabl.toml` or a `.df`, not to the open buffer, so attaching
    /// them to a span in the ABL file the user happens to have focused would put
    /// a squiggle on innocent code.
    fn report_config_warnings(&self, warnings: &[ConfigWarning]) {
        for warning in warnings {
            self.log_warning(&format!("oxabl-lsp: {warning}"));
        }
    }

    /// Send a `window/logMessage` at warning level.
    fn log_warning(&self, message: &str) {
        let notification = lsp_server::Notification {
            method: lsp_types::notification::LogMessage::METHOD.to_string(),
            params: serde_json::json!({
                "type": lsp_types::MessageType::WARNING,
                "message": message,
            }),
        };
        let _ = self
            .connection
            .sender
            .send(Message::Notification(notification));
    }

    /// Compute diagnostics for `uri` on a salsa snapshot and publish them
    /// (the immediate open path). The snapshot is dropped before returning so
    /// a subsequent write never blocks.
    fn compute_and_publish(&mut self, uri: &Uri) {
        let (Some(&buffer), Some(version)) = (
            self.buffers.get(uri),
            self.documents.get(uri).map(|d| d.version),
        ) else {
            return;
        };
        let snapshot = self.db.clone();
        // Guarded (R8): this runs on the main loop, so an unguarded panic here
        // takes the whole server down.
        let analysis = analyze_guarded(&snapshot, buffer, self.schema, uri.as_str());
        self.record_dependencies(uri, analysis.dependencies);
        let Some(collected) = analysis.diagnostics else {
            // A cancellation is not expected on this path (nothing writes to the
            // db while the main loop is inside this call), but if one ever does
            // arrive the open would otherwise publish nothing at all and no timer
            // would remain — the same stale-forever shape as in `handle_result`.
            if !analysis.panicked {
                self.debouncer
                    .schedule(uri.clone(), std::time::Instant::now());
            }
            return;
        };
        if let Some(doc) = self.documents.get(uri) {
            let lsp_diags = to_lsp_diagnostics(&collected, &doc.rope, &self.encoding);
            self.publish(uri.clone(), lsp_diags, Some(version));
        }
    }

    /// React to `workspace/didChangeWatchedFiles`: `.df` changes hot-reload the
    /// schema (R16); `oxabl.toml` changes re-resolve include paths + `[lint]`
    /// config (R17); `*.i` changes re-trigger the buffers that depend on them
    /// (R17); a **root source file** change invalidates that one file's index
    /// input (R10). Re-triggering bumps the buffer's salsa input so the coarse
    /// expansion memo recomputes and re-reads the changed file.
    fn handle_watched_files(&mut self, params: DidChangeWatchedFilesParams) {
        let now = std::time::Instant::now();
        let mut schema_changed = false;
        let mut config_changed = false;
        let mut changed_includes: Vec<PathBuf> = Vec::new();
        let mut changed_sources: Vec<PathBuf> = Vec::new();

        for event in &params.changes {
            let Some(path) = uri_to_path(&event.uri) else {
                continue;
            };
            let ext = path.extension().and_then(|e| e.to_str());
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if ext == Some("df") {
                schema_changed = true;
            } else if name == "oxabl.toml" {
                config_changed = true;
            } else if ext == Some("i") {
                changed_includes.push(path);
            } else if oxabl_workspace::is_root_file(&path) {
                // The four pre-existing branches are unchanged and still match
                // first: `.i` is never a root file, and neither is a `.df` or a
                // `.toml`, so nothing that used to dispatch one way now dispatches
                // the other.
                changed_sources.push(path);
            }
        }

        // Both events re-resolve the one configuration from the anchor, because
        // there is only one resolution to re-run: a `.df` change reloads the
        // schema by re-reading the config that names it (R16), and an
        // `oxabl.toml` change re-reads include paths, lint severities, style, and
        // the schema together (R17). Re-resolving on a `.df` change costs one
        // extra `oxabl.toml` read on a save-rate event, and buys the server a
        // single config path instead of a second schema-only loader.
        if config_changed || schema_changed {
            if let Some(anchor) = self.workspace_anchor.clone() {
                self.resolve_config_from(&anchor);
            }
            // Bump the config revision so the diagnostics query recomputes with
            // the new configuration (it is db state, not a salsa input, so it
            // would not otherwise invalidate — and re-triggering the buffer alone
            // backdates through the unchanged expansion).
            self.bump_config_revision();
        }

        if config_changed {
            // The re-trigger (set_text) additionally re-reads includes for a
            // changed PROPATH.
            self.retrigger_all(now);
        } else if schema_changed {
            // A changed schema cannot change any expansion, so re-reading
            // includes would be waste: the revision bump already invalidated the
            // collect phase for every buffer (R16). Just schedule the republish.
            for uri in self.open_uris() {
                self.debouncer.schedule(uri, now);
            }
        }

        // Idle include changes: re-trigger only buffers that depend on them.
        if !config_changed {
            for inc in &changed_includes {
                for uri in self.buffers_depending_on(inc) {
                    self.retrigger(&uri, now);
                }
            }
        }

        self.invalidate_indexed_sources(&changed_sources, now);
    }

    /// Invalidate the index inputs for changed workspace source files, and
    /// schedule the open buffers to republish (R10).
    ///
    /// # Why this is a bump and not a re-trigger
    ///
    /// An `.i` change has to force the *expansion* to re-read the file, which
    /// salsa cannot know about — hence `retrigger`'s `set_text` and the server's
    /// hand-rolled include→buffer map. A workspace source file is different: it is
    /// reached through a per-file salsa input, so bumping that one input
    /// invalidates precisely the queries that read it and, transitively, exactly
    /// the buffers that consulted it. Re-triggering here would be strictly worse —
    /// `set_text` invalidates a buffer's whole expansion, including for buffers
    /// that never looked at the changed file.
    ///
    /// # Why every open buffer is scheduled, not just the dependents
    ///
    /// Salsa's dependency graph is already the reverse index, and it is an *exact*
    /// one; a second, server-side map of cross-file edges could only ever be an
    /// approximation of it, and the failure mode of an approximation here is the
    /// silent one — a missed edge means a buffer quietly serving stale
    /// diagnostics. So the scheduling is deliberately coarse and the *answer* is
    /// precise: a buffer that consulted the changed file re-executes, and a buffer
    /// that did not has its memo validated and republishes an identical set. The
    /// early-out below keeps even that off the table for the overwhelmingly common
    /// case — a file no lookup has ever reached cannot be anyone's dependency, so
    /// nothing is scheduled at all.
    fn invalidate_indexed_sources(&mut self, changed: &[PathBuf], now: std::time::Instant) {
        let index = std::sync::Arc::clone(&self.db.config().index);
        let mut any = false;
        for path in changed {
            // Both spellings, because a watcher's URI and the path the index
            // search resolved need not agree on symlinks or `.` components — the
            // same reason `buffers_depending_on` canonicalizes.
            any |= index.bump(&mut self.db, path);
            if let Ok(canonical) = std::fs::canonicalize(path)
                && canonical != *path
            {
                any |= index.bump(&mut self.db, &canonical);
            }
        }
        if !any {
            return;
        }
        for uri in self.open_uris() {
            self.debouncer.schedule(uri, now);
        }
    }

    /// Bump the shared config/schema revision input so the diagnostics query is
    /// invalidated and recomputes with the current (untracked) db config.
    fn bump_config_revision(&mut self) {
        let next = self.schema.revision(&self.db).wrapping_add(1);
        self.schema.set_revision(&mut self.db).to(next);
    }

    /// Record a buffer's include dependency set, **keeping the previous one when
    /// the computation had no trustworthy answer** (R17).
    ///
    /// `None` arrives from a cancelled snapshot read or a contained panic, and
    /// neither says anything about what the buffer includes. Overwriting with an
    /// empty set would be worse than doing nothing: `buffers_depending_on` would
    /// stop matching this buffer, so editing an `.i` it includes would never
    /// re-trigger it and the file would quietly go stale until edited directly.
    /// The last successful computation's set is still the best information
    /// available, and the next successful one replaces it.
    fn record_dependencies(&mut self, uri: &Uri, dependencies: Option<Vec<PathBuf>>) {
        if let Some(dependencies) = dependencies {
            self.dependencies.insert(uri.clone(), dependencies);
        }
    }

    /// URIs of every buffer whose recorded include dependency set contains
    /// `include` (R17). Compared by canonical path so `.`/symlink components in
    /// a resolved include path don't defeat the match.
    fn buffers_depending_on(&self, include: &Path) -> Vec<Uri> {
        let target = std::fs::canonicalize(include).unwrap_or_else(|_| include.to_path_buf());
        self.dependencies
            .iter()
            .filter(|(_, deps)| {
                deps.iter().any(|p| {
                    std::fs::canonicalize(p)
                        .map(|c| c == target)
                        .unwrap_or_else(|_| p == include)
                })
            })
            .map(|(uri, _)| uri.clone())
            .collect()
    }

    fn open_uris(&self) -> Vec<Uri> {
        self.buffers.keys().cloned().collect()
    }

    fn retrigger_all(&mut self, now: std::time::Instant) {
        for uri in self.open_uris() {
            self.retrigger(&uri, now);
        }
    }

    /// Bump a buffer's salsa input to its current text (invalidating the coarse
    /// expansion memo so it re-reads includes) and schedule a recompute.
    fn retrigger(&mut self, uri: &Uri, now: std::time::Instant) {
        let buffer = self.buffers.get(uri).copied();
        let text = self.documents.get(uri).map(|d| d.text());
        if let (Some(buffer), Some(text)) = (buffer, text) {
            buffer.set_text(&mut self.db).to(text);
        }
        self.debouncer.schedule(uri.clone(), now);
    }

    /// Send a `textDocument/publishDiagnostics` notification.
    fn publish(&self, uri: Uri, diagnostics: Vec<lsp_types::Diagnostic>, version: Option<i32>) {
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version,
        };
        let notification = lsp_server::Notification {
            method: PublishDiagnostics::METHOD.to_string(),
            params: serde_json::to_value(params).unwrap_or(serde_json::Value::Null),
        };
        let _ = self
            .connection
            .sender
            .send(Message::Notification(notification));
    }

    /// Handle a `textDocument/formatting` request (R2, R4, R6, R7).
    ///
    /// Runs inline on the main loop — single-file formatting is sub-millisecond
    /// and needs no salsa snapshot or debounce round-trip (R7): it reads only the
    /// rope, never the lint pipeline's expansion (KTD1/KTD4 — the formatter must
    /// see raw bytes, and `FormatPipeline` has no way to see anything else). It
    /// also reads no configuration: the style was resolved once, with everything
    /// else, and the handle was built then. A malformed params payload or an
    /// unopened URI both resolve to an empty edit list (`[]`) rather than a
    /// protocol error (R6). The tokenize→parse→format pipeline is panic-guarded
    /// inside the shared `FormatPipeline`.
    fn handle_formatting(&self, req: Request) -> Result<()> {
        let edits: Vec<TextEdit> = serde_json::from_value::<DocumentFormattingParams>(req.params)
            .ok()
            .and_then(|params| self.documents.get(&params.text_document.uri))
            .map(|doc| {
                crate::formatting::compute_formatting_edits(doc, &self.formatter, &self.encoding)
            })
            .unwrap_or_default();
        self.respond(Response::new_ok(req.id, edits))
    }

    fn respond(&self, response: Response) -> Result<()> {
        self.connection
            .sender
            .send(Message::Response(response))
            .context("sending LSP response")
    }
}

/// Compute a buffer's diagnostics **and** its include dependencies under one
/// shared panic guard (R8), degrading both together to `(None, None)` on a
/// panic.
///
/// Both diagnostics paths — the main loop's `compute_and_publish` and the
/// debounced worker — call this rather than the two queries directly, so neither
/// can drift into a narrower guard.
///
/// **The guard must span both calls.** `buffer_dependencies` runs the same
/// buffer through salsa one line later, so a genuine panic in expansion just
/// past a diagnostics-only guard would still kill the worker or the main loop.
/// The two queries each carry their own `Cancelled::catch` *inside* this guard
/// (KTD6): a cancellation is a race to abandon, not a bug to contain, and
/// letting it reach `catch_panic` would report every concurrent edit as a panic.
///
/// Both halves are `Option`, and `None` means the same thing on each: **no
/// trustworthy answer** — cancelled, or panicked and contained. Neither may be
/// committed as an empty result, which is why the dependency half is not a bare
/// `Vec`: an empty dependency set is a real answer (a file with no includes),
/// and recording it for a buffer that does have includes stops the watcher from
/// ever re-triggering that buffer again.
///
/// Returning normally on a panic is the contract the worker relies on: its
/// `send` sits after this call, so a contained panic still produces a result and
/// that buffer never stalls waiting on one that never arrives.
///
/// `label` names the buffer in the report; it is only ever the URI string.
pub(crate) fn analyze_guarded(
    snapshot: &AnalysisDatabase,
    buffer: Buffer,
    schema: SchemaHandle,
    label: &str,
) -> Analysis {
    let computed = catch_panic(|| {
        let diagnostics = compute_diagnostics(snapshot, buffer, schema);
        // Dependency paths come from the (now-warm) expansion memo.
        let dependencies = buffer_dependencies(snapshot, buffer);
        (diagnostics, dependencies)
    });
    match computed {
        Ok((diagnostics, dependencies)) => Analysis {
            diagnostics,
            dependencies,
            panicked: false,
        },
        Err(panic) => {
            eprintln!("oxabl-lsp: analysis panicked for {label}: {panic}");
            Analysis {
                diagnostics: None,
                dependencies: None,
                panicked: true,
            }
        }
    }
}

/// The outcome of one [`analyze_guarded`] call.
///
/// The two `Option`s carry the "no trustworthy answer" contract described above;
/// `panicked` is what separates the two ways of having no answer. Client-visible
/// behavior does not distinguish them, but the server's *scheduling* must: a
/// cancelled computation is worth re-running, a panicked one is not.
pub(crate) struct Analysis {
    pub(crate) diagnostics: Option<oxabl_analyze::CollectedDiagnostics>,
    pub(crate) dependencies: Option<Vec<PathBuf>>,
    pub(crate) panicked: bool,
}

/// Best-effort conversion of a `file:` URI to a filesystem path. Returns `None`
/// for non-`file` schemes (the server simply skips workspace config for those).
///
/// `pub(crate)` so the formatting handler resolves URIs the same way the
/// watcher/`didOpen` code does, rather than hand-rolling a second decoder
/// (KTD2).
///
/// The path component is percent-**decoded**: every real client escapes a space
/// as `%20`, and a path with a space in it is ordinary. Leaving it encoded made
/// such a document fail to convert, which cost the whole session its
/// configuration anchor (see [`Server::ensure_config`]) and made
/// `buffers_depending_on` unable to match a watched include under such a
/// directory.
pub(crate) fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    let rest = s.strip_prefix("file://")?;
    // `file:///abs/path` → the authority is empty, leaving a leading `/abs`.
    Some(PathBuf::from(percent_decode(rest)?))
}

/// Percent-decode a URI path component.
///
/// A `%` not followed by two hex digits is kept literally rather than rejected:
/// the caller wants a best-effort path, and a lone `%` is a legal character in a
/// filename. `None` only when the decoded bytes are not valid UTF-8 — there is no
/// path to hand back in that case, and the callers all treat an unconvertible URI
/// as "no workspace here".
fn percent_decode(s: &str) -> Option<String> {
    if !s.contains('%') {
        // The overwhelmingly common case: no allocation beyond the copy.
        return Some(s.to_string());
    }
    let bytes = s.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && let (Some(hi), Some(lo)) = (
                bytes.get(i + 1).and_then(|b| (*b as char).to_digit(16)),
                bytes.get(i + 2).and_then(|b| (*b as char).to_digit(16)),
            )
        {
            out.push((hi * 16 + lo) as u8);
            i += 3;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8(out).ok()
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    fn server(connection: &Connection) -> Server<'_> {
        Server::new(
            connection,
            PositionEncodingKind::UTF8,
            std::time::Duration::from_millis(10),
            None,
        )
    }

    /// The regression a cancellation used to cause (R17): `buffer_dependencies`
    /// had no `Cancelled::catch`, so a concurrent write turned into a contained
    /// panic, the result carried an *empty* dependency set, and the buffer was
    /// recorded as depending on nothing. From then on `buffers_depending_on`
    /// never matched it, so editing an `.i` it includes stopped re-triggering it
    /// and the file went stale until edited directly.
    ///
    /// A cancelled computation now reports `None`, and the previously-recorded
    /// set must survive it.
    #[test]
    fn a_cancelled_computation_keeps_the_recorded_dependency_set() {
        let (connection, _client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");
        let include = PathBuf::from("/w/decls.i");
        server.record_dependencies(&uri, Some(vec![include.clone()]));

        // What a cancelled run delivers: no diagnostics, no dependency set.
        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            panicked: false,
            config_generation: 0, // no configuration is resolved in this test
        });

        assert_eq!(
            server.dependencies.get(&uri),
            Some(&vec![include.clone()]),
            "a cancellation must not erase the buffer's watcher registration"
        );
        assert_eq!(
            server.buffers_depending_on(&include),
            vec![uri],
            "and the include must still match the buffer"
        );
    }

    /// The other half of the contract: a *successful* computation still replaces
    /// the recorded set, including with an empty one — a file that no longer
    /// includes anything must stop matching its old includes.
    #[test]
    fn a_successful_computation_replaces_the_recorded_dependency_set() {
        let (connection, _client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");
        let include = PathBuf::from("/w/decls.i");
        server.record_dependencies(&uri, Some(vec![include.clone()]));

        server.record_dependencies(&uri, Some(Vec::new()));

        assert_eq!(server.dependencies.get(&uri), Some(&Vec::new()));
        assert!(
            server.buffers_depending_on(&include).is_empty(),
            "an empty set is a real answer and must take effect"
        );
    }

    /// Salsa cancellation is **global**: a write to buffer A's input flags every
    /// live snapshot, so an edit in A cancels B's in-flight computation even
    /// though B did not change. B's timer was consumed when its worker was
    /// spawned, so without an explicit reschedule the cancelled result is simply
    /// dropped and B keeps showing pre-edit diagnostics until it is touched
    /// again. The cancelled arm must re-arm the timer.
    #[test]
    fn a_cancelled_computation_reschedules_the_recompute() {
        let (connection, _client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");
        assert!(server.debouncer.next_deadline().is_none());

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            panicked: false,
            config_generation: 0, // no configuration is resolved in this test
        });

        assert!(
            server.debouncer.next_deadline().is_some(),
            "a cancelled computation must re-arm the buffer's debounce timer"
        );
    }

    /// The other side of that arm: a *contained panic* also arrives as absent
    /// diagnostics, and rescheduling it would spin the file forever — a panic is
    /// deterministic in the buffer's text, so every retry panics again. Only a
    /// cancellation is retried.
    #[test]
    fn a_contained_panic_does_not_reschedule() {
        let (connection, _client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            panicked: true,
            config_generation: 0, // no configuration is resolved in this test
        });

        assert!(
            server.debouncer.next_deadline().is_none(),
            "a contained panic must not be retried in a loop"
        );
    }

    /// A worker that finished just *before* an `oxabl.toml` change would
    /// otherwise publish one set of diagnostics computed under the old
    /// configuration — severities from the file the user just rewrote. The result
    /// carries the generation it was computed under and is dropped when that has
    /// moved, then recomputed under the current one.
    #[test]
    fn a_result_from_a_superseded_config_is_dropped_and_retried() {
        let (connection, client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");

        let stale = server.config_generation;
        server.install_config(PipelineConfig::default());
        assert_ne!(
            stale, server.config_generation,
            "installing a configuration must move the generation"
        );

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: Some(oxabl_analyze::CollectedDiagnostics::default()),
            dependencies: None,
            panicked: false,
            config_generation: stale,
        });

        assert!(
            client.receiver.try_recv().is_err(),
            "a result from the old configuration must not be published"
        );
        assert!(
            server.debouncer.next_deadline().is_some(),
            "and the buffer must be recomputed under the new configuration"
        );
    }

    /// The two rules meet here: the generation gate is reached *before* the
    /// absent-diagnostics arm, so a contained panic that happened to land under a
    /// superseded generation was rescheduled by the gate — the one retry `panicked`
    /// exists to prevent, since the panic is deterministic in the buffer's text and
    /// the retry panics again. Dropped, not retried.
    #[test]
    fn a_panic_from_a_superseded_config_is_dropped_and_not_retried() {
        let (connection, client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");

        let stale = server.config_generation;
        server.install_config(PipelineConfig::default());
        assert_ne!(stale, server.config_generation);

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            panicked: true,
            config_generation: stale,
        });

        assert!(
            client.receiver.try_recv().is_err(),
            "there is nothing to publish"
        );
        assert!(
            server.debouncer.next_deadline().is_none(),
            "and a panic must not be retried, whichever gate drops it"
        );
    }

    /// The gate must not swallow the ordinary case: a result computed under the
    /// current generation publishes.
    #[test]
    fn a_current_generation_result_publishes() {
        let (connection, client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 1, "MESSAGE \"hi\".\n");

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: Some(oxabl_analyze::CollectedDiagnostics::default()),
            dependencies: None,
            panicked: false,
            config_generation: server.config_generation,
        });

        let message = client.receiver.try_recv().expect("a publish was sent");
        match message {
            Message::Notification(n) => assert_eq!(n.method, "textDocument/publishDiagnostics"),
            other => panic!("expected a publish, got {other:?}"),
        }
    }

    /// Percent-escapes are how every real client spells a path with a space, and
    /// the decoded path is the one that exists on disk. An undecoded one anchored
    /// nothing: no `oxabl.toml` discovery, and no watcher match for an include
    /// under such a directory.
    #[test]
    fn uri_to_path_decodes_percent_escapes() {
        let uri = Uri::from_str("file:///home/dev/my%20project/main.p").unwrap();
        assert_eq!(
            uri_to_path(&uri),
            Some(PathBuf::from("/home/dev/my project/main.p"))
        );

        // A literal `%` in a filename round-trips through its own escape.
        let uri = Uri::from_str("file:///w/100%25.p").unwrap();
        assert_eq!(uri_to_path(&uri), Some(PathBuf::from("/w/100%.p")));

        // A `%` that is not an escape is kept, not rejected: best-effort, and a
        // bare `%` is a legal filename character. (Asserted on the decoder
        // directly — `Uri` will not parse an invalid octet in the first place.)
        assert_eq!(percent_decode("/w/a%zz.p").as_deref(), Some("/w/a%zz.p"));
        assert_eq!(
            percent_decode("/w/trailing%").as_deref(),
            Some("/w/trailing%")
        );

        // Unescaped paths are unaffected.
        let uri = Uri::from_str("file:///w/plain.p").unwrap();
        assert_eq!(uri_to_path(&uri), Some(PathBuf::from("/w/plain.p")));
    }

    /// A document with no filesystem path yields no anchor, which is what makes
    /// leaving `config_resolved` unset the right behavior for it.
    #[test]
    fn uri_to_path_rejects_non_file_schemes() {
        for s in ["untitled:Untitled-1", "git:/w/main.p?ref%3Dhead"] {
            let uri = Uri::from_str(s).unwrap();
            assert_eq!(uri_to_path(&uri), None, "{s} must not yield a path");
        }
    }

    /// A superseded result must not reschedule either: the edit that superseded
    /// it already scheduled its own recompute, and re-arming here would only
    /// duplicate that timer.
    #[test]
    fn a_superseded_result_does_not_reschedule() {
        let (connection, _client) = Connection::memory();
        let mut server = server(&connection);
        let uri = Uri::from_str("file:///main.p").unwrap();
        server.documents.open(uri.clone(), 2, "MESSAGE \"hi\".\n");

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            panicked: false,
            config_generation: 0, // no configuration is resolved in this test
        });

        assert!(server.debouncer.next_deadline().is_none());
    }
}
