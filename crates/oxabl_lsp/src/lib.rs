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
    let encoding = handshake(connection)?;
    let mut server = Server::new(connection, encoding, debounce_window);
    server.main_loop()
}

/// Perform the initialize handshake, negotiating the position encoding from the
/// client's advertised `general.positionEncodings` before replying with the v1
/// server capabilities (R2, R3).
fn handshake(connection: &Connection) -> Result<PositionEncodingKind> {
    let (id, params) = connection
        .initialize_start()
        .context("LSP initialize_start")?;
    let init: InitializeParams =
        serde_json::from_value(params).context("deserializing InitializeParams")?;

    let encoding = negotiate_position_encoding(&init.capabilities);
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
    Ok(encoding)
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
    /// The path the workspace configuration was resolved from (first opened
    /// document), used to re-resolve on `oxabl.toml` / `.df` changes (R16/R17).
    workspace_anchor: Option<PathBuf>,
    /// The format pipeline, rebuilt whenever configuration is resolved so a
    /// formatting request costs no style resolution and no `StyleGuide` clone.
    formatter: FormatPipeline,
    /// Per-URI include dependency paths (for watcher `*.i` matching, R17).
    dependencies: HashMap<Uri, Vec<PathBuf>>,
    /// Per-URI debounce timers (R13).
    debouncer: crate::debounce::Debouncer,
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
}

impl<'c> Server<'c> {
    fn new(
        connection: &'c Connection,
        encoding: PositionEncodingKind,
        debounce_window: std::time::Duration,
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
            formatter,
            dependencies: HashMap::new(),
            debouncer: crate::debounce::Debouncer::new(debounce_window),
        }
    }

    /// Dispatch loop. Multiplexes the client connection, completed background
    /// computations, and debounce-timer expirations via `select!`. Returns
    /// whether shutdown was clean.
    fn main_loop(&mut self) -> Result<bool> {
        use crossbeam_channel::{after, never, select};

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
            let tx = result_tx.clone();
            std::thread::spawn(move || {
                // The guard is inside `analyze_guarded`, so the send below is
                // reached on every path — a panic must not leave this buffer
                // waiting forever on a result that never arrives (R8).
                let (diagnostics, dependencies) =
                    analyze_guarded(&snapshot, buffer, schema, uri.as_str());
                let _ = tx.send(ComputeResult {
                    uri,
                    version,
                    diagnostics,
                    dependencies,
                });
            });
        }
    }

    /// Publish a completed computation, unless it was cancelled (`None`) or its
    /// buffer version has since been superseded by a newer edit (KTD7).
    fn handle_result(&mut self, res: ComputeResult) {
        match self.documents.get(&res.uri) {
            None => return,                                    // buffer closed
            Some(doc) if doc.version != res.version => return, // superseded by a newer edit
            Some(_) => {}
        }
        self.record_dependencies(&res.uri, res.dependencies);
        let Some(collected) = res.diagnostics else {
            return; // cancelled snapshot read
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
                    let buffer = Buffer::new(&self.db, doc.text);
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

    /// Resolve configuration from the first opened document's workspace (once),
    /// establishing include paths, the `[lint]` severity surface, the style, and
    /// the schema so include-resident symbols resolve (R9) and schema-gated
    /// rules go live (R10).
    ///
    /// One resolution feeds every surface: diagnostics *and* formatting read the
    /// same [`PipelineConfig`], so the server no longer has a second config path
    /// that could disagree with the first (KTD3).
    fn ensure_config(&mut self, uri: &Uri) {
        if self.config_resolved {
            return;
        }
        self.config_resolved = true;
        if let Some(path) = uri_to_path(uri) {
            self.workspace_anchor = Some(path.clone());
            self.resolve_config_from(&path);
        }
        self.register_file_watchers();
    }

    /// Best-effort dynamic registration of file watchers for `*.i`, `*.df`, and
    /// `oxabl.toml` so a real editor forwards their changes as
    /// `workspace/didChangeWatchedFiles` (R16/R17). Clients without dynamic
    /// registration simply ignore it; the server needs no response.
    fn register_file_watchers(&self) {
        let params = serde_json::json!({
            "registrations": [{
                "id": "oxabl-file-watchers",
                "method": "workspace/didChangeWatchedFiles",
                "registerOptions": {
                    "watchers": [
                        { "globPattern": "**/*.i" },
                        { "globPattern": "**/*.df" },
                        { "globPattern": "**/oxabl.toml" }
                    ]
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
    fn install_config(&mut self, resolved: PipelineConfig) {
        self.formatter = FormatPipeline::new(resolved.style.clone());
        self.db.set_config(AnalysisConfig {
            fs: std::sync::Arc::new(RealFileSystem),
            pipeline: std::sync::Arc::new(resolved),
            preprocess: true,
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
        let (collected, dependencies) =
            analyze_guarded(&snapshot, buffer, self.schema, uri.as_str());
        self.record_dependencies(uri, dependencies);
        let Some(collected) = collected else {
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
    /// (R17). Re-triggering bumps the buffer's salsa input so the coarse
    /// expansion memo recomputes and re-reads the changed file.
    fn handle_watched_files(&mut self, params: DidChangeWatchedFilesParams) {
        let now = std::time::Instant::now();
        let mut schema_changed = false;
        let mut config_changed = false;
        let mut changed_includes: Vec<PathBuf> = Vec::new();

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
) -> (
    Option<oxabl_analyze::CollectedDiagnostics>,
    Option<Vec<PathBuf>>,
) {
    let computed = catch_panic(|| {
        let diagnostics = compute_diagnostics(snapshot, buffer, schema);
        // Dependency paths come from the (now-warm) expansion memo.
        let dependencies = buffer_dependencies(snapshot, buffer);
        (diagnostics, dependencies)
    });
    match computed {
        Ok(pair) => pair,
        Err(panic) => {
            eprintln!("oxabl-lsp: analysis panicked for {label}: {panic}");
            (None, None)
        }
    }
}

/// Best-effort conversion of a `file:` URI to a filesystem path. Returns `None`
/// for non-`file` schemes (the server simply skips workspace config for those).
///
/// `pub(crate)` so the formatting handler resolves URIs the same way the
/// watcher/`didOpen` code does, rather than hand-rolling a second decoder
/// (KTD2). This is a bare `file://` strip with no percent-decoding: a
/// `%`-encoded path simply fails to convert and the caller falls back safely.
pub(crate) fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    let rest = s.strip_prefix("file://")?;
    // `file:///abs/path` → the authority is empty, leaving a leading `/abs`.
    Some(PathBuf::from(rest))
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
}
