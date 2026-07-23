//! `oxabl lsp` — a stdio LSP server that surfaces oxabl's parse + lint +
//! loud-preprocessor diagnostics live in an editor (Track A).
//!
//! v1 is a diagnostics-only skeleton: it completes the LSP handshake, syncs
//! open buffers incrementally, and pushes `publishDiagnostics`. It advertises
//! nothing else (R2). The heavy lifting — the actual analysis — reuses the
//! existing pure pipeline through the shared collector
//! ([`oxabl_analyze::collect_diagnostics`]); this crate is server plumbing.
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
use oxabl_schema::{Schema, SchemaLoader};
use oxabl_workspace::{
    RealFileSystem, WorkspaceConfig, find_workspace_root, resolved_include_paths,
    resolved_lint_config,
};
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
    /// Resolved `.df` schema file paths (for watcher matching, R16).
    schema_files: Vec<PathBuf>,
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
    /// Include dependency paths observed during this computation (R17).
    dependencies: Vec<PathBuf>,
}

impl<'c> Server<'c> {
    fn new(
        connection: &'c Connection,
        encoding: PositionEncodingKind,
        debounce_window: std::time::Duration,
    ) -> Self {
        let db = AnalysisDatabase::new(AnalysisConfig::default());
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
            schema_files: Vec::new(),
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
                let diagnostics = compute_diagnostics(&snapshot, buffer, schema);
                // Dependency paths come from the (now-warm) expansion memo.
                let dependencies = buffer_dependencies(&snapshot, buffer);
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
        let Some(doc) = self.documents.get(&res.uri) else {
            return; // buffer closed
        };
        if doc.version != res.version {
            return; // superseded by a newer edit
        }
        self.dependencies
            .insert(res.uri.clone(), res.dependencies.clone());
        let Some(collected) = res.diagnostics else {
            return; // cancelled snapshot read
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

    /// Resolve the db configuration from the first opened document's workspace
    /// (once), establishing include paths, the `[lint]` severity surface, and
    /// the schema so include-resident symbols resolve (R9) and schema-gated
    /// rules go live (R10).
    fn ensure_config(&mut self, uri: &Uri) {
        if self.config_resolved {
            return;
        }
        self.config_resolved = true;
        if let Some(path) = uri_to_path(uri) {
            self.workspace_anchor = Some(path.clone());
            let config = self.resolve_workspace_config(&path);
            self.db.set_config(config);
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

    /// Resolve the full [`AnalysisConfig`] for a document `path`: include paths,
    /// lint severities, and the loaded schema. Also records the resolved `.df`
    /// paths for watcher matching.
    fn resolve_workspace_config(&mut self, path: &Path) -> AnalysisConfig {
        let (include_paths, _err) = resolved_include_paths(path, &[]);
        let (lint_config, _lint_err) = resolved_lint_config(path, &[]);
        let (schema, schema_loaded, schema_files) = load_workspace_schema(path);
        self.schema_files = schema_files;
        AnalysisConfig {
            fs: std::sync::Arc::new(RealFileSystem),
            schema: std::sync::Arc::new(schema),
            schema_loaded,
            include_paths: std::sync::Arc::new(include_paths),
            lint_severities: std::sync::Arc::new(lint_config.to_severity_map()),
            preprocess: true,
        }
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
        let collected = compute_diagnostics(&snapshot, buffer, self.schema);
        self.dependencies
            .insert(uri.clone(), buffer_dependencies(&snapshot, buffer));
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

        // Re-resolve include paths + lint config from the anchor (R17). This
        // also reloads the schema, so it subsumes a `.df` change too.
        if config_changed {
            if let Some(anchor) = self.workspace_anchor.clone() {
                let config = self.resolve_workspace_config(&anchor);
                self.db.set_config(config);
            }
            // Bump the config revision so the diagnostics query recomputes with
            // the new lint severities (config is db state, not a salsa input, so
            // it would not otherwise invalidate — and re-triggering the buffer
            // alone backdates through the unchanged expansion). The re-trigger
            // (set_text) additionally re-reads includes for a changed PROPATH.
            self.bump_config_revision();
            self.retrigger_all(now);
        } else if schema_changed {
            self.reload_schema();
            // Bumping the schema handle invalidates the diagnostics query for
            // every buffer (R16); schedule them all to republish.
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

    /// Reload the schema from the resolved `.df` files and bump the schema
    /// handle so dependent diagnostics recompute live (R16).
    fn reload_schema(&mut self) {
        let fs = RealFileSystem;
        let (schema, _diags) = SchemaLoader::load_files(&self.schema_files, &fs);
        let loaded = !self.schema_files.is_empty();
        let mut config = self.db.config().clone();
        config.schema = std::sync::Arc::new(schema);
        config.schema_loaded = loaded;
        self.db.set_config(config);
        self.bump_config_revision();
    }

    /// Bump the shared config/schema revision input so the diagnostics query is
    /// invalidated and recomputes with the current (untracked) db config.
    fn bump_config_revision(&mut self) {
        let next = self.schema.revision(&self.db).wrapping_add(1);
        self.schema.set_revision(&mut self.db).to(next);
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
    /// and needs no salsa snapshot or debounce round-trip (R7): it reads only
    /// the rope and the filesystem, never `expanded_text`/`collect_from_expanded`
    /// (KTD1). A malformed params payload or an unopened URI both resolve to an
    /// empty edit list (`[]`) rather than a protocol error (R6). The whole
    /// tokenize→parse→format pipeline is panic-guarded inside
    /// [`compute_formatting_edits`] (KTD4).
    fn handle_formatting(&self, req: Request) -> Result<()> {
        let edits: Vec<TextEdit> = serde_json::from_value::<DocumentFormattingParams>(req.params)
            .ok()
            .and_then(|params| {
                let uri = params.text_document.uri;
                self.documents.get(&uri).map(|doc| {
                    crate::formatting::compute_formatting_edits(doc, &uri, &self.encoding)
                })
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

/// Discover `oxabl.toml` from `path` and load the schema declared in its
/// `[workspace.schema].files`. Returns the schema, whether a `.df` was actually
/// loaded (gates schema-dependent rules, R10), and the resolved `.df` paths
/// (for watcher matching, R16).
fn load_workspace_schema(path: &Path) -> (Schema, bool, Vec<PathBuf>) {
    let start_dir = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| path.to_path_buf())
    };
    let Some(root) = find_workspace_root(&start_dir) else {
        return (Schema::empty(), false, Vec::new());
    };
    let Ok(cfg) = WorkspaceConfig::from_path(&root) else {
        return (Schema::empty(), false, Vec::new());
    };
    let files: Vec<PathBuf> = cfg
        .workspace
        .schema
        .files
        .iter()
        .map(|f| {
            if f.is_absolute() {
                f.clone()
            } else {
                root.join(f)
            }
        })
        .collect();
    if files.is_empty() {
        return (Schema::empty(), false, Vec::new());
    }
    let fs = RealFileSystem;
    let (schema, _diags) = SchemaLoader::load_files(&files, &fs);
    (schema, true, files)
}
