//! `oxabl lsp` — a stdio LSP server that surfaces oxabl's parse + lint +
//! loud-preprocessor diagnostics live in an editor (Track A).
//!
//! v1 is a diagnostics, formatting and outline server: it completes the LSP handshake,
//! syncs open buffers incrementally, pushes `publishDiagnostics`, and answers
//! `textDocument/formatting`, `textDocument/rangeFormatting`, `textDocument/documentSymbol` (with the declarations of include files;
//! see `docs/design/document-outline.md`), `textDocument/foldingRange`,
//! `textDocument/selectionRange`, `workspace/symbol`,
//! `textDocument/definition`, `textDocument/hover`, `textDocument/documentHighlight` and
//! `textDocument/documentLink` (see `docs/design/navigation.md`), `textDocument/semanticTokens`
//! (see `docs/design/semantic-tokens.md`), and `textDocument/references` and the call
//! hierarchy (see `docs/design/references.md`), and the reference-count code lenses
//! (`docs/design/reference-lenses.md`). Strict database rules can also enable effect code lenses and
//! database inlay hints (`docs/design/inlay-hints.md`). An open include can be analysed in a
//! root that expands it (`oxabl/includerContexts`, `oxabl/selectIncluderContext`), and
//! `oxabl/expandedUnit` shows the source parsed for a file after preprocessing; see
//! `docs/design/include-context.md`. `oxabl/schema` and `oxabl/schemaTable`
//! answer the configured `.df` files as a tree for a schema browser; see
//! `docs/design/schema-browser.md`. With each diagnostics publish the server sends
//! `textDocument/inactiveRegions`, the `&IF` branches that are not compiled, to a
//! client that asks for them (`docs/design/inactive-regions.md`).
//!
//! **This crate orchestrates nothing.** Both the analysis and the formatting are
//! [`oxabl_pipeline`]'s, and the salsa substrate that memoizes them is
//! [`oxabl_daemon`]'s — the session core, where the four supersession disciplines
//! live so no client owns a second copy of them. What is left here is genuinely
//! server plumbing: *when* to run, *which* buffers to invalidate, and how to render
//! a byte span as a `Range` under the negotiated encoding.
//!
//! Threading discipline: input mutations run on the main loop; each debounced
//! diagnostics computation runs on a cloned salsa snapshot on a worker thread. See
//! `oxabl_daemon::db` and [`debounce`].

pub mod capabilities;
mod code_lens;
mod completion;
pub mod debounce;
pub mod diagnostics;
pub mod document;
mod document_symbol;
mod editor_syntax;
mod expanded_unit;
pub mod formatting;
mod hierarchy;
mod inactive_regions;
mod includer_context;
mod inlay_hint;
mod navigation;
mod outline_includes;
pub mod position;
mod range_formatting;
mod reference_lens;
mod references;
mod schema;
mod semantic_tokens;
mod signature_help;
mod workspace_symbol;

use std::collections::HashMap;
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::process::{Command, Stdio};

use anyhow::{Context, Result};
use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument, DidOpenTextDocument,
    DidSaveTextDocument, Notification as _, PublishDiagnostics,
};
use lsp_types::request::{
    CallHierarchyIncomingCalls, CallHierarchyOutgoingCalls, CallHierarchyPrepare, Completion,
    DocumentHighlightRequest, DocumentLinkRequest, DocumentSymbolRequest, FoldingRangeRequest,
    Formatting, GotoDefinition, GotoImplementation, HoverRequest, InlayHintRequest,
    RangeFormatting, References, Request as _, ResolveCompletionItem, SelectionRangeRequest,
    SemanticTokensFullDeltaRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
    SignatureHelpRequest, TypeHierarchyPrepare, TypeHierarchySubtypes, TypeHierarchySupertypes,
    WorkspaceSymbolRequest,
};
use lsp_types::{
    DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentSymbolParams, FoldingRangeParams, InitializeParams, InitializeResult,
    PositionEncodingKind, PublishDiagnosticsParams, SelectionRangeParams, ServerInfo, TextEdit,
    Uri,
};
use oxabl_pipeline::{ConfigOverrides, ConfigWarning, FormatPipeline, PipelineConfig};
use oxabl_workspace::FeaturesConfig;

use crate::capabilities::{negotiate_position_encoding, server_capabilities};
// The salsa substrate and the supersession disciplines live in `oxabl_daemon`
// now. This crate supplies the LSP framing and rendering around them; it does not
// own a second copy of either.
use crate::diagnostics::to_lsp_diagnostics;
use crate::document::DocumentStore;
use oxabl_daemon::{CompletedWork, Disposition, SessionHost, analyze_guarded, dispose};

/// Entry point invoked by the `oxabl lsp` subcommand. Owns the stdio
/// connection, runs the handshake and main loop, and joins the I/O threads.
///
/// Returns `Err` when the client sent `exit` without a prior `shutdown` (the
/// LSP-mandated non-zero exit), so the binary can map it to a failing
/// [`ExitCode`](std::process::ExitCode).
pub fn run() -> Result<()> {
    let (connection, io_threads) =
        oxabl_daemon::transport::stdio().context("starting the LSP I/O threads")?;
    let result = proxy_to_daemon(&connection);
    drop(connection);
    // The stdio reader can be blocked in the editor's still-open input pipe. On
    // a daemon failure there will never be another useful frame, so return and
    // let process exit close it instead of waiting forever to join it.
    result?;
    io_threads.join();
    Ok(())
}

#[cfg(unix)]
fn proxy_to_daemon(connection: &Connection) -> Result<()> {
    let first = connection
        .receiver
        .recv()
        .context("waiting for LSP initialize")?;
    let Message::Request(initialize) = &first else {
        anyhow::bail!("the first LSP frame was not an initialize request");
    };
    if initialize.method != "initialize" {
        anyhow::bail!(
            "the first LSP request was `{}`, not `initialize`",
            initialize.method
        );
    }
    let init: InitializeParams = serde_json::from_value(initialize.params.clone())
        .context("deserializing InitializeParams")?;
    let workspace_root = declared_workspace_root(&init)
        .or_else(|| std::env::current_dir().ok())
        .context("finding a workspace root for the daemon")?;
    // A daemon that exits idle refuses a client that reaches it after it decided: the
    // connect fails, or the connection closes before the daemon answers. Either means
    // "no daemon here", so the client looks again and starts one.
    const ATTEMPTS: u32 = 3;
    let mut attempt = 1;
    let mut notices = Vec::new();
    // One limit for every attempt below, so the wait for a daemon never exceeds it.
    let mut progress = StartProgress::new(connection, &init, start_limit(&workspace_root));
    let ((daemon, daemon_threads, answer), daemon_pid) = loop {
        let registration = match find_or_start_daemon(&workspace_root, &mut notices, &mut progress)
        {
            Ok(registration) => registration,
            Err(error) => {
                // The editor shows a server that fails to start only as a crash, so a
                // daemon of another version this client cannot replace is named in the
                // editor itself. The binary prints the error on stderr.
                if error.downcast_ref::<VersionMismatch>().is_some() {
                    notify_editor(
                        connection,
                        lsp_types::MessageType::WARNING,
                        &error.to_string(),
                    );
                }
                return Err(error);
            }
        };
        for notice in notices.drain(..) {
            eprintln!("oxabl lsp: {notice}");
            let _ = connection
                .sender
                .send(Message::Notification(Notification::new(
                    "window/logMessage".to_string(),
                    lsp_types::LogMessageParams {
                        typ: lsp_types::MessageType::INFO,
                        message: notice,
                    },
                )));
        }
        match initialize_daemon(&registration, &first) {
            Ok(joined) => break (joined, registration.pid),
            Err(refusal) if attempt < ATTEMPTS => {
                // A daemon that refused this client and stays is not exiting: asking it
                // again gets the same answer.
                if !wait_for_departure(&workspace_root, registration.pid) {
                    anyhow::bail!(
                        "{refusal}, and it still serves the root, so it is not exiting. \
                         Its stderr may say why; stop it with `kill {}`",
                        registration.pid
                    );
                }
                eprintln!(
                    "oxabl lsp: {refusal} (attempt {attempt} of {ATTEMPTS}); looking for the \
                     daemon again"
                );
                attempt += 1;
            }
            Err(refusal) => anyhow::bail!("{refusal} (attempt {attempt} of {ATTEMPTS})"),
        }
    };
    drop(progress);
    let over_limit = daemon_threads.over_limit();
    let outcome = editor_send(connection, answer)
        .and_then(|()| proxy_messages(connection, daemon, daemon_pid, over_limit));
    daemon_threads.shutdown();
    outcome
}

/// Connect to the daemon a registration names, forward the editor's `initialize` and
/// wait for the daemon's first frame. An error means the daemon did not take this
/// client.
#[cfg(unix)]
fn initialize_daemon(
    registration: &oxabl_daemon_protocol::Registration,
    initialize: &Message,
) -> std::result::Result<(Connection, oxabl_daemon::listener::ClientThreads, Message), String> {
    let stream = UnixStream::connect(&registration.socket_path).map_err(|error| {
        format!(
            "could not connect to the oxabl daemon (pid {}) at {}: {error}",
            registration.pid, registration.socket_path
        )
    })?;
    let (daemon, daemon_threads) = oxabl_daemon::connection_over(stream);
    let answer = daemon
        .sender
        .send(initialize.clone())
        .ok()
        .and_then(|()| daemon.receiver.recv().ok());
    match answer {
        Some(answer) => Ok((daemon, daemon_threads, answer)),
        None => {
            drop(daemon);
            daemon_threads.shutdown();
            Err(format!(
                "the oxabl daemon (pid {}) closed the connection before it answered \
                 `initialize`",
                registration.pid
            ))
        }
    }
}

/// Wait, for two seconds at most, until the daemon `pid` no longer serves `root`: a
/// daemon that refused a client because it is exiting is still found until its
/// registration is gone. Returns whether it went.
#[cfg(unix)]
fn wait_for_departure(root: &Path, pid: u32) -> bool {
    use std::time::{Duration, Instant};
    let deadline = Instant::now() + Duration::from_secs(2);
    while Instant::now() < deadline {
        match oxabl_daemon::discover(root) {
            oxabl_daemon::Discovery::Live(registration)
            | oxabl_daemon::Discovery::VersionMismatch(registration)
                if registration.pid == pid => {}
            _ => return true,
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    false
}

/// Forward one frame from the daemon to the editor.
#[cfg(unix)]
fn editor_send(editor: &Connection, message: Message) -> Result<()> {
    editor
        .sender
        .send(with_null_result(message))
        .context("the editor connection closed")
}

/// Show `message` in the editor and wait until it is written.
///
/// The stdio writer takes the next frame only after it has written the one before, so
/// the second frame, a copy in the editor's log, returns once the warning is out. The
/// process may exit right after this.
#[cfg(unix)]
fn notify_editor(connection: &Connection, typ: lsp_types::MessageType, message: &str) {
    let show = Notification::new(
        "window/showMessage".to_string(),
        lsp_types::ShowMessageParams {
            typ,
            message: format!("oxabl: {message}"),
        },
    );
    let log = Notification::new(
        "window/logMessage".to_string(),
        lsp_types::LogMessageParams {
            typ,
            message: format!("oxabl lsp: {message}"),
        },
    );
    let _ = connection.sender.send(Message::Notification(show));
    let _ = connection.sender.send(Message::Notification(log));
}

/// A daemon of another version serves the root and this client could not replace it.
#[cfg(unix)]
#[derive(Debug)]
struct VersionMismatch(String);

#[cfg(unix)]
impl std::fmt::Display for VersionMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[cfg(unix)]
impl std::error::Error for VersionMismatch {}

#[cfg(unix)]
fn mismatch(message: String) -> anyhow::Error {
    anyhow::Error::new(VersionMismatch(message))
}

#[cfg(not(unix))]
fn proxy_to_daemon(_connection: &Connection) -> Result<()> {
    anyhow::bail!("the oxabl daemon currently requires Unix sockets")
}

/// Find the daemon serving `root`, starting one if discovery says there is none.
///
/// One loop, not a check followed by a loop, because discovery has four answers and
/// only one of them may start a daemon:
///
/// * `Absent` — nothing is registered. Start a daemon and keep polling. Start another
///   only when the one started lost the start lock
///   ([`oxabl_daemon::listener::EXIT_START_LOCK_HELD`]): another process, such as a
///   daemon that another client started or one that is still exiting, holds it. Any
///   other exit repeats on every start, so it is reported at once.
/// * `Undecided` — the registration could not be read, or the socket could not be
///   probed. Neither starting a daemon nor connecting is defensible on that answer, so
///   the loop waits. The reason is kept, because a client that runs out of attempts
///   over a refused directory must say *that* and not "timed out".
/// * `VersionMismatch` — a daemon of another build serves the root. An older one,
///   typically one that outlived an upgrade, is stopped (`SIGTERM`, then `SIGKILL`
///   after [`STOP_GRACE`]) and the loop starts a daemon of this build once it is
///   gone. A newer one is an error: replacing it would let two builds that both stay
///   in use stop each other's daemon on every restart of their editors. So is a
///   second daemon of another build, which means another build keeps starting them.
/// * `Live` — decided; return it.
///
/// How long it waits: a start on a loaded machine can take many seconds, so no fixed
/// time is right for it. While the daemon this client started runs, the loop waits.
/// A daemon that lost the start lock exits and is started again, so the loop also
/// waits for the process that holds the lock. Without such a daemon it stops
/// [`START_QUIET`] after the last poll that showed one. At `progress.deadline` it stops
/// in every case: the deadline is the same for every call of one start.
#[cfg(unix)]
fn find_or_start_daemon(
    root: &Path,
    notices: &mut Vec<String>,
    progress: &mut StartProgress<'_>,
) -> Result<oxabl_daemon_protocol::Registration> {
    use std::time::{Duration, Instant};

    const INTERVAL: Duration = Duration::from_millis(20);
    // A daemon that lost the start lock is started again after this interval, which
    // doubles after each such start up to `RESTART_MAX`.
    const RESTART_FIRST: Duration = Duration::from_millis(250);
    const RESTART_MAX: Duration = Duration::from_secs(1);

    let limit = progress.deadline;
    let mut quiet_until = Instant::now() + START_QUIET;
    let mut restart_interval = RESTART_FIRST;

    let mut started = false;
    let mut starts = 0;
    let mut child: Option<std::process::Child> = None;
    let mut last_start: Option<Instant> = None;
    let mut child_exit: Option<std::process::ExitStatus> = None;
    let mut stopping: Option<StaleDaemon> = None;
    // A daemon of another build too busy to be named on its socket: that can clear,
    // so the loop keeps polling and reports it only at the deadline.
    let mut busy: Option<String> = None;
    let mut refusal: Option<String> = None;
    // A shared limit that an earlier attempt has used up leaves no time to start a daemon.
    if Instant::now() >= limit {
        anyhow::bail!(
            "the oxabl daemon did not become ready in time: the limit of {} s ended before \
             this client could look for it again",
            progress.limit.as_secs()
        );
    }
    loop {
        let mut absent = false;
        match oxabl_daemon::discover(root) {
            oxabl_daemon::Discovery::Live(registration) => return Ok(registration),
            oxabl_daemon::Discovery::VersionMismatch(registration) => match &mut stopping {
                // The daemon this client stopped, not gone yet.
                Some(stale) if stale.registration.pid == registration.pid => {
                    if !stale.killed && stale.since.elapsed() >= STOP_GRACE {
                        stop(&registration, root, true)?;
                        stale.killed = true;
                    }
                }
                // Another daemon of another build after this client stopped one or
                // started its own.
                Some(_) => return Err(mismatch(keeps_starting(&registration, root))),
                None if started => return Err(mismatch(keeps_starting(&registration, root))),
                None if !older_than_this_build(&registration) => {
                    return Err(mismatch(format!(
                        "{}. That daemon is not older than this client: update this oxabl, \
                         or stop it with `kill {}`. The next oxabl command then starts a \
                         daemon of this version",
                        mismatch_message(&registration, root),
                        registration.pid
                    )));
                }
                None => {
                    // Described before the signal: a daemon that has exited no longer
                    // shows that its executable was replaced.
                    let described = mismatch_message(&registration, root);
                    match oxabl_daemon::stop_daemon(&registration, false) {
                        Ok(()) => {
                            notices.push(format!(
                                "{described}. Stopped it; a daemon of this version serves the \
                             root, and its other clients reconnect"
                            ));
                            busy = None;
                            stopping = Some(StaleDaemon {
                                registration,
                                since: Instant::now(),
                                killed: false,
                            });
                            // The old daemon's exit is not this client's start: both get
                            // time.
                            quiet_until = Instant::now() + STOP_GRACE + START_QUIET;
                        }
                        Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                            busy = Some(not_stopped(&registration, root, &error));
                        }
                        Err(error) => {
                            return Err(mismatch(not_stopped(&registration, root, &error)));
                        }
                    }
                }
            },
            // Keep the latest reason rather than the first: a cause that has cleared
            // should not be the one reported, and one that has not will be read again
            // on the next pass.
            oxabl_daemon::Discovery::Undecided(reason) => refusal = Some(reason),
            oxabl_daemon::Discovery::Absent => absent = true,
        }
        // Whatever discovery said: a daemon that exited for a reason other than the
        // start lock fails the same way on every start, so it is reported at once.
        if let Some(status) = child.as_mut().and_then(|child| child.try_wait().ok()?) {
            if !lost_start_lock(status) {
                return Err(exited_before_serving(root, status, refusal.as_deref()));
            }
            // A live process holds the start lock: the kernel releases it when the
            // holder exits.
            child_exit = Some(status);
            child = None;
        }
        if absent && child.is_none() && last_start.is_none_or(|at| at.elapsed() >= restart_interval)
        {
            let executable = std::env::current_exe().context("locating the oxabl executable")?;
            // Every daemon writes to the editor's log, never to a pipe of this process,
            // because it may outlive this process. A restart does not repeat the first
            // start's refusal of the start lock.
            let mut command = Command::new(executable);
            command
                .arg("daemon")
                .arg(root)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::inherit());
            if started {
                command.arg("--quiet-if-locked");
                restart_interval = (restart_interval * 2).min(RESTART_MAX);
            }
            child = Some(command.spawn().context("starting the oxabl daemon")?);
            started = true;
            starts += 1;
            last_start = Some(Instant::now());
        }
        let running = child
            .as_mut()
            .is_some_and(|child| matches!(child.try_wait(), Ok(None)));
        if running {
            quiet_until = Instant::now() + START_QUIET;
        }
        progress.waiting(root);
        // Nothing changes between the last poll and the give-up, so it is not waited
        // for.
        let now = Instant::now();
        if now >= quiet_until || now >= limit {
            break;
        }
        std::thread::sleep(INTERVAL);
    }

    // The timeout is the symptom; the refusal, or a daemon that would not stop, is the
    // cause, and a message that reports only the symptom sends its reader looking for
    // a slow machine when the answer is a directory mode.
    if let Some(stale) = stopping.filter(|_| !started) {
        return Err(mismatch(format!(
            "{}, and it did not stop. Stop it with `kill -9 {}`",
            mismatch_message(&stale.registration, root),
            stale.registration.pid
        )));
    }
    if let Some(busy) = busy {
        return Err(mismatch(busy));
    }
    if let Some(child) = child.as_mut() {
        match child.try_wait() {
            Ok(Some(status)) if !lost_start_lock(status) => {
                return Err(exited_before_serving(root, status, refusal.as_deref()));
            }
            Ok(Some(status)) => child_exit = Some(status),
            Ok(None) => {
                let probe = refusal
                    .as_ref()
                    .map(|reason| format!(" The last look for it said: {reason}."))
                    .unwrap_or_default();
                anyhow::bail!(
                    "the oxabl daemon did not become ready in time: the daemon this client \
                     started for {} (pid {}) still runs after {} s and does not serve the \
                     root.{probe} It is left running, and the next start finds it when it is \
                     ready; stop it with `kill {}`. What it wrote is above",
                    root.display(),
                    child.id(),
                    progress.limit.as_secs(),
                    child.id()
                )
            }
            Err(_) => {}
        }
    }
    match (refusal, child_exit) {
        (Some(reason), _) => {
            anyhow::bail!("the oxabl daemon did not become ready in time: {reason}")
        }
        (None, Some(status)) => anyhow::bail!(
            "the oxabl daemon did not become ready in time: every daemon this \
             client started found the start lock held (starts: {starts}, last: {status})"
        ),
        (None, None) => anyhow::bail!("the oxabl daemon did not become ready in time"),
    }
}

/// Whether a daemon exited because another process holds the start lock of its root.
#[cfg(unix)]
fn lost_start_lock(status: std::process::ExitStatus) -> bool {
    status.code() == Some(i32::from(oxabl_daemon::listener::EXIT_START_LOCK_HELD))
}

/// The error for a started daemon that exited for another reason than the start lock.
#[cfg(unix)]
fn exited_before_serving(
    root: &Path,
    status: std::process::ExitStatus,
    refusal: Option<&str>,
) -> anyhow::Error {
    let probe = refusal
        .map(|reason| format!(" The last look for it said: {reason}."))
        .unwrap_or_default();
    anyhow::anyhow!(
        "the oxabl daemon this client started for {} exited ({status}) before it served \
         the root; its error is above.{probe}",
        root.display()
    )
}

/// The longest wait for a daemon to serve the root, from the first look for it,
/// over every attempt.
#[cfg(unix)]
const START_LIMIT: std::time::Duration = std::time::Duration::from_secs(60);

/// [`START_LIMIT`], or the shorter limit that a test build reads from the root's path
/// (`OXABL-TEST-LIMIT@<ms>`).
#[cfg(unix)]
fn start_limit(root: &Path) -> std::time::Duration {
    oxabl_common::injected_limit(&root.to_string_lossy()).unwrap_or(START_LIMIT)
}

/// How long the wait goes on after the last sign that a daemon is starting.
#[cfg(unix)]
const START_QUIET: std::time::Duration = std::time::Duration::from_secs(2);

/// A start that takes longer than this is shown to the editor.
#[cfg(unix)]
const SLOW_START: std::time::Duration = std::time::Duration::from_secs(1);

/// What the editor sees of a slow daemon start: a log message and, when the editor
/// sent a `workDoneToken` with `initialize`, a progress report on it. Both are among
/// the few messages a server may send before it answers `initialize`.
#[cfg(unix)]
struct StartProgress<'a> {
    connection: &'a Connection,
    token: Option<lsp_types::ProgressToken>,
    since: std::time::Instant,
    /// The longest wait, and the time it ends: the same for every attempt.
    limit: std::time::Duration,
    deadline: std::time::Instant,
    shown: bool,
}

#[cfg(unix)]
impl<'a> StartProgress<'a> {
    fn new(
        connection: &'a Connection,
        init: &InitializeParams,
        limit: std::time::Duration,
    ) -> Self {
        let since = std::time::Instant::now();
        StartProgress {
            connection,
            token: init.work_done_progress_params.work_done_token.clone(),
            since,
            limit,
            deadline: since + limit,
            shown: false,
        }
    }

    /// Called on each poll: shows the start once it has taken [`SLOW_START`].
    fn waiting(&mut self, root: &Path) {
        if self.shown || self.since.elapsed() < SLOW_START {
            return;
        }
        self.shown = true;
        let message = format!(
            "the oxabl daemon for {} is still starting; waiting for it while it runs, for {} s \
             at most",
            root.display(),
            self.limit.as_secs()
        );
        eprintln!("oxabl lsp: {message}");
        let _ = self
            .connection
            .sender
            .send(Message::Notification(Notification::new(
                "window/logMessage".to_string(),
                lsp_types::LogMessageParams {
                    typ: lsp_types::MessageType::INFO,
                    message: format!("oxabl lsp: {message}"),
                },
            )));
        self.report(lsp_types::WorkDoneProgress::Begin(
            lsp_types::WorkDoneProgressBegin {
                title: "oxabl".to_string(),
                cancellable: Some(false),
                message: Some("Starting the oxabl daemon".to_string()),
                percentage: None,
            },
        ));
    }

    fn report(&self, value: lsp_types::WorkDoneProgress) {
        let Some(token) = self.token.clone() else {
            return;
        };
        let _ = self
            .connection
            .sender
            .send(Message::Notification(Notification::new(
                "$/progress".to_string(),
                lsp_types::ProgressParams {
                    token,
                    value: lsp_types::ProgressParamsValue::WorkDone(value),
                },
            )));
    }
}

#[cfg(unix)]
impl Drop for StartProgress<'_> {
    fn drop(&mut self) {
        if self.shown {
            self.report(lsp_types::WorkDoneProgress::End(
                lsp_types::WorkDoneProgressEnd { message: None },
            ));
        }
    }
}

/// How long a daemon of another build has to exit on `SIGTERM` before `SIGKILL`.
#[cfg(unix)]
const STOP_GRACE: std::time::Duration = std::time::Duration::from_secs(1);

/// A daemon of another build this client has signalled to exit.
#[cfg(unix)]
struct StaleDaemon {
    registration: oxabl_daemon_protocol::Registration,
    since: std::time::Instant,
    killed: bool,
}

/// Signal a daemon of another build to exit, or fail naming both builds.
#[cfg(unix)]
fn stop(
    registration: &oxabl_daemon_protocol::Registration,
    root: &Path,
    force: bool,
) -> Result<()> {
    oxabl_daemon::stop_daemon(registration, force)
        .map_err(|error| mismatch(not_stopped(registration, root, &error)))
}

/// A daemon of another build that could not be signalled, with the way to stop it.
#[cfg(unix)]
fn not_stopped(
    registration: &oxabl_daemon_protocol::Registration,
    root: &Path,
    error: &std::io::Error,
) -> String {
    format!(
        "{}, and it could not be stopped: {error}. Stop it with `kill {}`",
        mismatch_message(registration, root),
        registration.pid
    )
}

/// Whether the daemon a registration names is of an older build than this client.
///
/// Versions compare by their numbers; a registration without one predates the field
/// and is older. At an equal version the contract decides. At an equal contract two
/// builds of one version are ordered by when their executables were written: a daemon
/// whose executable was replaced on disk, or that does not say when while this client
/// does, is older, and so is one written before this client's executable. A time that
/// does not order (equal, or unknown on this side) proves nothing, so the daemon stays.
/// Two editors on two builds of one version then leave each other's daemon alone, or
/// the newer build's daemon alone, and do not replace each other's daemon in turn.
#[cfg(unix)]
fn older_than_this_build(registration: &oxabl_daemon_protocol::Registration) -> bool {
    fn numbers(version: &str) -> Option<(u64, u64, u64)> {
        let core = version.split(['-', '+']).next()?;
        let mut parts = core.split('.').map(|part| part.parse::<u64>().ok());
        Some((parts.next()??, parts.next()??, parts.next()??))
    }
    if registration.daemon_version.is_empty() {
        return true;
    }
    match (
        numbers(&registration.daemon_version),
        numbers(oxabl_daemon::build_version()),
    ) {
        (Some(theirs), Some(ours)) if theirs != ours => theirs < ours,
        _ if registration.contract_version != oxabl_daemon_protocol::CONTRACT_VERSION => {
            registration.contract_version < oxabl_daemon_protocol::CONTRACT_VERSION
        }
        _ => {
            let (theirs, ours) = (
                registration.build_modified_ms,
                oxabl_daemon::build_identity().modified_ms,
            );
            oxabl_daemon::executable_replaced(registration.pid) || (ours != 0 && theirs < ours)
        }
    }
}

/// A daemon of another build that appeared after this client stopped one or
/// started its own.
#[cfg(unix)]
fn keeps_starting(registration: &oxabl_daemon_protocol::Registration, root: &Path) -> String {
    format!(
        "{}; another oxabl of that build keeps starting a daemon for this root",
        mismatch_message(registration, root)
    )
}

/// A daemon of another build, with both builds named.
#[cfg(unix)]
fn mismatch_message(registration: &oxabl_daemon_protocol::Registration, root: &Path) -> String {
    let version = if registration.daemon_version.is_empty() {
        // A registration without the field predates it, so the daemon is older, but
        // its version is not known.
        "an unknown, older version"
    } else {
        registration.daemon_version.as_str()
    };
    let ours = oxabl_daemon::build_identity();
    format!(
        "the oxabl daemon for {} (pid {}) is {version}{} with contract {}{}, but this client \
         is {}{} with contract {}",
        root.display(),
        registration.pid,
        build_suffix(&registration.build),
        registration.contract_version,
        if oxabl_daemon::executable_replaced(registration.pid) {
            " and its executable was replaced on disk"
        } else {
            ""
        },
        oxabl_daemon::build_version(),
        build_suffix(&ours.id),
        oxabl_daemon_protocol::CONTRACT_VERSION,
    )
}

/// ` (build <id>)`, or nothing when the build is not known.
#[cfg(unix)]
fn build_suffix(build: &str) -> String {
    if build.is_empty() {
        String::new()
    } else {
        format!(" (build {build})")
    }
}

/// Forward frames between the editor and the daemon until the editor sends `exit` or
/// either side closes.
///
/// Each direction has its own thread, so a frame that waits to reach one side never
/// stops the frames for the other. With one thread, a send to a daemon that is
/// itself waiting to write to this proxy deadlocks both processes once both socket
/// buffers are full.
///
/// The daemon is dropped before the function returns, so its writer ends; the caller
/// then shuts the socket down, which ends the daemon-to-editor thread.
#[cfg(unix)]
fn proxy_messages(
    editor: &Connection,
    daemon: Connection,
    daemon_pid: u32,
    over_limit: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> Result<()> {
    let Connection {
        sender: to_daemon,
        receiver: from_daemon,
    } = daemon;
    let (ended_tx, ended_rx) = crossbeam_channel::bounded::<Result<()>>(1);
    let to_editor = editor.sender.clone();
    let gone = over_limit.clone();
    std::thread::Builder::new()
        .name("oxabl-lsp-to-editor".to_string())
        .spawn(move || {
            let outcome = (|| loop {
                let Ok(message) = from_daemon.recv() else {
                    return Err(daemon_gone(daemon_pid, &gone));
                };
                to_editor
                    .send(with_null_result(message))
                    .context("the editor connection closed")?;
            })();
            let _ = ended_tx.send(outcome);
        })
        .context("starting the proxy thread")?;
    loop {
        crossbeam_channel::select! {
            recv(editor.receiver) -> message => {
                let message = message.context("the editor connection closed")?;
                let exiting = matches!(&message, Message::Notification(notification) if notification.method == "exit");
                if to_daemon.send(message).is_err() {
                    return Err(daemon_gone(daemon_pid, &over_limit));
                }
                if exiting {
                    return Ok(());
                }
            }
            recv(ended_rx) -> outcome => {
                return outcome.unwrap_or_else(|_| Err(anyhow::anyhow!("the proxy thread ended")));
            }
        }
    }
}

/// The error for a daemon connection that ended during the editor session: this
/// process closed it, the daemon exited, or it is still running and closed it.
///
/// A daemon that exits closes its sockets before the process is gone, so the process
/// gets a short time to go. A zombie (an exited child not yet reaped) has exited.
#[cfg(unix)]
fn daemon_gone(pid: u32, over_limit: &std::sync::atomic::AtomicBool) -> anyhow::Error {
    if over_limit.load(std::sync::atomic::Ordering::SeqCst) {
        return anyhow::anyhow!(
            "oxabl lsp closed its connection to the oxabl daemon (pid {pid}) because frames \
             waited too long in a queue; the line above on stderr says which"
        );
    }
    let running = || {
        oxabl_daemon::registry::process_is_alive(pid)
            && !std::fs::read_to_string(format!("/proc/{pid}/stat"))
                .ok()
                .and_then(|stat| {
                    stat.rsplit_once(')')
                        .map(|(_, rest)| rest.trim_start().starts_with('Z'))
                })
                .unwrap_or(false)
    };
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(200);
    let mut alive = running();
    while alive && std::time::Instant::now() < deadline {
        std::thread::sleep(std::time::Duration::from_millis(20));
        alive = running();
    }
    if alive {
        anyhow::anyhow!(
            "the oxabl daemon (pid {pid}) closed the connection to this editor during the \
             session; the daemon's stderr may say why"
        )
    } else {
        anyhow::anyhow!("the daemon exited during the editor session (pid {pid})")
    }
}

/// A response read back from the daemon with its `null` result restored.
///
/// `lsp_server` reads `"result": null` as no result and then writes the response
/// with neither `result` nor `error`, which a client rejects. A response without
/// an error always had a result, so the missing one was `null`.
fn with_null_result(message: Message) -> Message {
    match message {
        Message::Response(mut response)
            if response.result.is_none() && response.error.is_none() =>
        {
            response.result = Some(serde_json::Value::Null);
            Message::Response(response)
        }
        message => message,
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
    let host = SessionHost::new();
    let session_root = anchor_or_current_dir(workspace_root.clone());
    serve_initialized(
        connection,
        debounce_window,
        encoding,
        workspace_root,
        session_root,
        host,
        false,
    )
}

/// Serve an LSP socket after the daemon's protocol router consumed its initialize
/// request. The supplied host is the daemon's shared per-workspace session map.
///
/// ## Why the bound root is checked here rather than only at `oxabl/handshake`
///
/// `initialize` is a second entrance onto the same socket, and it carries a
/// client-chosen workspace root just as the handshake does. Checking only the
/// handshake would leave the rule unenforced on the route editors actually take:
/// a client could connect to a daemon bound to one project, name a home directory
/// or the filesystem root, and drive a full recursive pass over that tree — which
/// every other client of the shared daemon pays for in memory and latency, and
/// which reads that tree's contents into the daemon's index (R26).
///
/// The declared root is resolved before it keys anything, so one tree is one
/// session however the client spelled it (R2) — `Sessions` keys lexically and
/// trusts its callers to have resolved first — and a root that is not on disk is
/// refused rather than keyed on and answered about later.
///
/// ## Why an unbound host still serves whatever root arrives
///
/// An unbound host is the in-process server: `oxabl lsp` and the tests, where the
/// client and the server are the same program and there is no untrusted spelling
/// to constrain. Refusing there would break ordinary editor integration, which
/// declares its own root by design.
pub fn serve_with_first(
    connection: &Connection,
    debounce_window: std::time::Duration,
    first: Request,
    host: SessionHost,
) -> Result<bool> {
    let init: InitializeParams =
        serde_json::from_value(first.params.clone()).context("deserializing InitializeParams")?;
    let declared = declared_workspace_root(&init);
    let requested = anchor_or_current_dir(declared.clone());

    let mut workspace_root = declared;
    let session_root = match host.bound_root() {
        Some(bound) => {
            let resolved = match oxabl_daemon::canonical_root(&requested) {
                Ok(resolved) => resolved,
                Err(error) => {
                    return Err(refuse_initialize(
                        connection,
                        first.id,
                        format!(
                            "workspace root {} cannot be resolved: {error}",
                            requested.display()
                        ),
                    ));
                }
            };
            if resolved != bound {
                return Err(refuse_initialize(
                    connection,
                    first.id,
                    format!(
                        "this daemon serves {}, not {}",
                        bound.display(),
                        resolved.display()
                    ),
                ));
            }
            // The anchor follows the key, so the configuration and the file watch
            // describe the tree the session is actually keyed on.
            if workspace_root.is_some() {
                workspace_root = Some(resolved.clone());
            }
            resolved
        }
        None => requested,
    };

    host.with(|sessions| sessions.for_root(&session_root).attach(true));
    let (encoding, negotiated_root) = match finish_handshake(connection, first.id, first.params) {
        Ok(handshake) => handshake,
        Err(error) => {
            host.with(|sessions| sessions.for_root(&session_root).detach(true));
            return Err(error);
        }
    };
    // A bound host has already resolved the anchor above; an unbound one keeps the
    // spelling the client sent.
    if host.bound_root().is_none() {
        workspace_root = negotiated_root;
    }
    serve_initialized(
        connection,
        debounce_window,
        encoding,
        workspace_root,
        session_root,
        host,
        true,
    )
}

/// Answer an `initialize` this server will not serve, in the one way the LSP
/// transport has: an error response to the request itself (R26).
///
/// The code is JSON-RPC `InvalidRequest`, the same code the daemon's handshake
/// returns for the same refusal, and the message repeats its wording — a client
/// that reaches the daemon by either door reads the same sentence. The `Err` this
/// returns ends the connection: there is no session to serve on it.
fn refuse_initialize(connection: &Connection, id: RequestId, detail: String) -> anyhow::Error {
    let message = format!("oxabl: invalid request: {detail}");
    if let Err(error) = connection.sender.send(Message::Response(Response::new_err(
        id,
        ErrorCode::InvalidRequest as i32,
        message.clone(),
    ))) {
        return anyhow::Error::new(error).context(message);
    }
    anyhow::anyhow!(message)
}

/// The root a session keys on when the client declared one, else the working
/// directory — the anchor the server has always fallen back to.
fn anchor_or_current_dir(declared: Option<PathBuf>) -> PathBuf {
    declared.unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
}

/// What the handshake learned from the client's capabilities.
struct Negotiated {
    options: code_lens::Options,
    /// What the client takes in a completion item.
    completion: completion::Options,
    inlay: inlay_hint::Options,
    semantic: semantic_tokens::Options,
    /// The `outline` setting.
    outline: outline_includes::Settings,
    /// Whether the client takes `DocumentSymbol[]` rather than a flat list.
    hierarchical_symbols: bool,
    /// Whether the client takes `textDocument/inactiveRegions`.
    inactive_regions: bool,
    /// Whether the client takes `relatedInformation` on a diagnostic.
    related_information: bool,
    /// The most folding ranges the client takes (`rangeLimit`), if it says.
    folding_range_limit: Option<usize>,
}

fn serve_initialized(
    connection: &Connection,
    debounce_window: std::time::Duration,
    options: Negotiated,
    workspace_root: Option<PathBuf>,
    session_root: PathBuf,
    host: SessionHost,
    already_attached: bool,
) -> Result<bool> {
    let mut server = Server::new(
        connection,
        options,
        debounce_window,
        workspace_root,
        session_root,
        host,
    );
    if !already_attached {
        server.host.with(|sessions| {
            sessions.for_root(&server.session_root).attach(true);
        });
    }
    let result = server.main_loop();
    server.detach();
    result
}

/// Perform the initialize handshake, negotiating the position encoding from the
/// client's advertised `general.positionEncodings` before replying with the v1
/// server capabilities (R2, R3).
///
/// Also returns the client's declared **workspace root**, when it declared one.
/// That is what lets the server's watch and its configuration cover a *project*
/// rather than whichever directory happened to hold the first opened document.
fn handshake(connection: &Connection) -> Result<(Negotiated, Option<PathBuf>)> {
    let (id, params) = connection
        .initialize_start()
        .context("LSP initialize_start")?;
    finish_handshake(connection, id, params)
}

fn finish_handshake(
    connection: &Connection,
    id: RequestId,
    params: serde_json::Value,
) -> Result<(Negotiated, Option<PathBuf>)> {
    let inactive_regions = inactive_regions::client_supports(&params);
    let init: InitializeParams =
        serde_json::from_value(params).context("deserializing InitializeParams")?;

    let encoding = negotiate_position_encoding(&init.capabilities);
    let workspace_root = declared_workspace_root(&init);
    let options = code_lens::Options::from_client(
        &init.capabilities,
        encoding.clone(),
        init.initialization_options.as_ref(),
    );
    let inlay =
        inlay_hint::Options::from_client(&init.capabilities, init.initialization_options.as_ref());
    let semantic = semantic_tokens::Options::from_client(
        &init.capabilities,
        init.initialization_options.as_ref(),
    );
    let hierarchical_symbols = init
        .capabilities
        .text_document
        .as_ref()
        .and_then(|c| c.document_symbol.as_ref())
        .and_then(|c| c.hierarchical_document_symbol_support)
        .unwrap_or(false);
    let related_information = init
        .capabilities
        .text_document
        .as_ref()
        .and_then(|c| c.publish_diagnostics.as_ref())
        .and_then(|c| c.related_information)
        .unwrap_or(false);
    let folding_range_limit = init
        .capabilities
        .text_document
        .as_ref()
        .and_then(|c| c.folding_range.as_ref())
        .and_then(|c| c.range_limit)
        .map(|limit| limit as usize);
    let mut capabilities = server_capabilities(encoding);
    if options.supported
        && !options.dynamic
        && (options.reference_lenses
            || workspace_root.as_ref().is_some_and(|root| {
                let (config, _) =
                    PipelineConfig::resolve_for_editor(root, &ConfigOverrides::default());
                let fs = oxabl_workspace::RealFileSystem;
                oxabl_pipeline::LintPipeline::new(&config, &fs)
                    .with_file(root)
                    .effect_lenses_enabled()
            }))
    {
        capabilities.code_lens_provider = Some(lsp_types::CodeLensOptions {
            resolve_provider: Some(options.reference_lenses),
        });
    }
    if inlay.supported {
        capabilities.inlay_hint_provider = Some(lsp_types::OneOf::Left(true));
    }
    if semantic.supported {
        capabilities.semantic_tokens_provider = Some(semantic_tokens::capability());
    }
    let result = InitializeResult {
        capabilities,
        server_info: Some(ServerInfo {
            name: "oxabl-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    let mut result = serde_json::to_value(result)?;
    // Not a field of the LSP types: the name clangd gives the capability.
    result["capabilities"]["inactiveRegionsProvider"] = serde_json::Value::Bool(true);
    // `lsp_types` has no field for it.
    result["capabilities"]["typeHierarchyProvider"] = serde_json::Value::Bool(true);
    // The options type has no `rangesSupport` field either.
    result["capabilities"]["documentRangeFormattingProvider"] = range_formatting::capability();
    connection
        .initialize_finish(id, result)
        .context("LSP initialize_finish")?;
    Ok((
        Negotiated {
            options,
            completion: completion::Options::from_client(&init.capabilities),
            inlay,
            semantic,
            outline: outline_includes::Settings::from_init(init.initialization_options.as_ref()),
            hierarchical_symbols,
            inactive_regions,
            related_information,
            folding_range_limit,
        },
        workspace_root,
    ))
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
    /// Whether the client takes `DocumentSymbol[]` rather than a flat list.
    hierarchical_symbols: bool,
    /// Whether the client takes `textDocument/inactiveRegions`.
    inactive_regions: bool,
    /// Whether the client takes `relatedInformation` on a diagnostic.
    related_information: bool,
    /// The most folding ranges the client takes, if it says.
    folding_range_limit: Option<usize>,
    /// Open buffers, keyed by URI (R4).
    documents: DocumentStore,
    /// The daemon's one session map. No analysis state lives in this client.
    host: SessionHost,
    /// The session this LSP connection shares with every other client.
    ///
    /// Resolved by the caller, not here: on a bound daemon socket
    /// [`serve_with_first`] canonicalises the declared root before it keys
    /// anything, because [`oxabl_daemon`]'s session map keys lexically and one
    /// tree reached by two spellings must still be one session (R2).
    session_root: PathBuf,
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
    /// The `[workspace.features]` of the installed configuration, which counts
    /// as both off while `oxabl.toml` cannot be read and when there is none.
    /// Both off until a configuration is resolved.
    features: FeaturesConfig,
    /// Per-URI include dependency paths (for watcher `*.i` matching, R17).
    dependencies: HashMap<Uri, Vec<PathBuf>>,
    lenses: code_lens::LensCache,
    /// The resolved titles of the reference-count lenses.
    reference_cache: reference_lens::ResolveCache,
    /// When to ask the client for the reference lenses again.
    lens_refresh: reference_lens::Refresh,
    /// The client's inlay hint support and the kinds it wants.
    inlay: inlay_hint::Options,
    /// The client's semantic token support and the user's choice.
    semantic: semantic_tokens::Options,
    /// The `outline` setting.
    outline_settings: outline_includes::Settings,
    /// The last outline of each open buffer, for its version.
    outlines: document_symbol::OutlineCache,
    /// The folding and selection facts of each open buffer, for its version.
    syntax: editor_syntax::SyntaxCache,
    /// Per-URI debounce timers (R13).
    debouncer: crate::debounce::Debouncer,
    /// The selected compilation context of each open include.
    contexts: includer_context::Contexts,
    /// The highlight requests of each open document, so one runs at a time.
    highlight_turns: navigation::HighlightTurns,
    /// Where context workers report, and where the main loop reads them.
    context_events: crossbeam_channel::Sender<includer_context::ContextEvent>,
    context_results: crossbeam_channel::Receiver<includer_context::ContextEvent>,
    /// Hover requests, one at a time, the stale ones dropped.
    hovers: navigation::HoverGate,
    /// What the client takes in a completion item.
    completion: completion::Options,
    /// The completion requests of each open document, one at a time.
    completions: completion::Gates,
    /// The references and call hierarchy requests in flight, and the gaps
    /// already logged.
    references: references::InFlight,
    /// The generation of the newest `oxabl/schema` request; an older one stops.
    schema_requests: std::sync::Arc<std::sync::atomic::AtomicU64>,
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
    /// The `&IF` branches the buffer does not compile, from the same analysis.
    /// `(start, end)` byte offsets.
    inactive_regions: Option<Vec<(u32, u32)>>,
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
        negotiated: Negotiated,
        debounce_window: std::time::Duration,
        workspace_root: Option<PathBuf>,
        session_root: PathBuf,
        host: SessionHost,
    ) -> Self {
        let Negotiated {
            options,
            completion,
            inlay,
            semantic,
            outline,
            hierarchical_symbols,
            inactive_regions,
            related_information,
            folding_range_limit,
        } = negotiated;
        let formatter = FormatPipeline::new(oxabl_style::StyleGuide::default_base());
        let (context_events, context_results) = crossbeam_channel::unbounded();
        Server {
            connection,
            encoding: options.encoding.clone(),
            hierarchical_symbols,
            inactive_regions,
            related_information,
            folding_range_limit,
            documents: DocumentStore::new(),
            host,
            session_root,
            config_resolved: false,
            workspace_anchor: None,
            workspace_root,
            formatter,
            // Off until a resolved configuration says otherwise.
            features: FeaturesConfig::OFF,
            dependencies: HashMap::new(),
            lenses: code_lens::LensCache::new(options),
            reference_cache: reference_lens::ResolveCache::default(),
            lens_refresh: reference_lens::Refresh::new(debounce_window),
            inlay,
            semantic,
            outline_settings: outline,
            outlines: document_symbol::OutlineCache::default(),
            syntax: editor_syntax::SyntaxCache::default(),
            debouncer: crate::debounce::Debouncer::new(debounce_window),
            contexts: includer_context::Contexts::default(),
            highlight_turns: navigation::HighlightTurns::default(),
            context_events,
            context_results,
            hovers: navigation::HoverGate::default(),
            completion,
            completions: completion::Gates::default(),
            references: references::InFlight::default(),
            schema_requests: Default::default(),
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

        let (lens_tx, lens_rx) = crossbeam_channel::unbounded();
        let context_rx = self.context_results.clone();
        let mut shutdown_received = false;
        loop {
            // Size the timeout to the nearest pending debounce deadline.
            let timer = match [self.debouncer.next_deadline(), self.lens_refresh.due()]
                .into_iter()
                .flatten()
                .min()
            {
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
                            } else if req.method == "textDocument/codeLens" {
                                self.handle_code_lens(req, &lens_tx)?;
                            } else if req.method == "codeLens/resolve" {
                                self.handle_code_lens_resolve(req)?;
                            } else if req.method == InlayHintRequest::METHOD {
                                self.handle_inlay_hint(req)?;
                            } else if req.method == SemanticTokensFullRequest::METHOD {
                                self.handle_semantic_tokens_full(req)?;
                            } else if req.method == SemanticTokensFullDeltaRequest::METHOD {
                                self.handle_semantic_tokens_delta(req)?;
                            } else if req.method == SemanticTokensRangeRequest::METHOD {
                                self.handle_semantic_tokens_range(req)?;
                            } else if req.method == Formatting::METHOD {
                                self.handle_formatting(req)?;
                            } else if req.method == RangeFormatting::METHOD {
                                self.handle_range_formatting(req, false)?;
                            } else if req.method == range_formatting::RANGES_METHOD {
                                self.handle_range_formatting(req, true)?;
                            } else if req.method == DocumentSymbolRequest::METHOD {
                                self.handle_document_symbol(req)?;
                            } else if req.method == FoldingRangeRequest::METHOD {
                                self.handle_folding_range(req)?;
                            } else if req.method == SelectionRangeRequest::METHOD {
                                self.handle_selection_range(req)?;
                            } else if req.method == GotoDefinition::METHOD {
                                self.handle_definition(req)?;
                            } else if req.method == HoverRequest::METHOD {
                                self.handle_hover(req)?;
                            } else if req.method == Completion::METHOD {
                                self.handle_completion(req)?;
                            } else if req.method == ResolveCompletionItem::METHOD {
                                self.handle_completion_resolve(req)?;
                            } else if req.method == SignatureHelpRequest::METHOD {
                                self.handle_signature_help(req)?;
                            } else if req.method == DocumentHighlightRequest::METHOD {
                                self.handle_document_highlight(req)?;
                            } else if req.method == DocumentLinkRequest::METHOD {
                                self.handle_document_link(req)?;
                            } else if req.method == References::METHOD {
                                self.handle_references(req)?;
                            } else if req.method == CallHierarchyPrepare::METHOD {
                                self.handle_prepare_call_hierarchy(req)?;
                            } else if req.method == CallHierarchyIncomingCalls::METHOD {
                                self.handle_incoming_calls(req)?;
                            } else if req.method == CallHierarchyOutgoingCalls::METHOD {
                                self.handle_outgoing_calls(req)?;
                            } else if req.method == GotoImplementation::METHOD {
                                self.handle_implementation(req)?;
                            } else if req.method == TypeHierarchyPrepare::METHOD {
                                self.handle_prepare_type_hierarchy(req)?;
                            } else if req.method == TypeHierarchySupertypes::METHOD {
                                self.handle_type_supertypes(req)?;
                            } else if req.method == TypeHierarchySubtypes::METHOD {
                                self.handle_type_subtypes(req)?;
                            } else if req.method == hierarchy::SUPER_METHOD {
                                self.handle_super_method(req)?;
                            } else if req.method == expanded_unit::METHOD {
                                self.handle_expanded_unit(req)?;
                            } else if req.method == includer_context::LIST_METHOD {
                                self.handle_includer_contexts(req)?;
                            } else if req.method == includer_context::SELECT_METHOD {
                                self.handle_select_includer_context(req)?;
                            } else if req.method == schema::METHOD {
                                self.handle_schema(req)?;
                            } else if req.method == schema::TABLE_METHOD {
                                self.handle_schema_table(req)?;
                            } else if req.method == WorkspaceSymbolRequest::METHOD {
                                workspace_symbol::spawn(
                                    req,
                                    self.host.clone(),
                                    self.session_root.clone(),
                                    self.encoding.clone(),
                                    self.connection.sender.clone(),
                                );
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
                recv(lens_rx) -> res => {
                    if let Ok(res) = res && self.lenses.complete(res) {
                        self.connection.sender.send(Message::Request(Request::new(
                            self.lenses.request_id(),
                            "workspace/codeLens/refresh".into(), serde_json::Value::Null,
                        )))?;
                    }
                }
                recv(context_rx) -> event => {
                    if let Ok(event) = event {
                        self.handle_context_event(event)?;
                    }
                }
                recv(result_rx) -> res => {
                    if let Ok(res) = res {
                        self.handle_result(res);
                    }
                }
                recv(timer) -> _ => {
                    self.fire_due_debounces(&result_tx);
                    self.fire_lens_refresh();
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
        let mut context_due = false;
        let mut include_due = false;
        for uri in self.debouncer.take_due(now) {
            let Some(doc) = self.documents.get(&uri) else {
                continue;
            };
            let version = doc.version;
            // Another open file reads this one: its tokens change with it.
            include_due = include_due
                || uri_to_path(&uri).is_some_and(|path| self.include_read_by_another(&path, &uri));
            if !self.features.diagnostics {
                self.publish_inactive_regions(&uri, version, []);
                self.publish(uri, Vec::new(), Some(version));
                continue;
            }
            if let Some(root) = self.contexts.selected(&uri).cloned() {
                context_due = true;
                self.spawn_context_analysis(uri, version, root);
                continue;
            }
            let Some((buffer, snapshot, schema, config_generation)) = self.host.with(|sessions| {
                let session = sessions.get(&self.session_root)?;
                Some((
                    session.buffer(uri.as_str())?,
                    session.database().clone(),
                    session.schema_handle(),
                    session.config_generation(),
                ))
            }) else {
                continue;
            };
            let tx = result_tx.clone();
            oxabl_common::stack::builder()
                .spawn(move || {
                    // The guard is inside `analyze_guarded`, so the send below is
                    // reached on every path — a panic must not leave this buffer
                    // waiting forever on a result that never arrives (R8).
                    let analysis = analyze_guarded(&snapshot, buffer, schema, uri.as_str());
                    let _ = tx.send(ComputeResult {
                        uri,
                        version,
                        diagnostics: analysis.diagnostics,
                        dependencies: analysis.dependencies,
                        inactive_regions: analysis.inactive_regions.map(|regions| {
                            regions.iter().map(|span| (span.start, span.end)).collect()
                        }),
                        panicked: analysis.panicked,
                        config_generation,
                    });
                })
                .expect("failed to spawn an analysis thread");
        }
        // The hints of an include read in a root change with the root: ask
        // the client again once the edits settle, not on every keystroke.
        if context_due {
            self.refresh_inlay_hints();
        }
        // The same for the tokens of a file that reads an include with edits
        // that are not saved.
        if include_due && !context_due {
            self.refresh_semantic_tokens();
        }
    }

    /// Publish a completed computation, or drop it, or re-arm it — whichever
    /// [`oxabl_daemon::dispose`] says.
    ///
    /// The decision itself lives in the session core, because it is the same
    /// decision for every client and two copies of it would drift into publishing a
    /// stale answer. What stays here is the LSP-specific part: reading the buffer
    /// version out of the document store, recording the dependency set, and
    /// rendering diagnostics through the negotiated position encoding.
    ///
    /// The dependency set is recorded **before** the disposition is honoured, and
    /// only for a current-generation result: a cancelled or panicked computation
    /// carries `None` there, which `record_dependencies` keeps rather than committing
    /// as an empty set. An empty set for a buffer that does have includes would stop
    /// the watcher from ever re-triggering it.
    fn handle_result(&mut self, res: ComputeResult) {
        // An include analysed in a selected root publishes that root's answer
        // only; its fragment analysis must not replace it.
        if self.contexts.has_selection(&res.uri) {
            return;
        }
        let work = CompletedWork {
            read_version: res.version,
            current_version: self.documents.get(&res.uri).map(|doc| doc.version),
            read_generation: res.config_generation,
            current_generation: self.config_generation(),
            has_diagnostics: res.diagnostics.is_some(),
            panicked: res.panicked,
        };
        let current = work.current_version == Some(work.read_version);
        match dispose(work) {
            Disposition::Drop => {
                // A panic gives this version no answer and none follows, so the
                // regions of older text must not stand for it.
                if res.panicked && current {
                    self.publish_inactive_regions(&res.uri, res.version, []);
                }
            }
            Disposition::Retry => {
                self.debouncer.schedule(res.uri, std::time::Instant::now());
            }
            Disposition::Publish => {
                self.record_dependencies(&res.uri, res.dependencies);
                let Some(collected) = res.diagnostics else {
                    // Unreachable: `Publish` is only returned for a result that has
                    // diagnostics. Handled rather than unwrapped so a change to the
                    // disposition rules degrades to publishing nothing instead of
                    // taking down the loop.
                    return;
                };
                let Some(doc) = self.documents.get(&res.uri) else {
                    return;
                };
                let related = self.related_information.then_some(&res.uri);
                let lsp_diags = to_lsp_diagnostics(&collected, &doc.rope, &self.encoding, related);
                // No regions from this analysis clear the old ones, which belong
                // to other text.
                let regions = res.inactive_regions.unwrap_or_default();
                self.publish_inactive_regions(&res.uri, res.version, regions);
                self.publish(res.uri.clone(), lsp_diags, Some(res.version));
            }
        }
    }

    /// Dispatch a `textDocument/*` notification. Opens publish immediately (for
    /// instant feedback); edits mirror into the salsa input on the main thread
    /// and schedule a debounced recompute (R13). Unknown notifications are
    /// ignored (per LSP, servers must not error on them).
    fn handle_notification(&mut self, not: Notification) {
        if matches!(
            not.method.as_str(),
            "textDocument/didOpen"
                | "textDocument/didChange"
                | "textDocument/didSave"
                | "textDocument/didClose"
                | "workspace/didChangeWatchedFiles"
        ) {
            self.lenses.invalidate();
        }
        match not.method.as_str() {
            lsp_types::notification::Cancel::METHOD => {
                if let Ok(params) = serde_json::from_value::<lsp_types::CancelParams>(not.params) {
                    let id = match params.id {
                        lsp_types::NumberOrString::Number(n) => RequestId::from(n),
                        lsp_types::NumberOrString::String(s) => RequestId::from(s),
                    };
                    self.hovers.cancel(&id);
                    self.cancel_completion(&id);
                    self.references.cancel(&id);
                    self.outlines.requests.cancel(&id);
                }
            }
            DidOpenTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidOpenTextDocumentParams>(not.params)
                {
                    let doc = params.text_document;
                    oxabl_common::hold_if_injected(
                        oxabl_common::panic_sites::LSP_DID_OPEN,
                        &doc.text,
                    );
                    self.ensure_config(&doc.uri);
                    self.outlines.invalidate(&doc.uri);
                    self.note_inlay_version(&doc.uri, Some(doc.version));
                    self.note_semantic_version(&doc.uri, Some(doc.version));
                    self.syntax.invalidate(&doc.uri);
                    self.documents.open(doc.uri.clone(), doc.version, &doc.text);
                    // The path travels with the buffer because the *query* needs
                    // it: cross-file resolution must exclude this file from its
                    // own lookups, since the buffer is the unsaved text while
                    // the index answers from disk.
                    let path = uri_to_path(&doc.uri);
                    self.host.with(|sessions| {
                        sessions.for_root(&self.session_root).set_buffer(
                            doc.uri.as_str(),
                            doc.text,
                            path,
                        );
                    });
                    // An include analysed in this root now reads the buffer.
                    if let Some(path) = uri_to_path(&doc.uri) {
                        self.schedule_context_users(&path);
                    }
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
                    self.outlines.invalidate(&uri);
                    self.syntax.invalidate(&uri);
                    self.documents.change(
                        &uri,
                        params.text_document.version,
                        &params.content_changes,
                        &encoding,
                    );
                    self.note_inlay_version(&uri, Some(params.text_document.version));
                    self.note_semantic_version(&uri, Some(params.text_document.version));
                    // Mirror the new text into the salsa input (write on main).
                    // This cancels any in-flight snapshot read for a superseded
                    // version via salsa's `Cancelled` unwind (KTD7).
                    if let Some(doc) = self.documents.get(&uri) {
                        let text = doc.text();
                        self.host.with(|sessions| {
                            sessions.for_root(&self.session_root).set_buffer(
                                uri.as_str(),
                                text,
                                uri_to_path(&uri),
                            );
                        });
                    }
                    // Collapse the edit burst to a single debounced recompute.
                    if let Some(path) = uri_to_path(&uri) {
                        self.schedule_context_users(&path);
                    }
                    self.schedule_lens_refresh_for_edit(&uri);
                    self.debouncer.schedule(uri, std::time::Instant::now());
                }
            }
            DidSaveTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidSaveTextDocumentParams>(not.params)
                {
                    // Recompute on save so on-disk include changes are picked up.
                    self.schedule_lens_refresh();
                    self.outlines.invalidate_includes();
                    self.debouncer
                        .schedule(params.text_document.uri, std::time::Instant::now());
                }
            }
            DidChangeWatchedFiles::METHOD => {
                if let Ok(params) =
                    serde_json::from_value::<DidChangeWatchedFilesParams>(not.params)
                {
                    self.handle_watched_files(params);
                    self.schedule_lens_refresh();
                    self.inlay_disk_changed();
                }
            }
            "workspace/didChangeConfiguration" => {
                self.handle_semantic_tokens_settings(&not.params);
                self.handle_reference_lens_settings(&not.params);
                self.handle_outline_settings(&not.params);
                self.handle_configuration_change(not.params);
            }
            DidCloseTextDocument::METHOD => {
                if let Ok(params) = serde_json::from_value::<DidCloseTextDocumentParams>(not.params)
                {
                    let uri = params.text_document.uri;
                    self.documents.close(&uri);
                    self.contexts.forget(&uri);
                    self.highlight_turns.remove(&uri);
                    self.completions.remove(&uri);
                    self.outlines.invalidate(&uri);
                    self.syntax.invalidate(&uri);
                    self.host.with(|sessions| {
                        sessions
                            .for_root(&self.session_root)
                            .close_buffer(uri.as_str());
                    });
                    self.dependencies.remove(&uri);
                    self.note_inlay_version(&uri, None);
                    self.note_semantic_version(&uri, None);
                    self.debouncer.cancel(&uri);
                    self.lens_refresh.forget(&uri);
                    // An include analysed in this root now reads its saved copy.
                    if let Some(path) = uri_to_path(&uri) {
                        self.schedule_context_users(&path);
                    }
                    // Clear the client's diagnostics for the closed buffer.
                    self.publish(uri, Vec::new(), None);
                }
            }
            _ => {}
        }
        self.update_lens_registration();
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
        let (resolved, warnings) =
            PipelineConfig::resolve_for_editor(path, &ConfigOverrides::default());
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
        self.outlines.invalidate_includes();
        self.formatter = FormatPipeline::new(resolved.style.clone());
        self.features = resolved.features;
        self.host.with(|sessions| {
            sessions
                .for_root(&self.session_root)
                .install_config(resolved);
        });
    }

    /// Surface configuration warnings on the client's log (R7).
    ///
    /// `window/logMessage` rather than a published diagnostic: these problems
    /// belong to `oxabl.toml` or a `.df`, not to the open buffer, so attaching
    /// them to a span in the ABL file the user happens to have focused would put
    /// a squiggle on innocent code.
    ///
    /// A rejected `oxabl.toml` also gets one `window/showMessage`: the features
    /// count as off until it is fixed, and a log line alone would leave the user
    /// to wonder why the editor went quiet.
    fn report_config_warnings(&self, warnings: &[ConfigWarning]) {
        for warning in warnings {
            self.log_warning(&format!("oxabl-lsp: {warning}"));
            if matches!(warning, ConfigWarning::Config(_)) {
                self.show_warning(&format!(
                    "oxabl-lsp: {warning}. Diagnostics and formatting are off until \
                     oxabl.toml is fixed."
                ));
            }
        }
    }

    /// Send a `window/showMessage` at warning level.
    fn show_warning(&self, message: &str) {
        let notification = lsp_server::Notification {
            method: lsp_types::notification::ShowMessage::METHOD.to_string(),
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
        let Some(version) = self.documents.get(uri).map(|d| d.version) else {
            return;
        };
        if !self.features.diagnostics {
            self.publish_inactive_regions(uri, version, []);
            self.publish(uri.clone(), Vec::new(), Some(version));
            return;
        }
        if self.contexts.has_selection(uri) {
            self.debouncer
                .schedule(uri.clone(), std::time::Instant::now());
            return;
        }
        let Some((buffer, snapshot, schema)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.buffer(uri.as_str())?,
                session.database().clone(),
                session.schema_handle(),
            ))
        }) else {
            return;
        };
        // Guarded (R8): this runs on the main loop, so an unguarded panic here
        // takes the whole server down.
        let analysis = analyze_guarded(&snapshot, buffer, schema, uri.as_str());
        self.record_dependencies(uri, analysis.dependencies);
        let Some(collected) = analysis.diagnostics else {
            // A cancellation is not expected on this path (nothing writes to the
            // db while the main loop is inside this call), but if one ever does
            // arrive the open would otherwise publish nothing at all and no timer
            // would remain — the same stale-forever shape as in `handle_result`.
            if analysis.panicked {
                self.publish_inactive_regions(uri, version, []);
            } else {
                self.debouncer
                    .schedule(uri.clone(), std::time::Instant::now());
            }
            return;
        };
        if let Some(doc) = self.documents.get(uri) {
            let related = self.related_information.then_some(uri);
            let lsp_diags = to_lsp_diagnostics(&collected, &doc.rope, &self.encoding, related);
            let regions = analysis.inactive_regions.as_deref().unwrap_or_default();
            let regions = regions.iter().map(|span| (span.start, span.end));
            self.publish_inactive_regions(uri, version, regions);
            self.publish(uri.clone(), lsp_diags, Some(version));
        }
    }

    /// React to `workspace/didChangeWatchedFiles`: `.df` changes hot-reload the
    /// schema (R16); `oxabl.toml` changes re-resolve include paths + `[lint]`
    /// config (R17); `*.i` changes re-trigger the buffers that depend on them
    /// (R17); a **root source file** or include change invalidates that one
    /// file's index inputs (R10). Re-triggering bumps the buffer's salsa input so the coarse
    /// expansion memo recomputes and re-reads the changed file.
    fn handle_watched_files(&mut self, params: DidChangeWatchedFilesParams) {
        self.outlines.invalidate_includes();
        let now = std::time::Instant::now();
        let changed: Vec<PathBuf> = params
            .changes
            .iter()
            .filter_map(|event| uri_to_path(&event.uri))
            .collect();
        if let Some(bases) = self
            .host
            .with(|sessions| Some(sessions.get(&self.session_root)?.completion_bases()))
        {
            bases.forget_listings(&changed);
        }
        // A file on disk changed: no session generation says so.
        self.reference_cache.clear();
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

        // A root changed on disk changes the includes analysed in it.
        for source in &changed_sources {
            for uri in self.buffers_depending_on(source) {
                self.debouncer.schedule(uri, now);
            }
        }

        // An include can also be what an indexed file's facts were read through.
        changed_sources.extend(changed_includes);
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
        let any = self.host.with(|sessions| {
            let session = sessions.for_root(&self.session_root);
            let index = std::sync::Arc::clone(&session.database().config().index);
            let mut any = false;
            for path in changed {
                // Both spellings, because a watcher's URI and the path the index
                // search resolved need not agree on symlinks or `.` components — the
                // same reason `buffers_depending_on` canonicalizes.
                any |= index.bump(session.database_mut(), path);
                if let Ok(canonical) = std::fs::canonicalize(path)
                    && canonical != *path
                {
                    any |= index.bump(session.database_mut(), &canonical);
                }
            }
            any
        });
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
        self.host.with(|sessions| {
            sessions.for_root(&self.session_root).bump_schema();
        });
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
        self.documents.uris().cloned().collect()
    }

    fn retrigger_all(&mut self, now: std::time::Instant) {
        for uri in self.open_uris() {
            self.retrigger(&uri, now);
        }
    }

    /// Bump a buffer's salsa input to its current text (invalidating the coarse
    /// expansion memo so it re-reads includes) and schedule a recompute.
    fn retrigger(&mut self, uri: &Uri, now: std::time::Instant) {
        if self.documents.get(uri).is_some() {
            self.host.with(|sessions| {
                sessions
                    .for_root(&self.session_root)
                    .retrigger_buffer(uri.as_str());
            });
        }
        self.debouncer.schedule(uri.clone(), now);
    }

    fn config_generation(&self) -> u64 {
        self.host.with(|sessions| {
            sessions
                .get(&self.session_root)
                .map_or(0, |session| session.config_generation())
        })
    }

    fn detach(&mut self) {
        let keys: Vec<String> = self
            .documents
            .uris()
            .map(|uri| uri.as_str().to_string())
            .collect();
        self.host.with(|sessions| {
            let session = sessions.for_root(&self.session_root);
            for key in keys {
                session.close_buffer(&key);
            }
            session.detach(true);
        });
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
    ///
    /// With `format` off in `[workspace.features]` the answer is `[]`.
    fn handle_formatting(&self, req: Request) -> Result<()> {
        let edits: Vec<TextEdit> = serde_json::from_value::<DocumentFormattingParams>(req.params)
            .ok()
            .filter(|_| self.features.format)
            .and_then(|params| self.documents.get(&params.text_document.uri))
            .map(|doc| {
                crate::formatting::compute_formatting_edits(doc, &self.formatter, &self.encoding)
            })
            .unwrap_or_default();
        self.respond(Response::new_ok(req.id, edits))
    }

    /// `textDocument/documentSymbol`: the outline of the open buffer's raw
    /// text, from the cache when the version matches. An unopened URI gives
    /// `null`; a contained panic is a request failure, never an empty list.
    fn handle_document_symbol(&mut self, req: Request) -> Result<()> {
        let params = match serde_json::from_value::<DocumentSymbolParams>(req.params) {
            Ok(params) => params,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid documentSymbol params: {error}"),
                ));
            }
        };
        let uri = params.text_document.uri;
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(Response::new_ok(req.id, serde_json::Value::Null));
        };
        if self.outline_settings.includes && self.handle_document_symbol_includes(&req.id, &uri) {
            return Ok(());
        }
        let outline = self
            .outlines
            .get_or_compute(&uri, doc.version, || oxabl_pipeline::outline(&doc.text()));
        let response = match outline {
            Ok(symbols) => Response::new_ok(
                req.id,
                document_symbol::to_response(
                    symbols,
                    &uri,
                    &doc.rope,
                    &self.encoding,
                    self.hierarchical_symbols,
                ),
            ),
            Err(panic) => Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                format!("oxabl-lsp: the outline failed: {panic}"),
            ),
        };
        self.respond(response)
    }

    /// `textDocument/foldingRange`: syntax blocks, comments and region markers
    /// of the open buffer's raw text. An unopened URI gives `null`.
    fn handle_folding_range(&mut self, req: Request) -> Result<()> {
        let params = match serde_json::from_value::<FoldingRangeParams>(req.params) {
            Ok(params) => params,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid foldingRange params: {error}"),
                ));
            }
        };
        let uri = params.text_document.uri;
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(Response::new_ok(req.id, serde_json::Value::Null));
        };
        let syntax = self.syntax.get_or_compute(&uri, doc.version, || {
            oxabl_pipeline::document_syntax(&doc.text())
        });
        let response = match syntax {
            Ok(syntax) => Response::new_ok(
                req.id,
                editor_syntax::folding_ranges(
                    syntax,
                    self.folding_range_limit,
                    &doc.rope,
                    &self.encoding,
                ),
            ),
            Err(panic) => Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                format!("oxabl-lsp: folding ranges failed: {panic}"),
            ),
        };
        self.respond(response)
    }

    /// `textDocument/selectionRange`: nested source ranges for every requested
    /// position. An unopened URI gives `null`.
    fn handle_selection_range(&mut self, req: Request) -> Result<()> {
        let params = match serde_json::from_value::<SelectionRangeParams>(req.params) {
            Ok(params) => params,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid selectionRange params: {error}"),
                ));
            }
        };
        let uri = params.text_document.uri;
        let Some(doc) = self.documents.get(&uri) else {
            return self.respond(Response::new_ok(req.id, serde_json::Value::Null));
        };
        let offsets: Vec<u32> = params
            .positions
            .iter()
            .map(|position| {
                crate::position::position_to_byte(&doc.rope, *position, &self.encoding) as u32
            })
            .collect();
        let syntax = self.syntax.get_or_compute(&uri, doc.version, || {
            oxabl_pipeline::document_syntax(&doc.text())
        });
        let response = match syntax {
            Ok(syntax) => Response::new_ok(
                req.id,
                editor_syntax::selection_ranges(syntax, &offsets, &doc.rope, &self.encoding),
            ),
            Err(panic) => Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                format!("oxabl-lsp: selection ranges failed: {panic}"),
            ),
        };
        self.respond(response)
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

/// A `file:` URI for an absolute path, percent-encoding every byte a URI path
/// cannot hold. The inverse of [`uri_to_path`].
pub(crate) fn path_to_uri(path: &Path) -> Option<Uri> {
    let path = path.to_str()?.replace('\\', "/");
    let mut uri = String::with_capacity(path.len() + 8);
    uri.push_str("file://");
    if !path.starts_with('/') {
        uri.push('/');
    }
    for byte in path.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' | b'/' | b':' => {
                uri.push(byte as char)
            }
            _ => uri.push_str(&format!("%{byte:02X}")),
        }
    }
    uri.parse().ok()
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
            Negotiated {
                options: code_lens::Options {
                    encoding: PositionEncodingKind::UTF8,
                    supported: false,
                    dynamic: false,
                    refresh: false,
                    reference_lenses: false,
                },
                completion: completion::Options::default(),
                inlay: inlay_hint::Options::from_client(&Default::default(), None),
                semantic: semantic_tokens::Options::from_client(&Default::default(), None),
                outline: outline_includes::Settings::default(),
                hierarchical_symbols: false,
                inactive_regions: false,
                related_information: false,
                folding_range_limit: None,
            },
            std::time::Duration::from_millis(10),
            None,
            anchor_or_current_dir(None),
            SessionHost::new(),
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
            inactive_regions: None,
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
            inactive_regions: None,
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
            inactive_regions: None,
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

        let stale = server.config_generation();
        server.install_config(PipelineConfig::default());
        assert_ne!(
            stale,
            server.config_generation(),
            "installing a configuration must move the generation"
        );

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: Some(oxabl_analyze::CollectedDiagnostics::default()),
            dependencies: None,
            inactive_regions: None,
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

        let stale = server.config_generation();
        server.install_config(PipelineConfig::default());
        assert_ne!(stale, server.config_generation());

        server.handle_result(ComputeResult {
            uri: uri.clone(),
            version: 1,
            diagnostics: None,
            dependencies: None,
            inactive_regions: None,
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
            inactive_regions: None,
            panicked: false,
            config_generation: server.config_generation(),
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
            inactive_regions: None,
            panicked: false,
            config_generation: 0, // no configuration is resolved in this test
        });

        assert!(server.debouncer.next_deadline().is_none());
    }

    /// Discovery has an answer that means "I do not know", and this is what the client
    /// owes it: keep polling, and when the attempts run out, report the cause.
    ///
    /// Two failures used to share this path. A refused registration directory read as
    /// absent, so the client started a rival daemon that walked the same directory and
    /// refused it identically; and whatever the reason, the message the user finally saw
    /// named a two-second timeout — sending them to look at a slow machine when the
    /// answer was a mode bit on a directory the error never mentioned.
    ///
    /// The wait is the real one, so the loop's give-up path is exercised as it ships.
    #[cfg(unix)]
    #[test]
    fn a_client_that_cannot_read_the_registration_gives_up_naming_the_cause() {
        use std::os::unix::fs::PermissionsExt;

        // The base-directory variables are process-wide, so this serialises against
        // itself; no other test in this binary reads them.
        static ENVIRONMENT: std::sync::Mutex<()> = std::sync::Mutex::new(());
        let _guard = ENVIRONMENT
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        // Every base variable, not just the cache one: the registration directory now
        // prefers `XDG_RUNTIME_DIR`, so redirecting `XDG_CACHE_HOME` alone would leave
        // this test reading the developer's real runtime directory.
        const BASE_VARIABLES: [&str; 3] = ["XDG_RUNTIME_DIR", "XDG_CACHE_HOME", "HOME"];

        let base = tempfile::tempdir().expect("a base directory");
        let workspace = tempfile::tempdir().expect("a workspace root");
        // A registration directory the daemon would never have created: every poll
        // refuses it, so discovery is undecided on every pass and never absent.
        let dir = base.path().join("oxabl").join("daemon");
        std::fs::create_dir_all(&dir).expect("a registration directory");
        std::fs::set_permissions(&dir, PermissionsExt::from_mode(0o755)).expect("widen it");
        let previous: Vec<_> = BASE_VARIABLES
            .iter()
            .map(|name| (*name, std::env::var_os(name)))
            .collect();
        // SAFETY: the lock above makes this the only thread mutating the environment.
        unsafe {
            for name in BASE_VARIABLES {
                std::env::set_var(name, base.path());
            }
        }

        let (editor, _client) = Connection::memory();
        let mut progress = StartProgress::new(&editor, &InitializeParams::default(), START_LIMIT);
        let outcome = find_or_start_daemon(workspace.path(), &mut Vec::new(), &mut progress);

        unsafe {
            for (name, value) in previous {
                match value {
                    Some(value) => std::env::set_var(name, value),
                    None => std::env::remove_var(name),
                }
            }
        }

        let message = outcome
            .expect_err("a client that never gets an answer must fail rather than connect")
            .to_string();
        assert!(
            message.contains(&dir.display().to_string()),
            "the give-up must name the cause it kept seeing, not only the timeout: \
             {message}"
        );
        assert_eq!(
            std::fs::read_dir(&dir)
                .expect("the refused directory")
                .count(),
            0,
            "and an answer that proves nothing must not have started a daemon"
        );
    }
}
