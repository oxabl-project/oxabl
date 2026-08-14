//! One session per workspace root, and the disciplines that keep its answers
//! honest under concurrency.
//!
//! # What a session is
//!
//! One salsa instance, the open buffers it holds, the schema revision handle, and
//! the configuration generation. Several clients share one session — an editor's
//! language client and the desktop app on the same workspace root hold one index
//! between them rather than one each, which is the resource sharing the daemon
//! exists for.
//!
//! # The four disciplines
//!
//! These are the load-bearing part of the session, not the routing around it. A
//! background computation can come back irrelevant in four different ways, and
//! three of them look identical at the receiving end:
//!
//! 1. **Buffer-version supersession.** The text moved on while the worker ran, so
//!    the answer describes bytes nobody is looking at.
//! 2. **Configuration-generation supersession.** The severities or include paths
//!    were replaced, so the answer was computed under rules the user has changed.
//!    The buffer version cannot catch this: the text did not move, only the
//!    configuration did.
//! 3. **Cancellation.** A concurrent write flagged every live snapshot, so this
//!    computation unwound. Salsa's cancellation is global — an edit in one file
//!    cancels every other file's in-flight work — so the buffer must be **re-armed**
//!    or it keeps displaying pre-edit answers until someone types in it.
//! 4. **A genuine panic.** Deterministic in the buffer's text, so it must fail
//!    exactly one request and never be retried. Retrying spins forever.
//!
//! [`Disposition`] is the one decision that separates them. Writing it as a
//! returned value rather than as control flow inside a handler is what lets it be
//! tested directly and shared by every client, which is the whole point of the
//! extraction: two copies of this reasoning would drift, and the failure mode of
//! drifting is publishing a stale answer.

use std::collections::HashMap;
use std::fs::Metadata;
use std::path::{Path, PathBuf};
use std::sync::{
    Arc,
    atomic::{AtomicU32, Ordering},
};
use std::time::SystemTime;

use oxabl_analyze::CollectedDiagnostics;
use oxabl_common::catch_panic;
use oxabl_daemon_protocol::SymbolRow;
use oxabl_index::search::normalize_lexically;
use oxabl_pipeline::{PipelineConfig, ReverseGraph};
use salsa::Setter;

use crate::db::{
    AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, buffer_dependencies,
    compute_diagnostics,
};

/// One workspace root's analysis state: one salsa instance, its open buffers, and
/// the two generations a completed computation is judged against.
pub struct Session {
    /// The root this session answers for, lexically normalised so two spellings of
    /// one root are one session.
    root: PathBuf,
    /// The salsa substrate. Writes happen here on the owning thread; each
    /// computation runs on a cloned snapshot.
    db: AnalysisDatabase,
    /// The schema revision handle. Bumping it invalidates every buffer's
    /// diagnostics, which is how a `.df` change hot-reloads.
    schema: SchemaHandle,
    /// Open buffers by document key (a URI for an editor, a path for anything
    /// else).
    buffers: HashMap<String, Buffer>,
    /// Bumped every time a configuration is installed. A computation carries the
    /// generation it read, so one that finished under rules the user has since
    /// replaced is not published — the buffer version cannot catch that, because
    /// the text did not move.
    config_generation: u64,
    /// How many clients are attached. Two clients on one root share this session
    /// rather than holding an index each, which is the resource sharing the daemon
    /// exists for.
    clients: u32,
    /// How many attached clients contribute unsaved buffers, so an answer can state
    /// whether it measured disk or a working tree.
    editor_clients: u32,
    /// Bumped when an open buffer changes. Disk changes do not touch it: those
    /// make the stored graph stale until the user requests a reindex (KTD7).
    buffer_generation: u64,
    /// The last completed whole-workspace pass, shared by every client.
    workspace: Option<WorkspaceSnapshot>,
    /// Live counters for the one whole-workspace pass now running.
    workspace_progress: Option<WorkspaceProgress>,
}

/// Live counters for one whole-workspace pass, plus the signal that it ended.
///
/// The signal lives here rather than beside the session state on purpose. A waiter
/// clones this out from under the session lock, releases that lock, and blocks on
/// the condvar — so waiting costs nothing and, more importantly, does not contend
/// with the pass it is waiting for. Polling the session lock did: every wake took
/// the same lock the running pass needs to install its result.
#[derive(Clone, Default)]
pub(crate) struct WorkspaceProgress {
    indexed: Arc<AtomicU32>,
    total: Arc<AtomicU32>,
    /// Set once when the pass ends, **however** it ends. A waiter that is not woken
    /// on the failure path hangs, which is worse than the polling this replaces.
    finished: Arc<(std::sync::Mutex<bool>, std::sync::Condvar)>,
}

impl WorkspaceProgress {
    pub fn values(&self) -> (u32, u32) {
        (
            self.indexed.load(Ordering::Relaxed),
            self.total.load(Ordering::Relaxed),
        )
    }

    pub fn set_total(&self, total: usize) {
        self.total.store(total as u32, Ordering::Relaxed);
    }

    pub fn advance(&self) {
        self.indexed.fetch_add(1, Ordering::Relaxed);
    }

    pub fn complete(&self) {
        let total = self.total.load(Ordering::Relaxed);
        self.indexed.store(total, Ordering::Relaxed);
    }

    /// Mark the pass ended and wake every waiter.
    ///
    /// Idempotent, and called on success, on a discarded stale result, and on
    /// failure alike — one call site covering all three, because three would be
    /// three chances to forget one.
    pub fn finish(&self) {
        let (lock, condvar) = &*self.finished;
        let mut finished = lock.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        *finished = true;
        condvar.notify_all();
    }

    /// Block until the pass ends, or until `timeout` elapses.
    ///
    /// The timeout is a backstop, not the mechanism: a missed wake becomes a slow
    /// re-check rather than a hung daemon, which is the failure mode condvar code
    /// is worth defending against.
    pub fn wait_until_finished(&self, timeout: std::time::Duration) {
        let (lock, condvar) = &*self.finished;
        let finished = lock.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if *finished {
            return;
        }
        let _ = condvar.wait_timeout(finished, timeout);
    }
}

#[derive(Clone)]
pub(crate) struct WorkspaceSnapshot {
    pub graph: Arc<ReverseGraph>,
    pub symbols: Arc<Vec<SymbolRow>>,
    pub files: Arc<Vec<FileStamp>>,
    pub config: Arc<PipelineConfig>,
    pub buffer_generation: u64,
    pub pass_millis: u64,
    pub graph_bytes: u64,
}

#[derive(Clone)]
pub(crate) struct FileStamp {
    path: PathBuf,
    len: u64,
    modified: Option<SystemTime>,
}

impl FileStamp {
    pub fn capture(path: PathBuf) -> Self {
        let metadata = std::fs::metadata(&path).ok();
        FileStamp {
            path,
            len: metadata.as_ref().map_or(0, Metadata::len),
            modified: metadata.and_then(|value| value.modified().ok()),
        }
    }

    pub fn changed(&self) -> bool {
        let Ok(metadata) = std::fs::metadata(&self.path) else {
            return true;
        };
        metadata.len() != self.len || metadata.modified().ok() != self.modified
    }
}

impl Session {
    /// A session for `root`, with the default configuration.
    ///
    /// Reads nothing: the configuration is resolved and installed by whoever owns
    /// the workspace, and no file is indexed until a lookup reaches it.
    pub fn new(root: impl AsRef<Path>) -> Self {
        let config = AnalysisConfig::default();
        let db = AnalysisDatabase::new(config);
        let schema = SchemaHandle::new(&db, 0);
        Session {
            root: normalize_lexically(root.as_ref()),
            db,
            schema,
            buffers: HashMap::new(),
            config_generation: 0,
            clients: 0,
            editor_clients: 0,
            buffer_generation: 0,
            workspace: None,
            workspace_progress: None,
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn database(&self) -> &AnalysisDatabase {
        &self.db
    }

    pub fn database_mut(&mut self) -> &mut AnalysisDatabase {
        &mut self.db
    }

    pub fn schema_handle(&self) -> SchemaHandle {
        self.schema
    }

    pub fn config_generation(&self) -> u64 {
        self.config_generation
    }

    pub fn clients(&self) -> u32 {
        self.clients
    }

    pub fn editor_clients(&self) -> u32 {
        self.editor_clients
    }

    /// How many buffers are open — the count that makes "a working tree" a quantity
    /// rather than a mood.
    pub fn open_buffers(&self) -> u32 {
        self.buffers.len() as u32
    }

    pub fn buffer_generation(&self) -> u64 {
        self.buffer_generation
    }

    pub(crate) fn workspace(&self) -> Option<WorkspaceSnapshot> {
        self.workspace.clone()
    }

    pub(crate) fn install_workspace(&mut self, workspace: WorkspaceSnapshot) {
        self.workspace = Some(workspace);
        self.workspace_progress = None;
    }

    pub(crate) fn workspace_progress(&self) -> Option<WorkspaceProgress> {
        self.workspace_progress.clone()
    }

    pub(crate) fn begin_workspace_pass(&mut self) -> WorkspaceProgress {
        let progress = WorkspaceProgress::default();
        self.workspace_progress = Some(progress.clone());
        progress
    }

    /// Claim the right to run a pass, or report that one is already running.
    ///
    /// Reading the state and claiming it has to be one step. Split across two, a
    /// caller that decides to start a pass because it saw none can be beaten to it
    /// by another caller doing the same — and the loser exists only to wait for a
    /// result it then discards.
    pub(crate) fn claim_workspace_pass(&mut self) -> Option<WorkspaceProgress> {
        if self.workspace_progress.is_some() {
            return None;
        }
        Some(self.begin_workspace_pass())
    }

    pub(crate) fn clear_workspace_progress(&mut self) {
        self.workspace_progress = None;
    }

    /// Unsaved file buffers, keyed by their workspace path.
    pub(crate) fn buffer_overlay(&self) -> HashMap<PathBuf, Arc<str>> {
        self.buffers
            .values()
            .filter_map(|buffer| {
                let path = buffer.path(&self.db).as_ref()?;
                Some((
                    normalize_lexically(path),
                    Arc::from(buffer.text(&self.db).as_str()),
                ))
            })
            .collect()
    }

    /// Record a client attaching. `contributes_buffers` is true for an editor,
    /// whose unsaved text becomes part of what every answer measures.
    pub fn attach(&mut self, contributes_buffers: bool) {
        self.clients += 1;
        if contributes_buffers {
            self.editor_clients += 1;
        }
    }

    /// Record a client detaching. Saturating, so a double detach cannot wrap the
    /// count into a very large number of imaginary clients.
    pub fn detach(&mut self, contributed_buffers: bool) {
        self.clients = self.clients.saturating_sub(1);
        if contributed_buffers {
            self.editor_clients = self.editor_clients.saturating_sub(1);
        }
    }

    /// The buffer for `key`, if it is open.
    pub fn buffer(&self, key: &str) -> Option<Buffer> {
        self.buffers.get(key).copied()
    }

    /// Open or replace a buffer's text.
    pub fn set_buffer(&mut self, key: &str, text: String, path: Option<PathBuf>) -> Buffer {
        match self.buffers.get(key).copied() {
            Some(buffer) => {
                if buffer.text(&self.db).as_str() != text {
                    buffer.set_text(&mut self.db).to(text);
                    self.buffer_generation = self.buffer_generation.wrapping_add(1);
                }
                buffer
            }
            None => {
                let buffer = Buffer::new(&self.db, text, path);
                self.buffers.insert(key.to_string(), buffer);
                self.buffer_generation = self.buffer_generation.wrapping_add(1);
                buffer
            }
        }
    }

    /// Close a buffer. Its salsa input stays allocated — salsa has no removal — but
    /// nothing reads it again.
    pub fn close_buffer(&mut self, key: &str) {
        if self.buffers.remove(key).is_some() {
            self.buffer_generation = self.buffer_generation.wrapping_add(1);
        }
    }

    /// Force an open buffer's incremental queries to re-read external inputs.
    ///
    /// The text is intentionally written back unchanged. This is for an include
    /// or search-path change, where the buffer bytes stayed put but expansion did
    /// not. Ordinary [`set_buffer`](Self::set_buffer) keeps its byte-identical
    /// early cut-off.
    pub fn retrigger_buffer(&mut self, key: &str) -> bool {
        let Some(buffer) = self.buffers.get(key).copied() else {
            return false;
        };
        let text = buffer.text(&self.db).clone();
        buffer.set_text(&mut self.db).to(text);
        true
    }

    /// Every open buffer's key.
    pub fn buffer_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.buffers.keys().cloned().collect();
        keys.sort_unstable();
        keys
    }

    /// Install a re-resolved configuration, bump the generation, and drop the
    /// stored workspace.
    ///
    /// # Who may call this
    ///
    /// Only a caller acting on a genuine configuration change — a client that has
    /// re-resolved its own settings, or a watcher that saw `oxabl.toml` or a schema
    /// file change. A **query handler must never call it**. The session is shared by
    /// every client on the root, so a query that installed the configuration it had
    /// resolved for its own pass would replace the configuration another client
    /// resolved, and answer that client's next request under rules it never chose.
    ///
    /// The write is invisible at the moment it lands, which is what makes the rule
    /// worth stating: the configuration is a plain field rather than a salsa input,
    /// so every memoized result stays valid and keeps being served. The first wrong
    /// answer arrives later, at the next recompute, with nothing to connect it to
    /// the query that caused it.
    ///
    /// The index registry is carried forward deliberately: the per-file salsa inputs
    /// live in the database and this map is the only way back to them, so a fresh
    /// handle would orphan every input and a later disk change would find nothing to
    /// bump.
    pub fn install_config(&mut self, pipeline: PipelineConfig) {
        let previous = self.db.config().clone();
        self.db.set_config(AnalysisConfig {
            fs: Arc::clone(&previous.fs),
            pipeline: Arc::new(pipeline),
            preprocess: previous.preprocess,
            index: Arc::clone(&previous.index),
        });
        self.config_generation += 1;
        self.workspace = None;
    }

    /// Replace the whole analysis configuration, for a caller that owns the
    /// filesystem too — the tests, and a client that supplies its own.
    pub fn install_analysis_config(&mut self, config: AnalysisConfig) {
        self.db.set_config(config);
        self.config_generation += 1;
        self.workspace = None;
    }

    /// Bump the schema revision, invalidating every buffer's diagnostics.
    pub fn bump_schema(&mut self) {
        let revision = self.schema.revision(&self.db).wrapping_add(1);
        self.schema.set_revision(&mut self.db).to(revision);
        self.workspace = None;
    }
}

/// One session per workspace root, held in a map (KTD21).
///
/// A map rather than a wider field on one session: the language server reads only
/// the first workspace folder today and its own comment says nothing in it can hold
/// two, so multi-root is a question of how many sessions exist rather than how much
/// each one holds.
#[derive(Default)]
pub struct Sessions {
    by_root: HashMap<PathBuf, Session>,
}

impl Sessions {
    pub fn new() -> Self {
        Sessions::default()
    }

    /// The session for `root`, creating it on first sight.
    ///
    /// Creating one allocates a database and reads nothing, so a client naming a
    /// root it never queries costs an empty session and no I/O.
    pub fn for_root(&mut self, root: impl AsRef<Path>) -> &mut Session {
        let key = normalize_lexically(root.as_ref());
        self.by_root
            .entry(key.clone())
            .or_insert_with(|| Session::new(&key))
    }

    /// The session for `root`, if one exists.
    pub fn get(&self, root: impl AsRef<Path>) -> Option<&Session> {
        self.by_root.get(&normalize_lexically(root.as_ref()))
    }

    /// The mutable session for `root`, if one exists.
    pub fn get_mut(&mut self, root: impl AsRef<Path>) -> Option<&mut Session> {
        self.by_root.get_mut(&normalize_lexically(root.as_ref()))
    }

    /// How many sessions exist. Two clients on one root must leave this at one.
    pub fn len(&self) -> usize {
        self.by_root.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_root.is_empty()
    }

    /// Every root with a session, sorted.
    pub fn roots(&self) -> Vec<&Path> {
        let mut roots: Vec<&Path> = self.by_root.keys().map(PathBuf::as_path).collect();
        roots.sort_unstable();
        roots
    }
}

/// The sessions, shared by every connected client.
///
/// # The locking rule
///
/// Hold the lock to **write** — open a buffer, install a configuration, bump a
/// revision — or to **clone a snapshot**. Never hold it across a query. That is the
/// same write-on-main / read-on-snapshot discipline the substrate already documents,
/// and here it is also what keeps one client's slow answer from stalling another: a
/// handler that clones a snapshot and releases the lock leaves every other client
/// free, while one that queries under the lock serialises the whole daemon.
///
/// A handler therefore receives this host rather than `&mut Sessions`, so the shape
/// of the borrow is visible in the handler itself.
pub struct SessionHost {
    sessions: Arc<std::sync::Mutex<Sessions>>,
}

impl Clone for SessionHost {
    fn clone(&self) -> Self {
        SessionHost {
            sessions: Arc::clone(&self.sessions),
        }
    }
}

impl Default for SessionHost {
    fn default() -> Self {
        SessionHost::new()
    }
}

impl SessionHost {
    pub fn new() -> Self {
        SessionHost {
            sessions: Arc::new(std::sync::Mutex::new(Sessions::new())),
        }
    }

    /// Run `body` with the sessions locked.
    ///
    /// Keep the body short: take what is needed and get out. Poisoning is recovered
    /// from rather than propagated — a handler that unwound while holding the lock
    /// has already had its request failed, and the sessions behind it are a cache of
    /// per-root state that a partial update leaves usable.
    pub fn with<T>(&self, body: impl FnOnce(&mut Sessions) -> T) -> T {
        let mut sessions = self
            .sessions
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        body(&mut sessions)
    }
}

/// The outcome of one guarded analysis.
///
/// Both `Option`s carry one contract: `None` means **no trustworthy answer**, from
/// a cancellation or from a contained panic. Neither may be committed as an empty
/// result. The dependency half is not a bare `Vec` for exactly that reason — an
/// empty dependency set is a real answer (a file with no includes), and recording
/// it for a file that has includes stops the watcher from ever re-triggering it.
///
/// `panicked` is what separates the two ways of having no answer. A client cannot
/// tell them apart from the outside, but scheduling must: a cancelled computation
/// is worth re-running, a panicked one is not.
pub struct Analysis {
    pub diagnostics: Option<CollectedDiagnostics>,
    pub dependencies: Option<Vec<PathBuf>>,
    pub panicked: bool,
}

/// Compute a buffer's diagnostics **and** its include dependencies under one
/// shared panic guard, degrading both together on a panic.
///
/// **The guard must span both calls.** The dependency query runs the same buffer
/// through salsa one line later, so a genuine panic in expansion just past a
/// diagnostics-only guard would still kill the worker.
///
/// Each query carries its own `salsa::Cancelled::catch` *inside* this guard: a
/// cancellation is a race to abandon, not a bug to contain, and letting one reach
/// `catch_panic` would report every concurrent edit as a panic.
///
/// Returning normally on a panic is the contract a worker relies on: its send sits
/// after this call, so a contained panic still produces a result and the request
/// never stalls waiting on one that never arrives.
///
/// `label` names the buffer in the report.
pub fn analyze_guarded(
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
            eprintln!("oxabl: analysis panicked for {label}: {panic}");
            Analysis {
                diagnostics: None,
                dependencies: None,
                panicked: true,
            }
        }
    }
}

/// What a completed computation is worth.
///
/// Three arms rather than a boolean, because "do not publish this" splits into two
/// opposite follow-ups and collapsing them is the bug: dropping a cancelled result
/// leaves an untouched file showing pre-edit answers, and retrying a panicked one
/// spins at the debounce interval forever.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    /// Current and trustworthy. Publish it.
    Publish,
    /// Irrelevant now, and nothing will make it relevant. Drop it.
    Drop,
    /// Irrelevant now, but the question is still open. Re-arm the computation.
    Retry,
}

/// What a completed computation looks like from the receiving end.
///
/// The facts the decision turns on, and no more — so the decision can be tested
/// without standing up a database, a worker, or a client.
#[derive(Debug, Clone, Copy)]
pub struct CompletedWork {
    /// The buffer version the worker read, and the version the session now holds.
    /// `None` for the session's version means the buffer is closed.
    pub read_version: i32,
    pub current_version: Option<i32>,
    /// The configuration generation the worker read, and the session's current one.
    pub read_generation: u64,
    pub current_generation: u64,
    /// Whether the computation produced diagnostics at all.
    pub has_diagnostics: bool,
    /// Whether the absent answer came from a genuine panic rather than a
    /// cancellation.
    pub panicked: bool,
}

/// Decide what to do with a completed computation.
///
/// The order of the gates is deliberate and the never-retry rule threads through
/// all of them: a panic that happens to land under a superseded generation must not
/// collect the one retry the rule exists to prevent.
pub fn dispose(work: CompletedWork) -> Disposition {
    // 1. The buffer closed, or its text moved on. Nothing to publish and nothing
    //    to retry: a newer edit has its own computation coming.
    match work.current_version {
        None => return Disposition::Drop,
        Some(current) if current != work.read_version => return Disposition::Drop,
        Some(_) => {}
    }

    // 2. Computed under a configuration the user has since replaced. Publishing it
    //    would show one round of answers under the old rules. The text did not
    //    move, so nothing else would ever re-fire this buffer.
    if work.read_generation != work.current_generation {
        return retry_unless_panicked(work);
    }

    // 3. No answer. A cancellation is a race worth re-running; a panic is
    //    deterministic in the text and must fail this request only.
    if !work.has_diagnostics {
        return retry_unless_panicked(work);
    }

    Disposition::Publish
}

/// A superseded or absent answer is retried — unless a panic produced it.
fn retry_unless_panicked(work: CompletedWork) -> Disposition {
    if work.panicked {
        Disposition::Drop
    } else {
        Disposition::Retry
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A computation that is current and produced an answer.
    fn healthy() -> CompletedWork {
        CompletedWork {
            read_version: 7,
            current_version: Some(7),
            read_generation: 3,
            current_generation: 3,
            has_diagnostics: true,
            panicked: false,
        }
    }

    #[test]
    fn a_current_result_publishes() {
        assert_eq!(dispose(healthy()), Disposition::Publish);
    }

    #[test]
    fn a_closed_buffer_drops_its_result() {
        assert_eq!(
            dispose(CompletedWork {
                current_version: None,
                ..healthy()
            }),
            Disposition::Drop
        );
    }

    #[test]
    fn a_superseded_buffer_version_does_not_overwrite_a_newer_result() {
        assert_eq!(
            dispose(CompletedWork {
                current_version: Some(8),
                ..healthy()
            }),
            Disposition::Drop,
            "a newer edit has its own computation coming"
        );
    }

    #[test]
    fn a_superseded_config_generation_is_dropped_and_retried() {
        assert_eq!(
            dispose(CompletedWork {
                read_generation: 2,
                ..healthy()
            }),
            Disposition::Retry,
            "the text did not move, so nothing else would re-fire this buffer"
        );
    }

    #[test]
    fn a_cancelled_computation_re_arms() {
        assert_eq!(
            dispose(CompletedWork {
                has_diagnostics: false,
                ..healthy()
            }),
            Disposition::Retry,
            "salsa cancels every live snapshot, so this buffer may not have changed"
        );
    }

    #[test]
    fn a_genuine_panic_fails_one_request_and_is_never_retried() {
        assert_eq!(
            dispose(CompletedWork {
                has_diagnostics: false,
                panicked: true,
                ..healthy()
            }),
            Disposition::Drop,
            "a panic is deterministic in the text; retrying spins forever"
        );
    }

    /// The gate order matters: a panic under a superseded generation reaches the
    /// generation check first, and must still not be retried.
    #[test]
    fn a_panic_under_a_superseded_generation_is_still_never_retried() {
        assert_eq!(
            dispose(CompletedWork {
                read_generation: 2,
                has_diagnostics: false,
                panicked: true,
                ..healthy()
            }),
            Disposition::Drop
        );
    }
}
#[test]
fn workspace_progress_advances_and_completes() {
    let progress = WorkspaceProgress::default();
    progress.set_total(3);
    progress.advance();
    assert_eq!(progress.values(), (1, 3));
    progress.complete();
    assert_eq!(progress.values(), (3, 3));
}

/// A waiter parks on the running pass instead of polling for it.
///
/// The polling this replaced woke about a hundred times a second and took the
/// session lock on every wake — the same lock the running pass needs to install
/// its result, so waiting contended with the work being waited for. Asserted here
/// as the property that matters: a waiter makes no progress until the pass ends,
/// and then returns promptly.
///
/// Scoped to the primitive rather than driven through a slow workspace pass:
/// `build_workspace` constructs its filesystem inline, so there is no seam to park
/// a real pass on without adding one.
#[test]
fn a_waiter_parks_until_the_pass_finishes() {
    use std::sync::atomic::AtomicBool;
    use std::time::Duration;

    let progress = WorkspaceProgress::default();
    let returned = Arc::new(AtomicBool::new(false));

    let waiter = {
        let progress = progress.clone();
        let returned = Arc::clone(&returned);
        std::thread::spawn(move || {
            progress.wait_until_finished(Duration::from_secs(30));
            returned.store(true, Ordering::SeqCst);
        })
    };

    std::thread::sleep(Duration::from_millis(50));
    assert!(
        !returned.load(Ordering::SeqCst),
        "the waiter must stay parked while the pass runs"
    );

    progress.finish();
    waiter.join().expect("the waiter wakes and returns");
    assert!(returned.load(Ordering::SeqCst));
}

/// The signal fires however the pass ended, and a waiter that arrives after it is
/// not left parked for a wake that already happened.
#[test]
fn a_finished_pass_releases_a_late_waiter_immediately() {
    use std::time::{Duration, Instant};

    let progress = WorkspaceProgress::default();
    // The failure path finishes without ever calling `complete`, so a waiter must
    // be released by `finish` alone. Missing that wake is a hang, which is worse
    // than the polling this replaced.
    progress.finish();

    let started = Instant::now();
    progress.wait_until_finished(Duration::from_secs(30));
    assert!(
        started.elapsed() < Duration::from_secs(5),
        "a waiter arriving after the signal must not block"
    );
}

/// Only one caller can claim a pass, so the loser never starts a second one.
///
/// Reading the state and claiming it used to be two steps, and between them a
/// second caller could read "no pass running" and spawn a thread whose only work
/// was to wait for a result it then discarded.
#[test]
fn only_one_caller_can_claim_a_pass() {
    let mut session = Session::new("/proj");

    let first = session.claim_workspace_pass();
    assert!(first.is_some(), "the first caller claims the pass");
    assert!(
        session.claim_workspace_pass().is_none(),
        "a second caller must not start its own pass"
    );

    session.clear_workspace_progress();
    assert!(
        session.claim_workspace_pass().is_some(),
        "the slot is claimable again once the pass ends"
    );
}
