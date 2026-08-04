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
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_analyze::CollectedDiagnostics;
use oxabl_common::catch_panic;
use oxabl_index::search::normalize_lexically;
use oxabl_pipeline::PipelineConfig;
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
                buffer.set_text(&mut self.db).to(text);
                buffer
            }
            None => {
                let buffer = Buffer::new(&self.db, text, path);
                self.buffers.insert(key.to_string(), buffer);
                buffer
            }
        }
    }

    /// Close a buffer. Its salsa input stays allocated — salsa has no removal — but
    /// nothing reads it again.
    pub fn close_buffer(&mut self, key: &str) {
        self.buffers.remove(key);
    }

    /// Every open buffer's key.
    pub fn buffer_keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.buffers.keys().cloned().collect();
        keys.sort_unstable();
        keys
    }

    /// Install a re-resolved configuration and bump the generation.
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
    }

    /// Replace the whole analysis configuration, for a caller that owns the
    /// filesystem too — the tests, and a client that supplies its own.
    pub fn install_analysis_config(&mut self, config: AnalysisConfig) {
        self.db.set_config(config);
        self.config_generation += 1;
    }

    /// Bump the schema revision, invalidating every buffer's diagnostics.
    pub fn bump_schema(&mut self) {
        let revision = self.schema.revision(&self.db).wrapping_add(1);
        self.schema.set_revision(&mut self.db).to(revision);
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
    sessions: std::sync::Mutex<Sessions>,
}

impl Default for SessionHost {
    fn default() -> Self {
        SessionHost::new()
    }
}

impl SessionHost {
    pub fn new() -> Self {
        SessionHost {
            sessions: std::sync::Mutex::new(Sessions::new()),
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
