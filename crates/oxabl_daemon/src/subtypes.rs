//! The subtype index that a session keeps, and its build in the background.
//! See `docs/design/type-hierarchy.md`.
//!
//! The index is a value built from the facts of every class file
//! ([`oxabl_index::hierarchy::SubtypeIndex`]). A workspace pass that ends starts
//! a build on a thread of its own, so the first request that needs the index
//! finds it ready. A request that finds no current index starts the build and
//! waits for it. A request that is cancelled stops waiting. The build goes on
//! and is kept for the next request.
//!
//! A kept index is current for the [`Stamp`] it was built for, and for no other.
//! A build holds a database snapshot, and a write to a buffer waits until every
//! snapshot drops. The build reads each class file through a salsa query, and
//! a query checks for a pending write, so the build ends at the next file and
//! the write does not wait for it.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, OnceLock, Weak};
use std::time::Duration;

use oxabl_index::hierarchy::SubtypeIndex;
use oxabl_pipeline::ReverseGraph;

use crate::SessionHost;
use crate::db::{AnalysisDatabase, with_hierarchy};
use crate::hierarchy::{MAX_CLASSES, class_files_in, is_class_file, with_open_classes};
use crate::references::ReferenceError;
use crate::session::Session;

/// How many builds one request starts and waits for before it gives up: an
/// edit stops a build, and a request that keeps meeting edits must end.
const MAX_ATTEMPTS: u32 = 5;

/// How often a waiting request looks at its cancel flag and reports.
const POLL: Duration = Duration::from_millis(50);

/// What a request that waits shows the client.
///
/// Each method is called about every [`POLL`] while the request waits, with the
/// counts of that moment. A request that does not wait calls none.
pub trait Progress: Sync {
    /// The request waits for a workspace pass: `indexed` of `total` files.
    fn waiting_for_pass(&self, indexed: u32, total: u32);
    /// The request waits for the subtype index: the class files read so far,
    /// and all of them once the build knows them (0 before).
    fn reading_classes(&self, read: usize, total: usize);
}

/// A [`Progress`] that shows nothing.
pub struct NoProgress;

impl Progress for NoProgress {
    fn waiting_for_pass(&self, _indexed: u32, _total: u32) {}
    fn reading_classes(&self, _read: usize, _total: usize) {}
}

/// The state of the session that a kept index depends on. Two stamps are
/// the same when no input of the index changed between them.
///
/// - The workspace pass: it lists the class files and reads each class file with
///   its includes, so a file that changed on disk, or that a buffer changed and
///   the pass read, gives a new pass. A new configuration or schema clears the
///   stored pass, so it gives a new pass too.
/// - The text of the open class files: a buffer that holds a class counts as
///   a class of the workspace, saved or not.
#[derive(Clone)]
pub(crate) struct Stamp {
    // A `Weak` keeps the allocation, so no other graph can have its address.
    graph: Weak<ReverseGraph>,
    open: Vec<(PathBuf, Arc<str>)>,
}

impl Stamp {
    fn of(captured: &Captured, graph: &Arc<ReverseGraph>) -> Stamp {
        let mut open: Vec<(PathBuf, Arc<str>)> = captured
            .overlay
            .iter()
            .filter(|(path, _)| is_class_file(path))
            .map(|(path, text)| (path.clone(), Arc::clone(text)))
            .collect();
        open.sort_by(|a, b| a.0.cmp(&b.0));
        Stamp {
            graph: Arc::downgrade(graph),
            open,
        }
    }

    fn same(&self, other: &Stamp) -> bool {
        Weak::ptr_eq(&self.graph, &other.graph)
            && self.open.len() == other.open.len()
            && self
                .open
                .iter()
                .zip(&other.open)
                .all(|(a, b)| a.0 == b.0 && (Arc::ptr_eq(&a.1, &b.1) || a.1 == b.1))
    }
}

/// The inputs of a build or of an answer, taken together under the sessions
/// lock, so they describe one moment.
pub(crate) struct Captured {
    pub(crate) snapshot: AnalysisDatabase,
    pub(crate) overlay: HashMap<PathBuf, Arc<str>>,
    slot: Arc<IndexSlot>,
}

impl Captured {
    pub(crate) fn of(session: &Session) -> Captured {
        Captured {
            snapshot: session.database().clone(),
            overlay: session.buffer_overlay(),
            slot: session.subtype_slot(),
        }
    }
}

/// An index, and the state it is current for.
pub(crate) struct Built {
    stamp: Stamp,
    pub(crate) index: SubtypeIndex,
}

/// How a build ended.
#[derive(Debug, Clone)]
enum Outcome {
    /// The index is kept.
    Built,
    /// An edit, a newer build or nothing that is an error: ask again.
    Stopped,
    /// The build failed, and asking again fails again.
    Failed(String),
}

/// One run of the build.
pub(crate) struct Build {
    stamp: Stamp,
    ticket: u64,
    read: AtomicUsize,
    total: AtomicUsize,
    stop: AtomicBool,
    outcome: OnceLock<Outcome>,
}

#[derive(Default)]
struct State {
    built: Option<Arc<Built>>,
    /// The newest build that started, until it ends.
    building: Option<Arc<Build>>,
    tickets: u64,
}

/// What [`IndexSlot::claim`] decided.
enum Claim {
    /// The kept index is current.
    Ready(Arc<Built>),
    /// A build for this stamp runs: wait for it.
    Wait(Arc<Build>),
    /// No build for this stamp runs. The caller starts this one.
    Start(Arc<Build>),
}

/// The kept index and the running build of one session.
#[derive(Default)]
pub(crate) struct IndexSlot {
    state: Mutex<State>,
    changed: Condvar,
}

impl IndexSlot {
    fn lock(&self) -> MutexGuard<'_, State> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// The index for `stamp`: kept, or being built, or to be built by the caller.
    /// A running build for another stamp is stopped, because nothing waits for
    /// its index any more.
    fn claim(&self, stamp: &Stamp) -> Claim {
        let mut state = self.lock();
        if let Some(built) = &state.built
            && built.stamp.same(stamp)
        {
            return Claim::Ready(Arc::clone(built));
        }
        if let Some(running) = &state.building {
            if running.stamp.same(stamp) && !running.stop.load(Ordering::Relaxed) {
                return Claim::Wait(Arc::clone(running));
            }
            running.stop.store(true, Ordering::Relaxed);
        }
        state.tickets += 1;
        let build = Arc::new(Build {
            stamp: stamp.clone(),
            ticket: state.tickets,
            read: AtomicUsize::new(0),
            total: AtomicUsize::new(0),
            stop: AtomicBool::new(false),
            outcome: OnceLock::new(),
        });
        state.building = Some(Arc::clone(&build));
        Claim::Start(build)
    }

    /// Record how `build` ended, and wake every request that waits for it. The
    /// index is kept only when no newer build started.
    fn finish(&self, build: &Arc<Build>, result: Result<SubtypeIndex, Outcome>) {
        let mut state = self.lock();
        let newest = state
            .building
            .as_ref()
            .is_some_and(|running| running.ticket == build.ticket);
        let outcome = match result {
            Ok(index) if newest => {
                state.built = Some(Arc::new(Built {
                    stamp: build.stamp.clone(),
                    index,
                }));
                Outcome::Built
            }
            Ok(_) => Outcome::Stopped,
            Err(outcome) => outcome,
        };
        if newest {
            state.building = None;
        }
        let _ = build.outcome.set(outcome);
        self.changed.notify_all();
    }

    /// Wait until `build` ends. Stops with [`ReferenceError::Cancelled`] when
    /// `cancel` is set, and leaves the build running.
    fn wait(
        &self,
        build: &Build,
        cancel: &AtomicBool,
        progress: &dyn Progress,
    ) -> Result<Outcome, ReferenceError> {
        loop {
            if let Some(outcome) = build.outcome.get() {
                return Ok(outcome.clone());
            }
            if cancel.load(Ordering::Relaxed) {
                return Err(ReferenceError::Cancelled);
            }
            progress.reading_classes(
                build.read.load(Ordering::Relaxed),
                build.total.load(Ordering::Relaxed),
            );
            let state = self.lock();
            // The outcome is set under this lock, so a wake cannot be missed.
            if build.outcome.get().is_none() {
                let _ = self.changed.wait_timeout(state, POLL);
            }
        }
    }

    /// Block until no build runs. For a benchmark that must not time one.
    fn wait_until_idle(&self) {
        let mut state = self.lock();
        while state.building.is_some() {
            state = self
                .changed
                .wait_timeout(state, POLL)
                .unwrap_or_else(|poisoned| poisoned.into_inner())
                .0;
        }
    }
}

/// Test builds only: write a line to stderr when `OXABL_TEST_INDEX_COUNTS` is
/// set, for the tests that count the builds.
fn report_test_line(line: &str) {
    if oxabl_common::INJECTION_ENABLED && std::env::var_os("OXABL_TEST_INDEX_COUNTS").is_some() {
        use std::io::Write as _;
        let _ = writeln!(std::io::stderr(), "oxabl-test: {line}");
    }
}

fn report_build(trigger: &str, files: usize, outcome: &str) {
    report_test_line(&format!(
        "subtype index {outcome} for {files} class files, started by the {trigger}"
    ));
}

/// Test builds only: a build is held at a class file, so a test knows that the
/// build runs.
pub(crate) fn report_hold() {
    report_test_line("subtype index holds");
}

/// Start `build` on a thread of its own, over `captured` and the class files
/// of `graph`.
fn start(
    host: &SessionHost,
    session_root: &Path,
    captured: Captured,
    graph: Arc<ReverseGraph>,
    build: Arc<Build>,
    trigger: &'static str,
) {
    let slot = Arc::clone(&captured.slot);
    // Taken before the spawn, so the daemon cannot go idle between the end of
    // the pass and the start of the build.
    let busy = host.activity().work();
    let session_root = session_root.to_path_buf();
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-subtype-index".to_string())
        .spawn({
            let build = Arc::clone(&build);
            let slot = Arc::clone(&slot);
            move || {
                let _busy = busy;
                let result = match oxabl_common::catch_panic(|| {
                    run(&captured, &graph, &build, &session_root, trigger)
                }) {
                    Ok(result) => result,
                    Err(panic) => Err(Outcome::Failed(format!("the index failed: {panic}"))),
                };
                // The snapshot drops before anything else can wait for a write.
                drop(captured);
                slot.finish(&build, result);
            }
        });
    if let Err(error) = spawned {
        slot.finish(
            &build,
            Err(Outcome::Failed(format!("cannot start the index: {error}"))),
        );
    }
}

/// Read the class files of `graph` and the open buffers, and build the index.
fn run(
    captured: &Captured,
    graph: &ReverseGraph,
    build: &Build,
    session_root: &Path,
    trigger: &str,
) -> Result<SubtypeIndex, Outcome> {
    let stopped = || build.stop.load(Ordering::Relaxed);
    let made = with_hierarchy(&captured.snapshot, captured.overlay.clone(), |unit| {
        let mut files = class_files_in(graph);
        files.truncate(MAX_CLASSES);
        let files = with_open_classes(unit, files, session_root);
        build.total.store(files.len(), Ordering::Relaxed);
        let index = unit.subtype_index(&files, &stopped, &|| {
            build.read.fetch_add(1, Ordering::Relaxed);
        });
        (index, files.len())
    });
    match made {
        Some((Some(index), files)) => {
            report_build(trigger, files, "built");
            Ok(index)
        }
        // A write cancelled the snapshot, or a newer build stopped this one.
        Some((None, files)) => {
            report_build(trigger, files, "stopped");
            Err(Outcome::Stopped)
        }
        None => {
            report_build(trigger, 0, "cancelled");
            Err(Outcome::Stopped)
        }
    }
}

/// Take the inputs of the session at `session_root` together.
pub(crate) fn capture(host: &SessionHost, session_root: &Path) -> Option<Captured> {
    host.with(|sessions| sessions.get(session_root).map(Captured::of))
}

/// A kept index that is current, with the state it is current for.
pub(crate) struct Ready {
    /// The state of the session for which `built` is current: answer from
    /// it, so the answer and the index describe one moment.
    pub(crate) captured: Captured,
    pub(crate) built: Arc<Built>,
}

/// The index for the class files of `graph`, current for the state of the
/// session now. Waits for a build when there is none to use, and reports to
/// `progress` while it waits. The wait holds no database snapshot.
pub(crate) fn index_for(
    host: &SessionHost,
    session_root: &Path,
    graph: &Arc<ReverseGraph>,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<Ready, ReferenceError> {
    for _ in 0..MAX_ATTEMPTS {
        if cancel.load(Ordering::Relaxed) {
            return Err(ReferenceError::Cancelled);
        }
        let Some(captured) = capture(host, session_root) else {
            return Err(ReferenceError::Failed("no session".to_string()));
        };
        let stamp = Stamp::of(&captured, graph);
        let slot = Arc::clone(&captured.slot);
        let outcome = match slot.claim(&stamp) {
            Claim::Ready(built) => return Ok(Ready { captured, built }),
            Claim::Wait(build) => {
                drop(captured);
                slot.wait(&build, cancel, progress)?
            }
            Claim::Start(build) => {
                start(
                    host,
                    session_root,
                    captured,
                    Arc::clone(graph),
                    Arc::clone(&build),
                    "request",
                );
                slot.wait(&build, cancel, progress)?
            }
        };
        if let Outcome::Failed(reason) = outcome {
            return Err(ReferenceError::Failed(reason));
        }
    }
    Err(ReferenceError::Modified)
}

/// Start the build for the pass that `session` just installed, unless the
/// kept index or a running build is for the same state.
pub(crate) fn after_pass(host: &SessionHost, session: &Session, graph: &Arc<ReverseGraph>) {
    let captured = Captured::of(session);
    let stamp = Stamp::of(&captured, graph);
    let slot = Arc::clone(&captured.slot);
    if let Claim::Start(build) = slot.claim(&stamp) {
        start(
            host,
            session.root(),
            captured,
            Arc::clone(graph),
            build,
            "pass",
        );
    }
}

/// Block until the session at `session_root` runs no subtype index build.
/// For a benchmark: a build that runs on a thread of its own is not in the
/// number of the thread that times a request.
#[doc(hidden)]
pub fn wait_for_subtype_index(host: &SessionHost, session_root: &Path) {
    let slot = host.with(|sessions| sessions.get(session_root).map(Session::subtype_slot));
    if let Some(slot) = slot {
        slot.wait_until_idle();
    }
}
