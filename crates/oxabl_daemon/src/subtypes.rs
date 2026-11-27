//! The subtype index that a session keeps, and its build in the background.
//! See `docs/design/type-hierarchy.md`.
//!
//! The index is a value built from the facts of every class file
//! ([`oxabl_index::hierarchy::SubtypeIndex`]). A session builds none until a
//! request needs it. The first request starts the build and waits for it. From
//! then on, a workspace pass that ends starts a build on a thread of its own,
//! so the next request finds the index ready. A request that is cancelled
//! stops waiting. The build goes on and is kept for the next request.
//!
//! A kept index is current for the [`Stamp`] it was built for, and for no other.
//! A build holds a database snapshot, and a write to a buffer waits until every
//! snapshot drops. The build reads each class file through a salsa query, and
//! a query checks for a pending write, so the build ends at the next file and
//! the write does not wait for it.

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, OnceLock, Weak};
use std::time::Duration;

use oxabl_index::hierarchy::SubtypeIndex;
use oxabl_pipeline::ReverseGraph;

use crate::SessionHost;
use crate::db::{AnalysisDatabase, HierarchyUnit, with_hierarchy};
use crate::hierarchy::{MAX_CLASSES, is_class_file};
use crate::pass::KeptPass;
use crate::references::ReferenceError;
use crate::session::{Session, canonical_path};

/// How many claims one call of [`index_for`] makes. A build that ends is read
/// by the next claim; anything else makes the caller take the graph again.
const CLAIMS: u32 = 2;

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

/// The class files of one workspace pass. Listed once for each graph, not
/// once for each build or request.
pub(crate) struct ClassList {
    graph: Weak<ReverseGraph>,
    /// The class files, sorted, cut at [`MAX_CLASSES`].
    pub(crate) files: Vec<PathBuf>,
    /// The class files before the cut.
    pub(crate) count: usize,
    /// `files` canonicalized, made when an open class buffer needs it.
    canonical: OnceLock<HashSet<PathBuf>>,
}

impl ClassList {
    fn of(graph: &Arc<ReverseGraph>) -> ClassList {
        let mut files = graph.tracked_files_where(is_class_file);
        let count = files.len();
        files.truncate(MAX_CLASSES);
        ClassList {
            graph: Arc::downgrade(graph),
            files,
            count,
            canonical: OnceLock::new(),
        }
    }

    /// No class file: the list when no pass gave a graph.
    pub(crate) fn empty() -> ClassList {
        ClassList {
            graph: Weak::new(),
            files: Vec::new(),
            count: 0,
            canonical: OnceLock::new(),
        }
    }

    fn canonical(&self) -> &HashSet<PathBuf> {
        self.canonical
            .get_or_init(|| self.files.iter().map(|path| canonical_path(path)).collect())
    }

    /// The class files, and the open buffers that hold a class file of the
    /// workspace and are not among them: a class not saved yet is a subtype
    /// too.
    pub(crate) fn with_open_classes(
        &self,
        unit: &HierarchyUnit<'_>,
        session_root: &Path,
    ) -> Cow<'_, [PathBuf]> {
        let mut added: Vec<PathBuf> = Vec::new();
        let mut seen: HashSet<PathBuf> = HashSet::new();
        for open in unit.open_class_files() {
            let open_canonical = canonical_path(&open);
            if !open_canonical.starts_with(session_root)
                || self.canonical().contains(&open_canonical)
            {
                continue;
            }
            if seen.insert(open_canonical) {
                added.push(open);
            }
        }
        if added.is_empty() {
            return Cow::Borrowed(&self.files);
        }
        let mut files = self.files.clone();
        files.append(&mut added);
        Cow::Owned(files)
    }
}

/// The state of the session that a kept index depends on. Two stamps are
/// the same when no input of the index changed between them.
///
/// - The workspace pass: it lists the class files and reads each class file with
///   its includes, so a file that changed on disk, or that a buffer changed and
///   the pass read, gives a new pass. A new configuration or schema clears the
///   stored pass, so it gives a new pass too.
/// - The open class buffers whose text is not the text that the pass read: a
///   buffer that holds a class counts as a class of the workspace, saved or
///   not. A buffer with the text that the pass read changes nothing, so
///   opening a saved class file keeps the index.
#[derive(Clone)]
pub(crate) struct Stamp {
    // A `Weak` keeps the allocation, so no other graph can have its address.
    graph: Weak<ReverseGraph>,
    open: Vec<(PathBuf, Arc<str>)>,
}

impl Stamp {
    fn of(captured: &Captured, graph: &Arc<ReverseGraph>) -> Stamp {
        // What the pass read is known only for the stored pass of `graph`.
        let kept = captured
            .pass
            .as_ref()
            .filter(|(stored, _)| Arc::ptr_eq(stored, graph))
            .map(|(_, kept)| kept);
        let mut open: Vec<(PathBuf, Arc<str>)> = captured
            .overlay
            .iter()
            .filter(|(path, _)| is_class_file(path))
            .filter(|(path, text)| {
                kept.is_none_or(|kept| kept.read_hash(path) != Some(blake3::hash(text.as_bytes())))
            })
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

    fn is_of(&self, graph: Option<&Arc<ReverseGraph>>) -> bool {
        graph.is_some_and(|graph| std::ptr::eq(self.graph.as_ptr(), Arc::as_ptr(graph)))
    }
}

/// The inputs of a build or of an answer, taken together under the sessions
/// lock, so they describe one moment.
pub(crate) struct Captured {
    pub(crate) snapshot: AnalysisDatabase,
    pub(crate) overlay: HashMap<PathBuf, Arc<str>>,
    /// The stored workspace pass: its graph, and what it read.
    pass: Option<(Arc<ReverseGraph>, Arc<KeptPass>)>,
    slot: Arc<IndexSlot>,
}

impl Captured {
    pub(crate) fn of(session: &Session) -> Captured {
        Captured {
            snapshot: session.database().clone(),
            overlay: session.buffer_overlay(),
            pass: session
                .workspace()
                .map(|stored| (stored.graph, stored.kept)),
            slot: session.subtype_slot(),
        }
    }

    fn stored_graph(&self) -> Option<&Arc<ReverseGraph>> {
        self.pass.as_ref().map(|(graph, _)| graph)
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
    /// A request needed the index: from then on, each stored pass starts a
    /// build. Until then, a pass starts none.
    wanted: bool,
    /// The class files of the newest graph that a build or a request listed.
    classes: Option<Arc<ClassList>>,
}

/// What [`IndexSlot::claim`] decided.
enum Claim {
    /// The kept index is current.
    Ready(Arc<Built>),
    /// A build for this stamp runs: wait for it.
    Wait(Arc<Build>),
    /// No build for this stamp runs. The caller starts this one.
    Start(Arc<Build>),
    /// A build for the stored pass runs, and the stamp is of an older graph:
    /// wait for that build, and take the graph again.
    Newer(Arc<Build>),
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

    fn want(&self) {
        self.lock().wanted = true;
    }

    fn wanted(&self) -> bool {
        self.lock().wanted
    }

    /// The class files of `graph`, listed once for each graph.
    fn classes(&self, graph: &Arc<ReverseGraph>) -> Arc<ClassList> {
        if let Some(classes) = &self.lock().classes
            && std::ptr::eq(classes.graph.as_ptr(), Arc::as_ptr(graph))
        {
            return Arc::clone(classes);
        }
        // Listed outside the lock: a claim must not wait for it.
        let classes = Arc::new(ClassList::of(graph));
        self.lock().classes = Some(Arc::clone(&classes));
        classes
    }

    /// The index for `stamp`: kept, or being built, or to be built by the caller.
    ///
    /// A running build for another stamp is stopped, because nothing waits for
    /// its index any more, except when it is a build for the stored pass
    /// (`stored`) and `stamp` is not: a request that took an older graph never
    /// stops the build of a newer one.
    fn claim(&self, stamp: &Stamp, stored: Option<&Arc<ReverseGraph>>) -> Claim {
        let mut state = self.lock();
        if let Some(built) = &state.built
            && built.stamp.same(stamp)
        {
            return Claim::Ready(Arc::clone(built));
        }
        if let Some(running) = &state.building {
            let stopped = running.stop.load(Ordering::Relaxed);
            if running.stamp.same(stamp) && !stopped {
                return Claim::Wait(Arc::clone(running));
            }
            if !stopped && running.stamp.is_of(stored) && !stamp.is_of(stored) {
                return Claim::Newer(Arc::clone(running));
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
    fn finish(&self, build: &Arc<Build>, result: Result<SubtypeIndex, Outcome>) -> Outcome {
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
        let _ = build.outcome.set(outcome.clone());
        self.changed.notify_all();
        outcome
    }

    /// Drop the kept index and stop the running build: a file changed in a way
    /// that the stamp may not show.
    fn forget(&self) {
        let mut state = self.lock();
        state.built = None;
        if let Some(running) = &state.building {
            running.stop.store(true, Ordering::Relaxed);
        }
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

/// Test builds only: how `build` ended. `built` is a kept index.
fn report_build(trigger: &str, build: &Build, outcome: &Outcome) {
    let word = match outcome {
        Outcome::Built => "built",
        Outcome::Stopped => "stopped",
        Outcome::Failed(_) => "failed",
    };
    let files = build.total.load(Ordering::Relaxed);
    report_test_line(&format!(
        "subtype index {word} for {files} class files, started by the {trigger}"
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
                    // Listed on this thread: a pass that starts the build holds
                    // the sessions lock.
                    let classes = slot.classes(&graph);
                    run(&captured, &classes, &build, &session_root)
                }) {
                    Ok(result) => result,
                    Err(panic) => Err(Outcome::Failed(format!("the index failed: {panic}"))),
                };
                // The snapshot drops before anything else can wait for a write.
                drop(captured);
                let outcome = slot.finish(&build, result);
                report_build(trigger, &build, &outcome);
            }
        });
    if let Err(error) = spawned {
        let outcome = slot.finish(
            &build,
            Err(Outcome::Failed(format!("cannot start the index: {error}"))),
        );
        report_build(trigger, &build, &outcome);
    }
}

/// Read `classes` and the open buffers, and build the index.
fn run(
    captured: &Captured,
    classes: &ClassList,
    build: &Build,
    session_root: &Path,
) -> Result<SubtypeIndex, Outcome> {
    let stopped = || build.stop.load(Ordering::Relaxed);
    let made = with_hierarchy(&captured.snapshot, captured.overlay.clone(), |unit| {
        let files = classes.with_open_classes(unit, session_root);
        build.total.store(files.len(), Ordering::Relaxed);
        unit.subtype_index(&files, &stopped, &|| {
            build.read.fetch_add(1, Ordering::Relaxed);
        })
    });
    // `None`: a write cancelled the snapshot, or a newer build stopped this
    // one.
    made.flatten().ok_or(Outcome::Stopped)
}

/// Take the inputs of the session at `session_root` together.
pub(crate) fn capture(host: &SessionHost, session_root: &Path) -> Option<Captured> {
    host.with(|sessions| sessions.get(session_root).map(Captured::of))
}

/// The class files of `graph`, from the list that the session keeps for it.
pub(crate) fn class_list(
    host: &SessionHost,
    session_root: &Path,
    graph: &Arc<ReverseGraph>,
) -> Arc<ClassList> {
    match slot_of(host, session_root) {
        Some(slot) => slot.classes(graph),
        None => Arc::new(ClassList::of(graph)),
    }
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
///
/// `None` when the caller must take the graph again and ask again: an edit
/// or a newer pass stopped the build, or a build for a newer pass runs.
pub(crate) fn index_for(
    host: &SessionHost,
    session_root: &Path,
    graph: &Arc<ReverseGraph>,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<Option<Ready>, ReferenceError> {
    for _ in 0..CLAIMS {
        if cancel.load(Ordering::Relaxed) {
            return Err(ReferenceError::Cancelled);
        }
        let Some(captured) = capture(host, session_root) else {
            return Err(ReferenceError::Failed("no session".to_string()));
        };
        let slot = Arc::clone(&captured.slot);
        slot.want();
        let stamp = Stamp::of(&captured, graph);
        let outcome = match slot.claim(&stamp, captured.stored_graph()) {
            Claim::Ready(built) => return Ok(Some(Ready { captured, built })),
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
            Claim::Newer(build) => {
                drop(captured);
                slot.wait(&build, cancel, progress)?;
                return Ok(None);
            }
        };
        match outcome {
            Outcome::Built => {}
            Outcome::Stopped => return Ok(None),
            Outcome::Failed(reason) => return Err(ReferenceError::Failed(reason)),
        }
    }
    Ok(None)
}

/// Start the build for the pass that `session` just installed, unless no
/// request has needed the index yet, or the kept index or a running build is
/// for the same state.
pub(crate) fn after_pass(host: &SessionHost, session: &Session, graph: &Arc<ReverseGraph>) {
    let slot = session.subtype_slot();
    // A session that never asked for a hierarchy pays nothing.
    if !slot.wanted() {
        return;
    }
    let captured = Captured::of(session);
    let stamp = Stamp::of(&captured, graph);
    if let Claim::Start(build) = slot.claim(&stamp, Some(graph)) {
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

/// Drop the kept index of the session at `session_root`: a file changed on
/// disk, and the stamp of the pass may not show it.
pub fn forget_subtype_index(host: &SessionHost, session_root: &Path) {
    if let Some(slot) = slot_of(host, session_root) {
        slot.forget();
    }
}

fn slot_of(host: &SessionHost, session_root: &Path) -> Option<Arc<IndexSlot>> {
    host.with(|sessions| sessions.get(session_root).map(Session::subtype_slot))
}

/// Block until the session at `session_root` runs no subtype index build.
/// For a benchmark: a build that runs on a thread of its own is not in the
/// number of the thread that times a request.
#[doc(hidden)]
pub fn wait_for_subtype_index(host: &SessionHost, session_root: &Path) {
    if let Some(slot) = slot_of(host, session_root) {
        slot.wait_until_idle();
    }
}

/// Mark the session at `session_root` as one that asked for the index, so that
/// each stored pass starts a build. For a benchmark of what a request after
/// the pass costs.
#[doc(hidden)]
pub fn want_subtype_index(host: &SessionHost, session_root: &Path) {
    if let Some(slot) = slot_of(host, session_root) {
        slot.want();
    }
}

/// How many builds the session at `session_root` has started. For a
/// benchmark that must time no build, or one build.
#[doc(hidden)]
pub fn subtype_index_builds(host: &SessionHost, session_root: &Path) -> u64 {
    slot_of(host, session_root).map_or(0, |slot| slot.lock().tickets)
}

/// Build the subtype index of the stored pass on the calling thread, and keep
/// it. The number of class files read, or `None` when the kept index is
/// current or another build runs. For a benchmark: CodSpeed counts only the
/// thread that runs the bench.
#[doc(hidden)]
pub fn build_subtype_index_here(host: &SessionHost, session_root: &Path) -> Option<usize> {
    let captured = capture(host, session_root)?;
    let graph = Arc::clone(captured.stored_graph()?);
    let stamp = Stamp::of(&captured, &graph);
    let slot = Arc::clone(&captured.slot);
    let Claim::Start(build) = slot.claim(&stamp, Some(&graph)) else {
        return None;
    };
    let classes = slot.classes(&graph);
    let result = run(&captured, &classes, &build, session_root);
    drop(captured);
    let outcome = slot.finish(&build, result);
    matches!(outcome, Outcome::Built).then(|| build.total.load(Ordering::Relaxed))
}
