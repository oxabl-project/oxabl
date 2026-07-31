//! Coarse salsa 0.28 substrate (KTD2) with the write-on-main / read-on-snapshot
//! threading discipline (KTD7).
//!
//! The graph over one *open buffer* is deliberately coarse — one input per open
//! file, two tracked queries — while the graph over the *workspace files it
//! references* is fine-grained, one input per referenced file:
//!
//! ```text
//! Buffer(text, path) ──▶ expanded_text ──▶ diagnostics ◀── SchemaHandle(revision)
//!                                              │
//!                                              ▼ (one edge per file consulted)
//!             IndexedFile(disk_revision) ──▶ indexed_facts ──▶ indexed_class
//!                                                          ├─▶ indexed_class_members
//!                                                          ├─▶ indexed_program
//!                                                          └─▶ indexed_defines_shared
//! ```
//!
//! - [`Buffer`] — a per-file salsa input holding the open buffer's text and the
//!   path it is a buffer of.
//! - [`SchemaHandle`] — a salsa input carrying a bumpable revision; bumping it
//!   invalidates the diagnostics query so a `.df` change hot-reloads without a
//!   restart (R16) and without re-setting every buffer.
//! - `expanded_text` — the shared pipeline's **first phase**, preprocessing the
//!   buffer into an [`Expansion`]; memoized so an edit with byte-identical
//!   expansion cuts off `diagnostics` early.
//! - `diagnostics` — the shared pipeline's **second phase** over that expansion.
//! - [`IndexedFile`] — **one input per workspace file the index has been asked
//!   about**, and the reason a dependency edit invalidates its dependents and
//!   nobody else (KTD12, R10). Explicitly *not* the `SchemaHandle`: one global
//!   handle would invalidate every open buffer on any indexed-file change.
//! - `indexed_*` — the four seam questions as tracked functions, keyed per
//!   referenced file and per class or name, which is where R9's early cutoff
//!   comes from: editing a method body in a parent re-extracts that parent's
//!   facts, they compare equal, and the buffers that inherit from it never
//!   recompute.
//!
//! Salsa's dependency graph **is** the cross-file reverse-dependency index. The
//! server keeps a hand-rolled path map for include files because a changed `.i`
//! has to force the *expansion* to re-read, which is a re-trigger rather than an
//! invalidation; a changed workspace file needs no such map, because bumping its
//! one input is already precise. See `Server::handle_watched_files`.
//!
//! The two queries are the *only* orchestration left here: expanding, parsing,
//! analyzing, and linting all live in [`oxabl_pipeline`], which the CLI and the
//! browser drive through the same handle. This crate holds the pipeline, decides
//! *when* to run each phase, and renders the result.
//!
//! ## Why two phases and not one `run` (KTD2)
//!
//! [`LintPipeline::run`] would be shorter, and it is what the non-incremental
//! clients call — but it is wrong here twice over. The intermediate
//! [`Expansion`] is what [`buffer_dependencies`] reads for the watcher's
//! changed-include → buffer matching, and memoizing it is what gives salsa its
//! early cutoff: [`Expansion`] is `Clone + PartialEq + Eq` precisely so an edit
//! whose expanded text is byte-identical backdates and skips the expensive
//! parse/semantic/lint entirely. `run` is also *guarded*, and a `catch_panic`
//! inside the salsa call would swallow the [`salsa::Cancelled`] unwind — turning
//! "abandon this stale snapshot" into "this file failed to analyze" and
//! publishing stale diagnostics. [`LintPipeline::expand`] and
//! [`LintPipeline::collect`] are unguarded on purpose so this crate can layer
//! its own guard *outside* its cancellation catch (KTD6); see
//! [`crate::analyze_guarded`].
//!
//! The resolved [`PipelineConfig`], the filesystem, and the preprocess switch
//! are **not** salsa inputs — they are plain db configuration (R17). Changing
//! them requires re-triggering affected buffers, which the watcher does; the
//! schema *revision* handle is the one exception that auto-invalidates.
//!
//! ## Threading (KTD7)
//!
//! Writes (`set_*` on `&mut db`) run on the main loop. Each diagnostics
//! computation runs on a **cloned snapshot** (`db.clone()`) on a worker thread
//! and is wrapped in [`salsa::Cancelled::catch`]: a concurrent write on the
//! main thread cancels in-flight snapshot reads via a `Cancelled` unwind, which
//! [`compute_diagnostics`] swallows. [`buffer_dependencies`] carries its own
//! catch for the same reason — every entry point that reads a snapshot needs
//! one, or a cancellation escapes as a panic. Version tag-and-discard in the
//! debounce layer (U6) is the primary correctness mechanism.
//!
//! Cancellation is **not** free on top of it: salsa flags every live snapshot,
//! not just the one whose buffer was written, so an edit in one file cancels
//! every other file's in-flight computation. A cancelled computation is
//! *abandoned work on a buffer that may not have changed at all*, so the server
//! re-arms that buffer's debounce timer when a cancelled result arrives
//! ([`crate::Server::handle_result`]) — dropping it would leave the untouched
//! file displaying pre-edit diagnostics until the user typed in it.
//!
//! **Those two are still the only snapshot-reading entry points.** The index
//! queries are reached exclusively from *inside* `diagnostics` — that nesting is
//! what forms the dependency edges in the first place — so they are covered by
//! `compute_diagnostics`'s catch and must not carry one of their own: a catch
//! inside a lookup would turn a cancelled recompute into `NotFound` and publish a
//! buffer with its cross-file names silently unresolved. The two `#[cfg(test)]`
//! helpers below read a snapshot without a catch because they run on the main
//! database with no concurrent writer; anything `pub` would take on the
//! obligation, which is why neither is.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use oxabl_analyze::CollectedDiagnostics;
use oxabl_index::search::{self, NameKind};
use oxabl_index::{FileFacts, index_file};
use oxabl_pipeline::{Expansion, LintPipeline, PipelineConfig};
use oxabl_semantic::{
    ClassDescriptor, IndexAnswer, IndexName, IndexRevision, IndexedFileId, MemberDescriptor,
    WorkspaceIndex,
};
use oxabl_workspace::{FileSystem, RealFileSystem};
use salsa::{Setter, Storage};

/// The root buffer's file id, re-exported from its single owner (KTD12).
///
/// Each open file is analyzed independently (no cross-file salsa), and the
/// preprocessor assigns include file ids starting at `root + 1`, so a fixed root
/// of 1 never collides with an include. This crate used to declare its own
/// `FileId::new(1)` that happened to agree with the umbrella's; the pipeline now
/// owns the constant and both point here.
pub use oxabl_pipeline::ROOT_FILE_ID;

/// Per-file salsa input: the open buffer's current text, and the path it is a
/// buffer *of*.
///
/// The path never changes for the life of the input — a rename closes one
/// document and opens another — so reading it inside a query costs a dependency
/// edge that can never fire. It is here rather than in a side map because the
/// one place that needs it is the query: cross-file resolution has to know which
/// file it is analysing so that file can be excluded from its own lookups, and a
/// buffer is the unsaved text while the index answers from disk (see
/// [`SnapshotIndex`]).
///
/// `None` for a document whose URI is not a `file:` path — the server keeps
/// serving it, with no self-exclusion to apply.
#[salsa::input]
pub struct Buffer {
    #[returns(ref)]
    pub text: String,
    #[returns(ref)]
    pub path: Option<PathBuf>,
}

/// Per-file salsa input for **one file the cross-file index has been asked
/// about** (KTD12).
///
/// This is what makes a dependency edit invalidate its dependents at per-file
/// granularity (R10). The alternative — reusing the single [`SchemaHandle`]
/// revision every buffer's query already reads — would invalidate *every* open
/// buffer whenever *any* indexed file changed, which is the opposite of the
/// early cutoff R9 asks for.
///
/// - `path` and `index_id` are immutable: assigned when the file is first
///   registered and never touched again.
/// - `disk_revision` is the invalidation handle. The facts query reads it and
///   then reads the file through the configured [`FileSystem`] — the same
///   revision-handle-plus-untracked-read shape [`SchemaHandle`] uses, and for the
///   same reason: the *content* is not something the server can hand salsa, since
///   the server does not know a file is interesting until a lookup reaches it.
///   Bumping it (from [`IndexHandle::bump`]) is therefore the whole invalidation
///   protocol.
#[salsa::input]
pub struct IndexedFile {
    #[returns(ref)]
    pub path: PathBuf,
    /// The identity this file answers under, in the *index's* id space.
    pub index_id: u32,
    /// Bumped when the watcher reports this file changed on disk.
    pub disk_revision: u32,
}

/// Salsa input carrying a monotonically-bumped schema revision. The diagnostics
/// query reads it so bumping it (on a `.df` change) invalidates and recomputes
/// live (R16). The schema *value* itself is db configuration, not a salsa field.
#[salsa::input]
pub struct SchemaHandle {
    pub revision: u32,
}

/// The cross-file index's handle: the path → [`IndexedFile`] registry, and the
/// disk revision each of those inputs is currently set to.
///
/// # Why this and not an `Arc<dyn WorkspaceIndex>`
///
/// KTD8 asks for an `Arc`-shaped index handle in [`AnalysisConfig`], because the
/// configuration is cloned on every debounced recompute and `fs` and `pipeline`
/// are already `Arc` so that clone stays a pointer bump. This is that handle. It
/// is not the trait object itself, and it cannot be: answering a query means
/// *calling* tracked functions, so the answering value has to borrow a database
/// handle — and salsa deliberately makes a database `Send` but not `Sync`. So the
/// long-lived, shared, cloned-per-recompute part is this registry, and the
/// borrowing [`SnapshotIndex`] view is built inside the query that uses it.
///
/// # Why the registry survives a configuration change
///
/// The inputs live in the database; this map is the only way back to them. A
/// re-resolved configuration (an `oxabl.toml` or `.df` change) installs a fresh
/// [`AnalysisConfig`], and it must carry the *same* handle forward — a new one
/// would orphan every input, so a later disk change would find nothing to bump
/// and dependents would silently keep stale answers. See
/// `Server::install_config`.
#[derive(Default)]
pub struct IndexHandle {
    /// Interior mutability because registration happens *inside* a query, which
    /// holds only `&self` on the whole configuration.
    ///
    /// Not a hot-path lock: it is taken once per cross-file name that resolves to
    /// a path, not per token or per node, and only the first time each file is
    /// seen does it take the write side for a real insert. Poisoning is recovered
    /// from rather than propagated, for the same reason `oxabl_index`'s batch memo
    /// does it — a `salsa::Cancelled` travels as a panic payload, and a cancelled
    /// lookup must not leave the index permanently unusable.
    registry: RwLock<Registry>,
}

#[derive(Default)]
struct Registry {
    by_path: HashMap<PathBuf, Registered>,
    /// Ids are minted from 1 so a zero never reads as a real file in a dump,
    /// matching the batch index.
    next_id: u32,
}

/// One registered file: its salsa input, and the revision that input is set to.
///
/// The revision is mirrored here rather than read back from the database, so
/// bumping needs no database read at all — which keeps the watcher path off the
/// query graph entirely.
#[derive(Clone, Copy)]
struct Registered {
    input: IndexedFile,
    /// Mirrored for the same reason: the `SHARED`-producer scan needs every
    /// registered file's id, and reading them back one field at a time would put
    /// a database read on a path that has no snapshot to read from.
    index_id: u32,
    disk_revision: u32,
}

impl IndexHandle {
    /// An empty registry. Nothing is read, and no file is known, until a lookup
    /// reaches one (R6).
    pub fn new() -> Self {
        IndexHandle::default()
    }

    /// The registry, recovering from poisoning — see the `registry` field.
    fn registry(&self) -> std::sync::RwLockWriteGuard<'_, Registry> {
        self.registry
            .write()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// The input for `path`, registering it on first sight.
    ///
    /// # Why an input may be created from inside a query
    ///
    /// Creating a salsa *input* allocates a value and nothing else: it does not
    /// bump the revision, does not cancel snapshots, and needs only `&db` (unlike
    /// `set_*`, which needs `&mut`). That is what makes lazy registration possible
    /// at all, and lazy registration is what R6 requires — the server cannot know
    /// which files matter without scanning the workspace, and a scan is exactly
    /// what "resolution proceeds outward from what was asked for" forbids.
    ///
    /// The map is what keeps the identity stable across re-executions: a query
    /// that runs again finds the entry rather than minting a second input for one
    /// file. Keyed on the *lexically normalized* path, for the same reason the
    /// batch memo is — two spellings of one file must not become two ids, or the
    /// exclusion check and the `SHARED`-producer scan both start lying.
    fn file_for(&self, db: &dyn AblDatabase, path: &Path) -> IndexedFile {
        let key = search::normalize_lexically(path);
        let mut registry = self.registry();
        if let Some(hit) = registry.by_path.get(&key) {
            return hit.input;
        }
        registry.next_id += 1;
        let index_id = registry.next_id;
        let input = IndexedFile::new(db, path.to_path_buf(), index_id, 0);
        registry.by_path.insert(
            key,
            Registered {
                input,
                index_id,
                disk_revision: 0,
            },
        );
        input
    }

    /// Every registered file, as `(input, id)` pairs.
    ///
    /// Snapshotted out from under the lock so the caller can run tracked queries
    /// without holding it — a query may unwind on cancellation, and unwinding
    /// through a held write guard is what poisons the lock.
    fn registered(&self) -> Vec<(IndexedFile, IndexedFileId)> {
        self.registry()
            .by_path
            .values()
            .map(|r| (r.input, IndexedFileId::new(r.index_id)))
            .collect()
    }

    /// Bump the disk revision of the input for `path`, invalidating exactly the
    /// queries that read that file — and nothing else (R10).
    ///
    /// Returns `false` when `path` is not registered, which is the common case and
    /// a genuine early-out: a file no lookup has ever reached cannot be any
    /// buffer's dependency, so there is nothing to invalidate and no buffer to
    /// re-run.
    pub fn bump(&self, db: &mut AnalysisDatabase, path: &Path) -> bool {
        let key = search::normalize_lexically(path);
        let mut registry = self.registry();
        let Some(entry) = registry.by_path.get_mut(&key) else {
            return false;
        };
        entry.disk_revision = entry.disk_revision.wrapping_add(1);
        let (input, revision) = (entry.input, entry.disk_revision);
        // The guard is dropped before the write: `set_*` waits for outstanding
        // snapshots to drop, and a worker thread blocked on this very lock while
        // holding a snapshot would deadlock.
        drop(registry);
        input.set_disk_revision(db).to(revision);
        true
    }

    /// Whether `path` has been registered — i.e. whether any lookup has ever
    /// reached it.
    pub fn knows(&self, path: &Path) -> bool {
        let key = search::normalize_lexically(path);
        self.registry().by_path.contains_key(&key)
    }
}

/// Non-salsa db configuration shared by every query on a snapshot.
///
/// The resolved [`PipelineConfig`] — include paths, lint severities, style, and
/// the schema, all from **one** read of `oxabl.toml` (KTD3) — sits behind an
/// `Arc` for two reasons that both matter on the interactive path: cloning a
/// snapshot per debounced recompute is a pointer bump rather than a `Schema`
/// clone, and replacing the configuration on a watcher event is a pointer swap.
///
/// The two fields beside it are exactly what `PipelineConfig` deliberately does
/// not own: the filesystem include reads go through, and whether to preprocess
/// at all.
#[derive(Clone)]
pub struct AnalysisConfig {
    /// Filesystem used to read include files (real disk for the live server).
    pub fs: Arc<dyn FileSystem>,
    /// The one resolved configuration every phase runs under.
    pub pipeline: Arc<PipelineConfig>,
    /// Whether to run the preprocessor (always `true` for the live server, R6;
    /// the threading tests turn it off to isolate the substrate).
    pub preprocess: bool,
    /// The cross-file index's per-file input registry (KTD8). `Arc` for exactly
    /// the reason the two fields above are: this is cloned per debounced
    /// recompute, and it must be the *same* registry across configuration
    /// changes — see [`IndexHandle`].
    pub index: Arc<IndexHandle>,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        AnalysisConfig {
            fs: Arc::new(RealFileSystem),
            pipeline: Arc::new(PipelineConfig::default()),
            preprocess: true,
            index: Arc::new(IndexHandle::new()),
        }
    }
}

impl AnalysisConfig {
    /// The shared lint pipeline for this configuration.
    ///
    /// Built per query rather than stored, and that is the cheap direction:
    /// [`LintPipeline`] *borrows* its config and its filesystem, so this is two
    /// pointers and a bool — no `Schema` clone, no per-run allocation on the
    /// keystroke path (the 50ms interactivity budget, R14).
    pub fn lint_pipeline(&self) -> LintPipeline<'_> {
        LintPipeline::new(&self.pipeline, &*self.fs).with_preprocess(self.preprocess)
    }
}

/// Access to the non-salsa configuration from inside a query.
pub trait HasConfig {
    fn config(&self) -> &AnalysisConfig;
}

/// The database trait tracked queries are written against.
#[salsa::db]
pub trait AblDatabase: HasConfig + salsa::Database {}

#[salsa::db]
impl<T: HasConfig + salsa::Database> AblDatabase for T {}

/// The concrete analysis database. Cloning it produces a salsa snapshot that
/// shares memoized state for reads (KTD7).
#[salsa::db]
#[derive(Clone)]
pub struct AnalysisDatabase {
    storage: Storage<Self>,
    config: AnalysisConfig,
}

impl AnalysisDatabase {
    /// Create a database with the given configuration.
    pub fn new(config: AnalysisConfig) -> Self {
        AnalysisDatabase {
            storage: Storage::default(),
            config,
        }
    }

    /// Replace the non-salsa configuration (include paths, schema value, lint
    /// severities, …). Callers must re-trigger affected buffers afterwards
    /// (bump their input) so memoized results recompute (R17).
    pub fn set_config(&mut self, config: AnalysisConfig) {
        self.config = config;
    }

    /// Read the current configuration.
    pub fn config(&self) -> &AnalysisConfig {
        &self.config
    }

    /// A database that records the `Debug` rendering of every query salsa
    /// actually **executes**, so a test can assert an early cutoff rather than
    /// merely assert that two results happen to be equal (KTD2).
    ///
    /// Test-only, and deliberately not part of the public surface: the live
    /// server has no use for an event sink, and adding one would put a callback
    /// on the hottest path in the product.
    #[cfg(test)]
    pub(crate) fn recording(
        config: AnalysisConfig,
        executed: Arc<std::sync::Mutex<Vec<String>>>,
    ) -> Self {
        let sink = move |event: salsa::Event| {
            if let salsa::EventKind::WillExecute { database_key } = event.kind {
                executed.lock().unwrap().push(format!("{database_key:?}"));
            }
        };
        AnalysisDatabase {
            storage: Storage::new(Some(Box::new(sink))),
            config,
        }
    }
}

impl HasConfig for AnalysisDatabase {
    fn config(&self) -> &AnalysisConfig {
        &self.config
    }
}

#[salsa::db]
impl salsa::Database for AnalysisDatabase {}

// ---------------------------------------------------------------------------
// The cross-file index, backed by the incremental machinery
// ---------------------------------------------------------------------------

/// The generation this backend reports.
///
/// A constant, and deliberately so. [`IndexRevision`] exists for a consumer
/// holding results across an index *rebuild*, which is a batch cache's whole
/// staleness story — and it is the wrong mechanism here: this backend never
/// rebuilds, it invalidates per file, and a counter bumped on every disk change
/// would be a second global handle read by every buffer, which is exactly what
/// KTD12 removed. Salsa is the staleness mechanism; the number only has to say
/// "an index is present", which is what [`IndexRevision::ABSENT`] is the absence
/// of.
const SALSA_INDEX_REVISION: IndexRevision = IndexRevision::new(1);

/// The facts one indexed file contributes, memoized per file (R8, R10).
///
/// Reading `disk_revision` first is the dependency edge that makes an edit to
/// this file invalidate every buffer that consulted it — and only those buffers.
/// The file's *content* is then read through the configured filesystem, which is
/// an untracked read gated by that revision: the server cannot supply the content
/// as an input because it does not know a file is interesting until a lookup
/// reaches it.
///
/// Extraction is [`oxabl_index::index_file`] — the same routine the batch cache
/// calls. Only the memoization differs (KTD2), which is what makes "every client
/// resolves identically" structural rather than documented.
///
/// Unguarded, like every query here: a `salsa::Cancelled` travels as a panic
/// payload and must reach the entry point's catch.
#[salsa::tracked(returns(clone))]
fn indexed_facts(db: &dyn AblDatabase, file: IndexedFile) -> Arc<FileFacts> {
    // Establish the dependency edge on this file's disk revision *first*. Every
    // answer below is derived from it, and it is the whole invalidation protocol
    // (R10) — so it is read unconditionally, including on the unreadable-file path.
    let _revision = file.disk_revision(db);
    let id = IndexedFileId::new(*file.index_id(db));
    let path = file.path(db);
    let Ok(source) = db.config().fs.read(path) else {
        // Located but unreadable is *knowably unusable*, which the seam spells
        // `NotFound` — not an error, and remembered so the read is not retried on
        // every reference until the file changes.
        return Arc::new(FileFacts::unparseable(id));
    };
    // The injection seam, ahead of the expensive work, and keyed on the
    // *referenced* file's text — so the test drives it by putting the marker in a
    // sibling file, not in the buffer.
    oxabl_common::panic_if_injected(oxabl_common::panic_sites::LSP_INDEX, &source);
    // Cooperative cancellation checkpoint (KTD7), in the same position as the one
    // in `diagnostics`: after the cheap read, before the parse. A concurrent
    // main-thread write has already made this answer irrelevant.
    db.unwind_if_revision_cancelled();
    Arc::new(index_file(id, &source))
}

/// One class's descriptor, keyed per file **and per class** (R9).
///
/// The per-class key is what buys early cutoff: [`indexed_facts`] re-executes
/// whenever its file's revision moves, but an equal `FileFacts` backdates, so an
/// edit that leaves this class's header alone stops here rather than reaching the
/// buffers that inherit from it.
#[salsa::tracked(returns(clone))]
fn indexed_class(
    db: &dyn AblDatabase,
    file: IndexedFile,
    name: IndexName,
) -> IndexAnswer<Arc<ClassDescriptor>> {
    // The file exists but may not declare the class its path promised — a
    // mis-namespaced file, or one whose parse recovered errors. Both are knowably
    // unusable, so both are `NotFound`, exactly as in the batch cache.
    indexed_facts(db, file)
        .class(&name)
        .map_or(IndexAnswer::NotFound, |facts| {
            IndexAnswer::Found(Arc::clone(&facts.descriptor))
        })
}

/// One class's own member list, keyed per file and per class.
///
/// A separate query from [`indexed_class`] rather than a projection of it,
/// because they invalidate independently: adding a member changes this answer and
/// not the descriptor, and changing `INHERITS` changes the descriptor and not
/// this. Both read the same `indexed_facts` memo, so the consumer's
/// descriptor-then-members pair still costs one extraction.
#[salsa::tracked(returns(clone))]
fn indexed_class_members(
    db: &dyn AblDatabase,
    file: IndexedFile,
    name: IndexName,
) -> IndexAnswer<Arc<[MemberDescriptor]>> {
    indexed_facts(db, file)
        .class(&name)
        .map_or(IndexAnswer::NotFound, |facts| {
            IndexAnswer::Found(Arc::clone(&facts.members))
        })
}

/// Whether a located file is usable as a `RUN` target, keyed per file.
///
/// A broken file answers `NotFound` — the seam's rule that a file we cannot use
/// is *not found* rather than an error.
#[salsa::tracked(returns(clone))]
fn indexed_program(db: &dyn AblDatabase, file: IndexedFile) -> IndexAnswer<IndexedFileId> {
    let facts = indexed_facts(db, file);
    if facts.parsed {
        IndexAnswer::Found(facts.file)
    } else {
        IndexAnswer::NotFound
    }
}

/// Whether one file produces a named `SHARED` definition, keyed per file and per
/// name.
///
/// Per name rather than "hand back the whole list", so the producer scan over the
/// registry gets early cutoff per pair: an edit elsewhere in a producing file
/// leaves every consumer's answer backdated.
#[salsa::tracked(returns(clone))]
fn indexed_defines_shared(db: &dyn AblDatabase, file: IndexedFile, name: IndexName) -> bool {
    indexed_facts(db, file).defines_shared(&name)
}

/// The [`WorkspaceIndex`] the language server answers from: the shared questions,
/// the shared search, and the shared extraction — memoized by salsa, per file.
///
/// Built *inside* the query that uses it and thrown away with it, because it
/// borrows the database handle. That is also why the trait is not `Send + Sync`:
/// salsa makes a database handle single-threaded on purpose (each worker clones
/// its own snapshot), so no value that can answer from one could ever be `Sync`.
/// The long-lived half — the registry of per-file inputs — is the `Arc` in
/// [`AnalysisConfig`].
///
/// # The search is deliberately *not* memoized
///
/// Turning a name into a path (`oxabl_index::search::find_name`) runs on every
/// execution of the query that asked. Memoizing it would key on the name and
/// would then have no way to notice a file *appearing* on the search paths: a
/// recorded `NotFound` would outlive the creation of the very file that answers
/// it, and there is no per-file input to bump for a file that was never
/// registered. Re-running it costs a couple of existence checks per unresolved
/// cross-file name, only on the executions that were going to happen anyway,
/// while the part that is actually expensive — parsing and declaring the
/// referenced file — sits behind [`indexed_facts`] (R12).
struct SnapshotIndex<'db> {
    db: &'db dyn AblDatabase,
    /// The analysed buffer's own path, lexically normalized, or `None` when the
    /// document has no `file:` path.
    analysed: Option<PathBuf>,
}

impl<'db> SnapshotIndex<'db> {
    fn new(db: &'db dyn AblDatabase, analysed: Option<&Path>) -> Self {
        SnapshotIndex {
            db,
            analysed: analysed.map(search::normalize_lexically),
        }
    }

    /// Locate `name` and register the file it names, or decline.
    ///
    /// The self-exclusion is applied **here**, before registration, and that is
    /// the one place this backend differs from the batch cache — which has to
    /// compare ids after the fact because its search is buried inside its memo.
    /// Deciding on the path is both simpler and stricter: the analysed file is
    /// never even registered by its own lookups.
    ///
    /// Why exclude at all: the client analyses a *buffer* — for an editor, unsaved
    /// text — while the index answers from disk. Resolving a name to the analysed
    /// file would attribute that file's own, possibly stale, disk copy to it as a
    /// foreign file: a class inheriting from itself, or a `SHARED` consumer linked
    /// to a producer that is really its own earlier revision.
    fn locate(&self, name: &IndexName, kind: NameKind) -> IndexAnswer<IndexedFile> {
        let config = self.db.config();
        match search::find_name(&*config.fs, &config.pipeline.include_paths, name, kind) {
            IndexAnswer::Found(path) => {
                if self.is_analysed(&path) {
                    return IndexAnswer::NotFound;
                }
                IndexAnswer::Found(config.index.file_for(self.db, &path))
            }
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        }
    }

    /// Whether `path` is the file being analysed. `false` with no identity, which
    /// is what makes a path-less document behave exactly like an unexcluded index.
    fn is_analysed(&self, path: &Path) -> bool {
        self.analysed
            .as_ref()
            .is_some_and(|analysed| *analysed == search::normalize_lexically(path))
    }
}

impl WorkspaceIndex for SnapshotIndex<'_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        match self.locate(name, NameKind::Class) {
            IndexAnswer::Found(file) => indexed_class(self.db, file, name.clone()),
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        }
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        match self.locate(class, NameKind::Class) {
            IndexAnswer::Found(file) => indexed_class_members(self.db, file, class.clone()),
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        }
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        match self.locate(target, NameKind::Program) {
            IndexAnswer::Found(file) => indexed_program(self.db, file),
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        }
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        // A `SHARED` name maps onto no path, so — exactly as in the batch cache —
        // the only honest answers are "enumerate the workspace", which R6 forbids
        // and the `FileSystem` trait cannot do anyway, or "consult the files
        // something already pulled in". This is that second set: the registry.
        //
        // The registry's *membership* is not a tracked dependency, and that is a
        // conservative rather than a correctness gap: a file registered later does
        // not retroactively turn an earlier `NotFound` into a hit until the asking
        // buffer recomputes for some other reason. A missing link produces no
        // finding (the R11 firewall), which is the same direction the batch cache
        // errs in when nothing has pulled the producer in yet.
        //
        // Two producers is an ambiguity of the same shape the literal-`RUN` rule
        // declines to guess at, so it declines too — which also keeps the answer
        // independent of map iteration order.
        let mut found = None;
        for (file, id) in self.db.config().index.registered() {
            if self.is_analysed(file.path(self.db)) {
                continue;
            }
            if !indexed_defines_shared(self.db, file, name.clone()) {
                continue;
            }
            match found {
                None => found = Some(id),
                Some(_) => return IndexAnswer::Unknowable,
            }
        }
        found.map_or(IndexAnswer::NotFound, IndexAnswer::Found)
    }

    fn revision(&self) -> IndexRevision {
        SALSA_INDEX_REVISION
    }
}

/// Phase one, memoized: the shared pipeline's expansion of a buffer. Carries the
/// loud root-origin preprocessor errors on its fatal-failure arm rather than
/// making the caller handle an `Err` before it can ask a question.
#[salsa::tracked(returns(clone))]
fn expanded_text(db: &dyn AblDatabase, buffer: Buffer) -> Expansion {
    db.config().lint_pipeline().expand(buffer.text(db).as_str())
}

/// Phase two: the shared pipeline's collection over the memoized expansion,
/// yielding the full root-resolved diagnostic set. Depends on the expansion and
/// on the schema handle's revision (so schema hot-reload invalidates, R16).
#[salsa::tracked(returns(clone))]
fn diagnostics(db: &dyn AblDatabase, buffer: Buffer, schema: SchemaHandle) -> CollectedDiagnostics {
    // Establish the dependency edge on the schema revision (R16 hot-reload).
    let _revision = schema.revision(db);
    let expansion = expanded_text(db, buffer);
    // Cooperative cancellation checkpoint (KTD7): if a concurrent main-thread
    // write cancelled this snapshot while we were expanding, unwind here — before
    // the expensive parse/semantic/lint — rather than compute a doomed result.
    // This sits *between* the two phases deliberately: it is the one point where
    // the cheap work is done and the expensive work has not started.
    db.unwind_if_revision_cancelled();
    // The cross-file index, built per execution because it borrows this snapshot
    // (see `SnapshotIndex`). Constructing it reads nothing; every lookup it
    // answers becomes a nested tracked call, which is how this buffer acquires
    // dependency edges on the files it consulted (R8).
    let index = SnapshotIndex::new(db, buffer.path(db).as_deref());
    // The pipeline's fatal-preprocessing arm already yields the preprocessor's
    // own diagnostics under `DiagnosticSource::Preproc`, so there is no second
    // mapping to keep in step here. `into_diagnostics` takes the set rather than
    // cloning it — this runs per keystroke.
    db.config()
        .lint_pipeline()
        .with_index(&index)
        .collect(&expansion)
        .into_diagnostics()
}

/// Compute diagnostics on a snapshot, swallowing a [`salsa::Cancelled`] unwind
/// (returns `None` when the read was cancelled by a concurrent write, KTD7).
///
/// Intended to run on a worker thread against a cloned snapshot; never call it
/// with the `&mut` main database.
pub fn compute_diagnostics(
    snapshot: &AnalysisDatabase,
    buffer: Buffer,
    schema: SchemaHandle,
) -> Option<CollectedDiagnostics> {
    // The snapshot holds two things that are not `RefUnwindSafe`: an
    // `Arc<dyn FileSystem>`, and now an `Arc<IndexHandle>` whose registry is
    // behind a lock. Salsa's own snapshots are designed to cross the
    // `catch_unwind` boundary of `Cancelled::catch`, and the index handle is safe
    // to observe after an unwind for the reason its lock recovers from poisoning:
    // the registry is a monotonically growing cache of pure identities, so a
    // partially updated one is still correct — a half-registered file is simply
    // one that is registered. So asserting unwind-safety here is sound.
    oxabl_common::panic_if_injected(
        oxabl_common::panic_sites::LSP_DIAGNOSTICS,
        buffer.text(snapshot),
    );
    salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
        diagnostics(snapshot, buffer, schema)
    }))
    .ok()
}

/// The include file paths a buffer transitively depends on, for the watcher's
/// `*.i` → buffer matching (R17). Computed via the memoized expansion query, so
/// this is cheap on an unchanged buffer. Empty on a fatal preprocessing error.
///
/// `None` means the snapshot read was **cancelled** by a concurrent main-thread
/// write, exactly as in [`compute_diagnostics`] — and for the same reason it
/// needs its own [`salsa::Cancelled::catch`] rather than borrowing the caller's
/// panic guard. Salsa unwinds at the top of every fetch once the cancellation
/// flag is set, so without this catch a cancellation here surfaces as a
/// contained *panic*: the server logs an ordinary race as a bug, and the caller
/// — which cannot tell the two apart — would commit the empty dependency set
/// that a panic degrades to. A buffer with no recorded dependencies is never
/// re-triggered by an edit to an `.i` it includes, so it silently stops being
/// re-analyzed until it is edited directly. An absent answer is the honest one:
/// the previous dependency set is still the best information available.
pub fn buffer_dependencies(snapshot: &AnalysisDatabase, buffer: Buffer) -> Option<Vec<PathBuf>> {
    oxabl_common::panic_if_injected(
        oxabl_common::panic_sites::LSP_DEPENDENCIES,
        buffer.text(snapshot),
    );
    // Unwind-safety is asserted for the same reason as in `compute_diagnostics`:
    // the snapshot's `Arc<dyn FileSystem>` and `Arc<IndexHandle>` are not
    // `RefUnwindSafe`, but salsa's snapshots are designed to cross
    // `Cancelled::catch`'s boundary and the index registry stays correct across
    // one — see the fuller note there.
    salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
        expanded_text(snapshot, buffer).dependency_paths().to_vec()
    }))
    .ok()
}

/// The tracked expansion query, exposed for tests that exercise early cutoff.
#[cfg(test)]
pub(crate) fn expanded_text_for_test(db: &AnalysisDatabase, buffer: Buffer) -> Expansion {
    expanded_text(db, buffer)
}

/// One index question driven directly, for the tests that pin per-key early
/// cutoff.
///
/// Test-only, and deliberately not a public entry point: the server has no reason
/// to ask the index anything outside a query — every real lookup is nested inside
/// `diagnostics`, which is what forms the dependency edges. A public sibling would
/// be a second way in, with its own cancellation-catch obligation and nothing
/// calling it.
#[cfg(test)]
pub(crate) fn indexed_members_for_test(
    db: &AnalysisDatabase,
    class: &str,
) -> IndexAnswer<Arc<[MemberDescriptor]>> {
    SnapshotIndex::new(db, None).class_members(&IndexName::new(class))
}

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    fn config_with(source_fs: oxabl_workspace::InMemoryFileSystem) -> AnalysisConfig {
        AnalysisConfig {
            fs: Arc::new(source_fs),
            preprocess: true,
            ..Default::default()
        }
    }

    fn db_with(source_fs: oxabl_workspace::InMemoryFileSystem) -> AnalysisDatabase {
        AnalysisDatabase::new(config_with(source_fs))
    }

    /// The shared guard both diagnostics paths go through (R8). Client-observable
    /// behavior cannot tell a contained panic from a dead worker thread, so the
    /// guard's real contract — *return normally* with both halves degraded, so
    /// the worker's later `send` is reached — is pinned here rather than e2e.
    ///
    /// Injected panics via `oxabl_common`'s test-only `test-panics` feature; no
    /// ABL input panics.
    #[test]
    fn analyze_guarded_contains_a_panic_in_either_query() {
        for site in [
            oxabl_common::panic_sites::LSP_DIAGNOSTICS,
            oxabl_common::panic_sites::LSP_DEPENDENCIES,
        ] {
            let db = db_with(oxabl_workspace::InMemoryFileSystem::new());
            let text =
                format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
            let buffer = Buffer::new(&db, text, None);
            let schema = SchemaHandle::new(&db, 0);

            let previous = std::panic::take_hook();
            std::panic::set_hook(Box::new(|_| {}));
            let analysis = crate::analyze_guarded(&db, buffer, schema, "file:///injected.p");
            std::panic::set_hook(previous);

            assert!(
                analysis.diagnostics.is_none(),
                "a panic at {site} must degrade to no diagnostics"
            );
            assert!(
                analysis.dependencies.is_none(),
                "a panic at {site} must degrade dependencies together with diagnostics"
            );
            assert!(
                analysis.panicked,
                "a panic at {site} must be reported as a panic, not as a cancellation — \
                 the scheduler retries the latter and must not retry the former"
            );
        }
    }

    /// The healthy shape of the dependency half: `Some`, so "no trustworthy
    /// answer" (`None`) stays distinguishable from "this file includes nothing"
    /// (`Some(vec![])`). The distinction is what keeps a cancelled run from
    /// erasing a buffer's watcher registration.
    #[test]
    fn dependencies_are_present_on_a_healthy_buffer() {
        let db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(&db, "MESSAGE \"hi\".\n".to_string(), None);

        assert_eq!(
            buffer_dependencies(&db, buffer),
            Some(Vec::new()),
            "a file with no includes has an empty set, not an absent one"
        );
    }

    /// The same helper on a healthy buffer still returns real results, so the
    /// guard has not swallowed the normal path.
    #[test]
    fn analyze_guarded_passes_through_a_healthy_buffer() {
        let db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(
            &db,
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
            None,
        );
        let schema = SchemaHandle::new(&db, 0);

        let analysis = crate::analyze_guarded(&db, buffer, schema, "file:///healthy.p");
        assert!(!analysis.panicked);
        let diagnostics = analysis
            .diagnostics
            .expect("a healthy buffer yields diagnostics");
        assert!(diagnostics.all().any(|c| c.diagnostic.code.0 == "LINT0002"));
    }

    #[test]
    fn diagnostics_recompute_on_change_memo_on_stable() {
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(
            &db,
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
            None,
        );
        let schema = SchemaHandle::new(&db, 0);

        // An unused variable → LINT0002.
        let d1 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(d1.all().any(|c| c.diagnostic.code.0 == "LINT0002"));

        // Recompute on unchanged input returns the same set (memo hit; equal).
        let d2 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert_eq!(d1, d2);

        // Editing the buffer to *read* x clears the unused-variable diagnostic.
        buffer
            .set_text(&mut db)
            .to("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n".to_string());
        let d3 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(
            !d3.all().any(|c| c.diagnostic.code.0 == "LINT0002"),
            "expected the unused-variable diagnostic to clear, got {:?}",
            d3.all().map(|c| c.diagnostic.code.0).collect::<Vec<_>>()
        );
    }

    #[test]
    fn early_cutoff_on_byte_identical_edit() {
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string();
        let buffer = Buffer::new(&db, text.clone(), None);

        let e1 = expanded_text_for_test(&db, buffer);

        // Setting the input to a byte-identical value bumps the input revision,
        // so `expanded_text` re-executes — but produces an equal `Expansion`,
        // which salsa backdates. This is the enabling property: `Expansion`
        // derives `PartialEq`/`Eq` for exactly this comparison (KTD2).
        buffer.set_text(&mut db).to(text);
        let e2 = expanded_text_for_test(&db, buffer);
        assert_eq!(e1, e2, "byte-identical edit must yield identical expansion");
    }

    /// The property the two-phase split exists for (KTD2): after a byte-identical
    /// edit the expansion re-executes and backdates, so the *expensive* second
    /// phase — parse, semantic, lint — is skipped entirely.
    ///
    /// Asserted on salsa's own execution events rather than on equal outputs,
    /// because equal outputs are also what a full recompute produces: only the
    /// event log can tell a cutoff from a coincidence. Rewiring the queries onto
    /// the pipeline is exactly the edit that could lose this.
    #[test]
    fn byte_identical_edit_skips_the_collect_phase() {
        let executed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let mut db = AnalysisDatabase::recording(
            config_with(oxabl_workspace::InMemoryFileSystem::new()),
            Arc::clone(&executed),
        );
        let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n".to_string();
        let buffer = Buffer::new(&db, text.clone(), None);
        let schema = SchemaHandle::new(&db, 0);

        let first = compute_diagnostics(&db, buffer, schema).unwrap();
        {
            let log = executed.lock().unwrap();
            assert!(
                log.iter().any(|e| e.contains("diagnostics")),
                "the cold run must execute the collect phase, got {log:?}"
            );
        }

        executed.lock().unwrap().clear();
        buffer.set_text(&mut db).to(text);
        let second = compute_diagnostics(&db, buffer, schema).unwrap();

        let log = executed.lock().unwrap();
        assert!(
            log.iter().any(|e| e.contains("expanded_text")),
            "the expansion itself re-executes (its input revision moved), got {log:?}"
        );
        assert!(
            !log.iter().any(|e| e.contains("diagnostics")),
            "an equal expansion must backdate and skip parse/semantic/lint, got {log:?}"
        );
        assert_eq!(first, second);
    }

    #[test]
    fn schema_handle_revision_invalidates() {
        // Bumping the schema handle recomputes diagnostics even though the
        // buffer is unchanged (the R16 hot-reload dependency edge exists).
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(&db, "MESSAGE \"hi\".\n".to_string(), None);
        let schema = SchemaHandle::new(&db, 0);

        let _ = compute_diagnostics(&db, buffer, schema).unwrap();
        schema.set_revision(&mut db).to(1);
        // Recompute succeeds after the bump (no panic / stale-handle issue).
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();
    }

    // -----------------------------------------------------------------------
    // The incremental cross-file index (U11: R8, R9, R10)
    // -----------------------------------------------------------------------

    /// A filesystem whose contents can change *after* the database was built —
    /// which is the whole subject here, and something `InMemoryFileSystem` cannot
    /// do because its `insert` takes `&mut self` and the config holds it behind an
    /// `Arc`.
    struct EditableFs {
        files: std::sync::RwLock<HashMap<PathBuf, String>>,
    }

    impl EditableFs {
        fn new(files: &[(&str, &str)]) -> Arc<Self> {
            Arc::new(EditableFs {
                files: std::sync::RwLock::new(
                    files
                        .iter()
                        .map(|(p, s)| (PathBuf::from(p), (*s).to_string()))
                        .collect(),
                ),
            })
        }

        /// Rewrite a file, as a save in another editor or a branch switch would.
        fn write(&self, path: &str, source: &str) {
            self.files
                .write()
                .unwrap()
                .insert(PathBuf::from(path), source.to_string());
        }
    }

    impl FileSystem for EditableFs {
        fn read(&self, path: &Path) -> Result<Arc<str>, std::io::Error> {
            self.files
                .read()
                .unwrap()
                .get(path)
                .map(|s| Arc::from(s.as_str()))
                .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::NotFound))
        }

        fn exists(&self, path: &Path) -> bool {
            self.files.read().unwrap().contains_key(path)
        }
    }

    /// A parent class with one public method. Synthetic.
    const PARENT: &str = "CLASS orders.calc-base:\n\
                          METHOD PUBLIC INTEGER calc-total():\n\
                          RETURN 0.\n\
                          END METHOD.\n\
                          END CLASS.";

    /// The same parent with a different method *body* and an identical header and
    /// member list — the edit whose facts compare equal, so an incremental cache
    /// must backdate rather than propagate.
    const PARENT_OTHER_BODY: &str = "CLASS orders.calc-base:\n\
                                     METHOD PUBLIC INTEGER calc-total():\n\
                                     RETURN 1 + 1.\n\
                                     END METHOD.\n\
                                     END CLASS.";

    /// The same parent with the method gone. Now the child's call resolves to
    /// nothing, which is the observable a stale cached answer would hide.
    const PARENT_WITHOUT_MEMBER: &str = "CLASS orders.calc-base:\n\
                                         END CLASS.";

    /// An unrelated class in an unrelated file, so "invalidates that parent and
    /// not this one" has two keys to distinguish.
    const OTHER: &str = "CLASS orders.audit-log:\n\
                         METHOD PUBLIC VOID note():\n\
                         END METHOD.\n\
                         END CLASS.";

    const PARENT_PATH: &str = "/src/orders/calc-base.cls";
    const OTHER_PATH: &str = "/src/orders/audit-log.cls";
    const CHILD_PATH: &str = "/src/orders/child.cls";

    /// A subclass calling the inherited method. With the parent resolved this is
    /// clean; with it unresolved — or with the member gone — the call is a
    /// `LINT0001` undefined-symbol finding.
    const CHILD: &str = "CLASS orders.child INHERITS orders.calc-base:\n\
                         METHOD PUBLIC VOID run-it():\n\
                         DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
                         v-total = calc-total().\n\
                         MESSAGE v-total.\n\
                         END METHOD.\n\
                         END CLASS.";

    fn searching_config(fs: Arc<dyn FileSystem>) -> AnalysisConfig {
        AnalysisConfig {
            fs,
            pipeline: Arc::new(PipelineConfig {
                include_paths: vec![PathBuf::from("/src")],
                ..PipelineConfig::default()
            }),
            preprocess: true,
            ..Default::default()
        }
    }

    fn undefined(diagnostics: &CollectedDiagnostics) -> bool {
        diagnostics.all().any(|c| c.diagnostic.code.0 == "LINT0001")
    }

    /// Bump one file's index input the way the watcher does. Cloned out of the
    /// configuration first because the bump needs `&mut db`.
    fn bump(db: &mut AnalysisDatabase, path: &str) -> bool {
        let index = Arc::clone(&db.config().index);
        index.bump(db, Path::new(path))
    }

    /// AE4 at the substrate level: the child resolves against the parent, the
    /// parent changes on disk, and the child's next computation reflects the
    /// change instead of the memoized answer.
    #[test]
    fn a_parent_change_on_disk_recomputes_the_dependent_buffer() {
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT), (CHILD_PATH, PARENT)]);
        let mut db =
            AnalysisDatabase::new(searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>));
        let buffer = Buffer::new(
            &db,
            CHILD.to_string(),
            Some(PathBuf::from("/src/orders/child.cls")),
        );
        let schema = SchemaHandle::new(&db, 0);

        let before = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(
            !undefined(&before),
            "the inherited call resolves through the index, got {:?}",
            before
                .all()
                .map(|c| c.diagnostic.code.0)
                .collect::<Vec<_>>()
        );

        // The parent loses the method, and the watcher reports it.
        fs.write(PARENT_PATH, PARENT_WITHOUT_MEMBER);
        assert!(bump(&mut db, PARENT_PATH), "the parent was registered");

        let after = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(
            undefined(&after),
            "a dependency edit must reach the dependent buffer, not be served from \
             the memo: {:?}",
            after.all().map(|c| c.diagnostic.code.0).collect::<Vec<_>>()
        );
    }

    /// The other half of R10: a change to a file this buffer never consulted must
    /// not recompute it. Asserted on salsa's execution log, because equal
    /// diagnostics are also what a full recompute produces — only the log can tell
    /// a validated memo from a repeat of the work.
    #[test]
    fn an_unrelated_file_change_does_not_recompute_the_buffer() {
        let executed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT), (OTHER_PATH, OTHER)]);
        let mut db = AnalysisDatabase::recording(
            searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>),
            Arc::clone(&executed),
        );
        let buffer = Buffer::new(&db, CHILD.to_string(), Some(PathBuf::from(CHILD_PATH)));
        let schema = SchemaHandle::new(&db, 0);
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();

        // Pull the unrelated file into the index so the change below has something
        // to bump — a file nothing ever looked at would be filtered out one step
        // earlier, which would make this a weaker test.
        assert!(matches!(
            indexed_members_for_test(&db, "orders.audit-log"),
            IndexAnswer::Found(_)
        ));

        executed.lock().unwrap().clear();
        fs.write(OTHER_PATH, "CLASS orders.audit-log:\nEND CLASS.");
        assert!(bump(&mut db, OTHER_PATH));
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();

        let log = executed.lock().unwrap();
        assert!(
            !log.iter().any(|e| e.contains("diagnostics")),
            "the buffer never consulted that file, so nothing about it may \
             recompute, got {log:?}"
        );
    }

    /// Per-key early cutoff, the sharp version: an edit to a parent's method
    /// **body** re-extracts that parent's facts, they compare equal, and the
    /// member-list query — which every consumer reads — never runs again.
    #[test]
    fn a_method_body_edit_backdates_before_reaching_the_member_list() {
        let executed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT), (OTHER_PATH, OTHER)]);
        let mut db = AnalysisDatabase::recording(
            searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>),
            Arc::clone(&executed),
        );
        let first = indexed_members_for_test(&db, "orders.calc-base");
        assert!(matches!(first, IndexAnswer::Found(_)));

        executed.lock().unwrap().clear();
        fs.write(PARENT_PATH, PARENT_OTHER_BODY);
        assert!(bump(&mut db, PARENT_PATH));
        let second = indexed_members_for_test(&db, "orders.calc-base");

        let log = executed.lock().unwrap();
        assert!(
            log.iter().any(|e| e.contains("indexed_facts")),
            "the file's revision moved, so its facts are re-extracted, got {log:?}"
        );
        assert!(
            !log.iter().any(|e| e.contains("indexed_class_members")),
            "equal facts must backdate, so the member list is not recomputed, \
             got {log:?}"
        );
        assert_eq!(first, second);
    }

    /// And when the member list genuinely *does* change, exactly one key
    /// recomputes: the edited parent's, not the unrelated class's.
    #[test]
    fn a_member_list_change_recomputes_one_key_not_both() {
        let executed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT), (OTHER_PATH, OTHER)]);
        let mut db = AnalysisDatabase::recording(
            searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>),
            Arc::clone(&executed),
        );
        let other_before = indexed_members_for_test(&db, "orders.audit-log");
        let _ = indexed_members_for_test(&db, "orders.calc-base");

        executed.lock().unwrap().clear();
        fs.write(PARENT_PATH, PARENT_WITHOUT_MEMBER);
        assert!(bump(&mut db, PARENT_PATH));
        let parent_after = indexed_members_for_test(&db, "orders.calc-base");
        let other_after = indexed_members_for_test(&db, "orders.audit-log");

        let log = executed.lock().unwrap();
        assert_eq!(
            log.iter()
                .filter(|e| e.contains("indexed_class_members"))
                .count(),
            1,
            "one member list changed, so one key recomputes, got {log:?}"
        );
        assert_eq!(
            log.iter().filter(|e| e.contains("indexed_facts")).count(),
            1,
            "and only the edited file is re-extracted, got {log:?}"
        );
        // The class is still declared, so it is still found — with an empty member
        // list. That distinction is the answer that changed.
        assert_eq!(
            parent_after,
            IndexAnswer::Found(Arc::from([] as [MemberDescriptor; 0])),
            "the class still exists and now declares nothing"
        );
        assert_eq!(
            other_after, other_before,
            "the unrelated class's answer is untouched"
        );
    }

    /// The self-exclusion, in this backend: the buffer *is* the parent's file, and
    /// the buffer now declares a subclass of the class that file used to hold.
    /// Resolving it would inherit from the buffer's own — possibly stale — copy on
    /// disk.
    #[test]
    fn a_buffer_does_not_resolve_a_class_to_its_own_file_on_disk() {
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT)]);
        let db = AnalysisDatabase::new(searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>));
        let schema = SchemaHandle::new(&db, 0);

        let itself = Buffer::new(
            &db,
            CHILD.to_string(),
            Some(PathBuf::from("/src/orders/./calc-base.cls")),
        );
        assert!(
            undefined(&compute_diagnostics(&db, itself, schema).unwrap()),
            "a file must not resolve a class to itself — including through a \
             differently spelled path"
        );

        // The control: any other file's buffer still resolves it.
        let neighbour = Buffer::new(&db, CHILD.to_string(), Some(PathBuf::from(CHILD_PATH)));
        assert!(!undefined(
            &compute_diagnostics(&db, neighbour, schema).unwrap()
        ));

        // And a document with no path at all behaves like an unexcluded index,
        // which is the browser-shaped case.
        let anonymous = Buffer::new(&db, CHILD.to_string(), None);
        assert!(!undefined(
            &compute_diagnostics(&db, anonymous, schema).unwrap()
        ));
    }

    /// A panic while indexing somebody *else's* file is contained and reports no
    /// answer — never an empty diagnostic set committed as if the buffer were
    /// clean. The marker lives in the referenced file, which is what makes this a
    /// test of the index query rather than of the two guards already pinned above.
    #[test]
    fn a_panic_while_indexing_a_referenced_file_is_contained() {
        let site = oxabl_common::panic_sites::LSP_INDEX;
        let poisoned = format!("/* OXABL-TEST-PANIC:{site} */\n{PARENT}");
        let fs = EditableFs::new(&[(PARENT_PATH, &poisoned)]);
        let db = AnalysisDatabase::new(searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>));
        let buffer = Buffer::new(&db, CHILD.to_string(), Some(PathBuf::from(CHILD_PATH)));
        let schema = SchemaHandle::new(&db, 0);

        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let (diagnostics, dependencies) =
            crate::analyze_guarded(&db, buffer, schema, "file:///child.cls");
        std::panic::set_hook(previous);

        assert!(
            diagnostics.is_none(),
            "a panic in the index must degrade to no diagnostics, not to a clean file"
        );
        assert!(dependencies.is_none(), "and both halves degrade together");
    }

    /// The registry is the watcher's early-out, and it must not answer for a file
    /// no lookup ever reached: there is nothing to invalidate and nothing to
    /// re-run.
    #[test]
    fn an_untouched_file_is_not_registered_and_cannot_be_bumped() {
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT), (OTHER_PATH, OTHER)]);
        let mut db =
            AnalysisDatabase::new(searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>));
        let buffer = Buffer::new(&db, CHILD.to_string(), Some(PathBuf::from(CHILD_PATH)));
        let schema = SchemaHandle::new(&db, 0);
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();

        let index = Arc::clone(&db.config().index);
        assert!(
            index.knows(Path::new(PARENT_PATH)),
            "the parent was consulted, so it is registered"
        );
        assert!(
            !index.knows(Path::new(OTHER_PATH)),
            "nothing referenced the other file, so R6 keeps it unread"
        );
        assert!(
            !bump(&mut db, OTHER_PATH),
            "an unregistered file cannot be anyone's dependency"
        );
    }

    /// A configuration change (an `oxabl.toml` or `.df` edit) replaces the
    /// `AnalysisConfig`, and the index handle must survive it: it is the only route
    /// back to the per-file inputs, so a fresh registry would orphan them and every
    /// later disk change would silently invalidate nothing.
    #[test]
    fn the_index_handle_survives_a_configuration_change() {
        let fs = EditableFs::new(&[(PARENT_PATH, PARENT)]);
        let mut db =
            AnalysisDatabase::new(searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>));
        let buffer = Buffer::new(&db, CHILD.to_string(), Some(PathBuf::from(CHILD_PATH)));
        let schema = SchemaHandle::new(&db, 0);
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();

        // What `Server::install_config` does: a new configuration carrying the same
        // handle forward.
        let carried = Arc::clone(&db.config().index);
        db.set_config(AnalysisConfig {
            index: carried,
            ..searching_config(Arc::clone(&fs) as Arc<dyn FileSystem>)
        });

        fs.write(PARENT_PATH, PARENT_WITHOUT_MEMBER);
        assert!(
            bump(&mut db, PARENT_PATH),
            "the file registered before the configuration change is still known"
        );
        assert!(undefined(
            &compute_diagnostics(&db, buffer, schema).unwrap()
        ));
    }
}
