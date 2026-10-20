//! The batch index: a plain in-run cache over [`index_file`].
//!
//! Built **once per run**, not once per file. That is the whole point of the
//! memo: hundreds of files inheriting one base class must read and index that
//! base once, and a per-file index would re-index it per file no matter how good
//! its internal caching was.
//!
//! Resolution is *outward from what was asked for* (R6): nothing is read until a
//! name is looked up, so opening one file does not cost a workspace scan. The
//! graph grows one referenced file at a time, and files nobody references are
//! never touched.

use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassDescriptor, IndexAnswer, IndexName, IndexRevision, IndexedFileId, LocateKind, Located,
    MemberDescriptor, ProgramSignatures, Signature, WorkspaceIndex,
};
use oxabl_workspace::{FileSystem, PreprocessorConfig};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::facts::{ClassFacts, FileFacts};
use crate::producers::{SharedProducers, may_define_shared, scan_procedures};
use crate::{IncludeContext, index_file_at, program_signatures_at, search};

/// The generation a batch reports.
///
/// A batch is immutable for the life of the run that built it — it only ever
/// *grows* its memo, and it never sees an edit — so it has exactly one
/// generation. The counter exists for incremental implementations, which rebuild
/// and must let a consumer detect that results computed under an older index are
/// stale. The one thing this must not be is
/// [`IndexRevision::ABSENT`](oxabl_semantic::IndexRevision::ABSENT), which means
/// *no index*, and `IndexRevision::new` refuses that value at compile time here.
const BATCH_REVISION: IndexRevision = IndexRevision::new(1);

/// A [`WorkspaceIndex`] that resolves names against a borrowed filesystem and
/// the run's resolved include paths, memoizing per key.
///
/// Borrowing rather than owning the filesystem is deliberate: the client already
/// holds one (the CLI a real filesystem, the browser a virtual one), and building
/// the index over a *second* one would let the index search a different tree than
/// the client reads sources from.
pub struct BatchIndex<'a> {
    fs: &'a dyn FileSystem,
    include_paths: &'a [PathBuf],
    /// Paths resolved against the asking file's directory and searched first.
    root_relative: &'a [PathBuf],
    /// Extensions an extensionless `RUN` target is tried with, in order; `None`
    /// is [`search::BARE_PROGRAM_EXTENSIONS`].
    run_extensions: Option<&'a [String]>,
    /// Whether a lookup that misses falls back to a case-insensitive search.
    case_insensitive: bool,
    /// The environment the predefined names answer for; `None` is
    /// [`PreprocessorConfig::default`].
    preprocessor: Option<&'a PreprocessorConfig>,
    /// Where `SHARED` producers come from. See [`ProducerSource`].
    producers: ProducerSource<'a>,
    /// Interior mutability because [`WorkspaceIndex`] takes `&self` — it has to,
    /// since the language server shares one index behind an `Arc` across
    /// threads, so the trait cannot ask for `&mut`.
    ///
    /// The lock is taken once per *unresolved cross-file name*, not per token or
    /// per node, and is never held while a file is searched for or indexed, so
    /// one index can serve many analysis threads. A file is still indexed once:
    /// the first lookup to reach it claims a [`Pending`] slot, and a lookup that
    /// reaches it meanwhile waits on that slot rather than indexing it again.
    memo: RwLock<Memo>,
    /// The outcome of the scan of [`ProducerSource::Files`], once it has run.
    ///
    /// Kept outside the memo, so the scan holds no lock that other lookups
    /// take. A `SHARED` lookup that asks meanwhile waits for the scan rather
    /// than running it again.
    known: OnceLock<KnownProducers>,
    /// The internal procedures the files of [`ProducerSource::Files`] define,
    /// read once on the first question about them.
    procedure_names: OnceLock<FxHashSet<Box<str>>>,
}

/// What the scan of [`ProducerSource::Files`] found.
enum KnownProducers {
    /// The ids of the files of the list that may produce a `SHARED` name: the
    /// files any view may find a producer in.
    Seeded(FxHashSet<IndexedFileId>),
    /// The scan stopped part-way: a file panicked while it was indexed. The
    /// files it did not reach may produce any name.
    Failed,
}

/// The files [`BatchIndex::shared_producer`] may find a producer in, besides the
/// ones other lookups have already indexed.
enum ProducerSource<'a> {
    /// Nothing else: only files a lookup pulled in.
    None,
    /// The run's own file list, scanned on the first `SHARED` lookup. See
    /// [`BatchIndex::seeded_with`].
    Files(&'a [PathBuf]),
    /// A map built once by the caller and shared by every index that borrows it.
    /// See [`BatchIndex::with_shared_producers`].
    Shared(&'a SharedProducers),
}

#[derive(Debug)]
struct Memo {
    /// Facts per indexed file. Keyed by the path the search resolved, **lexically
    /// normalized** ([`search::normalize_lexically`]), which is what dedups a
    /// shared dependency: two names reaching the same file find the entry already
    /// there.
    ///
    /// The normalization is load-bearing, not tidiness. The key is a joined path
    /// derived from source text, and `find_name` tries two candidate spellings, so
    /// two lookups can reach one physical file under two different strings —
    /// `/src/thing.p` and `/src/sub/../thing.p`, or an include-path entry written
    /// `/src/.`. Keyed verbatim, that mints **two** [`IndexedFileId`]s for one
    /// file, and the ids are what everything downstream compares. It also breaks
    /// [`BatchIndex::shared_producer`], which scans these values and answers
    /// `Unknowable` when two *different* files define one `SHARED` name: two
    /// entries for one file look exactly like two producers, so a name with one
    /// real producer answers "cannot know" instead of naming it.
    ///
    /// Lexical, not [`std::fs::canonicalize`] — see
    /// [`search::normalize_lexically`] for why the real filesystem is not
    /// available here.
    facts: FxHashMap<PathBuf, Arc<FileFacts>>,
    /// Files a lookup has claimed and is indexing outside the lock, by the same
    /// key as `facts`. An entry moves to `facts` when its facts are published.
    pending: FxHashMap<PathBuf, Arc<Pending>>,
    /// Answers per class key. Kept separately from `facts` because a key can
    /// resolve to *no* file at all, and remembering that miss is what stops a
    /// repeated reference to an external class from re-searching every path
    /// entry on every lookup.
    ///
    /// Keyed by the **as-written** spelling ([`IndexName::as_written_atom`]),
    /// not by the `IndexName` itself. An `IndexName`'s `Hash`/`Eq` ignore
    /// casing, but the search now depends on the casing: two spellings of one
    /// name are the same `IndexName` key while being able to produce *different*
    /// search outcomes, so a memoized miss under `myapp.cache` would be served
    /// to `MyApp.Cache`, which would have found the file. A case-sensitive key
    /// costs at most one extra search per distinct spelling and cannot poison.
    ///
    /// It does not cost an extra *parse*: both spellings resolve to the same
    /// path, and `facts` is keyed by path, so the second search finds the file
    /// already indexed.
    classes: FxHashMap<OxablAtom, IndexAnswer<ClassFacts>>,
    /// Answers per `RUN` target key, for the same reasons.
    programs: FxHashMap<OxablAtom, IndexAnswer<IndexedFileId>>,
    /// Answers per asking file's directory and class key, when root-relative
    /// paths are configured: the directory decides which paths come first.
    scoped_classes: FxHashMap<(PathBuf, OxablAtom), IndexAnswer<ClassFacts>>,
    /// Answers per asking file's directory and `RUN` target key.
    scoped_programs: FxHashMap<(PathBuf, OxablAtom), IndexAnswer<IndexedFileId>>,
    /// Parameter lists per indexed file, read on the first question about them:
    /// a run that asks none reads nothing.
    signatures: FxHashMap<IndexedFileId, Option<Arc<ProgramSignatures>>>,
    /// The inverse of `facts`: the path each minted id was indexed under.
    ///
    /// Kept rather than derived. Deriving it means scanning `facts` for the entry
    /// whose id matches, and that lookup is the only way an id on a dependency edge
    /// becomes a path a client can render — so the absorbing caller runs it per edge
    /// per file, not once per distinct target as its documentation used to claim.
    /// A scan per edge over a map that grows with the workspace is a quadratic shape
    /// in a lookup documented as cheap.
    ///
    /// Written in exactly one place, beside the `facts` insert that mints the id, so
    /// the two cannot disagree: an id exists in one iff it exists in the other.
    paths: FxHashMap<IndexedFileId, PathBuf>,
    /// Next id to mint. Ids are assigned in first-index order and mean nothing
    /// outside this index, exactly as [`IndexedFileId`] documents. Starts at 1
    /// so a zero id never reads as a real file in a dump.
    next_file_id: u32,
}

impl<'a> BatchIndex<'a> {
    /// Build an index over `fs` and `include_paths`. Reads nothing yet.
    ///
    /// Kept to the two things every client already has. The analysed file's own
    /// identity — so a file cannot resolve a name to itself — is **not** state
    /// here: it arrives per asking file through [`BatchIndex::excluding`], which
    /// hands back a view rather than mutating the index. That is what lets one
    /// index span a whole multi-file walk while each file in it excludes only
    /// itself.
    pub fn new(fs: &'a dyn FileSystem, include_paths: &'a [PathBuf]) -> Self {
        BatchIndex {
            fs,
            include_paths,
            root_relative: &[],
            run_extensions: None,
            case_insensitive: false,
            preprocessor: None,
            producers: ProducerSource::None,
            memo: RwLock::new(Memo {
                facts: FxHashMap::default(),
                pending: FxHashMap::default(),
                paths: FxHashMap::default(),
                classes: FxHashMap::default(),
                programs: FxHashMap::default(),
                scoped_classes: FxHashMap::default(),
                scoped_programs: FxHashMap::default(),
                signatures: FxHashMap::default(),
                next_file_id: 1,
            }),
            known: OnceLock::new(),
            procedure_names: OnceLock::new(),
        }
    }

    /// Try `extensions`, in order, for a `RUN` target written without one,
    /// instead of [`search::BARE_PROGRAM_EXTENSIONS`].
    ///
    /// Apply before any lookup.
    #[must_use]
    pub fn with_run_extensions(mut self, extensions: &'a [String]) -> Self {
        self.run_extensions = Some(extensions);
        self
    }

    /// Search `paths`, resolved against the directory of the file that asks,
    /// before the include paths. An indexed file reads its own includes with the
    /// paths of its own directory.
    ///
    /// Only a view with a file ([`excluding`](Self::excluding)) searches them for
    /// a name. Apply before any lookup.
    #[must_use]
    pub fn with_root_relative_paths(mut self, paths: &'a [PathBuf]) -> Self {
        self.root_relative = paths;
        self
    }

    /// Answer `{&OPSYS}` and the other predefined names from `preprocessor`
    /// when an indexed file's own directives are applied.
    ///
    /// Apply before any lookup.
    #[must_use]
    pub fn with_preprocessor(mut self, preprocessor: &'a PreprocessorConfig) -> Self {
        self.preprocessor = Some(preprocessor);
        self
    }

    /// Fall back to a case-insensitive file search when the as-written and
    /// folded spellings of a class or `RUN` target both miss. Off by default.
    ///
    /// Apply before any lookup.
    #[must_use]
    pub fn with_case_insensitive_lookup(mut self, enabled: bool) -> Self {
        self.case_insensitive = enabled;
        self
    }

    fn predefined(&self) -> Cow<'a, PreprocessorConfig> {
        match self.preprocessor {
            Some(config) => Cow::Borrowed(config),
            None => Cow::Owned(PreprocessorConfig::default()),
        }
    }

    /// Where an indexed file reads its includes from: the paths this index
    /// searches.
    fn includes<'p>(&self, predefined: &'p PreprocessorConfig) -> IncludeContext<'p>
    where
        'a: 'p,
    {
        IncludeContext {
            fs: self.fs,
            include_paths: self.include_paths,
            root_relative_paths: self.root_relative,
            predefined,
        }
    }

    /// The directory of `file` and its root-relative paths, or `None` when there
    /// are none to search.
    fn scope(&self, file: Option<&Path>) -> Option<(PathBuf, Vec<PathBuf>)> {
        let file = file.filter(|_| !self.root_relative.is_empty())?;
        let dir = search::normalize_lexically(file.parent()?);
        let paths = search::root_include_paths(file, self.root_relative, &[]);
        Some((dir, paths))
    }

    fn find_path(&self, name: &IndexName, kind: search::NameKind) -> IndexAnswer<PathBuf> {
        self.find_name(name, kind)
    }

    fn find_name(&self, name: &IndexName, kind: search::NameKind) -> IndexAnswer<PathBuf> {
        match self.run_extensions {
            Some(extensions) => self.find_name_in(name, kind, extensions),
            None => self.find_name_in(name, kind, search::BARE_PROGRAM_EXTENSIONS),
        }
    }

    fn find_name_in<E: AsRef<str>>(
        &self,
        name: &IndexName,
        kind: search::NameKind,
        extensions: &[E],
    ) -> IndexAnswer<PathBuf> {
        self.find_name_on(self.include_paths, name, kind, extensions)
    }

    fn find_name_on<E: AsRef<str>>(
        &self,
        paths: &[PathBuf],
        name: &IndexName,
        kind: search::NameKind,
        extensions: &[E],
    ) -> IndexAnswer<PathBuf> {
        let answer = search::find_name_with(self.fs, paths, name, kind, extensions);
        if self.case_insensitive && answer == IndexAnswer::NotFound {
            return search::find_name_ignoring_case(self.fs, paths, name, kind, extensions);
        }
        answer
    }

    /// [`find_name`](Self::find_name) on `paths` alone.
    fn find_scoped(
        &self,
        paths: &[PathBuf],
        name: &IndexName,
        kind: search::NameKind,
    ) -> IndexAnswer<PathBuf> {
        match self.run_extensions {
            Some(extensions) => self.find_name_on(paths, name, kind, extensions),
            None => self.find_name_on(paths, name, kind, search::BARE_PROGRAM_EXTENSIONS),
        }
    }

    /// [`class_facts`](Self::class_facts) for a lookup from the file at `file`:
    /// its root-relative paths first, then the shared answer.
    fn class_facts_from(&self, name: &IndexName, file: Option<&Path>) -> IndexAnswer<ClassFacts> {
        let Some((dir, paths)) = self.scope(file) else {
            return self.class_facts(name);
        };
        let key = (dir, name.as_written_atom().clone());
        if let Some(hit) = self.memo_read().scoped_classes.get(&key) {
            return hit.clone();
        }
        let answer = match self.find_scoped(&paths, name, search::NameKind::Class) {
            IndexAnswer::Found(path) => self.class_in(name, &path),
            IndexAnswer::NotFound => self.class_facts(name),
            IndexAnswer::Unusable => IndexAnswer::Unusable,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        };
        self.memo()
            .scoped_classes
            .entry(key)
            .or_insert(answer)
            .clone()
    }

    /// The class `name` in the file at `path`, indexing it on first touch.
    fn class_in(&self, name: &IndexName, path: &Path) -> IndexAnswer<ClassFacts> {
        let facts = self.facts_for(path);
        match facts.class(name).cloned() {
            Some(class) => IndexAnswer::Found(class),
            None => IndexAnswer::Unusable,
        }
    }

    /// [`program`](WorkspaceIndex::program) for a lookup from the file at `file`.
    fn program_from(&self, target: &IndexName, file: Option<&Path>) -> IndexAnswer<IndexedFileId> {
        let Some((dir, paths)) = self.scope(file) else {
            return self.program(target);
        };
        let key = (dir, target.as_written_atom().clone());
        if let Some(hit) = self.memo_read().scoped_programs.get(&key) {
            return hit.clone();
        }
        let answer = match self.find_scoped(&paths, target, search::NameKind::Program) {
            IndexAnswer::Found(path) => {
                let facts = self.facts_for(&path);
                if facts.parsed {
                    IndexAnswer::Found(facts.file)
                } else {
                    IndexAnswer::Unusable
                }
            }
            IndexAnswer::NotFound => self.program(target),
            IndexAnswer::Unusable => IndexAnswer::Unusable,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        };
        self.memo()
            .scoped_programs
            .entry(key)
            .or_insert(answer)
            .clone()
    }

    /// [`located`](WorkspaceIndex::located) for a lookup from the file at `file`.
    fn located_from(&self, kind: LocateKind, name: &IndexName, file: Option<&Path>) -> Located {
        if let Some((_, paths)) = self.scope(file) {
            let search_kind = match kind {
                LocateKind::Class => search::NameKind::Class,
                LocateKind::Program => search::NameKind::Program,
            };
            if let IndexAnswer::Found(path) = self.find_scoped(&paths, name, search_kind) {
                return Located::In(self.facts_for(&path).file);
            }
        }
        self.located(kind, name)
    }

    /// Tell the index which files the run already knows it will read.
    ///
    /// Only [`shared_producer`](WorkspaceIndex::shared_producer) uses them, and
    /// only because a `SHARED` name maps onto no path: with nothing to derive a
    /// candidate from, the only honest answers are "enumerate the workspace" —
    /// the scan R6 forbids, and one the [`FileSystem`] trait cannot perform
    /// anyway, having no directory listing — or "consult the files this run
    /// already knows about". This is that second set, and it is the walk's *own*
    /// file list, never a directory scan.
    ///
    /// The list is scanned for `SHARED` definitions on the first such lookup. An
    /// index of its own per thread repeats that scan per thread; build one
    /// [`SharedProducers`] and use [`with_shared_producers`](Self::with_shared_producers)
    /// instead.
    ///
    /// Apply before any lookup. A client with no such list (the language server,
    /// the browser) simply does not call this, and behavior is exactly as
    /// before: `shared_producer` then answers only from the files some other
    /// lookup pulled in.
    #[must_use]
    pub fn seeded_with(mut self, known_files: &'a [PathBuf]) -> Self {
        self.producers = ProducerSource::Files(known_files);
        self
    }

    /// Answer `SHARED` producer lookups from a map built once over the run's
    /// files, instead of scanning them in this index.
    ///
    /// The answers are those of [`seeded_with`](Self::seeded_with) over the same
    /// files. The map is `Sync`, so indexes on several threads can borrow one, and
    /// the cost of finding the producers is paid once.
    ///
    /// Apply before any lookup.
    #[must_use]
    pub fn with_shared_producers(mut self, producers: &'a SharedProducers) -> Self {
        self.producers = ProducerSource::Shared(producers);
        self
    }

    /// A view of this index that answers as though the file at `path` did not
    /// exist, leaving the memo — and therefore the whole run's I/O — shared.
    ///
    /// `None` excludes nothing, which is the right answer for a client that has
    /// no path for the buffer it is analysing (the browser): a file with no
    /// identity cannot collide with itself.
    ///
    /// # Why a view and not a field
    ///
    /// The exclusion varies per *asking file* while the memo must span the whole
    /// **run** — those are different lifetimes. Storing the analysed path on the
    /// index would force one index per file, which is exactly the shape that
    /// makes the shared-dependency dedup stop paying: a hundred files inheriting
    /// one base would read that base a hundred times.
    pub fn excluding<'v>(&'v self, path: Option<&'v Path>) -> ExcludingFile<'v, 'a> {
        ExcludingFile {
            index: self,
            path,
            excludes: true,
            reached: Mutex::default(),
        }
    }

    /// A view that searches as [`excluding`](Self::excluding) does for the
    /// file at `path`, its root-relative paths first, and excludes nothing: a
    /// name of that file resolves to it as to any other file.
    ///
    /// For a client that caches an answer for every file of one directory and
    /// checks, per file, whether the answer reached that file.
    pub fn searching_from<'v>(&'v self, path: Option<&'v Path>) -> ExcludingFile<'v, 'a> {
        ExcludingFile {
            excludes: false,
            ..self.excluding(path)
        }
    }

    /// The id this run assigned to `path`, or `None` if nothing has indexed it.
    ///
    /// A memo probe only — it never reads. That is what makes it usable *after* a
    /// lookup has answered: by then the file behind the answer is in `facts`, so
    /// [`ExcludingFile`] can compare ids without having to know how the search
    /// turned a name into a path.
    /// The parameter lists of the indexed file `file`, or `None` when the file
    /// cannot be read or parsed.
    ///
    /// A list the file's own text does not show whole (an `{include}` may hold
    /// a parameter) is read again from the expanded text.
    fn signatures_of(&self, file: IndexedFileId) -> Option<Arc<ProgramSignatures>> {
        let path = {
            let memo = self.memo_read();
            if let Some(hit) = memo.signatures.get(&file) {
                return hit.clone();
            }
            memo.paths.get(&file).cloned()?
        };
        let answer = self.read_signatures(&path).map(Arc::new);
        self.memo().signatures.insert(file, answer.clone());
        answer
    }

    fn read_signatures(&self, path: &Path) -> Option<ProgramSignatures> {
        let source = self.fs.read(path).ok()?;
        let predefined = self.predefined();
        program_signatures_at(path, &source, &self.includes(&predefined)).0
    }

    /// See [`WorkspaceIndex::workspace_defines_procedure`].
    fn defines_procedure(&self, name: &IndexName) -> Option<bool> {
        let predefined = self.predefined();
        let includes = self.includes(&predefined);
        match self.producers {
            ProducerSource::None => None,
            ProducerSource::Shared(map) => Some(map.defines_procedure(name, &includes)),
            ProducerSource::Files(files) => {
                let names = self.procedure_names.get_or_init(|| {
                    let threads = std::thread::available_parallelism().map_or(1, |n| n.get());
                    scan_procedures(files, &includes, threads)
                });
                Some(names.contains(name.as_str()))
            }
        }
    }

    fn indexed_id(&self, path: &Path) -> Option<IndexedFileId> {
        let key = search::normalize_lexically(path);
        self.memo_read().facts.get(&key).map(|facts| facts.file)
    }

    /// The path this run indexed `id` under, or `None` if `id` came from another
    /// index.
    ///
    /// The inverse of `indexed_id`, and the only way an
    /// [`IndexedFileId`] on a dependency edge becomes something a client can render
    /// or open: the id space is this index's own, so nothing outside it can do the
    /// mapping.
    ///
    /// A direct lookup in a mapping maintained beside `facts`. It used to be a linear
    /// scan, and its documentation said the scan was affordable because it ran once per
    /// distinct edge target — but the absorbing caller runs it per edge per file, so the
    /// cost was a scan of a workspace-sized map per edge. The mapping removes the shape;
    /// the documentation is corrected here rather than left describing a call pattern
    /// that was never the real one.
    pub fn indexed_path(&self, id: IndexedFileId) -> Option<PathBuf> {
        self.memo_read().paths.get(&id).cloned()
    }

    /// The memo, recovering from poisoning.
    ///
    /// A poisoned lock means a previous lookup unwound while holding it — in this
    /// workspace most plausibly a salsa `Cancelled`, which travels as a panic
    /// payload. Cancellation must not leave the index permanently unusable, and
    /// it must certainly not be converted into a panic here: recovering the inner
    /// value keeps the answer path total while letting the original unwind
    /// continue on its way. The memo is a cache of pure results, so a partially
    /// updated one is still correct.
    fn memo(&self) -> RwLockWriteGuard<'_, Memo> {
        self.memo
            .write()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// The memo for a probe that changes nothing, recovering from poisoning as
    /// [`memo`](Self::memo) does.
    fn memo_read(&self) -> RwLockReadGuard<'_, Memo> {
        self.memo
            .read()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// The facts for `path`, indexing it on first touch, outside the lock.
    fn facts_for(&self, path: &Path) -> Arc<FileFacts> {
        // Normalized *before* the probe and used for the insert as well, so the
        // two spellings of one file are one key on both halves. `path` itself is
        // what gets read: the normalized form is the key, not the I/O target, so a
        // filesystem that treats the two spellings differently is still asked the
        // question the caller actually posed.
        let key = search::normalize_lexically(path);
        if let Some(hit) = self.memo_read().facts.get(&key) {
            return Arc::clone(hit);
        }
        let pending = match self.memo().claim(&key) {
            Ok(hit) => return hit,
            Err(pending) => pending,
        };
        let predefined = self.predefined();
        let includes = self.includes(&predefined);
        // Located but unreadable is not an error case: a file we cannot read is
        // knowably unusable, and remembering that keeps us from re-attempting the
        // read on every reference to the same name.
        let facts = pending.index(&key, || includes.fs.read(path).ok(), &includes);
        self.memo().publish(key, facts)
    }

    /// The file that produces the `SHARED` name `name`, answered from the run's
    /// known files plus either every file this run has indexed (`reached` is
    /// `None`) or only the files in `reached`.
    ///
    /// Not memoized. A `SHARED` name maps onto no path — unlike a class name or a
    /// `RUN` target, there is nothing to derive a candidate file from — so the
    /// only ways to answer are to enumerate the workspace or to consult files the
    /// run already knows. Enumerating is exactly the whole-workspace scan R6
    /// forbids, and the `FileSystem` trait offers no directory listing to do it
    /// with. The known files are the walk's own list, scanned once into a
    /// producer map; the rest are the files lookups pulled in. Caching the
    /// answer would freeze a `NotFound` that a later `RUN` lookup could have
    /// turned into a hit, so the question is asked each time, over the
    /// producers of this one name and the handful of files this run pulled in.
    ///
    /// Which files the run has pulled in depends on which files it analysed
    /// before, so [`ExcludingFile`] passes the files its own lookups reached:
    /// one file's answer then does not depend on what the run did for others.
    ///
    /// Two producers of one name is an ambiguity of the same shape the
    /// literal-`RUN` rule refuses to guess at, so it declines rather than
    /// picking whichever file happened to be indexed first — which also keeps
    /// the answer independent of `HashMap` iteration order.
    fn shared_producer_among(
        &self,
        name: &IndexName,
        reached: Option<&FxHashSet<IndexedFileId>>,
    ) -> IndexAnswer<IndexedFileId> {
        let mut listed: &[PathBuf] = &[];
        let mut seeded = None;
        match self.producers {
            ProducerSource::None => {}
            ProducerSource::Shared(map) => listed = map.producers_of(name),
            // Scanned on first ask. This is the only query the list feeds, and
            // deferring it to here is what keeps a run that consumes no `SHARED`
            // name from reading the whole walk.
            ProducerSource::Files(files) => match self.known_producers(files) {
                KnownProducers::Seeded(ids) => seeded = Some(ids),
                KnownProducers::Failed => return IndexAnswer::Unknowable,
            },
        }
        let mut found = None;
        let mut consider = |file: IndexedFileId| match found {
            Some(first) if first != file => false,
            _ => {
                found = Some(file);
                true
            }
        };
        // Indexed one at a time: a name many files produce is `Unknowable` as
        // soon as a second one is read.
        for path in listed {
            let facts = self.facts_for(path);
            if facts.defines_shared(name) && !consider(facts.file) {
                return IndexAnswer::Unknowable;
            }
        }
        let memo = self.memo_read();
        for facts in memo.facts.values() {
            let candidate = reached.is_none_or(|reached| {
                reached.contains(&facts.file) || seeded.is_some_and(|ids| ids.contains(&facts.file))
            });
            if candidate && facts.defines_shared(name) && !consider(facts.file) {
                return IndexAnswer::Unknowable;
            }
        }
        found.map_or(IndexAnswer::NotFound, IndexAnswer::Found)
    }

    /// The outcome of the scan of `files`, run by the first lookup that asks.
    ///
    /// A panic in the scan unwinds through the lookup that ran it and leaves
    /// [`KnownProducers::Failed`]: every later lookup, and every lookup that
    /// waited meanwhile, gets that at once and does not scan again.
    fn known_producers(&self, files: &[PathBuf]) -> &KnownProducers {
        let mut panicked = None;
        let known = self.known.get_or_init(|| {
            let scan = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                self.index_possible_producers(files)
            }));
            scan.map_or_else(
                |payload| {
                    panicked = Some(payload);
                    KnownProducers::Failed
                },
                KnownProducers::Seeded,
            )
        });
        if let Some(payload) = panicked {
            std::panic::resume_unwind(payload);
        }
        known
    }

    /// Index the files of `files` that can produce a `SHARED` name, reading each
    /// once, and return their ids. A file this index already knows is left as
    /// it is.
    ///
    /// The prefilter skips only a file that [`index_file_at`] would find no
    /// definition in, so a file is a producer by the same rule whether an
    /// earlier lookup indexed it or not. Files are read, prefiltered and
    /// indexed on the machine's threads, with no memo lock held, and claimed in
    /// list order, so the ids they get do not depend on the thread count.
    fn index_possible_producers(&self, files: &[PathBuf]) -> FxHashSet<IndexedFileId> {
        let predefined = self.predefined();
        let includes = self.includes(&predefined);
        let threads = std::thread::available_parallelism().map_or(1, |n| n.get());
        let candidates = map_parallel(files, threads, |path| {
            let key = search::normalize_lexically(path);
            if let Some(known) = self.memo_read().facts.get(&key) {
                return Some(Err(known.file));
            }
            let source = includes.fs.read(path).ok()?;
            may_define_shared(&key, &source, &includes).then_some(Ok((key, source)))
        });
        let mut seeded = FxHashSet::default();
        let mut claimed = Vec::new();
        {
            let mut memo = self.memo();
            for candidate in candidates.into_iter().flatten() {
                match candidate {
                    Err(known) => {
                        seeded.insert(known);
                    }
                    Ok((key, source)) => match memo.claim(&key) {
                        Ok(known) => {
                            seeded.insert(known.file);
                        }
                        Err(pending) => claimed.push((key, pending, source)),
                    },
                }
            }
        }
        seeded.extend(map_parallel(&claimed, threads, |(key, pending, source)| {
            let facts = pending.index(key, || Some(Arc::clone(source)), &includes);
            self.memo().publish(key.clone(), facts).file
        }));
        seeded
    }

    /// Resolve `name` to the facts of the class it names, memoized per key.
    fn class_facts(&self, name: &IndexName) -> IndexAnswer<ClassFacts> {
        if let Some(hit) = self.memo_read().classes.get(name.as_written_atom()) {
            return hit.clone();
        }

        let answer = match self.find_name(name, search::NameKind::Class) {
            IndexAnswer::Found(path) => {
                let facts = self.facts_for(&path);
                // A file *was* located at the path the class name derives, and
                // it does not visibly declare the class. That is `Unusable`,
                // not `NotFound`, and the difference decides whether a
                // consumer may tell a user the name does not exist — which
                // here it may not, because the file plainly exists.
                //
                // Three causes, and the index cannot distinguish them: the
                // parse recovered errors; the declaration is spliced in from
                // an `{include}` the index could not read; or the class header
                // is genuinely mis-namespaced.
                // Only the third is the user's mistake, and it is the rarest —
                // an include-built class is an ordinary ABL idiom. `NotFound`
                // stays reserved for the one case that *is* conclusive: no
                // file at the derived path at all.
                //
                // The class comparison is case-insensitive, since the
                // declaring file is free to spell the name differently from
                // the reference.
                match facts.class(name).cloned() {
                    Some(class) => IndexAnswer::Found(class),
                    None => IndexAnswer::Unusable,
                }
            }
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unusable => IndexAnswer::Unusable,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        };
        // Another thread may have answered meanwhile; the answers agree, and the
        // first one stays.
        self.memo()
            .classes
            .entry(name.as_written_atom().clone())
            .or_insert(answer)
            .clone()
    }
}

/// `f` of each of `items`, in order, computed on up to `threads` threads. A
/// panic in `f` unwinds through the caller once every thread has stopped.
fn map_parallel<T: Sync, R: Send>(
    items: &[T],
    threads: usize,
    f: impl Fn(&T) -> R + Sync,
) -> Vec<R> {
    let threads = threads.clamp(1, items.len().max(1));
    if threads == 1 {
        return items.iter().map(f).collect();
    }
    let next = AtomicUsize::new(0);
    let mut done: Vec<(usize, R)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                scope.spawn(|| {
                    let mut mine = Vec::new();
                    loop {
                        let at = next.fetch_add(1, Ordering::Relaxed);
                        let Some(item) = items.get(at) else {
                            return mine;
                        };
                        mine.push((at, f(item)));
                    }
                })
            })
            .collect();
        workers
            .into_iter()
            .flat_map(|worker| match worker.join() {
                Ok(mine) => mine,
                Err(panic) => std::panic::resume_unwind(panic),
            })
            .collect()
    });
    done.sort_unstable_by_key(|(at, _)| *at);
    done.into_iter().map(|(_, r)| r).collect()
}

/// A file one lookup is indexing: the id minted when the lookup claimed it, and
/// its facts once extracted. A second lookup that reaches the file meanwhile
/// waits on `facts` instead of indexing it again.
#[derive(Debug)]
struct Pending {
    id: IndexedFileId,
    facts: OnceLock<Arc<FileFacts>>,
}

impl Pending {
    /// The facts of the file at `key`, extracted from what `read` returns on
    /// first call; `None` is an unreadable file. Nothing here takes the memo
    /// lock, so a lookup may wait on it while holding the lock.
    fn index(
        &self,
        key: &Path,
        read: impl FnOnce() -> Option<Arc<str>>,
        includes: &IncludeContext<'_>,
    ) -> Arc<FileFacts> {
        let facts = self.facts.get_or_init(|| {
            Arc::new(match read() {
                Some(source) => index_file_at(self.id, key, &source, includes).0,
                None => FileFacts::unparseable(self.id),
            })
        });
        Arc::clone(facts)
    }
}

impl Memo {
    /// The facts of `key` if it is indexed, otherwise the slot it is indexed
    /// in, claimed now if no lookup has claimed it yet. The claim mints the id,
    /// so ids follow the order in which lookups first reach files.
    fn claim(&mut self, key: &Path) -> Result<Arc<FileFacts>, Arc<Pending>> {
        if let Some(hit) = self.facts.get(key) {
            return Ok(Arc::clone(hit));
        }
        if let Some(pending) = self.pending.get(key) {
            return Err(Arc::clone(pending));
        }
        let pending = Arc::new(Pending {
            id: IndexedFileId::new(self.next_file_id),
            facts: OnceLock::new(),
        });
        self.next_file_id += 1;
        self.pending.insert(key.to_path_buf(), Arc::clone(&pending));
        Err(pending)
    }

    /// Record `facts` as the facts of `key`, unless another lookup recorded them
    /// first; either way, the recorded facts.
    fn publish(&mut self, key: PathBuf, facts: Arc<FileFacts>) -> Arc<FileFacts> {
        self.pending.remove(&key);
        if let Some(known) = self.facts.get(&key) {
            return Arc::clone(known);
        }
        // Both halves of one mapping, inserted together: `indexed_path` is only sound
        // because nothing can add an entry to one without adding it to the other.
        self.paths.insert(facts.file, key.clone());
        self.facts.insert(key, Arc::clone(&facts));
        facts
    }
}

impl WorkspaceIndex for BatchIndex<'_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        self.class_facts(name).map(|facts| facts.descriptor)
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        // Same lookup, same memo entry, so the consumer's two-question pattern
        // (descriptor, then members, then up the chain) costs one search and one
        // parse per class.
        self.class_facts(class).map(|facts| facts.members)
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        if let Some(hit) = self.memo_read().programs.get(target.as_written_atom()) {
            return hit.clone();
        }

        let answer = match self.find_path(target, search::NameKind::Program) {
            IndexAnswer::Found(path) => {
                // The target is indexed rather than merely existence-checked,
                // for two reasons: the seam's totality rule needs a verdict for
                // an unusable file, and a program a file `RUN`s is the realistic
                // producer of the `SHARED` names that file consumes — indexing it
                // here is what gives `shared_producer` something true to say.
                let facts = self.facts_for(&path);
                if facts.parsed {
                    IndexAnswer::Found(facts.file)
                } else {
                    // Located, and oxabl could not parse it. Reporting the target
                    // as absent would blame the caller for oxabl's own gap.
                    IndexAnswer::Unusable
                }
            }
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unusable => IndexAnswer::Unusable,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        };
        self.memo()
            .programs
            .entry(target.as_written_atom().clone())
            .or_insert(answer)
            .clone()
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        self.shared_producer_among(name, None)
    }

    fn workspace_defines_procedure(&self, name: &IndexName) -> Option<bool> {
        self.defines_procedure(name)
    }

    fn signature(
        &self,
        program: &IndexName,
        procedure: Option<&IndexName>,
    ) -> Option<Arc<Signature>> {
        let IndexAnswer::Found(file) = self.program(program) else {
            return None;
        };
        signature_in(&*self.signatures_of(file)?, procedure)
    }

    fn located(&self, kind: LocateKind, name: &IndexName) -> Located {
        let kind = match kind {
            LocateKind::Class => search::NameKind::Class,
            LocateKind::Program => search::NameKind::Program,
        };
        match self.find_path(name, kind) {
            IndexAnswer::Found(path) => Located::In(self.facts_for(&path).file),
            _ => Located::Nowhere,
        }
    }

    fn revision(&self) -> IndexRevision {
        BATCH_REVISION
    }

    /// The resolved include paths this index searches. With none, every name-based
    /// query answers `NotFound` without anything having been looked at, and the
    /// resolve pass files such a miss as "we did not look" rather than as a fact
    /// about the workspace.
    ///
    /// Note this is *not* the same as the index being absent: a seeded file list
    /// still lets `shared_producer` answer with no path configured.
    fn searches_any_path(&self) -> bool {
        !self.include_paths.is_empty()
    }
}

/// The list of the program or of its internal procedure `procedure`.
fn signature_in(
    signatures: &ProgramSignatures,
    procedure: Option<&IndexName>,
) -> Option<Arc<Signature>> {
    let list = match procedure {
        Some(name) => signatures.procedure(name),
        None => signatures.main.as_ref(),
    };
    list.map(|s| Arc::new(s.clone()))
}

/// One asking file's view of a [`BatchIndex`]: every answer that would name the
/// file being analysed becomes [`IndexAnswer::NotFound`].
///
/// Built by [`BatchIndex::excluding`]. The memo stays shared, so a walk of a
/// thousand files still reads each dependency once while each file answers as if
/// its own copy on disk were absent.
///
/// # Why a file must not resolve a name to itself
///
/// The client analyses a *buffer*, which for an editor is the unsaved text and
/// for the CLI is the bytes it just read; the index answers from what is on
/// disk. Letting the two meet would attribute the buffer's own — possibly
/// stale — disk copy to it as a *foreign* file: a class would inherit from
/// itself, and a `SHARED` consumer would be linked to a producer that is really
/// its own earlier revision. Both are wrong answers of the kind this seam
/// declines to guess at, so the file is excluded from its own lookups.
///
/// # How the exclusion is decided
///
/// By [`IndexedFileId`], **after** the underlying query has answered. The
/// answers carry the id of the file they came from, and by the time one arrives
/// the file behind it is in the memo, so the analysed path has an id to compare
/// against. Deciding beforehand would not work: a file the run has not touched
/// yet has no id at all.
pub struct ExcludingFile<'v, 'a> {
    index: &'v BatchIndex<'a>,
    /// The analysed file, or `None` for a client with no path for its buffer.
    path: Option<&'v Path>,
    /// Whether answers that name `path` are withheld; see
    /// [`BatchIndex::searching_from`].
    excludes: bool,
    /// Every file this view's own lookups answered with: the files besides the
    /// seeded set that `shared_producer` may name.
    reached: Mutex<FxHashSet<IndexedFileId>>,
}

impl ExcludingFile<'_, '_> {
    fn reach(&self, file: IndexedFileId) {
        self.reached
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(file);
    }

    /// Whether `file` is the analysed file. `false` whenever there is no
    /// identity to compare against, which is what makes the no-path client's
    /// behavior identical to an unexcluded index.
    fn is_analysed(&self, file: IndexedFileId) -> bool {
        self.path
            .filter(|_| self.excludes)
            .and_then(|path| self.index.indexed_id(path))
            .is_some_and(|analysed| analysed == file)
    }
}

impl WorkspaceIndex for ExcludingFile<'_, '_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        let answer = self
            .index
            .class_facts_from(name, self.path)
            .map(|facts| facts.descriptor);
        if let IndexAnswer::Found(descriptor) = &answer {
            self.reach(descriptor.file);
        }
        match answer {
            // `Unusable`, not `NotFound`: the file exists and is simply not usable
            // as a *foreign* file, because it is the one being analysed. Answering
            // `NotFound` would let a consumer report a name whose declaration is in
            // the buffer in front of the user as absent from the workspace.
            IndexAnswer::Found(descriptor) if self.is_analysed(descriptor.file) => {
                IndexAnswer::Unusable
            }
            other => other,
        }
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        // The member list carries no file id of its own, so the owning class is
        // asked for one first. That costs nothing: both queries read the same
        // memo entry, so the pair is still one search and one parse.
        let answer = self.index.class_facts_from(class, self.path);
        if let IndexAnswer::Found(facts) = &answer {
            self.reach(facts.descriptor.file);
            if self.is_analysed(facts.descriptor.file) {
                // Same answer `class` gives for the same reason: the file exists and
                // is not usable as a foreign one.
                return IndexAnswer::Unusable;
            }
        }
        answer.map(|facts| facts.members)
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        let answer = self.index.program_from(target, self.path);
        if let IndexAnswer::Found(file) = answer {
            self.reach(file);
        }
        match answer {
            IndexAnswer::Found(file) if self.is_analysed(file) => IndexAnswer::Unusable,
            other => other,
        }
    }

    fn workspace_defines_procedure(&self, name: &IndexName) -> Option<bool> {
        self.index.defines_procedure(name)
    }

    fn signature(
        &self,
        program: &IndexName,
        procedure: Option<&IndexName>,
    ) -> Option<Arc<Signature>> {
        let IndexAnswer::Found(file) = self.index.program_from(program, self.path) else {
            return None;
        };
        if self.is_analysed(file) {
            return None;
        }
        signature_in(&*self.index.signatures_of(file)?, procedure)
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        let reached = self
            .reached
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        match self.index.shared_producer_among(name, Some(&reached)) {
            // Deliberately `NotFound` rather than `Unusable`: an unlinked `SHARED`
            // consumer produces no diagnostic either way, and a producer that is
            // really the analysed file's own earlier revision is not a producer at
            // all — there is nothing here for a consumer to be told about.
            IndexAnswer::Found(file) if self.is_analysed(file) => IndexAnswer::NotFound,
            other => other,
        }
    }

    fn located(&self, kind: LocateKind, name: &IndexName) -> Located {
        let answer = self.index.located_from(kind, name, self.path);
        if let Located::In(file) = answer {
            self.reach(file);
        }
        match answer {
            Located::In(file) if self.is_analysed(file) => Located::Itself,
            other => other,
        }
    }

    fn revision(&self) -> IndexRevision {
        // The view is the same generation as what it views: it filters answers,
        // it does not compute newer ones. Reporting anything else — least of all
        // `ABSENT`, which means *no index* — would make a consumer's staleness
        // check disagree with the index it actually asked.
        self.index.revision()
    }

    /// The index's paths, and the root-relative ones when the view has a file to
    /// resolve them against.
    fn searches_any_path(&self) -> bool {
        self.index.searches_any_path() || self.index.scope(self.path).is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_workspace::InMemoryFileSystem;
    use std::io;
    use std::sync::Mutex;

    /// A filesystem that counts reads per path, so "indexed once" is asserted
    /// against the I/O the index actually performed rather than against a memo
    /// field only the test can see.
    struct CountingFs {
        inner: InMemoryFileSystem,
        reads: Mutex<FxHashMap<PathBuf, usize>>,
    }

    impl CountingFs {
        fn new(files: &[(&str, &str)]) -> Self {
            let mut inner = InMemoryFileSystem::new();
            for (path, source) in files {
                inner.insert(PathBuf::from(path), *source);
            }
            CountingFs {
                inner,
                reads: Mutex::new(FxHashMap::default()),
            }
        }

        fn reads_of(&self, path: &str) -> usize {
            self.reads
                .lock()
                .unwrap()
                .get(&PathBuf::from(path))
                .copied()
                .unwrap_or(0)
        }
    }

    impl FileSystem for CountingFs {
        fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
            *self
                .reads
                .lock()
                .unwrap()
                .entry(path.to_path_buf())
                .or_insert(0) += 1;
            self.inner.read(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }
    }

    const PARENT: &str = r#"CLASS orders.calc-base:
                                METHOD PUBLIC INTEGER calc-total():
                                    RETURN 0.
                                END METHOD.
                            END CLASS."#;

    fn dirs(entries: &[&str]) -> Vec<PathBuf> {
        entries.iter().map(PathBuf::from).collect()
    }

    #[test]
    fn a_known_file_scan_cut_short_by_a_panic_answers_unknowable_and_is_not_run_again() {
        /// Panics on every read of one path.
        struct PanicsOn {
            inner: CountingFs,
            path: &'static str,
        }
        impl FileSystem for PanicsOn {
            fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
                let read = self.inner.read(path);
                assert!(
                    path != Path::new(self.path),
                    "cannot read {}",
                    path.display()
                );
                read
            }
            fn exists(&self, path: &Path) -> bool {
                self.inner.exists(path)
            }
        }
        let fs = PanicsOn {
            inner: CountingFs::new(&[
                (
                    "/src/a.p",
                    "DEFINE NEW SHARED VARIABLE g AS INTEGER NO-UNDO.",
                ),
                (
                    "/src/b.p",
                    "DEFINE NEW SHARED VARIABLE h AS INTEGER NO-UNDO.",
                ),
            ]),
            path: "/src/b.p",
        };
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/a.p", "/src/b.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
        let name = IndexName::new("g");
        let first = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            index.shared_producer(&name)
        }));
        assert!(first.is_err(), "the scan reads the file that panics");
        for _ in 0..3 {
            assert_eq!(index.shared_producer(&name), IndexAnswer::Unknowable);
        }
        assert_eq!(
            fs.inner.reads_of("/src/b.p"),
            1,
            "the scan is not run again"
        );
        assert_eq!(fs.inner.reads_of("/src/a.p"), 1);
    }

    /// Holds the first read of one path until the test opens the gate, then
    /// panics on it if asked to. Other reads pass through.
    struct Gated {
        inner: CountingFs,
        path: &'static str,
        panics: bool,
        state: Mutex<(bool, bool)>,
        changed: std::sync::Condvar,
    }

    impl Gated {
        fn new(inner: CountingFs, path: &'static str, panics: bool) -> Self {
            Gated {
                inner,
                path,
                panics,
                state: Mutex::new((false, false)),
                changed: std::sync::Condvar::new(),
            }
        }

        fn wait_until_entered(&self) {
            let state = self.state.lock().unwrap();
            drop(self.changed.wait_while(state, |s| !s.0).unwrap());
        }

        fn open(&self) {
            self.state.lock().unwrap().1 = true;
            self.changed.notify_all();
        }
    }

    impl FileSystem for Gated {
        fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
            let read = self.inner.read(path);
            if path == Path::new(self.path) && self.inner.reads_of(self.path) == 1 {
                let mut state = self.state.lock().unwrap();
                state.0 = true;
                self.changed.notify_all();
                drop(self.changed.wait_while(state, |s| !s.1).unwrap());
                assert!(!self.panics, "cannot read {}", path.display());
            }
            read
        }
        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }
    }

    #[test]
    fn lookups_go_on_while_the_known_file_scan_runs_and_its_waiters_see_its_panic() {
        let fs = Gated::new(
            CountingFs::new(&[
                (
                    "/src/a.p",
                    "DEFINE NEW SHARED VARIABLE g AS INTEGER NO-UNDO.",
                ),
                (
                    "/src/b.p",
                    "DEFINE NEW SHARED VARIABLE h AS INTEGER NO-UNDO.",
                ),
                ("/src/orders/calc-base.cls", PARENT),
            ]),
            "/src/b.p",
            true,
        );
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/a.p", "/src/b.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
        let name = IndexName::new("g");
        std::thread::scope(|scope| {
            let scanner = scope.spawn(|| index.shared_producer(&name));
            fs.wait_until_entered();
            let waiter = scope.spawn(|| index.shared_producer(&name));
            // The scan is held inside a read: a lookup that needs no producer
            // must not wait for it.
            let (sent, class) = std::sync::mpsc::channel();
            let shared = &index;
            scope.spawn(move || sent.send(shared.class(&IndexName::new("orders.calc-base"))));
            let class = class.recv_timeout(std::time::Duration::from_secs(10));
            fs.open();
            assert!(
                matches!(class, Ok(IndexAnswer::Found(_))),
                "the class lookup waited for the scan"
            );
            assert!(
                scanner.join().is_err(),
                "the scan reads the file that panics"
            );
            assert_eq!(waiter.join().unwrap(), IndexAnswer::Unknowable);
        });
        assert_eq!(index.shared_producer(&name), IndexAnswer::Unknowable);
        assert_eq!(
            fs.inner.reads_of("/src/b.p"),
            1,
            "the scan is not run again"
        );
    }

    #[test]
    fn the_known_file_scan_gives_ids_in_list_order() {
        let sources: Vec<(String, String)> = (0..24)
            .map(|k| {
                (
                    format!("/src/p{k:02}.p"),
                    format!("DEFINE NEW SHARED VARIABLE g{k} AS INTEGER NO-UNDO."),
                )
            })
            .collect();
        let files: Vec<(&str, &str)> = sources
            .iter()
            .map(|(path, source)| (path.as_str(), source.as_str()))
            .collect();
        let fs = CountingFs::new(&files);
        let paths = dirs(&["/src"]);
        // Not in path order, so a sort cannot pass for the list order.
        let known: Vec<PathBuf> = sources
            .iter()
            .rev()
            .map(|(path, _)| PathBuf::from(path))
            .collect();
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
        assert!(matches!(
            index.shared_producer(&IndexName::new("g3")),
            IndexAnswer::Found(_)
        ));
        let ids: Vec<_> = known.iter().map(|path| index.indexed_id(path)).collect();
        let expected: Vec<_> = (1..=24).map(|n| Some(IndexedFileId::new(n))).collect();
        assert_eq!(ids, expected);
    }

    #[test]
    fn concurrent_first_procedure_questions_scan_the_files_once() {
        let fs = CountingFs::new(&[
            ("/src/a.p", "PROCEDURE do-a:\nEND PROCEDURE."),
            ("/src/b.p", "MESSAGE 1."),
        ]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/a.p", "/src/b.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
        let barrier = std::sync::Barrier::new(8);
        let answers: Vec<_> = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..8)
                .map(|_| {
                    scope.spawn(|| {
                        barrier.wait();
                        index.workspace_defines_procedure(&IndexName::new("do-a"))
                    })
                })
                .collect();
            workers.into_iter().map(|w| w.join().unwrap()).collect()
        });
        assert!(answers.iter().all(|a| *a == Some(true)));
        assert_eq!(fs.reads_of("/src/a.p"), 1);
        assert_eq!(fs.reads_of("/src/b.p"), 1);
    }

    #[test]
    fn threads_sharing_one_index_index_each_file_once_and_agree() {
        let fs = CountingFs::new(&[
            ("/src/orders/calc-base.cls", PARENT),
            ("/src/orders/post.p", "MESSAGE \"posted\"."),
        ]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let class = IndexName::new("orders.calc-base");
        let program = IndexName::new("orders/post.p");
        let answers: Vec<_> = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..8)
                .map(|_| {
                    scope.spawn(|| {
                        let view = index.excluding(Some(Path::new("/src/main.p")));
                        let IndexAnswer::Found(class) = view.class(&class) else {
                            panic!("the class is on the paths");
                        };
                        (class.file, view.program(&program))
                    })
                })
                .collect();
            workers.into_iter().map(|w| w.join().unwrap()).collect()
        });
        assert!(answers.windows(2).all(|pair| pair[0] == pair[1]));
        assert_eq!(fs.reads_of("/src/orders/calc-base.cls"), 1);
        assert_eq!(fs.reads_of("/src/orders/post.p"), 1);
    }

    #[test]
    fn a_view_searching_from_a_file_does_not_exclude_it() {
        let fs = CountingFs::new(&[("/src/orders/calc-base.cls", PARENT)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let name = IndexName::new("orders.calc-base");
        let own = Path::new("/src/orders/calc-base.cls");
        assert!(matches!(
            index.searching_from(Some(own)).class(&name),
            IndexAnswer::Found(_)
        ));
        assert_eq!(
            index.excluding(Some(own)).class(&name),
            IndexAnswer::Unusable
        );
    }

    #[test]
    fn a_class_on_the_paths_resolves_with_its_members() {
        let fs = CountingFs::new(&[("/src/orders/calc-base.cls", PARENT)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        let name = IndexName::new("orders.calc-base");
        let IndexAnswer::Found(descriptor) = index.class(&name) else {
            panic!("the class is on the paths");
        };
        assert_eq!(descriptor.name, name);
        let IndexAnswer::Found(members) = index.class_members(&name) else {
            panic!("members come from the same lookup");
        };
        assert_eq!(members.len(), 1);
        assert_eq!(members[0].name, IndexName::new("calc-total"));
    }

    #[test]
    fn a_class_with_no_file_is_not_found_and_an_ambiguous_program_is_unknowable() {
        let fs = CountingFs::new(&[
            ("/first/calc-total.p", "MESSAGE \"a\"."),
            ("/second/calc-total.p", "MESSAGE \"b\"."),
        ]);
        let paths = dirs(&["/first", "/second"]);
        let index = BatchIndex::new(&fs, &paths);

        assert_eq!(
            index.class(&IndexName::new("orders.absent")),
            IndexAnswer::NotFound
        );
        assert_eq!(
            index.class_members(&IndexName::new("orders.absent")),
            IndexAnswer::NotFound
        );
        assert_eq!(
            index.program(&IndexName::new("calc-total.p")),
            IndexAnswer::Unknowable,
            "a wrong RUN link is worse than declining"
        );
        assert_eq!(
            index.program(&IndexName::new("post-order.p")),
            IndexAnswer::NotFound
        );
    }

    /// The reverse mapping answers exactly what the scan it replaced answered, for
    /// every id the run minted and for one it never did.
    ///
    /// The scan is kept here as the oracle rather than described, because the property
    /// being pinned is equality with the old behaviour — including the normalised key
    /// it returned, which is the part a hand-written expectation would get wrong.
    #[test]
    fn the_reverse_path_mapping_agrees_with_the_scan_it_replaced() {
        let fs = CountingFs::new(&[
            ("/src/orders/calc-total.p", "MESSAGE \"posted\"."),
            ("/src/orders/post-order.p", "MESSAGE \"posted\"."),
            ("/src/shared/audit.p", "MESSAGE \"audited\"."),
        ]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        for name in [
            "orders/calc-total.p",
            "orders/post-order.p",
            "shared/audit.p",
        ] {
            let IndexAnswer::Found(_) = index.program(&IndexName::new(name)) else {
                panic!("{name} is on the paths");
            };
        }

        let scanned: Vec<(IndexedFileId, Option<PathBuf>)> = {
            let memo = index.memo();
            memo.facts
                .values()
                .map(|facts| {
                    let by_scan = memo
                        .facts
                        .iter()
                        .find(|(_, candidate)| candidate.file == facts.file)
                        .map(|(path, _)| path.clone());
                    (facts.file, by_scan)
                })
                .collect()
        };

        assert!(
            scanned.len() >= 3,
            "the run must have indexed every file, got {}",
            scanned.len()
        );
        for (id, by_scan) in scanned {
            assert_eq!(
                index.indexed_path(id),
                by_scan,
                "the mapping and the scan must agree for {id:?}"
            );
        }
    }

    /// An id this index never minted has no path, which is the same absence the scan
    /// produced — and the distinction that keeps an id from another index from being
    /// rendered as one of ours.
    #[test]
    fn an_identifier_this_index_never_minted_has_no_path() {
        let fs = CountingFs::new(&[("/src/calc-total.p", "MESSAGE \"posted\".")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let IndexAnswer::Found(known) = index.program(&IndexName::new("calc-total.p")) else {
            panic!("the file is on the paths");
        };

        assert!(index.indexed_path(known).is_some());
        assert_eq!(
            index.indexed_path(IndexedFileId::new(known.raw() + 1_000)),
            None
        );
    }

    /// Two spellings of one file are one id and therefore one path, so the reverse
    /// mapping cannot be the place the two-spellings dedup comes apart.
    #[test]
    fn one_file_reached_through_two_spellings_has_one_reverse_path() {
        let fs = CountingFs::new(&[("/src/calc-total.p", "MESSAGE \"posted\".")]);
        let paths = dirs(&["/src", "/src/."]);
        let index = BatchIndex::new(&fs, &paths);

        let IndexAnswer::Found(file) = index.program(&IndexName::new("calc-total.p")) else {
            panic!("exactly one match after normalisation");
        };
        assert_eq!(
            index.indexed_path(file),
            Some(PathBuf::from("/src/calc-total.p"))
        );
    }

    #[test]
    fn a_program_resolves_to_a_file_identity() {
        let fs = CountingFs::new(&[("/src/orders/calc-total.p", "MESSAGE \"posted\".")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        let IndexAnswer::Found(file) = index.program(&IndexName::new("orders/calc-total.p")) else {
            panic!("exactly one match on the paths");
        };
        assert_ne!(file.raw(), 0, "ids are minted from 1");
    }

    #[test]
    fn a_broken_program_is_unusable_rather_than_absent_or_an_error() {
        let fs = CountingFs::new(&[("/src/calc-total.p", "DEFINE VARIABLE .")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        assert_eq!(
            index.program(&IndexName::new("calc-total.p")),
            IndexAnswer::Unusable,
            "the file is right there and oxabl could not parse it, which is not \
             the same claim as `NotFound` — reporting the target as absent would \
             blame the caller for oxabl's own gap"
        );
        // And a name no file supplies is still the other answer, so the split is
        // a real distinction rather than a rename.
        assert_eq!(
            index.program(&IndexName::new("never-shipped.p")),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn a_shared_parent_is_indexed_once_for_twenty_askers() {
        // Twenty distinct child classes, each naming the same parent. The parent
        // must be read once, which is the property that makes one batch index per
        // run pay for itself.
        //
        // The children deliberately spell the parent three different ways. Each
        // spelling is its own answer-cache key and so does its own *search*
        // (cheap, and required — the caches are case-sensitive so a miss cannot
        // poison another spelling), but all three land on the same path, and the
        // facts cache is keyed by path. So "indexed once" must still hold across
        // spellings, which is the property a case-sensitive answer key could
        // plausibly have broken.
        const PARENT_SPELLINGS: [&str; 3] =
            ["orders.calc-base", "Orders.Calc-Base", "ORDERS.CALC-BASE"];
        let mut files = vec![("/src/orders/calc-base.cls".to_string(), PARENT.to_string())];
        for i in 0..20 {
            let parent = PARENT_SPELLINGS[i % PARENT_SPELLINGS.len()];
            files.push((
                format!("/src/orders/child-{i:02}.cls"),
                format!("CLASS orders.child-{i:02} INHERITS {parent}: END CLASS."),
            ));
        }
        let borrowed: Vec<(&str, &str)> = files
            .iter()
            .map(|(p, s)| (p.as_str(), s.as_str()))
            .collect();
        let fs = CountingFs::new(&borrowed);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        for i in 0..20 {
            let child = IndexName::new(&format!("orders.child-{i:02}"));
            let IndexAnswer::Found(descriptor) = index.class(&child) else {
                panic!("child {i} is on the paths");
            };
            // What every consumer does: walk to the parent.
            let parent = descriptor
                .inherits
                .clone()
                .expect("child declares a parent");
            assert!(matches!(
                index.class_members(&parent),
                IndexAnswer::Found(_)
            ));
        }

        assert_eq!(
            fs.reads_of("/src/orders/calc-base.cls"),
            1,
            "the shared parent is indexed once, not once per child"
        );
        assert_eq!(fs.reads_of("/src/orders/child-00.cls"), 1);
    }

    #[test]
    fn a_mixed_case_class_file_resolves_from_the_mixed_case_source_spelling() {
        // The case-sensitive-filesystem case: the file on disk is `MyApp/Cache.cls`
        // and the source writes `USING MyApp.Cache`. Deriving the path from the
        // folded key alone would look for `myapp/cache.cls` and miss.
        let fs = CountingFs::new(&[("/src/MyApp/Cache.cls", "CLASS MyApp.Cache: END CLASS.")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        assert!(
            matches!(
                index.class(&IndexName::new("MyApp.Cache")),
                IndexAnswer::Found(_)
            ),
            "a mixed-case file must resolve from the mixed-case spelling"
        );
    }

    #[test]
    fn a_folded_checkout_resolves_a_mixed_case_source_spelling() {
        // The fallback half, end to end: the file is lower case on disk and the
        // source spells the class in mixed case.
        let fs = CountingFs::new(&[("/src/myapp/cache.cls", "CLASS myapp.cache: END CLASS.")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        assert!(matches!(
            index.class(&IndexName::new("MyApp.Cache")),
            IndexAnswer::Found(_)
        ));
    }

    #[test]
    fn a_mixed_case_run_target_resolves_against_a_mixed_case_file() {
        let fs = CountingFs::new(&[("/src/Orders/Post-Order.p", "MESSAGE \"posted\".")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        let IndexAnswer::Found(file) = index.program(&IndexName::new("Orders/Post-Order.p")) else {
            panic!("a RUN target keeps its casing, so the mixed-case file is found");
        };
        assert_ne!(file.raw(), 0, "ids are minted from 1");
    }

    #[test]
    fn a_miss_under_one_spelling_does_not_poison_another() {
        // The memo-poisoning case. `myapp.cache` misses (no lower-case file), and
        // that miss must not be handed to `MyApp.Cache`, whose own search finds
        // the file. The answer caches are keyed case-sensitively for exactly this.
        let fs = CountingFs::new(&[("/src/MyApp/Cache.cls", "CLASS MyApp.Cache: END CLASS.")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        assert_eq!(
            index.class(&IndexName::new("myapp.cache")),
            IndexAnswer::NotFound,
            "no lower-case file on the paths, and the folded fallback is the same search"
        );
        assert!(
            matches!(
                index.class(&IndexName::new("MyApp.Cache")),
                IndexAnswer::Found(_)
            ),
            "the memoized miss must not be served to a different spelling"
        );
        // And the reverse direction: the recorded hit is likewise not handed to
        // the spelling that genuinely misses.
        assert_eq!(
            index.class(&IndexName::new("myapp.cache")),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn a_repeated_query_hits_the_memo_rather_than_re_reading() {
        let fs = CountingFs::new(&[("/src/orders/calc-base.cls", PARENT)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        let name = IndexName::new("orders.calc-base");
        assert!(matches!(index.class(&name), IndexAnswer::Found(_)));
        assert!(matches!(index.class(&name), IndexAnswer::Found(_)));
        assert!(matches!(index.class_members(&name), IndexAnswer::Found(_)));
        assert_eq!(fs.reads_of("/src/orders/calc-base.cls"), 1);
    }

    #[test]
    fn a_miss_is_memoized_too() {
        // Nothing to read, so the read count cannot prove this; what it proves is
        // that the answer is stable and cheap to repeat.
        let fs = CountingFs::new(&[]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let name = IndexName::new("orders.absent");
        assert_eq!(index.class(&name), IndexAnswer::NotFound);
        assert_eq!(index.class(&name), IndexAnswer::NotFound);
    }

    #[test]
    fn a_shared_producer_is_found_among_the_files_the_run_indexed() {
        let fs = CountingFs::new(&[(
            "/src/init-globals.p",
            "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
        )]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let shared = IndexName::new("v-site-code");

        assert_eq!(
            index.shared_producer(&shared),
            IndexAnswer::NotFound,
            "nothing has pulled the producer in yet"
        );
        let IndexAnswer::Found(file) = index.program(&IndexName::new("init-globals.p")) else {
            panic!("the program is on the paths");
        };
        assert_eq!(index.shared_producer(&shared), IndexAnswer::Found(file));
        assert_eq!(
            index.shared_producer(&IndexName::new("v-never-defined")),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn a_view_names_only_a_producer_its_own_lookups_reached() {
        let fs = CountingFs::new(&[(
            "/src/init-globals.p",
            "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
        )]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let shared = IndexName::new("v-site-code");

        let earlier = index.excluding(Some(Path::new("/src/starter.p")));
        let IndexAnswer::Found(file) = earlier.program(&IndexName::new("init-globals.p")) else {
            panic!("the program is on the paths");
        };
        assert_eq!(earlier.shared_producer(&shared), IndexAnswer::Found(file));

        // The memo now holds the producer, but this file never reached it.
        let later = index.excluding(Some(Path::new("/src/report.p")));
        assert_eq!(later.shared_producer(&shared), IndexAnswer::NotFound);
        assert_eq!(
            later.program(&IndexName::new("init-globals.p")),
            IndexAnswer::Found(file)
        );
        assert_eq!(later.shared_producer(&shared), IndexAnswer::Found(file));
    }

    #[test]
    fn two_producers_of_one_shared_name_are_unknowable() {
        let definition = "DEFINE NEW GLOBAL SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.";
        let fs = CountingFs::new(&[("/src/init-a.p", definition), ("/src/init-b.p", definition)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        assert!(matches!(
            index.program(&IndexName::new("init-a.p")),
            IndexAnswer::Found(_)
        ));
        assert!(matches!(
            index.program(&IndexName::new("init-b.p")),
            IndexAnswer::Found(_)
        ));
        assert_eq!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Unknowable
        );
    }

    #[test]
    fn one_file_reached_through_two_path_spellings_gets_one_id_and_one_answer() {
        // Two `RUN` targets naming the same physical file two ways —
        // `init-globals.p` and `sub/../init-globals.p`. Both entries are present in
        // the filesystem because that is what a real filesystem does with a `..`
        // segment: it resolves it and hands back the same file.
        //
        // Keyed verbatim, the `facts` memo mints two `IndexedFileId`s for the one
        // file, and both of the assertions below fail: `program` answers two
        // different ids, and `shared_producer` — which scans `facts` and declines
        // when two *different* files define one name — sees the duplicate as two
        // producers and answers `Unknowable` for a name with exactly one.
        let definition = "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.";
        let fs = CountingFs::new(&[
            ("/src/init-globals.p", definition),
            ("/src/sub/../init-globals.p", definition),
        ]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);

        let IndexAnswer::Found(direct) = index.program(&IndexName::new("init-globals.p")) else {
            panic!("the program is on the paths");
        };
        let IndexAnswer::Found(via_dot_dot) =
            index.program(&IndexName::new("sub/../init-globals.p"))
        else {
            panic!("and so is the same program spelled with a `..`");
        };
        assert_eq!(
            direct, via_dot_dot,
            "two spellings of one path are one file, so one id"
        );
        assert_eq!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Found(direct),
            "and one producer — not an `Unknowable` faked by the file being \
             counted twice"
        );
    }

    #[test]
    fn the_seeded_file_set_gives_a_shared_producer_link_with_no_run_to_pull_it_in() {
        // The command-line shape: the walk knows every file it is about to read,
        // so a `SHARED` consumer can be linked to its producer even though no
        // `RUN` names the producing file. Without the seed this answers
        // `NotFound`, which is the assertion in
        // `a_shared_producer_is_found_among_the_files_the_run_indexed`.
        let fs = CountingFs::new(&[
            (
                "/src/init-globals.p",
                "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
            ),
            (
                "/src/report.p",
                "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
            ),
        ]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/init-globals.p", "/src/report.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);

        assert_eq!(
            fs.reads_of("/src/init-globals.p"),
            0,
            "seeding reads nothing until a `SHARED` name is actually asked about"
        );
        let IndexAnswer::Found(producer) = index.shared_producer(&IndexName::new("v-site-code"))
        else {
            panic!("the seeded set contains the producer");
        };
        assert_eq!(fs.reads_of("/src/init-globals.p"), 1);
        // Repeating the question must not re-index the set.
        assert_eq!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Found(producer)
        );
        assert_eq!(fs.reads_of("/src/init-globals.p"), 1);
        assert_eq!(
            index.shared_producer(&IndexName::new("v-never-defined")),
            IndexAnswer::NotFound,
            "a name nothing in the walk produces is still not found"
        );
    }

    #[test]
    fn a_seeded_file_is_the_same_entry_a_name_lookup_would_have_produced() {
        // The seed must not mint a second identity for a file a lookup also
        // reaches, or `shared_producer` would see one file as two producers.
        let fs = CountingFs::new(&[(
            "/src/init-globals.p",
            "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
        )]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/init-globals.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);

        let IndexAnswer::Found(via_run) = index.program(&IndexName::new("init-globals.p")) else {
            panic!("the program is on the paths");
        };
        assert_eq!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Found(via_run),
            "one file, one id, however it was reached"
        );
        assert_eq!(fs.reads_of("/src/init-globals.p"), 1);
    }

    #[test]
    fn a_shared_map_answers_what_the_seeded_list_answers() {
        let fs = CountingFs::new(&[
            (
                "/src/init-globals.p",
                "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
            ),
            (
                "/src/report.p",
                "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
            ),
            ("/src/quiet.p", "MESSAGE \"quiet\"."),
        ]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/init-globals.p", "/src/report.p", "/src/quiet.p"]);
        let map = SharedProducers::build(
            &known,
            &IncludeContext {
                fs: &fs,
                include_paths: &paths,
                root_relative_paths: &[],
                predefined: &PreprocessorConfig::default(),
            },
        );
        let seeded = BatchIndex::new(&fs, &paths).seeded_with(&known);
        let mapped = BatchIndex::new(&fs, &paths).with_shared_producers(&map);

        for name in ["v-site-code", "v-never-defined"] {
            let name = IndexName::new(name);
            let answer = |index: &BatchIndex<'_>| match index.shared_producer(&name) {
                IndexAnswer::Found(file) => index.indexed_path(file),
                _ => None,
            };
            assert_eq!(answer(&mapped), answer(&seeded), "{name:?}");
        }
    }

    #[test]
    fn indexes_that_borrow_one_map_do_not_scan_the_file_list() {
        let fs = CountingFs::new(&[
            (
                "/src/init-globals.p",
                "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
            ),
            ("/src/quiet.p", "MESSAGE \"quiet\"."),
        ]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/init-globals.p", "/src/quiet.p"]);
        let map = SharedProducers::build(
            &known,
            &IncludeContext {
                fs: &fs,
                include_paths: &paths,
                root_relative_paths: &[],
                predefined: &PreprocessorConfig::default(),
            },
        );
        assert_eq!(fs.reads_of("/src/quiet.p"), 1);

        for _ in 0..3 {
            let index = BatchIndex::new(&fs, &paths).with_shared_producers(&map);
            assert!(matches!(
                index.shared_producer(&IndexName::new("v-site-code")),
                IndexAnswer::Found(_)
            ));
        }
        assert_eq!(
            fs.reads_of("/src/quiet.p"),
            1,
            "no index went back to a file that produces nothing"
        );
    }

    #[test]
    fn a_producer_a_lookup_pulled_in_still_counts_beside_the_map() {
        // The map covers the walk. A file outside it, reached by a `RUN`, can
        // produce the same name, and two producers is `Unknowable` however they
        // were found.
        let definition = "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.";
        let fs = CountingFs::new(&[
            ("/src/init-globals.p", definition),
            ("/elsewhere/init-copy.p", definition),
        ]);
        let paths = dirs(&["/src", "/elsewhere"]);
        let walk = dirs(&["/src/init-globals.p"]);
        let map = SharedProducers::build(
            &walk,
            &IncludeContext {
                fs: &fs,
                include_paths: &paths,
                root_relative_paths: &[],
                predefined: &PreprocessorConfig::default(),
            },
        );
        let index = BatchIndex::new(&fs, &paths).with_shared_producers(&map);

        assert!(matches!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Found(_)
        ));
        assert!(matches!(
            index.program(&IndexName::new("init-copy.p")),
            IndexAnswer::Found(_)
        ));
        assert_eq!(
            index.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Unknowable
        );
    }

    #[test]
    fn an_excluded_file_answers_no_class_of_its_own() {
        let fs = CountingFs::new(&[("/src/orders/calc-base.cls", PARENT)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let name = IndexName::new("orders.calc-base");

        // The control: with no identity, nothing is excluded — the browser's case.
        let open = index.excluding(None);
        assert!(matches!(open.class(&name), IndexAnswer::Found(_)));
        assert!(matches!(open.class_members(&name), IndexAnswer::Found(_)));
        assert_eq!(open.revision(), index.revision());

        // Asking *as* that file: its own class must not come back, either as a
        // descriptor or as a member list. `Unusable` rather than `NotFound`,
        // because the file exists — it just cannot stand in as a foreign one, and
        // a consumer that got `NotFound` could report a name declared in the
        // buffer in front of the user as absent from the workspace.
        let itself = index.excluding(Some(Path::new("/src/orders/calc-base.cls")));
        assert_eq!(itself.class(&name), IndexAnswer::Unusable);
        assert_eq!(itself.class_members(&name), IndexAnswer::Unusable);
        // A different file's view is unaffected, and the memo is shared, so the
        // parent was still read exactly once across all three views.
        let other = index.excluding(Some(Path::new("/src/orders/child.cls")));
        assert!(matches!(other.class(&name), IndexAnswer::Found(_)));
        assert_eq!(fs.reads_of("/src/orders/calc-base.cls"), 1);
    }

    #[test]
    fn an_excluded_file_answers_no_program_or_shared_producer_of_its_own() {
        let fs = CountingFs::new(&[(
            "/src/init-globals.p",
            "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.",
        )]);
        let paths = dirs(&["/src"]);
        let known = dirs(&["/src/init-globals.p"]);
        let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
        let excluded = index.excluding(Some(Path::new("/src/init-globals.p")));

        assert_eq!(
            excluded.program(&IndexName::new("init-globals.p")),
            IndexAnswer::Unusable,
            "a file does not `RUN` itself into the index — and its own path is not \
             absent from the workspace either, so the answer is the located-but- \
             unusable one. A program that runs itself persistently is an ordinary \
             ABL idiom, and reporting it as undefined would be a false claim."
        );
        assert_eq!(
            excluded.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::NotFound,
            "and it is not its own SHARED producer — that link would name its \
             own copy on disk"
        );
        // The same index, asked as a different file, still answers.
        let other = index.excluding(Some(Path::new("/src/report.p")));
        assert!(matches!(
            other.shared_producer(&IndexName::new("v-site-code")),
            IndexAnswer::Found(_)
        ));
    }

    #[test]
    fn a_path_spelled_differently_is_still_the_excluded_file() {
        // Exclusion goes through the same lexical normalization the facts memo
        // keys on, so a caller that spells the analysed path with a `.` or a `..`
        // is not silently un-excluded.
        let fs = CountingFs::new(&[("/src/orders/calc-base.cls", PARENT)]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let name = IndexName::new("orders.calc-base");
        let spelled = index.excluding(Some(Path::new("/src/./orders/sub/../calc-base.cls")));
        assert_eq!(spelled.class(&name), IndexAnswer::Unusable);
    }

    #[test]
    fn an_excluding_view_is_shareable_across_threads() {
        // `WorkspaceIndex` deliberately does *not* require `Send + Sync` (a
        // salsa-snapshot-backed index could never satisfy it), so this
        // implementation pins its own shareability rather than inheriting it.
        // The view is what a client actually hands to the semantic layer.
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<ExcludingFile<'_, '_>>();
    }

    #[test]
    fn the_revision_is_never_absent() {
        let fs = InMemoryFileSystem::new();
        let paths = Vec::new();
        let index = BatchIndex::new(&fs, &paths);
        assert_ne!(index.revision(), IndexRevision::ABSENT);
    }

    #[test]
    fn the_index_is_shareable_across_threads() {
        // Not inherited from `WorkspaceIndex`, which deliberately carries no such
        // bound — this implementation claims it for itself. The memo's interior
        // mutability is the only reason the claim could fail, so pin it.
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<BatchIndex<'_>>();

        let fs = InMemoryFileSystem::new();
        let paths = Vec::new();
        let index: &dyn WorkspaceIndex = &BatchIndex::new(&fs, &paths);
        assert_eq!(
            index.class(&IndexName::new("orders.absent")),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn case_insensitive_lookup_is_off_by_default_and_opt_in() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            PathBuf::from("/src/pkg/Foo.cls"),
            "CLASS pkg.Foo: END CLASS.",
        );
        fs.insert(PathBuf::from("/src/sub/Prog.p"), "MESSAGE 1.");
        let paths = dirs(&["/src"]);
        let class = IndexName::new("pkg.FOO");
        let program = IndexName::new("SUB/prog.p");

        let off = BatchIndex::new(&fs, &paths);
        assert_eq!(off.class(&class), IndexAnswer::NotFound);
        assert_eq!(off.program(&program), IndexAnswer::NotFound);

        let on = BatchIndex::new(&fs, &paths).with_case_insensitive_lookup(true);
        assert!(matches!(on.class(&class), IndexAnswer::Found(_)));
        assert!(matches!(on.program(&program), IndexAnswer::Found(_)));
    }
}
