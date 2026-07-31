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

use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock, RwLockWriteGuard};

use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassDescriptor, IndexAnswer, IndexName, IndexRevision, IndexedFileId, MemberDescriptor,
    WorkspaceIndex,
};
use oxabl_workspace::FileSystem;
use rustc_hash::FxHashMap;

use crate::facts::{ClassFacts, FileFacts};
use crate::{index_file, search};

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
    /// Files the run already knows it is going to read — the CLI walk's own
    /// discovered file list, handed over by [`BatchIndex::seeded_with`].
    ///
    /// Consulted by exactly one query, [`BatchIndex::shared_producer`], and only
    /// on its first call. See that method for why a `SHARED` name needs a file
    /// set at all, and [`Memo::known_indexed`] for why the indexing is deferred
    /// rather than done at construction.
    known_files: &'a [PathBuf],
    /// Interior mutability because [`WorkspaceIndex`] takes `&self` — it has to,
    /// since the language server shares one index behind an `Arc` across
    /// threads, so the trait cannot ask for `&mut`.
    ///
    /// A lock is acceptable here, and specifically not a hot-path concern: it is
    /// taken once per *unresolved cross-file name*, not per token or per node,
    /// and a batch run has a single analysis thread so it is uncontended. The
    /// guard is held across extraction on purpose — that is what makes
    /// "indexed once" true even if two threads ask at the same moment — and
    /// nothing under the guard re-enters the index, so it cannot deadlock.
    memo: RwLock<Memo>,
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
    /// Next id to mint. Ids are assigned in first-index order and mean nothing
    /// outside this index, exactly as [`IndexedFileId`] documents. Starts at 1
    /// so a zero id never reads as a real file in a dump.
    next_file_id: u32,
    /// Whether [`BatchIndex::known_files`] has been folded into `facts` yet.
    ///
    /// The seeded set is indexed **lazily**, on the first `shared_producer` call,
    /// rather than eagerly in [`BatchIndex::new`]. Two reasons, both about R6's
    /// "nothing is read until a name is looked up":
    ///
    /// * `shared_producer` is the only query that cannot derive a candidate path
    ///   from its key, so it is the only one the set exists for. A run whose
    ///   files declare no `DEFINE SHARED` consumer never asks, and must not pay
    ///   for a parse of every file in the walk.
    /// * Eager indexing would double the parse cost of the whole walk up front —
    ///   once for facts, once for the real analysis — including for the files a
    ///   run bails out before reaching.
    ///
    /// A flag rather than "is `facts` empty": the set may legitimately contribute
    /// nothing new (every file already indexed by a class lookup), and re-running
    /// the seed on every subsequent `shared_producer` call would re-`exists`-check
    /// the whole walk per consumed name.
    known_indexed: bool,
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
            known_files: &[],
            memo: RwLock::new(Memo {
                facts: FxHashMap::default(),
                classes: FxHashMap::default(),
                programs: FxHashMap::default(),
                next_file_id: 1,
                known_indexed: false,
            }),
        }
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
    /// Apply before any lookup. A client with no such list (the language server,
    /// the browser) simply does not call this, and behavior is exactly as
    /// before: `shared_producer` then answers only from the files some other
    /// lookup pulled in.
    #[must_use]
    pub fn seeded_with(mut self, known_files: &'a [PathBuf]) -> Self {
        self.known_files = known_files;
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
        ExcludingFile { index: self, path }
    }

    /// The id this run assigned to `path`, or `None` if nothing has indexed it.
    ///
    /// A memo probe only — it never reads. That is what makes it usable *after* a
    /// lookup has answered: by then the file behind the answer is in `facts`, so
    /// [`ExcludingFile`] can compare ids without having to know how the search
    /// turned a name into a path.
    fn indexed_id(&self, path: &Path) -> Option<IndexedFileId> {
        let key = search::normalize_lexically(path);
        self.memo().facts.get(&key).map(|facts| facts.file)
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

    /// Resolve `name` to the facts of the class it names, memoized per key.
    fn class_facts(&self, name: &IndexName) -> IndexAnswer<ClassFacts> {
        let mut memo = self.memo();
        if let Some(hit) = memo.classes.get(name.as_written_atom()) {
            return hit.clone();
        }

        let answer =
            match search::find_name(self.fs, self.include_paths, name, search::NameKind::Class) {
                IndexAnswer::Found(path) => {
                    let facts = memo.facts_for(self.fs, &path);
                    // The file exists but may not declare the class the path
                    // promised — a mis-namespaced file, or one whose parse failed.
                    // Both are knowably unusable, so both are `NotFound`. The
                    // comparison here is the case-insensitive one: the declaring
                    // file is free to spell the class differently from the reference.
                    facts
                        .class(name)
                        .cloned()
                        .map_or(IndexAnswer::NotFound, IndexAnswer::Found)
                }
                IndexAnswer::NotFound => IndexAnswer::NotFound,
                IndexAnswer::Unknowable => IndexAnswer::Unknowable,
            };
        memo.classes
            .insert(name.as_written_atom().clone(), answer.clone());
        answer
    }
}

impl Memo {
    /// The facts for `path`, indexing it on first touch.
    ///
    /// Takes `&mut self` (i.e. runs under the write guard) so the read, the
    /// extraction, and the insert are one atomic step — which is what makes the
    /// shared-dependency dedup a guarantee rather than a race.
    fn facts_for(&mut self, fs: &dyn FileSystem, path: &Path) -> Arc<FileFacts> {
        // Normalized *before* the probe and used for the insert as well, so the
        // two spellings of one file are one key on both halves. `path` itself is
        // what gets read: the normalized form is the key, not the I/O target, so a
        // filesystem that treats the two spellings differently is still asked the
        // question the caller actually posed.
        let key = search::normalize_lexically(path);
        if let Some(hit) = self.facts.get(&key) {
            return Arc::clone(hit);
        }
        let id = IndexedFileId::new(self.next_file_id);
        self.next_file_id += 1;
        let facts = Arc::new(match fs.read(path) {
            Ok(source) => index_file(id, &source),
            // Located but unreadable. Not an error case: a file we cannot read
            // is knowably unusable, and remembering that keeps us from
            // re-attempting the read on every reference to the same name.
            Err(_) => FileFacts::unparseable(id),
        });
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
        let mut memo = self.memo();
        if let Some(hit) = memo.programs.get(target.as_written_atom()) {
            return hit.clone();
        }

        let answer = match search::find_name(
            self.fs,
            self.include_paths,
            target,
            search::NameKind::Program,
        ) {
            IndexAnswer::Found(path) => {
                // The target is indexed rather than merely existence-checked,
                // for two reasons: an unusable file must answer `NotFound`
                // per the seam's totality rule, and a program a file `RUN`s
                // is the realistic producer of the `SHARED` names that file
                // consumes — indexing it here is what gives
                // `shared_producer` something true to say.
                let facts = memo.facts_for(self.fs, &path);
                if facts.parsed {
                    IndexAnswer::Found(facts.file)
                } else {
                    IndexAnswer::NotFound
                }
            }
            IndexAnswer::NotFound => IndexAnswer::NotFound,
            IndexAnswer::Unknowable => IndexAnswer::Unknowable,
        };
        memo.programs
            .insert(target.as_written_atom().clone(), answer.clone());
        answer
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        // Answered from the files this run has already indexed, and deliberately
        // not memoized.
        //
        // A `SHARED` name maps onto no path — unlike a class name or a `RUN`
        // target, there is nothing to derive a candidate file from — so the only
        // ways to answer are to enumerate the workspace or to consult what is
        // already loaded. Enumerating is exactly the whole-workspace scan R6
        // forbids, and the `FileSystem` trait offers no directory listing to do
        // it with, so this consults the memo. Caching the answer would freeze a
        // `NotFound` that a later `RUN` lookup could have turned into a hit,
        // which is why the scan runs each time; it is over the handful of files
        // this run actually pulled in.
        //
        // Two producers of one name is an ambiguity of the same shape the
        // literal-`RUN` rule refuses to guess at, so it declines rather than
        // picking whichever file happened to be indexed first — which also keeps
        // the answer independent of `HashMap` iteration order.
        let mut memo = self.memo();
        // The seeded set, folded in on first ask. This is the only query it feeds,
        // and deferring it to here is what keeps a run that consumes no `SHARED`
        // name from parsing the whole walk — see `Memo::known_indexed`.
        if !memo.known_indexed {
            memo.known_indexed = true;
            for path in self.known_files {
                memo.facts_for(self.fs, path);
            }
        }
        let mut found = None;
        for facts in memo.facts.values() {
            if !facts.defines_shared(name) {
                continue;
            }
            match found {
                None => found = Some(facts.file),
                Some(_) => return IndexAnswer::Unknowable,
            }
        }
        found.map_or(IndexAnswer::NotFound, IndexAnswer::Found)
    }

    fn revision(&self) -> IndexRevision {
        BATCH_REVISION
    }
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
}

impl ExcludingFile<'_, '_> {
    /// Whether `file` is the analysed file. `false` whenever there is no
    /// identity to compare against, which is what makes the no-path client's
    /// behavior identical to an unexcluded index.
    fn is_analysed(&self, file: IndexedFileId) -> bool {
        self.path
            .and_then(|path| self.index.indexed_id(path))
            .is_some_and(|analysed| analysed == file)
    }
}

impl WorkspaceIndex for ExcludingFile<'_, '_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        match self.index.class(name) {
            IndexAnswer::Found(descriptor) if self.is_analysed(descriptor.file) => {
                IndexAnswer::NotFound
            }
            other => other,
        }
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        // The member list carries no file id of its own, so the owning class is
        // asked for one first. That costs nothing: both queries read the same
        // memo entry, so the pair is still one search and one parse.
        if let IndexAnswer::Found(descriptor) = self.index.class(class)
            && self.is_analysed(descriptor.file)
        {
            return IndexAnswer::NotFound;
        }
        self.index.class_members(class)
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        match self.index.program(target) {
            IndexAnswer::Found(file) if self.is_analysed(file) => IndexAnswer::NotFound,
            other => other,
        }
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        match self.index.shared_producer(name) {
            IndexAnswer::Found(file) if self.is_analysed(file) => IndexAnswer::NotFound,
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
    fn a_broken_program_is_not_found_rather_than_an_error() {
        let fs = CountingFs::new(&[("/src/calc-total.p", "DEFINE VARIABLE .")]);
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        assert_eq!(
            index.program(&IndexName::new("calc-total.p")),
            IndexAnswer::NotFound,
            "a broken file is knowably unusable"
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
        // descriptor or as a member list.
        let itself = index.excluding(Some(Path::new("/src/orders/calc-base.cls")));
        assert_eq!(itself.class(&name), IndexAnswer::NotFound);
        assert_eq!(itself.class_members(&name), IndexAnswer::NotFound);
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
            IndexAnswer::NotFound,
            "a file does not `RUN` itself into the index"
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
        assert_eq!(spelled.class(&name), IndexAnswer::NotFound);
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
}
