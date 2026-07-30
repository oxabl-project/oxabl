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
    /// Facts per indexed file. Keyed by the path the search resolved, which is
    /// what dedups a shared dependency: two names reaching the same file find
    /// the entry already there.
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
}

impl<'a> BatchIndex<'a> {
    /// Build an index over `fs` and `include_paths`. Reads nothing yet.
    ///
    /// Kept to the two things every client already has. The analysed file's own
    /// identity — which a later unit needs so a file cannot resolve a name to
    /// itself — arrives as a builder method on top of this, the way
    /// `with_preprocess` sits on the pipeline's config: applied before any
    /// lookup, so it cannot invalidate a memo entry that was computed without it.
    pub fn new(fs: &'a dyn FileSystem, include_paths: &'a [PathBuf]) -> Self {
        BatchIndex {
            fs,
            include_paths,
            memo: RwLock::new(Memo {
                facts: FxHashMap::default(),
                classes: FxHashMap::default(),
                programs: FxHashMap::default(),
                next_file_id: 1,
            }),
        }
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
        if let Some(hit) = self.facts.get(path) {
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
        self.facts.insert(path.to_path_buf(), Arc::clone(&facts));
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
        let memo = self.memo();
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
    fn the_revision_is_never_absent() {
        let fs = InMemoryFileSystem::new();
        let paths = Vec::new();
        let index = BatchIndex::new(&fs, &paths);
        assert_ne!(index.revision(), IndexRevision::ABSENT);
    }

    #[test]
    fn the_index_is_shareable_across_threads() {
        // `WorkspaceIndex` requires `Send + Sync`; the memo's interior mutability
        // is the only reason that could fail, so pin it.
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
