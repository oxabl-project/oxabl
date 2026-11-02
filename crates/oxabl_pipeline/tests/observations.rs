//! The observation set of a root: what its analysis read, found absent and
//! listed, including what a shared run computed once for several roots.

use std::collections::{BTreeSet, HashMap};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use oxabl_pipeline::{
    DependencyEdges, LintPipeline, NotObserving, ObservedEdges, PipelineConfig, SharedRun,
};
use oxabl_workspace::{
    CachedFileSystem, FileSystem, InMemoryFileSystem, Observations, ObservingFileSystem,
    RealFileSystem,
};

/// Counts reads per path.
struct CountingFs {
    inner: InMemoryFileSystem,
    reads: Mutex<HashMap<PathBuf, usize>>,
}

impl CountingFs {
    fn new(files: &[(&str, &str)]) -> Self {
        let mut inner = InMemoryFileSystem::new();
        for (path, text) in files {
            inner.insert(PathBuf::from(*path), *text);
        }
        CountingFs {
            inner,
            reads: Mutex::default(),
        }
    }

    fn reads_of(&self, path: &str) -> usize {
        self.reads
            .lock()
            .unwrap()
            .get(Path::new(path))
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
            .or_default() += 1;
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.inner.exists(path)
    }

    fn find_ignoring_case_listing(
        &self,
        dir: &Path,
        relative: &Path,
        listed: &mut Vec<PathBuf>,
    ) -> Vec<PathBuf> {
        self.inner.find_ignoring_case_listing(dir, relative, listed)
    }
}

fn config(include_paths: &[&str]) -> PipelineConfig {
    PipelineConfig {
        include_paths: include_paths.iter().map(PathBuf::from).collect(),
        ..PipelineConfig::default()
    }
}

fn paths(set: &BTreeSet<PathBuf>) -> Vec<&str> {
    set.iter().map(|p| p.to_str().unwrap()).collect()
}

fn observed(run: &LintPipeline<'_>, source: &str) -> Observations {
    let ObservedEdges {
        edges,
        observations,
    } = run
        .edges_of_observed(source)
        .expect("the file system observes");
    edges.expect("the root is analysed");
    observations
}

const BASE: &str = "CLASS app.Base:\n{base-members.i}\nEND CLASS.\n";
const MEMBERS: &str = "DEFINE PUBLIC PROPERTY label AS CHARACTER NO-UNDO GET. SET.\n";
const FIRST: &str = "DEFINE VARIABLE b AS app.Base NO-UNDO.\n";
const SECOND: &str = "DEFINE VARIABLE c AS app.Base NO-UNDO.\nMESSAGE 'two'.\n";

fn shared_class_fs() -> ObservingFileSystem<CountingFs> {
    ObservingFileSystem::new(CountingFs::new(&[
        ("/proj/app/Base.cls", BASE),
        ("/proj/base-members.i", MEMBERS),
        ("/proj/r1.p", FIRST),
        ("/proj/r2.p", SECOND),
    ]))
}

/// The two roots of a shared run name one class, which includes a file. The
/// run reads each of them once, and each root observes both.
#[test]
fn an_include_read_through_a_shared_entry_is_observed_by_each_root() {
    for order in [[1, 2], [2, 1]] {
        let fs = shared_class_fs();
        let config = config(&["/proj"]);
        let run = SharedRun::new(&config, &fs);
        let mut sets = HashMap::new();
        for root in order {
            let (path, source) = match root {
                1 => ("/proj/r1.p", FIRST),
                _ => ("/proj/r2.p", SECOND),
            };
            let handle = run.handle();
            let handle = handle.with_file(path);
            sets.insert(root, observed(&handle, source));
        }

        assert_eq!(fs.inner().reads_of("/proj/base-members.i"), 1);
        for root in [1, 2] {
            let read = paths(sets[&root].read());
            for file in ["/proj/app/Base.cls", "/proj/base-members.i"] {
                assert!(
                    read.contains(&file),
                    "root {root} (order {order:?}) does not observe {file}: {read:?}"
                );
            }
        }
    }
}

/// The target of a conditional include is read and analysed once for a shared
/// run. Each root that names it observes what that took.
#[test]
fn a_conditional_include_target_is_observed_by_each_root_that_names_it() {
    let source = "&IF DEFINED(NEVER) &THEN\n{cond.i}\n&ENDIF\n";
    let fs = ObservingFileSystem::new(CountingFs::new(&[
        (
            "/proj/cond.i",
            "DEFINE VARIABLE z AS app.Base NO-UNDO.\n{deep.i}\n",
        ),
        ("/proj/deep.i", "DEFINE VARIABLE d AS INTEGER NO-UNDO.\n"),
        ("/proj/app/Base.cls", BASE),
        ("/proj/base-members.i", MEMBERS),
    ]));
    let config = config(&["/proj"]);
    let run = SharedRun::new(&config, &fs);
    let mut reads_after_first = 0;
    for root in ["/proj/r1.p", "/proj/r2.p"] {
        if root == "/proj/r2.p" {
            reads_after_first = fs.inner().reads_of("/proj/deep.i");
        }
        let handle = run.handle().with_conditional_include_closure(true);
        let handle = handle.with_file(root);
        let set = observed(&handle, source);
        let read = paths(set.read());
        for file in [
            "/proj/cond.i",
            "/proj/deep.i",
            "/proj/app/Base.cls",
            "/proj/base-members.i",
        ] {
            assert!(
                read.contains(&file),
                "{root} does not observe {file}: {read:?}"
            );
        }
    }
    assert_eq!(
        fs.inner().reads_of("/proj/deep.i"),
        reads_after_first,
        "the second root reads nothing again"
    );
}

/// Roots analysed at once on several threads over one shared run each observe
/// what the entry they share took, whichever thread computed it.
#[test]
fn roots_on_several_threads_each_observe_a_shared_entry() {
    for _ in 0..20 {
        let fs = shared_class_fs();
        let config = config(&["/proj"]);
        let run = SharedRun::new(&config, &fs);
        let sets: Vec<Observations> = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..8)
                .map(|n| {
                    let run = &run;
                    scope.spawn(move || {
                        let handle = run.handle();
                        let handle = handle.with_file(format!("/proj/t{n}.p"));
                        observed(&handle, FIRST)
                    })
                })
                .collect();
            workers.into_iter().map(|w| w.join().unwrap()).collect()
        });
        assert_eq!(fs.inner().reads_of("/proj/base-members.i"), 1);
        for (n, set) in sets.iter().enumerate() {
            let read = paths(set.read());
            assert!(
                read.contains(&"/proj/base-members.i"),
                "thread {n} does not observe the include: {read:?}"
            );
        }
    }
}

/// An include search that finds the file in its second directory observes the
/// first directory's path as absent. A file created there is named by the
/// set, and it changes the edges the root gets.
#[test]
fn a_file_created_at_an_observed_absent_path_is_named_by_the_set() {
    let files = [
        ("/proj/b/x.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n"),
        ("/proj/b/y.i", "DEFINE VARIABLE w AS INTEGER NO-UNDO.\n"),
    ];
    let config = config(&["/proj/a", "/proj/b"]);
    let source = "{x.i}\n{y.i}\n";

    let before = ObservingFileSystem::new(CountingFs::new(&files));
    let root = LintPipeline::new(&config, &before);
    let root = root.with_file("/proj/main.p");
    let set = observed(&root, source);
    assert_eq!(paths(set.absent()), ["/proj/a/x.i", "/proj/a/y.i"]);
    assert!(paths(set.read()).contains(&"/proj/b/x.i"));

    let mut shadowed: Vec<_> = files.to_vec();
    shadowed.push(("/proj/a/x.i", "DEFINE VARIABLE other AS INTEGER NO-UNDO.\n"));
    let after = ObservingFileSystem::new(CountingFs::new(&shadowed));
    let root = LintPipeline::new(&config, &after);
    let root = root.with_file("/proj/main.p");
    let later = observed(&root, source);
    assert!(
        set.absent().contains(Path::new("/proj/a/x.i")),
        "the created path is not named by the earlier set"
    );
    assert!(paths(later.read()).contains(&"/proj/a/x.i"));
    assert!(!paths(later.read()).contains(&"/proj/b/x.i"));
}

/// A root that includes a missing file is analysed, and the missing path is
/// observed, so the file's creation is a change.
#[test]
fn a_missing_include_is_observed_as_absent() {
    let fs = ObservingFileSystem::new(CountingFs::new(&[]));
    let config = config(&["/proj/a", "/proj/b"]);
    let root = LintPipeline::new(&config, &fs);
    let root = root.with_file("/proj/main.p");
    let set = observed(&root, "{gone.i}\n");
    assert_eq!(paths(set.absent()), ["/proj/a/gone.i", "/proj/b/gone.i"]);
}

/// A lookup that falls back to a case-insensitive search lists the
/// directories it searched.
#[test]
fn a_case_insensitive_lookup_is_observed_as_a_listing() {
    let fs = ObservingFileSystem::new(CountingFs::new(&[(
        "/proj/App/base.cls",
        "CLASS app.Base:\nEND CLASS.\n",
    )]));
    let config = PipelineConfig {
        case_insensitive_lookup: true,
        ..config(&["/proj"])
    };
    let root = LintPipeline::new(&config, &fs);
    let root = root.with_file("/proj/main.p");
    let set = observed(&root, "DEFINE VARIABLE b AS app.Base NO-UNDO.\n");
    assert_eq!(paths(set.listed()), ["/proj", "/proj/App"]);
    assert!(paths(set.read()).contains(&"/proj/App/base.cls"));
}

/// The listing of a real directory names every directory the lookup
/// listed, including a matching one that holds no answer.
#[test]
fn a_listing_of_a_real_tree_names_each_matching_directory() {
    let dir = tempfile::tempdir().unwrap();
    let root_dir = dir.path();
    for sub in ["App", "APP"] {
        std::fs::create_dir_all(root_dir.join(sub)).unwrap();
    }
    std::fs::write(
        root_dir.join("App/base.cls"),
        "CLASS app.Base:\nEND CLASS.\n",
    )
    .unwrap();
    let fs = ObservingFileSystem::new(RealFileSystem);
    let config = PipelineConfig {
        case_insensitive_lookup: true,
        include_paths: vec![root_dir.to_path_buf()],
        ..PipelineConfig::default()
    };
    let main = root_dir.join("main.p");
    let root = LintPipeline::new(&config, &fs);
    let root = root.with_file(&main);
    let set = observed(&root, "DEFINE VARIABLE b AS app.Base NO-UNDO.\n");
    let listed: Vec<PathBuf> = set.listed().iter().cloned().collect();
    assert_eq!(
        listed,
        [
            root_dir.to_path_buf(),
            root_dir.join("APP"),
            root_dir.join("App")
        ]
    );
}

fn facts(edges: &DependencyEdges) -> Vec<String> {
    let mut out: Vec<String> = edges
        .edges()
        .iter()
        .map(|e| format!("edge {:?} {:?} {:?}", e.kind, e.span, e.include_site))
        .collect();
    out.extend(
        edges
            .unresolved()
            .iter()
            .map(|u| format!("unresolved {u:?}")),
    );
    out.extend(
        edges
            .conditional_includes()
            .iter()
            .map(|c| format!("conditional {c:?}")),
    );
    out.push(format!("completeness {:?}", edges.completeness()));
    out.sort();
    out
}

/// Observing does not change what a root's analysis finds.
#[test]
fn observing_does_not_change_the_edge_set() {
    let files = [
        ("/proj/app/Base.cls", BASE),
        ("/proj/base-members.i", MEMBERS),
        ("/proj/b/x.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n"),
    ];
    let source = "{x.i}\n{gone.i}\nDEFINE VARIABLE b AS app.Base NO-UNDO.\nRUN missing.p.\n";
    let config = config(&["/proj/a", "/proj/b", "/proj"]);

    let plain_fs = CountingFs::new(&files);
    let plain = LintPipeline::new(&config, &plain_fs);
    let plain = plain.with_file("/proj/main.p");
    let want = plain.edge_set(source).expect("analysed");

    let observing = ObservingFileSystem::new(CachedFileSystem::new(CountingFs::new(&files)));
    let observing_run = LintPipeline::new(&config, &observing);
    let observing_run = observing_run.with_file("/proj/main.p");
    let ObservedEdges { edges, .. } = observing_run.edges_of_observed(source).unwrap();
    assert_eq!(facts(&edges.expect("analysed")), facts(&want));

    let shared = SharedRun::new(&config, &observing);
    let handle = shared.handle();
    let handle = handle.with_file("/proj/main.p");
    let ObservedEdges { edges, .. } = handle.edges_of_observed(source).unwrap();
    assert_eq!(facts(&edges.expect("analysed")), facts(&want));
}

/// A file system that does not observe gives no observation set, never an
/// empty one.
#[test]
fn a_file_system_that_does_not_observe_is_refused() {
    let fs = CountingFs::new(&[]);
    let config = config(&["/proj"]);
    let root = LintPipeline::new(&config, &fs);
    let root = root.with_file("/proj/main.p");
    assert_eq!(
        root.edges_of_observed("MESSAGE 'x'.\n").unwrap_err(),
        NotObserving
    );
}
