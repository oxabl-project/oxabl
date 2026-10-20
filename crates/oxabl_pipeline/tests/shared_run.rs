//! A [`SharedRun`] walk on several threads gives every file the edge set its
//! own run gives, and reads what the files share once.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use oxabl_pipeline::{DependencyEdges, EdgeTarget, LintPipeline, PipelineConfig, SharedRun};
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

/// Counts reads per path.
struct CountingFs {
    inner: InMemoryFileSystem,
    reads: Mutex<HashMap<PathBuf, usize>>,
}

impl CountingFs {
    fn new(files: &[(&str, &str)]) -> Self {
        let mut inner = InMemoryFileSystem::new();
        for (path, text) in files {
            inner.insert(PathBuf::from(format!("/proj/{path}")), *text);
        }
        CountingFs {
            inner,
            reads: Mutex::default(),
        }
    }

    fn reads_of(&self, path: &str) -> usize {
        let path = PathBuf::from(format!("/proj/{path}"));
        self.reads.lock().unwrap().get(&path).copied().unwrap_or(0)
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
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    }
}

/// `target` as a path, so edge sets from two indexes compare.
fn target(index_path: &dyn Fn(&EdgeTarget) -> Option<PathBuf>, target: &EdgeTarget) -> String {
    match index_path(target) {
        Some(path) => format!("file {}", path.display()),
        None => format!("{target:?}"),
    }
}

/// Every fact of `edges`, sorted, with paths for index ids.
fn facts(
    edges: &DependencyEdges,
    index_path: &dyn Fn(&EdgeTarget) -> Option<PathBuf>,
) -> Vec<String> {
    let t = |x: &Option<EdgeTarget>| x.as_ref().map(|x| target(index_path, x));
    let mut out: Vec<String> = edges
        .edges()
        .iter()
        .map(|e| {
            let to = target(index_path, &e.target);
            format!("edge {:?} {to} {:?} {:?}", e.kind, e.span, e.include_site)
        })
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
    out.extend(edges.conditional_references().iter().map(|r| {
        let to = t(&r.target);
        format!("reference {:?} {} {to:?} {:?}", r.kind, r.name, r.span)
    }));
    for closure in edges.conditional_include_closure() {
        let references: Vec<String> = closure
            .references
            .iter()
            .map(|r| {
                format!(
                    "{:?} {} {} {:?}",
                    r.kind,
                    r.conditional,
                    r.name,
                    t(&r.target)
                )
            })
            .collect();
        out.push(format!(
            "closure {} {:?} {:?} {:?} {references:?}",
            closure.name, closure.via, closure.path, closure.reaches
        ));
    }
    out.push(format!("{:?}", edges.completeness()));
    out.sort();
    out
}

const FILES: &[(&str, &str)] = &[
    (
        "app/a.p",
        "{inc/top.i}\n&IF DEFINED(never) &THEN\n{inc/cond.i}\n&ENDIF\nRUN lib/util.p.\n",
    ),
    (
        "app/b.p",
        "&IF DEFINED(never) &THEN\n{inc/cond.i}\n&ENDIF\nDEFINE VARIABLE t AS app.Thing NO-UNDO.\n",
    ),
    (
        "app/c.p",
        "&IF DEFINED(never) &THEN\n{inc/cond.i}\n{inc/self.i}\n&ENDIF\nRUN lib/util.p.\n",
    ),
    // Its conditional include names the class itself: an edge set never holds
    // its own file, whichever file the target was first analysed for.
    (
        "app/Main.cls",
        "CLASS app.Main:\n&IF DEFINED(never) &THEN\n{inc/self.i}\n&ENDIF\nEND CLASS.\n",
    ),
    ("inc/top.i", "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n"),
    (
        "inc/cond.i",
        "DEFINE VARIABLE o AS app.Thing NO-UNDO.\nRUN lib/util.p.\n{inc/deep.i}\n",
    ),
    ("inc/deep.i", "o = NEW app.Other().\n"),
    (
        "inc/self.i",
        "DEFINE VARIABLE m AS app.Main NO-UNDO.\nm = NEW app.Main().\n",
    ),
    ("lib/util.p", "MESSAGE 1.\n"),
    ("app/Thing.cls", "CLASS app.Thing:\nEND CLASS.\n"),
    ("app/Other.cls", "CLASS app.Other:\nEND CLASS.\n"),
];

const ROOTS: [&str; 4] = ["app/c.p", "app/a.p", "app/b.p", "app/Main.cls"];

/// Each root's facts from a run of its own.
fn alone(config: &PipelineConfig) -> Vec<Vec<String>> {
    let fs = CountingFs::new(FILES);
    ROOTS
        .iter()
        .map(|root| {
            let run = LintPipeline::new(config, &fs).with_conditional_include_closure(true);
            let path = PathBuf::from(format!("/proj/{root}"));
            let source = fs.read(&path).unwrap();
            let handle = run.with_file(path);
            let edges = handle.edge_set(&source).expect("edge set");
            facts(&edges, &|t| match t {
                EdgeTarget::IndexedFile { file, .. } => run.indexed_path(*file),
                _ => None,
            })
        })
        .collect()
}

/// Each root's facts from one shared run on `threads` threads, and the run's
/// filesystem.
fn shared(
    config: &PipelineConfig,
    threads: usize,
    rounds: usize,
) -> (Vec<Vec<String>>, CountingFs) {
    let fs = CountingFs::new(FILES);
    let shared = SharedRun::new(config, &fs);
    let next = AtomicUsize::new(0);
    let out: Mutex<Vec<Option<Vec<String>>>> = Mutex::new(vec![None; ROOTS.len()]);
    std::thread::scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                let run = shared.handle().with_conditional_include_closure(true);
                loop {
                    let at = next.fetch_add(1, Ordering::Relaxed);
                    if at >= ROOTS.len() * rounds {
                        break;
                    }
                    let index = at % ROOTS.len();
                    let path = PathBuf::from(format!("/proj/{}", ROOTS[index]));
                    let source = fs.read(&path).unwrap();
                    let edges = run.with_file(path).edge_set(&source).expect("edge set");
                    let facts = facts(&edges, &|t| match t {
                        EdgeTarget::IndexedFile { file, .. } => shared.indexed_path(*file),
                        _ => None,
                    });
                    let mut out = out.lock().unwrap();
                    if let Some(known) = &out[index] {
                        assert_eq!(*known, facts, "{} differs between rounds", ROOTS[index]);
                    }
                    out[index] = Some(facts);
                }
            });
        }
    });
    let facts = out
        .into_inner()
        .unwrap()
        .into_iter()
        .map(Option::unwrap)
        .collect();
    (facts, fs)
}

#[test]
fn a_shared_run_gives_each_file_the_edge_set_of_its_own_run() {
    let config = config();
    let expected = alone(&config);
    for threads in [1, 4] {
        let (got, _) = shared(&config, threads, 3);
        for ((root, expected), got) in ROOTS.iter().zip(&expected).zip(&got) {
            assert_eq!(expected, got, "{root} on {threads} threads");
        }
    }
    let main = &expected[3];
    assert!(
        !main.iter().any(|f| f.contains("app/Main.cls")),
        "an edge set never names its own file: {main:#?}"
    );
    let c = &expected[0];
    assert!(
        c.iter().any(|f| f.contains("app/Main.cls")),
        "another file's closure names it: {c:#?}"
    );
}

#[test]
fn a_shared_run_reads_what_its_files_share_once() {
    let config = config();
    let (_, fs) = shared(&config, 4, 1);
    // Indexed once for the whole run, whichever threads reach them.
    assert_eq!(fs.reads_of("lib/util.p"), 1);
    assert_eq!(fs.reads_of("app/Thing.cls"), 1);
    // Each of the three files that name `inc/cond.i` expands it to find what
    // it includes, and one of them analyses it, which reads `inc/deep.i` again,
    // for every file.
    assert_eq!(fs.reads_of("inc/cond.i"), 3);
    assert_eq!(fs.reads_of("inc/deep.i"), 3 + 1);
}

#[test]
fn a_shared_run_is_sync() {
    fn sync<T: Sync>() {}
    sync::<SharedRun<'static>>();
}
