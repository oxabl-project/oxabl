//! The subtype index: its build, and the first request after the workspace
//! pass.
//!
//! The workspace is one base class and 400 direct subclasses, each with a few
//! members.
//!
//! - `hierarchy/build_after_pass`: the build of the index on a new session
//!   whose workspace pass is current, so no fact of a class file is memoized.
//!   It reads the facts of every class file and builds the reverse index.
//! - `hierarchy/build_after_edit`: the build again after an edit of one
//!   subclass buffer, on a session that built the index once. The facts of the
//!   other class files are memoized.
//! - `hierarchy/first_subtypes`: the `subtypes` request for the base class on
//!   a new session that asked for the index before its workspace pass, so the
//!   pass started the build in the background (`want_subtype_index`). The setup
//!   waits for the build (`wait_for_subtype_index`), and the request must find
//!   the index ready: a request that starts a build fails the bench.
//! - `hierarchy/next_subtypes`: the same request on a session that already
//!   answered one.
//!
//! The workspace is written once, in a temporary directory. The pass reads
//! from disk, outside the timed part, and runs on the calling thread
//! (`methods::limit_pass_jobs(1)`): CodSpeed counts the instructions of the
//! thread that runs the bench and none of a worker's. For the same reason, the
//! two build benches build on the calling thread (`build_subtype_index_here`).
//! The request benches time no build.

use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;

use criterion::{BatchSize, Criterion, black_box, criterion_group, criterion_main};
use oxabl_daemon::SessionHost;
use oxabl_daemon::hierarchy::{
    HierarchyQuery, NoProgress, TypeKey, answer_hierarchy, build_subtype_index_here,
    subtype_index_builds, wait_for_subtype_index, want_subtype_index,
};
use oxabl_daemon::methods::{limit_pass_jobs, reference_graph};
use oxabl_daemon::session::canonical_root;
use oxabl_pipeline::PipelineConfig;

/// Direct subclasses of the base class.
const SUBCLASSES: usize = 400;

const BASE: &str = "CLASS app.Base:\n    METHOD PUBLIC CHARACTER Describe():\n        RETURN \"base\".\n    END METHOD.\nEND CLASS.\n";

fn subclass(n: usize) -> String {
    format!(
        "USING app.*.\nCLASS app.Sub{n} INHERITS app.Base:\n\
         \x20   DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.\n\
         \x20   DEFINE PRIVATE VARIABLE cName AS CHARACTER NO-UNDO.\n\
         \x20   METHOD PUBLIC OVERRIDE CHARACTER Describe():\n\
         \x20       RETURN \"sub{n}\".\n\
         \x20   END METHOD.\n\
         \x20   METHOD PUBLIC VOID Reset():\n\
         \x20       cName = \"\".\n\
         \x20       Count = 0.\n\
         \x20   END METHOD.\n\
         END CLASS.\n"
    )
}

/// The workspace on disk, and the base class's path.
struct Workspace {
    root: PathBuf,
    base: PathBuf,
    // Removes the directory when the workspace drops.
    _dir: tempfile::TempDir,
}

impl Workspace {
    fn new() -> Workspace {
        let dir = tempfile::tempdir().expect("a temporary directory");
        let write = |relative: &str, text: &str| {
            let path = dir.path().join(relative);
            std::fs::create_dir_all(path.parent().expect("a parent")).expect("a directory");
            std::fs::write(path, text).expect("a file");
        };
        write(
            "oxabl.toml",
            "[workspace]\nname = \"hierarchy\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
        );
        write("app/Base.cls", BASE);
        for n in 0..SUBCLASSES {
            write(&format!("app/Sub{n}.cls"), &subclass(n));
        }
        let root = canonical_root(dir.path()).expect("the root is on disk");
        let base = root.join("app/Base.cls");
        Workspace {
            root,
            base,
            _dir: dir,
        }
    }

    /// A new session whose workspace pass is current. With `wanted`, the
    /// session asked for the index before the pass, so the pass started its
    /// build, and the build is done.
    fn session(&self, wanted: bool) -> SessionHost {
        let host = SessionHost::new();
        // The configuration a client resolves from `oxabl.toml`.
        host.with(|sessions| {
            sessions
                .for_root(&self.root)
                .install_config(PipelineConfig {
                    include_paths: vec![self.root.clone()],
                    ..PipelineConfig::default()
                });
        });
        if wanted {
            want_subtype_index(&host, &self.root);
        }
        let graph = reference_graph(&host, &self.root)
            .expect("the pass runs")
            .expect("the workspace has include paths");
        assert!(
            graph.gaps.is_empty(),
            "the pass is current: {:?}",
            graph.gaps
        );
        assert_eq!(graph.graph.tracked_files().len(), SUBCLASSES + 1);
        wait_for_subtype_index(&host, &self.root);
        assert_eq!(
            subtype_index_builds(&host, &self.root),
            u64::from(wanted),
            "a pass builds the index only for a session that asked for it"
        );
        host
    }

    fn ask(
        &self,
        host: &SessionHost,
        query: &HierarchyQuery,
    ) -> oxabl_daemon::hierarchy::HierarchyAnswer {
        let cancel = AtomicBool::new(false);
        answer_hierarchy(host, &self.root, &self.base, query, &cancel, &NoProgress)
            .expect("the request is answered")
    }

    /// Build the index on this thread, and check that it read every class
    /// file.
    fn build_here(&self, host: &SessionHost) -> usize {
        let read = build_subtype_index_here(host, &self.root)
            .expect("the build runs on the calling thread and keeps its index");
        assert_eq!(read, SUBCLASSES + 1);
        read
    }

    /// Open the `n`th edit of the first subclass: a new member, so its facts
    /// change.
    fn edit(&self, host: &SessionHost, n: u64) {
        let path = self.root.join("app/Sub0.cls");
        let text = subclass(0).replace(
            "END CLASS.",
            &format!("METHOD PUBLIC VOID Edit{n}():\n    END METHOD.\nEND CLASS."),
        );
        host.with(|sessions| {
            sessions.for_root(&self.root).set_buffer(
                &path.to_string_lossy(),
                text,
                Some(path.clone()),
            );
        });
    }

    /// The item of the base class, as `prepareTypeHierarchy` gives it.
    fn base_key(&self, host: &SessionHost) -> TypeKey {
        let offset = BASE.find("app.Base").expect("the class name") as u32 + 4;
        let answer = self.ask(
            host,
            &HierarchyQuery::Prepare {
                file: self.base.clone(),
                offset,
            },
        );
        answer.items.into_iter().next().expect("the base class").key
    }

    /// The subtypes request. A bench that times a cut or a gap reports a
    /// number that means nothing, so the answer is checked. The request must
    /// find the index built: a build that it started would run on another
    /// thread, which CodSpeed does not count.
    fn subtypes(&self, host: &SessionHost, key: &TypeKey) -> usize {
        let builds = subtype_index_builds(host, &self.root);
        let answer = self.ask(host, &HierarchyQuery::Subtypes(key.clone()));
        assert_eq!(
            subtype_index_builds(host, &self.root),
            builds,
            "the request found no index built and started a build"
        );
        assert!(
            answer.gaps.is_empty(),
            "a complete answer: {:?}",
            answer.gaps
        );
        assert_eq!(answer.items.len(), SUBCLASSES);
        answer.items.len()
    }
}

fn hierarchy_benchmarks(c: &mut Criterion) {
    limit_pass_jobs(1);
    let workspace = Workspace::new();
    assert!(Path::new(&workspace.base).exists());
    let mut group = c.benchmark_group("hierarchy");
    // Reading 400 class files takes tens of milliseconds at the least.
    group.sample_size(10);

    group.bench_function("build_after_pass", |bencher| {
        bencher.iter_batched(
            || workspace.session(false),
            |host| black_box(workspace.build_here(&host)),
            BatchSize::PerIteration,
        );
    });

    {
        let host = workspace.session(false);
        workspace.build_here(&host);
        let mut edits = 0_u64;
        group.bench_function("build_after_edit", |bencher| {
            bencher.iter_batched(
                || {
                    // New text for every call of the setup, so each measured
                    // build meets a changed buffer.
                    edits += 1;
                    workspace.edit(&host, edits);
                },
                |()| black_box(workspace.build_here(&host)),
                BatchSize::PerIteration,
            );
        });
    }

    group.bench_function("first_subtypes", |bencher| {
        bencher.iter_batched(
            || {
                let host = workspace.session(true);
                let key = workspace.base_key(&host);
                (host, key)
            },
            |(host, key)| black_box(workspace.subtypes(&host, &key)),
            BatchSize::PerIteration,
        );
    });

    {
        let host = workspace.session(true);
        let key = workspace.base_key(&host);
        workspace.subtypes(&host, &key);
        group.bench_function("next_subtypes", |bencher| {
            bencher.iter(|| black_box(workspace.subtypes(&host, &key)));
        });
    }

    group.finish();
}

criterion_group!(benches, hierarchy_benchmarks);
criterion_main!(benches);
