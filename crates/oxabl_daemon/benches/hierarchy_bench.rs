//! The first `typeHierarchy/subtypes` request after the workspace pass.
//!
//! - `hierarchy/first_subtypes`: a workspace of one base class and 400 direct
//!   subclasses, each with a few members. The setup opens a new session for the
//!   root and runs the workspace pass. The daemon builds the subtype index on a
//!   thread of its own after the pass, and the setup waits for it
//!   (`wait_for_subtype_index`). The timed part is the `subtypes` request for
//!   the base class, as the first request of a session meets the index.
//! - `hierarchy/next_subtypes`: the same request on a session that already
//!   answered one.
//!
//! The workspace is written once, in a temporary directory. The pass reads
//! from disk, outside the timed part, and runs on the calling thread
//! (`methods::limit_pass_jobs(1)`): CodSpeed counts the instructions of the
//! thread that runs the bench and none of a worker's. The build of the index
//! is on a thread of its own, so it is not in the number: the number is what
//! the request itself costs.

use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;

use criterion::{BatchSize, Criterion, black_box, criterion_group, criterion_main};
use oxabl_daemon::SessionHost;
use oxabl_daemon::hierarchy::{
    HierarchyQuery, NoProgress, TypeKey, answer_hierarchy, wait_for_subtype_index,
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

    /// A new session whose workspace pass is current and whose subtype index is
    /// built.
    fn session(&self) -> SessionHost {
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
    /// number that means nothing, so the answer is checked.
    fn subtypes(&self, host: &SessionHost, key: &TypeKey) -> usize {
        let answer = self.ask(host, &HierarchyQuery::Subtypes(key.clone()));
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

    group.bench_function("first_subtypes", |bencher| {
        bencher.iter_batched(
            || {
                let host = workspace.session();
                let key = workspace.base_key(&host);
                (host, key)
            },
            |(host, key)| black_box(workspace.subtypes(&host, &key)),
            BatchSize::PerIteration,
        );
    });

    {
        let host = workspace.session();
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
