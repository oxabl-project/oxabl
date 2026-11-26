//! The workspace pass after an edit, through the daemon's own entry point.
//!
//! The daemon keeps one workspace pass for each root (`methods::reference_graph`
//! is the call that find references, the reference lenses and the call hierarchy
//! make to get it). An edit of an open buffer makes the next call run a pass
//! again. These benches time that one call, after one edit, on a workspace of
//! 1,200 roots that `oxabl_bench::generate` writes once in the setup:
//!
//! - `edit_pass/leaf`: the edited program is run by two other programs.
//! - `edit_pass/shared_include`: the edited file is a hot include that
//!   hundreds of the roots read.
//! - `edit_pass/no_edit`: no edit between two calls. The stored pass is current.
//!
//! The edit is the one an editor makes: `Session::set_buffer` with new text for
//! a file that is open. The text toggles between two values, so each iteration
//! is a real change. Nothing is saved to disk in the timed part.
//!
//! **File-system calls.** The pass reads `oxabl.toml`, walks the root and reads
//! the files through the real file system: the daemon has no in-memory file
//! system for a pass, and the workspace is written once, in the setup, to a
//! temporary directory. The edited file is the open buffer, so its text does not
//! come from disk. A pass still stats and lists; CodSpeed's simulation leaves
//! those calls out and warns that the bench is dominated by system calls, so
//! the wall time of `cargo bench` is the number to read for the whole cost.
//!
//! The setup opens the edited buffers with the text of the files and runs one
//! full pass, so each timed call is a warm pass after an edit. The bench uses
//! only the public API that every version of the daemon has.

use std::path::PathBuf;

use criterion::{BatchSize, Criterion, black_box, criterion_group, criterion_main};
use oxabl_bench::generate::{GenFile, Knobs, Preset, Workspace, generate};
use oxabl_daemon::SessionHost;
use oxabl_daemon::methods::reference_graph;
use oxabl_daemon::session::canonical_root;

/// The roots that read `h0.i`: a hot include of the `ci` preset.
const SHARED_INCLUDE: &str = "inc/hot/h0.i";

/// The `ci` preset with `NEW SHARED` producers and their consumers, so the
/// pass builds the map of producers as it does on a real workspace. A new
/// workspace, so the pinned `ci` workspace of the other benches does not move.
fn knobs() -> Knobs {
    Knobs {
        programs: 1_100,
        classes: 100,
        lib_includes: 240,
        chain_programs: 40,
        shared_producers: 40,
        shared_vars: 4,
        shared_consumers: 20,
        ..Preset::Ci.knobs()
    }
}

/// The text of `file` with one more line at the end. A line of code, never a
/// comment, so the analysis reads it.
fn edited(text: &str) -> String {
    format!("{text}\nMESSAGE \"edit\".\n")
}

/// A program that exactly two other roots run, by the `RUN <path>` text of the
/// generated files.
fn leaf(workspace: &Workspace) -> &GenFile {
    workspace
        .files
        .iter()
        .filter(|file| file.root && file.path.ends_with(".p"))
        .find(|file| {
            let target = file.path.strip_prefix("src/").unwrap_or(&file.path);
            let call = format!("RUN {target} ");
            let readers = workspace
                .roots()
                .filter(|other| other.path != file.path && other.text.contains(&call))
                .count();
            readers == 2
        })
        .expect("the generated workspace has a program that two roots run")
}

/// A workspace root on disk, a host with a session for it, and one edited file.
struct Fixture {
    host: SessionHost,
    root: PathBuf,
    key: String,
    path: PathBuf,
    original: String,
    changed: String,
    // Removes the directory when the fixture drops.
    _dir: tempfile::TempDir,
}

impl Fixture {
    fn new(workspace: &Workspace, edited_path: &str) -> Fixture {
        let dir = tempfile::tempdir().expect("a temporary directory");
        workspace
            .write(dir.path())
            .expect("the workspace is written");
        let root = canonical_root(dir.path()).expect("the root is on disk");
        let file = workspace.file(edited_path).expect("the file is generated");
        let path = root.join(&file.path);
        let key = path.to_string_lossy().into_owned();
        let host = SessionHost::new();
        host.with(|sessions| {
            sessions
                .for_root(&root)
                .set_buffer(&key, file.text.clone(), Some(path.clone()));
        });
        let fixture = Fixture {
            host,
            root,
            key,
            path,
            original: file.text.clone(),
            changed: edited(&file.text),
            _dir: dir,
        };
        // The first pass is the full one, outside the timed part.
        fixture.pass();
        fixture
    }

    fn edit(&self, text: &str) {
        self.host.with(|sessions| {
            sessions.for_root(&self.root).set_buffer(
                &self.key,
                text.to_string(),
                Some(self.path.clone()),
            );
        });
    }

    /// One call for the workspace pass. An error or a refusal fails the bench:
    /// a bench that times a refusal reports a number that means nothing.
    fn pass(&self) -> usize {
        let graph = reference_graph(&self.host, &self.root)
            .expect("the pass runs")
            .expect("the workspace has include paths");
        assert!(
            graph.gaps.is_empty(),
            "the pass is current: {:?}",
            graph.gaps
        );
        graph.graph.tracked_files().len()
    }
}

fn edit_pass_benchmarks(c: &mut Criterion) {
    let workspace = generate(&knobs());
    assert_eq!(workspace.roots().count(), 1_200);
    let leaf_path = leaf(&workspace).path.clone();
    let shared = workspace
        .files
        .iter()
        .position(|file| file.path == SHARED_INCLUDE)
        .expect("the hot include is generated");
    let readers = workspace
        .files
        .iter()
        .filter(|file| file.includes.contains(&shared))
        .count();
    assert!(readers >= 100, "the shared include has {readers} readers");

    let mut group = c.benchmark_group("edit_pass");
    // A pass over 1,200 files takes tens of milliseconds at the least.
    group.sample_size(10);

    for (name, edited_path) in [
        ("leaf", leaf_path.as_str()),
        ("shared_include", SHARED_INCLUDE),
    ] {
        let fixture = Fixture::new(&workspace, edited_path);
        let tracked = fixture.pass();
        assert!(tracked >= 1_200, "{name}: the pass tracks {tracked} files");
        let mut toggle = false;
        group.bench_function(name, |bencher| {
            bencher.iter_batched(
                || {
                    toggle = !toggle;
                    // The first edit of a run goes to the changed text, the
                    // next back to the original, and so on.
                    if toggle {
                        &fixture.changed
                    } else {
                        &fixture.original
                    }
                },
                |text| {
                    fixture.edit(text);
                    black_box(fixture.pass())
                },
                BatchSize::PerIteration,
            );
        });
    }

    {
        let fixture = Fixture::new(&workspace, &leaf_path);
        group.bench_function("no_edit", |bencher| {
            bencher.iter(|| black_box(fixture.pass()));
        });
    }

    group.finish();
}

criterion_group!(benches, edit_pass_benchmarks);
criterion_main!(benches);
