//! The fake runtime: the runtime rule of the shared case table, and what a
//! project runs after `oxabl build` when its runtime PROPATH comes from its
//! own environment. The runtime is the fake AVM's `-p` and `-new`; the
//! PROPATH is its `PROPATH` variable, never one that oxabl derives.

mod common;

use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, SystemTime};

use common::built::Built;
use common::fake_avm;
use common::fake_runtime::{self, Ran};
use common::runtime_cases::{CASES, Case, Made, class_of};

/// The text of a source that prints `text`: for a path below `pkg/`, the
/// class `pkg.<Name>`.
fn source(path: &str, text: &str) -> String {
    match class_of(path) {
        Some(class) => format!("CLASS {class}:\n/* FAKE-SAY {text} */\nEND CLASS.\n"),
        None => format!("/* FAKE-SAY {text} */\n"),
    }
}

fn write(root: &Path, relative: &str, text: &str) {
    let path = root.join(relative);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, text).unwrap();
}

fn stamp(root: &Path, relative: &str, seconds: u64) {
    let time = SystemTime::UNIX_EPOCH + Duration::from_secs(1_700_000_000 + seconds);
    fs::File::options()
        .write(true)
        .open(root.join(relative))
        .unwrap()
        .set_modified(time)
        .unwrap();
}

/// Makes the files of a case below `root`. r-code is compiled from a source
/// in a stage of its own, then copied into place with an older time.
fn make(root: &Path, case: &Case) {
    for (at, (path, made)) in case.files.iter().enumerate() {
        match made {
            Made::Source(text) => {
                write(root, path, &source(path, text));
                stamp(root, path, 100);
            }
            Made::Rcode(text) => {
                let stage = format!("stage{at}");
                let name = match class_of(path) {
                    Some(class) => format!("{}.cls", class.replace('.', "/")),
                    None => {
                        let file = path.rsplit('/').next().unwrap();
                        format!("{}.p", file.trim_end_matches(".r"))
                    }
                };
                write(root, &format!("{stage}/{name}"), &source(path, text));
                fake_runtime::compile(root, &[&stage], &name, &format!("{stage}-r"));
                let rcode = format!("{stage}-r/{}.r", name.rsplit_once('.').unwrap().0);
                let target = root.join(path);
                fs::create_dir_all(target.parent().unwrap()).unwrap();
                fs::copy(root.join(rcode), &target).unwrap();
                stamp(root, path, 0);
            }
        }
    }
}

/// Whether a run gave what the case expects, or why not.
fn verdict(case: &Case, ran: &Ran) -> Result<(), String> {
    let said = ran.said();
    let ok = match case.says {
        Some(text) => ran.code == 0 && said == [text],
        None => ran.code != 0 && said.is_empty() && ran.stderr.contains("(293)"),
    };
    match ok {
        true => Ok(()),
        false => Err(format!(
            "{}: expected {:?}, got exit {} saying {said:?}\n{}{}",
            case.what, case.says, ran.code, ran.stdout, ran.stderr
        )),
    }
}

#[test]
fn the_shared_case_table_holds_on_the_fake_runtime() {
    let mut failed = Vec::new();
    for case in CASES {
        let dir = tempfile::tempdir().unwrap();
        make(dir.path(), case);
        let ran = fake_runtime::start(dir.path(), case.propath, case.start);
        if let Err(why) = verdict(case, &ran) {
            failed.push(why);
        }
    }
    assert!(failed.is_empty(), "{}", failed.join("\n"));
}

#[test]
fn the_fake_runtime_runs_nothing_without_a_propath() {
    let dir = tempfile::tempdir().unwrap();
    write(dir.path(), "x.p", "/* FAKE-SAY x */\n");
    for propath in [None, Some("")] {
        let mut command = Command::new(fake_avm());
        command.current_dir(dir.path()).args(["-p", "x.p"]);
        match propath {
            Some(value) => command.env("PROPATH", value),
            None => command.env_remove("PROPATH"),
        };
        let output = command.output().unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(1), "{propath:?}: {stderr}");
        assert!(output.stdout.is_empty(), "{propath:?}");
        assert!(stderr.contains("PROPATH is"), "{propath:?}: {stderr}");
    }
}

/// The project of the runtime loop: a program, a program with an include and
/// a user of a class, each printing `<what> <version>`.
fn write_loop_sources(workspace: &Built, version: &str) {
    workspace.write(
        "src/hello.p",
        &format!("/* FAKE-SAY program {version} */\n"),
    );
    workspace.write("src/withinc.p", "{msg.i}\n");
    workspace.write("src/msg.i", &format!("/* FAKE-SAY include {version} */\n"));
    workspace.write(
        "src/pkg/Greet.cls",
        &format!("CLASS pkg.Greet:\n/* FAKE-SAY class {version} */\nEND CLASS.\n"),
    );
    workspace.write(
        "src/greet.p",
        "DEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\n",
    );
}

const PROGRAMS: [&str; 3] = ["hello.p", "withinc.p", "greet.p"];

/// What the three programs print when the project runs them on `propath`.
fn run_loop(workspace: &Built, propath: &[&str]) -> Vec<String> {
    PROGRAMS
        .iter()
        .flat_map(|program| {
            let ran = fake_runtime::run(workspace.root(), propath, program);
            assert_eq!(ran.code, 0, "{program}: {}{}", ran.stdout, ran.stderr);
            ran.said()
        })
        .collect()
}

/// The project at version 1, compiled by the project's own tool with
/// `COMPILE ... SAVE` beside the sources, then edited to version 2 and built
/// by `oxabl build` into `build/r`.
fn edited_and_built() -> Built {
    let workspace = Built::new();
    write_loop_sources(&workspace, "v1");
    fake_runtime::compile(workspace.root(), &["src"], "pkg/Greet.cls", "src");
    for program in PROGRAMS {
        fake_runtime::compile(workspace.root(), &["src"], program, "");
    }
    for rcode in ["hello.r", "withinc.r", "greet.r", "pkg/Greet.r"] {
        assert!(workspace.exists(&format!("src/{rcode}")), "{rcode}");
    }
    assert_eq!(
        run_loop(&workspace, &["src"]),
        ["program v1", "include v1", "class v1"]
    );
    write_loop_sources(&workspace, "v2");
    workspace.build().succeeded();
    for rcode in ["hello.r", "withinc.r", "greet.r", "pkg/Greet.r"] {
        assert!(workspace.exists(&format!("build/r/{rcode}")), "{rcode}");
    }
    workspace
}

/// The incident, pinned: the runtime PROPATH holds the source directory only,
/// as the project has it, and r-code of the project's own tool sits beside the
/// sources. After an edit and `oxabl build`, the program, the include and the
/// class all still print the old text, because the runtime never searches
/// `build/r`. This test asserts the old text on purpose.
#[test]
fn the_runtime_ignores_an_output_directory_it_does_not_search() {
    let workspace = edited_and_built();
    assert_eq!(
        run_loop(&workspace, &["src"]),
        ["program v1", "include v1", "class v1"]
    );
    let ran = fake_runtime::run(workspace.root(), &["src"], "hello.p");
    assert_eq!(
        ran.loaded("hello.p"),
        Some(("src/hello.r".to_string(), "rcode".to_string()))
    );
}

/// The same project with the output entry before the source entry on the
/// runtime PROPATH: the new text runs, compiled from the current source.
#[test]
fn an_edit_shows_in_a_runtime_that_searches_the_output_first() {
    let workspace = edited_and_built();
    assert_eq!(
        run_loop(&workspace, &["build/r", "src"]),
        ["program v2", "include v2", "class v2"]
    );
    let ran = fake_runtime::run(workspace.root(), &["build/r", "src"], "hello.p");
    assert_eq!(
        ran.loaded("hello.p"),
        Some(("build/r/hello.r".to_string(), "rcode".to_string()))
    );
    assert_eq!(
        ran.read_hash("src/hello.p"),
        Some(workspace.sha256("src/hello.p"))
    );
}
