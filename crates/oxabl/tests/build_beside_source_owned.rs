//! The r-code that oxabl installed beside the sources, after the source is
//! gone, after a switch of `[build].rcode`, after a change by hand and after
//! a kill. The oracle is the fake runtime with the PROPATH the project runs
//! with; the files on disk only say where a file is.

mod common;

use std::fs;
use std::os::unix::process::ExitStatusExt;
use std::process::Command;
use std::time::{Duration, Instant, SystemTime};

use common::built::Built;
use common::fake_runtime;

/// The manifest of the r-code beside the sources.
const MANIFEST: &str = ".oxabl/build/installed";

fn beside() -> Built {
    let workspace = Built::new();
    workspace.beside_source();
    workspace
}

/// Deletes the build store, as a store format change or a user does.
fn delete_store(workspace: &Built) {
    for file in ["state.db", "state.db-wal", "state.db-shm"] {
        let _ = fs::remove_file(workspace.path(&format!(".oxabl/build/{file}")));
    }
}

/// What `program` prints on `propath`, and the file the runtime loaded for it
/// with how; `None` when the runtime finds nothing to run.
fn runs(workspace: &Built, propath: &[&str], program: &str) -> Option<(Vec<String>, String)> {
    let ran = fake_runtime::run(workspace.root(), propath, program);
    let (file, how) = ran.loaded(program)?;
    assert_eq!(ran.code, 0, "{program}: {}{}", ran.stdout, ran.stderr);
    Some((ran.said(), format!("{file} {how}")))
}

/// The paths in the manifest, and whether each has a hash.
fn manifest(workspace: &Built) -> Vec<(String, bool)> {
    if !workspace.exists(MANIFEST) {
        return Vec::new();
    }
    workspace
        .read(MANIFEST)
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let value: serde_json::Value = serde_json::from_str(line).unwrap();
            (
                value["path"].as_str().unwrap().to_string(),
                value.get("blake3").is_some(),
            )
        })
        .collect()
}

/// Every file below `directory` whose name holds the temporary infix of an
/// install.
fn temporaries(workspace: &Built, directory: &str) -> Vec<String> {
    let mut found = Vec::new();
    let mut stack = vec![workspace.path(directory)];
    while let Some(directory) = stack.pop() {
        for entry in fs::read_dir(directory).unwrap().flatten() {
            if entry.file_type().unwrap().is_dir() {
                stack.push(entry.path());
            } else if entry.file_name().to_string_lossy().contains(".oxabl-tmp-") {
                found.push(entry.path().display().to_string());
            }
        }
    }
    found
}

/// A program whose source is deleted does not run any more, also when the
/// store is gone and only the manifest knows the r-code; the directory it
/// leaves empty goes too. R-code with no unit that oxabl never installed
/// stays as it is.
#[test]
fn a_deleted_source_no_longer_runs() {
    for store in ["kept", "deleted"] {
        let workspace = beside();
        workspace.write("src/stay.p", "/* FAKE-SAY stay */\n");
        workspace.write("src/sub/gone.p", "/* FAKE-SAY gone */\n");
        workspace.write("src/shipped.p", "/* FAKE-SAY shipped */\n");
        fake_runtime::compile(workspace.root(), &["src"], "shipped.p", "");
        workspace.remove("src/shipped.p");
        let shipped = workspace.sha256("src/shipped.r");
        workspace.build().succeeded();
        assert_eq!(
            runs(&workspace, &["src/sub"], "gone.p"),
            Some((vec!["gone".to_string()], "src/sub/gone.r rcode".to_string()))
        );

        workspace.remove("src/sub/gone.p");
        if store == "deleted" {
            delete_store(&workspace);
        }
        workspace.build().succeeded();

        assert_eq!(
            runs(&workspace, &["src/sub", "src"], "gone.p"),
            None,
            "{store}"
        );
        assert!(
            !workspace.exists("src/sub"),
            "{store}: the emptied directory"
        );
        assert_eq!(
            runs(&workspace, &["src"], "stay.p").unwrap().1,
            "src/stay.r rcode",
            "{store}"
        );
        assert_eq!(workspace.sha256("src/shipped.r"), shipped, "{store}");
        assert_eq!(
            manifest(&workspace),
            [("src/stay.r".to_string(), true)],
            "{store}"
        );
    }
}

/// R-code beside a deleted source whose bytes are not the ones oxabl
/// installed stays, and the build says so.
#[test]
fn rcode_of_a_deleted_source_that_oxabl_did_not_write_stays() {
    let workspace = beside();
    workspace.write("src/x.p", "/* FAKE-SAY oxabl */\n");
    workspace.build().succeeded();
    workspace.write("src/x.p", "/* FAKE-SAY by hand */\n");
    fake_runtime::compile(workspace.root(), &["src"], "x.p", "");
    let by_hand = workspace.sha256("src/x.r");
    workspace.remove("src/x.p");

    let run = workspace.build();

    run.succeeded();
    assert!(workspace.exists("src/x.r"), "{}", run.stderr);
    assert_eq!(workspace.sha256("src/x.r"), by_hand);
    assert!(run.stderr.contains("kept src/x.r"), "{}", run.stderr);
    assert_eq!(manifest(&workspace), Vec::<(String, bool)>::new());
}

/// A switch to r-code beside the source removes the copy in the output
/// directory, with the directory: a runtime that searched it first now runs
/// the r-code beside the source.
#[test]
fn a_switch_to_beside_source_removes_the_output_directory_rcode() {
    let workspace = Built::new();
    workspace.write("src/hello.p", "/* FAKE-SAY v1 */\n");
    workspace.write("src/sub/deep.p", "/* FAKE-SAY deep v1 */\n");
    workspace.build().succeeded();
    assert_eq!(
        runs(&workspace, &["build/r", "src"], "hello.p"),
        Some((vec!["v1".to_string()], "build/r/hello.r rcode".to_string()))
    );
    assert!(workspace.exists("build/.gitignore"));

    workspace.beside_source();
    workspace.write("src/hello.p", "/* FAKE-SAY v2 */\n");
    workspace.build().succeeded();

    assert_eq!(
        runs(&workspace, &["build/r", "src"], "hello.p"),
        Some((vec!["v2".to_string()], "src/hello.r rcode".to_string()))
    );
    assert_eq!(
        runs(&workspace, &["build/r", "src/sub"], "deep.p")
            .unwrap()
            .1,
        "src/sub/deep.r rcode"
    );
    assert!(!workspace.exists("build"), "the old output directory");
}

/// A new `[build].output` removes the r-code of the old one, which a runtime
/// that still searches it would run.
#[test]
fn a_new_output_directory_removes_the_old_one() {
    let workspace = Built::new();
    workspace.write("src/hello.p", "/* FAKE-SAY v1 */\n");
    workspace.build().succeeded();

    workspace.output("out/r");
    workspace.write("src/hello.p", "/* FAKE-SAY v2 */\n");
    workspace.build().succeeded();

    assert_eq!(
        runs(&workspace, &["build/r", "out/r", "src"], "hello.p"),
        Some((vec!["v2".to_string()], "out/r/hello.r rcode".to_string()))
    );
    assert!(!workspace.exists("build"), "the old output directory");
}

/// A switch back to an output directory removes the r-code beside the
/// sources: a runtime that searches only the sources compiles them, as it
/// did before oxabl, and never runs r-code of an older build.
#[test]
fn a_switch_back_removes_the_rcode_beside_the_source() {
    for store in ["kept", "deleted"] {
        let workspace = beside();
        workspace.write("src/hello.p", "/* FAKE-SAY v1 */\n");
        workspace.write("src/pkg/Greet.cls", "CLASS pkg.Greet:\nEND CLASS.\n");
        workspace.build().succeeded();
        assert_eq!(
            runs(&workspace, &["src"], "hello.p").unwrap().1,
            "src/hello.r rcode"
        );

        workspace.output("build/r");
        workspace.rcode("directory");
        workspace.write("src/hello.p", "/* FAKE-SAY v2 */\n");
        if store == "deleted" {
            delete_store(&workspace);
        }
        workspace.build().succeeded();

        assert_eq!(
            runs(&workspace, &["src"], "hello.p"),
            Some((
                vec!["v2".to_string()],
                "src/hello.p compiled-on-the-fly".to_string()
            )),
            "{store}"
        );
        assert!(!workspace.exists("src/pkg/Greet.r"), "{store}");
        assert_eq!(
            runs(&workspace, &["build/r", "src"], "hello.p").unwrap().1,
            "build/r/hello.r rcode",
            "{store}"
        );
        assert!(!workspace.exists(MANIFEST), "{store}");
    }
}

/// R-code deleted or replaced by hand is installed again, and the runtime
/// runs this build's code.
#[test]
fn rcode_deleted_or_replaced_by_hand_is_restored() {
    let workspace = beside();
    workspace.write("src/hello.p", "/* FAKE-SAY oxabl */\n");
    workspace.build().succeeded();

    workspace.remove("src/hello.r");
    workspace.build().succeeded();
    assert_eq!(
        runs(&workspace, &["src"], "hello.p"),
        Some((vec!["oxabl".to_string()], "src/hello.r rcode".to_string()))
    );

    let source = workspace.read("src/hello.p");
    workspace.write("src/hello.p", "/* FAKE-SAY by hand */\n");
    fake_runtime::compile(workspace.root(), &["src"], "hello.p", "");
    workspace.write("src/hello.p", &source);
    workspace.build().succeeded();
    assert_eq!(runs(&workspace, &["src"], "hello.p").unwrap().0, ["oxabl"]);
}

/// A change by hand that keeps the size and puts the old modification time
/// back still changes the ctime: the next build installs the r-code again.
#[test]
fn rcode_changed_with_its_old_size_and_mtime_is_restored() {
    let workspace = beside();
    workspace.write("src/hello.p", "/* FAKE-SAY version 1 */\n");
    workspace.build().succeeded();
    // A build after the racy window records a stat it trusts.
    std::thread::sleep(Duration::from_millis(2_100));
    workspace.build().succeeded();
    workspace.clear_log();
    let rcode = workspace.path("src/hello.r");
    let modified = fs::metadata(&rcode).unwrap().modified().unwrap();
    let text = workspace.read("src/hello.r");
    assert!(text.contains("version 1"), "{text}");
    fs::write(&rcode, text.replace("version 1", "version 9")).unwrap();
    fs::File::options()
        .write(true)
        .open(&rcode)
        .unwrap()
        .set_modified(modified)
        .unwrap();
    assert_eq!(fs::metadata(&rcode).unwrap().modified().unwrap(), modified);
    assert_eq!(
        runs(&workspace, &["src"], "hello.p").unwrap().0,
        ["version 9"]
    );

    workspace.build().succeeded();

    assert_eq!(workspace.compiled(), ["hello.p"]);
    assert_eq!(
        runs(&workspace, &["src"], "hello.p").unwrap().0,
        ["version 1"]
    );
}

/// The temporary file of an install that a kill stopped is removed.
#[test]
fn a_temporary_file_of_a_stopped_install_is_removed() {
    let workspace = beside();
    workspace.write("src/sub/x.p", "/* FAKE-SAY x */\n");
    workspace.build().succeeded();
    workspace.write("src/sub/x.r.oxabl-tmp-4242", "half");
    workspace.write("src/sub/notes.oxabl-tmp-txt", "the user's");

    workspace.build().succeeded();

    assert!(!workspace.exists("src/sub/x.r.oxabl-tmp-4242"));
    assert!(workspace.exists("src/sub/notes.oxabl-tmp-txt"));
}

/// Kills the build and checks that the kill, not the end of the build,
/// stopped it.
fn kill(child: std::process::Child) {
    let status = Command::new("kill")
        .args(["-KILL", &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success());
    let output = child.wait_with_output().unwrap();
    assert_eq!(
        output.status.signal(),
        Some(9),
        "the build ended before the kill: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Waits until `ready` holds, for up to 30 seconds.
fn wait_until(what: &str, ready: impl Fn() -> bool) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while !ready() {
        assert!(Instant::now() < deadline, "never: {what}");
        std::thread::sleep(Duration::from_millis(5));
    }
}

/// A kill after a unit's r-code is installed and before the store records
/// it: the manifest holds the path, so when the source is gone the next build
/// deletes the r-code, also with no store.
#[test]
fn a_kill_after_an_install_leaves_no_rcode_that_runs() {
    let workspace = beside();
    workspace.workers("count = 1");
    workspace.write("src/a.p", "/* FAKE-SAY a */\n");
    workspace.write("src/b.p", "/* FAKE-SLOW */\n/* FAKE-SAY b */\n");
    let started = SystemTime::now();
    let child = workspace.spawn(&[]);
    wait_until("a.r is installed", || {
        fs::metadata(workspace.path("src/a.r"))
            .and_then(|meta| meta.modified())
            .is_ok_and(|at| at >= started)
    });
    kill(child);
    assert_eq!(
        runs(&workspace, &["src"], "a.p").unwrap().1,
        "src/a.r rcode"
    );

    workspace.remove("src/a.p");
    delete_store(&workspace);
    workspace.build().succeeded();

    assert_eq!(runs(&workspace, &["src"], "a.p"), None);
    assert_eq!(runs(&workspace, &["src"], "b.p").unwrap().0, ["b"]);
    assert!(temporaries(&workspace, "src").is_empty());
    assert_eq!(manifest(&workspace), [("src/b.r".to_string(), true)]);
}

/// A kill after the intents and before any install, with the project's own
/// r-code beside each unit: after the next build each program and class runs
/// this build's code, a unit whose source went in between leaves no r-code,
/// and the manifest names only units.
#[test]
fn a_kill_after_the_intents_leaves_nothing_stale() {
    let workspace = beside();
    workspace.workers("count = 1");
    let write = |version: &str| {
        workspace.write(
            "src/pkg/Greet.cls",
            &format!(
                "CLASS pkg.Greet:\n/* FAKE-SLOW */\n/* FAKE-SAY class {version} */\nEND CLASS.\n"
            ),
        );
        workspace.write(
            "src/greet.p",
            &format!("/* FAKE-SAY greet {version} */\nDEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\n"),
        );
        workspace.write("src/gone.p", &format!("/* FAKE-SAY gone {version} */\n"));
    };
    write("v1");
    for name in ["pkg/Greet.cls", "greet.p", "gone.p"] {
        fake_runtime::compile(workspace.root(), &["src"], name, "");
    }
    write("v2");
    let child = workspace.spawn(&[]);
    wait_until("the intents are in the manifest", || {
        let lines = manifest(&workspace);
        lines.len() == 3 && lines.iter().all(|(_, hashed)| !hashed)
    });
    kill(child);

    workspace.remove("src/gone.p");
    workspace.build().succeeded();

    assert_eq!(
        runs(&workspace, &["src"], "greet.p").unwrap().0,
        ["greet v2", "class v2"]
    );
    assert_eq!(runs(&workspace, &["src"], "gone.p"), None);
    assert!(temporaries(&workspace, "src").is_empty());
    assert_eq!(
        manifest(&workspace),
        [
            ("src/greet.r".to_string(), true),
            ("src/pkg/Greet.r".to_string(), true)
        ]
    );
}
