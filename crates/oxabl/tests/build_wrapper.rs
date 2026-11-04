//! A wrapper drives `oxabl build` the way the wrapper guide says: it writes an
//! overlay for the machine, runs `oxabl build --root --config-overlay --events
//! --report`, reads the event stream line by line and keeps the report. The
//! wrapper is a shell script, and the worker is the fake AVM.

mod common;

use std::fs;
use std::path::Path;
use std::process::Command;

use common::{fake_avm, run, stderr, stdout};
use tempfile::TempDir;

const SCRIPT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/scripts/acme_build.sh");

/// A workspace that commits `oxabl.toml` with the sources only; everything of
/// the machine comes from the overlay that the wrapper writes.
fn workspace() -> TempDir {
    let dir = TempDir::new().unwrap();
    write(
        dir.path(),
        "oxabl.toml",
        "[workspace]\nname = \"acme\"\n\n[workspace.sources]\ndirectories = [\"src\"]\n",
    );
    write(dir.path(), "env.json", "{}");
    write(dir.path(), "src/ar/post.p", "MESSAGE \"post\".\n");
    write(
        dir.path(),
        "src/ar/close.p",
        "/* FAKE-WARN */\nMESSAGE \"c\".\n",
    );
    write(
        dir.path(),
        "src/gl/Ledger.cls",
        "CLASS gl.Ledger:\nEND CLASS.\n",
    );
    fs::create_dir_all(dir.path().join("fake-state")).unwrap();
    dir
}

fn write(root: &Path, relative: &str, text: &str) {
    let path = root.join(relative);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, text).unwrap();
}

/// The lines the wrapper prints and the code it exits with.
fn wrapper(root: &Path) -> (i32, Vec<String>, String) {
    let mut command = Command::new("bash");
    command
        .arg(SCRIPT)
        .arg(env!("CARGO_BIN_EXE_oxabl"))
        .arg(root)
        .arg(fake_avm())
        .env("FAKE_AVM_LOG", root.join("fake.log"))
        .env("FAKE_AVM_STATE", root.join("fake-state"))
        .env("FAKE_AVM_ENV", root.join("env.json"))
        .env("OXABL_CACHE_MODE", "ci")
        .env_remove("CI")
        .env_remove("OXABL_BUILD_CACHE_DIR")
        .env_remove("OXABL_BUILD_OVERLAY")
        .env_remove("OXABL_DLC_SOURCES");
    let output = run(command);
    let mut lines: Vec<String> = stdout(&output).lines().map(str::to_string).collect();
    lines.sort_by_key(|line| line.starts_with("oxabl exit") || line.starts_with("report "));
    (output.status.code().unwrap_or(-1), lines, stderr(&output))
}

#[test]
fn a_wrapper_builds_through_an_overlay_reads_the_events_and_keeps_the_report() {
    let dir = workspace();

    let (code, lines, errors) = wrapper(dir.path());

    assert_eq!(code, 0, "{lines:?}\n{errors}");
    assert_eq!(lines.first().unwrap(), "rebuild 3");
    for line in [
        "finished ok src/ar/post.p",
        "finished ok src/ar/close.p",
        "finished ok src/gl/Ledger.cls",
        "message warning src/ar/close.p",
        "summary compiled=3 failed=0",
        "oxabl exit 0",
        "report ok=3 failed=0 exit=0",
    ] {
        assert!(lines.iter().any(|l| l == line), "{line}\n{lines:#?}");
    }
    for output in ["ar/post.r", "ar/close.r", "gl/Ledger.r"] {
        assert!(dir.path().join("build/r").join(output).exists(), "{output}");
    }
    let overlay = fs::read_to_string(dir.path().join(".acme/overlay.toml")).unwrap();
    assert!(overlay.contains("[build.workers]"));
}

#[test]
fn a_second_run_of_the_wrapper_compiles_nothing() {
    let dir = workspace();
    wrapper(dir.path());

    let (code, lines, errors) = wrapper(dir.path());

    assert_eq!(code, 0, "{lines:?}\n{errors}");
    assert_eq!(lines.first().unwrap(), "rebuild 0");
    assert!(
        lines.iter().any(|l| l == "summary compiled=0 failed=0"),
        "{lines:#?}"
    );
    assert!(
        !lines.iter().any(|l| l.starts_with("finished")),
        "{lines:#?}"
    );
}

#[test]
fn a_failed_unit_reaches_the_wrapper_as_an_event_a_report_entry_and_the_exit_code() {
    let dir = workspace();
    write(
        dir.path(),
        "src/ar/bad.p",
        "/* FAKE-FAIL */\nMESSAGE \"bad\".\n",
    );

    let (code, lines, errors) = wrapper(dir.path());

    assert_eq!(code, 1, "{lines:?}\n{errors}");
    for line in [
        "finished fail src/ar/bad.p",
        "message error src/ar/bad.p",
        "oxabl exit 1",
        "report ok=3 failed=1 exit=1",
    ] {
        assert!(lines.iter().any(|l| l == line), "{line}\n{lines:#?}");
    }
}

#[test]
fn the_wrapper_guide_shows_the_script_that_the_tests_run() {
    let guide = fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../docs/build-wrapper.md"
    ))
    .unwrap();
    let script = fs::read_to_string(SCRIPT).unwrap();

    assert!(
        guide.contains(&format!("```bash\n{script}```")),
        "docs/build-wrapper.md must hold the text of tests/scripts/acme_build.sh"
    );
}
