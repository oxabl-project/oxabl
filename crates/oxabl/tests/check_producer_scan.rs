//! The scan of `oxabl check` for the `NEW SHARED` producers of the walk reads
//! each file once: at most one parse of the text as written and at most one
//! expansion. It runs only when a file asks for a producer.
//!
//! A test build of the binary counts the parses and expansions of the scan for
//! each file and prints them when `OXABL_TEST_TALLY` is set. All fixtures are
//! synthetic ABL written for this file.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\n\
                      include_paths = [\".\", \"inc\"]\n";

const CONSUMER: &str = "DEFINE SHARED VARIABLE v-a AS INTEGER NO-UNDO.\n\
                        DEFINE SHARED VARIABLE v-b AS INTEGER NO-UNDO.\n\
                        DEFINE SHARED VARIABLE v-c AS INTEGER NO-UNDO.\n\
                        MESSAGE v-a v-b v-c.\n";

/// A producer of each kind the scan treats differently, and files that
/// produce nothing.
fn tree(consumer: bool) -> TempDir {
    let dir = TempDir::new().unwrap();
    let root = dir.path();
    write(root, "oxabl.toml", CONFIG);
    write(
        root,
        "inc/defs.i",
        "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.\n",
    );
    write(root, "inc/head.i", "NEW SHARED");
    write(root, "inc/plain.i", "MESSAGE 1.\n");
    // Nothing but an include: read through its expansion.
    write(root, "src/only-inc.p", "{defs.i}\n");
    // Does not parse as written: read through its expansion.
    write(
        root,
        "src/spliced.p",
        "DEFINE {head.i} VARIABLE v-b AS INTEGER NO-UNDO.\nMESSAGE v-b.\n",
    );
    // Says `NEW SHARED` as written, with code of its own: not expanded.
    write(
        root,
        "src/written.p",
        "DEFINE NEW SHARED VARIABLE v-c AS INTEGER NO-UNDO.\n{plain.i}\nMESSAGE v-c.\n",
    );
    // A class with an include: expanded, and it produces nothing.
    write(
        root,
        "src/app/Thing.cls",
        "CLASS app.Thing:\n{plain.i}\nEND CLASS.\n",
    );
    // No `{` and no `NEW SHARED`: neither parsed nor expanded.
    write(root, "src/quiet.p", "MESSAGE \"quiet\".\n");
    if consumer {
        write(root, "src/consumer.p", CONSUMER);
    }
    dir
}

/// The counts of the scan, by file name and event, of `oxabl check -j <jobs>`.
fn tally(dir: &Path, jobs: &str) -> BTreeMap<(String, String), usize> {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_TALLY", "1")
        .args(["check", "--no-format", "-j", jobs, "src"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr:\n{stderr}");
    stderr
        .lines()
        .filter_map(|line| line.strip_prefix("oxabl-test: tally "))
        .map(|line| {
            let mut parts = line.splitn(3, ' ');
            let event = parts.next().unwrap().to_string();
            let count = parts.next().unwrap().parse().unwrap();
            let file = Path::new(parts.next().unwrap())
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned();
            ((file, event), count)
        })
        .collect()
}

fn count(tally: &BTreeMap<(String, String), usize>, file: &str, event: &str) -> usize {
    tally
        .get(&(file.to_string(), event.to_string()))
        .copied()
        .unwrap_or(0)
}

#[test]
fn the_scan_parses_and_expands_each_file_at_most_once() {
    let dir = tree(true);
    for jobs in ["1", "4"] {
        let tally = tally(dir.path(), jobs);
        for ((file, event), n) in &tally {
            assert!(*n <= 1, "-j {jobs}: {file} {event} {n} times: {tally:?}");
        }
        // The control: the scan ran, and expanded the files it must.
        for file in ["only-inc.p", "spliced.p", "Thing.cls"] {
            assert_eq!(
                count(&tally, file, "index.expand"),
                1,
                "-j {jobs}: {file}: {tally:?}"
            );
        }
        assert_eq!(count(&tally, "written.p", "index.parse"), 1, "{tally:?}");
        assert_eq!(count(&tally, "written.p", "index.expand"), 0, "{tally:?}");
        assert_eq!(count(&tally, "quiet.p", "index.parse"), 0, "{tally:?}");
    }
}

#[test]
fn a_walk_with_no_shared_consumer_does_not_scan() {
    let dir = tree(false);
    let quiet = tally(dir.path(), "1");
    assert!(quiet.is_empty(), "{quiet:?}");
    // The control: the same tree with a consumer scans.
    write(dir.path(), "src/consumer.p", CONSUMER);
    assert!(!tally(dir.path(), "1").is_empty());
}
