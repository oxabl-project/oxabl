//! The scan of `oxabl check` for the `NEW SHARED` producers of the walk reads
//! each file once: at most one parse of the text as written and at most one
//! expansion. It runs only when a file asks for a producer.
//!
//! A test build of the binary prints the programs it parsed when
//! `OXABL_TEST_PARSES` is set, and the expansions of the scan for each file
//! when `OXABL_TEST_TALLY` is set. The consumer of the trees asks for a name
//! that no file produces, so the scan is the only difference between a run
//! that asks and one that does not: nothing is indexed to confirm a producer.
//! All fixtures are synthetic ABL written for this file.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

use tempfile::TempDir;

/// A run of `oxabl check` that must end within this time.
const LIMIT: Duration = Duration::from_secs(60);

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\n\
                      include_paths = [\".\", \"inc\"]\n";

/// Asks for a `SHARED` name that no file of the trees produces.
const ASKING: &str = "DEFINE SHARED VARIABLE v-none AS INTEGER NO-UNDO.\nMESSAGE v-none.\n";

/// The same file with a name of its own: it asks for nothing.
const NOT_ASKING: &str = "DEFINE VARIABLE v-none AS INTEGER NO-UNDO.\nMESSAGE v-none.\n";

/// A producer of each kind the scan treats differently, and files that
/// produce nothing.
fn tree(consumer: &str) -> TempDir {
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
    write(root, "src/consumer.p", consumer);
    dir
}

/// `oxabl check -j <jobs>` in `dir`. The test fails, and the process is
/// killed, when it does not end within [`LIMIT`].
fn check(dir: &Path, jobs: &str) -> Output {
    let capture = TempDir::new().unwrap();
    let file = |name: &str| fs::File::create(capture.path().join(name)).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_TALLY", "1")
        .env("OXABL_TEST_PARSES", "1")
        .args(["check", "--no-format", "-j", jobs, "src"])
        .stdout(Stdio::from(file("out")))
        .stderr(Stdio::from(file("err")))
        .spawn()
        .unwrap();
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if started.elapsed() > LIMIT {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("oxabl check -j {jobs} did not end within {LIMIT:?}");
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    Output {
        status,
        stdout: fs::read(capture.path().join("out")).unwrap(),
        stderr: fs::read(capture.path().join("err")).unwrap(),
    }
}

/// The counts of the scan, by file name and event, of `oxabl check -j <jobs>`.
fn tally(dir: &Path, jobs: &str) -> BTreeMap<(String, String), usize> {
    let out = check(dir, jobs);
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    tally_of(&out)
}

/// The counts a run printed.
fn tally_of(out: &Output) -> BTreeMap<(String, String), usize> {
    let stderr = String::from_utf8_lossy(&out.stderr);
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

/// The programs a run parsed.
fn parses(out: &Output) -> usize {
    let stderr = String::from_utf8_lossy(&out.stderr);
    stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: parses: "))
        .unwrap_or_else(|| panic!("the test build printed no count; stderr:\n{stderr}"))
        .parse()
        .unwrap()
}

fn count(tally: &BTreeMap<(String, String), usize>, file: &str, event: &str) -> usize {
    tally
        .get(&(file.to_string(), event.to_string()))
        .copied()
        .unwrap_or(0)
}

#[test]
fn the_scan_parses_and_expands_each_file_at_most_once() {
    let asking = tree(ASKING);
    let quiet = tree(NOT_ASKING);
    for jobs in ["1", "4"] {
        let without = check(quiet.path(), jobs);
        let with = check(asking.path(), jobs);
        for out in [&without, &with] {
            assert_eq!(out.status.code(), Some(0), "-j {jobs}");
        }
        // The programs the scan parses, one for each file that needs it: the
        // text of `written.p` for its own definition, and `only-inc.p` and
        // `spliced.p` to know whether the index may read them expanded. A class
        // header says that from its tokens (`Thing.cls`), and `quiet.p` has no
        // `{` and no `NEW SHARED`.
        assert_eq!(
            parses(&with) - parses(&without),
            3,
            "-j {jobs}: parses of the scan"
        );
        let tally = tally_of(&with);
        for ((file, event), n) in &tally {
            assert!(*n <= 1, "-j {jobs}: {file} {event} {n} times: {tally:?}");
        }
        for file in ["only-inc.p", "spliced.p", "Thing.cls"] {
            assert_eq!(
                count(&tally, file, "index.expand"),
                1,
                "-j {jobs}: {file}: {tally:?}"
            );
        }
        assert_eq!(count(&tally, "written.p", "index.expand"), 0, "{tally:?}");
        assert_eq!(count(&tally, "quiet.p", "index.expand"), 0, "{tally:?}");
    }
}

#[test]
fn a_walk_with_no_shared_consumer_does_not_scan() {
    let dir = tree(NOT_ASKING);
    let quiet = tally(dir.path(), "1");
    assert!(quiet.is_empty(), "{quiet:?}");
    // The control: the same tree with a consumer scans.
    write(dir.path(), "src/consumer.p", ASKING);
    assert!(!tally(dir.path(), "1").is_empty());
}

/// A definition with a `TRIGGERS` phrase holds a `DEFINE` of its own. The scan
/// of a walk with a consumer must end.
#[test]
fn a_producer_that_holds_a_define_does_not_hang_the_scan() {
    let dir = TempDir::new().unwrap();
    let root = dir.path();
    write(root, "oxabl.toml", CONFIG);
    let trigger = "DEFINE NEW SHARED VARIABLE v-a AS CHARACTER NO-UNDO\n\
                   VIEW-AS FILL-IN\n\
                   TRIGGERS:\n\
                   ON LEAVE DO:\n\
                   DEFINE VARIABLE v-b AS INTEGER NO-UNDO.\n\
                   END.\n\
                   END TRIGGERS.\n";
    write(root, "inc/trig.i", trigger);
    write(root, "src/written.p", trigger);
    write(root, "src/through-include.p", "{trig.i}\n");
    // The output of the walk with no consumer, which does not scan.
    let alone = check(root, "1");
    write(
        root,
        "src/consumer.p",
        "DEFINE SHARED VARIABLE v-a AS CHARACTER NO-UNDO.\nMESSAGE v-a.\n",
    );
    for jobs in ["1", "4"] {
        let out = check(root, jobs);
        assert_eq!(out.status.code(), alone.status.code(), "-j {jobs}");
        assert_eq!(out.stdout, alone.stdout, "-j {jobs}");
        let tally = tally_of(&out);
        assert_eq!(
            count(&tally, "through-include.p", "index.expand"),
            1,
            "{tally:?}"
        );
    }
}
