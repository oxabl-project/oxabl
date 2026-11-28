//! The threads of a daemon workspace pass share the facts of the files they
//! reach: a program that many files run is parsed once for the pass, not once
//! for each thread.
//!
//! The test runs the daemon's pass in this process on four threads and reads
//! the bytes that the parser counted (`oxabl_parser::parse_counts`, a test
//! build). Nothing else runs in this process, so the count is the pass's.
//!
//! All fixtures are synthetic ABL written for this file.

use std::fs;

use oxabl_daemon::SessionHost;
use oxabl_daemon::methods::{limit_pass_jobs, reference_graph};
use oxabl_daemon::session::canonical_root;

const CALLERS: usize = 60;
const JOBS: usize = 4;

/// A large target that every caller runs.
fn target() -> String {
    let mut source = String::new();
    for n in 0..60 {
        source.push_str(&format!(
            "PROCEDURE work{n}:\n    DEFINE VARIABLE iCount{n} AS INTEGER NO-UNDO.\n    iCount{n} = {n}.\nEND PROCEDURE.\n\n"
        ));
    }
    source
}

#[test]
fn a_program_that_many_files_run_is_parsed_once_for_the_pass() {
    let dir = tempfile::tempdir().unwrap();
    let root = canonical_root(dir.path()).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"facts\"\n[workspace.sources]\ninclude_paths = [\"lib\"]\n",
    )
    .unwrap();
    fs::create_dir_all(root.join("lib")).unwrap();
    fs::create_dir_all(root.join("src")).unwrap();
    let target = target();
    fs::write(root.join("lib/util.p"), &target).unwrap();
    let mut callers = 0;
    for n in 0..CALLERS {
        let text = format!("RUN util.p.\nMESSAGE \"{n}\".\n");
        callers += text.len();
        fs::write(root.join(format!("src/c{n}.p")), text).unwrap();
    }

    limit_pass_jobs(JOBS);
    let (_, before) = oxabl_parser::parse_counts().expect("a test build counts the parses");
    let host = SessionHost::new();
    let graph = reference_graph(&host, &root)
        .expect("the pass runs")
        .expect("the graph is available");
    let (_, after) = oxabl_parser::parse_counts().unwrap();
    assert!(
        graph.graph.tracked_files().len() > CALLERS,
        "the pass reached the callers and the target"
    );

    let parsed = after - before;
    // Each caller once. The target twice: once as a root of the pass, and once
    // as the facts that every caller takes from. A target that each thread
    // parses for its own facts passes the bound.
    let size = target.len();
    assert!(
        parsed >= callers + 2 * size,
        "the pass must read the callers and the target: {parsed} bytes"
    );
    assert!(
        parsed <= callers + 2 * size + size / 2,
        "the target ({size} bytes) was parsed more than twice for the pass, or the callers \
         ({callers} bytes) more than once each: {parsed} bytes"
    );
}
