//! Compiling a project on a real AVM.

use std::io::{BufRead, BufReader, Lines};
use std::process::{Child, ChildStdout, Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::Value;

use crate::common::built::unit;
use crate::common::openedge::Target;
use crate::common::openedge::compile::{
    Compile, Options, compile_workspace, compile_workspace_with,
};
use crate::common::openedge::worker::live_workers;
use crate::setup;

/// Every unit of the fixture and the r-code that it makes.
pub const FIXTURE_UNITS: &[(&str, &str)] = &[
    ("src/cyc/Ping.cls", "cyc/Ping.r"),
    ("src/cyc/Pong.cls", "cyc/Pong.r"),
    ("src/dbread.p", "dbread.r"),
    ("src/helper.p", "helper.r"),
    ("src/bulk.p", "bulk.r"),
    ("src/main.p", "main.r"),
    ("src/pkg/Base.cls", "pkg/Base.r"),
    ("src/pkg/Leaf.cls", "pkg/Leaf.r"),
    ("src/pkg/Mid.cls", "pkg/Mid.r"),
    ("src/usecycle.p", "usecycle.r"),
    ("src/useleaf.p", "useleaf.r"),
];

/// Runs a program of the fixture from the r-code alone.
pub fn run_from_rcode(program: &str) -> String {
    format!(
        "FILE-INFO:FILE-NAME = \".\".\nPROPATH = FILE-INFO:FULL-PATHNAME + \"/build/r\".\nRUN {program}.\nPUT UNFORMATTED \"RAN\" SKIP.\nQUIT.\n"
    )
}

const BASE: &str = "CLASS pkg.Greeter:
    METHOD PUBLIC CHARACTER Greeting():
        RETURN \"hello from a class\".
    END METHOD.
END CLASS.
";

const USER: &str = "DEFINE VARIABLE o AS pkg.Greeter NO-UNDO.
o = NEW pkg.Greeter().
PUT UNFORMATTED o:Greeting() SKIP.
";

/// Reads the CRC of each r-code file and runs the user from r-code alone.
const LOAD: &str = "FILE-INFO:FILE-NAME = \".\".
PROPATH = FILE-INFO:FULL-PATHNAME + \"/build/r\".
RCODE-INFO:FILE-NAME = FILE-INFO:FULL-PATHNAME + \"/build/r/pkg/Greeter.r\".
PUT UNFORMATTED \"CRC-GREETER \" STRING(RCODE-INFO:CRC-VALUE) SKIP.
RCODE-INFO:FILE-NAME = FILE-INFO:FULL-PATHNAME + \"/build/r/greet.r\".
PUT UNFORMATTED \"CRC-GREET \" STRING(RCODE-INFO:CRC-VALUE) SKIP.
RUN greet.p.
QUIT.
";

/// A class and its user compile on a real AVM: the class first, both r-code
/// files exist and load, the user runs from r-code alone, and a second build
/// compiles nothing.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn compile_a_class_and_its_user() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.write("src/pkg/Greeter.cls", BASE);
    workspace.write("src/greet.p", USER);

    let output = workspace.build(&["--json", "--report", "report.json"], 0);

    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    let report: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("one JSON document");
    assert_eq!(report["summary"]["compiled"], 2, "{report}");
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    for unit in ["src/pkg/Greeter.cls", "src/greet.p"] {
        let found = report["units"]
            .as_array()
            .unwrap()
            .iter()
            .find(|candidate| candidate["path"] == unit)
            .expect("the unit is in the report");
        assert_eq!(found["status"], "ok", "{found}");
    }
    for rcode in ["build/r/pkg/Greeter.r", "build/r/greet.r"] {
        let size = std::fs::metadata(workspace.path(rcode))
            .unwrap_or_else(|e| panic!("{rcode} was not produced: {e}"))
            .len();
        assert!(size > 0, "{rcode} is empty");
    }
    let xref = std::fs::read_to_string(workspace.path("build/xref/greet.p.xref.xml"))
        .expect("the XREF-XML of the user");
    eprintln!("XREF-XML of greet.p:\n{xref}");

    let printed = workspace.run_program(&target, LOAD);
    eprintln!("the program printed:\n{printed}");
    assert!(
        printed.contains("hello from a class"),
        "the user did not run from r-code: {printed}"
    );
    assert!(
        !printed.contains("CRC-GREETER ?") && !printed.contains("CRC-GREET ?"),
        "r-code did not load: {printed}"
    );

    let again = workspace.build(&["--json"], 0);
    let again: serde_json::Value = serde_json::from_slice(&again.stdout).unwrap();
    assert_eq!(again["summary"]["compiled"], 0, "{again}");
}

/// The same project built in two workspaces that share a local cache. With
/// no XREF output requested, the build writes a private text `XREF` and reads
/// it back: this is the first time that the read check meets the file names
/// of a real compiler. The second workspace must compile nothing, and its
/// r-code must load.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn two_workspaces_share_compiled_units_through_the_local_cache() {
    let target = setup();
    let cache = tempfile::TempDir::new().unwrap();
    let mut first = compile_workspace(&target);
    first.without_xref_output();
    first.write("src/pkg/Greeter.cls", BASE);
    first.write("src/greet.p", USER);

    let output = first.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));

    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    eprintln!("{report}");
    assert_eq!(report["summary"]["compiled"], 2, "{report}");
    assert_eq!(
        report["cache"]["stored"], 2,
        "both units pass the checks: {report}"
    );
    assert!(
        !report["notes"].to_string().contains("outside the closure"),
        "the compiler opened a file that the analysis did not see: {}",
        report["notes"]
    );

    let mut second = compile_workspace(&target);
    second.without_xref_output();
    second.write("src/pkg/Greeter.cls", BASE);
    second.write("src/greet.p", USER);
    let output = second.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["summary"]["compiled"], 0, "{report}");
    assert_eq!(report["summary"]["fetched"], 2, "{report}");
    let printed = second.run_program(&target, LOAD);
    assert!(
        printed.contains("hello from a class"),
        "the fetched r-code did not run: {printed}"
    );
}

/// The fixture builds, its r-code runs, and a planted error gives a message
/// with a row and a column.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn compile_fixture() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.copy_fixture();

    let report = workspace.build_report();

    assert_eq!(
        report["summary"]["compiled"],
        FIXTURE_UNITS.len(),
        "{report}"
    );
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    for (path, rcode) in FIXTURE_UNITS {
        assert_eq!(unit(&report, path)["status"], "ok", "{path}");
        assert!(
            workspace.bytes(&format!("build/r/{rcode}")).len() > 100,
            "{rcode} is empty"
        );
    }
    let printed = workspace.run_program(&target, &run_from_rcode("main.p"));
    assert!(
        printed.contains("hello 10") && printed.contains("helper hello") && printed.contains("RAN"),
        "main.p did not run from r-code: {printed}"
    );
    let printed = workspace.run_program(&target, &run_from_rcode("dbread.p"));
    assert!(printed.contains("tables "), "dbread.p: {printed}");

    workspace.write(
        "src/broken.p",
        "DEFINE VARIABLE total AS INTEGER NO-UNDO.\ntotal = nosuchvariable + 1.\n",
    );
    let output = workspace.build(&["--json"], 1);
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    let broken = unit(&report, "src/broken.p");
    eprintln!("planted error: {broken}");
    assert_eq!(broken["status"], "fail");
    let errors: Vec<&Value> = broken["messages"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|message| message["type"] == "error")
        .collect();
    assert!(!errors.is_empty(), "no error message: {broken}");
    assert_eq!(errors[0]["file"], "src/broken.p", "{broken}");
    assert_eq!(errors[0]["row"], 2, "{broken}");
    assert!(errors[0]["column"].as_u64().unwrap() >= 1, "{broken}");
    assert!(!errors[0]["text"].as_str().unwrap().is_empty());
    assert!(!workspace.exists("build/r/broken.r"));
}

fn finished_before_started(events: &[Value], first: &str, then: &str) -> bool {
    let at = |kind: &str, path: &str| {
        events
            .iter()
            .position(|event| event["event"] == kind && event["path"] == path)
            .unwrap_or_else(|| panic!("no {kind} for {path}"))
    };
    at("unit-finished", first) < at("unit-started", then)
}

/// The hierarchy and the cycle compile; after an edit of the base class,
/// each class compiles after the class it inherits and before its users.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_order() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.copy_fixture();
    let report = workspace.build_report();
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    let printed = workspace.run_program(&target, &run_from_rcode("usecycle.p"));
    assert!(printed.contains("ping pong"), "the cycle: {printed}");
    let printed = workspace.run_program(&target, &run_from_rcode("useleaf.p"));
    assert!(
        printed.contains("leaf of mid of base"),
        "the hierarchy: {printed}"
    );

    let base = workspace.read("src/pkg/Base.cls");
    workspace.write("src/pkg/Base.cls", &base.replace("\"base\"", "\"basis\""));
    let output = workspace.build(&["--events"], 0);

    let events: Vec<Value> = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    for (first, then) in [
        ("src/pkg/Base.cls", "src/pkg/Mid.cls"),
        ("src/pkg/Mid.cls", "src/pkg/Leaf.cls"),
        ("src/pkg/Leaf.cls", "src/useleaf.p"),
    ] {
        assert!(
            finished_before_started(&events, first, then),
            "{first} must finish before {then} starts"
        );
    }
    let printed = workspace.run_program(&target, &run_from_rcode("useleaf.p"));
    assert!(
        printed.contains("leaf of mid of basis"),
        "the user did not follow the base class: {printed}"
    );
}

/// A build of 3,000 small units with one worker, started and read until its
/// third unit begins.
fn long_build(target: &Target) -> (Compile, Child, Lines<BufReader<ChildStdout>>) {
    let workspace = compile_workspace_with(
        target,
        Options {
            workers: 1,
            ..Options::default()
        },
    );
    for number in 0..3000 {
        workspace.write(
            &format!("src/gen/g{number}.p"),
            &format!("DEFINE VARIABLE sum AS INTEGER NO-UNDO.\nsum = {number}.\nPUT UNFORMATTED sum SKIP.\n"),
        );
    }
    let mut child = workspace
        .oxabl(&["--events"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("oxabl starts");
    let mut lines = BufReader::new(child.stdout.take().unwrap()).lines();
    let mut started = 0;
    while started < 3 {
        let line = lines.next().expect("the build ended too early").unwrap();
        if line.contains("\"unit-started\"") {
            started += 1;
        }
    }
    let running = live_workers(target, &workspace.host, &workspace.worker);
    assert!(
        !running.is_empty(),
        "the process list does not see the worker, so it cannot show that it ends"
    );
    assert!(
        child.try_wait().unwrap().is_none(),
        "the build ended before the test could stop it"
    );
    (workspace, child, lines)
}

/// Waits until no worker of the build runs in `workspace`.
fn assert_workers_end(target: &Target, workspace: &Compile, within: Duration) {
    let deadline = Instant::now() + within;
    loop {
        let alive = live_workers(target, &workspace.host, &workspace.worker);
        if alive.is_empty() {
            return;
        }
        assert!(
            Instant::now() < deadline,
            "workers of the stopped build are alive: {alive:?}"
        );
        std::thread::sleep(Duration::from_secs(2));
    }
}

/// After `SIGKILL` of oxabl, no worker of that build stays alive.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn orphans() {
    let target = setup();
    let (workspace, mut child, _lines) = long_build(&target);

    child.kill().unwrap();
    child.wait().unwrap();

    assert_workers_end(&target, &workspace, Duration::from_secs(120));
}

/// `SIGINT` stops the build with exit code 130, and no worker stays alive.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn cancel_leaves_no_worker() {
    let target = setup();
    let (workspace, mut child, lines) = long_build(&target);

    let signalled = Command::new("kill")
        .args(["-INT", &child.id().to_string()])
        .status()
        .unwrap();
    assert!(signalled.success());
    let summary = lines
        .map(|line| line.unwrap())
        .filter(|line| line.contains("\"summary\""))
        .last()
        .expect("the stream ends with a summary");

    let status = child.wait().unwrap();
    assert_eq!(status.code(), Some(130), "{summary}");
    assert_workers_end(&target, &workspace, Duration::from_secs(60));
}
