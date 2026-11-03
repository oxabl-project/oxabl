//! `oxabl build --probe` against a real AVM. Opt-in: every test is `#[ignore]`.
//! Run on purpose without `OXABL_OPENEDGE_COMMAND` or `OXABL_OPENEDGE_WORKSPACE`,
//! a test fails; it never passes silently.
//!
//! | Variable | Meaning |
//! | --- | --- |
//! | `OXABL_OPENEDGE_COMMAND` | The worker command as a JSON array, with the placeholders `{program}` and `{root}`, for example `["docker","exec","-i","-w","{root}","abl","_progres","-b","-q","-pf","/work/db.pf","-p","{program}"]`. It must connect the databases and start in `{root}`. |
//! | `OXABL_OPENEDGE_WORKSPACE` | A host directory that the worker sees; the test makes and removes a directory in it. |
//! | `OXABL_OPENEDGE_WORKER_ROOT` | Where the worker sees that directory. Default: the same path. |
//! | `OXABL_OPENEDGE_PROVERSION` | The `PROVERSION` of the AVM. Default `12.8`. |
//! | `OXABL_OPENEDGE_DATABASES` | The logical names the command connects, comma-separated, in connection order. Default: none. |
//! | `OXABL_OPENEDGE_WORKERS` | Worker counts for `probe_time_by_worker_count`, comma-separated. Default `1,4,8`. |
//! | `OXABL_OPENEDGE_SAME_DF` | Logical names of databases that were loaded from one `.df`, comma-separated. Their schema identities must be equal. |
//!
//! Run: `OXABL_OPENEDGE_COMMAND='[...]' OXABL_OPENEDGE_WORKSPACE=/path
//! cargo test -p oxabl --test build_openedge -- --ignored --nocapture`.

use std::path::PathBuf;

mod common;

use common::openedge::{Target, compile_workspace, is_hex64, probe};

struct Setup {
    target: Target,
    same_df: Vec<String>,
}

fn list(name: &str) -> Vec<String> {
    std::env::var(name)
        .unwrap_or_default()
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .map(String::from)
        .collect()
}

fn setup() -> Setup {
    let command = std::env::var("OXABL_OPENEDGE_COMMAND")
        .expect("OXABL_OPENEDGE_COMMAND names the worker command");
    let command: Vec<String> =
        serde_json::from_str(&command).expect("OXABL_OPENEDGE_COMMAND is a JSON array of strings");
    let workspace = PathBuf::from(
        std::env::var("OXABL_OPENEDGE_WORKSPACE")
            .expect("OXABL_OPENEDGE_WORKSPACE names a host directory"),
    );
    let worker_root = std::env::var("OXABL_OPENEDGE_WORKER_ROOT")
        .unwrap_or_else(|_| workspace.to_string_lossy().into_owned());
    Setup {
        target: Target {
            command,
            workspace,
            worker_root,
            proversion: std::env::var("OXABL_OPENEDGE_PROVERSION")
                .unwrap_or_else(|_| "12.8".into()),
            databases: list("OXABL_OPENEDGE_DATABASES"),
        },
        same_df: list("OXABL_OPENEDGE_SAME_DF"),
    }
}

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn probe_a_real_avm() {
    let setup = setup();

    let (report, _guard) = probe(&setup.target, 1);
    let (again, _again_guard) = probe(&setup.target, 1);

    let environment = &report["environment"];
    assert!(is_hex64(&environment["identity"]));
    assert_eq!(
        environment["identity"], again["environment"]["identity"],
        "the identity is stable"
    );
    let fields = &environment["fields"];
    assert!(
        fields["proversion"]
            .as_str()
            .unwrap()
            .starts_with(&setup.target.proversion)
    );
    for name in [
        "dlc",
        "dlc_version",
        "opsys",
        "window_system",
        "cpinternal",
        "cpstream",
        "date_format",
        "year_offset",
        "cwd",
    ] {
        assert!(
            !fields[name].as_str().unwrap_or_default().is_empty(),
            "field {name} is empty: {fields}"
        );
    }
    assert_eq!(fields["batch_mode"], "yes");
    let reported: Vec<&str> = environment["databases"]
        .as_array()
        .unwrap()
        .iter()
        .map(|database| database["name"].as_str().unwrap())
        .collect();
    assert_eq!(
        reported,
        setup
            .target
            .databases
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>()
    );
    for database in environment["databases"].as_array().unwrap() {
        assert!(is_hex64(&database["schema_identity"]), "{database}");
        assert!(database["records"].as_u64().unwrap() >= 1, "{database}");
    }
    assert_eq!(environment["unknown"].as_array().unwrap().len(), 0);
}

/// The schema image is split across the workers, so the identity must be the
/// same for every worker count. Prints the wall time of each probe.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn probe_time_by_worker_count() {
    let setup = setup();
    let counts: Vec<usize> = match list("OXABL_OPENEDGE_WORKERS") {
        given if !given.is_empty() => given.iter().map(|n| n.parse().unwrap()).collect(),
        _ => vec![1, 4, 8],
    };

    let mut identities = Vec::new();
    for workers in counts {
        let started = std::time::Instant::now();
        let (report, _guard) = probe(&setup.target, workers);
        eprintln!(
            "TIMING workers={workers} wall={:.1}s",
            started.elapsed().as_secs_f64()
        );
        identities.push((workers, report["environment"]["identity"].clone()));
    }

    assert!(
        identities.windows(2).all(|pair| pair[0].1 == pair[1].1),
        "the identity depends on the worker count: {identities:?}"
    );
}

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn two_workers_agree_on_one_environment() {
    let setup = setup();

    let (single, _single_guard) = probe(&setup.target, 1);
    let (pair, _pair_guard) = probe(&setup.target, 2);

    assert_eq!(pair["workers"]["started"], 2);
    assert_eq!(
        single["environment"]["identity"],
        pair["environment"]["identity"]
    );
}

#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn databases_loaded_from_one_df_give_one_schema_identity() {
    let setup = setup();
    if setup.same_df.len() < 2 {
        eprintln!("skipped: OXABL_OPENEDGE_SAME_DF names fewer than two databases");
        return;
    }

    let (report, _guard) = probe(&setup.target, 1);

    let identities: Vec<(&str, &str)> = report["environment"]["databases"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|database| {
            setup
                .same_df
                .iter()
                .any(|name| name == database["name"].as_str().unwrap())
        })
        .map(|database| {
            (
                database["name"].as_str().unwrap(),
                database["schema_identity"].as_str().unwrap(),
            )
        })
        .collect();
    assert_eq!(
        identities.len(),
        setup.same_df.len(),
        "every named database is connected"
    );
    assert!(
        identities.windows(2).all(|pair| pair[0].1 == pair[1].1),
        "schema identities differ: {identities:?}"
    );
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
    let setup = setup();
    let workspace = compile_workspace(&setup.target);
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

    let printed = workspace.run_program(&setup.target, LOAD);
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
/// no XREF output requested, the build writes a private `XREF-XML` and reads
/// it back: this is the first time that the read check meets the file names
/// of a real compiler. The second workspace must compile nothing, and its
/// r-code must load.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn two_workspaces_share_compiled_units_through_the_local_cache() {
    let setup = setup();
    let cache = tempfile::TempDir::new().unwrap();
    let first = compile_workspace(&setup.target);
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

    let second = compile_workspace(&setup.target);
    second.without_xref_output();
    second.write("src/pkg/Greeter.cls", BASE);
    second.write("src/greet.p", USER);
    let output = second.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["summary"]["compiled"], 0, "{report}");
    assert_eq!(report["summary"]["fetched"], 2, "{report}");
    let printed = second.run_program(&setup.target, LOAD);
    assert!(
        printed.contains("hello from a class"),
        "the fetched r-code did not run: {printed}"
    );
}
