//! `oxabl build` against a real AVM. Opt-in: every test is `#[ignore]`. Run on
//! purpose without `OXABL_OPENEDGE_COMMAND` or `OXABL_OPENEDGE_WORKSPACE`, a
//! test fails; it never passes silently.
//!
//! | Variable | Meaning |
//! | --- | --- |
//! | `OXABL_OPENEDGE_COMMAND` | The worker command as a JSON array, with the placeholders `{program}` and `{root}`, for example `["docker","exec","-i","-w","{root}","abl","_progres","-b","-q","-pf","/work/db.pf","-p","{program}"]`. It must connect the databases and start in `{root}`. |
//! | `OXABL_OPENEDGE_WORKSPACE` | A host directory that the worker sees; each test makes and removes a directory in it. |
//! | `OXABL_OPENEDGE_WORKER_ROOT` | Where the worker sees that directory. |
//! | `OXABL_OPENEDGE_PROVERSION` | The `PROVERSION` of the AVM, as it prints it, for example `12.8`. |
//! | `OXABL_OPENEDGE_DATABASES` | The logical names the command connects, comma-separated, in connection order. At least one. |
//! | `OXABL_OPENEDGE_WORKERS` | Worker counts for `probe_time_by_worker_count`, comma-separated. Default `1,4,8`. |
//!
//! Run: `OXABL_OPENEDGE_COMMAND='[...]' OXABL_OPENEDGE_WORKSPACE=/path ... cargo
//! test -p oxabl --test build_openedge -- --ignored --nocapture --test-threads 1`.

mod common;

#[path = "build_openedge/compile.rs"]
mod compile;
#[path = "build_openedge/environment.rs"]
mod environment;
#[path = "build_openedge/probe.rs"]
mod probe;
#[path = "build_openedge/propath.rs"]
mod propath;
#[path = "build_openedge/rcode.rs"]
mod rcode;
#[path = "build_openedge/xref.rs"]
mod xref;

use std::path::PathBuf;

use common::openedge::Target;

fn var(name: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| panic!("{name} must be set to run this test"))
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

/// The AVM that the tests run against.
fn setup() -> Target {
    let command: Vec<String> = serde_json::from_str(&var("OXABL_OPENEDGE_COMMAND"))
        .expect("OXABL_OPENEDGE_COMMAND is a JSON array of strings");
    let target = Target {
        command,
        workspace: PathBuf::from(var("OXABL_OPENEDGE_WORKSPACE")),
        worker_root: var("OXABL_OPENEDGE_WORKER_ROOT"),
        proversion: var("OXABL_OPENEDGE_PROVERSION"),
        databases: list("OXABL_OPENEDGE_DATABASES"),
    };
    assert!(
        !target.databases.is_empty(),
        "OXABL_OPENEDGE_DATABASES must name at least one database"
    );
    target
}
