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

use common::openedge::{Target, is_hex64, probe};

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
