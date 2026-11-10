//! The worker pool through the public `oxabl::build` API, against the fake
//! AVM: the `$DLC` hash phase that the planner runs between the closure step
//! and the keys.

mod common;

use common::Workspace;
use oxabl::build::config::BuildWorkspace;
use oxabl::build::key::FileDigest;
use oxabl::build::worker::random::build_id;
use oxabl::build::worker::{Cancel, Pool, PoolError, PoolSpec};
use serde_json::json;

const SHARED: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";

fn pool(workspace: &Workspace) -> Pool {
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    let spec = PoolSpec::from_config(&config, None, build_id()).expect("a worker command");
    Pool::start(&spec, &Cancel::new())
        .expect("the workers start")
        .pool
}

fn workspace(second_copy_of_b: &str) -> Workspace {
    let workspace = Workspace::new(
        "count = 2\nenv = { FAKE_AVM_REQUEST_LOG = \"requests.log\" }\n",
        &["--env", "{root}/env-{worker}.json"],
    );
    for worker in [1, 2] {
        workspace.write(&format!("dlc-{worker}/src/a.i"), SHARED);
        let b = if worker == 1 {
            SHARED
        } else {
            second_copy_of_b
        };
        workspace.write(&format!("dlc-{worker}/src/b.i"), b);
        workspace.env_file(
            &format!("env-{worker}.json"),
            &json!({ "dlc_dir": workspace.path(&format!("dlc-{worker}")), "databases": [] }),
        );
    }
    workspace
}

fn paths() -> Vec<String> {
    ["$DLC/src/b.i", "$DLC/src/a.i", "$DLC/src/missing.i", "$DLC"]
        .map(String::from)
        .to_vec()
}

#[test]
fn workers_that_see_the_same_dlc_files_agree_and_the_digests_come_back_sorted() {
    let workspace = workspace(SHARED);
    let mut pool = pool(&workspace);

    let digests = pool.hash_dlc_files(&paths()).expect("the workers agree");

    let names: Vec<&str> = digests.iter().map(|(path, _)| path.as_str()).collect();
    assert_eq!(
        names,
        ["$DLC", "$DLC/src/a.i", "$DLC/src/b.i", "$DLC/src/missing.i"]
    );
    assert_eq!(digests[0].1, FileDigest::Dir);
    assert!(matches!(&digests[1].1, FileDigest::File { size, .. } if *size == SHARED.len() as u64));
    assert_eq!(digests[1].1, digests[2].1);
    assert_eq!(digests[3].1, FileDigest::Absent);
    pool.shutdown();
}

#[test]
fn the_workers_share_the_hashing_and_each_checks_the_whole_list() {
    let workspace = workspace(SHARED);
    let mut pool = pool(&workspace);
    let _ = std::fs::remove_file(workspace.path("requests.log"));

    pool.hash_dlc_files(&paths()).expect("the workers agree");
    pool.shutdown();

    let log = std::fs::read_to_string(workspace.path("requests.log")).unwrap();
    let mut by_worker: std::collections::BTreeMap<&str, (usize, usize)> = Default::default();
    for line in log.lines() {
        let mut words = line.split(' ').skip(1);
        let (pid, what) = (words.next().unwrap(), words.next().unwrap());
        let counts = by_worker.entry(pid).or_default();
        match what {
            "HASH" => counts.0 += 1,
            "HASHSET" => counts.1 += 1,
            _ => {}
        }
    }
    let counts: Vec<(usize, usize)> = by_worker.into_values().collect();
    assert_eq!(counts, [(2, 1), (2, 1)], "{log}");
}

#[test]
fn workers_that_see_different_dlc_files_disagree() {
    let workspace = workspace("DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");
    let mut pool = pool(&workspace);

    let error = pool
        .hash_dlc_files(&paths())
        .expect_err("the copies differ");

    match error {
        PoolError::Config(message) => {
            assert!(
                message.contains("workers see different $DLC files"),
                "{message}"
            );
            assert!(message.contains("worker 2"), "{message}");
        }
        other => panic!("expected a configuration error, got {other:?}"),
    }
    pool.shutdown();
}

#[test]
fn a_different_copy_in_the_share_of_the_second_worker_is_found_too() {
    let workspace = workspace(SHARED);
    workspace.write("dlc-2/src/a.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");
    let mut pool = pool(&workspace);

    let error = pool
        .hash_dlc_files(&paths())
        .expect_err("the copies differ");

    match error {
        PoolError::Config(message) => {
            assert!(
                message.contains("workers see different $DLC files"),
                "{message}"
            );
            assert!(
                message.contains("worker 1") && message.contains("worker 2"),
                "{message}"
            );
        }
        other => panic!("expected a configuration error, got {other:?}"),
    }
    pool.shutdown();
}
