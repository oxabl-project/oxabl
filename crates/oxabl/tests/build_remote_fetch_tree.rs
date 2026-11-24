//! A whole tree fetched from the remote cache on a fresh machine: every unit
//! is installed, the outputs are the files the writer made, and the store
//! names them.
//!
//! `OXABL_FETCH_TREE_UNITS` sets the number of units (300 at first); with
//! 10000 the test measures a whole-tree fetch and prints its wall time.

#![cfg(feature = "remote-cache")]

mod common;

use std::collections::BTreeMap;
use std::time::Instant;

use common::built::Built;
use common::cache::{entries, tree};
use common::remote::world::World;
use serde_json::Value;
use tempfile::TempDir;

const OUTPUTS: &str = "[build.outputs]\ndebug_list = \"build/dbg\"\n";

fn units() -> usize {
    std::env::var("OXABL_FETCH_TREE_UNITS")
        .ok()
        .map(|count| count.parse().expect("OXABL_FETCH_TREE_UNITS is a number"))
        .unwrap_or(300)
}

/// `count` procedures in directories of 100. The debug listing copies the
/// source, so each set holds a few KB to tens of KB, like real r-code.
fn tree_of(count: usize) -> Built {
    let workspace = Built::with("include_paths = [\"src\"]", OUTPUTS);
    workspace.workers("count = 8");
    for index in 0..count {
        let lines = 40 + (index * 7919) % 900;
        let mut text = String::with_capacity(lines * 40);
        for line in 0..lines {
            text.push_str(&format!("MESSAGE \"unit {index} line {line}\".\n"));
        }
        workspace.write(&format!("src/m{}/u{index}.p", index / 100), &text);
    }
    workspace
}

fn files(workspace: &Built) -> BTreeMap<String, Vec<u8>> {
    tree(&workspace.path("build"))
        .into_iter()
        .filter(|(path, _)| !path.rsplit('/').next().unwrap_or("").starts_with('.'))
        .collect()
}

#[test]
fn a_fresh_machine_fetches_the_whole_tree() {
    let count = units();
    let world = World::start();
    let ci = tree_of(count);
    world.attach(&ci, &world.ci, "ci");
    let run = ci.build();
    run.succeeded();
    assert!(
        run.stdout
            .contains(&format!("remote: {count} sets offered, {count} stored")),
        "{}",
        run.stdout
    );
    let local = TempDir::new().unwrap();
    let fresh = tree_of(count);
    world.attach(&fresh, &world.developer, "developer");
    fresh.cache_in(local.path());

    let started = Instant::now();
    let run = fresh.build_with(&["--report", "report.json"]);
    let took = started.elapsed();

    run.succeeded();
    eprintln!("fetched {count} units in {took:?}");
    let report: Value = serde_json::from_str(&fresh.read("report.json")).unwrap();
    let units = report["units"].as_array().unwrap();
    assert_eq!(units.len(), count);
    assert!(
        units.iter().all(|unit| unit["source"] == "remote-cache"),
        "every unit comes from the remote cache: {:?}",
        units.iter().find(|unit| unit["source"] != "remote-cache")
    );
    assert!(fresh.compiled().is_empty(), "{:?}", fresh.compiled());
    assert_eq!(
        files(&fresh),
        files(&ci),
        "the fetched tree is the written one"
    );
    assert_eq!(
        entries(local.path()).len(),
        count,
        "every remote hit fills the local cache"
    );

    fresh.clear_log();
    let again = fresh.build_with(&["--plan", "--json"]);
    again.succeeded();
    let plan: Value = serde_json::from_str(&again.stdout).unwrap();
    assert_eq!(
        plan["summary"]["rebuild"], 0,
        "the store names every fetched output: {}",
        again.stdout
    );
    assert_eq!(plan["summary"]["up_to_date"], count);
}
