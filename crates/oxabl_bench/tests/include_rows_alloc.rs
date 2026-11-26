//! A finding in an include costs a bounded number of allocations in the lint
//! run, both when it makes a row and when it joins the row of an earlier
//! expansion of the same include.
//!
//! The fold of the findings in includes (`IncludeRows` in `oxabl_analyze`)
//! looks a finding up by a key of plain data. A lookup that clones a path or a
//! message adds allocations to each finding, and this test fails on that. It
//! counts allocations only: a slower hash or a longer key that allocates
//! nothing is for the `check/include_findings_repeated` bench. All input is
//! synthetic.
//!
//! Each run lints a root that includes `d.i` in `procedures` procedures. Each
//! expansion states the same unused variables, so the first expansion makes a
//! row for each and every later one adds an expansion to that row.

use std::path::PathBuf;

use oxabl_bench::alloc::{Counting, measure};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

const FEW: usize = 50;
const MANY: usize = 250;
const SHORT: usize = 2;
const LONG: usize = 6;

/// The lint finding, its row and its include chain.
const MAX_PER_NEW_ROW: f64 = 9.0;
/// The lint finding, its expansion entry and its include chain. A hit that
/// clones the path and the message once each makes 2 more.
const MAX_PER_ADDED_EXPANSION: f64 = 7.0;

fn include_of(count: usize) -> String {
    (0..count)
        .map(|i| format!("DEFINE VARIABLE unused-{i} AS INTEGER NO-UNDO.\n"))
        .collect()
}

fn root_of(procedures: usize) -> String {
    (0..procedures)
        .map(|p| format!("PROCEDURE p{p}:\n{{d.i}}\nEND PROCEDURE.\n"))
        .collect()
}

/// The allocations of one lint run over a root that includes a file of
/// `findings` unused variables in `procedures` procedures, and the number of
/// rows and expansions it reported.
fn run(findings: usize, procedures: usize) -> (u64, usize, usize) {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/w/d.i"), include_of(findings));
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w")],
        ..PipelineConfig::default()
    };
    let pipeline = LintPipeline::new(&config, &fs);
    let root = root_of(procedures);
    // One run first, so the lazy statics and the rule registry are not counted.
    let _ = pipeline.run(&root);
    let (result, usage) = measure(|| pipeline.run(&root));
    let rows = result.include_origin();
    let expansions = rows.iter().map(|row| row.expansions.len()).sum();
    (usage.allocations, rows.len(), expansions)
}

/// The counters are global, so the two measurements run in one test, one after
/// the other.
#[test]
fn a_finding_in_an_include_allocates_a_bounded_amount() {
    a_finding_that_makes_a_row_allocates_a_bounded_amount();
    a_finding_that_joins_a_row_allocates_a_bounded_amount();
}

fn a_finding_that_makes_a_row_allocates_a_bounded_amount() {
    let (few, few_rows, _) = run(FEW, 1);
    let (many, many_rows, _) = run(MANY, 1);
    assert_eq!((few_rows, many_rows), (FEW, MANY), "the findings changed");
    let per_row = (many - few) as f64 / (MANY - FEW) as f64;
    eprintln!("{few} allocations for {FEW} rows, {many} for {MANY}: {per_row} each");
    assert!(
        per_row <= MAX_PER_NEW_ROW,
        "{per_row} allocations for each finding that makes a row, over {MAX_PER_NEW_ROW}"
    );
}

fn a_finding_that_joins_a_row_allocates_a_bounded_amount() {
    let (short, short_rows, short_expansions) = run(FEW, SHORT);
    let (long, long_rows, long_expansions) = run(FEW, LONG);
    assert_eq!(
        (short_rows, long_rows),
        (FEW, FEW),
        "the expansions fold into one row for each finding"
    );
    assert_eq!(
        (short_expansions, long_expansions),
        (FEW * SHORT, FEW * LONG),
        "the expansions changed"
    );
    let per_expansion = (long - short) as f64 / (FEW * (LONG - SHORT)) as f64;
    eprintln!(
        "{short} allocations for {SHORT} expansions, {long} for {LONG}: {per_expansion} each"
    );
    assert!(
        per_expansion <= MAX_PER_ADDED_EXPANSION,
        "{per_expansion} allocations for each finding that joins a row, over \
         {MAX_PER_ADDED_EXPANSION}"
    );
}
