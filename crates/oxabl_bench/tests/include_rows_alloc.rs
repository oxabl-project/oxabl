//! A finding in an include costs a bounded number of allocations.
//!
//! The fold of the findings in includes (`IncludeRows` in `oxabl_analyze`) once
//! cloned a path and a message to build the key of each finding. This test
//! lints a root that includes a file of `FEW` and of `MANY` unused variables,
//! and wants no more than `MAX_PER_FINDING` allocations for each added
//! finding. That is the lint finding, its row and its include chain; the lookup
//! of the row allocates nothing. The count was 10.6 with the cloned key and is
//! 8.6 without it. All input is synthetic.

use std::path::PathBuf;

use oxabl_bench::alloc::{Counting, measure};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

const FEW: usize = 50;
const MANY: usize = 250;
const MAX_PER_FINDING: f64 = 9.0;

fn include_of(count: usize) -> String {
    (0..count)
        .map(|i| format!("DEFINE VARIABLE unused-{i} AS INTEGER NO-UNDO.\n"))
        .collect()
}

/// The allocations of one lint run over a root with `count` findings in its
/// include, and the number of rows it reported.
fn run(count: usize) -> (u64, usize) {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/w/d.i"), include_of(count));
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w")],
        ..PipelineConfig::default()
    };
    let pipeline = LintPipeline::new(&config, &fs);
    // One run first, so the lazy statics and the rule registry are not counted.
    let _ = pipeline.run("{d.i}\nMESSAGE 1.\n");
    let (result, usage) = measure(|| pipeline.run("{d.i}\nMESSAGE 1.\n"));
    (usage.allocations, result.include_origin().len())
}

#[test]
fn a_finding_in_an_include_allocates_a_bounded_amount() {
    let (few, few_rows) = run(FEW);
    let (many, many_rows) = run(MANY);
    assert_eq!((few_rows, many_rows), (FEW, MANY), "the findings changed");
    let per_finding = (many - few) as f64 / (MANY - FEW) as f64;
    eprintln!("{few} allocations for {FEW} findings, {many} for {MANY}: {per_finding} each");
    assert!(
        per_finding <= MAX_PER_FINDING,
        "{per_finding} allocations for each finding in an include, over {MAX_PER_FINDING}"
    );
}
