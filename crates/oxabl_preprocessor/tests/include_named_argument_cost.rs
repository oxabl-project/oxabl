//! The cost of branch tracking in a chain of includes that each pass many named
//! arguments. All fixtures are synthetic ABL.

use std::path::PathBuf;
use std::time::{Duration, Instant};

use oxabl_common::FileId;
use oxabl_preprocessor::{BranchState, Preprocessor};
use oxabl_workspace::InMemoryFileSystem;

const DEPTH: usize = 60;
const ARGUMENTS: usize = 400;
const ROOTS: usize = 4;

/// How many times as long as the run without tracking the tracked run may take.
/// Linear tracking takes about twice as long; a walk over the arguments of every
/// include around (quadratic in the depth) takes more than ten times as long.
const LIMIT: u32 = 6;

/// `f0.i` to `f{DEPTH-1}.i`, each passing `ARGUMENTS` distinct named arguments
/// to the next, and a root that references `f0.i` `ROOTS` times.
fn chain() -> (InMemoryFileSystem, String) {
    let mut fs = InMemoryFileSystem::new();
    for level in 0..DEPTH {
        let text = if level + 1 == DEPTH {
            "&IF \"{&a0_0}\" = \"1\" &THEN\nA\n&ENDIF\n".to_string()
        } else {
            let arguments: String = (0..ARGUMENTS)
                .map(|n| format!(" &a{}_{n}=1", level + 1))
                .collect();
            format!("{{f{}.i{arguments}}}\n", level + 1)
        };
        fs.insert(PathBuf::from(format!("/inc/f{level}.i")), text);
    }
    (fs, "{f0.i}\n".repeat(ROOTS))
}

fn time(fs: &InMemoryFileSystem, root: &str, tracking: bool) -> (Duration, Vec<BranchState>) {
    let paths = vec![PathBuf::from("/inc")];
    let mut best = Duration::MAX;
    let mut states = Vec::new();
    for _ in 0..3 {
        let started = Instant::now();
        let result = Preprocessor::new(fs, &paths)
            .with_branch_tracking(tracking)
            .process(FileId::new(1), root)
            .unwrap();
        best = best.min(started.elapsed());
        states = result.branches.iter().map(|b| b.state).collect();
    }
    (best, states)
}

#[test]
fn a_chain_of_includes_with_many_named_arguments_costs_about_what_the_arguments_cost() {
    let (fs, root) = chain();
    let (without, _) = time(&fs, &root, false);
    let (with, states) = time(&fs, &root, true);
    // `a0_0` is no argument of any include here, so the condition is decided.
    assert_eq!(states, vec![BranchState::NotTaken; ROOTS]);
    assert!(
        with < without * LIMIT,
        "tracking {DEPTH} includes of {ARGUMENTS} arguments, {ROOTS} times, took {with:?}, more than {LIMIT} times the {without:?} without tracking"
    );
}
