//! The cost of branch tracking in a chain of includes that each pass many named
//! arguments. All fixtures are synthetic ABL.

use std::path::PathBuf;
use std::time::{Duration, Instant};

use oxabl_common::FileId;
use oxabl_preprocessor::{BranchState, Preprocessor};
use oxabl_workspace::InMemoryFileSystem;

/// How many times as long as the run without tracking the tracked run may take.
/// Linear tracking takes two to four times as long; a walk over the arguments
/// of every include around (quadratic in the depth) or a scan of what an
/// include saved for each argument (quadratic in the arguments) takes more than
/// ten times as long.
const LIMIT: u32 = 8;

/// `f0.i` to `f{depth-1}.i`, each passing `arguments` distinct named arguments
/// to the next, and a root that references `f0.i` `roots` times.
fn chain(depth: usize, arguments: usize, roots: usize) -> (InMemoryFileSystem, String) {
    let mut fs = InMemoryFileSystem::new();
    for level in 0..depth {
        let text = if level + 1 == depth {
            "&IF \"{&a0_0}\" = \"1\" &THEN\nA\n&ENDIF\n".to_string()
        } else {
            let arguments: String = (0..arguments)
                .map(|n| format!(" &a{}_{n}=1", level + 1))
                .collect();
            format!("{{f{}.i{arguments}}}\n", level + 1)
        };
        fs.insert(PathBuf::from(format!("/inc/f{level}.i")), text);
    }
    (fs, "{f0.i}\n".repeat(roots))
}

/// The best time of tracked and untracked runs, taken alternately so that a
/// slow moment of the machine hits both.
fn times(
    fs: &InMemoryFileSystem,
    root: &str,
    includer_unknown: bool,
) -> (Duration, Duration, Vec<BranchState>) {
    let paths = vec![PathBuf::from("/inc")];
    let mut best = [Duration::MAX; 2];
    let mut states = Vec::new();
    for _ in 0..5 {
        for (slot, tracking) in [false, true].into_iter().enumerate() {
            let started = Instant::now();
            let result = Preprocessor::new(fs, &paths)
                .with_branch_tracking(tracking)
                .with_includer_unknown(includer_unknown)
                .process(FileId::new(1), root)
                .unwrap();
            best[slot] = best[slot].min(started.elapsed());
            if tracking {
                states = result.branches.iter().map(|b| b.state).collect();
            }
        }
    }
    (best[0], best[1], states)
}

fn check(includer_unknown: bool, expected: BranchState, shape: (usize, usize, usize)) {
    let (depth, arguments, roots) = shape;
    let (fs, root) = chain(depth, arguments, roots);
    let (without, with, states) = times(&fs, &root, includer_unknown);
    assert_eq!(states, vec![expected; roots]);
    assert!(
        with < without * LIMIT,
        "tracking {depth} includes of {arguments} arguments, {roots} times (includer unknown: {includer_unknown}), took {with:?}, more than {LIMIT} times the {without:?} without tracking"
    );
}

#[test]
fn a_chain_of_includes_with_many_named_arguments_costs_about_what_the_arguments_cost() {
    // `a0_0` is no argument of any include here, so the condition is decided.
    check(false, BranchState::NotTaken, (60, 400, 4));
}

#[test]
fn the_same_chain_costs_the_same_without_the_includer() {
    // The includer may define `a0_0`, so the condition is not decided. Every
    // argument is saved in each include, so a scan of what an include saved
    // for each argument shows in the cost of one include with many arguments.
    check(true, BranchState::Undecided, (10, 3000, 2));
}
