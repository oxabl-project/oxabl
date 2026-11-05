//! `oxabl build --plan` while a file changes under it: the graph that it leaves
//! in the store must still fit the files.

mod build_support;

use std::fs;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use build_support::Workspace;

const UNITS: usize = 80;
const ATTEMPTS: usize = 30;
/// Time between two rewrites, by attempt: from faster than a refresh round to
/// slower than one.
const PAUSES_MS: [u64; 5] = [6, 12, 25, 50, 100];

/// An include that names one of two other includes, rewritten without pause
/// while the first plan runs. Each rewrite is atomic: a reader sees one
/// version or the other.
fn attempt(attempt: usize) {
    let workspace = Workspace::with_sources("include_paths = [\"inc\"]");
    workspace.write("inc/one.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n");
    workspace.write("inc/two.i", "DEFINE VARIABLE two AS INTEGER NO-UNDO.\n");
    let version = |n: u32| format!("{{{}.i}}\n", if n == 1 { "one" } else { "two" });
    workspace.write("inc/shared.i", &version(1));
    for unit in 0..UNITS {
        workspace.write(
            &format!("src/u{unit}.p"),
            &format!("{{shared.i}}\nMESSAGE \"{unit}\".\n"),
        );
    }

    let pause = std::time::Duration::from_millis(PAUSES_MS[attempt % PAUSES_MS.len()]);
    let running = Arc::new(AtomicBool::new(true));
    let writer = {
        let running = Arc::clone(&running);
        let shared = workspace.path("inc/shared.i");
        let scratch = workspace.path("inc/shared.tmp");
        std::thread::spawn(move || {
            let mut n = 1;
            while running.load(Ordering::Relaxed) {
                n = 3 - n;
                fs::write(
                    &scratch,
                    format!("{{{}.i}}\n", if n == 1 { "one" } else { "two" }),
                )
                .unwrap();
                fs::rename(&scratch, &shared).unwrap();
                std::thread::sleep(pause);
            }
        })
    };
    let first = workspace.plan();
    running.store(false, Ordering::Relaxed);
    writer.join().unwrap();
    // A plan may give up on a tree that never settles; it must say so.
    if first.code != 0 {
        assert_eq!(first.code, 3, "{}", first.stderr);
        assert!(first.stderr.contains("kept changing"), "{}", first.stderr);
    }

    let final_version = 1 + (attempt % 2) as u32;
    workspace.write("inc/shared.i", &version(final_version));
    workspace.plan().succeeded();
    workspace.plan().succeeded();

    let (reached, not_reached) = if final_version == 1 {
        ("inc/one.i", "inc/two.i")
    } else {
        ("inc/two.i", "inc/one.i")
    };
    let named = workspace.run(&["--plan", "--files", reached]);
    named.succeeded();
    assert_eq!(
        named.rebuild().len(),
        UNITS,
        "attempt {attempt}: units that include shared.i must reach {reached}\n{}",
        named.summary()
    );
    let other = workspace.run(&["--plan", "--files", not_reached]);
    other.succeeded();
    assert_eq!(
        other.rebuild().len(),
        0,
        "attempt {attempt}: no unit may reach {not_reached}\n{}",
        other.summary()
    );
}

#[test]
fn a_file_rewritten_during_the_refresh_leaves_a_graph_that_fits_the_files() {
    for n in 0..ATTEMPTS {
        attempt(n);
    }
}
