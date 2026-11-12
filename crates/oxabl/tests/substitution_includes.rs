//! Includes that do not resolve, reached through nested `{&var}` substitutions.
//!
//! Each define below holds an include and two forward references, so every use
//! rescans its value and the tree doubles at each level. Within the expansion
//! limit that is 124,736 unresolved includes in a file of 600 bytes. Each one
//! was checked against every substitution with a linear search: `oxabl check`
//! took 34 s in a release build. The test runs it under a time limit that the
//! old behaviour cannot meet.

#![cfg(unix)]

use std::process::Command;

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// Seconds for one run of a debug build.
const SECONDS: u32 = 60;

#[test]
fn many_unresolved_includes_through_nested_substitutions() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("defines.p");
    let mut text = String::new();
    for level in (1..=20).rev() {
        let below = level - 1;
        text.push_str(&format!(
            "&SCOPED-DEFINE a{level} {{q}}{{&a{below}}}{{&a{below}}}\n"
        ));
    }
    text.push_str("{&a20}\n");
    std::fs::write(&path, text).unwrap();

    let output = Command::new("sh")
        .arg("-c")
        .arg(format!(r#"ulimit -c 0 && exec timeout {SECONDS} "$@""#))
        .arg("sh")
        .arg(OXABL_BIN)
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.code().is_some_and(|code| code <= 1),
        "expected exit 0 or 1 within {SECONDS} s (124 is the time limit), got {:?}\n{}",
        output.status,
        &stderr[stderr.floor_char_boundary(stderr.len().saturating_sub(1000))..]
    );
    assert!(
        stderr.contains("PREPROC007"),
        "expected the unresolved-include warning:\n{}",
        &stderr[..stderr.floor_char_boundary(stderr.len().min(1000))]
    );
}
