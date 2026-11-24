//! A binary built without the `self-update` feature says how to update.
#![cfg(not(feature = "self-update"))]

use std::process::Command;

#[test]
fn self_update_without_the_feature_exits_2_and_names_cargo_install() {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("self-update")
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(2));
    assert!(String::from_utf8_lossy(&output.stderr).contains("cargo install"));
}
