//! `oxabl --version` and `-V` print the crate version, as clap does.

use std::process::Command;

#[test]
fn both_flags_print_the_crate_version_and_exit_0() {
    for flag in ["--version", "-V"] {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .arg(flag)
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(0), "{flag}");
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            concat!("oxabl ", env!("CARGO_PKG_VERSION"), "\n"),
            "{flag}"
        );
    }
}
