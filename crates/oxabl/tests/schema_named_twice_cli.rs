//! `oxabl check` over a schema that `[workspace.schema] files` names a file of
//! twice: as itself and through its directory. The file loads once, at its
//! last place, which keeps every field that reading it at each place accepted.
//!
//! `OXABL_CHECK_BIN` names another `oxabl` to run the same cases with: an
//! older build passes them too.
//!
//! All fixtures are synthetic.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

/// Declares `Customer` with `Name`.
const BASE: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
                    ADD FIELD \"Name\" OF \"Customer\" AS character\n";
/// Adds `Extra` to `Customer`, which it does not declare.
const EXTRA: &str = "ADD FIELD \"Extra\" OF \"Customer\" AS character\n";
const MAIN: &str = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                    MESSAGE Customer.Name Customer.Extra.\n";

fn write(path: &Path, text: &str) {
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, text).unwrap();
}

/// A project whose `oxabl.toml` names `files`, with `db/` holding each of
/// `schema`, and a `main.p` that reads `Customer.Name` and `Customer.Extra`.
fn project(files: &str, schema: &[(&str, &str)]) -> TempDir {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &format!("[workspace]\nname = \"twice\"\n[workspace.schema]\nfiles = {files}\n"),
    );
    for (name, text) in schema {
        write(&dir.path().join("db").join(name), text);
    }
    write(&dir.path().join("main.p"), MAIN);
    dir
}

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

fn check(dir: &Path) -> Run {
    let bin = std::env::var("OXABL_CHECK_BIN").unwrap_or(env!("CARGO_BIN_EXE_oxabl").to_string());
    let out = Command::new(bin)
        .current_dir(dir)
        .args(["check", "--no-format", "main.p"])
        .output()
        .unwrap();
    Run {
        code: out.status.code(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// The result `oxabl check` gave before a file named twice was loaded once:
/// no finding and exit 0. Only the warnings on stderr differ (an older build
/// warns that the first read of the file refused `Extra`).
#[track_caller]
fn assert_extra_is_known(run: &Run) {
    let all = format!("{}{}", run.stdout, run.stderr);
    assert!(
        !all.contains("LINT0003"),
        "a field reported unknown:\n{all}"
    );
    assert_eq!(
        run.stdout, "checked 1 file: no findings, format suppressed\n",
        "stderr:\n{}",
        run.stderr
    );
    assert_eq!(run.code, Some(0), "stderr:\n{}", run.stderr);
}

#[test]
fn a_file_named_before_its_directory_keeps_its_fields() {
    // Loaded at its last place: `a.df`, then `b.df`.
    let dir = project(r#"["db/b.df", "db"]"#, &[("a.df", BASE), ("b.df", EXTRA)]);
    assert_extra_is_known(&check(dir.path()));
}

#[test]
fn a_file_named_after_its_directory_keeps_its_fields() {
    // `db` loads `a_ext.df` before `z_base.df`; its last place is after.
    let dir = project(
        r#"["db", "db/a_ext.df"]"#,
        &[("a_ext.df", EXTRA), ("z_base.df", BASE)],
    );
    assert_extra_is_known(&check(dir.path()));
}
