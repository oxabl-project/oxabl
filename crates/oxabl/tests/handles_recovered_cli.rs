//! `oxabl check` and the `strict.handles` rules on a file the parser recovered
//! from: recovery can move a free out of its unit, so the file is not judged.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n\
     [workspace.lint]\nenable = [\"strict.handles\"]\n";

/// The free sits after a `DO` the parser cannot read, so recovery closes the
/// procedure first.
const MOVED_FREE: &str = "\
DEFINE VARIABLE lx AS LOGICAL NO-UNDO.
PROCEDURE b:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  main:
  DO ON ERROR UNDO main, LEAVE main
     ON STOP UNDO main, LEAVE main
     TRANSACTION
  :
    lx = TRUE.
  END.
  DELETE OBJECT hq.
END PROCEDURE.
";

fn codes(dir: &Path) -> Vec<(String, String)> {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--no-format", "--json", "."])
        .output()
        .unwrap();
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["path"].as_str().unwrap().replace('\\', "/"),
                d["code"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

#[test]
fn a_recovered_root_gets_no_handle_finding() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    fs::write(dir.path().join("rec.p"), MOVED_FREE).unwrap();
    let found = codes(dir.path());
    assert!(found.iter().any(|(_, c)| c == "PARSE001"), "{found:?}");
    assert!(!found.iter().any(|(_, c)| c.starts_with("HD")), "{found:?}");
}

#[test]
fn a_recovered_include_is_still_judged_in_the_include() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    fs::write(dir.path().join("rec.i"), MOVED_FREE).unwrap();
    fs::write(dir.path().join("a.p"), "{rec.i}\n").unwrap();
    fs::write(dir.path().join("b.p"), "{rec.i}\n").unwrap();
    let hd: Vec<_> = codes(dir.path())
        .into_iter()
        .filter(|(_, c)| c == "HD001")
        .collect();
    assert_eq!(hd.len(), 1, "{hd:?}");
    assert!(hd[0].0.ends_with("rec.i"), "{hd:?}");
}
