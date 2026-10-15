use std::path::Path;
use std::process::Command;

fn fixture() -> tempfile::TempDir {
    let tmp = tempfile::tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/explain");
    for name in ["main.p", "read.p", "defs.i", "schema.df"] {
        std::fs::copy(fixtures.join(name), tmp.path().join(name)).unwrap();
    }
    std::fs::write(tmp.path().join("oxabl.toml"), "[workspace]\nname = \"fixture\"\n[workspace.sources]\ninclude_paths = [\".\"]\n[workspace.schema]\nfiles = [\"schema.df\"]\n").unwrap();
    tmp
}

fn snapshot(name: &str, args: &[&str]) {
    let tmp = fixture();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .arg("explain")
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let actual = String::from_utf8(output.stdout)
        .unwrap()
        .replace(tmp.path().to_str().unwrap(), "<root>");
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/explain")
        .join(name);
    if std::env::var_os("OXABL_UPDATE_SNAPSHOTS").is_some() {
        std::fs::write(&path, &actual).unwrap();
    }
    assert_eq!(actual, std::fs::read_to_string(path).unwrap());
}

#[test]
fn text_and_json_snapshots() {
    snapshot("main.txt", &["main.p"]);
    snapshot("main.json", &["main.p", "--json"]);
    snapshot("scan.txt", &["main.p:5"]);
    snapshot("root.txt", &["main.p:2"]);
}

#[test]
fn bad_line_and_unreadable_file_fail() {
    let tmp = fixture();
    for file in ["main.p:0", "main.p:99999", "absent.p"] {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .current_dir(tmp.path())
            .args(["explain", file])
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(2));
    }
}

#[test]
fn recovered_parse_has_a_partial_summary() {
    let tmp = fixture();
    std::fs::write(tmp.path().join("broken.p"), "DEFINE VARIABLE broken AS .").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .args(["explain", "broken.p", "--json"])
        .output()
        .unwrap();
    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(value["version"], 1);
    assert_eq!(value["units"][0]["summary"]["complete"], false);
    assert!(
        value["units"][0]["summary"]["partial"]
            .as_array()
            .unwrap()
            .iter()
            .any(|r| r == "unit:recovered-parse")
    );
}

#[test]
fn line_selector_accepts_indented_unit_headers() {
    let tmp = fixture();
    std::fs::write(
        tmp.path().join("indented.p"),
        "MESSAGE \"hi\".\n    PROCEDURE inner:\n    END PROCEDURE.\n",
    )
    .unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .args(["explain", "indented.p:2", "--json"])
        .output()
        .unwrap();
    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(value["units"].as_array().unwrap().len(), 1);
    assert_eq!(value["units"][0]["name"], "inner");
}
