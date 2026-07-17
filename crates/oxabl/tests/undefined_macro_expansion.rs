//! End-to-end test for GitHub #64: an undefined `{&name}` reference must
//! expand to the empty string (ABL/AVM behavior), not be preserved as literal
//! text. Preserved text corrupted declaration idioms like
//! `{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.` — the line
//! never parsed as a DEFINE, `myvar` never declared, and every use was a
//! silent LINT0001.
//!
//! Drives the built binary via `CARGO_BIN_EXE_oxabl` (main.rs helpers are
//! private to the binary target).

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

const DEF_I: &str = "{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.\n";

fn analyze_json(root: &Path, host: &Path) -> serde_json::Value {
    let output = oxabl()
        .arg("analyze")
        .arg("--preprocess")
        .arg("--format")
        .arg("json")
        .arg("-I")
        .arg(root.join("inc"))
        .arg(host)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "analyze must succeed; stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_str(&String::from_utf8_lossy(&output.stdout)).unwrap()
}

fn lint0001_messages(v: &serde_json::Value) -> Vec<String> {
    v.get("diagnostics")
        .and_then(|d| d.as_array())
        .expect("diagnostics array present in analyze JSON")
        .iter()
        .filter(|d| d.get("code").and_then(|c| c.as_str()) == Some("LINT0001"))
        .map(|d| {
            d.get("message")
                .and_then(|m| m.as_str())
                .unwrap_or_default()
                .to_string()
        })
        .collect()
}

fn symbol_names(v: &serde_json::Value) -> Vec<String> {
    v.get("symbols")
        .and_then(|s| s.as_array())
        .expect("symbols array present in analyze JSON")
        .iter()
        .filter_map(|s| s.get("name").and_then(|n| n.as_str()))
        .map(str::to_string)
        .collect()
}

#[test]
fn undefined_macro_slot_declares_symbol_no_lint0001() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("inc").join("def.i"), DEF_I);
    let host = root.join("host.p");
    write(
        &host,
        "{def.i &var-type = \"DEFINE NEW GLOBAL SHARED VARIABLE \"}\nDISPLAY myvar.\n",
    );

    let v = analyze_json(root, &host);

    let lint0001 = lint0001_messages(&v);
    assert!(
        lint0001.is_empty(),
        "undefined {{&batch_global_alt}} must expand to empty so myvar declares; got LINT0001: {lint0001:?}"
    );
    assert!(
        symbol_names(&v).iter().any(|n| n == "myvar"),
        "myvar must be declared via the expanded DEFINE"
    );
}

#[test]
fn control_with_global_define_still_clean() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("inc").join("def.i"), DEF_I);
    let host = root.join("host.p");
    write(
        &host,
        "&GLOBAL-DEFINE batch_global_alt\n{def.i &var-type = \"DEFINE NEW GLOBAL SHARED VARIABLE \"}\nDISPLAY myvar.\n",
    );

    let v = analyze_json(root, &host);

    let lint0001 = lint0001_messages(&v);
    assert!(
        lint0001.is_empty(),
        "control case must stay clean; got LINT0001: {lint0001:?}"
    );
    assert!(symbol_names(&v).iter().any(|n| n == "myvar"));
}
