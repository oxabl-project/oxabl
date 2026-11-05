//! A single-quoted include argument with a `:U` suffix, end to end through
//! `oxabl analyze --preprocess`.
//!
//! The compiler groups only double-quoted include arguments. A single quote is
//! plain text, so `'a,b':U` is one argument written as is. Split in two, the
//! `:U` became the next argument, and a handle built from it (`:U:PRIVATE-DATA`)
//! was reported as a class named `:u` once the file had a `USING`.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

/// Picks the handle from argument 2, defaulting to `THIS-PROCEDURE`, the way
/// accessor includes do. The `RUN` shows which branch was taken.
const ACCESSOR: &str = "\
&IF \"{2}\":U = \"\":U &THEN &SCOPED-DEFINE Hdl THIS-PROCEDURE
RUN default-handle.p.
&ELSE &SCOPED-DEFINE Hdl {2}
RUN given-handle.p.
&ENDIF
v = ENTRY(1, {&Hdl}:PRIVATE-DATA).
v = v + {1}.
";

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// `analyze --preprocess -I <root>/inc` of `source`, as JSON.
fn analyze(source: &str) -> serde_json::Value {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("inc/acc.i"), ACCESSOR);
    let main = root.join("main.p");
    write(&main, source);
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--preprocess")
        .arg("-I")
        .arg(root.join("inc"))
        .arg(&main)
        .output()
        .unwrap();
    assert!(out.status.success(), "{out:?}");
    serde_json::from_slice(&out.stdout).expect("valid analyze JSON")
}

fn rows(v: &serde_json::Value, via: &str) -> Vec<String> {
    v["dependencies"]["unresolved"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|row| row["via"] == via)
        .map(|row| row["name"].as_str().unwrap().to_string())
        .collect()
}

fn class_rows(v: &serde_json::Value) -> Vec<String> {
    rows(v, "class")
}

fn diagnostic_codes(v: &serde_json::Value) -> Vec<String> {
    v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap_or_default().to_string())
        .collect()
}

#[test]
fn a_single_quoted_argument_with_a_suffix_is_one_argument() {
    let v = analyze(
        "USING acme.Thing.\n\
         DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n\
         {acc.i 'a,b':U}\n",
    );
    assert_eq!(class_rows(&v), Vec::<String>::new());
    assert_eq!(rows(&v, "program"), ["default-handle.p"]);
    let parse: Vec<String> = diagnostic_codes(&v)
        .into_iter()
        .filter(|c| c.starts_with("PARSE") || c.starts_with("PREPROC"))
        .collect();
    assert_eq!(parse, Vec::<String>::new(), "{v:#}");
}

#[test]
fn a_named_type_still_gives_a_class_row_with_its_span() {
    let source = "USING acme.Thing.\n\
                  DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n\
                  {acc.i 'a,b':U}\n\
                  Thing:Run().\n";
    let v = analyze(source);
    let rows = v["dependencies"]["unresolved"].as_array().unwrap();
    let class: Vec<&serde_json::Value> = rows.iter().filter(|r| r["via"] == "class").collect();
    assert_eq!(class.len(), 1, "{rows:#?}");
    assert_eq!(class[0]["name"], "acme.thing");
    let start = class[0]["span"]["start"].as_u64().unwrap() as usize;
    let end = class[0]["span"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&source[start..end], "Thing");
}

#[test]
fn an_attribute_of_the_widget_in_focus_is_no_class() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(
        &main,
        "USING acme.Thing.\n\
         DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n\
         v = :U:PRIVATE-DATA.\n",
    );
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg(&main)
        .output()
        .unwrap();
    assert!(out.status.success(), "{out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(class_rows(&v), Vec::<String>::new());
}
