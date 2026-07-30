//! End-to-end tests for include-path config loading and the loud
//! unresolvable-include diagnostic (PREPROC007).
//!
//! `main.rs`'s helpers are private to the binary target and cannot be imported
//! here, so these drive the *built binary* via `CARGO_BIN_EXE_oxabl`.

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

#[test]
fn check_reports_preproc007_when_include_dir_absent() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    // The include elides to nothing, but the remaining program still parses,
    // so the *only* signal should be the loud PREPROC007 on stderr.
    write(&root.join("main.p"), "{globals.i}\nMESSAGE \"hi\".\n");

    let output = oxabl()
        .arg("check")
        .arg("--preprocess")
        .arg(root)
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("PREPROC007"),
        "expected a loud PREPROC007 on stderr, got:\n{stderr}"
    );
    assert!(
        stderr.contains("globals.i"),
        "diagnostic should name the missing include, got:\n{stderr}"
    );
    // Elided include still parses → success exit code, not a failure.
    assert!(
        output.status.success(),
        "expected exit 0 (parse still succeeds)"
    );
}

#[test]
fn check_clean_when_oxabl_toml_provides_include_path() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(
        &root.join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
    );
    write(
        &root.join("inc").join("globals.i"),
        "DEFINE VARIABLE gcCompany AS CHARACTER NO-UNDO.\n",
    );
    write(&root.join("main.p"), "{globals.i}\nMESSAGE gcCompany.\n");

    let output = oxabl()
        .arg("check")
        .arg("--preprocess")
        .arg(root)
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("PREPROC007"),
        "config include_paths should resolve the include; no PREPROC007 expected, got:\n{stderr}"
    );
    assert!(output.status.success(), "expected exit 0");
}

/// The unresolvable include reaches the envelope's `preproc` section — a real
/// versioned section now, not a key the CLI splices in after the library returns
/// the document.
#[test]
fn analyze_json_lists_preproc_diagnostics() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    let main = root.join("main.p");
    write(&main, "{globals.i}\nMESSAGE \"hi\".\n");

    let output = oxabl()
        .arg("analyze")
        .arg("--preprocess")
        .arg("--format")
        .arg("json")
        .arg(&main)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert!(
        v["sections"]["preproc"].is_number(),
        "the preproc section must be versioned like every other, got:\n{stdout}"
    );
    let diags = v
        .get("preproc")
        .and_then(|d| d.as_array())
        .expect("preproc array present in analyze JSON");
    assert!(
        diags
            .iter()
            .any(|d| d.get("code").and_then(|c| c.as_str()) == Some("PREPROC007")),
        "expected a PREPROC007 entry in the preproc section, got:\n{stdout}"
    );
}

/// The same fact reaches `--format text`, which it never did while the CLI
/// spliced it into the JSON only.
#[test]
fn analyze_text_renders_the_preproc_and_coverage_sections() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, "{globals.i}\nMESSAGE \"hi\".\n");

    let output = oxabl()
        .arg("analyze")
        .arg("--preprocess")
        .arg("--format")
        .arg("text")
        .arg(&main)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("=== Preprocessor (1) ==="),
        "expected the preproc section in text output, got:\n{stdout}"
    );
    assert!(
        stdout.contains("PREPROC007"),
        "expected the unresolvable-include code, got:\n{stdout}"
    );
    assert!(
        stdout.contains("=== Coverage ===") && stdout.contains("unjudged symbols: 0"),
        "expected the coverage section in text output, got:\n{stdout}"
    );
}

/// A nested-include diagnostic must render without a root-relative line/col.
///
/// Driven through `conformance`, not `check`: this assertion is about
/// `surface_preproc_diagnostics`, which renders the *preprocessor's own*
/// diagnostic list, and the conformance walk is now its only caller. `check` and
/// `analyze` go through the shared expansion instead, which drops loud
/// diagnostics whose span belongs to an include rather than to the root file —
/// so there is no include-origin diagnostic left for them to mis-position. The
/// behavior described here is unchanged; only the subcommand that exercises it
/// moved.
#[test]
fn nested_unresolved_include_renders_without_garbage_linecol() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(
        &root.join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
    );
    // outer.i resolves; the {missing.i} it references does not.
    write(
        &root.join("inc").join("outer.i"),
        "MESSAGE \"outer\".\n{missing.i}\n",
    );
    write(&root.join("main.p"), "{outer.i}\n");

    let output = oxabl()
        .arg("conformance")
        .arg("--preprocess")
        .arg(root)
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    // The nested-include diagnostic belongs to outer.i's FileId, so the CLI must
    // NOT compute a line/col against the root file's SourceMap — it degrades to
    // an "in included file" message instead of a fabricated position.
    assert!(stderr.contains("PREPROC007"), "stderr:\n{stderr}");
    assert!(
        stderr.contains("in included file"),
        "nested-include diagnostic must render without a root-relative line/col, got:\n{stderr}"
    );
    assert!(stderr.contains("missing.i"));
}
