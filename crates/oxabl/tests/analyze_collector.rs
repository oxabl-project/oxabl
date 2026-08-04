//! U4 characterization + parity coverage for the `analyze` refactor onto the
//! shared diagnostics collector.
//!
//! `main.rs`'s helpers are private to the binary target, so these drive the
//! built binary via `CARGO_BIN_EXE_oxabl` and compare its emitted diagnostic
//! set against the collector library directly.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use oxabl_analyze::{DiagnosticSource, collect_diagnostics};
use oxabl_common::FileId;
use oxabl_schema::Schema;
use oxabl_workspace::{InMemoryFileSystem, LintConfig};
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

fn analyze_json(source: &str, preprocess: bool) -> serde_json::Value {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, source);
    let mut cmd = oxabl();
    cmd.arg("analyze").arg("--format").arg("json");
    if preprocess {
        cmd.arg("--preprocess");
    }
    let output = cmd.arg(&main).output().unwrap();
    assert!(
        output.status.success(),
        "analyze must not abort (recovery), stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("valid analyze JSON")
}

/// Characterization: a clean, parse-success file yields the stable envelope
/// with all sections and an (empty-of-parse-errors) diagnostics array.
///
/// `preproc` and `coverage` are asserted here alongside the original five
/// because they are ordinary versioned sections now, not keys the CLI splices in
/// after the library hands the document back.
#[test]
fn parse_success_envelope_shape_preserved() {
    let v = analyze_json("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = 1.\n", false);
    assert_eq!(v["envelope"], 1);
    for section in [
        "scopes",
        "symbols",
        "references",
        "types",
        "diagnostics",
        "preproc",
    ] {
        assert!(v[section].is_array(), "{section} array present");
        assert!(v["sections"][section].is_number());
    }
    assert!(v["sections"]["coverage"].is_number());
    assert!(v["coverage"]["unjudged_symbols"].is_u64());
    // `dependencies` is the eighth section and, like `coverage`, an object — so
    // the next index fact is an added key rather than a ninth section. Present
    // and empty for a file with nothing cross-file about it, never missing.
    assert!(v["sections"]["dependencies"].is_number());
    assert!(v["dependencies"].is_object());
    assert!(v["dependencies"]["index_revision"].is_u64());
    assert!(v["dependencies"]["files"].as_array().unwrap().is_empty());
    assert!(
        v["dependencies"]["unresolved"]
            .as_array()
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        v["sections"].as_object().unwrap().len(),
        8,
        "the envelope emits eight versioned sections"
    );
    // No parse errors in a clean file.
    let diags = v["diagnostics"].as_array().unwrap();
    assert!(
        diags.iter().all(|d| d["source"] != "parse"),
        "clean file has no parse diagnostics: {diags:?}"
    );
}

/// Characterization of the *intended* behavior change: with a parse error,
/// `analyze` recovers (exit 0) and still emits parse + lint diagnostics
/// instead of aborting.
#[test]
fn parse_error_recovers_and_still_dumps() {
    let v = analyze_json(
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n@ @ @\nDEFINE VARIABLE y AS INTEGER NO-UNDO.\n",
        false,
    );
    let diags = v["diagnostics"].as_array().unwrap();
    let codes: Vec<&str> = diags.iter().filter_map(|d| d["code"].as_str()).collect();
    assert!(
        codes.contains(&"PARSE001"),
        "expected parse error: {codes:?}"
    );
    assert!(
        codes.contains(&"LINT0002"),
        "expected lint despite parse error: {codes:?}"
    );
}

/// R7 parity: the CLI's emitted diagnostic set (envelope `diagnostics` section
/// plus the `preproc` section) equals the collector's set for the same file —
/// same codes and severities. Both go through the shared collector, so this locks
/// that they cannot drift.
#[test]
fn cli_matches_collector_diagnostic_set() {
    // Unresolvable include → PREPROC007 (preproc channel); unused var → LINT0002;
    // stray tokens → PARSE001. Exercises the full union across channels.
    let source = "{missing.i}\nDEFINE VARIABLE unusedVar AS INTEGER NO-UNDO.\n@ @ @\n";

    // CLI side.
    let v = analyze_json(source, true);
    let mut cli: Vec<(String, String)> = Vec::new();
    for d in v["diagnostics"].as_array().unwrap() {
        cli.push((
            d["code"].as_str().unwrap().to_string(),
            d["severity"].as_str().unwrap().to_string(),
        ));
    }
    for d in v["preproc"].as_array().unwrap() {
        cli.push((
            d["code"].as_str().unwrap().to_string(),
            d["severity"].as_str().unwrap().to_string(),
        ));
    }
    cli.sort();

    // Collector side (same inputs: no includes, no schema, preprocessing on).
    let fs = InMemoryFileSystem::new();
    let schema = Schema::empty();
    // Match the CLI's resolved default severity surface (no oxabl.toml here).
    let collected = collect_diagnostics(
        FileId::new(1),
        source,
        &fs,
        &[] as &[PathBuf],
        &schema,
        false,
        &LintConfig::default().to_severity_map(),
        true,
    );
    let mut lib: Vec<(String, String)> = collected
        .all()
        .map(|c| {
            (
                c.diagnostic.code.0.to_string(),
                format!("{:?}", c.diagnostic.severity).to_lowercase(),
            )
        })
        .collect();
    lib.sort();

    // Sanity: this fixture exercises the preproc channel on both sides.
    assert!(
        collected.by_source(DiagnosticSource::Preproc).count() > 0,
        "expected preproc content from the collector"
    );
    assert!(
        !v["preproc"].as_array().unwrap().is_empty(),
        "expected preproc content from the CLI"
    );
    assert_eq!(cli, lib, "CLI diagnostic set must equal the collector's");
}

// ---------------------------------------------------------------------------
// The subcommand's own contract: hidden, non-gating, and configuration-tolerant
// ---------------------------------------------------------------------------

/// Hidden from `--help` (R23) but otherwise a normal subcommand: its own help
/// works and it runs. A hidden command with no way in would be a deleted one.
#[test]
fn analyze_is_hidden_from_help_but_fully_usable() {
    let help = oxabl().arg("--help").output().unwrap();
    assert!(
        !String::from_utf8_lossy(&help.stdout).contains("analyze"),
        "analyze must not appear in the advertised surface"
    );

    let sub = oxabl().arg("analyze").arg("--help").output().unwrap();
    assert!(sub.status.success(), "`analyze --help` must work");
    let sub_help = String::from_utf8_lossy(&sub.stdout);
    assert!(sub_help.contains("--no-lint"), "got:\n{sub_help}");
    assert!(sub_help.contains("--schema"), "got:\n{sub_help}");

    // And it still runs — `analyze_json` asserts success internally.
    let v = analyze_json("MESSAGE \"hi\".\n", false);
    assert_eq!(v["envelope"], 1);
}

/// The cross-file contract, end to end through the built binary and in **both**
/// formats: a child class analysed with its parent on the include path shows the
/// inherited member with its return type, the call site resolving to it, and the
/// parent's file as a consulted dependency. Text output carries the same section —
/// which is the property `preproc` and `coverage` lost for as long as the CLI
/// spliced them into the finished JSON.
///
/// Fixtures are synthetic ABL, and the temp directory is the only path involved.
#[test]
fn a_cross_file_child_reports_its_inherited_member_in_both_formats() {
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    write(
        &src.join("orders/calc-base.cls"),
        "CLASS orders.calc-base:\n    METHOD PUBLIC INTEGER calc-total():\n        RETURN 0.\n    END METHOD.\nEND CLASS.\n",
    );
    let child = src.join("orders/child.cls");
    write(
        &child,
        "CLASS orders.child INHERITS orders.calc-base:\n    METHOD PUBLIC VOID run-it():\n        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n        v-total = calc-total().\n        MESSAGE v-total.\n    END METHOD.\nEND CLASS.\n",
    );

    let json_out = oxabl()
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("-I")
        .arg(&src)
        .arg(&child)
        .output()
        .unwrap();
    assert!(json_out.status.success(), "{json_out:?}");
    let v: serde_json::Value = serde_json::from_slice(&json_out.stdout).unwrap();

    let member = v["symbols"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["name"] == "calc-total")
        .expect("the inherited member is dumped");
    assert_eq!(member["data_type"], "integer");
    assert_eq!(member["data_type_source"], "inherited");
    assert_eq!(member["origin"], "cross_file");

    let sid = member["id"].as_u64().unwrap();
    assert!(
        v["references"]
            .as_array()
            .unwrap()
            .iter()
            .any(|r| r["symbol"].as_u64() == Some(sid) && r["origin"] == "cross_file"),
        "the call site must resolve cross-file"
    );
    assert!(
        v["dependencies"]["files"]
            .as_array()
            .unwrap()
            .iter()
            .any(|f| f["via"] == "class" && f["target"] == "orders.calc-base"),
        "the parent's file is a consulted dependency: {}",
        v["dependencies"]
    );
    assert!(v["dependencies"]["index_revision"].as_u64().unwrap() > 0);

    let text_out = oxabl()
        .arg("analyze")
        .arg("--format")
        .arg("text")
        .arg("-I")
        .arg(&src)
        .arg(&child)
        .output()
        .unwrap();
    assert!(text_out.status.success(), "{text_out:?}");
    let text = String::from_utf8_lossy(&text_out.stdout);
    assert!(text.contains("=== Dependencies ==="), "got:\n{text}");
    assert!(text.contains("class orders.calc-base"), "got:\n{text}");
    assert!(
        text.contains("integer(inherited)"),
        "the inherited return type must show in text too, got:\n{text}"
    );
}

/// Introspection, not a gate (KTD9): a file full of findings is still exit 0.
#[test]
fn a_file_with_lint_findings_still_exits_0() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, "DEFINE VARIABLE neverUsed AS INTEGER NO-UNDO.\n");

    let out = oxabl().arg("analyze").arg(&main).output().unwrap();
    assert!(out.status.success(), "analyze must not gate: {out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let codes: Vec<&str> = v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|d| d["code"].as_str())
        .collect();
    assert!(codes.contains(&"LINT0002"), "{codes:?}");
}

/// `--no-lint` is a source filter, so the lint entries go and the parse and
/// semantic ones stay. A skipped run would have taken all three.
#[test]
fn no_lint_drops_only_the_lint_sourced_diagnostics() {
    // `neverUsed` → LINT0002; the stray tokens → PARSE001; `undefinedThing` →
    // LINT0001, which is *semantic*-sourced… so assert on the source labels
    // rather than guessing which rule owns which code.
    let source = "DEFINE VARIABLE neverUsed AS INTEGER NO-UNDO.\n@ @ @\n";

    let with_lint = analyze_json(source, false);
    let sources: Vec<&str> = with_lint["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|d| d["source"].as_str())
        .collect();
    assert!(
        sources.contains(&"lint"),
        "fixture must produce lint: {sources:?}"
    );
    assert!(
        sources.contains(&"parse"),
        "fixture must produce parse: {sources:?}"
    );

    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, source);
    let out = oxabl()
        .arg("analyze")
        .arg("--no-lint")
        .arg("--format")
        .arg("json")
        .arg(&main)
        .output()
        .unwrap();
    assert!(out.status.success());
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let sources: Vec<&str> = v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|d| d["source"].as_str())
        .collect();
    assert!(
        !sources.contains(&"lint"),
        "--no-lint must drop lint findings: {sources:?}"
    );
    assert!(
        sources.contains(&"parse"),
        "--no-lint must keep parse errors: {sources:?}"
    );
    // The model is still dumped — the whole reason this is a filter and not a
    // skipped run.
    assert!(!v["symbols"].as_array().unwrap().is_empty());
}

/// An unrecognized `--format` is its own exit code, distinct from a usage error.
#[test]
fn an_unsupported_format_exits_7() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, "MESSAGE \"hi\".\n");

    let out = oxabl()
        .arg("analyze")
        .arg("--format")
        .arg("yaml")
        .arg(&main)
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(7), "{out:?}");
}

/// A `--schema` path that cannot be read is a warning, never a failure: a
/// partially-loaded (or absent) schema still drives resolution, and the dump is
/// the product.
#[test]
fn an_unreadable_schema_path_is_non_fatal() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, "MESSAGE \"hi\".\n");

    let out = oxabl()
        .arg("analyze")
        .arg("--schema")
        .arg(tmp.path().join("absent.df"))
        .arg(&main)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "an unreadable schema must not change the exit code: {out:?}"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("schema:"),
        "the load problem is still reported: {out:?}"
    );
    serde_json::from_slice::<serde_json::Value>(&out.stdout).expect("the dump still happens");
}

/// A malformed `oxabl.toml` degrades to defaults with **one** `warning:` line
/// (R7) — not one per configured surface, which is what three independent
/// `resolved_*` calls used to produce.
#[test]
fn a_malformed_config_warns_once_and_proceeds() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("oxabl.toml"), "this is not valid toml {{{");
    let main = root.join("main.p");
    write(&main, "DEFINE VARIABLE neverUsed AS INTEGER NO-UNDO.\n");

    let out = oxabl().arg("analyze").arg(&main).output().unwrap();
    assert!(out.status.success(), "{out:?}");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let warnings = stderr.lines().filter(|l| l.starts_with("warning:")).count();
    assert_eq!(warnings, 1, "exactly one config warning, got:\n{stderr}");
    // Defaults still apply, so the default-severity rule still fires.
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let codes: Vec<&str> = v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|d| d["code"].as_str())
        .collect();
    assert!(codes.contains(&"LINT0002"), "{codes:?}");
}

/// Exit 3 — fatal preprocessing failure — is documented but **not reachable from
/// any ABL input**, so there is deliberately no test that pretends to cover it.
///
/// `Preprocessor::process` returns `Err` only when it emits an error diagnostic
/// *and* produces an empty span tree. Every loud case that looks like a candidate
/// (an unclosed `&IF`, a malformed condition, an orphan `&ELSE` or `&ENDIF`, an
/// empty file, a self-include) returns `Ok` with text the parser can still see, so
/// each of them exits 0 with diagnostics rather than 3. A test driving one of
/// those would pass while exercising the wrong arm — worse than no test.
///
/// This one pins the *reachable* neighbour so the claim stays honest: an
/// unresolvable include, the loudest preprocessor problem there is, exits 0.
#[test]
fn the_loudest_preprocessor_problem_is_still_exit_0_not_3() {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, "{missing.i}\nMESSAGE \"hi\".\n");

    let out = oxabl()
        .arg("analyze")
        .arg("--preprocess")
        .arg(&main)
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "an unresolvable include is a coverage warning, not a fatal expansion: {out:?}"
    );
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert!(
        !v["preproc"].as_array().unwrap().is_empty(),
        "and it is reported: {v}"
    );
}
