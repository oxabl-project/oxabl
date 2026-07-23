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
#[test]
fn parse_success_envelope_shape_preserved() {
    let v = analyze_json("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = 1.\n", false);
    assert_eq!(v["envelope"], 1);
    for section in ["scopes", "symbols", "references", "types", "diagnostics"] {
        assert!(v[section].is_array(), "{section} array present");
        assert!(v["sections"][section].is_number());
    }
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
/// plus the `preproc_diagnostics` channel) equals the collector's set for the
/// same file — same codes and severities. Both go through `collect_with_model`,
/// so this locks that they cannot drift.
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
    for d in v["preproc_diagnostics"].as_array().unwrap() {
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
        !v["preproc_diagnostics"].as_array().unwrap().is_empty(),
        "expected preproc content from the CLI"
    );
    assert_eq!(cli, lib, "CLI diagnostic set must equal the collector's");
}
