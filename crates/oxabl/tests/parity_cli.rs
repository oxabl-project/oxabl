//! Leg 2 of 4: the CLI (R19).
//!
//! Drives the *built binary* over the shared fixture table — `main.rs`'s helpers
//! are private to the binary target, so this is the only honest way to test what
//! a user actually runs. The comparison is against
//! `oxabl_pipeline::fixtures`, the same table the pipeline, LSP, and browser legs
//! assert against, so a divergence names the client rather than the expectation.
//!
//! **Byte spans, not rendered positions.** `check --json` carries both: `span`
//! is the pipeline's own coordinate space and `start`/`end` are this client's
//! rendering of it. Only `span` is compared here (KTD5) — a line/column bug in
//! one client must not read as a pipeline divergence.
//!
//! `check`'s two channels are re-joined here: lint findings live under
//! `diagnostics` and the loud preprocessor warnings under
//! `preproc_diagnostics`, which is a *reporting* split, not a different
//! diagnostic set. Every fixture is synthetic ABL from the shared table.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use oxabl_pipeline::fixtures::{
    self, Capability, ExpectedFormat, FIXTURES, ObservedDiagnostic, ParityFixture,
};
use serde_json::Value;
use tempfile::TempDir;

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

/// A temp directory holding one fixture's source (and its `.df` when the fixture
/// needs a schema). No `oxabl.toml`, so the binary resolves the same
/// all-defaults configuration [`fixtures::canonical_config`] models.
struct Case {
    _dir: TempDir,
    source: PathBuf,
    schema: Option<PathBuf>,
}

fn case(fixture: &ParityFixture) -> Case {
    let dir = TempDir::new().unwrap();
    let source = dir.path().join("main.p");
    fs::write(&source, fixture.source).unwrap();
    let schema = fixture.needs_capability(Capability::Schema).then(|| {
        let path = dir.path().join("schema.df");
        fs::write(&path, fixtures::CUSTOMER_DF).unwrap();
        path
    });
    Case {
        _dir: dir,
        source,
        schema,
    }
}

/// `oxabl check --json`, with `--schema` when the fixture needs it — and with no
/// preprocessing flag at all.
///
/// That omission is deliberate (R19). This leg runs `check` in its **default**
/// configuration precisely so a default-only divergence cannot hide: the whole
/// point of the parity suite is that the CLI, the pipeline, and the language
/// server answer the same question, and a leg that hand-tunes its flags until
/// the answers line up compensates for a divergence instead of reporting it.
/// This suite previously passed `--preprocess` here to make the leg
/// "equivalent", and that flag is exactly why it stayed green while `check`
/// defaulted preprocessing off and the language server defaulted it on.
fn check_json(case: &Case, extra: &[&str]) -> (Value, Option<i32>, String) {
    let mut command = oxabl();
    command.arg("check").arg("--json").arg(&case.source);
    if let Some(schema) = &case.schema {
        command.arg("--schema").arg(schema);
    }
    for arg in extra {
        command.arg(arg);
    }
    let output = command.output().unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let value = serde_json::from_str(&stdout).unwrap_or_else(|e| {
        panic!("check --json must emit JSON ({e}); stdout: {stdout}\n{stderr}")
    });
    (value, output.status.code(), stderr)
}

/// Re-join `check`'s two diagnostic channels into the shared comparison form.
fn observed(report: &Value) -> Vec<ObservedDiagnostic> {
    let mut all = Vec::new();
    for d in report["diagnostics"].as_array().expect("diagnostics array") {
        all.push(ObservedDiagnostic::from_wire(
            d["code"].as_str().unwrap(),
            d["severity"].as_str().unwrap(),
            d["source"].as_str().unwrap(),
            d["span"]["start"].as_u64().unwrap() as u32,
            d["span"]["end"].as_u64().unwrap() as u32,
        ));
    }
    for d in report["preproc_diagnostics"]
        .as_array()
        .expect("preproc_diagnostics array")
    {
        // This key is the preprocessor channel by construction, so its entries
        // carry no `source` tag of their own.
        all.push(ObservedDiagnostic::from_wire(
            d["code"].as_str().unwrap(),
            d["severity"].as_str().unwrap(),
            "preproc",
            d["span"]["span"]["start"].as_u64().unwrap() as u32,
            d["span"]["span"]["end"].as_u64().unwrap() as u32,
        ));
    }
    all
}

fn format_check(path: &Path) -> Option<i32> {
    oxabl()
        .arg("format")
        .arg("--check")
        .arg(path)
        .output()
        .unwrap()
        .status
        .code()
}

fn format_stdout(path: &Path) -> String {
    let output = oxabl()
        .arg("format")
        .arg("--stdout")
        .arg(path)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(0), "--stdout must not fail");
    String::from_utf8_lossy(&output.stdout).to_string()
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// Every fixture the CLI has the capabilities for yields exactly the shared
/// table's diagnostic set — codes, severities, byte spans, sources.
#[test]
fn every_fixture_matches_the_shared_table_through_the_cli() {
    for fixture in FIXTURES {
        let case = case(fixture);
        let (report, _code, _stderr) = check_json(&case, &[]);
        fixture.assert_diagnostics("cli check --json", observed(&report));
    }
}

/// The clean fixture is clean through the CLI too, in both channels and in the
/// exit code — silence has to mean silence.
#[test]
fn the_clean_fixture_reports_nothing_and_exits_zero() {
    let fixture = fixtures::fixture("clean");
    let case = case(fixture);
    let (report, code, _stderr) = check_json(&case, &[]);

    assert!(observed(&report).is_empty(), "got {report}");
    assert_eq!(report["format"]["drifted_count"], 0);
    assert_eq!(report["failures"].as_array().map(Vec::len), Some(0));
    assert_eq!(code, Some(0));
}

/// The parse-error fixture's *recovered* set survives the CLI: the parse error is
/// reported and the lint pass still ran over the recovered tree.
#[test]
fn a_parse_error_yields_the_same_recovered_set() {
    let fixture = fixtures::fixture("parse_error");
    let case = case(fixture);
    let (report, code, _stderr) = check_json(&case, &[]);

    let observed = observed(&report);
    fixture.assert_diagnostics("cli check --json", observed.clone());
    assert!(
        observed.iter().any(|d| d.code == "PARSE001")
            && observed.iter().any(|d| d.code.starts_with("LINT")),
        "recovery must yield both a parse error and a lint finding: {observed:?}"
    );
    assert_eq!(code, Some(1));
}

/// The loud unresolvable-include warning reaches the CLI's machine-readable
/// channel *and* stderr, and does not move the exit code by itself.
#[test]
fn the_loud_include_warning_reaches_both_cli_channels() {
    let fixture = fixtures::fixture("unresolvable_include");
    let case = case(fixture);
    let (report, code, stderr) = check_json(&case, &[]);

    fixture.assert_diagnostics("cli check --json", observed(&report));
    assert!(
        stderr.contains("PREPROC007"),
        "the warning must be loud on stderr, got: {stderr}"
    );
    assert_eq!(
        code,
        Some(0),
        "a coverage warning is not a finding and must not fail the gate"
    );
}

/// The schema-gated fixture is a *capability*, not a behavior difference: with
/// `--schema` it fires, without it the file is silent.
#[test]
fn the_schema_gated_fixture_needs_the_schema_flag() {
    let fixture = fixtures::fixture("unknown_field");
    let case = case(fixture);
    fixture.assert_diagnostics("cli check --json", observed(&check_json(&case, &[]).0));

    // Same source, same binary, schema withheld.
    let dir = TempDir::new().unwrap();
    let source = dir.path().join("main.p");
    fs::write(&source, fixture.source).unwrap();
    let bare = Case {
        _dir: dir,
        source,
        schema: None,
    };
    assert!(
        observed(&check_json(&bare, &[]).0).is_empty(),
        "without a schema the rule must be inert, not differently loud"
    );
}

// ---------------------------------------------------------------------------
// Format
// ---------------------------------------------------------------------------

/// `check`'s drift channel and `format --check`'s exit code are the same answer
/// for every fixture, and both match the table.
#[test]
fn format_outcomes_agree_through_check_and_through_format() {
    for fixture in FIXTURES {
        let case = case(fixture);
        let (report, _code, _stderr) = check_json(&case, &[]);
        let (would_change, _has_output, _refused) = fixture.expected_format_facts();

        let drifted = report["format"]["drifted"].as_array().unwrap();
        assert_eq!(
            !drifted.is_empty(),
            would_change,
            "fixture `{}`: check's drift channel disagrees with the table",
            fixture.name
        );
        assert_eq!(
            format_check(&case.source),
            Some(i32::from(would_change)),
            "fixture `{}`: format --check disagrees with check's drift channel",
            fixture.name
        );

        // `--stdout` never loses the file: the reformatted bytes on the drift
        // fixture, the original bytes on both leave-it-alone arms.
        let expected_bytes = match fixture.format {
            ExpectedFormat::Reformatted(bytes) => bytes,
            ExpectedFormat::Unchanged | ExpectedFormat::Refused => fixture.source,
        };
        assert_eq!(
            format_stdout(&case.source),
            expected_bytes,
            "fixture `{}`: format --stdout bytes disagree",
            fixture.name
        );
        // A refusal must leave the file untouched on disk in write mode too.
        assert_eq!(fs::read_to_string(&case.source).unwrap(), fixture.source);
    }
}

/// A refusal is neutral, not drift: `check` reports no drifting file, no failure,
/// and exits on its lint channel alone.
#[test]
fn a_format_refusal_is_not_drift() {
    let fixture = fixtures::fixture("parse_error");
    assert!(matches!(fixture.format, ExpectedFormat::Refused));
    let case = case(fixture);
    let (report, _code, stderr) = check_json(&case, &[]);

    assert_eq!(report["format"]["drifted_count"], 0);
    assert_eq!(
        report["failures"].as_array().map(Vec::len),
        Some(0),
        "a bail is not an internal failure: {stderr}"
    );
}

// ---------------------------------------------------------------------------
// Per-rule severity
// ---------------------------------------------------------------------------

/// An `oxabl.toml` override moves the severity and nothing else — the identical
/// transformation the pipeline and LSP legs assert through their own surfaces.
#[test]
fn a_per_rule_severity_override_applies_through_oxabl_toml() {
    let fixture = fixtures::fixture(fixtures::OVERRIDE_FIXTURE);
    let case = case(fixture);
    let baseline = observed(&check_json(&case, &[]).0);

    fs::write(
        case.source.parent().unwrap().join("oxabl.toml"),
        fixtures::OVERRIDE_TOML,
    )
    .unwrap();
    let overridden = observed(&check_json(&case, &[]).0);

    let target = overridden
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .unwrap_or_else(|| panic!("expected {} after the override", fixtures::OVERRIDE_CODE));
    let before = baseline
        .iter()
        .find(|d| d.code == fixtures::OVERRIDE_CODE)
        .expect("the baseline carries the same code");

    assert_eq!(target.severity, fixtures::OVERRIDE_SEVERITY);
    assert_ne!(before.severity, target.severity);
    assert_eq!((target.start, target.end), (before.start, before.end));
    assert_eq!(target.source, before.source);
    assert_eq!(overridden.len(), baseline.len());
}
