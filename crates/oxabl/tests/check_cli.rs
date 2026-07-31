//! End-to-end tests for the `oxabl check` subcommand — the lint-and-format gate.
//!
//! `check` reports two channels, deliberately not merged (KTD7): span-anchored
//! lint diagnostics, and a per-file format-drift summary naming the drifting
//! paths. These tests pin both channels, the `--no-lint`/`--no-format`
//! suppression switches (R16), the exit-code contract (R15), the
//! report-and-continue posture on a per-file failure (R24), and the
//! unjudged-symbol coverage note that must never move the exit code (R26).
//!
//! `main.rs`'s helpers are private to the binary target, so these drive the
//! *built binary* via `CARGO_BIN_EXE_oxabl` over `tempfile` dirs. All fixtures
//! are synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use oxabl_common::panic_sites;
use serde_json::Value;
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

/// A variable declared and never referenced: one LINT0002, no format drift.
const UNUSED: &str = "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n";
/// Lints clean and is already formatted: no findings in either channel.
const CLEAN: &str = "MESSAGE \"hi\".\n";
/// Lints clean but is mis-indented: format drift only.
const MIS_INDENTED: &str = "DO:\nMESSAGE \"x\".\nEND.\n";
/// Mis-indented *and* holding an unused variable: one finding in each channel.
const BOTH: &str = "DO:\nDEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nEND.\n";
/// `PUT` is recognized but unmodelled, so the count-gated rules go blind on
/// `v-total` — a coverage note, not a finding.
const UNJUDGED: &str = "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\nPUT v-total.\n";
/// A hard `PARSE001` *and* a lint finding, so a run can be asked which channel
/// a suppression flag actually silenced.
const PARSE_ERROR_AND_UNUSED: &str = "DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\n@ @ @\n";

/// A one-file project holding `source`, returned with the file's path.
fn project(name: &str, source: &str) -> (TempDir, std::path::PathBuf) {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join(name);
    write(&file, source);
    (tmp, file)
}

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

impl Run {
    fn json(&self) -> Value {
        serde_json::from_str(&self.stdout)
            .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{}", self.stdout))
    }
}

fn check<I, S>(args: I) -> Run
where
    I: IntoIterator<Item = S>,
    S: AsRef<std::ffi::OsStr>,
{
    let out = oxabl().arg("check").args(args).output().unwrap();
    Run {
        code: out.status.code(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

// ---------------------------------------------------------------------------
// The lint channel
// ---------------------------------------------------------------------------

#[test]
fn a_lint_finding_exits_1_and_is_reported_with_path_line_and_column() {
    let (_tmp, file) = project("unused.p", UNUSED);

    let run = check([file.as_os_str()]);

    assert_eq!(run.code, Some(1), "a finding is a failed gate (R15)");
    assert!(
        run.stdout.contains("LINT0002"),
        "expected the rule code, got:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains(&format!("{}:1:17", file.display())),
        "expected path:line:column, got:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("v-total"),
        "the finding should name the symbol, got:\n{}",
        run.stdout
    );
}

/// A passing run says how many files it checked (D5).
///
/// Silence on success is indistinguishable from silence on a mistyped path that
/// happened to resolve to one clean file — and a gate whose green light might mean
/// "I checked nothing you cared about" is not one you can trust in CI. The count
/// is the cheapest thing that separates the two.
#[test]
fn a_clean_already_formatted_file_exits_0_with_a_summary_naming_the_count() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("a-clean.p"), CLEAN);
    write(&tmp.path().join("z-clean.p"), CLEAN);

    let run = check([tmp.path().as_os_str()]);

    assert_eq!(
        run.code,
        Some(0),
        "stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
    assert!(
        run.stdout.contains("checked 2 files"),
        "expected the file count, got:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("no findings") && run.stdout.contains("no drift"),
        "expected both channels reported clean, got:\n{}",
        run.stdout
    );
    // A finding of any kind replaces the summary — the summary is the *pass*
    // message, so it must never appear alongside a failure.
    let (_tmp2, unused) = project("unused.p", UNUSED);
    let failing = check([unused.as_os_str()]);
    assert!(
        !failing.stdout.contains("no findings"),
        "a failing run must not claim to be clean, got:\n{}",
        failing.stdout
    );
}

/// The summary is a text-mode nicety: `--json` already carries `files_checked`,
/// and a prose line printed alongside the document would make stdout unparseable.
#[test]
fn the_success_summary_stays_out_of_the_json_document() {
    let (_tmp, file) = project("clean.p", CLEAN);

    let run = check([Path::new("--json").as_os_str(), file.as_os_str()]);

    assert_eq!(run.code, Some(0));
    // Parses at all, which a prepended prose line would prevent.
    let v = run.json();
    assert_eq!(v["files_checked"], 1);
    assert!(
        !run.stdout.contains("no findings"),
        "the count belongs to the document in JSON mode, got:\n{}",
        run.stdout
    );
}

// ---------------------------------------------------------------------------
// The format-drift channel (R14): its own summary, naming the paths
// ---------------------------------------------------------------------------

#[test]
fn format_drift_is_a_summary_naming_the_path_not_a_lint_diagnostic() {
    let (_tmp, file) = project("messy.p", MIS_INDENTED);

    let run = check([file.as_os_str()]);

    assert_eq!(run.code, Some(1), "drift alone fails the gate (R15)");
    assert!(
        run.stdout.contains(&file.display().to_string()),
        "the drift channel must name the file (R14), got:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("1 file would be reformatted"),
        "expected the count as a trailing total (R14), got:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("oxabl format"),
        "the summary should point at the fix, got:\n{}",
        run.stdout
    );
    // KTD7: drift is a per-file boolean, so it must never be dressed up as a
    // span-anchored finding with a fabricated position.
    assert!(
        !run.stdout.contains("LINT"),
        "drift must not be reported as a lint diagnostic, got:\n{}",
        run.stdout
    );
}

#[test]
fn format_drift_names_the_path_in_json_too() {
    let (_tmp, file) = project("messy.p", MIS_INDENTED);

    let run = check([Path::new("--json").as_os_str(), file.as_os_str()]);

    assert_eq!(run.code, Some(1));
    let v = run.json();
    assert_eq!(v["format"]["drifted_count"], 1);
    assert_eq!(
        v["format"]["drifted"][0].as_str(),
        Some(file.display().to_string().as_str())
    );
    assert!(
        v["diagnostics"].as_array().unwrap().is_empty(),
        "drift belongs to the format key alone, got:\n{}",
        run.stdout
    );
}

// ---------------------------------------------------------------------------
// Channel suppression (R16)
// ---------------------------------------------------------------------------

#[test]
fn no_format_exits_0_on_a_file_whose_only_problem_is_drift() {
    let (_tmp, file) = project("messy.p", MIS_INDENTED);

    let run = check([Path::new("--no-format").as_os_str(), file.as_os_str()]);

    assert_eq!(run.code, Some(0), "stdout:\n{}", run.stdout);
    assert!(!run.stdout.contains("would be reformatted"));
}

#[test]
fn no_lint_exits_0_on_a_file_whose_only_problem_is_a_lint_finding() {
    let (_tmp, file) = project("unused.p", UNUSED);

    let run = check([Path::new("--no-lint").as_os_str(), file.as_os_str()]);

    assert_eq!(run.code, Some(0), "stdout:\n{}", run.stdout);
    assert!(!run.stdout.contains("LINT0002"));
}

/// `--no-lint` suppresses the **lint** findings, not the parse and semantic
/// errors that share the same pipeline run (A1).
///
/// The flag is named for the channel it silences, and `analyze --no-lint` has
/// always meant exactly this: filter the lint-sourced diagnostics out of the
/// reported set. `check`'s used to skip the run outright, which also threw away
/// the only source of `PARSE001` — so a file oxabl could not even parse passed
/// the gate with a green exit code. A gate that reports success on unparseable
/// source is worse than no gate.
#[test]
fn no_lint_still_gates_on_parse_errors() {
    let (_tmp, file) = project("bad.p", PARSE_ERROR_AND_UNUSED);

    let run = check([Path::new("--no-lint").as_os_str(), file.as_os_str()]);

    assert_eq!(
        run.code,
        Some(1),
        "a parse error must still fail the gate under --no-lint. stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
    assert!(
        run.stdout.contains("PARSE001"),
        "the parse error must still be reported, got:\n{}",
        run.stdout
    );
    assert!(
        !run.stdout.contains("LINT0002"),
        "the lint channel is what --no-lint silences, got:\n{}",
        run.stdout
    );

    let json = check([
        Path::new("--json").as_os_str(),
        Path::new("--no-lint").as_os_str(),
        file.as_os_str(),
    ])
    .json();
    let sources: Vec<&str> = json["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["source"].as_str().unwrap())
        .collect();
    assert!(
        sources.contains(&"parse"),
        "expected a parse-sourced entry, got {sources:?} in:\n{json}"
    );
    assert!(
        !sources.contains(&"lint"),
        "expected no lint-sourced entry, got {sources:?} in:\n{json}"
    );
}

#[test]
fn both_channels_suppressed_exits_0_on_a_file_with_findings_in_each() {
    let (_tmp, file) = project("both.p", BOTH);

    // Sanity: the fixture really does trip both channels.
    let loud = check([file.as_os_str()]);
    assert_eq!(loud.code, Some(1));
    assert!(loud.stdout.contains("LINT0002"), "{}", loud.stdout);
    assert!(
        loud.stdout.contains("would be reformatted"),
        "{}",
        loud.stdout
    );

    let run = check([
        Path::new("--no-lint").as_os_str(),
        Path::new("--no-format").as_os_str(),
        file.as_os_str(),
    ]);
    assert_eq!(run.code, Some(0), "stdout:\n{}", run.stdout);

    // With both channels off, an empty result must not read as "clean".
    let json = check([
        Path::new("--json").as_os_str(),
        Path::new("--no-lint").as_os_str(),
        Path::new("--no-format").as_os_str(),
        file.as_os_str(),
    ])
    .json();
    assert_eq!(json["lint_enabled"], false);
    assert_eq!(json["format_enabled"], false);
}

// ---------------------------------------------------------------------------
// The `--json` shape (KTD7): two findings keys
// ---------------------------------------------------------------------------

#[test]
fn json_carries_two_findings_keys_and_span_anchored_lint_entries() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("unused.p"), UNUSED);
    write(&tmp.path().join("messy.p"), MIS_INDENTED);

    let run = check([Path::new("--json").as_os_str(), tmp.path().as_os_str()]);
    assert_eq!(run.code, Some(1));

    let v = run.json();
    assert_eq!(v["version"], 2, "bumped when a key's meaning changes (D1)");
    assert_eq!(v["files_checked"], 2);
    assert_eq!(v["lint_enabled"], true);
    assert_eq!(v["format_enabled"], true);

    // Two keys, not one merged array.
    let diagnostics = v["diagnostics"].as_array().expect("diagnostics array");
    assert_eq!(v["format"]["drifted_count"], 1);

    let finding = diagnostics
        .iter()
        .find(|d| d["code"] == "LINT0002")
        .unwrap_or_else(|| panic!("expected a LINT0002 entry, got:\n{}", run.stdout));
    assert_eq!(finding["severity"], "warning");
    assert_eq!(finding["source"], "lint");
    assert!(
        finding["path"].as_str().unwrap().ends_with("unused.p"),
        "each entry carries its own path, got {:?}",
        finding["path"]
    );
    // The byte span over `v-total` in the fixture, plus the derived position.
    assert_eq!(finding["span"]["start"], 16);
    assert_eq!(finding["span"]["end"], 23);
    assert_eq!(finding["start"]["byte"], 16);
    assert_eq!(finding["start"]["line"], 1);
    assert_eq!(finding["start"]["column"], 17);

    assert!(v["failures"].as_array().unwrap().is_empty());
    assert_eq!(v["unjudged_symbols"], 0);
}

// ---------------------------------------------------------------------------
// Usage errors (R15: exit 2)
// ---------------------------------------------------------------------------

#[test]
fn a_path_that_does_not_exist_exits_2() {
    let tmp = TempDir::new().unwrap();
    let missing = tmp.path().join("nope.p");

    let run = check([missing.as_os_str()]);

    assert_eq!(run.code, Some(2), "a typo'd path is a usage error");
    assert!(
        run.stderr.contains("Path does not exist"),
        "got:\n{}",
        run.stderr
    );
}

#[test]
fn a_directory_with_no_abl_files_exits_2() {
    let tmp = TempDir::new().unwrap();
    // A non-root extension: present, but nothing `check` treats as a unit.
    write(&tmp.path().join("notes.txt"), "not ABL\n");

    let run = check([tmp.path().as_os_str()]);

    assert_eq!(run.code, Some(2), "an empty source tree is a usage error");
    assert!(
        run.stderr.contains("no ABL files found in"),
        "got:\n{}",
        run.stderr
    );
}

#[test]
fn an_explicitly_named_file_is_checked_without_extension_filtering() {
    // `.abl` is not a root extension, so a directory walk would skip it —
    // naming it is an explicit instruction and must be honored.
    let (_tmp, file) = project("snippet.abl", UNUSED);

    let run = check([file.as_os_str()]);

    assert_eq!(run.code, Some(1), "stdout:\n{}", run.stdout);
    assert!(run.stdout.contains("LINT0002"), "got:\n{}", run.stdout);
}

// ---------------------------------------------------------------------------
// Config: `[workspace.lint]` severities reach the gate
// ---------------------------------------------------------------------------

#[test]
fn a_lint_severity_override_changes_the_exit_code() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("unused.p");
    write(&file, UNUSED);

    // Without a config the rule fires and the gate fails.
    assert_eq!(check([file.as_os_str()]).code, Some(1));

    // Turning it off is the difference between exit 1 and exit 0.
    write(
        &tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.lint]\nunused-variable = \"off\"\n",
    );
    let run = check([file.as_os_str()]);
    assert_eq!(run.code, Some(0), "stdout:\n{}", run.stdout);
}

#[test]
fn a_rule_configured_off_produces_no_finding_at_all() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("unused.p");
    write(&file, UNUSED);
    write(
        &tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.lint]\nunused-variable = \"off\"\n",
    );

    let run = check([Path::new("--json").as_os_str(), file.as_os_str()]);

    assert_eq!(run.code, Some(0));
    let v = run.json();
    assert!(
        v["diagnostics"].as_array().unwrap().is_empty(),
        "an `off` rule must not appear as a suppressed-but-present entry, got:\n{}",
        run.stdout
    );
}

#[test]
fn schema_flag_drives_schema_backed_diagnostics() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("src").join("cust.p");
    // A known table, an unknown field on it: LINT0003 only fires under a schema.
    write(
        &file,
        "FIND FIRST Customer.\nDISPLAY Customer.NoSuchField.\n",
    );
    let schema_dir = tmp.path().join("schema");
    write(
        &schema_dir.join("s.df"),
        "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n",
    );

    // No schema: the rule stays silent, so the file passes the gate.
    let without = check([file.as_os_str()]);
    assert!(
        !without.stdout.contains("LINT0003"),
        "unknown-table-or-field needs a schema, got:\n{}",
        without.stdout
    );

    let with = check([
        Path::new("--schema").as_os_str(),
        schema_dir.as_os_str(),
        file.as_os_str(),
    ]);
    assert_eq!(with.code, Some(1), "stdout:\n{}", with.stdout);
    assert!(
        with.stdout.contains("LINT0003"),
        "expected the schema-backed rule to fire, got:\n{}",
        with.stdout
    );
}

/// A `--schema` directory that matches no `.df` file is a misconfiguration, and
/// the gate says so instead of turning every table reference into a finding (A2).
///
/// Loading it as an empty-but-present schema made the whole tree light up:
/// `undefined-symbol` on each table plus `unknown-table-or-field` on each field,
/// with nothing explaining why. The most likely cause is a typo in the flag, so
/// the useful answer is one warning naming it — not a hundred findings about
/// correct code.
#[test]
fn a_schema_dir_with_no_df_files_warns_instead_of_flooding_findings() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("src").join("cust.p");
    write(&file, "FIND FIRST Customer.\nDISPLAY Customer.CustNum.\n");
    let empty_dir = tmp.path().join("schema");
    fs::create_dir_all(&empty_dir).unwrap();

    let run = check([
        Path::new("--schema").as_os_str(),
        empty_dir.as_os_str(),
        file.as_os_str(),
    ]);

    assert!(
        run.stderr.contains("no .df files"),
        "expected a warning naming the empty schema directory, got:\n{}",
        run.stderr
    );
    assert!(
        !run.stdout.contains("LINT0001"),
        "a schema that did not load must not make every table undefined, got:\n{}",
        run.stdout
    );
    assert!(
        !run.stdout.contains("LINT0003"),
        "nor every field unknown, got:\n{}",
        run.stdout
    );
    assert_eq!(
        run.code,
        Some(0),
        "the file itself is clean. stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
}

// ---------------------------------------------------------------------------
// Preprocessing is on by default (R19)
// ---------------------------------------------------------------------------

/// The default configuration expands includes. Pinned on its own because the
/// default is the only configuration most users ever run, and getting it wrong
/// is not a subtle failure: a gate that cannot see what an include declares
/// reports every reference to it as `undefined-symbol`, so the whole file
/// becomes noise about the caller's own correct code. It is also the
/// configuration the language server always runs (R19), so a divergence here
/// makes the gate and the editor disagree on any project that uses an include.
///
/// No preprocessing flag is passed — that is the point of the test.
#[test]
fn preprocessing_is_on_by_default_so_include_declared_symbols_resolve() {
    let tmp = TempDir::new().unwrap();
    let includes = tmp.path().join("inc");
    // The include declares the variable; the root file writes and reads it, so
    // nothing but a missing expansion could produce a finding here.
    write(
        &includes.join("decls.i"),
        "DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n",
    );
    let file = tmp.path().join("main.p");
    write(&file, "{decls.i}\nASSIGN v-count = 1.\nMESSAGE v-count.\n");

    let run = check([
        Path::new("-I").as_os_str(),
        includes.as_os_str(),
        file.as_os_str(),
    ]);

    assert_eq!(
        run.code,
        Some(0),
        "the default must expand includes. stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
    assert!(
        !run.stdout.contains("LINT0001"),
        "an include-declared symbol must not read as undefined, got:\n{}",
        run.stdout
    );
}

/// The escape hatch still works, and shows what the default is protecting
/// against: with expansion skipped, the same file reports its include-declared
/// symbol as undefined at every reference.
#[test]
fn no_preprocess_skips_expansion_and_the_symbol_goes_undefined() {
    let tmp = TempDir::new().unwrap();
    let includes = tmp.path().join("inc");
    write(
        &includes.join("decls.i"),
        "DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n",
    );
    let file = tmp.path().join("main.p");
    write(&file, "{decls.i}\nASSIGN v-count = 1.\nMESSAGE v-count.\n");

    let run = check([
        Path::new("--no-preprocess").as_os_str(),
        Path::new("-I").as_os_str(),
        includes.as_os_str(),
        file.as_os_str(),
    ]);

    assert_eq!(run.code, Some(1), "stdout:\n{}", run.stdout);
    assert!(
        run.stdout.contains("LINT0001"),
        "expected undefined-symbol without expansion, got:\n{}",
        run.stdout
    );
}

/// `--preprocess` is accepted and ignored: an invocation written against the
/// flag's earlier opt-in form keeps working and keeps meaning the same thing.
#[test]
fn the_legacy_preprocess_flag_is_accepted_and_changes_nothing() {
    let tmp = TempDir::new().unwrap();
    let includes = tmp.path().join("inc");
    write(
        &includes.join("decls.i"),
        "DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n",
    );
    let file = tmp.path().join("main.p");
    write(&file, "{decls.i}\nASSIGN v-count = 1.\nMESSAGE v-count.\n");

    let run = check([
        Path::new("--preprocess").as_os_str(),
        Path::new("-I").as_os_str(),
        includes.as_os_str(),
        file.as_os_str(),
    ]);

    assert_eq!(
        run.code,
        Some(0),
        "stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
}

// ---------------------------------------------------------------------------
// Coverage channels: loud, but never a gate (R26)
// ---------------------------------------------------------------------------

#[test]
fn an_unresolvable_include_surfaces_preproc007_without_failing_the_gate() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("main.p");
    // The include elides to nothing and the rest still parses, so PREPROC007 is
    // the only signal — one honest diagnostic at the true cause.
    write(&file, "{globals.i}\nMESSAGE \"hi\".\n");

    let run = check([Path::new("--preprocess").as_os_str(), file.as_os_str()]);

    assert!(
        run.stderr.contains("PREPROC007"),
        "expected the loud unresolvable-include warning, got:\n{}",
        run.stderr
    );
    assert!(run.stderr.contains("globals.i"), "got:\n{}", run.stderr);
    assert_eq!(
        run.code,
        Some(0),
        "a coverage warning is not a finding. stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );

    let json = check([
        Path::new("--json").as_os_str(),
        Path::new("--preprocess").as_os_str(),
        file.as_os_str(),
    ])
    .json();
    assert!(
        json["preproc"]
            .as_array()
            .unwrap()
            .iter()
            .any(|d| d["code"] == "PREPROC007"),
        "preproc diagnostics keep their own key, got:\n{json}"
    );
    assert!(
        json["diagnostics"].as_array().unwrap().is_empty(),
        "and stay out of the findings channel, got:\n{json}"
    );
}

/// Each `preproc` entry names the file it came from (D1).
///
/// Without a path, N files failing to resolve an include produced N entries a
/// machine consumer could not tell apart — and the whole value of the channel is
/// knowing *where* coverage was lost. The entries use the same shape as
/// `diagnostics`, built by the same helper, so one deserializer serves both keys.
#[test]
fn preproc_entries_are_attributed_to_the_file_they_came_from() {
    let tmp = TempDir::new().unwrap();
    write(
        &tmp.path().join("a-one.p"),
        "{missing-one.i}\nMESSAGE \"a\".\n",
    );
    write(
        &tmp.path().join("z-two.p"),
        "{missing-two.i}\nMESSAGE \"z\".\n",
    );

    let json = check([Path::new("--json").as_os_str(), tmp.path().as_os_str()]).json();

    let entries = json["preproc"].as_array().expect("preproc array");
    assert_eq!(entries.len(), 2, "one per file, got:\n{json}");
    let mut paths: Vec<&str> = entries
        .iter()
        .map(|d| d["path"].as_str().expect("each entry carries its path"))
        .collect();
    paths.sort_unstable();
    assert!(
        paths[0].ends_with("a-one.p") && paths[1].ends_with("z-two.p"),
        "the two entries must be distinguishable, got {paths:?} in:\n{json}"
    );
    for entry in entries {
        assert_eq!(entry["code"], "PREPROC007");
        assert_eq!(
            entry["source"], "preproc",
            "same shape as a diagnostics entry, got:\n{json}"
        );
    }
}

#[test]
fn a_partly_unjudged_file_reports_the_count_and_still_exits_0() {
    let (_tmp, file) = project("unj.p", UNJUDGED);

    let run = check([file.as_os_str()]);

    assert_eq!(
        run.code,
        Some(0),
        "the coverage note must never move the exit code (R26). stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
    assert!(
        run.stderr.contains("could not be fully checked"),
        "expected the coverage note on stderr, got:\n{}",
        run.stderr
    );

    let json = check([Path::new("--json").as_os_str(), file.as_os_str()]).json();
    assert_eq!(
        json["unjudged_symbols"], 1,
        "the count is a scalar for machine consumers, got:\n{json}"
    );
}

// ---------------------------------------------------------------------------
// Per-file failures never abort the walk (R24)
// ---------------------------------------------------------------------------

/// The panic is injected, not provoked: `oxabl_common`'s test-only
/// `test-panics` feature (on through this crate's dev-dependencies, off in any
/// real build) makes the guarded analyze site panic when the source carries the
/// marker in a comment. No ABL input panics today.
#[test]
fn an_analysis_panic_is_reported_and_the_walk_continues() {
    let tmp = TempDir::new().unwrap();
    // `a-` / `z-` prefixes so the panicking file is neither first nor last in
    // the walker's sorted order: the files on both sides must still be linted.
    write(&tmp.path().join("a-first.p"), UNUSED);
    write(
        &tmp.path().join("m-panics.p"),
        &format!(
            "/* OXABL-TEST-PANIC:{} */\nMESSAGE \"hi\".\n",
            panic_sites::ANALYZE
        ),
    );
    write(&tmp.path().join("z-last.p"), UNUSED);

    let run = check([tmp.path().as_os_str()]);

    assert_eq!(
        run.code,
        Some(1),
        "a contained panic fails the gate — but with 1, never `analyze`'s 4 \
         (R24). stderr:\n{}",
        run.stderr
    );
    assert!(
        run.stderr.contains("analysis failed"),
        "expected a readable per-file failure report, got:\n{}",
        run.stderr
    );
    assert!(
        run.stderr.contains("m-panics.p"),
        "the failure must name the file, got:\n{}",
        run.stderr
    );
    // The walk kept going in both directions.
    assert!(
        run.stdout.contains("a-first.p") && run.stdout.contains("z-last.p"),
        "the remaining files must still be linted, got:\n{}",
        run.stdout
    );

    let json = check([Path::new("--json").as_os_str(), tmp.path().as_os_str()]).json();
    let failures = json["failures"].as_array().unwrap();
    assert_eq!(failures.len(), 1, "got:\n{json}");
    assert!(
        failures[0]["path"]
            .as_str()
            .unwrap()
            .ends_with("m-panics.p"),
        "got:\n{json}"
    );
    assert_eq!(
        json["diagnostics"].as_array().unwrap().len(),
        2,
        "a per-file failure is its own key, and the other two files still \
         report findings, got:\n{json}"
    );
}

/// An unreadable file is reported and the walk continues (R24).
///
/// Unix-only and self-skipping: `chmod 000` does not stop a privileged process,
/// so if the file is still readable after the mode change this test says so and
/// stops rather than asserting something vacuous.
#[cfg(unix)]
#[test]
fn an_unreadable_file_is_reported_and_the_walk_continues() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = TempDir::new().unwrap();
    let unreadable = tmp.path().join("m-locked.p");
    write(&unreadable, CLEAN);
    write(&tmp.path().join("z-last.p"), UNUSED);
    fs::set_permissions(&unreadable, fs::Permissions::from_mode(0o000)).unwrap();

    if fs::read_to_string(&unreadable).is_ok() {
        eprintln!(
            "skipping: this process can read a 0o000 file (running privileged), \
             so there is no unreadable-file case to observe"
        );
        return;
    }

    let run = check([tmp.path().as_os_str()]);

    assert_eq!(run.code, Some(1), "stderr:\n{}", run.stderr);
    assert!(
        run.stderr.contains("cannot read") && run.stderr.contains("m-locked.p"),
        "expected the unreadable file to be named, got:\n{}",
        run.stderr
    );
    assert!(
        run.stdout.contains("z-last.p") && run.stdout.contains("LINT0002"),
        "the rest of the walk must still be checked, got:\n{}",
        run.stdout
    );
}

// ---------------------------------------------------------------------------
// Cross-file resolution through the walk
// ---------------------------------------------------------------------------

/// A parent class with one public method. Synthetic, written for this file.
const CALC_BASE: &str = "CLASS orders.calc-base:\n\
                         METHOD PUBLIC INTEGER calc-total():\n\
                         RETURN 0.\n\
                         END METHOD.\n\
                         END CLASS.\n";

/// A subclass calling the inherited method. Without cross-file resolution the
/// call reads as an undefined symbol — the false positive the walk's index
/// removes.
const CALC_CHILD: &str = "CLASS orders.child INHERITS orders.calc-base:\n\
                          METHOD PUBLIC VOID run-it():\n\
                          DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
                          v-total = calc-total().\n\
                          MESSAGE v-total.\n\
                          END METHOD.\n\
                          END CLASS.\n";

/// A two-class project: the parent where a qualified name maps it, the child
/// beside it. `--no-format` throughout, so the lint channel is the only variable
/// (these fixtures are not written to the formatter's taste).
fn inheritance_project() -> TempDir {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("orders/calc-base.cls"), CALC_BASE);
    write(&tmp.path().join("orders/child.cls"), CALC_CHILD);
    tmp
}

#[test]
fn a_walk_with_a_search_path_resolves_an_inherited_member() {
    let tmp = inheritance_project();

    let run = check([
        Path::new("--no-format").as_os_str(),
        Path::new("-I").as_os_str(),
        tmp.path().as_os_str(),
        tmp.path().as_os_str(),
    ]);

    assert_eq!(
        run.code,
        Some(0),
        "the inherited call must resolve. stdout:\n{}\nstderr:\n{}",
        run.stdout,
        run.stderr
    );
    assert!(
        !run.stdout.contains("LINT0001"),
        "a parent's method is not an undefined symbol, got:\n{}",
        run.stdout
    );
}

/// The control, and the shape of every workspace that configures no search path:
/// nothing is reachable, so the walk answers exactly as it did before cross-file
/// resolution existed — and the report keys are the same either way.
#[test]
fn the_same_walk_with_no_search_path_keeps_todays_answer_and_json_shape() {
    let tmp = inheritance_project();

    let run = check([
        Path::new("--no-format").as_os_str(),
        Path::new("--json").as_os_str(),
        tmp.path().as_os_str(),
    ]);

    assert_eq!(run.code, Some(1), "stderr:\n{}", run.stderr);
    let v = run.json();
    assert_eq!(v["version"], 1);
    assert_eq!(v["files_checked"], 2);
    assert_eq!(v["lint_enabled"], true);
    assert_eq!(v["format_enabled"], false);
    assert_eq!(v["format"]["drifted_count"], 0);
    assert!(v["failures"].as_array().unwrap().is_empty());
    let diagnostics = v["diagnostics"].as_array().expect("diagnostics array");
    assert!(
        diagnostics.iter().any(|d| d["code"] == "LINT0001"),
        "with nowhere to search, the inherited call is undefined as before:\n{}",
        run.stdout
    );
}
