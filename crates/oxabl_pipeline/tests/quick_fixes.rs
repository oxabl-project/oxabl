//! The fixes of a finding through the shared pipeline: where they map to in
//! the root's own bytes, and every rule that drops them
//! (`docs/design/quick-fixes.md`). Synthetic sources only.

use std::path::PathBuf;

use oxabl_common::{Diagnostic, Edit, FileSpan, Fix, Severity};
use oxabl_pipeline::fix::place_fixes;
use oxabl_pipeline::{LintPipeline, LintResult, PipelineConfig, ROOT_FILE_ID, Span};
use oxabl_style::KeywordCase;
use oxabl_workspace::InMemoryFileSystem;

fn config() -> PipelineConfig {
    let mut config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w")],
        ..PipelineConfig::default()
    };
    config.lint_severities.set("TX006", Some(Severity::Warning));
    config
}

fn fs_of(files: &[(&str, &str)]) -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    for (path, content) in files {
        fs.insert(PathBuf::from(path), *content);
    }
    fs
}

fn run_at(files: &[(&str, &str)], path: &str, source: &str) -> LintResult {
    let fs = fs_of(files);
    let config = config();
    LintPipeline::new(&config, &fs)
        .with_branch_tracking(true)
        .with_file(path)
        .run(source)
}

fn run(files: &[(&str, &str)], source: &str) -> LintResult {
    run_at(files, "/w/main.p", source)
}

/// The fixes of the TX006 findings, as `(start, end, new_text)` per edit.
fn tx006_fixes(result: &LintResult) -> Vec<Vec<(u32, u32, String)>> {
    assert!(result.failure().is_none(), "{result:?}");
    let found: Vec<_> = result
        .all()
        .filter(|c| c.diagnostic.code.0 == "TX006")
        .collect();
    assert!(!found.is_empty(), "no TX006 finding: {result:?}");
    found
        .iter()
        .flat_map(|c| &c.diagnostic.fixes)
        .map(|fix| {
            fix.edits
                .iter()
                .map(|e| (e.span.span.start, e.span.span.end, e.new_text.clone()))
                .collect()
        })
        .collect()
}

/// `source` with the one edit of the one fix applied.
fn fixed(source: &str, result: &LintResult) -> String {
    let fixes = tx006_fixes(result);
    assert_eq!(fixes.len(), 1, "one fix: {fixes:?}");
    let (start, end, text) = &fixes[0][0];
    format!(
        "{}{}{}",
        &source[..*start as usize],
        text,
        &source[*end as usize..]
    )
}

#[test]
fn the_edit_is_in_the_root_bytes_after_a_directive() {
    let source = "&SCOPED-DEFINE NOTHING 1\nDEFINE VARIABLE i AS INTEGER.\nDISPLAY i.\n";
    let result = run(&[], source);
    assert_eq!(
        fixed(source, &result),
        "&SCOPED-DEFINE NOTHING 1\nDEFINE VARIABLE i AS INTEGER NO-UNDO.\nDISPLAY i.\n"
    );
}

#[test]
fn the_edit_follows_an_include_before_the_statement() {
    let source = "{a.i}\nDEFINE VARIABLE i AS INTEGER.\nDISPLAY i.\n";
    let result = run(&[("/w/a.i", "MESSAGE \"a\".\n")], source);
    assert_eq!(
        fixed(source, &result),
        "{a.i}\nDEFINE VARIABLE i AS INTEGER NO-UNDO.\nDISPLAY i.\n"
    );
}

#[test]
fn an_include_file_alone_gets_no_fix() {
    let source = "DEFINE VARIABLE i AS INTEGER.\nDISPLAY i.\n";
    let result = run_at(&[], "/w/defs.i", source);
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn a_finding_in_an_include_gets_no_fix() {
    let result = run(
        &[("/w/defs.i", "DEFINE VARIABLE i AS INTEGER.\n")],
        "{defs.i}\nDISPLAY i.\n",
    );
    let found: Vec<_> = result
        .include_origin()
        .iter()
        .filter(|d| d.diagnostic.code.0 == "TX006")
        .collect();
    assert_eq!(found.len(), 1, "{result:?}");
    assert!(found[0].diagnostic.fixes.is_empty());
    assert!(result.all().all(|c| c.diagnostic.fixes.is_empty()));
}

#[test]
fn a_type_from_a_substitution_gets_no_fix() {
    let result = run(
        &[],
        "&SCOPED-DEFINE T INTEGER\nDEFINE VARIABLE i AS {&T}.\nDISPLAY i.\n",
    );
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn an_include_in_the_statement_gets_no_fix() {
    let result = run(
        &[("/w/opts.i", "INITIAL 5")],
        "DEFINE VARIABLE i AS INTEGER {opts.i}.\nDISPLAY i.\n",
    );
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn an_empty_reference_in_the_statement_gets_no_fix() {
    let result = run(&[], "DEFINE VARIABLE i AS INTEGER {&EXTRA}.\nDISPLAY i.\n");
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn a_recovered_parse_gets_no_fix() {
    let result = run(&[], "DEFINE VARIABLE i AS INTEGER.\n@ @ @\nDISPLAY i.\n");
    assert!(result.all().any(|c| c.diagnostic.code.0 == "PARSE001"));
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn an_undecided_branch_gets_no_fix_and_a_decided_one_does() {
    let undecided =
        "&IF \"{&PROVERSION}\" = \"\" &THEN\nDEFINE VARIABLE i AS INTEGER.\n&ENDIF\nDISPLAY 1.\n";
    assert_eq!(tx006_fixes(&run(&[], undecided)), Vec::<Vec<_>>::new());
    let decided = "&IF 1 = 1 &THEN\nDEFINE VARIABLE i AS INTEGER.\n&ENDIF\nDISPLAY 1.\n";
    let result = run(&[], decided);
    assert_eq!(
        fixed(decided, &result),
        "&IF 1 = 1 &THEN\nDEFINE VARIABLE i AS INTEGER NO-UNDO.\n&ENDIF\nDISPLAY 1.\n"
    );
}

#[test]
fn an_expansion_without_branch_facts_gets_no_fix() {
    let fs = fs_of(&[]);
    let config = config();
    let result = LintPipeline::new(&config, &fs)
        .with_file("/w/main.p")
        .run("DEFINE VARIABLE i AS INTEGER.\nDISPLAY i.\n");
    assert_eq!(tx006_fixes(&result), Vec::<Vec<_>>::new());
}

#[test]
fn the_keyword_case_follows_the_statement() {
    let source = "define variable i as integer.\ndisplay i.\n";
    let result = run(&[], source);
    assert_eq!(
        fixed(source, &result),
        "define variable i as integer no-undo.\ndisplay i.\n"
    );
}

#[test]
fn the_keyword_case_follows_the_style() {
    let fs = fs_of(&[]);
    let mut config = config();
    config.style.keyword_case = KeywordCase::Lowercase;
    let source = "DEFINE VARIABLE i AS INTEGER.\nDISPLAY i.\n";
    let result = LintPipeline::new(&config, &fs)
        .with_branch_tracking(true)
        .with_file("/w/main.p")
        .run(source);
    assert_eq!(
        fixed(source, &result),
        "DEFINE VARIABLE i AS INTEGER no-undo.\nDISPLAY i.\n"
    );
}

fn finding_with(edit_text: &str, at: u32, statement: Span) -> Diagnostic {
    let span = |start, end| FileSpan {
        file: ROOT_FILE_ID,
        span: Span { start, end },
    };
    Diagnostic::warning("X", "x".to_string(), span(0, 1)).with_fix(Fix::new(
        "Fix it",
        Edit {
            span: span(at, at),
            new_text: edit_text.to_string(),
            statement: span(statement.start, statement.end),
        },
        true,
    ))
}

#[test]
fn a_fix_that_fails_the_token_guard_is_dropped() {
    let text = "DEFINE VARIABLE i AS INTEGER.\r\nDISPLAY i.\r\n";
    let statement = Span { start: 0, end: 29 };
    // Without its space, the keyword joins the type into one name.
    let mut joined = finding_with("NO-UNDO", 28, statement);
    let mut spaced = finding_with(" NO-UNDO", 28, statement);
    place_fixes(
        [&mut joined, &mut spaced].into_iter(),
        text,
        KeywordCase::Preserve,
    );
    assert!(joined.fixes.is_empty());
    assert_eq!(spaced.fixes.len(), 1);
}

#[test]
fn a_line_break_is_written_as_the_file_writes_it() {
    let text = "DISPLAY 1.\r\nDISPLAY 2.\r\n";
    let mut finding = finding_with("MESSAGE 3.\n", 12, Span { start: 12, end: 22 });
    place_fixes(std::iter::once(&mut finding), text, KeywordCase::Preserve);
    assert_eq!(finding.fixes[0].edits[0].new_text, "MESSAGE 3.\r\n");
}

#[test]
fn a_deferred_fix_with_no_edits_is_dropped() {
    let text = "DISPLAY 1.\n";
    let mut finding = finding_with("", 0, Span { start: 0, end: 10 });
    finding.fixes[0].edits.clear();
    finding.fixes[0].deferred = Some(oxabl_common::Deferred::AddUsing {
        name: "Thing".to_string(),
    });
    place_fixes(std::iter::once(&mut finding), text, KeywordCase::Preserve);
    assert!(finding.fixes.is_empty());
}
