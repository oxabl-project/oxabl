//! A root that expands to itself is lexed once for its analysis and its format
//! check, and every answer is the one that the stages give on their own.
//!
//! The lex counters belong to the process, so each test takes `COUNTERS`. All
//! sources are synthetic.

use std::path::Path;
use std::sync::{Arc, Mutex};

use oxabl_lexer::lex_counts;
use oxabl_pipeline::{
    ExpandedUnit, FormatOutcome, FormatPipeline, LazyRawUnit, LintPipeline, LintResult,
    PipelineConfig, RawUnit,
};
use oxabl_style::StyleGuide;
use oxabl_workspace::InMemoryFileSystem;

static COUNTERS: Mutex<()> = Mutex::new(());

/// A root with an unused variable, comments and drift.
fn source() -> String {
    let mut text =
        String::from("/* header comment */\nDEFINE VARIABLE iUnused AS INTEGER NO-UNDO.\n");
    for n in 0..40 {
        text.push_str(&format!(
            "DEFINE VARIABLE iCount{n} AS INTEGER NO-UNDO.\nDO iCount{n} = 1 TO 10:\nMESSAGE iCount{n}. // note\nEND.\n"
        ));
    }
    text
}

fn bytes_lexed() -> usize {
    lex_counts().1
}

/// Every finding of a result, as text, in order.
fn findings(result: &LintResult) -> Vec<String> {
    result.all().map(|d| format!("{d:?}")).collect()
}

fn run_plain(config: &PipelineConfig, fs: &InMemoryFileSystem, text: &str) -> LintResult {
    let run = LintPipeline::new(config, fs);
    run.with_file(Path::new("/ws/root.p")).run(text)
}

#[test]
fn an_identity_root_is_lexed_once_for_both_stages_and_the_answers_do_not_change() {
    let _guard = COUNTERS.lock().unwrap_or_else(|e| e.into_inner());
    let config = PipelineConfig::default();
    let fs = InMemoryFileSystem::new();
    let format = FormatPipeline::new(StyleGuide::default_base());
    // Already formatted: a candidate that differs from the text is a new text,
    // and is lexed too.
    let text = {
        let drifted = source();
        format
            .format(&drifted)
            .output()
            .expect("the fixture drifts")
            .to_string()
    };
    assert_eq!(format.format(&text), FormatOutcome::Unchanged);

    let before = bytes_lexed();
    let plain = run_plain(&config, &fs, &text);
    let plain_format = format.format(&text);
    let separate = bytes_lexed() - before;

    let before = bytes_lexed();
    let raw = LazyRawUnit::new(&text);
    let run = LintPipeline::new(&config, &fs);
    let (shared, _) = run.with_file(Path::new("/ws/root.p")).run_with_unit(&raw);
    let shared_format = format.format_unit(raw.made().expect("the analysis made the unit"));
    let together = bytes_lexed() - before;

    assert!(!findings(&plain).is_empty(), "the fixture has findings");
    assert_eq!(findings(&shared), findings(&plain));
    assert_eq!(shared_format, plain_format);
    assert_eq!(shared_format, FormatOutcome::Unchanged);
    assert!(
        separate >= 2 * text.len(),
        "the separate stages lex the root twice: {separate} bytes of {}",
        text.len()
    );
    assert!(
        together < text.len() + text.len() / 4,
        "the stages lexed the root more than once: {together} bytes of {}",
        text.len()
    );
}

#[test]
fn a_root_with_an_include_does_not_make_the_unit_for_its_analysis() {
    let _guard = COUNTERS.lock().unwrap_or_else(|e| e.into_inner());
    let mut fs = InMemoryFileSystem::new();
    let part = "DEFINE VARIABLE iFromInclude AS INTEGER NO-UNDO.\n".repeat(40);
    fs.insert("/ws/part.i".into(), part.as_str());
    let config = PipelineConfig {
        include_paths: vec!["/ws".into()],
        ..PipelineConfig::default()
    };
    let text = "{part.i}\nMESSAGE \"done\".\n";

    let plain = run_plain(&config, &fs, text);
    let raw = LazyRawUnit::new(text);
    let run = LintPipeline::new(&config, &fs);
    let before = bytes_lexed();
    let (shared, _) = run.with_file(Path::new("/ws/root.p")).run_with_unit(&raw);
    let lexed = bytes_lexed() - before;

    assert_eq!(findings(&shared), findings(&plain));
    assert!(
        raw.made().is_none(),
        "the unit is not made for an expansion"
    );
    // The expanded text is lexed by the analysis. The raw text is not: nothing
    // asked for the unit, so it was not made.
    assert!(
        lexed >= part.len() && lexed < part.len() + text.len(),
        "the analysis lexed {lexed} bytes; the expansion is {}",
        part.len() + text.len()
    );
}

#[test]
fn a_unit_serves_only_the_text_that_it_was_made_from() {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(Path::new("/ws/root.p"));
    let text = "DEFINE VARIABLE iOne AS INTEGER NO-UNDO.\n";
    let expansion = handle.expand(text);
    let expanded = expansion.expanded().expect("a clean expansion");
    assert!(expanded.is_identity());

    assert!(RawUnit::parse(text).serves(expanded));
    // Same length and a different byte.
    let other = "DEFINE VARIABLE iTwo AS INTEGER NO-UNDO.\n";
    assert_eq!(other.len(), text.len());
    assert!(!RawUnit::parse(other).serves(expanded));
}

#[test]
fn format_through_a_unit_is_the_format_of_the_text() {
    let format = FormatPipeline::new(StyleGuide::default_base());
    for text in [
        "IF TRUE THEN DO:\n    MESSAGE \"hi\".\nEND.\n",
        "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n",
        "IF THEN.",
        "",
        "/* only a comment */\n",
    ] {
        let unit = RawUnit::parse(text);
        assert_eq!(format.format_unit(&unit), format.format(text), "{text:?}");
    }
}

#[test]
fn a_unit_makes_its_line_table_once_and_lazily() {
    let unit = RawUnit::parse("a.\nb.\n");
    let first: *const _ = unit.source_map();
    let second: *const _ = unit.source_map();
    assert_eq!(first, second);
    assert_eq!(unit.source_map().lookup(3), (2, 1));
}

#[test]
fn an_expanded_unit_holds_the_tokens_and_the_tree_of_the_expanded_text() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        "/ws/part.i".into(),
        "DEFINE VARIABLE iPart AS INTEGER NO-UNDO.\n",
    );
    let config = PipelineConfig {
        include_paths: vec!["/ws".into()],
        ..PipelineConfig::default()
    };
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(Path::new("/ws/root.p"));
    let expansion = handle.expand("{part.i}\nMESSAGE iPart.\n");
    let expanded = Arc::new(expansion.expanded().expect("a clean expansion").clone());
    assert!(!expanded.is_identity());

    let unit = ExpandedUnit::parse(Arc::clone(&expanded));
    assert_eq!(unit.expanded().text, expanded.text);
    let last = unit.tokens().last().expect("an end token");
    assert_eq!(last.end as usize, expanded.text.len());
    assert!(unit.program().errors.is_empty());
    assert_eq!(unit.program().statements.len(), 2);
    let lines = expanded.text.matches('\n').count() + 1;
    assert_eq!(unit.source_map().lookup(expanded.text.len()).0, lines);
}
