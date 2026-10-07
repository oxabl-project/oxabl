//! A TAB inside a quoted literal is expanded by the ABL compiler to the next
//! 8-column stop, measured from the start of the *source line*. A line holding
//! one therefore keeps its column: moving it, or changing the width of anything
//! before the literal, would change the compiled value.

use oxabl_formatter::format;
use oxabl_lexer::{Kind, tokenize};
use oxabl_parser::{Parser, Program};
use oxabl_style::{IndentStyle, StyleGuide};

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

/// Column modulo 8 of every TAB inside a string literal, in source order.
fn tab_stops(src: &str) -> Vec<usize> {
    let mut out = Vec::new();
    for t in tokenize(src) {
        if t.kind != Kind::StringLiteral {
            continue;
        }
        for (i, b) in src[t.start..t.end].bytes().enumerate() {
            if b == b'\t' {
                let at = t.start + i;
                let line_start = src[..at].rfind('\n').map_or(0, |p| p + 1);
                out.push((at - line_start) % 8);
            }
        }
    }
    out
}

fn styles() -> Vec<(&'static str, StyleGuide)> {
    let mut tabs = StyleGuide::default_base();
    tabs.indent_style = IndentStyle::Tabs;
    let mut three = StyleGuide::default_base();
    three.indent_size = 3;
    vec![
        ("default_base", StyleGuide::default_base()),
        ("oestandards", StyleGuide::oestandards()),
        ("indent3", three),
        ("tabs", tabs),
    ]
}

fn assert_value_preserved(src: &str) -> Vec<(&'static str, String)> {
    assert!(parse(src).is_ok(), "fixture must parse: {src:?}");
    let before = tab_stops(src);
    assert!(!before.is_empty(), "fixture has no TAB literal: {src:?}");
    let mut outs = Vec::new();
    for (name, style) in styles() {
        let out = format(src, &parse(src), &style)
            .unwrap_or_else(|e| panic!("{name}: bailed on {src:?}: {e}"));
        assert_eq!(before, tab_stops(&out), "{name}: TAB moved in {src:?}");
        let again = format(&out, &parse(&out), &style).unwrap();
        assert_eq!(out, again, "{name}: not idempotent for {src:?}");
        outs.push((name, out));
    }
    outs
}

#[test]
fn double_quoted_literal_line_is_not_reindented() {
    for (name, out) in assert_value_preserved("DO:\nc = \"a\tb\".\nEND.\n") {
        assert!(out.contains("\nc = \"a\tb\".\n"), "{name}: {out:?}");
    }
}

#[test]
fn single_quoted_literal_line_is_not_reindented() {
    for (name, out) in assert_value_preserved("DO:\nc = 'a\tb'.\nEND.\n") {
        assert!(out.contains("\nc = 'a\tb'.\n"), "{name}: {out:?}");
    }
}

#[test]
fn over_indented_literal_line_keeps_its_indent() {
    for (name, out) in assert_value_preserved("DO:\n     c = \"a\tb\".\nEND.\n") {
        assert!(out.contains("\n     c = \"a\tb\".\n"), "{name}: {out:?}");
    }
}

#[test]
fn neighbouring_lines_are_still_reindented() {
    let src = "DO:\nx = 1.\nc = \"a\tb\".\ny = 2.\nEND.\n";
    let out = format(src, &parse(src), &StyleGuide::default_base()).unwrap();
    assert_eq!(out, "DO:\n    x = 1.\nc = \"a\tb\".\n    y = 2.\nEND.\n");
}

#[test]
fn first_line_of_multiline_literal_keeps_its_indent() {
    assert_value_preserved("DO:\nc = \"first\tline\nsecond\tline\".\nEND.\n");
}

#[test]
fn tab_on_a_later_line_of_a_multiline_literal_is_untouched() {
    let src = "DO:\nc = \"first\nsecond\tline\".\nEND.\n";
    for (name, out) in assert_value_preserved(src) {
        assert!(out.contains("\nsecond\tline\".\n"), "{name}: {out:?}");
    }
}

#[test]
fn literal_later_on_the_line() {
    assert_value_preserved("DO:\nMESSAGE \"plain\" + \"a\tb\".\nEND.\n");
}

#[test]
fn closing_line_of_multiline_literal_followed_by_tab_literal() {
    assert_value_preserved("DO:\nc = \"one\ntwo\" + \"a\tb\".\nEND.\n");
}

#[test]
fn width_changing_keyword_edit_before_the_literal_is_skipped() {
    // Expanding `def`/`var`/`char` would shift the TAB.
    let mut style = StyleGuide::oestandards();
    style.keyword_abbreviation = oxabl_style::KeywordAbbreviation::AbbreviateNothing;
    let control = "def var c as char init \"ab\".\n";
    let expanded = format(control, &parse(control), &style).unwrap();
    assert_ne!(
        expanded, control,
        "control: expansion applies without a TAB"
    );

    let src = "def var c as char init \"a\tb\".\n";
    let out = format(src, &parse(src), &style).unwrap();
    assert_eq!(tab_stops(src), tab_stops(&out));
    let again = format(&out, &parse(&out), &style).unwrap();
    assert_eq!(out, again);
}

#[test]
fn width_preserving_keyword_edit_still_applies() {
    let src = "message \"a\tb\".\n";
    let out = format(src, &parse(src), &StyleGuide::oestandards()).unwrap();
    assert_eq!(out, "MESSAGE \"a\tb\".\n");
}

#[test]
fn end_with_type_is_skipped_before_a_tab_literal_on_the_end_line() {
    let src = "PROCEDURE p:\nEND. MESSAGE \"a\tb\".\n";
    assert_value_preserved(src);
}

#[test]
fn shift_by_a_whole_tab_stop_is_not_special_cased() {
    // The line is simply left alone, so no shift happens at all.
    let src = "DO:\n        c = \"a\tb\".\nEND.\n";
    for (name, out) in assert_value_preserved(src) {
        assert!(out.contains("\n        c = \"a\tb\".\n"), "{name}: {out:?}");
    }
}

#[test]
fn continuation_lines_of_a_tab_line_statement_do_not_drift() {
    // The first line stays put, so the continuation lines must not shift by the
    // delta it would have taken.
    for src in [
        "DO:\nMESSAGE \"a\tb\"\n   \"c\".\nEND.\n",
        "DO:\nIF TRUE THEN MESSAGE \"a\tb\"\n   \"c\".\nEND.\n",
        "DO:\nc = \"a\tb\" +\n\"c\".\nEND.\n",
    ] {
        assert_value_preserved(src);
    }
}
