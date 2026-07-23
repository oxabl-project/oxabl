//! Idempotency + stability property tests (U9 / R6.1 / R6.2 / S3).
//!
//! `format(format(x)) == format(x)` byte-for-byte over every synthetic fixture,
//! under both `default_base()` and the strict `oestandards()` preset.
//!
//! CC-1: every committed fixture is **synthetic** ABL — no corpus, no PII. The
//! full idempotency sweep over a large real-world ABL codebase is run **locally
//! against an out-of-repo path** and is never committed.

use std::fs;
use std::path::PathBuf;

use oxabl_formatter::format;
use oxabl_lexer::tokenize;
use oxabl_parser::{Parser, Program};
use oxabl_style::StyleGuide;

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

fn fixtures() -> Vec<(String, String)> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let mut out = Vec::new();
    for entry in fs::read_dir(&dir).expect("fixtures dir") {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) == Some("p") {
            let name = path.file_name().unwrap().to_string_lossy().into_owned();
            out.push((name, fs::read_to_string(&path).unwrap()));
        }
    }
    assert!(!out.is_empty(), "no fixtures found");
    out
}

fn presets() -> Vec<(&'static str, StyleGuide)> {
    vec![
        ("default_base", StyleGuide::default_base()),
        ("oestandards", StyleGuide::oestandards()),
    ]
}

#[test]
fn every_fixture_is_idempotent_under_both_presets() {
    for (name, src) in fixtures() {
        for (pname, style) in presets() {
            let program = parse(&src);
            assert!(
                program.is_ok(),
                "fixture {name} did not parse cleanly: {:?}",
                program.errors
            );
            let out1 = format(&src, &program, &style)
                .unwrap_or_else(|e| panic!("{name}/{pname}: first format bailed: {e}"));

            let program2 = parse(&out1);
            assert!(
                program2.is_ok(),
                "{name}/{pname}: formatted output did not re-parse: {:?}",
                program2.errors
            );
            let out2 = format(&out1, &program2, &style)
                .unwrap_or_else(|e| panic!("{name}/{pname}: second format bailed: {e}"));

            assert_eq!(out1, out2, "{name}/{pname}: not idempotent");
        }
    }
}

#[test]
fn oestandards_is_non_vacuous_end_with_type() {
    // Idempotency under a strict preset is only meaningful if `format` actually
    // succeeds (not silently bailing). Assert a bare-END procedure formats OK
    // AND gains its `END PROCEDURE` under oestandards (Fable finding 2).
    let (_, src) = fixtures()
        .into_iter()
        .find(|(n, _)| n == "procedure.p")
        .expect("procedure.p fixture");
    let program = parse(&src);
    let out = format(&src, &program, &StyleGuide::oestandards())
        .expect("oestandards must not bail on a bare-END procedure");
    assert!(
        out.contains("END PROCEDURE"),
        "end_with_type should have applied:\n{out}"
    );
}

#[test]
fn keyword_case_opt_in_is_idempotent() {
    let src = "def var i as int.\ndo:\nmessage i.\nend.\n";
    let mut style = StyleGuide::default_base();
    style.keyword_case = oxabl_style::KeywordCase::Uppercase;
    style.keyword_abbreviation = oxabl_style::KeywordAbbreviation::AbbreviateNothing;
    let p1 = parse(src);
    let out1 = format(src, &p1, &style).unwrap();
    let p2 = parse(&out1);
    let out2 = format(&out1, &p2, &style).unwrap();
    assert_eq!(out1, out2, "keyword opt-in must be idempotent");
    assert!(out1.contains("DEFINE VARIABLE"));
}

#[test]
fn bail_is_idempotent_on_parse_error() {
    // An unterminated block bails both passes with the same reason.
    let src = "DO:\n    MESSAGE \"x\".\n";
    let program = parse(src);
    assert!(!program.is_ok());
    let r1 = format(src, &program, &StyleGuide::default_base());
    let r2 = format(src, &program, &StyleGuide::default_base());
    assert!(r1.is_err());
    assert_eq!(r1, r2);
}

#[test]
fn wrapped_multiline_branch_is_idempotent() {
    // Issue #98: a wrapped multi-line non-block THEN branch (a multi-line
    // ASSIGN). `format` must be a fixpoint on its own output and never bail on
    // the semantic-preservation guard.
    let src =
        "IF AVAILABLE bar THEN\nASSIGN\nbar.qty = bar.qty + 1\nbar.total =\nbar.total + bar.qty.\n";
    for (pname, style) in presets() {
        let p1 = parse(src);
        assert!(p1.is_ok(), "fixture must parse: {:?}", p1.errors);
        let out1 = format(src, &p1, &style)
            .unwrap_or_else(|e| panic!("{pname}: first format bailed: {e}"));
        let p2 = parse(&out1);
        let out2 = format(&out1, &p2, &style)
            .unwrap_or_else(|e| panic!("{pname}: second format bailed: {e}"));
        assert_eq!(out1, out2, "{pname}: not idempotent");
    }
}

#[test]
fn multiline_token_shapes_are_idempotent() {
    // #95 shapes: a multi-line string literal and a multi-line `{include}`
    // reference, both inside an under-indented block. `format` must succeed
    // (not bail on the semantic guard) and be a fixpoint on its own output.
    //
    // Run under both presets. `oestandards()` turns keyword recasing on: the
    // protected interior of a multi-line token must survive recasing untouched
    // (it carries no transformable keyword sub-tokens), so recasing can never
    // start corrupting a string/include interior without this test failing.
    let cases = [
        "PROCEDURE p:\nmsg = \"first line\nsecond line\".\nEND.\n",
        "PROCEDURE p:\n{shared/report.i &event = \"start\"\n&mode = \"batch\"}\nEND.\n",
        "DO:\nmsg = \"line one\n\n\nline four\".\nEND.\n",
    ];
    for src in cases {
        for (pname, style) in presets() {
            let p1 = parse(src);
            assert!(p1.is_ok(), "fixture must parse: {:?}", p1.errors);
            let out1 = format(src, &p1, &style)
                .unwrap_or_else(|e| panic!("{pname}: first format bailed on {src:?}: {e}"));
            // The interior line (verbatim, no leading whitespace in these
            // fixtures) survives regardless of preset.
            let p2 = parse(&out1);
            let out2 = format(&out1, &p2, &style)
                .unwrap_or_else(|e| panic!("{pname}: second format bailed on {src:?}: {e}"));
            assert_eq!(out1, out2, "{pname}: not idempotent for {src:?}");
        }
    }
}
