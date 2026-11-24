//! Folding and selection ranges through the real `oxabl lsp` over stdio.
//! All ABL fixtures are synthetic.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::Session;

fn folding(session: &mut Session, path: &Path) -> Vec<Value> {
    let answer = session.request(
        "textDocument/foldingRange",
        json!({"textDocument": {"uri": session::file_uri(path)}}),
    );
    assert!(answer.get("error").is_none(), "folding failed: {answer}");
    let folds = answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("folding gave no list: {answer}"))
        .clone();
    // VS Code keeps only the first range that starts on a line, so a second
    // one is a fold the editor drops.
    let mut lines: Vec<&Value> = folds.iter().map(|fold| &fold["startLine"]).collect();
    lines.dedup();
    assert_eq!(
        lines.len(),
        folds.len(),
        "two folds start on one line: {folds:#?}"
    );
    folds
}

fn selection(session: &mut Session, path: &Path, line: u32, character: u32) -> Value {
    let answer = session.request(
        "textDocument/selectionRange",
        json!({
            "textDocument": {"uri": session::file_uri(path)},
            "positions": [{"line": line, "character": character}]
        }),
    );
    assert!(answer.get("error").is_none(), "selection failed: {answer}");
    answer["result"][0].clone()
}

fn has_fold(folds: &[Value], start: u32, end: u32, kind: Option<&str>) -> bool {
    folds.iter().any(|fold| {
        fold["startLine"] == json!(start)
            && fold["endLine"] == json!(end)
            && fold["kind"].as_str() == kind
    })
}

fn chain_of(mut selection: Value) -> Vec<Value> {
    let mut out = Vec::new();
    loop {
        let parent = selection["parent"].clone();
        out.push(selection["range"].clone());
        if parent.is_null() {
            return out;
        }
        selection = parent;
    }
}

fn position_of(source: &str, needle: &str) -> (u32, u32) {
    let offset = source
        .find(needle)
        .unwrap_or_else(|| panic!("no {needle:?}"));
    let before = &source[..offset];
    let line = before.bytes().filter(|byte| *byte == b'\n').count() as u32;
    let column = before.rsplit_once('\n').map_or(before, |(_, tail)| tail);
    (line, column.encode_utf16().count() as u32)
}

/// The position just after `needle`.
fn position_after(source: &str, needle: &str) -> (u32, u32) {
    let (line, character) = position_of(source, needle);
    let tail = needle.rsplit('\n').next().unwrap();
    if needle.contains('\n') {
        (
            line + needle.matches('\n').count() as u32,
            tail.encode_utf16().count() as u32,
        )
    } else {
        (line, character + needle.encode_utf16().count() as u32)
    }
}

fn byte_offset(source: &str, position: &Value) -> usize {
    let line = position["line"].as_u64().unwrap() as usize;
    let character = position["character"].as_u64().unwrap() as usize;
    let line_start = if line == 0 {
        0
    } else {
        source
            .match_indices('\n')
            .nth(line - 1)
            .map_or(0, |(at, _)| at + 1)
    };
    let tail = &source[line_start..];
    let mut units = 0;
    for (at, ch) in tail.char_indices() {
        if units == character {
            return line_start + at;
        }
        units += ch.len_utf16();
    }
    line_start + tail.len()
}

fn range_text<'a>(source: &'a str, range: &Value) -> &'a str {
    &source[byte_offset(source, &range["start"])..byte_offset(source, &range["end"])]
}

fn assert_containment(chain: &[Value]) {
    for pair in chain.windows(2) {
        let inner = &pair[0];
        let outer = &pair[1];
        let starts = (
            outer["start"]["line"].as_u64().unwrap(),
            outer["start"]["character"].as_u64().unwrap(),
        );
        let inner_start = (
            inner["start"]["line"].as_u64().unwrap(),
            inner["start"]["character"].as_u64().unwrap(),
        );
        let inner_end = (
            inner["end"]["line"].as_u64().unwrap(),
            inner["end"]["character"].as_u64().unwrap(),
        );
        let ends = (
            outer["end"]["line"].as_u64().unwrap(),
            outer["end"]["character"].as_u64().unwrap(),
        );
        assert!(
            starts <= inner_start && inner_end <= ends,
            "parent {outer} does not contain {inner}"
        );
    }
}

/// The folds as `(startLine, endLine, kind)`, sorted.
fn fold_lines(folds: &[Value]) -> Vec<(u64, u64, Option<String>)> {
    let mut lines: Vec<_> = folds
        .iter()
        .map(|fold| {
            (
                fold["startLine"].as_u64().unwrap(),
                fold["endLine"].as_u64().unwrap(),
                fold["kind"].as_str().map(str::to_string),
            )
        })
        .collect();
    lines.sort();
    lines
}

/// The indentation contradicts the structure on purpose: every fold comes
/// from the syntax, and a block keeps its `END` line visible.
#[test]
fn folds_every_block_kind_from_the_syntax_not_the_indentation() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("blocks.cls");
    let source = r#"CLASS C:
        METHOD PUBLIC VOID M():
DO:
            MESSAGE "do".
    END.
  REPEAT:
MESSAGE "repeat".
          END.
FOR EACH Customer:
    MESSAGE "for".
END.
      CASE x:
WHEN 1 THEN MESSAGE "case".
          OTHERWISE MESSAGE "other".
END CASE.
CATCH e AS Progress.Lang.Error:
                    MESSAGE "catch".
END CATCH.
  FINALLY:
MESSAGE "finally".
            END FINALLY.
    END METHOD.
            END CLASS.
IF a THEN DO:
x = 1.
END.
ELSE IF b THEN DO:
y = 1.
END.
ELSE DO:
z = 1.
END.
IF a THEN DO:
x = 1.
END. ELSE DO:
y = 1.
END.
FOR EACH Customer: /* walk every
customer once */
DISPLAY Customer.Name.
DISPLAY Customer.City.
END.
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    let expected: Vec<(u64, u64, Option<String>)> = vec![
        (0, 21, None),  // class
        (1, 20, None),  // method
        (2, 3, None),   // do
        (5, 6, None),   // repeat
        (8, 9, None),   // for each
        (11, 13, None), // case
        (15, 16, None), // catch
        (18, 19, None), // finally
        (23, 24, None), // IF ... THEN DO, not the whole IF
        (26, 27, None), // ELSE IF ... THEN DO
        (29, 30, None), // ELSE DO
        (32, 33, None), // IF ... THEN DO before `END. ELSE DO:`
        (34, 35, None), // that ELSE DO
        (37, 40, None), // FOR EACH, not the comment that starts on its line
    ];
    assert_eq!(fold_lines(&folds), expected, "{folds:#?}");
}

#[test]
fn folds_routines_definitions_and_long_statements() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("routines.p");
    let source = r#"PROCEDURE p:
MESSAGE "p".
END PROCEDURE.
FUNCTION f RETURNS INTEGER ():
RETURN 1.
END FUNCTION.
DEFINE TEMP-TABLE tt
FIELD id AS INTEGER
FIELD name AS CHARACTER.
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE VARIABLE b AS INTEGER NO-UNDO.
DEFINE QUERY q
FOR tt.
ASSIGN
a = 1
b = 2.
DEFINE VARIABLE c AS INTEGER NO-UNDO.
MESSAGE "one line".
VAR INTEGER m,
n.
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    let expected: Vec<(u64, u64, Option<String>)> = vec![
        (0, 1, None),   // procedure
        (3, 4, None),   // function
        (6, 8, None),   // temp-table with its fields
        (9, 12, None),  // the run of DEFINE statements after the temp-table
        (11, 12, None), // a long DEFINE
        (13, 15, None), // a long statement
        (18, 19, None), // a list of declarations, once
    ];
    assert_eq!(fold_lines(&folds), expected, "{folds:#?}");
}

#[test]
fn folds_comments_regions_analyze_and_every_preprocessor_branch() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("regions.p");
    let source = r#"/* first
second */
// #region named
MESSAGE "region".
// #endregion
&ANALYZE-SUSPEND _CREATE-WINDOW
MESSAGE "analyze".
&ANALYZE-RESUME
&IF DEFINED(A) &THEN
MESSAGE "active".
&ELSEIF DEFINED(B) &THEN
MESSAGE "inactive one".
&ELSE
MESSAGE "inactive two".
&IF DEFINED(C) &THEN
MESSAGE "nested".
&ENDIF
&ENDIF
x = 1 +
&IF DEFINED(D) &THEN
2
&ELSE
3
&ENDIF
.
// #endregion
/* one line */
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    let region = Some("region".to_string());
    let expected: Vec<(u64, u64, Option<String>)> = vec![
        (0, 1, Some("comment".to_string())), // block comment
        (2, 4, region.clone()),              // #region
        (5, 7, region.clone()),              // &ANALYZE-SUSPEND
        (8, 9, region.clone()),              // &IF
        (10, 11, region.clone()),            // &ELSEIF
        (12, 16, region.clone()),            // &ELSE
        (14, 15, region.clone()),            // nested &IF
        (18, 24, None),                      // the statement around a conditional
        (19, 20, region.clone()),            // &IF inside an expression
        (21, 22, region.clone()),            // its &ELSE
    ];
    assert_eq!(fold_lines(&folds), expected, "{folds:#?}");
}

#[test]
fn a_recovered_file_keeps_folds_for_the_parts_that_parse() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("recovered.p");
    let source = r#"PROCEDURE first:
DEFINE VARIABLE x AS INTEGER NO-UNDO.
END PROCEDURE.
PROCEDURE broken:
DEFINE VARIABLE = = .
MESSAGE ( ) ) .
IF THEN THEN.
END PROCEDURE.
PROCEDURE last:
REPEAT:
MESSAGE "last".
END.
END PROCEDURE.
"#;
    // A project file turns diagnostics on: the file must really fail to
    // parse, or this test proves nothing.
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"recovered\"\n",
    )
    .unwrap();
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let uri = json!(session::file_uri(&path));
    session.wait_for(|message| {
        message["method"] == json!("textDocument/publishDiagnostics")
            && message["params"]["uri"] == uri
            && message["params"]["diagnostics"]
                .as_array()
                .is_some_and(|list| list.iter().any(|d| d["code"] == json!("PARSE001")))
    });
    let folds = folding(&mut session, &path);
    assert!(has_fold(&folds, 0, 1, None), "{folds:#?}");
    assert!(has_fold(&folds, 8, 11, None), "{folds:#?}");
    assert!(has_fold(&folds, 9, 10, None), "{folds:#?}");
}

/// A client folds whole lines, so a range never ends before code on its
/// last line. A comment there may hide.
#[test]
fn a_fold_never_hides_code_on_its_last_line() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("tails.p");
    let source = r#"x = 1 +
&IF DEFINED(X) &THEN
2 &ELSE 3
&ENDIF
.
ASSIGN a = 1
b = 2
c = 3. MESSAGE "after".
/* a
*/ x = 1.
ASSIGN
a = 1
b = 2. /* note */
ASSIGN
a = 1
b = 2. // note
/* c
d */ // tail
x = 1.
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    let expected: Vec<(u64, u64, Option<String>)> = vec![
        (0, 4, None),   // the statement around the conditional; its branches are one line each
        (5, 6, None),   // ASSIGN stops before the line with `MESSAGE "after"`
        (10, 12, None), // a trailing comment may hide
        (13, 15, None), // a trailing `//` comment may hide too
        (16, 17, Some("comment".to_string())), // and does not take the block comment's line
    ];
    assert_eq!(fold_lines(&folds), expected, "{folds:#?}");
}

/// With `rangeLimit`, the outer folds stay, also at the end of the file.
#[test]
fn range_limit_keeps_the_outer_folds() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("limit.p");
    let mut source = String::new();
    for index in 0..10 {
        source.push_str(&format!(
            "PROCEDURE p{index}:\nDO:\nMESSAGE {index}.\nMESSAGE {index}.\nEND.\nEND PROCEDURE.\n"
        ));
    }
    let mut session = Session::start_with(
        root.path(),
        json!({"textDocument": {"foldingRange": {"rangeLimit": 12}}}),
    );
    session.open(&path, &source);
    let folds = folding(&mut session, &path);
    let mut expected: Vec<(u64, u64, Option<String>)> = Vec::new();
    for index in 0..10 {
        let start = index * 6;
        expected.push((start, start + 4, None)); // procedure
        if index < 2 {
            expected.push((start + 1, start + 3, None)); // DO
        }
    }
    expected.sort();
    assert_eq!(fold_lines(&folds), expected, "{folds:#?}");
}

#[test]
fn selection_at_the_end_of_a_word_or_in_a_comment() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("ends.p");
    let source = r#"FOR EACH Customer:
MESSAGE Customer.Name.
x = foo(a + bcd).
/* comment here */
END.
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let mut texts = |at: (u32, u32)| -> Vec<String> {
        let chain = chain_of(selection(&mut session, &path, at.0, at.1));
        assert_containment(&chain);
        chain
            .iter()
            .map(|range| range_text(source, range).to_string())
            .collect()
    };
    let name = texts(position_after(source, "Customer.Name"));
    assert_eq!(
        name[..3],
        ["Name", "Customer.Name", "MESSAGE Customer.Name."],
        "{name:#?}"
    );
    let bcd = texts(position_after(source, "a + bcd"));
    assert_eq!(
        bcd[..4],
        ["bcd", "a + bcd", "foo(a + bcd)", "x = foo(a + bcd)."],
        "{bcd:#?}"
    );
    let call = texts(position_after(source, "foo(a + bcd)"));
    assert_eq!(call[..2], [")", "foo(a + bcd)"], "{call:#?}");
    let comment = texts(position_of(source, "here"));
    assert_eq!(comment[0], "/* comment here */", "{comment:#?}");
    assert!(comment[1].starts_with("FOR EACH"), "{comment:#?}");
}

#[test]
fn selection_grows_from_a_nested_expression_to_the_file() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("selection.p");
    let source = r#"DEFINE VARIABLE before AS INTEGER NO-UNDO.
PROCEDURE p:
FOR EACH Customer WHERE Customer.CustNum > (1 + 2) * 3:
MESSAGE Customer.Name.
END.
END PROCEDURE.
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    // Two positions in one request: one answer each, in order.
    let positions: Vec<Value> = ["1 + 2", "Name"]
        .iter()
        .map(|needle| {
            let (line, character) = position_of(source, needle);
            json!({"line": line, "character": character})
        })
        .collect();
    let answer = session.request(
        "textDocument/selectionRange",
        json!({"textDocument": {"uri": session::file_uri(&path)}, "positions": positions}),
    );
    let answers = answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("selection gave no list: {answer}"));
    assert_eq!(answers.len(), 2, "{answer}");
    let texts: Vec<Vec<&str>> = answers
        .iter()
        .map(|answer| {
            let chain = chain_of(answer.clone());
            assert_containment(&chain);
            chain
                .iter()
                .map(|range| range_text(source, range))
                .collect()
        })
        .collect();
    assert_eq!(
        texts[0],
        [
            "1",
            "(1 + 2)",
            "(1 + 2) * 3",
            "Customer.CustNum > (1 + 2) * 3",
            "FOR EACH Customer WHERE Customer.CustNum > (1 + 2) * 3:\nMESSAGE Customer.Name.\nEND.",
            "PROCEDURE p:\nFOR EACH Customer WHERE Customer.CustNum > (1 + 2) * 3:\nMESSAGE Customer.Name.\nEND.\nEND PROCEDURE.",
            source,
        ],
        "unexpected selection chain in the expression"
    );
    assert_eq!(
        texts[1],
        [
            "Name",
            "Customer.Name",
            "MESSAGE Customer.Name.",
            "FOR EACH Customer WHERE Customer.CustNum > (1 + 2) * 3:\nMESSAGE Customer.Name.\nEND.",
            "PROCEDURE p:\nFOR EACH Customer WHERE Customer.CustNum > (1 + 2) * 3:\nMESSAGE Customer.Name.\nEND.\nEND PROCEDURE.",
            source,
        ],
        "unexpected selection chain in the block body"
    );
}

#[test]
fn crlf_and_utf16_positions_map_to_the_source() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("unicode.p");
    let source = "/* 😀 comment\r\nsecond 😀 line\r\n*/\r\nPROCEDURE p:\r\n  ASSIGN\r\n    x = \"😀\" + \"é\"\r\n    y = 2.\r\nEND PROCEDURE.\r\n";
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    let comment = folds
        .iter()
        .find(|fold| fold["kind"] == json!("comment"))
        .unwrap_or_else(|| panic!("no comment fold: {folds:#?}"));
    assert_eq!(comment["startLine"], json!(0));
    assert_eq!(comment["endLine"], json!(2));
    assert_eq!(comment["endCharacter"], json!(2));
    assert!(has_fold(&folds, 3, 6, None), "{folds:#?}");
    assert!(has_fold(&folds, 4, 6, None), "{folds:#?}");

    // LSP counts a lone CR as a line break too.
    let cr = root.path().join("cr.p");
    session.open(&cr, "PROCEDURE p:\r  x = 1.\r  y = 2.\rEND PROCEDURE.\r");
    let cr_folds = folding(&mut session, &cr);
    assert_eq!(fold_lines(&cr_folds), [(0, 2, None)], "{cr_folds:#?}");

    let (line, character) = position_of(source, "é");
    let chain = chain_of(selection(&mut session, &path, line, character));
    assert_containment(&chain);
    assert_eq!(chain[0]["start"], json!({"line": 5, "character": 15}));
    assert_eq!(chain[0]["end"], json!({"line": 5, "character": 18}));
    assert_eq!(range_text(source, &chain[0]), "\"é\"");
    assert_eq!(range_text(source, &chain[1]), "\"😀\" + \"é\"");
}

#[test]
fn preprocessor_text_maps_to_raw_source_and_include_text_stays_out() {
    let root = tempfile::tempdir().unwrap();
    let include = root.path().join("inc.i");
    std::fs::write(
        &include,
        "PROCEDURE included:\nREPEAT:\nMESSAGE \"include\".\nEND.\nEND PROCEDURE.\n",
    )
    .unwrap();
    let path = root.path().join("root.p");
    let source = r#"&IF DEFINED(X) &THEN
MESSAGE "one".
&ELSE
MESSAGE "two".
&ENDIF
{inc.i}
&SCOPED-DEFINE greeting "hi"
MESSAGE {&greeting} "x".
"#;
    let mut session = Session::start(root.path());
    session.open(&path, source);
    let folds = folding(&mut session, &path);
    assert_eq!(folds.len(), 2, "only the two raw branches fold: {folds:#?}");
    assert!(has_fold(&folds, 0, 1, Some("region")));
    assert!(has_fold(&folds, 2, 3, Some("region")));

    let (line, character) = position_of(source, "two");
    let chain = chain_of(selection(&mut session, &path, line, character));
    assert_containment(&chain);
    let text: Vec<&str> = chain
        .iter()
        .map(|range| range_text(source, range))
        .collect();
    assert!(text.contains(&"MESSAGE \"two\"."), "{text:#?}");
    assert!(
        text.contains(&"&IF DEFINED(X) &THEN\nMESSAGE \"one\".\n&ELSE\nMESSAGE \"two\".\n&ENDIF")
    );
    assert!(
        !text.iter().any(|text| text.contains("included")),
        "{text:#?}"
    );

    // A preprocessor reference selects as its source text, not its value.
    let (line, character) = position_of(source, "greeting}");
    let reference = chain_of(selection(&mut session, &path, line, character));
    assert_containment(&reference);
    assert_eq!(
        range_text(source, &reference[0]),
        "{&greeting}",
        "{reference:#?}"
    );
    let texts: Vec<&str> = reference
        .iter()
        .map(|range| range_text(source, range))
        .collect();
    assert!(texts.contains(&"MESSAGE {&greeting} \"x\"."), "{texts:#?}");
}

#[test]
fn a_deep_selection_chain_stays_within_the_client_json_limit() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("deep.p");
    let mut source = String::new();
    for index in 0..70 {
        source.push_str(&format!("l{index}: DO:\n"));
    }
    source.push_str("MESSAGE \"bottom\".\n");
    source.push_str(&"END.\n".repeat(70));
    let (line, character) = position_of(&source, "bottom");
    let mut session = Session::start(root.path());
    session.open(&path, &source);
    let chain = chain_of(selection(&mut session, &path, line, character));
    assert_eq!(chain.len(), 32, "{chain:#?}");
    assert_containment(&chain);
    assert_eq!(range_text(&source, chain.last().unwrap()), source);
}

/// A nest just under the parser's limits answers both requests: the walk
/// recurses once for each level.
#[test]
fn a_nest_under_the_parser_limits_answers_both_requests() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("nest.p");
    let source = format!(
        "{}x = 1{}.\n{}",
        "IF TRUE THEN DO:\n".repeat(250),
        " + 1".repeat(4000),
        "END.\n".repeat(250)
    );
    let (line, character) = position_of(&source, "x = 1");
    let mut session = Session::start(root.path());
    session.open(&path, &source);
    let folds = folding(&mut session, &path);
    assert!(folds.len() >= 250, "{}", folds.len());
    let chain = chain_of(selection(&mut session, &path, line, character));
    assert_eq!(chain.len(), 32);
    assert_containment(&chain);
}

/// One line of many statements costs about as much as the same statements on
/// many lines: no step of the pass scans the line once per statement.
#[test]
fn a_long_line_costs_no_more_than_many_lines() {
    let root = tempfile::tempdir().unwrap();
    let statements = 150_000;
    let mut session = Session::start(root.path());
    let mut times = Vec::new();
    for (name, separator) in [("lines.p", "\n"), ("line.p", " ")] {
        let path = root.path().join(name);
        let mut source = String::from("y = 1.");
        for _ in 1..statements {
            source.push_str(separator);
            source.push_str("y = 1.");
        }
        source.push('\n');
        session.open(&path, &source);
        let started = Instant::now();
        let folds = folding(&mut session, &path);
        times.push(started.elapsed());
        assert!(folds.is_empty(), "{} folds", folds.len());
    }
    let (many_lines, one_line) = (times[0], times[1]);
    eprintln!("{statements} statements: on many lines {many_lines:?}, on one line {one_line:?}");
    assert!(
        one_line < many_lines * 3 + Duration::from_secs(2),
        "one line took {one_line:?}, many lines {many_lines:?}"
    );
}

/// The time a debug build may take for one request on the large file. A walk
/// that is quadratic in the file size takes minutes.
const LARGE_FILE_LIMIT: Duration = Duration::from_secs(20);

#[test]
fn a_large_file_answers_both_requests() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("large.p");
    let mut source = String::new();
    let count = 3000;
    for index in 0..count {
        source.push_str(&format!(
            "/* procedure {index}\n   of the large file */\nPROCEDURE p{index}:\nDEFINE VARIABLE a AS INTEGER NO-UNDO.\nDEFINE VARIABLE b AS INTEGER NO-UNDO.\nFOR EACH Customer WHERE Customer.CustNum > ({index} + 2) * 3:\nASSIGN\na = {index}\nb = 2.\nEND.\nEND PROCEDURE.\n"
        ));
    }
    let (line, character) = position_of(&source, &format!("{} + 2", count - 1));
    let mut session = Session::start(root.path());
    session.open(&path, &source);
    let started = Instant::now();
    let folds = folding(&mut session, &path);
    let folding_time = started.elapsed();
    // A comment, a procedure, a DEFINE run, a FOR EACH and an ASSIGN each.
    assert_eq!(folds.len(), 5 * count, "{}", folds.len());
    let started = Instant::now();
    let chain = chain_of(selection(&mut session, &path, line, character));
    let selection_time = started.elapsed();
    assert_containment(&chain);
    assert_eq!(range_text(&source, chain.last().unwrap()), source);
    eprintln!(
        "large file ({} lines, {} KB): folding {folding_time:?}, then selection {selection_time:?}",
        source.lines().count(),
        source.len() / 1024
    );
    assert!(
        folding_time < LARGE_FILE_LIMIT,
        "folding took {folding_time:?}"
    );
    assert!(
        selection_time < LARGE_FILE_LIMIT,
        "selection took {selection_time:?}"
    );
}
