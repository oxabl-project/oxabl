//! Range formatting property tests.
//!
//! Over every synthetic fixture and a set of awkward sources, each also with its
//! indentation stripped, with an uneven indentation and with a constant
//! indentation added, many ranges are formatted under two styles. For each
//! range:
//!
//! * applying the edits and then formatting the whole file gives the same text
//!   as formatting the whole file at once;
//! * the edits stay inside the lines from the first to the last top-level
//!   statement or comment the range overlaps;
//! * the first line of each statement the range overlaps, at any depth, is the
//!   line that the whole-file format gives (so the edits are not short);
//! * a range that is the whole file gives the whole-file format.
//!
//! The ranges are every span of whole lines, every byte as an empty range, a
//! fixed pseudo-random set of byte spans, and pairs of those as two ranges.
//!
//! All sources are synthetic ABL.

use std::fs;
use std::ops::Range;
use std::path::PathBuf;

use oxabl_ast::{OnAction, OnKind, Span, Statement, StatementKind};
use oxabl_formatter::{RangeBail, RangeEdit, format_ranges, try_format_source};
use oxabl_lexer::{Kind, tokenize};
use oxabl_parser::{Parser, Program};
use oxabl_style::StyleGuide;

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

fn whole(src: &str, style: &StyleGuide) -> Option<String> {
    try_format_source(src, style).ok()
}

fn apply(src: &str, edits: &[RangeEdit]) -> String {
    let mut out = String::new();
    let mut at = 0;
    for edit in edits {
        assert!(
            at <= edit.start && edit.start <= edit.end,
            "edits overlap or are out of order"
        );
        out.push_str(&src[at..edit.start]);
        out.push_str(&edit.new_text);
        at = edit.end;
    }
    out.push_str(&src[at..]);
    out
}

fn fixtures() -> Vec<(String, String)> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut out = Vec::new();
    for entry in fs::read_dir(root.join("tests/fixtures")).expect("fixtures dir") {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) == Some("p") {
            let name = path.file_name().unwrap().to_string_lossy().into_owned();
            out.push((name, fs::read_to_string(&path).unwrap()));
        }
    }
    out.sort();
    assert!(!out.is_empty(), "no fixtures found");
    for (name, src) in AWKWARD {
        out.push((name.to_string(), src.to_string()));
    }
    out
}

const AWKWARD: &[(&str, &str)] = &[
    (
        "nested-blocks",
        "DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\n\n\nPROCEDURE p:\nDEFINE INPUT PARAMETER n AS INTEGER.\n\n\nDO i = 1 TO n:\nIF i MODULO 2 = 0 THEN DO:\nMESSAGE \"even\".\n/* keep */\nEND.\nELSE\nMESSAGE \"odd\".\n\nREPEAT:\ni = i + 1.\nLEAVE.\nEND.\nEND.\nEND PROCEDURE.\n\nMESSAGE \"done\".\n",
    ),
    (
        "case-and-wrapped",
        "CASE x:\nWHEN 1 THEN\nMESSAGE \"one\".\nWHEN 2 THEN DO:\nMESSAGE \"two\".\nMESSAGE\n\"wrapped\"\n+ \"line\".\nEND.\nOTHERWISE\nMESSAGE \"other\".\nEND CASE.\n",
    ),
    (
        "comments",
        "/* header */\n  // line\nDO:\n    /* own line */\nMESSAGE \"a\". /* trailing */\n  /* multi\n  line */\nMESSAGE \"b\".\n// last\nEND.\n/* tail */\n",
    ),
    (
        "class",
        "CLASS Foo:\nDEFINE PUBLIC PROPERTY Bar AS INTEGER NO-UNDO\nGET():\nRETURN 1.\nEND GET.\nSET(v AS INTEGER):\nMESSAGE v.\nEND SET.\n\nMETHOD PUBLIC VOID Run():\nDEFINE VARIABLE n AS INTEGER NO-UNDO.\nIF n > 0 THEN\nMESSAGE \"x\".\nEND METHOD.\n\nCONSTRUCTOR PUBLIC Foo():\nEND CONSTRUCTOR.\nEND CLASS.\n",
    ),
    (
        "preprocessor",
        "{inc.i &a=1}\n&SCOPED-DEFINE foo 1\n&IF DEFINED(foo) &THEN\nMESSAGE \"a\".\nDO:\nMESSAGE {&foo}.\nEND.\n&ELSE\nMESSAGE \"b\".\n&ENDIF\nPROCEDURE p:\n{inc2.i}\nMESSAGE \"x\".\nEND.\n",
    ),
    (
        "same-line",
        "DO: MESSAGE \"a\". MESSAGE \"b\". END.\nIF TRUE THEN MESSAGE \"c\". ELSE MESSAGE \"d\".\nMESSAGE \"e\". MESSAGE \"f\".\n",
    ),
    (
        "multiline-string",
        "DO:\nMESSAGE \"first\n   second\n\n   third\".\nMESSAGE \"after\".\nEND.\n",
    ),
    (
        "tab-in-string",
        "DO:\nMESSAGE \"a\tb\".\n    MESSAGE \"c\".\nEND.\n",
    ),
    (
        "crlf",
        "DO:\r\nMESSAGE \"a\".\r\n\r\n\r\n\r\n  IF TRUE THEN\r\nMESSAGE \"b\".\r\nEND.\r\n",
    ),
    (
        "non-ascii",
        "/* héllo 😀 */\nDO:\nMESSAGE \"日本語\". /* ünï */\nEND.\n",
    ),
    ("no-final-newline", "DO:\nMESSAGE \"a\".\nEND."),
    (
        "else-chain",
        "IF a THEN DO:\nMESSAGE \"1\".\nEND.\nELSE IF b AND\nc THEN DO:\nMESSAGE \"2\".\nEND.\nELSE DO:\nMESSAGE \"3\".\nEND.\nIF d THEN IF e THEN\nMESSAGE \"4\".\nELSE\nMESSAGE \"5\".\n",
    ),
    (
        "triggers-and-labels",
        "ON CHOOSE OF btn DO:\nMESSAGE \"x\".\nEND.\nouter: DO i = 1 TO 3:\nFOR EACH Customer NO-LOCK\nWHERE Customer.Id = i:\nDISPLAY Customer.Name.\nEND.\nEND.\n",
    ),
    (
        "function",
        "FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER,\nINPUT b AS INTEGER):\nRETURN a + b.\nEND FUNCTION.\n\nMESSAGE f(1,\n2).\n",
    ),
    (
        "trailing-comment-continuation",
        "      DO:\n      x = 1. /* a\n            b */\n      y = 2.\n      END.",
    ),
    (
        "trailing-comment-after-end",
        "  DO:\n  DO:\n  x = 1.\n  END. /* a\n       b */\n  y = 2.\n  END.\n",
    ),
    ("only-blanks", "\n\n  \n"),
    ("empty", ""),
];

/// The source, with its indentation stripped, with an uneven one, and with six
/// spaces more on every line.
fn variants(name: &str, src: &str) -> Vec<(String, String)> {
    let flat: String = src
        .split_inclusive('\n')
        .map(|l| l.trim_start_matches([' ', '\t']))
        .collect();
    let uneven: String = src
        .split_inclusive('\n')
        .enumerate()
        .map(|(i, l)| {
            format!(
                "{}{}",
                " ".repeat(i * 7 % 5),
                l.trim_start_matches([' ', '\t'])
            )
        })
        .collect();
    let indented: String = src
        .split_inclusive('\n')
        .map(|l| format!("      {l}"))
        .collect();
    vec![
        (name.to_string(), src.to_string()),
        (format!("{name}/flat"), flat),
        (format!("{name}/uneven"), uneven),
        (format!("{name}/indented"), indented),
    ]
}

fn one(range: Range<usize>) -> Vec<Range<usize>> {
    std::iter::once(range).collect()
}

/// A small deterministic generator.
struct Lcg(u64);

impl Lcg {
    fn next(&mut self, below: usize) -> usize {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((self.0 >> 33) as usize) % below.max(1)
    }
}

fn line_starts(src: &str) -> Vec<usize> {
    let mut starts = vec![0];
    starts.extend(src.match_indices('\n').map(|(i, _)| i + 1));
    starts.retain(|&s| s < src.len() || s == 0);
    starts
}

fn boundary(src: &str, mut at: usize) -> usize {
    at = at.min(src.len());
    while !src.is_char_boundary(at) {
        at -= 1;
    }
    at
}

fn ranges_for(src: &str) -> Vec<Vec<Range<usize>>> {
    let mut out: Vec<Vec<Range<usize>>> = Vec::new();
    let starts = line_starts(src);
    let ends: Vec<usize> = starts
        .iter()
        .enumerate()
        .map(|(i, _)| starts.get(i + 1).copied().unwrap_or(src.len()))
        .collect();
    // Every span of whole lines (capped, so a long file keeps its cost).
    for i in 0..starts.len() {
        for end in ends[i..].iter().take(12) {
            out.push(one(starts[i]..*end));
        }
    }
    // Every byte as an empty range.
    for at in 0..=src.len() {
        if src.is_char_boundary(at) {
            out.push(one(at..at));
        }
    }
    // Random byte spans, and pairs of them.
    let mut rng = Lcg(0x9E3779B97F4A7C15 ^ src.len() as u64);
    let random = |rng: &mut Lcg| {
        let a = boundary(src, rng.next(src.len() + 1));
        let b = boundary(src, a + rng.next(60));
        a..b
    };
    for _ in 0..150 {
        let range = random(&mut rng);
        out.push(one(range.clone()));
        out.push(vec![range, random(&mut rng)]);
    }
    out.push(one(0..src.len()));
    out
}

/// The line extent (bytes) from the first to the last top-level statement or
/// comment a range overlaps.
fn allowed(src: &str, program: &Program, ranges: &[Range<usize>]) -> Vec<Range<usize>> {
    let starts = line_starts(src);
    let line_of = |off: usize| starts.partition_point(|&s| s <= off).saturating_sub(1);
    let extent = |s: usize, e: usize| {
        let first = line_of(s);
        let last = line_of(e.saturating_sub(1));
        starts[first]..starts.get(last + 1).copied().unwrap_or(src.len())
    };
    let mut out = Vec::new();
    for r in ranges {
        let (rs, re) = if r.start >= r.end {
            // A point after the final line feed is on no line with content.
            if r.start >= src.len() && src.ends_with('\n') {
                continue;
            }
            let l = line_of(r.start);
            (starts[l], starts.get(l + 1).copied().unwrap_or(src.len()))
        } else {
            (r.start, r.end)
        };
        let touched = program
            .statements
            .iter()
            .map(|s| (s.span.start as usize, s.span.end as usize))
            .chain(
                program
                    .comments
                    .iter()
                    .map(|c| (c.span.start as usize, c.span.end as usize)),
            )
            .filter(|&(s, e)| s < re && rs < e)
            .map(|(s, e)| extent(s, e));
        // Blank lines between two touched things belong to the range.
        let (mut from, mut to) = (usize::MAX, 0);
        for e in touched {
            from = from.min(e.start);
            to = to.max(e.end);
        }
        if from <= to {
            out.push(from..to);
        }
    }
    out
}

/// The statements a body holds, as the printer nests them. A function's
/// parameters belong to its header.
fn children(kind: &StatementKind) -> Vec<&Statement> {
    let mut out: Vec<&Statement> = Vec::new();
    match kind {
        StatementKind::Block(body)
        | StatementKind::Do { body, .. }
        | StatementKind::Repeat { body, .. }
        | StatementKind::ForEach { body, .. }
        | StatementKind::Procedure { body, .. }
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Catch { body, .. }
        | StatementKind::Finally { body }
        | StatementKind::Destructor { body }
        | StatementKind::Method { body, .. }
        | StatementKind::Constructor { body, .. } => out.extend(body),
        StatementKind::Function { body, .. } => out.extend(
            body.iter()
                .skip_while(|s| matches!(s.kind, StatementKind::DefineParameter { .. })),
        ),
        StatementKind::Property {
            get_body, set_body, ..
        } => out.extend(get_body.iter().chain(set_body.iter()).flatten()),
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            out.push(then_branch);
            out.extend(else_branch.as_deref());
        }
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            out.extend(when_branches.iter().flat_map(|w| &w.body));
            out.extend(otherwise.iter().flatten());
        }
        StatementKind::Label { body, .. } => out.push(body),
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. },
        } => {
            if let OnAction::Block(stmt) = action {
                out.push(stmt);
            }
        }
        StatementKind::PreprocIf(p) => {
            out.extend(&p.then_branch);
            out.extend(p.elseif_branches.iter().flat_map(|(_, b)| b));
            out.extend(p.else_branch.iter().flatten());
        }
        _ => {}
    }
    out.retain(|s| s.span != Span::DUMMY);
    out.sort_by_key(|s| s.span.start);
    out
}

/// Every statement, at any depth, in source order.
fn all_statements(program: &Program) -> Vec<&Statement> {
    fn walk<'a>(stmt: &'a Statement, out: &mut Vec<&'a Statement>) {
        out.push(stmt);
        for child in children(&stmt.kind) {
            walk(child, out);
        }
    }
    let mut out = Vec::new();
    for stmt in program.statements.iter().filter(|s| s.span != Span::DUMMY) {
        walk(stmt, &mut out);
    }
    out
}

/// The lines of `text` from the first line of `stmt` to its last, as
/// `(content, tail_of_outside_comment)` for each line with content. Blank lines
/// are left out: the formatter may delete or clamp them. A tail line of a
/// comment that starts before the statement is flagged: its indent follows the
/// comment's anchor, which is outside the statement.
fn content_lines<'a>(
    text: &'a str,
    stmt: &Statement,
    comments: &[(usize, usize)],
) -> Vec<(&'a str, bool)> {
    let first = text[..stmt.span.start as usize]
        .rfind('\n')
        .map_or(0, |i| i + 1);
    let last = (stmt.span.end as usize).saturating_sub(1).max(first);
    let end = text[last..].find('\n').map_or(text.len(), |i| last + i);
    let mut out = Vec::new();
    let mut at = first;
    for line in text[first..end].split('\n') {
        let outside = comments
            .iter()
            .any(|&(cs, ce)| cs < stmt.span.start as usize && cs < at && at < ce);
        let line = line.trim_end_matches('\r');
        if !line.trim().is_empty() {
            out.push((line, outside));
        }
        at += line.len() + 1 + usize::from(text.as_bytes().get(at + line.len()) == Some(&b'\r'));
    }
    out
}

/// The indexes (in [`all_statements`] order) of statements a range surely
/// touches: those inside it, and the innermost ones it overlaps. A block that
/// the range only reaches into is not one of them, because a range inside a
/// body touches only the statements of that body. An empty range stands for its
/// line.
fn overlapped(
    src: &str,
    code: &[(usize, usize)],
    statements: &[&Statement],
    ranges: &[Range<usize>],
) -> Vec<usize> {
    let mut out = Vec::new();
    for r in ranges {
        let (rs, re) = if r.start >= r.end {
            let at = r.start.min(src.len());
            let start = src[..at].rfind('\n').map_or(0, |i| i + 1);
            let end = src[at..].find('\n').map_or(src.len(), |i| at + i);
            (start, end)
        } else {
            (r.start, r.end)
        };
        // White space and comments in a body touch no statement of it: keep
        // the code the range covers.
        let from = code.partition_point(|t| t.1 <= rs);
        let to = code.partition_point(|t| t.0 < re);
        let (Some(first), Some(last)) = (
            code[from..to.max(from)].first(),
            code[from..to.max(from)].last(),
        ) else {
            continue;
        };
        let (rs, re) = (rs.max(first.0), re.min(last.1));
        let hits = |s: &Statement| (s.span.start as usize) < re && rs < s.span.end as usize;
        for (i, s) in statements.iter().enumerate() {
            let inside = rs <= s.span.start as usize && s.span.end as usize <= re;
            let innermost = !children(&s.kind).into_iter().any(hits);
            if hits(s) && (inside || innermost) {
                out.push(i);
            }
        }
    }
    out
}

#[test]
fn a_range_then_the_whole_file_is_the_whole_file() {
    check_style("default_base", &StyleGuide::default_base());
}

#[test]
fn a_range_then_the_whole_file_is_the_whole_file_under_oestandards() {
    check_style("oestandards", &StyleGuide::oestandards());
}

fn check_style(style_name: &str, style: &StyleGuide) {
    let mut checked = 0usize;
    let mut with_edits = 0usize;
    let mut starts_checked = 0usize;
    for (name, src) in fixtures() {
        for (label, text) in variants(&name, &src) {
            let label = format!("{style_name}:{label}");
            let style = style.clone();
            let program = parse(&text);
            let Some(expected) = whole(&text, &style) else {
                // A refused file refuses every range, with its reason.
                let result = format_ranges(&text, &program, &style, &one(0..text.len()));
                assert!(
                    matches!(result, Err(RangeBail::File(_))),
                    "{label}: a file the formatter refuses must refuse a range, got {result:?}"
                );
                continue;
            };
            // An empty range after the final line feed touches nothing.
            if text.ends_with('\n') {
                let at = text.len();
                assert_eq!(
                    format_ranges(&text, &program, &style, &one(at..at)),
                    Err(RangeBail::NoStatement),
                    "{label}: an empty range at the end of the file"
                );
            }
            let expected_program = parse(&expected);
            let expected_statements = all_statements(&expected_program);
            let statements = all_statements(&program);
            // The code tokens (no comments), as byte spans.
            let code: Vec<(usize, usize)> = tokenize(&text)
                .iter()
                .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof))
                .map(|t| (t.start as usize, t.end as usize))
                .collect();
            let comment_spans: Vec<(usize, usize)> = program
                .comments
                .iter()
                .map(|c| (c.span.start as usize, c.span.end as usize))
                .collect();
            for ranges in ranges_for(&text) {
                let edits = match format_ranges(&text, &program, &style, &ranges) {
                    Ok(edits) => edits,
                    Err(RangeBail::NoStatement) => {
                        let missed = overlapped(&text, &code, &statements, &ranges);
                        assert!(
                            missed.is_empty(),
                            "{label} {ranges:?}: no statement, but the range covers code of statements {missed:?}"
                        );
                        continue;
                    }
                    Err(other) => panic!("{label} {ranges:?}: {other}"),
                };
                let applied = apply(&text, &edits);
                let again = whole(&applied, &style).unwrap_or_else(|| {
                    panic!("{label} {ranges:?}: the file no longer formats after the edits")
                });
                assert_eq!(
                    again, expected,
                    "{label} {ranges:?}: range, then whole file, differs from the whole file\nedits: {edits:?}"
                );

                let allowed = allowed(&text, &program, &ranges);
                for edit in &edits {
                    assert!(
                        allowed
                            .iter()
                            .any(|a| a.start <= edit.start && edit.end <= a.end),
                        "{label} {ranges:?}: edit {edit:?} is outside the touched lines {allowed:?}"
                    );
                }

                // Completeness: every content line of every statement the range
                // touches is the whole-file line, but for the tail of a comment
                // that starts before the statement.
                let wanted = overlapped(&text, &code, &statements, &ranges);
                if !wanted.is_empty() {
                    let applied_program = parse(&applied);
                    let applied_statements = all_statements(&applied_program);
                    assert_eq!(applied_statements.len(), statements.len(), "{label}");
                    for i in wanted {
                        let before = content_lines(&text, statements[i], &comment_spans);
                        let got = content_lines(&applied, applied_statements[i], &[]);
                        let want = content_lines(&expected, expected_statements[i], &[]);
                        assert_eq!(
                            (got.len(), want.len()),
                            (before.len(), before.len()),
                            "{label} {ranges:?}: the content lines of a statement changed"
                        );
                        for ((got, want), (_, outside)) in got.iter().zip(&want).zip(&before) {
                            if !outside {
                                assert_eq!(
                                    got.0, want.0,
                                    "{label} {ranges:?}: a line of a touched statement is not formatted\nedits: {edits:?}"
                                );
                            }
                        }
                        starts_checked += 1;
                    }
                }

                checked += 1;
                with_edits += usize::from(!edits.is_empty());
            }
            // The whole file as the range is the whole-file format, except for
            // the blank lines before the first and after the last statement and
            // the final line ending, which belong to no statement.
            if let Ok(edits) = format_ranges(&text, &program, &style, &one(0..text.len())) {
                assert_eq!(
                    apply(&text, &edits).trim(),
                    expected.trim(),
                    "{label}: the whole file as a range"
                );
            }
        }
    }
    assert!(
        checked > 5_000,
        "{style_name}: only {checked} ranges were checked"
    );
    assert!(
        with_edits > 1_000,
        "{style_name}: only {with_edits} ranges had edits"
    );
    assert!(
        starts_checked > 10_000,
        "{style_name}: only {starts_checked} statement lines were checked"
    );
}

#[test]
fn the_fuzz_findings_hold_too() {
    let style = StyleGuide::default_base();
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fuzz/findings/format");
    let mut found = 0;
    for entry in fs::read_dir(&dir).expect("findings dir") {
        let path = entry.unwrap().path();
        let Ok(text) = fs::read_to_string(&path) else {
            continue;
        };
        found += 1;
        let program = parse(&text);
        let Some(expected) = whole(&text, &style) else {
            continue;
        };
        for ranges in ranges_for(&text) {
            let edits = match format_ranges(&text, &program, &style, &ranges) {
                Ok(edits) => edits,
                Err(RangeBail::NoStatement) => continue,
                Err(other) => panic!("{} {ranges:?}: {other}", path.display()),
            };
            let applied = apply(&text, &edits);
            assert_eq!(
                whole(&applied, &style).as_deref(),
                Some(expected.as_str()),
                "{} {ranges:?}",
                path.display()
            );
        }
    }
    assert!(found > 0, "no fuzz findings read");
}
