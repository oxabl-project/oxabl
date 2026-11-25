//! Format a selection: the edits that give the statements a range touches the
//! layout they get in the whole file, and no others.
//!
//! The formatter lays out a whole file and works per source line, so a range is
//! not formatted on its own. [`format_ranges`] formats the whole file, then keeps
//! the output of the source lines that hold a touched statement (or an own-line
//! comment the range touches). The indentation of a line depends on the blocks
//! around it, so only the whole file can tell it. Applying the edits and then
//! formatting the whole file gives the same text as formatting the whole file
//! at once.
//!
//! # Which statements a range touches
//!
//! A statement is touched when the range overlaps it, so a range that starts or
//! ends inside a statement takes the whole statement. When the range lies inside
//! the body of one block, between its first and its last statement, only the
//! statements of that body that it overlaps are touched, at any depth. A range
//! that reaches the header or the end of a block touches the whole block.
//!
//! The edits stay inside the source lines of the touched statements. A line that
//! holds another statement as well, such as `IF x THEN MESSAGE "a".`, is
//! formatted as a whole line.

use std::fmt;
use std::ops::Range;

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_common::{InternalPanic, catch_panic};
use oxabl_parser::Program;
use oxabl_style::StyleGuide;

use crate::tree::{block_children, on_action};
use crate::{FormatBail, format_with_lines, guard};

/// One replacement of source bytes, in the coordinates of the source that was
/// formatted. Edits from one call do not overlap and are in source order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeEdit {
    pub start: usize,
    pub end: usize,
    pub new_text: String,
}

/// Why a range was left unformatted. On any of these the source stands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RangeBail {
    /// The whole file cannot be formatted, so no part of it can.
    File(FormatBail),
    /// The ranges touch no statement and no own-line comment: they hold only
    /// blank lines, a comment after code, or text between statements.
    NoStatement,
    /// The edits would have altered the token stream.
    SemanticGuardTripped,
}

impl fmt::Display for RangeBail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeBail::File(bail) => write!(f, "{bail}"),
            RangeBail::NoStatement => f.write_str(
                "the selection holds no statement and no comment on its own line; left unchanged",
            ),
            RangeBail::SemanticGuardTripped => f.write_str(
                "formatting the selection would have altered the token stream; left unchanged",
            ),
        }
    }
}

impl std::error::Error for RangeBail {}

/// The failure channel for [`try_format_ranges`]: a deliberate [`RangeBail`], or
/// an [`InternalPanic`] the guard contained.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum RangeFailure {
    Bail(RangeBail),
    Panic(InternalPanic),
}

impl fmt::Display for RangeFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeFailure::Bail(bail) => bail.fmt(f),
            RangeFailure::Panic(panic) => panic.fmt(f),
        }
    }
}

impl std::error::Error for RangeFailure {}

/// Parse `source` (raw, preprocessing off) and [`format_ranges`] it, containing
/// any internal panic.
pub fn try_format_ranges(
    source: &str,
    style: &StyleGuide,
    ranges: &[Range<usize>],
) -> Result<Vec<RangeEdit>, RangeFailure> {
    match catch_panic(|| {
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        format_ranges(source, &program, style, ranges)
    }) {
        Ok(Ok(edits)) => Ok(edits),
        Ok(Err(bail)) => Err(RangeFailure::Bail(bail)),
        Err(panic) => Err(RangeFailure::Panic(panic)),
    }
}

/// The edits that format the statements `ranges` touch (byte ranges of
/// `source`). An empty list means they already have their layout.
///
/// A range with no extent stands for the source line it is on. A range that
/// touches nothing is skipped when another touches something.
pub fn format_ranges(
    source: &str,
    program: &Program,
    style: &StyleGuide,
    ranges: &[Range<usize>],
) -> Result<Vec<RangeEdit>, RangeBail> {
    let formatted = format_with_lines(source, program, style).map_err(RangeBail::File)?;
    let lines = Lines::new(source);
    let tokens = oxabl_lexer::tokenize(source);
    let code = Code::new(&tokens, source.len());

    let top: Vec<&Statement> = program.statements.iter().collect();
    let mut spans: Vec<(usize, usize)> = Vec::new();
    for range in ranges {
        let (mut start, mut end) = (range.start.min(source.len()), range.end.min(source.len()));
        if start >= end {
            if source.is_empty() {
                continue;
            }
            let line = lines.index_of(start);
            (start, end) = (lines.starts[line], lines.content_end(source, line));
            if start >= end {
                continue;
            }
        }
        // The lines between the first and the last thing touched are the
        // range's own, blank lines included.
        let mut hull: Option<(usize, usize)> = None;
        let mut touch = |first: usize, last: usize| {
            hull = Some(hull.map_or((first, last), |(f, l)| (f.min(first), l.max(last))));
        };
        for stmt in select(top.clone(), start, end, &code) {
            touch(
                lines.index_of(stmt.span.start as usize),
                lines.index_of((stmt.span.end as usize).saturating_sub(1)),
            );
        }
        for comment in &program.comments {
            let (cs, ce) = (comment.span.start as usize, comment.span.end as usize);
            if cs < end && start < ce && lines.on_own_lines(source, cs, ce) {
                touch(lines.index_of(cs), lines.index_of(ce.saturating_sub(1)));
            }
        }
        spans.extend(hull);
    }
    if spans.is_empty() {
        return Err(RangeBail::NoStatement);
    }
    // A line moves with the first line of the statement that covers it, so a
    // statement that starts on a line of the group goes to its end.
    let mut reach = vec![0usize; lines.starts.len() - 1];
    for stmt in &program.statements {
        note_reach(stmt, &lines, &mut reach);
    }
    for (first, last) in &mut spans {
        let mut line = *first;
        while line <= *last {
            *last = (*last).max(reach[line]);
            line += 1;
        }
    }
    spans.sort_unstable();
    let mut merged: Vec<(usize, usize)> = Vec::new();
    for (first, last) in spans {
        match merged.last_mut() {
            Some(prev) if first <= prev.1 + 1 => prev.1 = prev.1.max(last),
            _ => merged.push((first, last)),
        }
    }

    let mut edits = Vec::new();
    for (first, last) in merged {
        let mut line = first;
        while line <= last {
            match formatted.lines[line] {
                None => {
                    // A blank line the formatter dropped. The ends of a group
                    // always hold code, so the line has a successor.
                    let mut upto = line;
                    while upto < last && formatted.lines[upto + 1].is_none() {
                        upto += 1;
                    }
                    edits.push(RangeEdit {
                        start: lines.starts[line],
                        end: lines.starts[upto + 1],
                        new_text: String::new(),
                    });
                    line = upto + 1;
                }
                Some((from, to)) => {
                    let old = &source[lines.starts[line]..lines.content_end(source, line)];
                    if let Some(edit) =
                        line_edit(lines.starts[line], old, &formatted.text[from..to])
                    {
                        edits.push(edit);
                    }
                    line += 1;
                }
            }
        }
    }
    if edits.is_empty() {
        return Ok(edits);
    }

    let mut candidate = String::with_capacity(source.len());
    let mut at = 0;
    for edit in &edits {
        candidate.push_str(&source[at..edit.start]);
        candidate.push_str(&edit.new_text);
        at = edit.end;
    }
    candidate.push_str(&source[at..]);
    if !guard::preserves_with_input_tokens(source, &tokens, &candidate) {
        return Err(RangeBail::SemanticGuardTripped);
    }
    Ok(edits)
}

/// The smallest edit that turns `old` (starting at byte `at`) into `new`.
fn line_edit(at: usize, old: &str, new: &str) -> Option<RangeEdit> {
    if old == new {
        return None;
    }
    let prefix = old
        .char_indices()
        .zip(new.chars())
        .take_while(|((_, a), b)| a == b)
        .last()
        .map_or(0, |((i, a), _)| i + a.len_utf8());
    let (old_rest, new_rest) = (&old[prefix..], &new[prefix..]);
    let suffix = old_rest
        .chars()
        .rev()
        .zip(new_rest.chars().rev())
        .take_while(|(a, b)| a == b)
        .map(|(a, _)| a.len_utf8())
        .sum::<usize>();
    Some(RangeEdit {
        start: at + prefix,
        end: at + old.len() - suffix,
        new_text: new_rest[..new_rest.len() - suffix].to_string(),
    })
}

/// The number of lines the printer splits `source` into: a final line without
/// a line feed counts, a final line feed does not start another.
pub(crate) fn source_line_count(source: &str) -> usize {
    if source.is_empty() {
        return 0;
    }
    source.bytes().filter(|b| *b == b'\n').count() + usize::from(!source.ends_with('\n'))
}

/// The line starts of a source, split at `\n` as the printer splits it.
struct Lines {
    /// One start per line, then the length of the source.
    starts: Vec<usize>,
}

impl Lines {
    fn new(source: &str) -> Lines {
        let mut starts = Vec::with_capacity(source_line_count(source) + 1);
        if !source.is_empty() {
            starts.push(0);
            starts.extend(
                source
                    .bytes()
                    .enumerate()
                    .filter(|(_, b)| *b == b'\n')
                    .map(|(i, _)| i + 1)
                    .filter(|&i| i < source.len()),
            );
        }
        starts.push(source.len());
        Lines { starts }
    }

    fn index_of(&self, offset: usize) -> usize {
        let count = self.starts.len() - 1;
        self.starts[..count]
            .partition_point(|&s| s <= offset)
            .saturating_sub(1)
    }

    /// The end of the line's content: before its line feed and the carriage
    /// returns that come before it.
    fn content_end(&self, source: &str, line: usize) -> usize {
        let bytes = source.as_bytes();
        let mut end = self.starts[line + 1];
        if end > self.starts[line] && bytes[end - 1] == b'\n' {
            end -= 1;
        }
        while end > self.starts[line] && bytes[end - 1] == b'\r' {
            end -= 1;
        }
        end
    }

    /// Does the span `start..end` have only white space before it on its first
    /// line and after it on its last line?
    fn on_own_lines(&self, source: &str, start: usize, end: usize) -> bool {
        let first = self.index_of(start);
        let last = self.index_of(end.saturating_sub(1));
        source[self.starts[first]..start]
            .bytes()
            .all(|b| b.is_ascii_whitespace())
            && source[end..self.starts[last + 1]]
                .bytes()
                .all(|b| b.is_ascii_whitespace())
    }
}

/// Record for each line the last line of the longest statement that starts on it.
fn note_reach(stmt: &Statement, lines: &Lines, reach: &mut [usize]) {
    if stmt.span == Span::DUMMY {
        return;
    }
    let first = lines.index_of(stmt.span.start as usize);
    let last = lines.index_of((stmt.span.end as usize).saturating_sub(1));
    reach[first] = reach[first].max(last);
    let children = match &stmt.kind {
        StatementKind::Function { body, .. } => body.iter().collect(),
        kind => child_lists(kind).into_iter().flatten().collect::<Vec<_>>(),
    };
    for child in children {
        note_reach(child, lines, reach);
    }
}

/// The positions of the code tokens (every token but comments).
struct Code {
    spans: Vec<(usize, usize)>,
    len: usize,
}

impl Code {
    fn new(tokens: &[oxabl_lexer::Token], len: usize) -> Code {
        Code {
            spans: tokens
                .iter()
                .filter(|t| {
                    t.kind != oxabl_lexer::Kind::Comment && t.kind != oxabl_lexer::Kind::Eof
                })
                .map(|t| (t.start, t.end))
                .collect(),
            len,
        }
    }

    /// The end of the last code token that starts before `at`, or 0.
    fn end_before(&self, at: usize) -> usize {
        let i = self.spans.partition_point(|&(s, _)| s < at);
        i.checked_sub(1).map_or(0, |i| self.spans[i].1)
    }

    /// The start of the first code token at or after `at`, or the source end.
    fn start_after(&self, at: usize) -> usize {
        let i = self.spans.partition_point(|&(s, _)| s < at);
        self.spans.get(i).map_or(self.len, |t| t.0)
    }
}

fn overlaps(stmt: &Statement, start: usize, end: usize) -> bool {
    (stmt.span.start as usize) < end && start < stmt.span.end as usize
}

/// The statements of `list` the range touches, going down into a body that
/// holds the whole range. A body runs from the end of the code before its first
/// statement (the `:` of a header, `THEN`, `ELSE`) to the start of the code after
/// its last one (`END`, `ELSE`) or the end of the statement that owns it.
fn select<'a>(
    list: Vec<&'a Statement>,
    start: usize,
    end: usize,
    code: &Code,
) -> Vec<&'a Statement> {
    let hit: Vec<&Statement> = list
        .into_iter()
        .filter(|s| s.span != Span::DUMMY && overlaps(s, start, end))
        .collect();
    if let [only] = hit.as_slice() {
        for body in child_lists(&only.kind) {
            if let (Some(first), Some(last)) = (body.first(), body.last())
                && code.end_before(first.span.start as usize) <= start
                && end
                    <= code
                        .start_after(last.span.end as usize)
                        .min(only.span.end as usize)
            {
                return select(body, start, end, code);
            }
        }
    }
    hit
}

fn live(list: Vec<&Statement>) -> Vec<&Statement> {
    list.into_iter().filter(|s| s.span != Span::DUMMY).collect()
}

/// The statement lists a range may lie inside, one per body. The words between
/// two bodies (`ELSE`, `WHEN ... THEN`) belong to the statement, so a range that
/// reaches one is not inside a body.
fn child_lists(kind: &StatementKind) -> Vec<Vec<&Statement>> {
    match kind {
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut lists = vec![live(vec![&**then_branch])];
            if let Some(else_branch) = else_branch {
                lists.push(live(vec![&**else_branch]));
            }
            lists
        }
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => when_branches
            .iter()
            .map(|w| live(w.body.iter().collect()))
            .chain(otherwise.iter().map(|o| live(o.iter().collect())))
            .collect(),
        StatementKind::Property {
            get_body, set_body, ..
        } => get_body
            .iter()
            .chain(set_body.iter())
            .map(|b| live(b.iter().collect()))
            .collect(),
        StatementKind::PreprocIf(p) => std::iter::once(&p.then_branch)
            .chain(p.elseif_branches.iter().map(|(_, b)| b))
            .chain(p.else_branch.iter())
            .map(|b| live(b.iter().collect()))
            .collect(),
        StatementKind::Label { body, .. } => vec![live(vec![&**body])],
        StatementKind::On { kind: on_kind } => on_action(on_kind)
            .map(|action| vec![live(vec![action])])
            .unwrap_or_default(),
        other => block_children(other).map(|c| vec![c]).unwrap_or_default(),
    }
}
