//! Quick fixes: the parts every client shares (`docs/design/quick-fixes.md`).
//!
//! The collector of `oxabl_analyze` maps the edits of each fix from the
//! expanded text to the root's own bytes. [`place_fixes`] then writes the
//! keywords of each edit in the project's keyword case and the line breaks of
//! the file, and keeps a fix only when it passes the token guard
//! ([`edits_keep_tokens`]) on the root's own text.

mod guard;

use oxabl_common::{Diagnostic, Fix};
use oxabl_lexer::{Token, tokenize};
use oxabl_style::KeywordCase;

use crate::completion::case::{Case, is_keyword};

pub use guard::{TextEdit, apply_edits, edits_keep_tokens};

/// The line break of `text`: `\r\n` when its first line break is `\r\n`,
/// else `\n`.
pub fn line_break(text: &str) -> &'static str {
    match text.find('\n') {
        Some(at) if at > 0 && text.as_bytes()[at - 1] == b'\r' => "\r\n",
        _ => "\n",
    }
}

/// `new_text` with its keywords in `case` and each `\n` as `line_break`.
/// Names, literals and comments keep their spelling.
fn render(new_text: &str, case: Case, line_break: &str) -> String {
    let mut out = String::with_capacity(new_text.len());
    let mut at = 0;
    for token in tokenize(new_text) {
        if !is_keyword(token.kind) || token.start >= token.end {
            continue;
        }
        out.push_str(&new_text[at..token.start]);
        out.push_str(&case.apply(&new_text[token.start..token.end]));
        at = token.end;
    }
    out.push_str(&new_text[at..]);
    if line_break == "\n" {
        out
    } else {
        out.replace('\n', line_break)
    }
}

/// The root's own text, lexed once for every fix of the file.
struct Root<'a> {
    text: &'a str,
    tokens: Option<Vec<Token>>,
}

impl Root<'_> {
    /// The keyword case of an edit of the statement `start..end`. `Preserve`
    /// follows the first keyword of the statement, then the nearest keyword
    /// before it, and else upper case.
    fn case(&mut self, keyword_case: KeywordCase, start: usize, end: usize) -> Case {
        if keyword_case != KeywordCase::Preserve {
            return Case::of(keyword_case, "", "", &[], 0);
        }
        let text = self.text;
        let tokens = self.tokens.get_or_insert_with(|| tokenize(text));
        let first = tokens.partition_point(|t| t.start < start);
        let word = tokens[first..]
            .iter()
            .take_while(|t| t.start < end)
            .find(|t| is_keyword(t.kind) && t.start < t.end)
            .map_or("", |t| &text[t.start..t.end]);
        Case::of(keyword_case, word, text, tokens, first)
    }
}

/// The lines of `text` that hold `start..end`: from the start of the line of
/// `start` to the end of the line of `end`, with its line break.
fn lines_around(text: &str, start: usize, end: usize) -> (usize, usize) {
    let from = text[..start].rfind('\n').map_or(0, |at| at + 1);
    let to = text[end..].find('\n').map_or(text.len(), |at| end + at + 1);
    (from, to)
}

/// Whether the edits of `fix`, already in the root's own bytes, pass the token
/// guard on the lines they and their statements touch.
fn guard_passes(text: &str, fix: &Fix) -> bool {
    let in_range = |start: u32, end: u32| {
        start <= end
            && (end as usize) <= text.len()
            && text.is_char_boundary(start as usize)
            && text.is_char_boundary(end as usize)
    };
    let spans = fix
        .edits
        .iter()
        .flat_map(|edit| [edit.span.span, edit.statement.span]);
    let (mut start, mut end) = (usize::MAX, 0);
    for span in spans {
        if !in_range(span.start, span.end) {
            return false;
        }
        start = start.min(span.start as usize);
        end = end.max(span.end as usize);
    }
    if start > end {
        return false;
    }
    let (from, to) = lines_around(text, start, end);
    let edits: Vec<TextEdit> = fix
        .edits
        .iter()
        .map(|edit| TextEdit {
            start: edit.span.span.start as usize - from,
            end: edit.span.span.end as usize - from,
            new_text: &edit.new_text,
        })
        .collect();
    edits_keep_tokens(&text[from..to], &edits)
}

/// Place the fixes of `diagnostics`, whose edits the collector mapped to the
/// bytes of `root_text`: write each edit's keywords in `keyword_case` and its
/// line breaks as the file's, and drop each fix that fails the token guard.
/// A deferred fix that no step fills is dropped.
pub fn place_fixes<'a>(
    diagnostics: impl Iterator<Item = &'a mut Diagnostic>,
    root_text: &str,
    keyword_case: KeywordCase,
) {
    let line_break = line_break(root_text);
    let mut root = Root {
        text: root_text,
        tokens: None,
    };
    for diagnostic in diagnostics {
        if diagnostic.fixes.is_empty() {
            continue;
        }
        let fixes = std::mem::take(&mut diagnostic.fixes);
        diagnostic.fixes = fixes
            .into_iter()
            .filter(|fix| fix.deferred.is_none() && !fix.edits.is_empty())
            .filter_map(|mut fix| {
                for edit in &mut fix.edits {
                    let statement = edit.statement.span;
                    if statement.end as usize > root_text.len() {
                        return None;
                    }
                    let case = root.case(
                        keyword_case,
                        statement.start as usize,
                        statement.end as usize,
                    );
                    edit.new_text = render(&edit.new_text, case, line_break);
                }
                guard_passes(root_text, &fix).then_some(fix)
            })
            .collect();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_line_break_is_the_first_of_the_file() {
        assert_eq!(line_break("a.\r\nb.\n"), "\r\n");
        assert_eq!(line_break("a.\nb.\r\n"), "\n");
        assert_eq!(line_break("a."), "\n");
        assert_eq!(line_break("\r\n"), "\r\n");
    }

    #[test]
    fn keywords_are_recased_and_names_kept() {
        assert_eq!(render(" NO-UNDO", Case::Lower, "\n"), " no-undo");
        assert_eq!(
            render("USING Pkg.Name.\n", Case::Lower, "\r\n"),
            "using Pkg.Name.\r\n"
        );
    }

    #[test]
    fn preserve_follows_the_first_keyword_of_the_statement() {
        let text = "DISPLAY 1.\ndefine variable x as integer.\nx = 1.\nDISPLAY x.\n";
        let mut root = Root { text, tokens: None };
        assert_eq!(root.case(KeywordCase::Preserve, 11, 40), Case::Lower);
        assert_eq!(root.case(KeywordCase::Preserve, 0, 10), Case::Upper);
        // No keyword in `x = 1.`: the nearest one before it.
        assert_eq!(root.case(KeywordCase::Preserve, 41, 47), Case::Lower);
        assert_eq!(root.case(KeywordCase::Uppercase, 11, 40), Case::Upper);
        assert_eq!(root.case(KeywordCase::Lowercase, 0, 10), Case::Lower);
    }
}
