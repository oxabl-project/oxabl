//! The fix guard: the tokens after the edits are the tokens before them, with
//! the tokens of the edits and nothing else (`docs/design/quick-fixes.md`,
//! "The token guard").

use oxabl_formatter::{Tok, literal_tab_columns, token_stream, tokens_equal};
use oxabl_lexer::{Kind, tokenize};

/// One edit of a text: a byte range and its new text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextEdit<'a> {
    pub start: usize,
    pub end: usize,
    pub new_text: &'a str,
}

/// `text` with `edits` applied. `None` when an edit is out of range, not on a
/// character boundary, or overlaps another. The edits may come in any order.
pub fn apply_edits(text: &str, edits: &[TextEdit]) -> Option<String> {
    let mut sorted: Vec<&TextEdit> = edits.iter().collect();
    sorted.sort_by_key(|edit| (edit.start, edit.end));
    let mut out = String::with_capacity(text.len());
    let mut at = 0;
    for edit in sorted {
        if edit.start < at
            || edit.start > edit.end
            || !text.is_char_boundary(edit.start)
            || !text.is_char_boundary(edit.end)
            || edit.end > text.len()
        {
            return None;
        }
        out.push_str(&text[at..edit.start]);
        out.push_str(edit.new_text);
        at = edit.end;
    }
    out.push_str(&text[at..]);
    Some(out)
}

/// Whether `edits` change the tokens of `text` by exactly the tokens they
/// remove and write, and leave each TAB in a string literal on its column.
///
/// An edit that starts or ends inside a token or a comment fails, as does a
/// new text whose tokens join with the tokens around it.
pub fn edits_keep_tokens(text: &str, edits: &[TextEdit]) -> bool {
    let Some(after_text) = apply_edits(text, edits) else {
        return false;
    };
    let before_tokens = tokenize(text);
    let cuts = |at: usize| {
        before_tokens
            .iter()
            .any(|t| t.kind != Kind::Eof && t.start < at && at < t.end)
    };
    if edits.iter().any(|edit| cuts(edit.start) || cuts(edit.end)) {
        return false;
    }
    let mut sorted: Vec<&TextEdit> = edits.iter().collect();
    sorted.sort_by_key(|edit| (edit.start, edit.end));
    let before = token_stream(text, &before_tokens);
    let new_tokens: Vec<_> = sorted.iter().map(|edit| tokenize(edit.new_text)).collect();
    let mut expected: Vec<Tok> = Vec::with_capacity(before.len() + 4);
    let mut next = 0;
    let starts: Vec<usize> = before_tokens
        .iter()
        .filter(|t| t.kind != Kind::Comment && t.kind != Kind::Eof)
        .map(|t| t.start)
        .collect();
    for (edit, tokens) in sorted.iter().zip(&new_tokens) {
        while next < before.len() && starts[next] < edit.start {
            expected.push(before[next].clone());
            next += 1;
        }
        // The tokens the edit replaces.
        while next < before.len() && starts[next] < edit.end {
            next += 1;
        }
        expected.extend(token_stream(edit.new_text, tokens));
    }
    expected.extend(before[next..].iter().cloned());
    let after_tokens = tokenize(&after_text);
    let after = token_stream(&after_text, &after_tokens);
    expected.len() == after.len()
        && expected.iter().zip(&after).all(|(a, b)| tokens_equal(a, b))
        && literal_tab_columns(text, &before_tokens)
            == literal_tab_columns(&after_text, &after_tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn insert<'a>(at: usize, new_text: &'a str) -> TextEdit<'a> {
        TextEdit {
            start: at,
            end: at,
            new_text,
        }
    }

    #[test]
    fn an_insertion_between_tokens_passes() {
        let text = "DEFINE VARIABLE i AS INTEGER.";
        assert!(edits_keep_tokens(text, &[insert(28, " NO-UNDO")]));
    }

    #[test]
    fn an_insertion_that_joins_a_token_fails() {
        // `INTEGERNO-UNDO` is one name, not two keywords.
        let text = "DEFINE VARIABLE i AS INTEGER.";
        assert!(!edits_keep_tokens(text, &[insert(28, "NO-UNDO")]));
    }

    #[test]
    fn an_edit_inside_a_token_or_a_comment_fails() {
        let text = "DEFINE VARIABLE i AS INTEGER. /* note */";
        assert!(!edits_keep_tokens(text, &[insert(24, " NO-UNDO ")]));
        assert!(!edits_keep_tokens(text, &[insert(34, " NO-UNDO")]));
    }

    #[test]
    fn a_tab_moved_off_its_column_fails() {
        let text = "x = 1. c = \"a\tb\".";
        assert!(!edits_keep_tokens(text, &[insert(6, " y = 2.")]));
        assert!(edits_keep_tokens(text, &[insert(6, " y = 2. ")]));
    }

    #[test]
    fn a_deletion_of_whole_tokens_passes() {
        let text = "DEFINE VARIABLE i AS INTEGER.\nDISPLAY 1.\n";
        assert!(edits_keep_tokens(
            text,
            &[TextEdit {
                start: 0,
                end: 30,
                new_text: ""
            }]
        ));
    }

    #[test]
    fn overlapping_edits_fail() {
        let text = "DISPLAY 1.";
        assert!(!edits_keep_tokens(
            text,
            &[
                TextEdit {
                    start: 0,
                    end: 8,
                    new_text: ""
                },
                insert(4, "x")
            ]
        ));
    }
}
