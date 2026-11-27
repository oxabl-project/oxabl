//! The tokens of a request's text, from the tokens of the base's text.
//!
//! A request reads the tokens before the cursor, the word the cursor is in,
//! and the two tokens after it. A base keeps the tokens of its own text, so a
//! request on the same text lexes nothing, and a request on a newer text keeps
//! the base's tokens up to the edit and lexes from there to the end of what the
//! request reads. The result is a prefix of what [`oxabl_lexer::tokenize`]
//! gives for the text, and holds all that the request reads.

use std::ops::Index;

use oxabl_lexer::{Kind, Lexer, Token};

use super::context::is_path_byte;

/// The text of an older analysis and its tokens.
#[derive(Clone, Copy)]
pub struct Prior<'p> {
    pub text: &'p str,
    /// The tokens of `text`, as [`oxabl_lexer::tokenize`] gives them.
    pub tokens: &'p [Token],
}

/// How the text of a request differs from the text of an older analysis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TextDiff {
    /// The two texts are equal.
    Same,
    /// The texts share their first `prefix` bytes and their last `suffix`
    /// bytes, which do not overlap.
    Differs { prefix: usize, suffix: usize },
    /// Not known: compare the texts.
    Unknown,
}

/// Zero-copy view of a request's token stream: an unchanged prefix borrowed
/// from `prior` followed by the freshly lexed tail.
pub(crate) struct TokenStream<'p> {
    prefix: &'p [Token],
    tail: Vec<Token>,
    /// The text of the prior this stream was made from, and how the request
    /// text differs from it, so `base::map` does not compare the texts again.
    prior: Option<(&'p str, TextDiff)>,
}

impl<'p> TokenStream<'p> {
    pub(crate) fn from_slice(tokens: &'p [Token]) -> Self {
        Self {
            prefix: tokens,
            tail: Vec::new(),
            prior: None,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.prefix.len() + self.tail.len()
    }

    #[cfg(test)]
    pub(crate) fn last(&self) -> Option<&Token> {
        self.tail.last().or_else(|| self.prefix.last())
    }

    /// How the request text differs from `text`. Known only when this stream
    /// was made from a prior whose text is `text` itself (the same slice, not
    /// an equal one); otherwise [`TextDiff::Unknown`].
    pub(crate) fn diff_from(&self, text: &str) -> TextDiff {
        match self.prior {
            Some((prior, diff)) if std::ptr::eq(prior, text) => diff,
            _ => TextDiff::Unknown,
        }
    }

    pub(crate) fn partition_point(&self, mut pred: impl FnMut(&Token) -> bool) -> usize {
        if self.prefix.is_empty() {
            return self.tail.partition_point(pred);
        }
        if let Some(last_prefix) = self.prefix.last()
            && pred(last_prefix)
        {
            return self.prefix.len() + self.tail.partition_point(pred);
        }
        self.prefix.partition_point(pred)
    }

    /// Iterate over tokens in `0..end`, supporting `.rev()`.
    pub(crate) fn iter_to(&self, end: usize) -> impl DoubleEndedIterator<Item = &Token> {
        let p_end = end.min(self.prefix.len());
        let t_end = end.saturating_sub(self.prefix.len()).min(self.tail.len());
        self.prefix[..p_end].iter().chain(self.tail[..t_end].iter())
    }

    /// Iterate over tokens in `start..self.len()`, supporting `.rev()`.
    pub(crate) fn iter_from(&self, start: usize) -> impl DoubleEndedIterator<Item = &Token> {
        let p_start = start.min(self.prefix.len());
        let t_start = start.saturating_sub(self.prefix.len()).min(self.tail.len());
        self.prefix[p_start..]
            .iter()
            .chain(self.tail[t_start..].iter())
    }

    /// Iterate over tokens in `start..end`, supporting `.rev()`.
    pub(crate) fn iter_range(
        &self,
        start: usize,
        end: usize,
    ) -> impl DoubleEndedIterator<Item = &Token> {
        let p_len = self.prefix.len();
        let p_start = start.min(p_len);
        let p_end = end.min(p_len).max(p_start);
        let t_start = start.saturating_sub(p_len).min(self.tail.len());
        let t_end = end.saturating_sub(p_len).min(self.tail.len()).max(t_start);
        self.prefix[p_start..p_end]
            .iter()
            .chain(self.tail[t_start..t_end].iter())
    }
}

impl Index<usize> for TokenStream<'_> {
    type Output = Token;

    #[inline]
    fn index(&self, index: usize) -> &Token {
        let p_len = self.prefix.len();
        if index < p_len {
            &self.prefix[index]
        } else {
            &self.tail[index - p_len]
        }
    }
}

/// The tokens of `text`, which has the cursor at `cursor`. Without `prior`,
/// all of them.
pub(crate) fn of_request<'p>(text: &str, cursor: u32, prior: Option<Prior<'p>>) -> TokenStream<'p> {
    let Some(prior) = prior else {
        return TokenStream {
            prefix: &[],
            tail: oxabl_lexer::tokenize(text),
            prior: None,
        };
    };
    let (prefix, suffix) = common_ends(prior.text.as_bytes(), text.as_bytes());
    if prefix == prior.text.len() && prefix == text.len() {
        return TokenStream {
            prefix: prior.tokens,
            tail: Vec::new(),
            prior: Some((prior.text, TextDiff::Same)),
        };
    }
    // The bytes that a request reads after the cursor, and the bytes that the
    // edit changed: all of them are lexed.
    let bytes = text.as_bytes();
    let mut word_end = (cursor as usize).min(bytes.len());
    while word_end < bytes.len() && is_path_byte(bytes[word_end]) {
        word_end += 1;
    }
    let need = word_end.max(text.len() - suffix);
    let keep = reusable(prior, prefix);
    let from = keep.checked_sub(1).map_or(0, |last| prior.tokens[last].end);
    let mut tail = Vec::with_capacity(64);
    let mut after = 0;
    for token in Lexer::starting_at(text, from) {
        let last = token.kind == Kind::Eof;
        let counts = token.start >= need && token.kind != Kind::Comment;
        tail.push(token);
        if last {
            break;
        }
        after += usize::from(counts);
        if after == AFTER {
            break;
        }
    }
    TokenStream {
        prefix: &prior.tokens[..keep],
        tail,
        prior: Some((prior.text, TextDiff::Differs { prefix, suffix })),
    }
}

/// The non-comment tokens that a request reads at and after the word: the
/// target of a `RUN` and the name after it.
const AFTER: usize = 2;

/// The length of the common start of `old` and `new`, and of their common end
/// that does not overlap it. Fast-paths equal slices through SIMD `memcmp`
/// before scanning mismatching ends in 32-byte and 8-byte chunks.
pub(crate) fn common_ends(old: &[u8], new: &[u8]) -> (usize, usize) {
    if std::ptr::eq(old, new) || old == new {
        return (old.len(), 0);
    }
    let min_len = old.len().min(new.len());
    let mut prefix = 0;
    while prefix + 32 <= min_len && old[prefix..prefix + 32] == new[prefix..prefix + 32] {
        prefix += 32;
    }
    while prefix + 8 <= min_len && old[prefix..prefix + 8] == new[prefix..prefix + 8] {
        prefix += 8;
    }
    while prefix < min_len && old[prefix] == new[prefix] {
        prefix += 1;
    }

    let room = min_len - prefix;
    let mut suffix = 0;
    let old_len = old.len();
    let new_len = new.len();
    while suffix + 32 <= room
        && old[old_len - suffix - 32..old_len - suffix]
            == new[new_len - suffix - 32..new_len - suffix]
    {
        suffix += 32;
    }
    while suffix + 8 <= room
        && old[old_len - suffix - 8..old_len - suffix]
            == new[new_len - suffix - 8..new_len - suffix]
    {
        suffix += 8;
    }
    while suffix < room && old[old_len - 1 - suffix] == new[new_len - 1 - suffix] {
        suffix += 1;
    }
    (prefix, suffix)
}

/// How many tokens of `prior` the text that shares its first `prefix` bytes
/// also has, and that a lexer can start after.
///
/// The lexer reads past the end of a token only along its own line, and one
/// line more after a `~` at the end of one, so a token that ends before the
/// line above the edit does not change. A `&SCOPED-DEFINE` or `&GLOBAL-DEFINE`
/// value is lexed in a mode that a new lexer does not know: the start is
/// before such a directive that no `PreprocEnd` has closed.
fn reusable(prior: Prior<'_>, prefix: usize) -> usize {
    let before = &prior.text.as_bytes()[..prefix];
    let line = before
        .iter()
        .rposition(|b| *b == b'\n')
        .map_or(0, |at| at + 1);
    let safe = before[..line.saturating_sub(1)]
        .iter()
        .rposition(|b| *b == b'\n')
        .map_or(0, |at| at + 1);
    let mut keep = prior
        .tokens
        .partition_point(|token| token.end <= safe && token.kind != Kind::Eof);
    for at in (0..keep).rev() {
        match prior.tokens[at].kind {
            Kind::PreprocEnd => break,
            Kind::PreprocScopedDefine | Kind::PreprocGlobalDefine => {
                keep = at;
                break;
            }
            _ => {}
        }
    }
    keep
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Texts with what makes a lexer depend on the text around a token: a
    /// block comment, a string over two lines, a define with its mode, a
    /// continuation, an include reference, a lock phrase and a date.
    const TEXTS: &[&str] = &[
        "DEFINE VARIABLE total AS INTEGER NO-UNDO.\nPROCEDURE p:\n  total = total + 1.\nEND PROCEDURE.\n",
        "/* a comment\n over lines */ x = 1.\n// line\nFIND FIRST Customer SHARE   LOCK NO-ERROR.\n",
        "MESSAGE \"a string\nover two lines\" VIEW-AS ALERT-BOX.\nx = 'it''s'.\n",
        "&SCOPED-DEFINE a 1 + ~\n   2\n&SCOPED-DEFINE b {&a}\nx = {&b}.\nIF x THEN y = 1.\n",
        "{inc.i &a=1}\nx = 01/02/2026.\nRUN p1 (INPUT 1.5e3).\n{1}\n",
        "x = 1.\n&GLOBAL-DEFINE g 2\ny = 3.\n{ unclosed\nz = 4.\n",
        "",
        "x",
    ];

    /// The edits: the text put at a position, and how many bytes it replaces.
    const EDITS: &[(&str, usize)] = &[
        ("a", 0),
        (" ", 0),
        ("\n", 0),
        ("/*", 0),
        ("*/", 0),
        ("\"", 0),
        ("{", 0),
        ("}", 0),
        ("~\n", 0),
        ("&SCOPED-DEFINE q ", 0),
        ("", 1),
        ("", 3),
        ("RUN ", 2),
    ];

    /// What a request reads: all tokens up to the end of the edit, and the
    /// two non-comment tokens after.
    fn covered(full: &[Token], tokens: &TokenStream<'_>, to: usize) -> bool {
        let first = full
            .iter()
            .position(|token| token.start >= to)
            .unwrap_or(full.len());
        tokens.last().is_some_and(|last| last.kind == Kind::Eof) || tokens.len() >= first + 2
    }

    #[test]
    fn the_tokens_of_an_edited_text_are_a_prefix_of_all_of_them() {
        for old in TEXTS {
            let old_tokens = oxabl_lexer::tokenize(old);
            let prior = Prior {
                text: old,
                tokens: &old_tokens,
            };
            for at in (0..=old.len()).filter(|at| old.is_char_boundary(*at)) {
                for (put, cut) in EDITS {
                    let end = (at + cut).min(old.len());
                    if !old.is_char_boundary(end) {
                        continue;
                    }
                    let new = format!("{}{put}{}", &old[..at], &old[end..]);
                    let cursor = at + put.len();
                    let full = oxabl_lexer::tokenize(&new);
                    let tokens = of_request(&new, cursor as u32, Some(prior));
                    assert!(
                        tokens.len() <= full.len()
                            && tokens.iter_from(0).eq(full[..tokens.len()].iter()),
                        "{put:?} at {at} (cut {cut}) of {old:?}: not a prefix of the tokens"
                    );
                    assert!(
                        covered(&full, &tokens, cursor),
                        "{put:?} at {at} (cut {cut}) of {old:?}: the tokens stop early"
                    );
                }
            }
        }
    }

    #[test]
    fn the_same_text_borrows_the_tokens() {
        let text = "x = 1.\n";
        let tokens = oxabl_lexer::tokenize(text);
        let prior = Prior {
            text,
            tokens: &tokens,
        };
        let stream = of_request(text, 3, Some(prior));
        assert!(stream.tail.is_empty());
        assert_eq!(stream.prefix.len(), tokens.len());
    }

    #[test]
    fn the_difference_is_known_only_for_the_prior_text_itself() {
        let old = "x = 1.\ny = 2.\n";
        let tokens = oxabl_lexer::tokenize(old);
        let prior = Prior {
            text: old,
            tokens: &tokens,
        };
        let same = of_request(old, 3, Some(prior));
        assert_eq!(same.diff_from(old), TextDiff::Same);

        let new = "x = 10.\ny = 2.\n";
        let edited = of_request(new, 5, Some(prior));
        assert_eq!(
            edited.diff_from(old),
            TextDiff::Differs {
                prefix: 5,
                suffix: 9
            }
        );

        // An equal text in another place, and a stream of bare tokens, say
        // nothing about a base: the caller must compare.
        let copy = old.to_owned();
        assert_eq!(same.diff_from(&copy), TextDiff::Unknown);
        assert_eq!(edited.diff_from(new), TextDiff::Unknown);
        assert_eq!(
            TokenStream::from_slice(&tokens).diff_from(old),
            TextDiff::Unknown
        );
    }

    #[test]
    fn common_ends_handles_equal_slices_and_long_chunked_matches() {
        let long = "DEFINE VARIABLE total AS INTEGER NO-UNDO.\n".repeat(4);
        let cloned = long.clone();
        assert_eq!(
            common_ends(long.as_bytes(), long.as_bytes()),
            (long.len(), 0)
        );
        assert_eq!(
            common_ends(long.as_bytes(), cloned.as_bytes()),
            (long.len(), 0)
        );

        let mut edited = long.clone();
        let at = 75;
        edited.insert_str(at, "/* edit */");
        let (prefix, suffix) = common_ends(long.as_bytes(), edited.as_bytes());
        assert_eq!(prefix, at);
        assert_eq!(suffix, long.len() - at);
    }
}
