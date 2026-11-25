//! The case completion writes keywords in.

use oxabl_lexer::{Kind, Token};
use oxabl_style::KeywordCase;

/// Upper or lower case, for the keywords of one answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Case {
    Upper,
    Lower,
}

impl Case {
    /// The case of `keyword_case` at the cursor. `Preserve` follows the word
    /// when it holds a letter (all lower case gives lower case), then the
    /// nearest keyword before the word, and else upper case.
    pub(crate) fn of(
        keyword_case: KeywordCase,
        word: &str,
        text: &str,
        tokens: &[Token],
        before: usize,
    ) -> Case {
        match keyword_case {
            KeywordCase::Uppercase => Case::Upper,
            KeywordCase::Lowercase => Case::Lower,
            KeywordCase::Preserve => written_case(word)
                .or_else(|| {
                    tokens[..before]
                        .iter()
                        .rev()
                        .filter(|token| is_keyword(token.kind))
                        .find_map(|token| written_case(&text[token.start..token.end]))
                })
                .unwrap_or(Case::Upper),
        }
    }

    pub(crate) fn apply(self, word: &str) -> String {
        match self {
            Case::Upper => word.to_ascii_uppercase(),
            Case::Lower => word.to_ascii_lowercase(),
        }
    }

    /// `body` with each `<<KEYWORDS>>` marker written in this case.
    pub(crate) fn render_snippet(self, body: &str) -> String {
        let mut out = String::with_capacity(body.len());
        let mut rest = body;
        while let Some(open) = rest.find("<<") {
            out.push_str(&rest[..open]);
            let after = &rest[open + 2..];
            match after.find(">>") {
                Some(close) => {
                    out.push_str(&self.apply(&after[..close]));
                    rest = &after[close + 2..];
                }
                None => {
                    out.push_str(&rest[open..]);
                    rest = "";
                }
            }
        }
        out.push_str(rest);
        out
    }
}

/// The case `text` is written in: lower when its letters are all lower case,
/// upper otherwise. `None` when it holds no letter.
fn written_case(text: &str) -> Option<Case> {
    let mut letters = text.chars().filter(char::is_ascii_alphabetic).peekable();
    letters.peek()?;
    if letters.all(|c| c.is_ascii_lowercase()) {
        Some(Case::Lower)
    } else {
        Some(Case::Upper)
    }
}

/// Whether a token of this kind is a keyword, not a name, a literal or
/// punctuation.
pub(crate) fn is_keyword(kind: Kind) -> bool {
    !matches!(
        kind,
        Kind::Eof
            | Kind::Invalid
            | Kind::Identifier
            | Kind::Comment
            | Kind::PreprocEnd
            | Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::DateLiteral
            | Kind::StringLiteral
            | Kind::IncludeReference
            | Kind::IncludeArgReference
            | Kind::Preprop
    ) && !is_punctuation(kind)
}

fn is_punctuation(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Plus
            | Kind::Add
            | Kind::Minus
            | Kind::Period
            | Kind::Star
            | Kind::Slash
            | Kind::Colon
            | Kind::Question
            | Kind::At
            | Kind::LeftBracket
            | Kind::RightBracket
            | Kind::Caret
            | Kind::LessThan
            | Kind::Equals
            | Kind::GreaterThan
            | Kind::GreaterThanOrEqual
            | Kind::LessThanOrEqual
            | Kind::NotEqual
            | Kind::PlusEquals
            | Kind::MinusEquals
            | Kind::StarEquals
            | Kind::SlashEquals
            | Kind::DoubleColon
            | Kind::LeftParen
            | Kind::RightParen
            | Kind::LeftBrace
            | Kind::RightBrace
            | Kind::Comma
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn case(keyword_case: KeywordCase, word: &str, text: &str) -> Case {
        let tokens = oxabl_lexer::tokenize(text);
        let before = tokens.iter().filter(|t| t.kind != Kind::Eof).count();
        Case::of(keyword_case, word, text, &tokens, before)
    }

    #[test]
    fn preserve_follows_the_word_then_the_nearest_keyword() {
        assert_eq!(case(KeywordCase::Preserve, "def", ""), Case::Lower);
        assert_eq!(case(KeywordCase::Preserve, "Def", ""), Case::Upper);
        assert_eq!(
            case(KeywordCase::Preserve, "", "define variable x."),
            Case::Lower
        );
        assert_eq!(
            case(KeywordCase::Preserve, "", "MESSAGE \"a\" x."),
            Case::Upper
        );
        assert_eq!(case(KeywordCase::Preserve, "", "x = 1."), Case::Upper);
        assert_eq!(case(KeywordCase::Uppercase, "def", ""), Case::Upper);
        assert_eq!(case(KeywordCase::Lowercase, "DEF", ""), Case::Lower);
    }

    #[test]
    fn a_snippet_body_is_written_in_the_case() {
        assert_eq!(
            Case::Lower.render_snippet(
                "<<DEFINE VARIABLE>> ${1:x} <<AS>> ${2|<<CHARACTER>>,<<INTEGER>>|}."
            ),
            "define variable ${1:x} as ${2|character,integer|}."
        );
    }
}
