//! The one decision every sink rule shares: is this argument a literal?
//!
//! An argument is constant when an attacker cannot change its text:
//!
//! - a string, number, boolean or `?` literal;
//! - a concatenation (`+`) of constants, and an `IF ... THEN a ELSE b` whose
//!   branches are both constant (the condition picks a literal, it does not
//!   build text);
//! - `VALUE(x)` of a constant;
//! - text that a preprocessor name expanded to one of the above. The rules run
//!   on preprocessed text, so `{&NAME}` is already that text. A reference the
//!   preprocessor left in place is not constant.
//!
//! Everything else is not: a variable, a field, a function or method call
//! (`QUOTER("x")`, `STRING(i)`, `SUBSTITUTE(...)`), a `NEW`, an array access.
//! Stage 1 does not model sanitisers, so a call that cleans a value does not
//! make it constant.
//!
//! [`is_constant`] reads an expression tree. [`StatementText`] answers the
//! same question for the statements the parser does not model, where only the
//! tokens exist.

use std::ops::Range;

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_lexer::{Kind, Token, tokenize};

/// Whether `expr` is a literal or built only from literals.
pub fn is_constant(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Literal(_) => true,
        ExpressionKind::Add(l, r) => is_constant(l) && is_constant(r),
        ExpressionKind::IfThenElse(_, then, otherwise) => {
            is_constant(then) && is_constant(otherwise)
        }
        ExpressionKind::FunctionCall { name, arguments } => {
            name.name.eq_ignore_ascii_case("VALUE")
                && matches!(arguments.as_slice(), [only] if is_constant(only))
        }
        _ => false,
    }
}

/// The span from the start of `lead` to the end of `to`, or `fallback` when
/// either node has no real extent (a hand-built tree).
///
/// A finding starts at the sink keyword and not at its argument. When the
/// argument is the expansion of a preprocessor name, its own text does not
/// belong to the file, and a finding that starts there is not reported.
pub fn cover(lead: Span, to: Span, fallback: Span) -> Span {
    if lead.start < lead.end && lead.start < to.end {
        Span {
            start: lead.start,
            end: to.end.max(lead.end),
        }
    } else {
        fallback
    }
}

/// Whether the text of `stmt` holds any of `needles`, ignoring ASCII case.
///
/// A cheap check before lexing a skipped statement: most hold none of the
/// words a rule looks for.
pub fn mentions(source: &str, stmt: &Statement, needles: &[&str]) -> bool {
    let Some(text) = super::super::syntactic::source_of(source, stmt.span) else {
        return false;
    };
    let bytes = text.as_bytes();
    needles.iter().any(|needle| {
        let n = needle.as_bytes();
        bytes.windows(n.len()).any(|w| w.eq_ignore_ascii_case(n))
    })
}

/// Whether the rules read `stmt` by lexing its text: a statement the parser
/// skips, or one it models only in part (`PUT`, `EXPORT`, `UPDATE`, ...), whose
/// options stay unparsed. A rule that reads the text of such a statement does
/// not also walk its tree, so a call is seen once.
pub fn is_text_read(stmt: &Statement) -> bool {
    matches!(
        stmt.kind,
        StatementKind::Skipped { .. }
            | StatementKind::Put { .. }
            | StatementKind::Export { .. }
            | StatementKind::Import { .. }
            | StatementKind::Pause { .. }
            | StatementKind::Down { .. }
            | StatementKind::Form { .. }
            | StatementKind::Update { .. }
            | StatementKind::Set { .. }
            | StatementKind::Apply { .. }
            | StatementKind::WaitFor { .. }
            | StatementKind::Enable { .. }
            | StatementKind::Disable { .. }
            | StatementKind::Hide { .. }
            | StatementKind::View { .. }
            | StatementKind::Up { .. }
            | StatementKind::DeleteWidgetPool { .. }
            | StatementKind::CreateWidgetPool { .. }
            | StatementKind::ReleaseObject { .. }
    )
}

/// The first word of `stmt`, upper-cased, or `None` when it has no tokens.
pub fn leading_word(source: &str, stmt: &Statement) -> Option<String> {
    super::super::syntactic::leading_tokens(source, stmt.span, 1)
        .first()
        .map(|(_, word)| word.to_ascii_uppercase())
}

/// The tokens of one statement, for rules that read an unmodelled statement.
///
/// Comments, the end-of-file token and the closing period are dropped. A token
/// is named by its position among the rest; [`span`](Self::span) maps a range
/// of positions back to the file under analysis.
pub struct StatementText<'a> {
    text: &'a str,
    base: u32,
    tokens: Vec<Token>,
}

impl<'a> StatementText<'a> {
    /// Lex `stmt`, or `None` when its span does not fall inside `source`.
    pub fn of(source: &'a str, stmt: &Statement) -> Option<Self> {
        let text = super::super::syntactic::source_of(source, stmt.span)?;
        let mut tokens: Vec<Token> = tokenize(text)
            .into_iter()
            .filter(|t| !matches!(t.kind, Kind::Eof | Kind::Comment))
            .collect();
        if tokens.last().is_some_and(|t| t.kind == Kind::Period) {
            tokens.pop();
        }
        Some(StatementText {
            text,
            base: stmt.span.start,
            tokens,
        })
    }

    /// Number of tokens.
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Whether the statement has no tokens.
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    /// The token at `i`.
    pub fn token(&self, i: usize) -> &Token {
        &self.tokens[i]
    }

    /// The source text of the token at `i`.
    pub fn word(&self, i: usize) -> &'a str {
        let t = &self.tokens[i];
        &self.text[t.start..t.end]
    }

    /// Whether the token at `i` exists and has this kind.
    pub fn is(&self, i: usize, kind: Kind) -> bool {
        self.tokens.get(i).is_some_and(|t| t.kind == kind)
    }

    /// Whether the token at `i` exists and spells `word`, ignoring case.
    pub fn is_word(&self, i: usize, word: &str) -> bool {
        i < self.tokens.len() && self.word(i).eq_ignore_ascii_case(word)
    }

    /// The file span of the tokens in `range`. An empty range gives an empty
    /// span at its start.
    pub fn span(&self, range: Range<usize>) -> Span {
        let at = |i: usize, end: bool| {
            let t = &self.tokens[i];
            self.base + if end { t.end } else { t.start } as u32
        };
        if range.start >= range.end || range.end > self.tokens.len() {
            let start = self
                .tokens
                .get(range.start)
                .map_or(self.base, |_| at(range.start, false));
            return Span { start, end: start };
        }
        Span {
            start: at(range.start, false),
            end: at(range.end - 1, true),
        }
    }

    /// The index of the `)` that closes the `(` at `open`.
    pub fn close_of(&self, open: usize) -> Option<usize> {
        if !self.is(open, Kind::LeftParen) {
            return None;
        }
        let mut depth = 0usize;
        for i in open..self.tokens.len() {
            match self.tokens[i].kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Whether the tokens in `range` hold only literals joined by `+`, as
    /// [`is_constant`] reads an expression. An empty range is not constant.
    pub fn is_constant(&self, range: Range<usize>) -> bool {
        if range.is_empty() || range.end > self.tokens.len() {
            return false;
        }
        range.clone().all(|i| match self.tokens[i].kind {
            Kind::StringLiteral
            | Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::Plus
            | Kind::LeftParen
            | Kind::RightParen
            | Kind::Question => true,
            Kind::Value => self.is(i + 1, Kind::LeftParen),
            // The lexer reports `+` as `Add` in some positions.
            Kind::Add => self.word(i) == "+",
            _ => false,
        })
    }

    /// The token range inside each `VALUE( ... )` that starts in `range`, in
    /// order. A group that never closes ends the scan.
    pub fn value_operands(&self, range: Range<usize>) -> Vec<Range<usize>> {
        let mut out = Vec::new();
        let mut i = range.start;
        while i < range.end.min(self.tokens.len()) {
            if self.is(i, Kind::Value)
                && self.is(i + 1, Kind::LeftParen)
                && let Some(close) = self.close_of(i + 1)
            {
                out.push(i + 2..close);
                i = close + 1;
            } else {
                i += 1;
            }
        }
        out
    }

    /// The argument ranges of the call whose `(` is at `open`, split at
    /// top-level commas. An empty argument list gives no ranges.
    pub fn call_args(&self, open: usize) -> Vec<Range<usize>> {
        let Some(close) = self.close_of(open) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        let (mut depth, mut start) = (0usize, open + 1);
        for i in open + 1..close {
            match self.tokens[i].kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth -= 1,
                Kind::Comma if depth == 0 => {
                    out.push(start..i);
                    start = i + 1;
                }
                _ => {}
            }
        }
        if start < close {
            out.push(start..close);
        }
        out
    }
}
