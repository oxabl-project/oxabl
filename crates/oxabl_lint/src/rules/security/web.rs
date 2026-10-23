//! How a program writes to a web response, for the rules that care what goes
//! into one (ABLSEC006, ABLSEC013).
//!
//! Two forms write the body:
//!
//! - the WebSpeed output stream: `{&OUT}` (which the preprocessor expands to
//!   `PUT STREAM WebStream UNFORMATTED` when the web include defines it) and
//!   `PUT STREAM WebStream ...` written out;
//! - the `OpenEdge.Web` API: `Write` on a response writer and `Entity` on a
//!   response, which [`is_response`] picks by the name of the object, since the
//!   parser has no types.

use std::ops::Range;

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_lexer::{Kind, Lexer};

use super::calls::Call;
use super::literal::{StatementText, is_text_read, leading_word};

/// Whether `name` looks like a web response or a response writer: it has the
/// word `response`, `resp` or `writer`.
pub fn is_response(name: &str) -> bool {
    super::names::segments(name)
        .iter()
        .any(|w| w == "resp" || w == "response" || w == "writer")
}

/// Whether `call` writes a body with the `OpenEdge.Web` API.
pub fn is_body_write(call: &Call) -> bool {
    call.is("Write") && call.receiver.as_deref().is_some_and(is_response)
}

/// The WebSpeed output name.
const OUT: &str = "{&OUT}";

/// A statement that writes to the WebSpeed output stream.
pub struct StreamWrite<'a> {
    pub text: StatementText<'a>,
    /// The tokens that are written.
    pub args: Range<usize>,
    /// The whole statement.
    pub span: Span,
}

/// `stmt` as a write to the output stream, if it is one.
pub fn stream_write<'a>(source: &'a str, stmt: &Statement) -> Option<StreamWrite<'a>> {
    match &stmt.kind {
        StatementKind::ExpressionStatement(_) => {
            // The parser reads `{&OUT} "a" b` as one expression that ends
            // early, so the span is not the statement. Read the text from the
            // reference up to the period.
            let start = stmt.span.start as usize;
            let rest = source.get(start..)?;
            if !rest
                .get(..OUT.len())
                .is_some_and(|head| head.eq_ignore_ascii_case(OUT))
            {
                return None;
            }
            let end = Lexer::new(rest)
                .find(|t| t.kind == Kind::Period)
                .map_or(source.len(), |t| start + t.end);
            let span = Span {
                start: start as u32,
                end: end as u32,
            };
            let text = StatementText::over(source, span)?;
            let args = 1..text.len();
            Some(StreamWrite { text, args, span })
        }
        _ if is_text_read(stmt) && leading_word(source, stmt).as_deref() == Some("PUT") => {
            let text = StatementText::of(source, stmt)?;
            let stream = (1..text.len().saturating_sub(1))
                .find(|&i| text.is_word(i, "STREAM") && text.is_word(i + 1, "WebStream"))?;
            let start = (stream + 2..text.len())
                .find(|&i| text.is_word(i, "UNFORMATTED"))
                .map_or(stream + 2, |i| i + 1);
            let args = start..text.len();
            Some(StreamWrite {
                text,
                args,
                span: stmt.span,
            })
        }
        _ => None,
    }
}

impl StreamWrite<'_> {
    /// Whether everything written is a literal, a line break or a space.
    pub fn is_constant(&self) -> bool {
        self.args.clone().all(|i| {
            self.text.is_constant(i..i + 1)
                || self.text.is_word(i, "SKIP")
                || self.text.is_word(i, "SPACE")
        })
    }
}
