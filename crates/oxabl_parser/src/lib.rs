//! Parser for Progress ABL source code.
//!
//! Consumes tokens produced by [`oxabl_lexer`] and builds an AST using the
//! node types defined in [`oxabl_ast`].

mod literal;
mod parser;

pub use parser::{ParseError, ParseResult, Parser, Program};

use oxabl_common::{InternalPanic, catch_panic};

/// Tokenize and parse `source` in the parser's error-recovery mode, containing
/// any internal panic instead of letting it kill the caller.
///
/// This is the fallible sibling of the panicking `oxabl::parse` convenience and
/// the entry point new code should reach for. The two failure kinds stay
/// separate:
///
/// - **Recovered parse errors are not failures here.** They arrive in the `Ok`
///   value's [`Program::errors`], exactly as they do from
///   [`Parser::parse_program`], so malformed-but-recoverable ABL still yields a
///   `Program`.
/// - `Err(`[`InternalPanic`]`)` means an oxabl bug — a panic in the lexer or
///   parser — with the panic message attached.
///
/// # Platform caveat
///
/// The guard is a documented pass-through on `wasm32-unknown-unknown`; see
/// [`catch_panic`].
pub fn try_parse(source: &str) -> Result<Program, InternalPanic> {
    catch_panic(|| {
        let tokens = oxabl_lexer::tokenize(source);
        Parser::new(&tokens, source).parse_program()
    })
}

/// The calls of [`Parser::parse_program`] and the bytes of text their tokens
/// cover, counted only with the test-only `test-counters` feature.
#[cfg(feature = "test-counters")]
static PARSES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
#[cfg(feature = "test-counters")]
static PARSED_BYTES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[cfg(feature = "test-counters")]
fn count_parse(tokens: &[oxabl_lexer::Token]) {
    use std::sync::atomic::Ordering::Relaxed;
    PARSES.fetch_add(1, Relaxed);
    if let (Some(first), Some(last)) = (tokens.first(), tokens.last()) {
        PARSED_BYTES.fetch_add(last.end.saturating_sub(first.start) as usize, Relaxed);
    }
}

/// How many programs this process parsed, or `None` without the test-only
/// `test-counters` feature, so a test of the binary can see that a command
/// parses a file a fixed number of times.
#[doc(hidden)]
pub fn parse_count() -> Option<usize> {
    parse_counts().map(|(parses, _)| parses)
}

/// `(calls of parse_program, bytes of text their tokens cover)` in this
/// process, or `None` without the test-only `test-counters` feature. A test
/// compares the counts before and after a run, so that a pipeline that parses
/// a text again fails it.
#[doc(hidden)]
pub fn parse_counts() -> Option<(usize, usize)> {
    #[cfg(feature = "test-counters")]
    {
        use std::sync::atomic::Ordering::Relaxed;
        Some((PARSES.load(Relaxed), PARSED_BYTES.load(Relaxed)))
    }
    #[cfg(not(feature = "test-counters"))]
    None
}

#[cfg(test)]
mod try_parse_tests {
    use super::*;

    #[test]
    fn well_formed_source_parses() {
        let program = try_parse("MESSAGE \"x\".").expect("no panic expected");
        assert!(program.is_ok());
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn recovered_parse_errors_are_not_panics() {
        // The malformed fixture the rest of the suite uses: it recovers into
        // `Program.errors`, so `try_parse` must still return `Ok`.
        let program = try_parse("DEFINE VARIABLE .").expect("a parse error is not a panic");
        assert!(!program.errors.is_empty());
    }
}
