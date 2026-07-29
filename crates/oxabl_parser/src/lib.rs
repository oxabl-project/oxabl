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
