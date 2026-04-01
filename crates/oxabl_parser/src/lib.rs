//! Parser for Progress ABL source code.
//!
//! Consumes tokens produced by [`oxabl_lexer`] and builds an AST using the
//! node types defined in [`oxabl_ast`].

mod literal;
mod parser;

pub use parser::{ParseError, ParseResult, Parser};
