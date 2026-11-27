//! [`FileInput`]: one parsed file as the analyses over its flow graphs read it.

use oxabl_ast::{AstPools, Statement};
use oxabl_lexer::Token;

/// One parsed file: its statements, the pools that hold their names and
/// expressions, its text and the tokens the statements were parsed from.
///
/// The four go together: a handle in `statements` is valid only with `pools`
/// (`docs/design/ast-invariants.md` §14), and the spans of both index
/// `source`. `tokens` are those of `oxabl_lexer::tokenize(source)`; an entry
/// point that accepts empty `tokens` says so.
#[derive(Clone, Copy)]
pub struct FileInput<'a> {
    pub statements: &'a [Statement],
    pub pools: &'a AstPools,
    pub source: &'a str,
    pub tokens: &'a [Token],
}

impl<'a> FileInput<'a> {
    pub fn new(
        statements: &'a [Statement],
        pools: &'a AstPools,
        source: &'a str,
        tokens: &'a [Token],
    ) -> Self {
        FileInput {
            statements,
            pools,
            source,
            tokens,
        }
    }
}
