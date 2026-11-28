//! One lexed and parsed text, for every stage that reads it (`docs/design/shared-unit.md`).
//!
//! A unit is a text with its tokens, its tree and, when a stage asks, its line
//! table. There are two kinds, and they are two types so that a stage cannot
//! take one for the other:
//!
//! * [`RawUnit`] is the text as written, lexed and parsed with preprocessing
//!   off. Every span is a byte offset in the file. The formatter reads only
//!   this kind.
//! * [`ExpandedUnit`] is an [`ExpandedFile`] with the tokens and the tree of its
//!   expanded text. The analysis reads only this kind. Its spans are offsets in
//!   the expanded text.
//!
//! A unit does not borrow. It is `Send + Sync + 'static`, so a client can keep
//! it where it keeps other values that its threads share (a salsa query, a map
//! of the daemon). The command line keeps a unit only for the root that its
//! worker is checking.
//!
//! # Names
//!
//! `oxabl_daemon::db` has an `ExpandedUnit` of its own: an inspection type that
//! holds a root's expansion for a debug request. It is not this type, and the
//! two never meet in one module. This crate sits beneath the daemon, so the
//! daemon names this one with its path.

use std::sync::{Arc, OnceLock};

use oxabl_analyze::ExpandedFile;
use oxabl_common::{InternalPanic, SourceMap, catch_panic};
use oxabl_lexer::{Token, tokenize};
use oxabl_parser::{Parser, Program};

/// A text as written, with its tokens and its tree.
///
/// The tree is parsed with its comment table: the formatter needs it. The
/// statements and pools are the ones that a parse without the table gives, so
/// the analysis can read them when the expansion is the identity
/// ([`ExpandedFile::is_identity`]).
pub struct RawUnit {
    text: Arc<str>,
    tokens: Vec<Token>,
    program: Program,
    map: OnceLock<SourceMap>,
}

impl RawUnit {
    /// Lex and parse `text`.
    ///
    /// Unguarded: a panic of the lexer or the parser unwinds to the caller.
    /// Use [`RawUnit::try_parse`] where a panic must become a value.
    pub fn parse(text: impl Into<Arc<str>>) -> Self {
        let text = text.into();
        let tokens = tokenize(&text);
        let program = Parser::new(&tokens, &text).parse_program();
        RawUnit {
            text,
            tokens,
            program,
            map: OnceLock::new(),
        }
    }

    /// [`RawUnit::parse`] with the panic of the lexer or the parser contained.
    pub fn try_parse(text: impl Into<Arc<str>>) -> Result<Self, InternalPanic> {
        let text = text.into();
        catch_panic(|| RawUnit::parse(text))
    }

    /// The text as written.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The tokens of [`text`](Self::text), ending in the `Eof` token.
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// The tree of [`text`](Self::text), with its parse errors and comments.
    pub fn program(&self) -> &Program {
        &self.program
    }

    /// The line table of [`text`](Self::text), built on the first call.
    pub fn source_map(&self) -> &SourceMap {
        self.map.get_or_init(|| SourceMap::new(&self.text))
    }

    /// Whether the unit holds the lex and the parse of exactly `expanded`'s
    /// text: the expansion is the identity and the bytes are the same. The
    /// bytes are compared as well as the flag, so a unit made from another
    /// text is never used. The comparison is a `memcmp` that costs a small
    /// share of one lex.
    pub fn serves(&self, expanded: &ExpandedFile) -> bool {
        expanded.is_identity() && *self.text == *expanded.text
    }
}

/// An expansion with the tokens and the tree of its expanded text.
pub struct ExpandedUnit {
    expanded: Arc<ExpandedFile>,
    tokens: Vec<Token>,
    program: Program,
    map: OnceLock<SourceMap>,
}

impl ExpandedUnit {
    /// Lex and parse the expanded text of `expanded`. The tree has no comment
    /// table: no stage that reads an expansion reads comments.
    ///
    /// Unguarded, like [`RawUnit::parse`].
    pub fn parse(expanded: Arc<ExpandedFile>) -> Self {
        let tokens = tokenize(&expanded.text);
        let program = Parser::new(&tokens, &expanded.text)
            .without_comments()
            .parse_program();
        ExpandedUnit {
            expanded,
            tokens,
            program,
            map: OnceLock::new(),
        }
    }

    /// The expansion: the expanded text and its map back to the files.
    pub fn expanded(&self) -> &Arc<ExpandedFile> {
        &self.expanded
    }

    /// The tokens of the expanded text, ending in the `Eof` token.
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// The tree of the expanded text.
    pub fn program(&self) -> &Program {
        &self.program
    }

    /// The line table of the expanded text, built on the first call.
    pub fn source_map(&self) -> &SourceMap {
        self.map.get_or_init(|| SourceMap::new(&self.expanded.text))
    }
}

/// The [`RawUnit`] of a root that two stages may read, made by the stage that
/// needs it for a reason of its own.
///
/// The analysis reads the raw unit only when the root expands to itself, and
/// then it makes the unit with [`make`](Self::make). The format check runs
/// after it and asks [`made`](Self::made): it takes the unit when the analysis
/// made one, so an identity root is lexed and parsed once, and it formats the
/// text alone otherwise. A root with an include or a directive makes no unit
/// here, so it pays no copy of its text and keeps no second token vector and
/// tree alive during its analysis (rule 3 of the design).
///
/// [`make`](Self::make) gives `None` when the lexer or the parser panicked.
/// The stage then runs on the text alone, as before the unit existed, and its
/// own guard reports the panic the way it always did.
pub struct LazyRawUnit<'t> {
    text: &'t str,
    unit: OnceLock<Option<RawUnit>>,
}

impl<'t> LazyRawUnit<'t> {
    /// A lazy unit of `text`. Nothing is lexed or copied.
    pub fn new(text: &'t str) -> Self {
        LazyRawUnit {
            text,
            unit: OnceLock::new(),
        }
    }

    /// The text as written.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// The unit, made on the first call; `None` after a panic.
    pub fn make(&self) -> Option<&RawUnit> {
        self.unit
            .get_or_init(|| RawUnit::try_parse(self.text).ok())
            .as_ref()
    }

    /// The unit if a stage made it. Makes nothing.
    pub fn made(&self) -> Option<&RawUnit> {
        self.unit.get().and_then(Option::as_ref)
    }
}

// A client keeps a unit where its threads share it, so a field that is not
// `Send + Sync` must not compile.
const _: () = {
    const fn shared<T: Send + Sync + 'static>() {}
    shared::<RawUnit>();
    shared::<ExpandedUnit>();
};
