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
//! The daemon keeps one unit of each kind for each revision of a buffer, in
//! salsa queries (`oxabl_daemon::db`). Its inspection type for a debug request
//! is `ExpansionInspection`, so `ExpandedUnit` names only this type.
//!
//! # Cancellation
//!
//! [`RawUnit::parse`] and [`ExpandedUnit::parse`] never stop. A client whose
//! request an edit may cancel (the daemon) must make a unit with
//! [`RawUnit::parse_between`] or [`ExpandedUnit::parse_between`]: they call its
//! checkpoint after the lex, before each top-level statement and after the
//! parse, as `collect_from_expanded_with_summaries` does.

use std::sync::{Arc, OnceLock};

use oxabl_analyze::ExpandedFile;
use oxabl_common::{InternalPanic, SourceMap, catch_panic, panic_if_injected, panic_sites};
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
        Self::parse_between(text, || {})
    }

    /// [`RawUnit::parse`], calling `between` after the lex, before each
    /// top-level statement and after the parse. A caller that may stop
    /// waiting unwinds from it. Unguarded, like [`RawUnit::parse`].
    pub fn parse_between(text: impl Into<Arc<str>>, mut between: impl FnMut()) -> Self {
        let text = text.into();
        panic_if_injected(panic_sites::UNIT, &text);
        let tokens = tokenize(&text);
        between();
        let program = Parser::new(&tokens, &text).parse_program_between(&mut between);
        between();
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
    parse: ExpandedParse,
    map: OnceLock<SourceMap>,
}

/// Where the tokens and the tree of an [`ExpandedUnit`] are.
enum ExpandedParse {
    /// Lexed and parsed for this unit.
    Own {
        tokens: Vec<Token>,
        program: Box<Program>,
    },
    /// The raw unit of the same text: the expansion is the identity.
    Raw(Arc<RawUnit>),
}

impl ExpandedUnit {
    /// Lex and parse the expanded text of `expanded`. The tree has no comment
    /// table: no stage that reads an expansion reads comments.
    ///
    /// Unguarded, like [`RawUnit::parse`].
    pub fn parse(expanded: Arc<ExpandedFile>) -> Self {
        Self::parse_between(expanded, || {})
    }

    /// [`ExpandedUnit::parse`], calling `between` as
    /// [`RawUnit::parse_between`] does.
    pub fn parse_between(expanded: Arc<ExpandedFile>, mut between: impl FnMut()) -> Self {
        let tokens = tokenize(&expanded.text);
        between();
        let program = Parser::new(&tokens, &expanded.text)
            .without_comments()
            .parse_program_between(&mut between);
        between();
        ExpandedUnit {
            expanded,
            parse: ExpandedParse::Own {
                tokens,
                program: Box::new(program),
            },
            map: OnceLock::new(),
        }
    }

    /// The unit of `expanded` that reads the tokens and the tree of `raw`,
    /// with no lex or parse of its own. `None` unless `raw`
    /// [serves](RawUnit::serves) `expanded`.
    pub fn of_raw(expanded: Arc<ExpandedFile>, raw: Arc<RawUnit>) -> Option<Self> {
        raw.serves(&expanded).then(|| ExpandedUnit {
            expanded,
            parse: ExpandedParse::Raw(raw),
            map: OnceLock::new(),
        })
    }

    /// The raw unit that this unit reads, when it was made with
    /// [`of_raw`](Self::of_raw).
    pub fn raw(&self) -> Option<&Arc<RawUnit>> {
        match &self.parse {
            ExpandedParse::Own { .. } => None,
            ExpandedParse::Raw(raw) => Some(raw),
        }
    }

    /// The expansion: the expanded text and its map back to the files.
    pub fn expanded(&self) -> &Arc<ExpandedFile> {
        &self.expanded
    }

    /// The tokens of the expanded text, ending in the `Eof` token.
    pub fn tokens(&self) -> &[Token] {
        match &self.parse {
            ExpandedParse::Own { tokens, .. } => tokens,
            ExpandedParse::Raw(raw) => raw.tokens(),
        }
    }

    /// The tree of the expanded text. When the unit reads a raw unit, the
    /// tree has the comment table of that parse too.
    pub fn program(&self) -> &Program {
        match &self.parse {
            ExpandedParse::Own { program, .. } => program,
            ExpandedParse::Raw(raw) => raw.program(),
        }
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
/// When the lexer or the parser panics, the unit keeps the contained
/// [`InternalPanic`]. Each stage reports it as its own failure and none lexes
/// or parses the text again: the text would panic the same way.
///
/// The raw unit parses with the comment table, which the analysis alone did
/// not. A panic that only the comment table causes now fails the analysis as
/// well as the format check. This can only add failures, never remove one, and
/// it is accepted to avoid a second lex and parse.
pub struct LazyRawUnit<'t> {
    text: &'t str,
    unit: OnceLock<Result<RawUnit, InternalPanic>>,
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

    /// The unit, made on the first call; the contained panic of the lexer or
    /// the parser when it panicked.
    pub fn make(&self) -> Result<&RawUnit, &InternalPanic> {
        self.unit
            .get_or_init(|| RawUnit::try_parse(self.text))
            .as_ref()
    }

    /// What a stage made, if one did: the unit, or the panic. Makes nothing.
    pub fn made(&self) -> Option<Result<&RawUnit, &InternalPanic>> {
        self.unit.get().map(Result::as_ref)
    }
}

/// `(lexed bytes, parses, parsed bytes)` in this process, in a build of the
/// lexer and the parser with their test-only `test-counters` feature, and
/// `None` in any other build. A test of the language server reads it through
/// the daemon, to see that one revision of a buffer is lexed and parsed once.
#[doc(hidden)]
pub fn lex_parse_counts() -> Option<(usize, usize, usize)> {
    let (_, lexed, _) = oxabl_lexer::lex_counts_if_counted()?;
    let (parses, parsed) = oxabl_parser::parse_counts()?;
    Some((lexed, parses, parsed))
}

// A client keeps a unit where its threads share it, so a field that is not
// `Send + Sync` must not compile.
const _: () = {
    const fn shared<T: Send + Sync + 'static>() {}
    shared::<RawUnit>();
    shared::<ExpandedUnit>();
};
