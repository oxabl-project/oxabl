//! Layout-only ("vertical") formatting engine for Progress ABL.
//!
//! `oxabl_formatter` consumes the fidelity substrate produced by the parser —
//! full-extent node spans, the sorted [`Program::comments`] side-table, and the
//! `blank_lines_between` primitive — and produces reformatted source that is
//! **idempotent** (`format(format(x)) == format(x)`) and **semantically
//! identical** to its input.
//!
//! The engine is a pure library: [`format()`] takes the original source, its
//! parsed [`Program`], and a resolved [`StyleGuide`] and returns either the
//! formatted string or a [`FormatBail`] explaining why the file was left
//! untouched. On any bail the **original bytes are returned unchanged** — the
//! formatter never emits a partial or best-effort rewrite.
//!
//! # What v1 does and does not do
//!
//! v1 is **layout only**: it fixes structural indentation, normalizes blank-line
//! runs, re-places comments by span, and — only when the [`StyleGuide`] opts in
//! — recases and expands/abbreviates keywords. It emits identifiers, literals,
//! and comment bodies **verbatim from the source by span**; it never renames or
//! rewrites them.
//!
//! ## Reflow is read but not enforced (intentional)
//!
//! [`StyleGuide::default_base`] ships `wrap_long_lines: true` and
//! `max_line_length: 120`, but **v1 does not enforce any reflow / width-driven
//! wrapping**. The width fields (`wrap_long_lines`, `max_line_length`,
//! `first_param_same_line`, `multi_param_threshold`, `where_placement`,
//! `and_or_placement`), the reorder rules (`using_sort`,
//! `class_structure_order`, `procedure_structure_order`), and the comment
//! content rewrite (`comment_style`) are read into the resolved config and
//! deliberately **not acted on**. A 200-column line stays 200 columns. This is
//! intent, not a bug: reflow and the doc-IR it needs are deferred to v2. This
//! keeps the v1 engine strictly "no-movement".
//!
//! # Formatting a range
//!
//! [`format_ranges`] gives the edits that lay out only the statements that a
//! selection touches, as the whole file lays them out. [`format()`] itself stays
//! whole-file: it never formats a region alone. [`format_ranges`] states which
//! statements a range touches.
//!
//! The `oxabl format` CLI, `--check`/`--stdout`, and `oxabl.toml` `[style]`
//! auto-discovery are a later slice and are not part of this crate.

mod attach;
mod blanks;
mod guard;
mod ir;
mod keyword_spelling;
mod keywords;
mod printer;
mod range;
mod tree;

pub use attach::{CommentMap, NodeComments, attach};
pub use range::{RangeBail, RangeEdit, RangeFailure, format_ranges, try_format_ranges};

use oxabl_common::{InternalPanic, SourceMap, catch_panic, panic_if_injected, panic_sites};
use oxabl_lexer::{Kind, Token};
use oxabl_parser::Program;
use oxabl_style::StyleGuide;
use std::fmt;

/// The single failure channel for [`format()`] (R6.5).
///
/// Bails are **whole-file**: on any variant, [`format()`] returns the input
/// source bytes unchanged. [`format()`] has no region-level partial formatting —
/// the contract is "a formatted string, or a reason and the exact bytes that
/// came in". A range is formatted by [`format_ranges`], which refuses a file
/// that has a bail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatBail {
    /// The [`Program`] carried parse errors; the tree is not trustworthy to
    /// re-emit, so the file is left untouched.
    ParseErrors,
    /// The semantic-preservation guard re-lexed the candidate output and found
    /// its non-trivia token stream differed from the input's. Rather than emit
    /// altered code, the formatter refuses.
    SemanticGuardTripped,
    /// The file only parses after preprocessor expansion, so raw-source spans
    /// are not real byte offsets and cannot be re-emitted faithfully.
    ExpansionDependent,
}

impl fmt::Display for FormatBail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            FormatBail::ParseErrors => {
                "file has parse errors; left unchanged (no partial formatting)"
            }
            FormatBail::SemanticGuardTripped => {
                "formatting would have altered the token stream; left unchanged"
            }
            FormatBail::ExpansionDependent => {
                "file only parses after preprocessor expansion; left unchanged"
            }
        };
        f.write_str(reason)
    }
}

impl std::error::Error for FormatBail {}

/// The failure channel for [`try_format_source`]: a deliberate [`FormatBail`],
/// or an [`InternalPanic`] contained by the guard.
///
/// This exists rather than an extra `FormatBail` variant because `FormatBail`
/// is a `PartialEq`, non-`#[non_exhaustive]` public enum that consumers compare
/// by value, so growing it is a semver-visible break. Keeping the panic arm in
/// a separate type also keeps a nested `Result` out of the public surface.
///
/// Unlike `FormatBail`, this type **is** `#[non_exhaustive]`, so it can grow a
/// variant later without repeating that mistake.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum FormatFailure {
    /// The formatter deliberately declined to rewrite the file. The original
    /// source bytes stand unchanged — the same contract as [`format()`].
    Bail(FormatBail),
    /// The formatter panicked. This is an oxabl bug, not a property of the
    /// input, and the caller should leave the file untouched and report it.
    Panic(InternalPanic),
}

impl fmt::Display for FormatFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatFailure::Bail(bail) => bail.fmt(f),
            FormatFailure::Panic(panic) => panic.fmt(f),
        }
    }
}

impl std::error::Error for FormatFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FormatFailure::Bail(bail) => Some(bail),
            FormatFailure::Panic(panic) => Some(panic),
        }
    }
}

impl From<FormatBail> for FormatFailure {
    fn from(bail: FormatBail) -> Self {
        FormatFailure::Bail(bail)
    }
}

impl From<InternalPanic> for FormatFailure {
    fn from(panic: InternalPanic) -> Self {
        FormatFailure::Panic(panic)
    }
}

/// Format ABL source, returning the reformatted string or a [`FormatBail`].
///
/// This is the pure, re-entrant entry point (R7.2). It reads the comment table
/// via [`Program::comments`], so there is no separate `comments` argument — the
/// table always comes from the same parse as the tree. `source` stays a
/// separate argument because [`Program`] does not hold it, and keeping it
/// explicit keeps the "raw, unexpanded source" offset contract visible at the
/// boundary: the caller must parse with preprocessing **off** so that every
/// span offset is a real byte offset into `source`.
///
/// On any [`FormatBail`], the returned error carries the reason and the caller
/// should treat the file as unchanged (the original `source` bytes).
///
/// See the [crate docs](crate) for the v1 layout-only scope and the reflow
/// no-op contract.
pub fn format(source: &str, program: &Program, style: &StyleGuide) -> Result<String, FormatBail> {
    if !program.errors.is_empty() {
        return Err(FormatBail::ParseErrors);
    }
    format_with_tokens(source, &oxabl_lexer::tokenize(source), program, style)
}

/// [`format()`] for a caller that holds the tokens of `source`.
///
/// `tokens` must be `oxabl_lexer::tokenize(source)`, the tokens that `program`
/// was parsed from. [`format()`] lexes `source` itself and calls this.
///
/// The function checks only that `tokens` end in the [`Kind::Eof`] token at the
/// end of `source`; tokens of another text fail that check and give
/// [`FormatBail::SemanticGuardTripped`]. Other tokens that do not match `source`
/// can still panic, so call [`try_format_source`] when you do not hold the
/// tokens of this exact text.
pub fn format_with_tokens(
    source: &str,
    tokens: &[Token],
    program: &Program,
    style: &StyleGuide,
) -> Result<String, FormatBail> {
    format_with_lines(source, tokens, program, style).map(|formatted| formatted.text)
}

/// The output of [`format_with_lines`]: the formatted text and where each
/// source line went.
pub(crate) struct LineFormat {
    pub text: String,
    /// Per source line (split at `\n`, as the printer does): the byte range of
    /// its output line in `text`, without the line ending, or `None` for a
    /// blank line the formatter dropped.
    pub lines: Vec<Option<(usize, usize)>>,
    /// Per source line: the earlier line whose leading indent its indent is
    /// measured against, if any.
    pub anchors: Vec<Option<usize>>,
}

/// Whether `tokens` end in the `Eof` token at the end of `source`, as the tokens
/// of `source` do.
fn tokens_end_source(source: &str, tokens: &[Token]) -> bool {
    tokens
        .last()
        .is_some_and(|t| t.kind == Kind::Eof && t.end == source.len())
}

/// [`format()`] with the map from source lines to output lines.
pub(crate) fn format_with_lines(
    source: &str,
    source_tokens: &[Token],
    program: &Program,
    style: &StyleGuide,
) -> Result<LineFormat, FormatBail> {
    if !program.errors.is_empty() {
        return Err(FormatBail::ParseErrors);
    }
    if !tokens_end_source(source, source_tokens) {
        return Err(FormatBail::SemanticGuardTripped);
    }
    let sm = SourceMap::new(source);
    let cmap = attach::attach(program, &sm);
    // The caller lexed `source` once. The printer shares the tokens for the
    // protected-line scan and the keyword transform, and the guard for the input
    // side of its comparison. Only the candidate is lexed again.
    let (mut buf, anchors) = printer::print(source, program, &cmap, style, source_tokens);
    blanks::normalize(&mut buf, style);
    let ending = ir::dominant_line_ending(source, &sm);
    let (out, lines) = buf.flush_mapped(
        style.indent_style,
        style.indent_size.max(1),
        ending,
        range::source_line_count(source),
    );

    // Semantic-preservation guard (U7): if re-lexing the candidate shows any
    // non-trivia drift, refuse to emit and bail whole-file (R6.3/R6.5). The
    // caller treats a bail as "file unchanged" — the original `source` bytes.
    if !guard::preserves_with_input_tokens(source, source_tokens, &out) {
        return Err(FormatBail::SemanticGuardTripped);
    }
    Ok(LineFormat {
        text: out,
        lines,
        anchors,
    })
}

/// One-call convenience: tokenize + parse `source` (raw, preprocessing off) and
/// [`format()`] the result, returning the reformatted string or a [`FormatBail`].
///
/// This folds the `tokenize → parse_program → format` dance every client
/// otherwise repeats, so the CLI, LSP, and the `oxabl` umbrella all format
/// through one shared entry point. On any bail the original bytes are unchanged
/// (see [`format()`]).
///
/// # Panics
///
/// Like the underlying lexer and parser, this may panic on some malformed
/// inputs, which is why it is deprecated in favor of [`try_format_source`].
#[deprecated(
    note = "may panic on malformed input; use `try_format_source`, which contains the panic"
)]
pub fn format_source(source: &str, style: &StyleGuide) -> Result<String, FormatBail> {
    format_source_inner(source, style)
}

/// Tokenize + parse + [`format()`] `source`, containing any internal panic.
///
/// The fallible sibling of `format_source` and the entry point new code should
/// reach for. Every outcome of the panicking twin is preserved exactly and the
/// guard only adds an arm:
///
/// - `Ok(String)` — the reformatted source.
/// - `Err(`[`FormatFailure::Bail`]`)` — the formatter declined; the caller
///   keeps the original bytes. Identical to the twin's `Err(FormatBail)`.
/// - `Err(`[`FormatFailure::Panic`]`)` — the formatter panicked. The caller
///   also keeps the original bytes, but this is an oxabl bug worth reporting,
///   not a property of the input.
///
/// # Platform caveat
///
/// The guard is a documented pass-through on `wasm32-unknown-unknown`; see
/// [`catch_panic`].
pub fn try_format_source(source: &str, style: &StyleGuide) -> Result<String, FormatFailure> {
    match catch_panic(|| format_source_inner(source, style)) {
        Ok(Ok(formatted)) => Ok(formatted),
        Ok(Err(bail)) => Err(FormatFailure::Bail(bail)),
        Err(panic) => Err(FormatFailure::Panic(panic)),
    }
}

/// The shared body, so `try_format_source` does not have to call its own
/// deprecated twin.
fn format_source_inner(source: &str, style: &StyleGuide) -> Result<String, FormatBail> {
    panic_if_injected(panic_sites::FORMAT, source);
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    format_with_tokens(source, &tokens, &program, style)
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_lexer::tokenize;
    use oxabl_parser::Parser;

    /// Parse `src` with preprocessing off (raw source == expanded source), the
    /// mode the formatter requires (R8).
    fn parse(src: &str) -> Program {
        let tokens = tokenize(src);
        Parser::new(&tokens, src).parse_program()
    }

    #[test]
    fn try_format_source_formats_a_clean_buffer() {
        let out = try_format_source(
            "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n",
            &StyleGuide::default_base(),
        )
        .expect("a clean buffer formats");
        assert!(out.contains("    MESSAGE"));
    }

    #[test]
    fn try_format_source_reports_a_bail_distinctly_from_a_panic() {
        // The existing bail fixture: parse-dirty input the formatter refuses.
        let err = try_format_source("IF THEN.", &StyleGuide::default_base())
            .expect_err("parse-dirty input bails");
        assert_eq!(err, FormatFailure::Bail(FormatBail::ParseErrors));
        assert!(
            !matches!(err, FormatFailure::Panic(_)),
            "a bail must never read as an internal panic"
        );
        // The bail's own message survives the extra wrapper.
        assert_eq!(err.to_string(), FormatBail::ParseErrors.to_string());
    }

    #[test]
    fn tokens_of_another_text_bail() {
        let src = "DO:\n  MESSAGE \"hi\".\nEND.\n";
        let program = parse(src);
        let style = StyleGuide::default_base();
        let longer = tokenize("DO:\n  MESSAGE \"hi there\".\nEND.\n");
        assert_eq!(
            format_with_tokens(src, &longer, &program, &style),
            Err(FormatBail::SemanticGuardTripped)
        );
        assert_eq!(
            format_with_tokens(src, &[], &program, &style),
            Err(FormatBail::SemanticGuardTripped)
        );
        assert!(format_with_tokens(src, &tokenize(src), &program, &style).is_ok());
    }

    #[test]
    fn empty_program_is_identity() {
        let src = "";
        let program = parse(src);
        assert_eq!(
            format(src, &program, &StyleGuide::default_base()),
            Ok(String::new())
        );
    }

    #[test]
    fn parse_errors_bail() {
        // An unterminated DO block (no matching END) produces parse errors.
        let src = "DO:\n  MESSAGE \"hi\".\n";
        let program = parse(src);
        assert!(
            !program.errors.is_empty(),
            "fixture should produce parse errors"
        );
        assert_eq!(
            format(src, &program, &StyleGuide::default_base()),
            Err(FormatBail::ParseErrors)
        );
    }

    #[test]
    fn format_bail_displays_reason() {
        assert!(!FormatBail::ParseErrors.to_string().is_empty());
        assert!(!FormatBail::SemanticGuardTripped.to_string().is_empty());
        assert!(!FormatBail::ExpansionDependent.to_string().is_empty());
    }

    // U8 — end-to-end pipeline assembly.

    #[test]
    fn end_to_end_comments_blanks_and_block() {
        let src = "\
/* header */



PROCEDURE greet:
DEFINE VARIABLE msg AS CHARACTER.
    MESSAGE msg. /* say it */
END.
";
        let program = parse(src);
        let out = format(src, &program, &StyleGuide::default_base()).unwrap();
        assert_eq!(
            out,
            "\
/* header */

PROCEDURE greet:
    DEFINE VARIABLE msg AS CHARACTER.
    MESSAGE msg. /* say it */
END.
"
        );
    }

    #[test]
    fn safe_default_preserves_keywords_end_to_end() {
        // default_base fixes indentation but preserves keyword spelling/casing.
        let src = "do:\nmessage \"x\".\nend.\n";
        let program = parse(src);
        let out = format(src, &program, &StyleGuide::default_base()).unwrap();
        assert_eq!(out, "do:\n    message \"x\".\nend.\n");
    }

    #[test]
    fn format_is_reentrant_across_threads() {
        // No global/mutable state: concurrent calls on different inputs must not
        // interfere (R7.2).
        let a = "DO:\nMESSAGE \"a\".\nEND.\n";
        let b = "DEFINE VARIABLE q AS INTEGER.\n";
        let ha = std::thread::spawn(move || {
            let p = parse(a);
            format(a, &p, &StyleGuide::default_base()).unwrap()
        });
        let hb = std::thread::spawn(move || {
            let p = parse(b);
            format(b, &p, &StyleGuide::oestandards()).unwrap()
        });
        let ra = ha.join().unwrap();
        let rb = hb.join().unwrap();
        assert_eq!(ra, "DO:\n    MESSAGE \"a\".\nEND.\n");
        assert_eq!(rb, "DEFINE VARIABLE q AS INTEGER.\n");
    }
}
