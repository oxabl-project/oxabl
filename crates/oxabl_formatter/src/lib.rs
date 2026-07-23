//! Layout-only ("vertical") formatting engine for Progress ABL.
//!
//! `oxabl_formatter` consumes the fidelity substrate produced by the parser —
//! full-extent node spans, the sorted [`Program::comments`] side-table, and the
//! `blank_lines_between` primitive — and produces reformatted source that is
//! **idempotent** (`format(format(x)) == format(x)`) and **semantically
//! identical** to its input.
//!
//! The engine is a pure library: [`format`] takes the original source, its
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
//! The `oxabl format` CLI, `--check`/`--stdout`, and `oxabl.toml` `[style]`
//! auto-discovery are a later slice and are not part of this crate.

mod attach;
mod blanks;
mod guard;
mod ir;
mod keyword_spelling;
mod keywords;
mod printer;
mod tree;

pub use attach::{CommentMap, NodeComments, attach};

use oxabl_common::SourceMap;
use oxabl_parser::Program;
use oxabl_style::StyleGuide;
use std::fmt;

/// The single failure channel for [`format`] (R6.5).
///
/// Bails are **whole-file**: on any variant, [`format`] returns the input
/// source bytes unchanged. There is no region-level partial formatting — the
/// contract is "a formatted string, or a reason and the exact bytes that came
/// in".
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
    let sm = SourceMap::new(source);
    let cmap = attach::attach(program, &sm);
    // Lex `source` once and share the tokens: the printer needs them for the
    // protected-line scan and the keyword transform, and the guard needs them as
    // the input side of its comparison. A single pass here keeps `format()` at
    // two tokenizations total (this input pass + the guard's candidate pass)
    // rather than three.
    let source_tokens = oxabl_lexer::tokenize(source);
    let mut buf = printer::print(source, program, &cmap, style, &source_tokens);
    blanks::normalize(&mut buf, style);
    let ending = ir::dominant_line_ending(source);
    let out = buf.flush(style.indent_style, style.indent_size.max(1), ending);

    // Semantic-preservation guard (U7): if re-lexing the candidate shows any
    // non-trivia drift, refuse to emit and bail whole-file (R6.3/R6.5). The
    // caller treats a bail as "file unchanged" — the original `source` bytes.
    // The input side reuses `source_tokens`; only the candidate is lexed fresh.
    if !guard::preserves_with_input_tokens(source, &source_tokens, &out) {
        return Err(FormatBail::SemanticGuardTripped);
    }
    Ok(out)
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
