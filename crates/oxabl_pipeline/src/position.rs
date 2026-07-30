//! One derivation of line/column from a byte span, for the clients whose
//! surface is byte offsets (R13).
//!
//! Pipeline results carry byte spans and nothing else (KTD5). Two clients then
//! need human positions from them: the CLI's text rendering and the browser's
//! wire struct, which reports `{byte, line, column}` per endpoint. Both used to
//! derive that themselves from [`SourceMap`], which is one derivation too many —
//! a change to the column convention in one would silently disagree with the
//! other. This module is the single derivation, and it keeps the byte offsets
//! **alongside** the derived positions so nothing is lost in translation.
//!
//! Line and column are 1-based, and the column is a **byte** distance from the
//! start of the line — [`SourceMap`]'s convention, unchanged, so this helper is a
//! swap for the hand-rolled `SourceMap::lookup` pairs rather than a behavior
//! change.
//!
//! # The language server deliberately does not use this (KTD5)
//!
//! Do not "unify" the language server onto this helper: it would break UTF-16
//! positions. The LSP must emit positions in the encoding its client negotiated
//! — usually UTF-16 code units — and its `Rope`, the same one it uses for
//! incremental document sync, is the only oracle that can produce those. A
//! `SourceMap`-derived byte column is not a slower way to compute the LSP's
//! number; it is a *different* number, and it is wrong whenever a line contains
//! a non-ASCII character. The LSP therefore keeps its own position code, and
//! that exclusion is intentional rather than an oversight.
//!
//! # Scope
//!
//! A resolution is relative to the source text handed in, so the caller is
//! responsible for pairing a diagnostic with the file it came from — a span
//! whose [`FileId`](oxabl_common::FileId) belongs to an include file resolves
//! against the wrong text. `render_diagnostics` in `oxabl_common` makes that
//! check for text output; this helper stays a pure span-to-position conversion
//! and does not repeat it.

use oxabl_common::{Diagnostic, FileSpan, SourceMap};

/// One resolved endpoint: the byte offset that was asked about, plus the
/// 1-based line and byte column it lands on.
///
/// The byte offset is preserved rather than consumed, because the browser's wire
/// shape reports all three and a client that only kept line/column could never
/// map back to the buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// The byte offset into the source, exactly as it arrived.
    pub byte: u32,
    /// 1-based line number.
    pub line: usize,
    /// 1-based column, counted in **bytes** from the start of the line.
    pub column: usize,
}

/// A span resolved to its two endpoints.
///
/// Both endpoints are resolved, not just the start: the browser reports a range
/// so an editor can underline it. A zero-width span resolves to two equal
/// positions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedSpan {
    /// The span's start.
    pub start: Position,
    /// The span's end.
    pub end: Position,
}

/// Resolve one byte offset against `map`.
pub fn resolve_offset(map: &SourceMap, byte: u32) -> Position {
    let (line, column) = map.lookup(byte as usize);
    Position { byte, line, column }
}

/// Resolve a start/end byte pair against `map`.
pub fn resolve_offsets(map: &SourceMap, start: u32, end: u32) -> ResolvedSpan {
    ResolvedSpan {
        start: resolve_offset(map, start),
        end: resolve_offset(map, end),
    }
}

/// Resolve a [`FileSpan`]'s endpoints against `map`.
///
/// The file id is not consulted; see the scope note in the [module
/// docs](self) — the caller decides which source a span belongs to.
pub fn resolve_file_span(map: &SourceMap, span: &FileSpan) -> ResolvedSpan {
    resolve_offsets(map, span.span.start, span.span.end)
}

/// Resolve a [`Diagnostic`]'s primary span — the common case, so a client
/// rendering a diagnostic set never reaches inside `span.span` itself.
///
/// Takes an already-built `map` because both callers resolve *many* diagnostics
/// against *one* source, and rebuilding the line table per diagnostic would turn
/// a linear render into a quadratic one.
pub fn resolve_diagnostic(map: &SourceMap, diagnostic: &Diagnostic) -> ResolvedSpan {
    resolve_file_span(map, &diagnostic.span)
}

#[cfg(test)]
mod tests {
    use oxabl_ast::Span;
    use oxabl_common::FileId;

    use super::*;

    fn file_span(start: u32, end: u32) -> FileSpan {
        FileSpan {
            file: FileId::new(1),
            span: Span { start, end },
        }
    }

    #[test]
    fn a_first_line_span_resolves_to_line_one() {
        let source = "MESSAGE \"hi\".\n";
        let map = SourceMap::new(source);
        // `MESSAGE` — offsets 0..7, so column 1 through the 1-based convention.
        let resolved = resolve_offsets(&map, 0, 7);

        assert_eq!(resolved.start.line, 1);
        assert_eq!(resolved.start.column, 1);
        assert_eq!(resolved.end.line, 1);
        assert_eq!(resolved.end.column, 8);
    }

    #[test]
    fn a_span_after_several_newlines_resolves_to_the_right_line() {
        let source = "DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\n\nMESSAGE i.\n";
        let map = SourceMap::new(source);
        let start = source.find("MESSAGE").expect("fixture contains MESSAGE") as u32;
        let resolved = resolve_offsets(&map, start, start + 7);

        assert_eq!(resolved.start.line, 4, "three newlines precede it");
        assert_eq!(resolved.start.column, 1);
        assert_eq!(resolved.end.line, 4);
    }

    // The column is a byte distance, so a multi-byte character before the span
    // widens it. Pinned against `SourceMap` directly, since this helper's whole
    // job is to be the same answer.
    #[test]
    fn a_span_after_a_multibyte_character_is_a_byte_column() {
        let source = "/* é */ MESSAGE \"hi\".\n";
        let map = SourceMap::new(source);
        let start = source.find("MESSAGE").expect("fixture contains MESSAGE") as u32;
        let resolved = resolve_offset(&map, start);

        assert_eq!(resolved.line, 1);
        let (line, column) = map.lookup(start as usize);
        assert_eq!((resolved.line, resolved.column), (line, column));
        // Nine bytes precede `MESSAGE` but only eight characters: the two-byte
        // `é` counts twice, so a byte column is 10 where a character column
        // would be 9. That difference is exactly why the language server cannot
        // use this helper (KTD5).
        assert_eq!(resolved.column, 10);
    }

    #[test]
    fn a_zero_width_span_at_end_of_file_resolves_without_panicking() {
        let source = "MESSAGE \"hi\".\n";
        let map = SourceMap::new(source);
        let eof = source.len() as u32;
        let resolved = resolve_offsets(&map, eof, eof);

        assert_eq!(resolved.start, resolved.end);
        assert_eq!(resolved.start.byte, eof);
    }

    #[test]
    fn byte_offsets_are_preserved_alongside_the_derived_positions() {
        let source = "DEFINE VARIABLE i AS INTEGER NO-UNDO.\nMESSAGE i.\n";
        let map = SourceMap::new(source);
        let resolved = resolve_file_span(&map, &file_span(38, 45));

        assert_eq!(resolved.start.byte, 38, "the offset arrives unchanged");
        assert_eq!(resolved.end.byte, 45);
        assert_eq!(resolved.start.line, 2);
    }

    #[test]
    fn a_diagnostic_resolves_through_its_primary_span() {
        let source = "DEFINE VARIABLE unusedVar AS INTEGER NO-UNDO.\n";
        let map = SourceMap::new(source);
        let start = source.find("unusedVar").expect("fixture") as u32;
        let diagnostic = Diagnostic::error(
            "LINT9999",
            "synthetic".to_string(),
            file_span(start, start + 9),
        );

        let resolved = resolve_diagnostic(&map, &diagnostic);
        assert_eq!(resolved, resolve_file_span(&map, &diagnostic.span));
        assert_eq!(resolved.start.line, 1);
        assert_eq!(resolved.start.column, start as usize + 1);
    }
}
