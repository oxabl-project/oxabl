//! Findings in an include fragment that depend on the including file.
//!
//! A context-free rule reads the text of one file. Run on an include fragment
//! alone, that text is expanded with no arguments and no includer defines: `{1}`
//! and `{&name}` are empty, and an `&IF DEFINED(...)` is decided without the
//! includer's `&GLOBAL-DEFINE`s. Most fragments give the same answer in every
//! includer. A statement that holds a reference, or sits under an `&IF`, may
//! not: `DEFINE {&mod} VARIABLE x ...` is `NEW SHARED` only where the includer
//! says so.
//!
//! [`IncludeDependence::scan`] finds those statements in the fragment's own
//! text. [`lower_confidence`] then changes a finding in one:
//!
//! - its help is the rule's generic help, never help chosen from a modifier that
//!   a reference supplies (TX006 asks for the `NEW SHARED` producer and every
//!   consumer only for a definition that says `SHARED` itself), and ends with
//!   [`INCLUDER_NOTE`];
//! - it is **not autofixable**: [`autofixable`] is `false` for it. A fix edits
//!   the fragment for every includer, and this run saw none of them.
//!
//! The finding is still reported. The run cannot see a finding that the
//! includer's arguments would add (a `RUN VALUE` under an `&IF` that is false
//! here), so the marking lowers confidence in what is shown, not in what is
//! missing.
//!
//! # Gating a fix
//!
//! No rule has an autofix yet. When one is added, it must ask [`autofixable`]
//! for the diagnostic first, and skip the fix when it answers `false`. The
//! answer is carried in the `help` text, so it survives every client and the
//! baseline. Replace [`autofixable`] with a typed field when a diagnostic gains
//! one; it is the only reader.

use std::ops::Range;

use oxabl_ast::Span;
use oxabl_common::Diagnostic;
use oxabl_lexer::{Kind, Lexer};

use crate::missing_no_undo::{self, SHARED_HELP};

/// The sentence that ends the help of a lower-confidence finding.
pub const INCLUDER_NOTE: &str = "This statement depends on the including file (a `{...}` \
     reference or an `&IF`), which was not seen. Check each includer before you change it.";

/// The statements of one include fragment whose text can change with the
/// includer.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IncludeDependence {
    ranges: Vec<Range<u32>>,
}

impl IncludeDependence {
    /// Find the statements of `source` that hold a `{&name}`, `{n}` or
    /// `{file.i}` reference, or that sit under an `&IF`.
    ///
    /// A statement ends at a period, or at a colon followed by white space (the
    /// end of a block header). That is a lexical reading: it does not need the
    /// fragment to parse.
    pub fn scan(source: &str) -> Self {
        let mut ranges = Vec::new();
        let mut depth = 0usize;
        let mut start: Option<usize> = None;
        let mut dependent = false;
        for token in Lexer::new(source) {
            let text = &source[token.start..token.end];
            let reference = text.len() > 1 && text.starts_with('{');
            let ends_statement = match token.kind {
                Kind::Eof | Kind::Period => true,
                Kind::Colon => source[token.end..]
                    .chars()
                    .next()
                    .is_none_or(char::is_whitespace),
                // A reference that stands alone on its line is a statement.
                _ if reference && start.is_none() => {
                    let rest = &source[token.end..];
                    rest.lines()
                        .next()
                        .is_none_or(|line| line.trim().is_empty())
                }
                _ => false,
            };
            let directive = matches!(
                token.kind,
                Kind::PreprocIf
                    | Kind::PreprocThen
                    | Kind::PreprocElse
                    | Kind::PreprocElseif
                    | Kind::PreprocEndif
            );
            match token.kind {
                // An `&IF` inside a statement makes it depend on the includer.
                Kind::PreprocIf => {
                    depth += 1;
                    dependent |= start.is_some();
                }
                Kind::PreprocEndif => depth = depth.saturating_sub(1),
                _ => {}
            }
            if token.kind != Kind::Eof && !directive {
                start.get_or_insert(token.start);
                dependent |= depth > 0 || reference;
            }
            if ends_statement {
                if let (true, Some(from)) = (dependent, start) {
                    ranges.push(from as u32..token.end as u32);
                }
                start = None;
                dependent = false;
            }
            if token.kind == Kind::Eof {
                break;
            }
        }
        IncludeDependence { ranges }
    }

    /// Whether the statement that holds `span` depends on the includer.
    pub fn covers(&self, span: Span) -> bool {
        let at = self.ranges.partition_point(|r| r.end <= span.start);
        self.ranges.get(at).is_some_and(|r| r.start <= span.start)
    }

    /// Whether no statement depends on the includer.
    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }
}

/// `d` as a lower-confidence finding: the rule's generic help, then
/// [`INCLUDER_NOTE`]. Applying it twice changes nothing more.
pub fn lower_confidence(mut d: Diagnostic) -> Diagnostic {
    if d.code.0 == crate::TX006 {
        d.help = Some(missing_no_undo::HELP.to_string());
    }
    let help = match d.help.take() {
        Some(help) if help.ends_with(INCLUDER_NOTE) => help,
        Some(help) => format!("{help} {INCLUDER_NOTE}"),
        None => INCLUDER_NOTE.to_string(),
    };
    d.help = Some(help);
    d
}

/// Whether an autofix may change the code for `d`.
///
/// `false` for a finding that [`lower_confidence`] changed, and for a TX006
/// finding on a `SHARED` definition, whose fix is in other files.
pub fn autofixable(d: &Diagnostic) -> bool {
    !d.help
        .as_deref()
        .is_some_and(|help| help == SHARED_HELP || help.ends_with(INCLUDER_NOTE))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn covered(source: &str) -> Vec<&str> {
        let found = IncludeDependence::scan(source);
        source
            .split_inclusive('\n')
            .scan(0usize, |at, line| {
                let start = *at;
                *at += line.len();
                Some((start, line.trim_end()))
            })
            .filter(|(start, line)| {
                let first = *start + (line.len() - line.trim_start().len());
                !line.is_empty()
                    && found.covers(Span {
                        start: first as u32,
                        end: first as u32 + 1,
                    })
            })
            .map(|(_, line)| line)
            .collect()
    }

    #[test]
    fn references_and_if_make_a_statement_dependent() {
        let src = "\
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE {&mod} VARIABLE b AS INTEGER.
DEFINE VARIABLE c AS INTEGER {&nu}.
DEFINE VARIABLE d AS {1}.
{inc.i}
&IF DEFINED(x) &THEN
RUN VALUE(y).
&ENDIF
RUN VALUE(z).
DEFINE VARIABLE e AS INTEGER &IF DEFINED(x) &THEN NO-UNDO &ENDIF.
";
        assert_eq!(
            covered(src),
            [
                "DEFINE {&mod} VARIABLE b AS INTEGER.",
                "DEFINE VARIABLE c AS INTEGER {&nu}.",
                "DEFINE VARIABLE d AS {1}.",
                "{inc.i}",
                "RUN VALUE(y).",
                "DEFINE VARIABLE e AS INTEGER &IF DEFINED(x) &THEN NO-UNDO &ENDIF.",
            ]
        );
    }

    #[test]
    fn a_block_header_and_its_body_are_separate_statements() {
        let src = "\
FOR EACH {&tt}:
    DEFINE VARIABLE a AS INTEGER.
END.
h:PREPARE-STRING.
";
        assert_eq!(covered(src), ["FOR EACH {&tt}:"]);
    }

    #[test]
    fn a_fragment_with_no_reference_has_no_dependent_statement() {
        assert!(IncludeDependence::scan("DEFINE VARIABLE a AS INTEGER.\nRUN VALUE(x).").is_empty());
    }

    #[test]
    fn lower_confidence_gives_the_generic_help_and_blocks_a_fix() {
        let d = |code: &'static str, help: Option<&str>| {
            let mut d = Diagnostic::warning(
                code,
                "m".to_string(),
                oxabl_common::FileSpan::from((
                    oxabl_common::FileId::UNKNOWN,
                    Span { start: 0, end: 0 },
                )),
            );
            d.help = help.map(str::to_string);
            d
        };
        assert!(autofixable(&d("TX006", Some(missing_no_undo::HELP))));
        assert!(!autofixable(&d("TX006", Some(SHARED_HELP))));

        let shared = lower_confidence(d("TX006", Some(SHARED_HELP)));
        assert_eq!(
            shared.help.as_deref(),
            Some(&*format!("{} {INCLUDER_NOTE}", missing_no_undo::HELP))
        );
        assert!(!autofixable(&shared));

        let other = lower_confidence(d("CL001", Some("Use a literal.")));
        assert_eq!(
            other.help.as_deref(),
            Some(&*format!("Use a literal. {INCLUDER_NOTE}"))
        );
        assert_eq!(lower_confidence(other.clone()), other);
        assert_eq!(
            lower_confidence(d("CL002", None)).help.as_deref(),
            Some(INCLUDER_NOTE)
        );
    }
}
