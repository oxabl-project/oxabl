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
//! text, and [`IncludeDependence::dependence`] answers for the statement of one
//! finding, reading only what the rule reads:
//!
//! - a reference is `{&name}`, `{n}`, `{*}` or `{file.i <args>}`. An
//!   argument-less `{file.i}` counts only when it did not resolve or its own
//!   text has a reference or an `&IF` (an [`IncludeProbe`] answers). Otherwise
//!   it expands to the same text in every includer that finds the file;
//! - `RUN VALUE(...)` (CL001) reads its target, so only a reference or `&IF`
//!   in the target counts. The definition rules (TX006, CL005) read the
//!   modifier slot, the type and the options, so a reference anywhere in the
//!   statement counts;
//! - a statement under an `&IF` counts, except under an include guard (an
//!   `&IF DEFINED(x) = 0 &THEN` around the whole file that defines `x`);
//! - a statement ends at a period, a block-header colon, the end of a
//!   `&SCOPED-DEFINE` or `&GLOBAL-DEFINE` line, or a reference alone on its
//!   line.
//!
//! [`lower_confidence`] then changes a finding in such a statement:
//!
//! - its help ends with [`INCLUDER_NOTE`]. TX006 chooses the rest from the
//!   modifier slot of the `DEFINE` ([`Slot`]): a literal `SHARED` keeps the
//!   producer-and-consumers help, because the definition is one side of a
//!   pair in every includer; a reference or `&IF` in the slot gets a third help
//!   (an includer may make it `SHARED`); never the plain help from a modifier
//!   that a reference supplies;
//! - it is **not autofixable**: [`autofixable`] is `false` for it. A fix edits
//!   the fragment for every includer, and this run saw none of them.
//!
//! The finding is still reported. The run cannot see a finding that the
//! includer's arguments would add (a `RUN VALUE` under an `&IF` that is false
//! here), so the marking lowers confidence in what is shown, not in what is
//! missing.
//!
//! Not confirmed against an OpenEdge install: that an include guard around the
//! whole file gives one answer for every includer. It does when each includer
//! includes the file once, and a second inclusion expands to nothing. See
//! `docs/design/strict-analysis-open-questions.md`.
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
use oxabl_lexer::{Kind, Lexer, Token};

use crate::missing_no_undo::{self, SHARED_HELP};

/// The sentence that ends the help of a lower-confidence finding.
pub const INCLUDER_NOTE: &str = "This statement depends on the including file (a `{...}` \
     reference or an `&IF`), which was not seen. Check each includer before you change it.";

/// Answers what only the file system and the expansion know: whether an
/// argument-less `{file.i}` can change with the includer.
pub trait IncludeProbe {
    /// Whether the include written at `site` (the `{file.i}` token, in the
    /// fragment's own text) did not resolve, or has a reference or an `&IF` in
    /// its own text. Such an include may expand to something else in another
    /// includer.
    fn depends_on_includer(&self, site: Span) -> bool;
}

impl<F: Fn(Span) -> bool> IncludeProbe for F {
    fn depends_on_includer(&self, site: Span) -> bool {
        self(site)
    }
}

/// An argument-less `{file.i}` in a statement.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Include {
    site: Span,
    in_run_target: bool,
}

/// One statement of the fragment's own text that can change with the includer.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Dependent {
    range: Range<u32>,
    /// The statement starts under an `&IF`.
    under_if: bool,
    /// A `{&name}`, `{n}`, `{*}` or `{file.i <args>}` reference, or an `&IF`
    /// inside the statement.
    reference: bool,
    /// The same, in the target of a `RUN VALUE(...)`.
    in_run_target: bool,
    includes: Vec<Include>,
    slot: Slot,
}

/// Where a finding's statement depends on the includer, for the rules that
/// need more than a yes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dependence {
    /// What the modifier slot of a `DEFINE` holds.
    pub slot: Slot,
}

/// The modifier slot of a `DEFINE VARIABLE` or `DEFINE TEMP-TABLE`: the words
/// between `DEFINE` and the keyword that says what is defined. `SHARED` goes
/// there.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Slot {
    /// No reference in it, and no literal `SHARED`; or not a definition that
    /// can be shared.
    #[default]
    Plain,
    /// It says `SHARED` in the fragment's own text.
    LiteralShared,
    /// It holds a reference or an `&IF`, and no literal `SHARED`: an includer
    /// may make the definition `SHARED`.
    Reference,
}

/// The statements of one include fragment whose text can change with the
/// includer.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IncludeDependence {
    statements: Vec<Dependent>,
    /// A reference or an `&IF` anywhere, define lines and guards aside.
    touched: bool,
}

/// Where the scan is in the target of a `RUN VALUE(...)`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Run {
    Outside,
    /// After `RUN`: the next word is `VALUE` or a static target.
    Keyword,
    /// Inside the parentheses of `VALUE(...)`.
    Value {
        depth: u32,
    },
}

/// The words after `DEFINE`, until the one that says what is defined.
#[derive(Clone, Copy, PartialEq, Eq)]
enum SlotScan {
    Closed,
    Open { words: u8 },
}

#[derive(Default)]
struct Current {
    start: Option<usize>,
    under_if: bool,
    reference: bool,
    in_run_target: bool,
    includes: Vec<Include>,
    literal_shared: bool,
    slot_reference: bool,
    shareable: bool,
    /// `&IF` depth at the `DEFINE`: a `SHARED` deeper is conditional text.
    define_depth: usize,
}

impl IncludeDependence {
    /// Find the statements of `source` that hold a `{&name}`, `{n}`, `{*}` or
    /// `{file.i <args>}` reference or an argument-less `{file.i}`, or that sit
    /// under an `&IF`.
    ///
    /// A statement ends at a period, at a colon followed by white space (the
    /// end of a block header), at the end of a `&SCOPED-DEFINE` or
    /// `&GLOBAL-DEFINE` line, and at a reference that stands alone on its line.
    /// That is a lexical reading: it does not need the fragment to parse.
    ///
    /// An include guard is no dependence: an `&IF DEFINED(x) = 0 &THEN` that
    /// encloses the whole file and defines `x` gives the same answer in every
    /// includer that includes the file once, and the first inclusion is the
    /// one that has the text. The `&IF` condition is never part of a statement.
    ///
    /// Whether an argument-less include matters is asked of an
    /// [`IncludeProbe`] when a finding needs the answer, in [`dependence`].
    ///
    /// [`dependence`]: Self::dependence
    pub fn scan(source: &str) -> Self {
        let tokens: Vec<Token> = Lexer::new(source).collect();
        let guard = find_guard(source, &tokens);
        let skipped = |i: usize| guard.is_some_and(|g| i <= g.then || i == g.endif);

        let mut out = IncludeDependence::default();
        let mut depth = 0usize;
        let mut cur = Current::default();
        let mut condition = false;
        let mut run = Run::Outside;
        let mut slot = SlotScan::Closed;
        // A `&SCOPED-DEFINE` or `&GLOBAL-DEFINE` line, apart from the statement
        // it may interrupt.
        let mut define_line = false;

        for (i, token) in tokens.iter().enumerate() {
            if skipped(i) {
                continue;
            }
            let text = &source[token.start..token.end];
            match token.kind {
                Kind::Eof => {
                    out.finish(&mut cur, token.start);
                    break;
                }
                Kind::PreprocIf | Kind::PreprocElseif => {
                    condition = true;
                    if token.kind == Kind::PreprocIf {
                        depth += 1;
                        out.touched = true;
                        // An `&IF` inside a statement makes it depend on the
                        // includer.
                        if cur.start.is_some() {
                            cur.reference = true;
                            cur.in_run_target |= run != Run::Outside;
                            cur.slot_reference |= matches!(slot, SlotScan::Open { .. });
                        }
                    }
                    continue;
                }
                Kind::PreprocThen => {
                    condition = false;
                    continue;
                }
                Kind::PreprocElse => continue,
                Kind::PreprocEndif => {
                    depth = depth.saturating_sub(1);
                    continue;
                }
                _ if condition => continue,
                Kind::PreprocGlobalDefine | Kind::PreprocScopedDefine | Kind::PreprocUndefine => {
                    define_line = true;
                    continue;
                }
                Kind::PreprocEnd => {
                    define_line = false;
                    continue;
                }
                _ => {}
            }

            let reference = match token.kind {
                Kind::Preprop | Kind::IncludeArgReference => true,
                // `{*}` is three tokens.
                Kind::LeftBrace => tokens.get(i + 1).is_some_and(|t| t.kind == Kind::Star),
                Kind::IncludeReference => include_has_arguments(text),
                _ => false,
            };
            if define_line {
                out.touched |= reference;
                continue;
            }
            let argless_include = token.kind == Kind::IncludeReference && !reference;
            out.touched |= reference;

            // A reference that stands alone on its line is a statement of its
            // own. Nothing in it is a finding.
            if (reference || argless_include) && cur.start.is_none() {
                let rest = &source[token.end..];
                if rest
                    .lines()
                    .next()
                    .is_none_or(|line| line.trim().is_empty())
                {
                    continue;
                }
            }

            let in_run_target = match run {
                Run::Outside => false,
                Run::Keyword => {
                    run = if token.kind == Kind::Value {
                        Run::Value { depth: 0 }
                    } else {
                        Run::Outside
                    };
                    false
                }
                Run::Value { depth } => {
                    run = match token.kind {
                        Kind::LeftParen => Run::Value { depth: depth + 1 },
                        Kind::RightParen if depth <= 1 => Run::Outside,
                        Kind::RightParen => Run::Value { depth: depth - 1 },
                        _ => run,
                    };
                    true
                }
            };
            if token.kind == Kind::Run {
                run = Run::Keyword;
            }

            if cur.start.is_none() {
                cur.start = Some(token.start);
                // An `&IF` that opens inside the statement is a reference.
                cur.under_if = depth > 0;
            }
            if reference {
                cur.reference = true;
                cur.in_run_target |= in_run_target;
            }
            if argless_include {
                cur.includes.push(Include {
                    site: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                    in_run_target,
                });
            }
            scan_slot(&mut cur, &mut slot, token.kind, reference, depth);

            let ends_statement = match token.kind {
                Kind::Period => true,
                Kind::Colon => source[token.end..]
                    .chars()
                    .next()
                    .is_none_or(char::is_whitespace),
                _ => false,
            };
            if ends_statement {
                out.finish(&mut cur, token.end);
                run = Run::Outside;
                slot = SlotScan::Closed;
            }
        }
        out
    }

    fn finish(&mut self, cur: &mut Current, end: usize) {
        let done = std::mem::take(cur);
        let Some(start) = done.start else { return };
        if !(done.under_if || done.reference || !done.includes.is_empty()) {
            return;
        }
        let slot = match (done.shareable, done.literal_shared, done.slot_reference) {
            (true, true, _) => Slot::LiteralShared,
            (true, false, true) => Slot::Reference,
            _ => Slot::Plain,
        };
        self.statements.push(Dependent {
            range: start as u32..end as u32,
            under_if: done.under_if,
            reference: done.reference,
            in_run_target: done.in_run_target,
            includes: done.includes,
            slot,
        });
    }

    /// Whether the text holds a reference, or an `&IF` that is no include
    /// guard. An argument-less `{file.i}` is not one.
    pub fn has_reference(&self) -> bool {
        self.touched
    }

    /// How the statement that holds a finding of rule `code` at `span`
    /// depends on the includer, or `None` when it does not.
    ///
    /// Each rule reads some of the statement. `RUN VALUE(...)` (CL001) reads
    /// the target, so a reference elsewhere in the statement (an argument,
    /// `NO-ERROR`) does not matter. The definition rules (TX006, CL005) read
    /// the modifier slot, the type and the options, which is the statement.
    /// A statement under an `&IF` may not be there in another includer, so it
    /// always counts.
    pub fn dependence(
        &self,
        code: &str,
        span: Span,
        probe: &dyn IncludeProbe,
    ) -> Option<Dependence> {
        let at = self
            .statements
            .partition_point(|s| s.range.end <= span.start);
        let s = self
            .statements
            .get(at)
            .filter(|s| s.range.start <= span.start)?;
        let target_only = code == crate::CL001;
        let depends = s.under_if
            || if target_only {
                s.in_run_target
            } else {
                s.reference
            }
            || s.includes
                .iter()
                .filter(|i| i.in_run_target || !target_only)
                .any(|i| probe.depends_on_includer(i.site));
        depends.then_some(Dependence { slot: s.slot })
    }

    /// Whether no statement can depend on the includer.
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}

/// Track the modifier slot of the `DEFINE` the statement holds.
fn scan_slot(cur: &mut Current, slot: &mut SlotScan, kind: Kind, reference: bool, depth: usize) {
    /// More words than any modifier list holds.
    const MAX_WORDS: u8 = 8;
    if kind == Kind::Define {
        *slot = SlotScan::Open { words: 0 };
        cur.literal_shared = false;
        cur.slot_reference = false;
        cur.shareable = false;
        cur.define_depth = depth;
        return;
    }
    let SlotScan::Open { words } = slot else {
        return;
    };
    match kind {
        Kind::Variable | Kind::TempTable => {
            cur.shareable = true;
            *slot = SlotScan::Closed;
        }
        Kind::Parameter | Kind::Property | Kind::Buffer => *slot = SlotScan::Closed,
        _ => {
            cur.literal_shared |= kind == Kind::Shared && depth == cur.define_depth;
            cur.slot_reference |= reference;
            *words += 1;
            if *words > MAX_WORDS {
                *slot = SlotScan::Closed;
            }
        }
    }
}

/// Whether an include reference has arguments (`{file.i &a=1}`, `{file.i x}`)
/// or a reference in its name (`{dir/{&d}/file.i}`).
fn include_has_arguments(text: &str) -> bool {
    let inner = text
        .strip_prefix('{')
        .and_then(|t| t.strip_suffix('}'))
        .unwrap_or(text)
        .trim();
    if inner.contains('{') {
        return true;
    }
    let rest = match inner.chars().next() {
        Some(q @ ('"' | '\'')) => inner[1..].split_once(q).map_or("", |(_, rest)| rest),
        _ => inner.split_once(char::is_whitespace).map_or("", |(_, r)| r),
    };
    !rest.trim().is_empty()
}

/// The token indexes of an include guard: `&IF DEFINED(x) = 0 &THEN` (or `&IF
/// NOT DEFINED(x) &THEN`) as the first token, its `&ENDIF` as the last, no
/// `&ELSE` between, and a `&GLOBAL-DEFINE` or `&SCOPED-DEFINE` of `x` inside.
#[derive(Clone, Copy)]
struct Guard {
    /// The `&THEN`: the guard is every token up to it, the `&IF` and the
    /// condition included.
    then: usize,
    endif: usize,
}

fn find_guard(source: &str, tokens: &[Token]) -> Option<Guard> {
    if tokens.first()?.kind != Kind::PreprocIf {
        return None;
    }
    let then = tokens.iter().position(|t| t.kind == Kind::PreprocThen)?;
    let condition: String = tokens[1..then]
        .iter()
        .map(|t| source[t.start..t.end].to_ascii_uppercase())
        .collect();
    let name = condition
        .strip_prefix("NOTDEFINED(")
        .or_else(|| condition.strip_prefix("DEFINED("))?;
    let (name, tail) = name.split_once(')')?;
    let negated = condition.starts_with("NOT");
    let tail_ok = if negated {
        tail.is_empty()
    } else {
        matches!(tail, "=0" | "EQ0")
    };
    if !tail_ok || name.is_empty() {
        return None;
    }

    let mut depth = 0usize;
    let mut endif = None;
    for (i, t) in tokens.iter().enumerate() {
        match t.kind {
            Kind::PreprocIf => depth += 1,
            Kind::PreprocElse | Kind::PreprocElseif if depth == 1 => return None,
            Kind::PreprocEndif => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    endif = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let endif = endif?;
    if tokens[endif + 1..].iter().any(|t| t.kind != Kind::Eof) {
        return None;
    }
    let defines = tokens[then..endif].windows(2).any(|w| {
        matches!(
            w[0].kind,
            Kind::PreprocGlobalDefine | Kind::PreprocScopedDefine
        ) && source[w[1].start..w[1].end].eq_ignore_ascii_case(name)
    });
    defines.then_some(Guard { then, endif })
}

/// `d` as a lower-confidence finding: the rule's help, then [`INCLUDER_NOTE`].
/// Applying it twice changes nothing more.
///
/// TX006 chooses its help from what the modifier slot says ([`Slot`]):
///
/// - a literal `SHARED` keeps [`SHARED_HELP`]: the definition is a producer
///   or a consumer in every includer, so the advice to change one side alone
///   is unsafe;
/// - a reference or an `&IF` in the slot, and no literal `SHARED`, gets
///   [`MAYBE_SHARED_HELP`]: an includer may make the definition `SHARED`;
/// - otherwise the plain help.
///
/// The plain help is never chosen from a modifier that a reference supplies.
pub fn lower_confidence(mut d: Diagnostic, dependence: Dependence) -> Diagnostic {
    if d.code.0 == crate::TX006 {
        let help = match dependence.slot {
            Slot::LiteralShared => SHARED_HELP,
            Slot::Reference => MAYBE_SHARED_HELP,
            Slot::Plain => missing_no_undo::HELP,
        };
        d.help = Some(help.to_string());
    }
    let help = match d.help.take() {
        Some(help) if help.ends_with(INCLUDER_NOTE) => help,
        Some(help) => format!("{help} {INCLUDER_NOTE}"),
        None => INCLUDER_NOTE.to_string(),
    };
    d.help = Some(help);
    d
}

/// The help of a TX006 finding whose modifier slot an includer fills.
pub const MAYBE_SHARED_HELP: &str = "If an includer makes this definition `SHARED`, add `NO-UNDO` \
     to the `NEW SHARED` producer and to every `SHARED` consumer; changing one side alone makes \
     the definitions disagree. If none does, add `NO-UNDO` to the definition.";

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

    /// The lines of `source` whose statement depends on the includer for a
    /// finding of rule `code`, with `probe` answering for argument-less
    /// includes.
    fn covered<'a>(source: &'a str, code: &str, probe: &dyn IncludeProbe) -> Vec<&'a str> {
        let found = IncludeDependence::scan(source);
        lines(source)
            .filter(|(start, line)| {
                let first = *start + (line.len() - line.trim_start().len());
                found
                    .dependence(
                        code,
                        Span {
                            start: first as u32,
                            end: first as u32 + 1,
                        },
                        probe,
                    )
                    .is_some()
            })
            .map(|(_, line)| line)
            .collect()
    }

    fn lines(source: &str) -> impl Iterator<Item = (usize, &str)> {
        source
            .split_inclusive('\n')
            .scan(0usize, |at, line| {
                let start = *at;
                *at += line.len();
                Some((start, line.trim_end()))
            })
            .filter(|(_, line)| !line.is_empty())
    }

    const NEVER: &dyn IncludeProbe = &|_: Span| false;
    const ALWAYS: &dyn IncludeProbe = &|_: Span| true;

    fn slot_of(line: &str) -> Slot {
        let found = IncludeDependence::scan(line);
        found
            .dependence(crate::TX006, Span { start: 0, end: 1 }, ALWAYS)
            .map_or(Slot::Plain, |d| d.slot)
    }

    #[test]
    fn references_and_if_make_a_statement_dependent() {
        let src = "\
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE {&mod} VARIABLE b AS INTEGER.
DEFINE VARIABLE c AS INTEGER {&nu}.
DEFINE VARIABLE d AS {1}.
DEFINE VARIABLE e AS INTEGER EXTENT {*}.
&IF DEFINED(x) &THEN
RUN VALUE(y).
&ENDIF
RUN VALUE(z).
DEFINE VARIABLE f AS INTEGER &IF DEFINED(x) &THEN NO-UNDO &ENDIF.
";
        assert_eq!(
            covered(src, crate::TX006, NEVER),
            [
                "DEFINE {&mod} VARIABLE b AS INTEGER.",
                "DEFINE VARIABLE c AS INTEGER {&nu}.",
                "DEFINE VARIABLE d AS {1}.",
                "DEFINE VARIABLE e AS INTEGER EXTENT {*}.",
                "RUN VALUE(y).",
                "DEFINE VARIABLE f AS INTEGER &IF DEFINED(x) &THEN NO-UNDO &ENDIF.",
            ]
        );
    }

    #[test]
    fn an_include_with_arguments_always_counts() {
        let src = "DEFINE VARIABLE a AS {inc.i &t=1}.\nDEFINE VARIABLE b AS {\"x y.i\" 5}.\n\
                   DEFINE VARIABLE c AS {dir/{&d}/t.i}.\n";
        assert_eq!(covered(src, crate::TX006, NEVER).len(), 3);
    }

    #[test]
    fn an_include_without_arguments_counts_when_the_probe_says_so() {
        let src = "DEFINE VARIABLE a AS INTEGER {inc.i}.\nDEFINE VARIABLE b AS INTEGER.\n";
        assert!(covered(src, crate::TX006, NEVER).is_empty());
        assert_eq!(
            covered(src, crate::TX006, ALWAYS),
            ["DEFINE VARIABLE a AS INTEGER {inc.i}."]
        );
        // Alone on its line it is a statement of its own, with no finding.
        let alone = "{inc.i}\nDEFINE VARIABLE b AS INTEGER.\n";
        assert!(covered(alone, crate::TX006, ALWAYS).is_empty());
    }

    #[test]
    fn a_run_target_alone_decides_for_run_value() {
        let src = "\
RUN VALUE(c) (INPUT {&arg}).
RUN VALUE(c) {&opts}.
RUN VALUE(c + {&sfx}).
RUN VALUE(f(g(1), {&x})).
RUN VALUE(f({inc.i})).
IF x THEN RUN VALUE(c) (INPUT {&a}).
";
        let cl001 = |probe| covered(src, crate::CL001, probe);
        assert_eq!(
            cl001(NEVER),
            ["RUN VALUE(c + {&sfx}).", "RUN VALUE(f(g(1), {&x}))."]
        );
        assert_eq!(
            cl001(ALWAYS),
            [
                "RUN VALUE(c + {&sfx}).",
                "RUN VALUE(f(g(1), {&x})).",
                "RUN VALUE(f({inc.i}))."
            ]
        );
        // A rule that reads the whole statement counts every reference.
        assert_eq!(covered(src, crate::CL002, NEVER).len(), 5);
    }

    #[test]
    fn an_if_in_a_run_target_counts_for_run_value() {
        let src = "RUN VALUE(&IF DEFINED(x) &THEN a &ELSE b &ENDIF).\nRUN VALUE(c) &IF DEFINED(x) &THEN NO-ERROR &ENDIF.\n";
        assert_eq!(
            covered(src, crate::CL001, NEVER),
            ["RUN VALUE(&IF DEFINED(x) &THEN a &ELSE b &ENDIF)."]
        );
    }

    #[test]
    fn a_define_line_ends_its_statement() {
        let src = "\
&SCOPED-DEFINE pfx {1}
DEFINE VARIABLE a AS INTEGER.
&GLOBAL-DEFINE nu NO-UNDO ~
    {&more}
DEFINE VARIABLE b AS INTEGER.
";
        assert!(covered(src, crate::TX006, ALWAYS).is_empty());
        // The line holds a reference, so the text depends on the includer.
        assert!(IncludeDependence::scan(src).has_reference());
    }

    #[test]
    fn an_include_guard_is_not_a_dependence() {
        let guarded = |head: &str, body: &str, tail: &str| {
            format!("{head}\n{body}\nDEFINE VARIABLE a AS INTEGER.\n&ENDIF\n{tail}")
        };
        let define = "&GLOBAL-DEFINE once 1";
        for head in [
            "&IF DEFINED(once) = 0 &THEN",
            "&IF DEFINED( ONCE ) EQ 0 &THEN",
            "&IF NOT DEFINED(once) &THEN",
        ] {
            let src = guarded(head, define, "");
            let found = IncludeDependence::scan(&src);
            assert!(found.is_empty() && !found.has_reference(), "{head}");
            // A scoped define counts too.
            let src = guarded(head, "&SCOPED-DEFINE once 1", "");
            assert!(IncludeDependence::scan(&src).is_empty(), "{head}");
        }
        // Not a guard: it does not define the name, text follows it, it has an
        // `&ELSE`, or it tests something else.
        let head = "&IF DEFINED(once) = 0 &THEN";
        for src in [
            guarded(head, "", ""),
            guarded(head, define, "DEFINE VARIABLE b AS INTEGER.\n"),
            guarded("&IF DEFINED(once) = 1 &THEN", define, ""),
            guarded("&IF DEFINED(other) = 0 &THEN", define, ""),
            guarded(head, &format!("{define}\n&ELSE"), ""),
        ] {
            assert!(!IncludeDependence::scan(&src).is_empty(), "{src}");
        }
        // A guard does not hide a reference inside it.
        let src = guarded(head, define, "").replace("INTEGER", "{&t}");
        assert!(!IncludeDependence::scan(&src).is_empty());
    }

    #[test]
    fn an_if_condition_is_not_part_of_the_next_statement() {
        let src = "&IF DEFINED(a) &THEN\nDEFINE VARIABLE x AS INTEGER.\n&ENDIF\nDEFINE VARIABLE y AS INTEGER.\n";
        assert_eq!(
            covered(src, crate::TX006, NEVER),
            ["DEFINE VARIABLE x AS INTEGER."]
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
        assert_eq!(covered(src, crate::TX006, NEVER), ["FOR EACH {&tt}:"]);
    }

    #[test]
    fn a_fragment_with_no_reference_has_no_dependent_statement() {
        let found = IncludeDependence::scan("DEFINE VARIABLE a AS INTEGER.\nRUN VALUE(x).");
        assert!(found.is_empty() && !found.has_reference());
    }

    #[test]
    fn the_modifier_slot_says_what_an_includer_may_change() {
        assert_eq!(
            slot_of("DEFINE {1} SHARED VARIABLE v AS INTEGER."),
            Slot::LiteralShared
        );
        assert_eq!(
            slot_of("DEFINE NEW SHARED VARIABLE v AS {&t}."),
            Slot::LiteralShared
        );
        assert_eq!(
            slot_of("DEFINE {&mod} VARIABLE v AS INTEGER."),
            Slot::Reference
        );
        assert_eq!(
            slot_of("DEFINE &IF DEFINED(x) &THEN SHARED &ENDIF VARIABLE v AS INTEGER."),
            Slot::Reference
        );
        assert_eq!(
            slot_of("DEFINE {&mod} TEMP-TABLE tt FIELD a AS INTEGER."),
            Slot::Reference
        );
        // A reference outside the slot, or a definition that cannot be shared.
        assert_eq!(slot_of("DEFINE VARIABLE v AS INTEGER {&nu}."), Slot::Plain);
        assert_eq!(
            slot_of("DEFINE {&dir} PARAMETER p AS INTEGER."),
            Slot::Plain
        );
    }

    #[test]
    fn lower_confidence_chooses_the_help_from_the_slot() {
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
        let with = |slot| Dependence { slot };
        assert!(autofixable(&d("TX006", Some(missing_no_undo::HELP))));
        assert!(!autofixable(&d("TX006", Some(SHARED_HELP))));

        let help = |slot, before: &str| {
            lower_confidence(d("TX006", Some(before)), with(slot))
                .help
                .unwrap()
        };
        // A literal SHARED keeps the producer-and-consumers help, whichever
        // help the standalone run chose.
        for before in [SHARED_HELP, missing_no_undo::HELP] {
            assert_eq!(
                help(Slot::LiteralShared, before),
                format!("{SHARED_HELP} {INCLUDER_NOTE}")
            );
        }
        // A reference in the slot never gets the plain help.
        for before in [SHARED_HELP, missing_no_undo::HELP] {
            let h = help(Slot::Reference, before);
            assert_eq!(h, format!("{MAYBE_SHARED_HELP} {INCLUDER_NOTE}"));
            assert!(h.contains("includer makes this definition `SHARED`"));
        }
        // Elsewhere the plain help, even if the standalone run saw a SHARED
        // that a reference supplied.
        for before in [SHARED_HELP, missing_no_undo::HELP] {
            assert_eq!(
                help(Slot::Plain, before),
                format!("{} {INCLUDER_NOTE}", missing_no_undo::HELP)
            );
        }
        for slot in [Slot::LiteralShared, Slot::Reference, Slot::Plain] {
            let lowered = lower_confidence(d("TX006", None), with(slot));
            assert!(!autofixable(&lowered));
        }

        let other = lower_confidence(d("CL001", Some("Use a literal.")), with(Slot::Plain));
        assert_eq!(
            other.help.as_deref(),
            Some(&*format!("Use a literal. {INCLUDER_NOTE}"))
        );
        assert_eq!(lower_confidence(other.clone(), with(Slot::Plain)), other);
        assert_eq!(
            lower_confidence(d("CL002", None), with(Slot::Plain))
                .help
                .as_deref(),
            Some(INCLUDER_NOTE)
        );
    }
}
