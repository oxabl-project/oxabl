//! Database inlay hints for one file, in the file's own bytes.
//!
//! The facts and their selection are the strict database rules' own
//! ([`oxabl_lint::hints`]); this module places them in the shown file and
//! words them. A hint whose place is not in the shown file's own text is
//! left out, and a block hint is shown only when both of its ends are in
//! that file. A kind shows only where its rule is on.

use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_effects::IndexReason;
use oxabl_lint::hints::DatabaseHint;
pub use oxabl_lint::hints::HintKinds;
use oxabl_semantic::SourceContext;
use oxabl_workspace::is_include_fragment;

use crate::{Expansion, LintPipeline};

/// The kind of a hint, which a client can turn off on its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InlayKind {
    Lock,
    Index,
    Scope,
    Transaction,
}

/// One hint, at a byte offset of the root file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHint {
    pub at: u32,
    pub kind: InlayKind,
    pub label: String,
    pub tooltip: String,
    /// The hint follows the text before `at`; else it precedes the text at
    /// `at`.
    pub after: bool,
}

/// A hint before its place is final: where the finding that selects it sits,
/// and its order among hints at the same offset.
struct Placed<'c> {
    hint: InlayHint,
    /// The code and span of the finding behind the hint, in the shown file's
    /// own bytes.
    finding: Option<(&'c str, Span)>,
    /// 0 for a start hint, 1 for a hint after a name, 2 for an end hint.
    group: u8,
    /// The block pair it belongs to; end hints at one offset sort in reverse,
    /// so that the pairs nest.
    pair: usize,
}

impl LintPipeline<'_> {
    /// Whether this file shows database hints: diagnostics are on in
    /// `[workspace.features]`, and the file enables a rule of
    /// `strict.locks`, `strict.transactions` or `strict.access`.
    pub fn database_hints_enabled(&self) -> bool {
        use oxabl_lint::groups::{STRICT_ACCESS, STRICT_LOCKS, STRICT_TRANSACTIONS};
        if !self.config().features.diagnostics {
            return false;
        }
        let registry = oxabl_lint::Registry::builtin_shared();
        let severities = self.hint_severities();
        registry.rules().any(|r| {
            matches!(
                r.meta().group,
                STRICT_LOCKS | STRICT_TRANSACTIONS | STRICT_ACCESS
            ) && registry.severity_for(r, &severities).is_some()
        })
    }

    fn hint_severities(&self) -> oxabl_common::LintSeverityMap {
        self.file()
            .map(|p| self.config().lint_severities.for_path(p))
            .unwrap_or_else(|| self.config().lint_severities.clone())
    }

    /// The database hints of `kinds` for this handle's file, whose text is
    /// `source`, in source order. Empty when the expansion failed. The caller
    /// checks [`database_hints_enabled`](Self::database_hints_enabled).
    ///
    /// An include file read alone (fragment mode) gives no hint whose finding
    /// depends on the includer (a `{...}` or an `&IF` in its statement), as
    /// the rules lower those findings; each other hint says that an includer
    /// can change it.
    ///
    /// **Unguarded** (KTD6), like [`collect`](Self::collect).
    pub fn database_hints(
        &self,
        source: &str,
        expansion: &Expansion,
        kinds: HintKinds,
    ) -> Vec<InlayHint> {
        let Some(expanded) = expansion.expanded() else {
            return Vec::new();
        };
        let fragment = self.file().is_some_and(is_include_fragment);
        let note = fragment.then_some(
            " Read from this include alone: a file that includes it can change this, for \
             example with a temp-table of the same name. Select an include context to read it \
             in a root file.",
        );
        let placed = self.place_hints(expanded, kinds, note, |span| {
            expanded.resolve_root_span(span)
        });
        let placed = if fragment {
            let dependence = oxabl_lint::fragment::IncludeDependence::scan(source);
            let probe = crate::lint::ExpansionProbe::new(source, expansion, self.file_system());
            placed
                .into_iter()
                .filter(|p| {
                    // A finding that cannot be placed in the file cannot be
                    // checked, so its hint is left out.
                    p.finding.is_some_and(|(code, span)| {
                        dependence.dependence(code, span, &probe).is_none()
                    })
                })
                .collect()
        } else {
            placed
        };
        order(placed)
    }

    /// The database hints of `kinds` that this handle's file, a root, gives
    /// for the include `include` it expands, in the include's own bytes. The
    /// analysis is the root's, as for the diagnostics of a selected include
    /// context. Empty when the root expands the include other than once:
    /// two expansions may disagree, and one place cannot hold both answers.
    ///
    /// **Unguarded** (KTD6), like [`collect`](Self::collect).
    pub fn database_hints_in_include(
        &self,
        expansion: &Expansion,
        include: &Path,
        kinds: HintKinds,
    ) -> Vec<InlayHint> {
        let Some(expanded) = expansion.expanded() else {
            return Vec::new();
        };
        let include = same_file_key(include);
        let expansions = expanded
            .include_expansions()
            .iter()
            .filter(|e| same_file_key(e.path) == include)
            .count();
        if expansions != 1 {
            return Vec::new();
        }
        let root = self
            .file()
            .and_then(|p| p.file_name())
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        let note = format!(" Read in `{root}`, the selected include context.");
        let placed = self.place_hints(expanded, kinds, Some(&note), |span| {
            let (path, span, _) = expanded.resolve_include_span(span)?;
            (same_file_key(path) == include).then_some(span)
        });
        order(placed)
    }

    /// The hints of the kinds whose rule this file enables, placed by
    /// `place`, which maps an expanded span to the shown file's bytes.
    fn place_hints(
        &self,
        expanded: &oxabl_analyze::ExpandedFile,
        kinds: HintKinds,
        note: Option<&str>,
        place: impl Fn(Span) -> Option<Span>,
    ) -> Vec<Placed<'static>> {
        let registry = oxabl_lint::Registry::builtin_shared();
        let severities = self.hint_severities();
        let on = |code: &str| {
            registry
                .rules()
                .any(|r| r.meta().code == code && registry.severity_for(r, &severities).is_some())
        };
        let (tx001, tx002) = (on(oxabl_lint::TX001), on(oxabl_lint::TX002));
        // A kind follows its rule: a project that turns the rule off does not
        // hold its fact (LK001 off may mean `-NL`, which oxabl does not read).
        let kinds = HintKinds {
            locks: kinds.locks && on(oxabl_lint::LK001),
            index: kinds.index,
            scope: kinds.scope && on(oxabl_lint::TX005),
            transactions: kinds.transactions && (tx001 || tx002),
        };
        let source_context = if self.file().is_some_and(is_include_fragment) {
            SourceContext::IncludeFragment
        } else {
            SourceContext::CompilationUnit
        };
        let facts = oxabl_analyze::database_hints_from_expanded(
            expanded,
            &self.config().schema,
            self.config().schema_loaded,
            source_context,
            kinds,
        );
        let text = expanded.text.as_str();
        let start = |span: Span| {
            place(Span {
                start: span.start,
                end: span.start.saturating_add(1).min(span.end),
            })
            .map(|s| s.start)
        };
        let end = |span: Span| {
            place(Span {
                start: span.end.saturating_sub(1).max(span.start),
                end: span.end,
            })
            .map(|s| s.end)
        };
        let note = note.unwrap_or("");
        let mut placed = Vec::new();
        let point = |at, kind, label, tooltip: String, finding| Placed {
            hint: InlayHint {
                at,
                kind,
                label,
                tooltip: format!("{tooltip}{note}"),
                after: true,
            },
            finding,
            group: 1,
            pair: 0,
        };
        for (pair, fact) in facts.into_iter().enumerate() {
            let mut block = |from: u32, to: u32, kind, what: &str, tooltip: String, finding| {
                let tooltip = format!("{tooltip}{note}");
                placed.push(Placed {
                    hint: InlayHint {
                        at: from,
                        kind,
                        label: format!("{what} starts"),
                        tooltip: tooltip.clone(),
                        after: false,
                    },
                    finding,
                    group: 0,
                    pair,
                });
                placed.push(Placed {
                    hint: InlayHint {
                        at: to,
                        kind,
                        label: format!("{what} ends"),
                        tooltip,
                        after: true,
                    },
                    finding,
                    group: 2,
                    pair,
                });
            };
            match fact {
                DatabaseHint::Lock { at, mode } => {
                    let Some(to) = end(at) else { continue };
                    let finding = place(at).map(|s| (oxabl_lint::LK001, s));
                    let mode = mode.as_str().to_ascii_uppercase();
                    placed.push(point(
                        to,
                        InlayKind::Lock,
                        mode.clone(),
                        format!(
                            "This read names no lock, so it takes {mode}, the default \
                             (default-lock, LK001). oxabl does not read the `-NL` startup \
                             option, which changes the default."
                        ),
                        finding,
                    ));
                }
                DatabaseHint::Index {
                    at,
                    name,
                    reason,
                    whole,
                } => {
                    let Some(to) = end(at) else { continue };
                    let finding = place(at).map(|s| (oxabl_lint::DB001, s));
                    let mut tooltip = format!(
                        "oxabl expects the compiler to use the index `{name}` for this search: {}.",
                        reason_text(reason)
                    );
                    if whole {
                        tooltip.push_str(
                            " No condition brackets the index, so the search reads all of it \
                             (WHOLE-INDEX).",
                        );
                    }
                    let label = if whole {
                        format!("index {name}, WHOLE-INDEX")
                    } else {
                        format!("index {name}")
                    };
                    placed.push(point(to, InlayKind::Index, label, tooltip, finding));
                }
                DatabaseHint::Scope {
                    buffer,
                    block: span,
                    described,
                    raised_by,
                    widened,
                } => {
                    let (Some(from), Some(to)) = (start(span), end(span)) else {
                        continue;
                    };
                    let finding = place(raised_by).map(|s| (oxabl_lint::TX005, s));
                    block(
                        from,
                        to,
                        InlayKind::Scope,
                        &format!("{buffer} scope"),
                        format!(
                            "A reference to `{}` outside {widened} raises the scope of \
                             `{buffer}` to {described}: the buffer keeps its last record, and \
                             any lock on it, until {described} ends (scope-widened, TX005).",
                            quote(text, raised_by),
                        ),
                        finding,
                    );
                }
                DatabaseHint::Transaction {
                    block: span,
                    described,
                    update,
                    procedure,
                } => {
                    if (procedure && !tx002) || (!procedure && !tx001) {
                        continue;
                    }
                    let (Some(from), Some(to)) = (start(span), end(span)) else {
                        continue;
                    };
                    let code = if procedure {
                        oxabl_lint::TX002
                    } else {
                        oxabl_lint::TX001
                    };
                    let finding = place(update).map(|s| (code, s));
                    let tooltip = if procedure {
                        format!(
                            "The main block is a transaction block because of `{}` \
                             (procedure-wide-transaction, TX002): a transaction starts with \
                             the procedure unless one is already active, and lasts until it \
                             returns.",
                            quote(text, update)
                        )
                    } else {
                        format!(
                            "{} is a transaction block only because of `{}` \
                             (implicit-transaction, TX001): a transaction starts here unless \
                             one is already active; then the block is a subtransaction.",
                            capitalize(&described),
                            quote(text, update)
                        )
                    };
                    block(
                        from,
                        to,
                        InlayKind::Transaction,
                        "transaction",
                        tooltip,
                        finding,
                    );
                }
            }
        }
        placed
    }
}

/// The hints in source order. At one offset, start hints come first in
/// pair order, then the hints after a name, then end hints in reverse pair
/// order, so that two block pairs at the same place nest.
fn order(mut placed: Vec<Placed<'static>>) -> Vec<InlayHint> {
    placed.sort_by_key(|p| {
        let rank = if p.group == 2 {
            usize::MAX - p.pair
        } else {
            p.pair
        };
        (p.hint.at, p.group, rank)
    });
    placed.into_iter().map(|p| p.hint).collect()
}

/// A path compared as a file: canonical when it exists, else absolute.
fn same_file_key(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| {
        oxabl_index::search::normalize_lexically(
            &std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf()),
        )
    })
}

fn reason_text(reason: IndexReason) -> &'static str {
    match reason {
        IndexReason::UseIndex => "`USE-INDEX` names it",
        IndexReason::Contains => "a `CONTAINS` on a field of this word index",
        IndexReason::KeyValue => "a search by a constant uses the primary index",
        IndexReason::UniqueEquality => "every component of this unique index has an equality match",
        IndexReason::Equality => "it has the most equality matches on leading components",
        IndexReason::Range => "a range match on its first component",
        IndexReason::Sort => "no condition matches an index; it has the most `BY` fields in order",
        IndexReason::Primary => "no condition matches an index, so the primary index",
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) => c.to_uppercase().chain(chars).collect(),
        None => String::new(),
    }
}

/// The text of `span` on one line, shortened.
fn quote(text: &str, span: Span) -> String {
    let raw = text
        .get(span.start as usize..span.end as usize)
        .unwrap_or("");
    let line: String = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    if line.chars().count() > 60 {
        let mut short: String = line.chars().take(57).collect();
        short.push_str("...");
        short
    } else {
        line
    }
}
