//! Database inlay hints for one file, in the file's own bytes.
//!
//! The facts and their selection are the strict database rules' own
//! ([`oxabl_lint::hints`]); this module places them in the root file and
//! words them. A hint whose place is not in the root file's own text (it
//! came from an include) is left out, and a block hint is shown only when
//! both of its ends are in the root file.

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

impl LintPipeline<'_> {
    /// Whether this file shows database hints: diagnostics are on in
    /// `[workspace.features]`, and the file enables a rule of
    /// `strict.locks`, `strict.transactions` or `strict.access`.
    pub fn database_hints_enabled(&self) -> bool {
        use oxabl_lint::groups::{STRICT_ACCESS, STRICT_LOCKS, STRICT_TRANSACTIONS};
        if !self.config().features.diagnostics {
            return false;
        }
        let severities = self
            .file()
            .map(|p| self.config().lint_severities.for_path(p))
            .unwrap_or_else(|| self.config().lint_severities.clone());
        let registry = oxabl_lint::Registry::builtin_shared();
        registry.rules().any(|r| {
            matches!(
                r.meta().group,
                STRICT_LOCKS | STRICT_TRANSACTIONS | STRICT_ACCESS
            ) && registry.severity_for(r, &severities).is_some()
        })
    }

    /// The database hints of `kinds` for this handle's file, in source order.
    /// Empty when the expansion failed. The caller checks
    /// [`database_hints_enabled`](Self::database_hints_enabled).
    ///
    /// **Unguarded** (KTD6), like [`collect`](Self::collect).
    pub fn database_hints(&self, expansion: &Expansion, kinds: HintKinds) -> Vec<InlayHint> {
        let Some(expanded) = expansion.expanded() else {
            return Vec::new();
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
        let root = |span: Span| expanded.resolve_root_span(span);
        let start = |span: Span| {
            root(Span {
                start: span.start,
                end: span.start.saturating_add(1).min(span.end),
            })
            .map(|s| s.start)
        };
        let end = |span: Span| {
            root(Span {
                start: span.end.saturating_sub(1).max(span.start),
                end: span.end,
            })
            .map(|s| s.end)
        };
        let mut hints = Vec::new();
        for fact in facts {
            match fact {
                DatabaseHint::Lock { at, mode } => {
                    let Some(at) = end(at) else { continue };
                    let mode = mode.as_str().to_ascii_uppercase();
                    hints.push(InlayHint {
                        at,
                        kind: InlayKind::Lock,
                        label: mode.clone(),
                        tooltip: format!(
                            "This read names no lock, so it takes {mode}, the default \
                             (default-lock, LK001)."
                        ),
                        after: true,
                    });
                }
                DatabaseHint::Index {
                    at,
                    name,
                    reason,
                    whole,
                } => {
                    let Some(at) = end(at) else { continue };
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
                    hints.push(InlayHint {
                        at,
                        kind: InlayKind::Index,
                        label: if whole {
                            format!("index {name}, WHOLE-INDEX")
                        } else {
                            format!("index {name}")
                        },
                        tooltip,
                        after: true,
                    });
                }
                DatabaseHint::Scope {
                    buffer,
                    block,
                    described,
                    raised_by,
                    widened,
                } => {
                    let (Some(from), Some(to)) = (start(block), end(block)) else {
                        continue;
                    };
                    let tooltip = format!(
                        "A reference to `{}` outside {widened} raises the scope of `{buffer}` \
                         to {described}: the buffer keeps its last record, and any lock on it, \
                         until {described} ends (scope-widened, TX005).",
                        quote(text, raised_by),
                    );
                    hints.push(InlayHint {
                        at: from,
                        kind: InlayKind::Scope,
                        label: format!("{buffer} scope starts"),
                        tooltip: tooltip.clone(),
                        after: false,
                    });
                    hints.push(InlayHint {
                        at: to,
                        kind: InlayKind::Scope,
                        label: format!("{buffer} scope ends"),
                        tooltip,
                        after: true,
                    });
                }
                DatabaseHint::Transaction {
                    block,
                    described,
                    update,
                    procedure,
                } => {
                    let (Some(from), Some(to)) = (start(block), end(block)) else {
                        continue;
                    };
                    let tooltip = if procedure {
                        format!(
                            "The main block is a transaction block because of `{}`: the \
                             transaction spans the whole procedure (procedure-wide-transaction, \
                             TX002).",
                            quote(text, update)
                        )
                    } else {
                        format!(
                            "{} is a transaction block only because of `{}`: the transaction \
                             starts and ends with it (implicit-transaction, TX001).",
                            capitalize(&described),
                            quote(text, update)
                        )
                    };
                    hints.push(InlayHint {
                        at: from,
                        kind: InlayKind::Transaction,
                        label: "transaction starts".to_string(),
                        tooltip: tooltip.clone(),
                        after: false,
                    });
                    hints.push(InlayHint {
                        at: to,
                        kind: InlayKind::Transaction,
                        label: "transaction ends".to_string(),
                        tooltip,
                        after: true,
                    });
                }
            }
        }
        hints.sort_by_key(|h| (h.at, h.after));
        hints
    }
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
