//! Candidate and skip predicates shared by the two unused-symbol rules,
//! `unused-variable` (LINT0002) and `assigned-but-never-read` (LINT0006).
//!
//! The two rules split one population — symbols nothing reads — by whether
//! anything ever wrote to them. Their exemptions are identical, and a rule that
//! forgot one would start producing exactly the false positives the other spent
//! commits eliminating, so the exemptions live here once rather than in two
//! copies. The rationale for each exemption travels with it: that reasoning is
//! what a future reader needs, and the first thing a drifting copy would lose.
//!
//! [`is_skipped`] has a third caller: `block-var-used-outside` (LINT0005), whose
//! `is_hazard` gate consults it rather than carrying a parallel clause. LINT0005
//! reasons about a different question — *where* a variable was written, not
//! whether it was read — but it gates on the same counts, so the same facts that
//! make those counts untrustworthy exempt it too. Two suppression paths that can
//! drift apart as exemptions are added is exactly the bug class this module
//! exists to prevent.

use oxabl_common::FileSpan;
use oxabl_semantic::{
    AnalysisContext, ScopeId, ScopeKind, ScopeTree, Symbol, SymbolFlags, SymbolId, SymbolKind,
    SymbolTable,
};

/// Whether `sym` is the kind of symbol either rule reasons about.
///
/// Deliberately narrow. Buffers, temp-tables and `Field` symbols are excluded,
/// which is also what keeps schema-synthesized symbols (never credited with a
/// read) and `DEFINE PARAMETER BUFFER` out of both rules.
pub fn is_candidate(sym: &Symbol) -> bool {
    matches!(sym.kind, SymbolKind::Variable | SymbolKind::Parameter)
}

/// Exemptions that apply to both rules.
pub fn is_skipped(sid: SymbolId, sym: &Symbol, tree: &ScopeTree, symbols: &SymbolTable) -> bool {
    // OUTPUT / INPUT-OUTPUT parameters: writing is the contract.
    if sym.kind == SymbolKind::Parameter
        && sym
            .flags
            .intersects(SymbolFlags::PARAM_OUTPUT | SymbolFlags::PARAM_INPUT_OUT)
    {
        return true;
    }
    // SHARED variables — readers may live in other files.
    if sym
        .flags
        .intersects(SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
    {
        return true;
    }
    // Passed to a callee as a write-back argument: the callee assigns into
    // it, which is a use of the binding regardless of `read_count`. For
    // LINT0006 this is also what leaves the callee-written dead store to the
    // separate opt-in advisory (#125) instead of annexing it at warning
    // severity.
    if sym.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG) {
        return true;
    }
    // Named inside a statement form the parser recognizes but does not model
    // (`PUT`, `UPDATE`, `ENABLE`, embedded SQL, …). Those statements credit no
    // reads and no writes, so this symbol's counts describe only part of what
    // the code does and neither rule can honestly conclude anything from them.
    // Coarse on purpose: the mark is per-symbol and file-wide, so this does cost
    // real findings — a genuine dead store early in a file goes unreported
    // because of an `ENABLE` mention much later. Over-suppressing loses a
    // diagnostic; the alternative reports one that is simply wrong. #136 drains
    // the population form by form.
    if sym
        .flags
        .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
    {
        return true;
    }
    // Parameters of an INTERFACE method or an ABSTRACT method never
    // execute a body; their read-count is meaningless.
    if sym.kind == SymbolKind::Parameter && in_skipped_method(sym.declared_in, tree, symbols) {
        return true;
    }
    // Don't self-warn on the rule's own books.
    let _ = sid;
    false
}

/// Whether `sym` is a `TABLE FOR` / `DATASET FOR` parameter, whose own
/// reference counts are meaningless because every reference to the name lands
/// on the backing temp-table or dataset declaration instead.
///
/// Kept separate from [`is_skipped`] on purpose. LINT0006 skips these outright —
/// a table-shaped parameter is never a dead store. LINT0002 must *not*, because
/// it owns a redirect that still reports the genuinely-unused case by asking the
/// backing declaration. Folding this into the shared skip would leave LINT0002
/// unable to consult that list at all, and a shared list with a hole in it is
/// worse than one explicit extra call at the single site that needs it.
///
/// Both callers disappear once CFG def-use records land (#126) and the redirect
/// becomes a def-use query.
pub fn is_table_like_param(sym: &Symbol) -> bool {
    sym.flags.contains(SymbolFlags::PARAM_TABLE_LIKE)
}

/// Whether the `Parameter` declared in `scope` lives inside a method scope
/// whose declaring method is ABSTRACT, or inside an INTERFACE body.
fn in_skipped_method(scope: ScopeId, tree: &ScopeTree, symbols: &SymbolTable) -> bool {
    let mut cur = Some(scope);
    while let Some(id) = cur {
        let s = tree.get(id);
        // Parameter declared inside an INTERFACE body — any method there
        // has no body; skip its parameters.
        if s.kind == ScopeKind::Interface {
            return true;
        }
        if s.kind == ScopeKind::Method {
            // Look up the Method symbol whose declaration NodeId matches
            // this scope's owner, and check its ABSTRACT flag.
            if let Some((_, msym)) = symbols.iter().find(|(_, sym)| {
                sym.kind == SymbolKind::Function && sym.declaration == s.owner_node
            }) && msym.flags.contains(SymbolFlags::ABSTRACT)
            {
                return true;
            }
        }
        cur = s.parent;
    }
    false
}

/// The span of `sym`'s own declaring identifier, for a diagnostic that should
/// point at the declaration rather than at a use.
pub fn declaration_span(ctx: &AnalysisContext, sym: &Symbol) -> FileSpan {
    FileSpan {
        file: ctx.file_id,
        span: oxabl_ast::Span {
            start: sym.name_span.start,
            end: sym.name_span.end,
        },
    }
}

/// Original-casing name for a diagnostic message: slice it out of the source,
/// falling back to the case-folded atom when the span maps outside the buffer
/// (e.g. hand-built AST in tests).
pub fn display_name(sym: &Symbol, source: &str) -> String {
    let start = sym.name_span.start as usize;
    let end = sym.name_span.end as usize;
    if end > start && end <= source.len() {
        source[start..end].to_string()
    } else {
        sym.name.as_ref().to_string()
    }
}
