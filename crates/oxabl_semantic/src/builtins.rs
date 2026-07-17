//! Built-in symbol seeding.
//!
//! The declare pass calls [`seed`] immediately after constructing the file
//! scope so that references to ambient ABL pseudo-variables and system
//! handles (`THIS-OBJECT`, `SESSION`, `THIS-PROCEDURE`, `WEB-CONTEXT`, ...)
//! resolve normally instead of producing `undefined-symbol` diagnostics.
//!
//! v1 seeded only five entries; the list below is the standard Progress
//! system-handle set grown from the Phase 5 corpus audit (#58). Member
//! access on a seeded handle (`SESSION:BATCH-MODE`) needs no attribute
//! modeling — the member side is already treated as External.

use oxabl_ast::NodeId;
use oxabl_common::VirtualSpan;
use oxabl_lexer::oxabl_atom::OxablAtom;

use crate::{NamespaceId, ScopeId, ScopeTree, Symbol, SymbolFlags, SymbolKind, SymbolTable};

/// ASCII-lowercased names of every ABL system handle / pseudo-variable
/// seeded into the root scope. Includes the documented abbreviation forms
/// the lexer recognizes as distinct spellings (`TERM`, `ERROR-STAT`,
/// `RCODE-INFO`, ...) because the parser preserves source text and the
/// resolver only case-folds — it does not canonicalize abbreviations.
pub static SYSTEM_HANDLES: &[&str] = &[
    // Original v1 five
    "this-object",
    "super",
    "self",
    "session",
    "error-status",
    // ERROR-STATUS documented abbreviations (min ERROR-STAT)
    "error-stat",
    "error-statu",
    // Procedure handles
    "this-procedure",
    "source-procedure",
    "target-procedure",
    // Window / UI handles
    "current-window",
    "active-window",
    "default-window",
    "active-form",
    "focus",
    "clipboard",
    // FILE-INFO system handle
    "file-info",
    "file-information",
    // Event / logging / terminal
    "last-event",
    "last-even", // documented minimum abbreviation
    "log-manager",
    "dslog-manager",
    "terminal",
    "term", // documented minimum abbreviation
    "termi",
    "termin",
    "termina",
    // Web / COM
    "web-context",
    "com-self",
    // Tooling handles
    "debugger",
    "profiler",
    "compiler",
    "codebase-locator",
    // Color / font tables
    "color-table",
    "font-table",
    // RCODE-INFO system handle (min RCODE-INFO, full RCODE-INFORMATION)
    "rcode-info",
    "rcode-infor",
    "rcode-inform",
    "rcode-informa",
    "rcode-informat",
    "rcode-informati",
    "rcode-informatio",
    "rcode-information",
    // Object chain pseudo-handles
    "first-object",
    "last-object",
    // Security / audit
    "audit-control",
    "audit-policy",
    "security-policy",
    // Codepage table (corpus evidence, #58)
    "codepage-table",
];

/// Seed the ABL system handles into the file scope.
pub(crate) fn seed(tree: &mut ScopeTree, symbols: &mut SymbolTable) {
    for name in SYSTEM_HANDLES {
        let atom = OxablAtom::from(*name);
        let sym = symbols.insert(Symbol {
            name: atom.clone(),
            namespace: NamespaceId::Values,
            kind: SymbolKind::BuiltIn,
            declared_in: ScopeId::ROOT,
            declaration: NodeId::DUMMY,
            name_span: VirtualSpan::new(0, 0),
            data_type: None,
            read_count: 0,
            write_count: 0,
            flags: SymbolFlags::empty(),
            table_id: None,
        });
        tree.get_mut(ScopeId::ROOT).bindings[NamespaceId::Values.index()].insert(atom, sym);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SymbolKind;

    #[test]
    fn seeds_all_system_handles_into_root() {
        let mut tree = ScopeTree::new();
        let mut symbols = SymbolTable::new();
        seed(&mut tree, &mut symbols);
        assert_eq!(symbols.len(), SYSTEM_HANDLES.len());
        let root = tree.get(ScopeId::ROOT);
        for name in SYSTEM_HANDLES {
            let sym = root
                .get_in(NamespaceId::Values, &OxablAtom::from(*name))
                .unwrap_or_else(|| panic!("{name} should be seeded"));
            assert_eq!(symbols.get(sym).kind, SymbolKind::BuiltIn);
        }
    }

    #[test]
    fn corpus_headline_handles_are_seeded() {
        let mut tree = ScopeTree::new();
        let mut symbols = SymbolTable::new();
        seed(&mut tree, &mut symbols);
        let root = tree.get(ScopeId::ROOT);
        for name in ["this-procedure", "current-window", "web-context"] {
            assert!(
                root.get_in(NamespaceId::Values, &OxablAtom::from(name))
                    .is_some(),
                "{name} should be seeded"
            );
        }
    }
}
