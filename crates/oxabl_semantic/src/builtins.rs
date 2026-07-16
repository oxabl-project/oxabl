//! Built-in symbol seeding.
//!
//! The declare pass calls [`seed`] immediately after constructing the file
//! scope so that references to ambient ABL pseudo-variables
//! (`THIS-OBJECT`, `SUPER`, `SELF`, `SESSION`, `ERROR-STATUS`) resolve
//! normally instead of producing `undefined-symbol` diagnostics in Phase 4a.
//!
//! v1 deliberately seeds only these five entries. The list grows data-driven
//! from the Phase 5 corpus audit — adding ~25 speculative handles without
//! corpus evidence risks both misses and noise.

use oxabl_ast::NodeId;
use oxabl_common::VirtualSpan;
use oxabl_lexer::oxabl_atom::OxablAtom;

use crate::{NamespaceId, ScopeId, ScopeTree, Symbol, SymbolFlags, SymbolKind, SymbolTable};

/// Seed the five v1 built-ins into the file scope.
pub(crate) fn seed(tree: &mut ScopeTree, symbols: &mut SymbolTable) {
    for name in ["this-object", "super", "self", "session", "error-status"] {
        let atom = OxablAtom::from(name);
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
    fn seeds_five_builtins_into_root() {
        let mut tree = ScopeTree::new();
        let mut symbols = SymbolTable::new();
        seed(&mut tree, &mut symbols);
        assert_eq!(symbols.len(), 5);
        let root = tree.get(ScopeId::ROOT);
        for name in ["this-object", "super", "self", "session", "error-status"] {
            let sym = root
                .get_in(NamespaceId::Values, &OxablAtom::from(name))
                .unwrap_or_else(|| panic!("{name} should be seeded"));
            assert_eq!(symbols.get(sym).kind, SymbolKind::BuiltIn);
        }
    }
}
