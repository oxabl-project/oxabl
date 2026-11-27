//! The declarations that a call may reach, as signature help reads them,
//! for the call fill of completion (`docs/design/call-fill.md`).

use std::ops::Range;
use std::path::Path;

use oxabl_ast::{NodeId, ParameterType, Statement, StatementKind};
use oxabl_semantic::{ParamMode, SymbolId, SymbolKind};

use super::super::hover::{find_declaration, find_doc, full_function_name};
use super::{CalleeKind, Resolver, Sig, builtin_table_sigs, class_of};
use crate::keyword_docs::DocKind;
use crate::navigate::Navigation;

/// The keyword a call writes before an argument, after its mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentKeyword {
    None,
    Buffer,
    Table,
    TableHandle,
    Dataset,
    DatasetHandle,
}

/// One parameter of a declaration, as a call to it writes the argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalleeParameter {
    /// The name the declaration gives it. For a built-in function, and for
    /// an unnamed parameter, its text in the signature without the mode.
    pub name: String,
    /// `None` when the declaration does not state it.
    pub mode: Option<ParamMode>,
    pub keyword: ArgumentKeyword,
    /// The syntax of a built-in function puts it in brackets.
    pub optional: bool,
}

/// One declaration that a call may reach, with the parameters signature help
/// shows for it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Callee {
    pub label: String,
    pub parameters: Vec<CalleeParameter>,
}

/// The declarations that the call a `(` opens may reach.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallAt {
    /// The call is the argument list of a `RUN` statement.
    pub run: bool,
    pub callees: Vec<Callee>,
}

impl Sig {
    /// `None` when an entry is a parameter include: its fill is one
    /// placeholder (`docs/design/call-fill.md`), which is not built yet, and
    /// its expanded parameters do not line up with the entries of the label.
    fn callee(&self) -> Option<Callee> {
        if self.slots.iter().any(|slot| slot.include.is_some()) {
            return None;
        }
        let label = &self.info.label;
        // Only the syntax of a built-in function marks optional parts.
        let optional_from = label.find('[').filter(|_| !self.modes_known);
        let parameters = self
            .params
            .iter()
            .zip(&self.info.parameters)
            .map(|(param, range)| {
                let text = label.get(range.clone()).unwrap_or_default();
                CalleeParameter {
                    name: param.name.clone().unwrap_or_else(|| without_mode(text)),
                    mode: param.mode,
                    keyword: param.keyword,
                    optional: optional_from.is_some_and(|at| range.start > at),
                }
            })
            .collect();
        Some(Callee {
            label: label.clone(),
            parameters,
        })
    }
}

/// The text of an unnamed parameter in a signature, without its mode.
fn without_mode(text: &str) -> String {
    let text = text.trim();
    match text.split_once(char::is_whitespace) {
        Some((first, rest))
            if ["INPUT", "OUTPUT", "INPUT-OUTPUT"]
                .iter()
                .any(|mode| first.eq_ignore_ascii_case(mode)) =>
        {
            rest.trim().to_string()
        }
        _ => text.to_string(),
    }
}

/// The keyword a call writes before the argument of the parameter
/// `statement` defines.
pub(super) fn argument_keyword(statement: &Statement) -> ArgumentKeyword {
    use oxabl_ast::HandleParamKind;
    match &statement.kind {
        StatementKind::DefineParameter { param_type, .. } => match param_type {
            ParameterType::Variable { .. } => ArgumentKeyword::None,
            ParameterType::Buffer { .. } => ArgumentKeyword::Buffer,
            ParameterType::Handle { kind, .. } => match kind {
                HandleParamKind::Table => ArgumentKeyword::Table,
                HandleParamKind::TableHandle => ArgumentKeyword::TableHandle,
                HandleParamKind::Dataset => ArgumentKeyword::Dataset,
                HandleParamKind::DatasetHandle => ArgumentKeyword::DatasetHandle,
            },
        },
        _ => ArgumentKeyword::None,
    }
}

impl Navigation<'_, '_> {
    /// The declarations that the call around byte `offset` of `file` may
    /// reach, read as [`Self::signature_help`] reads them, for the call fill.
    pub fn callees_at(&self, file: &Path, offset: u32) -> Option<CallAt> {
        let resolver = Resolver::new(self);
        let (call, _, sigs) = self.resolved_call(&resolver, file, offset)?;
        Some(CallAt {
            run: call.kind == CalleeKind::Run,
            callees: sigs.iter().map(Sig::callee).collect::<Option<_>>()?,
        })
    }

    /// The declaration of the internal procedure or user function `sym` of
    /// the unit, as signature help reads it when a call resolves to it, and
    /// the bytes of its statement in the expanded text. `None` for a method
    /// and for a declaration that oxabl may not read whole.
    pub fn declared_callee(&self, sym: SymbolId) -> Option<(Callee, Range<u32>)> {
        let symbols = &self.model.semantic.symbols;
        let symbol = symbols.get(sym);
        if !matches!(symbol.kind, SymbolKind::Procedure | SymbolKind::Function)
            || symbols.is_method(sym)
            || symbol.declaration == NodeId::DUMMY
            || symbols.member_file(sym).is_some()
        {
            return None;
        }
        let parsed = self.parsed();
        let found = find_declaration(&parsed.program.statements, symbol.name_span)?;
        let span = found.statement.span.start..found.statement.span.end;
        let sig = Resolver::new(self).unit_sig(symbol.name_span)?;
        Some((sig.callee()?, span))
    }

    /// The syntaxes of the built-in function `name` that signature help
    /// shows. `None` for a name that is not reserved when a routine of that
    /// name may hide it anywhere in the unit: a routine the unit declares,
    /// an include read alone, lost include text, or a class.
    pub fn builtin_callees(&self, name: &str) -> Option<Vec<Callee>> {
        let full = full_function_name(name);
        let doc = find_doc(DocKind::Function, full)?;
        if !doc.reserved {
            let tree = &self.model.semantic.scope_tree;
            let root = oxabl_semantic::ScopeId::ROOT;
            let declared = [
                oxabl_semantic::NamespaceId::Functions,
                oxabl_semantic::NamespaceId::Procedures,
            ]
            .into_iter()
            .any(|ns| tree.resolve_name(root, ns, full).is_some());
            if declared
                || self
                    .run
                    .file()
                    .is_some_and(oxabl_workspace::is_include_fragment)
                || self.expanded.lost_include_text()
                || class_of(&self.parsed().program.statements).is_some()
            {
                return None;
            }
        }
        builtin_table_sigs(full, doc)?
            .iter()
            .map(Sig::callee)
            .collect::<Option<_>>()
    }
}
