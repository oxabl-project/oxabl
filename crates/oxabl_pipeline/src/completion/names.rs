//! The names that are valid at the cursor: the "Values" collector, the names
//! that can start a statement, and the records of a record position.
//!
//! The names come from the scope at the cursor in the base and its ancestors.
//! A name declared in the same routine counts only when its declaration ends
//! before the cursor; class members, and functions declared or `FORWARD`ed
//! before the cursor, count everywhere in their scope. A symbol the resolve
//! pass made at a use (a schema field, an inherited member) is not a
//! declaration, and is never offered.

use std::collections::HashSet;

use oxabl_ast::NodeId;
use oxabl_semantic::{
    ClassLattice, NamespaceId, ResolvedType, ScopeId, ScopeKind, Symbol, SymbolFlags, SymbolId,
    SymbolKind, assignable,
};

use super::context::RecordsAfter;
use super::rank::nearness;
use super::{Candidate, CompletionBase, Engine, ItemData, ItemKind, base};
use crate::keyword_docs::{DocKind, KEYWORD_DOCS};

/// Which names a position takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Wanted {
    /// An expression: every value, function and record.
    Values,
    /// A statement start: what an assignment or a call can start with.
    StatementStart,
    /// A record position.
    Records(RecordsAfter),
}

impl Engine<'_, '_, '_, '_> {
    /// The names of `wanted` at the cursor, and whether the list can miss
    /// some. With several places of the cursor (an include the root reads
    /// twice), the names are the ones every place has.
    pub(super) fn names(&self, wanted: Wanted) -> Option<(Vec<Candidate>, bool)> {
        let (base, mapped) = self.based()?;
        let mut incomplete = self.base_incomplete();
        if mapped.scope_changed {
            return Some((Vec::new(), true));
        }
        let mut lists = mapped.places.iter().map(|place| {
            let (names, class_incomplete) = self.names_at(base, *place, wanted);
            incomplete |= class_incomplete;
            names
        });
        let mut names = lists.next().unwrap_or_default();
        for other in lists {
            let keys: HashSet<(String, ItemKind)> = other
                .iter()
                .map(|c| (c.label.to_ascii_lowercase(), c.kind))
                .collect();
            names.retain(|c| keys.contains(&(c.label.to_ascii_lowercase(), c.kind)));
        }
        if !matches!(wanted, Wanted::Records(_)) {
            names.extend(self.value_keywords(base, mapped.places[0], wanted));
        }
        names.extend(self.tables(base, wanted));
        if wanted != Wanted::StatementStart && !base.navigation.run.config().schema_loaded {
            incomplete |= matches!(wanted, Wanted::Records(_));
        }
        Some((names, incomplete))
    }

    /// The names valid at expanded byte `virt`, and whether the class of the
    /// scope can have members that are not listed.
    fn names_at(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        virt: u32,
        wanted: Wanted,
    ) -> (Vec<Candidate>, bool) {
        let semantic = &base.navigation.model.semantic;
        let tree = &semantic.scope_tree;
        let symbols = &semantic.symbols;
        let scope = base::scope_at(base, virt);
        let expected = match &self.found.context {
            super::Context::Values {
                target: Some(token),
            } => {
                let token = &self.tokens[*token];
                let name = &self.request.text[token.start..token.end];
                tree.resolve_name(scope, NamespaceId::Values, name)
                    .and_then(|sym| symbols.get(sym).data_type.clone())
                    .filter(known)
            }
            _ => None,
        };
        let namespaces: &[NamespaceId] = match wanted {
            Wanted::Values => &[
                NamespaceId::Values,
                NamespaceId::Buffers,
                NamespaceId::Functions,
            ],
            Wanted::StatementStart => &[
                NamespaceId::Values,
                NamespaceId::Buffers,
                NamespaceId::Functions,
            ],
            Wanted::Records(_) => &[NamespaceId::Buffers],
        };
        let mut seen: HashSet<(NamespaceId, &str)> = HashSet::new();
        let mut items = Vec::new();
        let mut class_incomplete = false;
        let statik = in_static_routine(base, scope);
        for (depth, id) in tree.ancestors(scope).enumerate() {
            let held = tree.get(id);
            let members = matches!(held.kind, ScopeKind::Class | ScopeKind::Interface);
            if held.kind == ScopeKind::Class {
                // Every class inherits members from `Progress.Lang.Object`,
                // and oxabl lists no inherited member yet.
                class_incomplete = true;
            }
            for ns in namespaces {
                let mut bound: Vec<(&str, SymbolId)> = held
                    .bindings
                    .get(*ns)
                    .into_iter()
                    .flat_map(|map| map.iter())
                    .map(|(name, sym)| (name.as_ref(), sym))
                    .collect();
                bound.sort_unstable_by_key(|(_, sym)| sym.raw());
                for (name, sym) in bound {
                    if seen.contains(&(*ns, name)) {
                        continue;
                    }
                    let symbol = symbols.get(sym);
                    // A name the cursor cannot see does not hide an outer one.
                    if symbol.declaration == NodeId::DUMMY
                        || symbol.kind == SymbolKind::BuiltIn
                        || !(members
                            || symbol.name_span.end <= virt
                            || symbol.kind == SymbolKind::Function && self.declared_before(name))
                        || self
                            .mapped
                            .is_some_and(|m| m.in_changed(base, symbol.name_span.start))
                    {
                        continue;
                    }
                    seen.insert((*ns, name));
                    // A static routine reaches only the static members.
                    if members && statik && !symbol.flags.contains(SymbolFlags::STATIC) {
                        continue;
                    }
                    if let Some(item) =
                        self.symbol_item(base, sym, symbol, members, wanted, expected.as_ref())
                    {
                        items.push(Candidate {
                            nearness: nearness::SCOPE + depth.min(30) as u8,
                            ..item
                        });
                    }
                }
            }
        }
        (items, class_incomplete)
    }

    /// Whether the current text declares function `name` (folded) before
    /// the statement at the cursor: its definition, or a `FORWARD`.
    fn declared_before(&self, name: &str) -> bool {
        let text = self.request.text;
        let window = self.found.window_start as usize;
        self.tokens
            .windows(2)
            .take_while(|pair| pair[1].end <= window)
            .any(|pair| {
                pair[0].kind == oxabl_lexer::Kind::Function
                    && text[pair[1].start..pair[1].end].eq_ignore_ascii_case(name)
            })
    }

    /// The item of one declared symbol, when `wanted` takes its kind.
    fn symbol_item(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        sym: SymbolId,
        symbol: &Symbol,
        member: bool,
        wanted: Wanted,
        expected: Option<&ResolvedType>,
    ) -> Option<Candidate> {
        let semantic = &base.navigation.model.semantic;
        let symbols = &semantic.symbols;
        let schema = &base.navigation.run.config().schema;
        let ty = symbol.data_type.as_ref().filter(|ty| known(ty));
        let type_text = || {
            ty.map(|ty| {
                ty.display_abl(symbols, schema)
                    .to_string()
                    .to_ascii_uppercase()
            })
        };
        let (kind, detail) = match symbol.kind {
            SymbolKind::Variable if member => (ItemKind::Property, type_text()),
            SymbolKind::Variable => (ItemKind::Variable, type_text()),
            SymbolKind::Parameter => {
                let mode = parameter_mode(symbol.flags);
                let detail = match (mode, type_text()) {
                    (Some(mode), Some(ty)) => Some(format!("{mode} {ty}")),
                    (Some(mode), None) => Some(mode.to_string()),
                    (None, ty) => ty,
                };
                (ItemKind::Variable, detail)
            }
            SymbolKind::Property => (ItemKind::Property, type_text()),
            SymbolKind::Dataset => (ItemKind::Variable, Some("Dataset".to_string())),
            SymbolKind::TempTable => (ItemKind::Struct, Some("Temp-table".to_string())),
            SymbolKind::Buffer => {
                let table = symbol
                    .table_id
                    .and_then(|id| schema.get_by_id(id))
                    .map(|table| table.display_name.clone())
                    .or_else(|| {
                        let tt = symbols.temp_table_of(sym)?;
                        Some(self.spelling(base, symbols.get(tt))?.to_string())
                    });
                (
                    ItemKind::Struct,
                    Some(table.map_or_else(|| "Buffer".to_string(), |t| format!("Buffer for {t}"))),
                )
            }
            SymbolKind::Function if symbols.is_method(sym) => {
                let returns = type_text().unwrap_or_else(|| "VOID".to_string());
                let statik = if symbol.flags.contains(SymbolFlags::STATIC) {
                    "STATIC "
                } else {
                    ""
                };
                let others = symbols
                    .method_overloads(sym)
                    .map_or(0, |all| all.len().saturating_sub(1));
                let more = match others {
                    0 => String::new(),
                    1 => " (+1 overload)".to_string(),
                    n => format!(" (+{n} overloads)"),
                };
                (
                    ItemKind::Method,
                    Some(format!("METHOD {statik}{returns}{more}")),
                )
            }
            SymbolKind::Function => (
                ItemKind::Function,
                Some(
                    type_text().map_or_else(|| "FUNCTION".to_string(), |t| format!("FUNCTION {t}")),
                ),
            ),
            _ => return None,
        };
        let takes = match wanted {
            Wanted::Values => true,
            Wanted::StatementStart => {
                matches!(
                    symbol.kind,
                    SymbolKind::Variable
                        | SymbolKind::Parameter
                        | SymbolKind::Property
                        | SymbolKind::Buffer
                        | SymbolKind::TempTable
                ) || symbols.is_method(sym)
            }
            Wanted::Records(after) => match after {
                RecordsAfter::BufferFor => symbol.kind == SymbolKind::TempTable,
                RecordsAfter::BufferForTempTable => symbol.kind == SymbolKind::TempTable,
                RecordsAfter::Like | RecordsAfter::Record => {
                    matches!(symbol.kind, SymbolKind::TempTable | SymbolKind::Buffer)
                }
            },
        };
        if !takes {
            return None;
        }
        let label = self.spelling(base, symbol)?.to_string();
        let fits = match (expected, ty) {
            (Some(expected), Some(ty)) => assignable(ty, expected, ClassLattice::new(symbols)),
            _ => false,
        };
        Some(Candidate {
            insert: label.clone(),
            label,
            kind,
            detail,
            snippet: false,
            data: ItemData::Symbol(sym.raw()),
            fits,
            nearness: nearness::SCOPE,
        })
    }

    /// The name of `symbol` as its declaration writes it.
    fn spelling<'t>(
        &self,
        base: &'t CompletionBase<'_, '_, '_>,
        symbol: &Symbol,
    ) -> Option<&'t str> {
        let span = symbol.name_span;
        let text = base
            .navigation
            .expanded
            .text
            .get(span.start as usize..span.end as usize)?;
        let text = text.trim().trim_matches(['"', '\'']);
        (!text.is_empty()).then_some(text)
    }

    /// The keywords that stand for a value at the cursor, and the built-in
    /// functions of an expression.
    fn value_keywords(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        virt: u32,
        wanted: Wanted,
    ) -> Vec<Candidate> {
        let tree = &base.navigation.model.semantic.scope_tree;
        let scope = base::scope_at(base, virt);
        let class = tree
            .ancestors(scope)
            .find(|id| tree.get(*id).kind == ScopeKind::Class);
        let mut words: Vec<&'static str> = Vec::new();
        if let Some(class) = class {
            words.push("THIS-OBJECT");
            if self.inherits(base, class) {
                words.push("SUPER");
            }
        }
        let mut items: Vec<Candidate> = words
            .into_iter()
            .map(|word| {
                Candidate::keyword(&super::keywords::Keyword { word, title: None }, self.case)
            })
            .collect();
        if wanted == Wanted::Values {
            items.push(Candidate::keyword(
                &super::keywords::Keyword {
                    word: "NEW",
                    title: None,
                },
                self.case,
            ));
            items.extend(
                KEYWORD_DOCS
                    .iter()
                    .enumerate()
                    .filter(|(_, doc)| doc.kind == DocKind::Function)
                    .map(|(at, doc)| {
                        let label = self.case.apply(doc.words[0]);
                        Candidate {
                            insert: label.clone(),
                            label,
                            kind: ItemKind::Function,
                            detail: Some(doc.title.to_string()),
                            snippet: false,
                            data: ItemData::Doc(at as u16),
                            fits: false,
                            nearness: nearness::KEYWORD,
                        }
                    }),
            );
        }
        items
    }

    /// Whether the class whose body is scope `class` names a parent.
    fn inherits(&self, base: &CompletionBase<'_, '_, '_>, class: ScopeId) -> bool {
        let semantic = &base.navigation.model.semantic;
        let owner = semantic.scope_tree.get(class).owner_node;
        semantic
            .symbols
            .iter()
            .find(|(_, symbol)| symbol.declaration == owner && symbol.kind == SymbolKind::Class)
            .and_then(|(sym, _)| semantic.symbols.supertypes(sym))
            .is_some_and(|supertypes| supertypes.inherits.is_some())
    }

    /// The tables of the schema a position takes. A table of the OpenEdge
    /// metaschema (`_file`, ...) only when the word starts with `_`.
    fn tables(&self, base: &CompletionBase<'_, '_, '_>, wanted: Wanted) -> Vec<Candidate> {
        if matches!(wanted, Wanted::Records(RecordsAfter::BufferForTempTable)) {
            return Vec::new();
        }
        let config = base.navigation.run.config();
        let schema = &config.schema;
        let word = &self.request.text[self.found.word_start as usize..self.request.offset as usize];
        let mut items: Vec<Candidate> = schema
            .tables()
            .map(|(_, table)| table)
            .filter(|table| !table.name.as_ref().starts_with('_') || word.starts_with('_'))
            .map(|table| Candidate {
                label: table.display_name.clone(),
                insert: table.display_name.clone(),
                kind: ItemKind::Struct,
                detail: Some("Database table".to_string()),
                snippet: false,
                data: ItemData::None,
                fits: false,
                nearness: nearness::SCHEMA,
            })
            .collect();
        if word.starts_with('_') && schema.metaschema_enabled() {
            items.extend(
                oxabl_schema::metaschema()
                    .tables()
                    .map(|(_, table)| Candidate {
                        label: table.display_name.clone(),
                        insert: table.display_name.clone(),
                        kind: ItemKind::Struct,
                        detail: Some("Database table".to_string()),
                        snippet: false,
                        data: ItemData::None,
                        fits: false,
                        nearness: nearness::SCHEMA,
                    }),
            );
        }
        items
    }
}

/// Whether `scope` is in a static method or property accessor.
fn in_static_routine(base: &CompletionBase<'_, '_, '_>, scope: ScopeId) -> bool {
    let semantic = &base.navigation.model.semantic;
    let tree = &semantic.scope_tree;
    let Some(routine) = tree.ancestors(scope).find(|id| {
        matches!(
            tree.get(*id).kind,
            ScopeKind::Method | ScopeKind::PropertyGet | ScopeKind::PropertySet
        )
    }) else {
        return false;
    };
    let owner = tree.get(routine).owner_node;
    semantic.symbols.iter().any(|(_, symbol)| {
        symbol.declaration == owner
            && matches!(symbol.kind, SymbolKind::Function | SymbolKind::Property)
            && symbol.flags.contains(SymbolFlags::STATIC)
    })
}

/// A type oxabl knows: not unknown, not an error.
fn known(ty: &ResolvedType) -> bool {
    !matches!(ty, ResolvedType::Unknown | ResolvedType::Error)
}

fn parameter_mode(flags: SymbolFlags) -> Option<&'static str> {
    if flags.contains(SymbolFlags::PARAM_INPUT_OUT) {
        Some("INPUT-OUTPUT")
    } else if flags.contains(SymbolFlags::PARAM_INPUT) {
        Some("INPUT")
    } else if flags.contains(SymbolFlags::PARAM_OUTPUT) {
        Some("OUTPUT")
    } else if flags.contains(SymbolFlags::PARAM_RETURN) {
        Some("RETURN")
    } else {
        None
    }
}
