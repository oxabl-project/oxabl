//! The members of a class after `receiver:`, and the inherited members of the
//! own class in a class body. See `docs/design/completion.md`, "Members".
//!
//! The receiver is read from the tokens of the current text, because the
//! statement is not complete and the parser drops it. The class of the
//! receiver and the members it reaches follow the rules of go to definition
//! (`oxabl_semantic::class_surface`). A receiver that oxabl cannot type gives
//! no list.

use std::sync::{Arc, Mutex};

use oxabl_lexer::Kind;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassDescriptor, ClassList, ClassRef, ClassSource, ClassSurface, IndexAnswer, IndexName,
    MemberDescriptor, MemberOrigin, MemberParam, MemberParamShape, MemberReach, MemberType,
    NamespaceId, ParamMode, ResolvedType, ScopeId, ScopeKind, Semantic, SurfaceMember, SymbolId,
    SymbolKind, SymbolTable, class_surface, member_class, resolve_named,
};
use std::collections::HashMap;

use super::context::is_word_byte;
use super::rank::{fit, nearness};
use super::{Candidate, CompletionBase, Engine, ItemData, ItemKind, MemberData, base};
use crate::navigate::Navigation;

/// The answers of a class source that the requests of one base share, so a
/// request reads no class file that an earlier request read. The daemon keeps
/// one for each base and clears it when a file changes.
///
/// A name is kept under the spelling the source wrote: a path derives from
/// that spelling, so two spellings of one name are two questions.
#[derive(Default)]
pub struct ClassMemo {
    entries: Mutex<HashMap<OxablAtom, Entry>>,
}

#[derive(Default, Clone)]
struct Entry {
    class: Option<IndexAnswer<Arc<ClassDescriptor>>>,
    members: Option<IndexAnswer<Arc<[MemberDescriptor]>>>,
}

impl ClassMemo {
    /// Forget every answer: a class file changed.
    pub fn clear(&self) {
        self.lock().clear();
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<OxablAtom, Entry>> {
        self.entries.lock().unwrap_or_else(|e| e.into_inner())
    }
}

/// A [`ClassSource`] over the workspace index of a run, with the answers kept
/// in a [`ClassMemo`] when the base has one.
struct MemoSource<'a> {
    index: &'a dyn oxabl_semantic::WorkspaceIndex,
    memo: Option<&'a ClassMemo>,
}

impl ClassSource for MemoSource<'_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        let Some(memo) = self.memo else {
            return self.index.class(name);
        };
        if let Some(answer) = memo
            .lock()
            .get(name.as_written_atom())
            .and_then(|e| e.class.clone())
        {
            return answer;
        }
        let answer = self.index.class(name);
        memo.lock()
            .entry(name.as_written_atom().clone())
            .or_default()
            .class = Some(answer.clone());
        answer
    }

    fn class_members(&self, name: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        let Some(memo) = self.memo else {
            return self.index.class_members(name);
        };
        if let Some(answer) = memo
            .lock()
            .get(name.as_written_atom())
            .and_then(|e| e.members.clone())
        {
            return answer;
        }
        let answer = self.index.class_members(name);
        memo.lock()
            .entry(name.as_written_atom().clone())
            .or_default()
            .members = Some(answer.clone());
        answer
    }

    fn classes_in(&self, _package: &str) -> ClassList {
        ClassList::default()
    }
}

/// What a request reads to type a receiver.
struct Typing<'a, 'b, 's, 'ba> {
    engine: &'a Engine<'a, 'b, 's, 'ba>,
    base: &'a CompletionBase<'b, 's, 'ba>,
    semantic: &'a Semantic,
    source: &'a dyn ClassSource,
    scope: ScopeId,
    /// The expanded byte of the cursor.
    place: u32,
    /// The class whose body holds the cursor.
    current: Option<SymbolId>,
}

/// The class a receiver evaluates to, and how it reaches it.
struct Typed {
    class: ClassRef,
    reach: MemberReach,
    inside: bool,
}

impl Engine<'_, '_, '_, '_> {
    /// The members of the receiver before the colon at the cursor.
    pub(super) fn members(&self) -> Option<(Vec<Candidate>, bool)> {
        let (base, mapped) = self.based()?;
        if mapped.scope_changed {
            return Some((Vec::new(), true));
        }
        let colon = self.found.before.checked_sub(1)?;
        let receiver = colon.checked_sub(1)?;
        let place = *mapped.places.first()?;
        let scope = base::scope_at(base, place);
        let semantic = &base.navigation.model.semantic;
        base.navigation.run.with_run_index(|index| {
            let source = MemoSource {
                index,
                memo: base.classes,
            };
            let typing = Typing {
                engine: self,
                base,
                semantic,
                source: &source,
                scope,
                place,
                current: current_class(semantic, scope),
            };
            let typed = typing.receiver(receiver)?;
            let surface = class_surface(semantic, &source, typed.class, typed.reach, typed.inside);
            let items = surface
                .members
                .iter()
                .filter_map(|member| self.member_item(base, member, nearness::SCOPE))
                .collect();
            Some((items, !surface.complete || self.base_incomplete()))
        })
    }

    /// The inherited members of the class whose body is scope `class`, for
    /// the names of a class body: what the surface of `THIS-OBJECT` and of
    /// the type name hold that the body does not declare. `statik` is true in
    /// a static routine, which reaches only the static members.
    pub(super) fn inherited_names(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        class: ScopeId,
        statik: bool,
    ) -> (Vec<(SurfaceMember, Candidate)>, bool) {
        let semantic = &base.navigation.model.semantic;
        let Some(symbol) = class_symbol(semantic, class) else {
            return (Vec::new(), true);
        };
        base.navigation.run.with_run_index(|index| {
            let source = MemoSource {
                index,
                memo: base.classes,
            };
            let mut surfaces: Vec<ClassSurface> = Vec::new();
            if !statik {
                surfaces.push(class_surface(
                    semantic,
                    &source,
                    ClassRef::Local(symbol),
                    MemberReach::ThisObject,
                    true,
                ));
            }
            surfaces.push(class_surface(
                semantic,
                &source,
                ClassRef::Local(symbol),
                MemberReach::TypeName,
                true,
            ));
            let mut complete = true;
            let mut items = Vec::new();
            for surface in surfaces {
                complete &= surface.complete;
                for member in surface.members {
                    if matches!(member.origin, MemberOrigin::Local(_)) {
                        continue;
                    }
                    if let Some(item) = self.member_item(base, &member, nearness::INHERITED) {
                        items.push((member, item));
                    }
                }
            }
            (items, !complete)
        })
    }

    /// The item of one member.
    fn member_item(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        member: &SurfaceMember,
        nearness: u8,
    ) -> Option<Candidate> {
        let symbols = &base.navigation.model.semantic.symbols;
        let label = match &member.origin {
            MemberOrigin::Local(anchor) => self.spelling(base, symbols.get(*anchor))?.to_string(),
            MemberOrigin::Workspace { .. } => member.name.clone(),
        };
        let first = member.signatures.first()?;
        let (kind, detail) = match member.kind {
            SymbolKind::Function => {
                let returns = match &first.ty {
                    MemberType::Untyped => "VOID".to_string(),
                    other => type_text(base, other).unwrap_or_else(|| "?".to_string()),
                };
                let statik = if member.is_static { "STATIC " } else { "" };
                let params = first.params.as_ref().map_or_else(String::new, |params| {
                    format!(" ({})", params_text(base, params))
                });
                let more = match member.signatures.len() - 1 {
                    0 => String::new(),
                    1 => " (+1 overload)".to_string(),
                    n => format!(" (+{n} overloads)"),
                };
                (
                    ItemKind::Method,
                    format!("METHOD {statik}{returns}{params}{more}"),
                )
            }
            SymbolKind::Property | SymbolKind::Variable => {
                if member.declared_in_enum && member.is_static {
                    (ItemKind::EnumMember, member.declared_in.clone())
                } else {
                    (
                        ItemKind::Property,
                        type_text(base, &first.ty).unwrap_or_default(),
                    )
                }
            }
            SymbolKind::Event => (ItemKind::Event, "EVENT".to_string()),
            SymbolKind::TempTable => (ItemKind::Struct, "Temp-table".to_string()),
            SymbolKind::Buffer => (ItemKind::Struct, "Buffer".to_string()),
            SymbolKind::Dataset => (ItemKind::Struct, "Dataset".to_string()),
            SymbolKind::DataSource => (ItemKind::Struct, "Data-source".to_string()),
            _ => return None,
        };
        let data = match &member.origin {
            MemberOrigin::Local(anchor) => ItemData::Symbol(anchor.raw()),
            MemberOrigin::Workspace { .. } => ItemData::Member(Box::new(MemberData {
                class: member.declared_in.clone(),
                kind: member_kind_code(member.kind),
                name: member.name.clone(),
            })),
        };
        Some(Candidate {
            insert: label.clone(),
            label,
            kind,
            detail: (!detail.is_empty()).then_some(detail),
            snippet: false,
            data,
            fit: fit::OTHER,
            nearness,
        })
    }
}

impl Typing<'_, '_, '_, '_> {
    fn symbols(&self) -> &SymbolTable {
        &self.semantic.symbols
    }

    fn text(&self, token: usize) -> &str {
        let token = &self.engine.tokens[token];
        &self.engine.request.text[token.start..token.end]
    }

    fn kind(&self, token: usize) -> Kind {
        self.engine.tokens[token].kind
    }

    /// Whether tokens `a` and `b` touch: nothing between them.
    fn joined(&self, a: usize, b: usize) -> bool {
        self.engine.tokens[a].end == self.engine.tokens[b].start
    }

    /// Whether token `token` is a name: an identifier, or a keyword spelled
    /// like one (a member can be named `Get` or `Message`).
    fn is_name(&self, token: usize) -> bool {
        let text = self.text(token);
        !text.is_empty()
            && text.bytes().all(is_word_byte)
            && text.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
    }

    /// The first token of the dotted name that ends at `end`.
    fn dotted_start(&self, end: usize) -> usize {
        let mut start = end;
        while start >= 2
            && self.kind(start - 1) == Kind::Period
            && self.is_name(start - 2)
            && self.joined(start - 2, start - 1)
            && self.joined(start - 1, start)
        {
            start -= 2;
        }
        start
    }

    /// The text of tokens `from..=to`.
    fn span(&self, from: usize, to: usize) -> &str {
        let tokens = self.engine.tokens;
        &self.engine.request.text[tokens[from].start..tokens[to].end]
    }

    /// The class and role of the receiver whose last token is `end`.
    fn receiver(&self, end: usize) -> Option<Typed> {
        match self.kind(end) {
            Kind::ThisObject => {
                let class = self.current?;
                Some(Typed {
                    class: ClassRef::Local(class),
                    reach: MemberReach::ThisObject,
                    inside: true,
                })
            }
            Kind::RightParen => self.parenthesized(end),
            _ if self.is_name(end) => self.named(end),
            _ => None,
        }
    }

    /// A receiver that ends in a name: `SUPER`, a chain link, a variable or a
    /// type name.
    fn named(&self, end: usize) -> Option<Typed> {
        let start = self.dotted_start(end);
        if start == end
            && end >= 2
            && self.kind(end - 1) == Kind::Colon
            && self.joined(end - 2, end - 1)
            && self.joined(end - 1, end)
        {
            return self.link(end, end - 2, false);
        }
        if start > 0 && self.kind(start - 1) == Kind::Colon && self.joined(start - 1, start) {
            // `a:b.c:` is no form oxabl reads.
            return None;
        }
        let name = self.span(start, end);
        if start == end && name.eq_ignore_ascii_case("super") {
            let class = self.current?;
            self.symbols()
                .supertypes(class)
                .and_then(|supers| supers.inherits.as_ref())?;
            return Some(Typed {
                class: ClassRef::Local(class),
                reach: MemberReach::Super,
                inside: true,
            });
        }
        let first = &name[..name.find('.').unwrap_or(name.len())];
        let tree = &self.semantic.scope_tree;
        if start == end
            && let Some(sym) = tree.resolve_name(self.scope, NamespaceId::Values, name)
        {
            return self.value(sym);
        }
        // A buffer, a table or a frame is no class: `FOR EACH Customer:` is
        // a block colon.
        if [
            NamespaceId::Values,
            NamespaceId::Buffers,
            NamespaceId::Tables,
            NamespaceId::Frames,
        ]
        .iter()
        .any(|ns| tree.resolve_name(self.scope, *ns, first).is_some())
        {
            return None;
        }
        let config = self.base.navigation.run.config();
        if config.schema_loaded
            && config
                .schema
                .resolve_table_with_metaschema(IndexName::new(first).as_atom())
                .is_some()
        {
            return None;
        }
        self.type_name(name)
    }

    /// A variable, parameter or property that the cursor sees, as a receiver.
    fn value(&self, sym: SymbolId) -> Option<Typed> {
        let symbols = self.symbols();
        let symbol = symbols.get(sym);
        let in_class = self.semantic.scope_tree.get(symbol.declared_in).kind() == ScopeKind::Class;
        // A name the cursor cannot see yet, or one that the text changed
        // since the base, is not typed.
        if symbol.declaration == oxabl_ast::NodeId::DUMMY
            || !(in_class || symbol.name_span.end <= self.place)
            || self
                .engine
                .mapped
                .is_some_and(|m| m.in_changed(self.base, symbol.name_span.start))
        {
            return None;
        }
        let Some(ResolvedType::Class(class)) = symbol.data_type else {
            return None;
        };
        self.instance(ClassRef::of(symbols, class)?)
    }

    /// A value of `class`.
    fn instance(&self, class: ClassRef) -> Option<Typed> {
        Some(Typed {
            inside: self.is_current(&class),
            class,
            reach: MemberReach::Instance,
        })
    }

    fn is_current(&self, class: &ClassRef) -> bool {
        matches!((class, self.current), (ClassRef::Local(sym), Some(current)) if *sym == current)
    }

    /// The type name `raw` as a receiver of static members.
    fn type_name(&self, raw: &str) -> Option<Typed> {
        let symbols = self.symbols();
        let local = self
            .semantic
            .scope_tree
            .resolve_name(self.scope, NamespaceId::Types, raw)
            .and_then(|sym| ClassRef::of(symbols, sym));
        let class = match local {
            Some(class) => class,
            None => {
                let imports = symbols.imports().map_or_else(Arc::default, Arc::clone);
                resolve_named(self.source, &imports, raw)?
            }
        };
        Some(Typed {
            inside: self.is_current(&class),
            class,
            reach: MemberReach::TypeName,
        })
    }

    /// A type name from the tokens `from..=to`: a dotted name, nothing else.
    fn type_tokens(&self, from: usize, to: usize) -> Option<ClassRef> {
        if from > to || self.dotted_start(to) != from || !self.is_name(from) {
            return None;
        }
        let raw = self.span(from, to);
        // A class of a name that a value or a buffer holds is not a type.
        let typed = self.type_name(raw)?;
        Some(typed.class)
    }

    /// The receiver `... )` where the parenthesis closes a `NEW`, a `CAST` or
    /// the call of a method.
    fn parenthesized(&self, close: usize) -> Option<Typed> {
        let open = self.matching_open(close)?;
        let before = open.checked_sub(1)?;
        if self.kind(before) == Kind::Cast {
            let comma = self.last_comma(open, close)?;
            let class = self.type_tokens(comma + 1, close.checked_sub(1)?)?;
            return self.instance(class);
        }
        if !self.is_name(before) {
            return None;
        }
        let start = self.dotted_start(before);
        if start > 0 && self.kind(start - 1) == Kind::New {
            let class = self.type_tokens(start, before)?;
            return self.instance(class);
        }
        if start == before
            && before >= 2
            && self.kind(before - 1) == Kind::Colon
            && self.joined(before - 2, before - 1)
            && self.joined(before - 1, before)
        {
            return self.link(before, before - 2, true);
        }
        None
    }

    /// The class of the member named by token `name` of the receiver that
    /// ends at token `parent`.
    fn link(&self, name: usize, parent: usize, call: bool) -> Option<Typed> {
        let typed = self.receiver(parent)?;
        let surface = class_surface(
            self.semantic,
            self.source,
            typed.class,
            typed.reach,
            typed.inside,
        );
        let wanted = self.text(name);
        let member = surface.members.iter().find(|member| {
            (member.kind == SymbolKind::Function) == call
                && member.name.eq_ignore_ascii_case(wanted)
        })?;
        self.instance(member_class(self.source, member, call)?)
    }

    /// The `(` that the `)` at `close` closes.
    fn matching_open(&self, close: usize) -> Option<usize> {
        let mut depth = 0usize;
        for at in (0..=close).rev() {
            match self.kind(at) {
                Kind::RightParen => depth += 1,
                Kind::LeftParen => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(at);
                    }
                }
                // A statement end: the parenthesis is not in this statement.
                Kind::Period if at + 1 < self.engine.tokens.len() && !self.joined(at, at + 1) => {
                    return None;
                }
                _ => {}
            }
        }
        None
    }

    /// The last comma between the parentheses `open` and `close` that is not
    /// inside a deeper pair.
    fn last_comma(&self, open: usize, close: usize) -> Option<usize> {
        let mut depth = 0usize;
        for at in (open + 1..close).rev() {
            match self.kind(at) {
                Kind::RightParen => depth += 1,
                Kind::LeftParen => depth = depth.checked_sub(1)?,
                Kind::Comma if depth == 0 => return Some(at),
                _ => {}
            }
        }
        None
    }
}

/// The class whose body holds `scope`, as its symbol.
fn current_class(semantic: &Semantic, scope: ScopeId) -> Option<SymbolId> {
    let tree = &semantic.scope_tree;
    let class = tree
        .ancestors(scope)
        .find(|id| tree.get(*id).kind() == ScopeKind::Class)?;
    class_symbol(semantic, class)
}

/// The symbol of the class or interface whose body is `scope`.
fn class_symbol(semantic: &Semantic, scope: ScopeId) -> Option<SymbolId> {
    let owner = semantic.scope_tree.get(scope).owner_node();
    semantic
        .symbols
        .iter()
        .find(|(_, symbol)| {
            symbol.declaration == owner
                && matches!(symbol.kind, SymbolKind::Class | SymbolKind::Interface)
        })
        .map(|(sym, _)| sym)
}

/// The code `ItemData::Member` carries for a member kind, which resolve maps
/// back with [`member_kind`].
fn member_kind_code(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::Function => "method",
        SymbolKind::Property => "property",
        SymbolKind::Variable => "variable",
        SymbolKind::Event => "event",
        SymbolKind::TempTable => "temp-table",
        SymbolKind::Buffer => "buffer",
        SymbolKind::Dataset => "dataset",
        _ => "other",
    }
}

fn member_kind(code: &str) -> Option<SymbolKind> {
    Some(match code {
        "method" => SymbolKind::Function,
        "property" => SymbolKind::Property,
        "variable" => SymbolKind::Variable,
        "event" => SymbolKind::Event,
        "temp-table" => SymbolKind::TempTable,
        "buffer" => SymbolKind::Buffer,
        "dataset" => SymbolKind::Dataset,
        _ => return None,
    })
}

/// A type as a detail: a primitive in upper case, a class as written. `None`
/// for a type oxabl does not know.
fn type_text(base: &CompletionBase<'_, '_, '_>, ty: &MemberType) -> Option<String> {
    match ty {
        MemberType::Named(name) => Some(name.as_written().to_string()),
        MemberType::Untyped => None,
        MemberType::Portable(portable) => {
            let resolved = portable.as_resolved();
            if matches!(resolved, ResolvedType::Unknown | ResolvedType::Error) {
                return None;
            }
            let symbols = &base.navigation.model.semantic.symbols;
            let schema = &base.navigation.run.config().schema;
            Some(
                resolved
                    .display_abl(symbols, schema)
                    .to_string()
                    .to_ascii_uppercase(),
            )
        }
    }
}

/// The parameters of a method as a detail: `INPUT CHARACTER, OUTPUT INTEGER`.
pub(super) fn params_text(base: &CompletionBase<'_, '_, '_>, params: &[MemberParam]) -> String {
    params
        .iter()
        .map(|param| {
            let mode = match param.mode {
                ParamMode::Input => "INPUT",
                ParamMode::Output => "OUTPUT",
                ParamMode::InputOutput => "INPUT-OUTPUT",
            };
            let shape = match &param.shape {
                MemberParamShape::Value(ty) => type_text(base, ty).unwrap_or_else(|| "?".into()),
                MemberParamShape::Like { source, .. } => {
                    format!("LIKE {}", source.as_written())
                }
                MemberParamShape::Buffer => "BUFFER".to_string(),
                MemberParamShape::Table => "TABLE".to_string(),
                MemberParamShape::TableHandle => "TABLE-HANDLE".to_string(),
                MemberParamShape::Dataset => "DATASET".to_string(),
                MemberParamShape::DatasetHandle => "DATASET-HANDLE".to_string(),
            };
            format!("{mode} {shape}")
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// The documentation of the member `name` (of `kind`, a code of
/// `ItemData::Member`) that `class` declares: each declaration and the class,
/// as the hover of an inherited member shows them.
pub fn member_documentation(
    navigation: &Navigation<'_, '_>,
    class: &str,
    kind: &str,
    name: &str,
) -> Option<String> {
    let kind = member_kind(kind)?;
    let file =
        navigation
            .run
            .with_run_index(|index| match index.class(&IndexName::new(class)) {
                IndexAnswer::Found(descriptor) => Some(descriptor.file),
                _ => None,
            })?;
    navigation.member_hover(file, kind, name)
}
