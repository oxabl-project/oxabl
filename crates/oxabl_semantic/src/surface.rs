//! One interface for the members and supertypes of a class of any origin: a
//! class of the analysed file, a workspace class or a built-in type. See
//! `docs/design/builtin-classes.md`, "One interface for classes of every
//! origin".
//!
//! Code completion, signature help and the type hierarchy read a class
//! through [`surface`] and [`supertypes`]. The resolve pass reads a built-in
//! level through [`builtin_level_members`], the one place that says what a
//! level of the catalog contributes, so the two cannot disagree on it.

use std::collections::VecDeque;

use oxabl_ast::{AccessModifier, NodeId};
use rustc_hash::FxHashSet;

use crate::builtin::{
    BuiltinCatalog, BuiltinClass, BuiltinMember, BuiltinMemberKind, BuiltinShape, BuiltinType,
};
use crate::index::{
    CandidateAnswer, ImportCandidate, ImportContext, IndexAnswer, MemberDescriptor, PortableType,
    lookup_candidate,
};
use crate::resolve::{SupertypeLookup, lookup_supertype};
use crate::signature::{MemberParam, MemberParamShape, ParamMode};
use crate::symbol::BuiltinMemberRef;
use crate::{
    ClassKind, IndexName, IndexedFileId, MemberType, NamespaceId, ResolvedType, Semantic,
    SymbolFlags, SymbolId, SymbolKind, WorkspaceIndex,
};

/// Where a class or interface comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassOrigin {
    /// A class of the analysed file, by its symbol.
    Local(SymbolId),
    /// A class of the workspace index.
    Workspace(IndexName),
    /// A documented built-in type.
    Builtin(&'static BuiltinClass),
}

/// How a receiver reaches its class, which decides the members it reaches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SurfaceRole {
    /// A value of the class: only `PUBLIC` instance members.
    Instance,
    /// The type name: only `PUBLIC` static members.
    TypeName,
    /// `THIS-OBJECT` in the class body: own members of any access, and what
    /// the class inherits.
    ThisObject,
    /// `SUPER` in the class body: only what the class inherits.
    Super,
}

/// One parameter of a member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceParam {
    /// `None` when the declaration or the documentation gives no name.
    pub name: Option<String>,
    /// `None` when the mode is not known (a built-in parameter).
    pub mode: Option<ParamMode>,
    pub shape: MemberParamShape,
}

/// Where the declaration of a member is, for its documentation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberOrigin {
    /// A member declared in the analysed file.
    Local(SymbolId),
    /// A member of a workspace class, in this file.
    File(IndexedFileId),
    /// A member of a built-in type: its catalog row gives the documentation.
    Builtin(BuiltinMemberRef),
}

/// One member a role reaches: one entry for each overload of a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceMember {
    /// As the declaration or the documentation writes it (a workspace member
    /// is folded, as the index keeps it).
    pub name: String,
    /// `Function` for a method, `Property`, `Event`, or a data member's kind.
    /// An enum member is a static `Property`.
    pub kind: SymbolKind,
    pub access: AccessModifier,
    pub is_static: bool,
    /// The type as written: a class by its qualified name.
    pub ty: MemberType,
    /// The parameters of a method; `None` for another member and for a list
    /// oxabl does not know.
    pub params: Option<Vec<SurfaceParam>>,
    /// The class whose level declares the member.
    pub declared_in: ClassOrigin,
    pub origin: MemberOrigin,
}

/// The members a role reaches, nearest level first.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassSurface {
    pub members: Vec<SurfaceMember>,
    /// `false` when a member can be missing: a level oxabl could not read, a
    /// level whose member list is not whole, an unknown or .NET supertype, a
    /// generic level, or a cycle.
    pub complete: bool,
}

/// A supertype as a header names it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SupertypeOrigin {
    Class(ClassOrigin),
    /// A name that names no class oxabl knows, as written.
    Unresolved(String),
}

/// The direct supertypes of a class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Supertypes {
    pub parent: Option<SupertypeOrigin>,
    pub interfaces: Vec<SupertypeOrigin>,
    /// Every supertype is known (a built-in type with a known header, or a
    /// class whose header names only classes oxabl found).
    pub header_known: bool,
}

/// What a built-in level contributes to a surface: each method, property and
/// event with its kind and namespace. Constructors are not members.
pub(crate) fn builtin_level_members(
    class: &'static BuiltinClass,
) -> impl Iterator<Item = (SymbolKind, NamespaceId, &'static BuiltinMember)> {
    with_namespace(class.members())
}

/// [`builtin_level_members`] for the members named `name` (any case).
pub(crate) fn builtin_level_members_named(
    class: &'static BuiltinClass,
    name: &str,
) -> impl Iterator<Item = (SymbolKind, NamespaceId, &'static BuiltinMember)> {
    with_namespace(class.members_named(name))
}

fn with_namespace(
    members: &'static [BuiltinMember],
) -> impl Iterator<Item = (SymbolKind, NamespaceId, &'static BuiltinMember)> {
    members.iter().filter_map(|m| {
        let (kind, ns) = match m.kind() {
            BuiltinMemberKind::Method => (SymbolKind::Function, NamespaceId::Functions),
            BuiltinMemberKind::Property => (SymbolKind::Property, NamespaceId::Values),
            BuiltinMemberKind::Event => (SymbolKind::Event, NamespaceId::Events),
            BuiltinMemberKind::Constructor => return None,
        };
        Some((kind, ns, m))
    })
}

/// The namespace a workspace member of `kind` resolves in, or `None` for a
/// kind that is no member.
pub(crate) fn member_namespace(kind: SymbolKind) -> Option<NamespaceId> {
    Some(match kind {
        SymbolKind::Function => NamespaceId::Functions,
        SymbolKind::Property
        | SymbolKind::Variable
        | SymbolKind::Dataset
        | SymbolKind::DataSource => NamespaceId::Values,
        SymbolKind::Buffer | SymbolKind::TempTable => NamespaceId::Buffers,
        SymbolKind::Event => NamespaceId::Events,
        _ => return None,
    })
}

/// The type of a built-in member as written: a class by its documented name.
pub fn builtin_member_type(ty: BuiltinType) -> MemberType {
    let portable = |t: ResolvedType| MemberType::Portable(PortableType::new(t).expect("portable"));
    let array = |t: ResolvedType| ResolvedType::Array {
        element: Box::new(t),
        extent: None,
    };
    match ty {
        BuiltinType::Void => MemberType::Untyped,
        BuiltinType::Primitive(p) => portable(ResolvedType::Primitive(p)),
        BuiltinType::PrimitiveArray(p) => portable(array(ResolvedType::Primitive(p))),
        BuiltinType::Class(row) => MemberType::Named(IndexName::new(BuiltinClass::at(row).name)),
        // An array of a class has no portable form.
        BuiltinType::ClassArray(_) | BuiltinType::Shape(_) | BuiltinType::Unknown => {
            portable(ResolvedType::Unknown)
        }
    }
}

fn builtin_params(member: &BuiltinMember) -> Vec<SurfaceParam> {
    member
        .params()
        .iter()
        .map(|p| SurfaceParam {
            name: (!p.name.is_empty()).then(|| p.name.to_string()),
            mode: None,
            shape: match p.ty {
                BuiltinType::Shape(BuiltinShape::Table) => MemberParamShape::Table,
                BuiltinType::Shape(BuiltinShape::TableHandle) => MemberParamShape::TableHandle,
                BuiltinType::Shape(BuiltinShape::Dataset) => MemberParamShape::Dataset,
                BuiltinType::Shape(BuiltinShape::DatasetHandle) => MemberParamShape::DatasetHandle,
                BuiltinType::Shape(BuiltinShape::Buffer) => MemberParamShape::Buffer,
                other => MemberParamShape::Value(builtin_member_type(other)),
            },
        })
        .collect()
}

fn workspace_params(params: &[MemberParam]) -> Vec<SurfaceParam> {
    params
        .iter()
        .map(|p| SurfaceParam {
            name: None,
            mode: Some(p.mode),
            shape: p.shape.clone(),
        })
        .collect()
}

/// A level of the walk.
enum Level {
    Workspace(IndexName),
    Builtin(&'static BuiltinClass),
    Local(SymbolId),
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Name(IndexName),
    Row(u16),
    Sym(SymbolId),
}

impl Level {
    fn key(&self) -> Key {
        match self {
            Level::Workspace(n) => Key::Name(n.clone()),
            Level::Builtin(c) => Key::Row(c.row()),
            Level::Local(s) => Key::Sym(*s),
        }
    }
}

/// The built-in type a supertype name written under `imports` means, by the
/// order of the compiler: a `Progress.*` type first, else a workspace class
/// wins over an `OpenEdge.*` or `Ccs.*` one. `None` when a workspace class
/// answers, or when an earlier candidate was not looked up.
fn builtin_supertype(
    index: &dyn WorkspaceIndex,
    searched: bool,
    imports: &ImportContext,
    raw: &str,
) -> Option<&'static BuiltinClass> {
    if !raw.contains('.') && imports.has_unseen_imports() {
        return None;
    }
    for candidate in imports.candidates(raw) {
        let ImportCandidate::Name(name) = candidate else {
            return None;
        };
        let found = lookup_candidate(&name, || {
            (!matches!(index.class(&name), IndexAnswer::NotFound)).then_some(())
        });
        match found {
            CandidateAnswer::Builtin(class) => return Some(class),
            CandidateAnswer::Index(()) => return None,
            CandidateAnswer::Next if !searched => return None,
            CandidateAnswer::Next => {}
        }
    }
    None
}

/// The members `role` reaches on the class `origin`, own and inherited,
/// nearest first. A workspace level is read through `index`; a built-in
/// level from `catalog`. A class of the analysed file needs its
/// [`Semantic`]: see [`local_surface`].
pub fn surface(
    origin: &ClassOrigin,
    index: &dyn WorkspaceIndex,
    catalog: &BuiltinCatalog,
    role: SurfaceRole,
) -> ClassSurface {
    Walk::new(index, catalog, None, role).run(origin)
}

/// [`surface`] for any origin, with the analysed file's [`Semantic`] for a
/// class it declares.
pub fn local_surface(
    sem: &Semantic,
    origin: &ClassOrigin,
    index: &dyn WorkspaceIndex,
    role: SurfaceRole,
) -> ClassSurface {
    Walk::new(index, &crate::BUILTIN_CATALOG, Some(sem), role).run(origin)
}

/// The direct supertypes of the class `origin`.
pub fn supertypes(
    origin: &ClassOrigin,
    index: &dyn WorkspaceIndex,
    catalog: &BuiltinCatalog,
) -> Supertypes {
    match origin {
        ClassOrigin::Builtin(class) => {
            let documented = |name: &str| {
                catalog
                    .class(name)
                    .map(|c| SupertypeOrigin::Class(ClassOrigin::Builtin(c)))
                    .unwrap_or_else(|| SupertypeOrigin::Unresolved(name.to_string()))
            };
            Supertypes {
                parent: class.inherits.map(documented),
                interfaces: class.implements.iter().map(|n| documented(n)).collect(),
                header_known: class.header_known(),
            }
        }
        ClassOrigin::Workspace(name) => match index.class(name) {
            IndexAnswer::Found(descriptor) => {
                let mut header_known = true;
                let mut resolve = |raw: &IndexName| {
                    let raw = raw.as_written();
                    if let Some(c) = builtin_supertype(index, true, &descriptor.imports, raw) {
                        return SupertypeOrigin::Class(ClassOrigin::Builtin(c));
                    }
                    match lookup_supertype(index, &descriptor.imports, raw) {
                        SupertypeLookup::Class(n) => {
                            if !matches!(index.class(&n), IndexAnswer::Found(_)) {
                                header_known = false;
                            }
                            SupertypeOrigin::Class(ClassOrigin::Workspace(n))
                        }
                        SupertypeLookup::Assembly | SupertypeLookup::Missing { .. } => {
                            header_known = false;
                            SupertypeOrigin::Unresolved(raw.to_string())
                        }
                    }
                };
                let parent = descriptor.inherits.as_ref().map(&mut resolve);
                let interfaces = descriptor.implements.iter().map(&mut resolve).collect();
                Supertypes {
                    parent,
                    interfaces,
                    header_known,
                }
            }
            IndexAnswer::NotFound | IndexAnswer::Unusable | IndexAnswer::Unknowable => Supertypes {
                parent: None,
                interfaces: Vec::new(),
                header_known: false,
            },
        },
        ClassOrigin::Local(_) => Supertypes {
            parent: None,
            interfaces: Vec::new(),
            header_known: false,
        },
    }
}

struct Walk<'a> {
    index: &'a dyn WorkspaceIndex,
    catalog: &'a BuiltinCatalog,
    sem: Option<&'a Semantic>,
    role: SurfaceRole,
    members: Vec<SurfaceMember>,
    complete: bool,
}

impl<'a> Walk<'a> {
    fn new(
        index: &'a dyn WorkspaceIndex,
        catalog: &'a BuiltinCatalog,
        sem: Option<&'a Semantic>,
        role: SurfaceRole,
    ) -> Self {
        Walk {
            index,
            catalog,
            sem,
            role,
            members: Vec::new(),
            complete: true,
        }
    }

    fn run(mut self, origin: &ClassOrigin) -> ClassSurface {
        let mut queue: VecDeque<(Level, bool)> = VecDeque::new();
        let mut visited: FxHashSet<Key> = FxHashSet::default();
        let start = match origin {
            ClassOrigin::Workspace(n) => Level::Workspace(n.clone()),
            ClassOrigin::Builtin(c) => Level::Builtin(c),
            ClassOrigin::Local(s) => Level::Local(*s),
        };
        // `SUPER` reaches only what the class inherits.
        let own = self.role != SurfaceRole::Super;
        queue.push_back((start, own));
        let object = self.catalog.class("Progress.Lang.Object");
        let mut first = true;
        loop {
            let Some((level, include)) = queue.pop_front().or_else(|| {
                object
                    .filter(|o| !visited.contains(&Key::Row(o.row())))
                    .map(|o| (Level::Builtin(o), true))
            }) else {
                break;
            };
            if !visited.insert(level.key()) {
                continue;
            }
            let near = first;
            first = false;
            match &level {
                Level::Builtin(class) => self.builtin(class, include, &mut queue),
                Level::Workspace(name) => self.workspace(name, include, near, &mut queue),
                Level::Local(sym) => self.local(*sym, include, &mut queue),
            }
        }
        ClassSurface {
            members: self.members,
            complete: self.complete,
        }
    }

    /// Whether `role` reaches a member with this access and staticness, on a
    /// level that is the class itself (`own`) or an ancestor.
    fn reaches(&self, access: AccessModifier, is_static: bool, own: bool) -> bool {
        let static_ok = match self.role {
            SurfaceRole::TypeName => is_static,
            SurfaceRole::Instance => !is_static,
            SurfaceRole::ThisObject | SurfaceRole::Super => true,
        };
        let access_ok = match self.role {
            SurfaceRole::Instance | SurfaceRole::TypeName => access == AccessModifier::Public,
            SurfaceRole::ThisObject | SurfaceRole::Super => {
                own || matches!(
                    access,
                    AccessModifier::Public
                        | AccessModifier::Protected
                        | AccessModifier::PackageProtected
                )
            }
        };
        static_ok && access_ok
    }

    /// Add `member` unless a nearer level has it: the same name for a
    /// property or event, the same name and parameter list for a method.
    fn push(&mut self, member: SurfaceMember) {
        let shadowed = self.members.iter().any(|m| {
            m.name.eq_ignore_ascii_case(&member.name)
                && (m.kind != SymbolKind::Function
                    || member.kind != SymbolKind::Function
                    || same_params(m.params.as_deref(), member.params.as_deref()))
        });
        if !shadowed {
            self.members.push(member);
        }
    }

    fn builtin(
        &mut self,
        class: &'static BuiltinClass,
        include: bool,
        queue: &mut VecDeque<(Level, bool)>,
    ) {
        if !class.members_trusted() || !class.header_known() || class.is_generic() {
            self.complete = false;
        }
        if class.kind == ClassKind::Enum && class.enum_members().is_empty() {
            self.complete = false;
        }
        for parent in class.supertypes() {
            queue.push_back((Level::Builtin(parent), true));
        }
        if !include {
            return;
        }
        for (kind, _, m) in builtin_level_members(class) {
            if !self.reaches(AccessModifier::Public, m.is_static(), false) {
                continue;
            }
            self.push(SurfaceMember {
                name: m.name().to_string(),
                kind,
                access: AccessModifier::Public,
                is_static: m.is_static(),
                ty: builtin_member_type(m.ty()),
                params: (kind == SymbolKind::Function).then(|| builtin_params(m)),
                declared_in: ClassOrigin::Builtin(class),
                origin: MemberOrigin::Builtin(BuiltinMemberRef::Member(m)),
            });
        }
        if self.reaches(AccessModifier::Public, true, false) {
            for name in class.enum_members() {
                self.push(SurfaceMember {
                    name: name.to_string(),
                    kind: SymbolKind::Property,
                    access: AccessModifier::Public,
                    is_static: true,
                    ty: MemberType::Named(IndexName::new(class.name)),
                    params: None,
                    declared_in: ClassOrigin::Builtin(class),
                    origin: MemberOrigin::Builtin(BuiltinMemberRef::EnumMember(class, name)),
                });
            }
        }
    }

    fn workspace(
        &mut self,
        name: &IndexName,
        include: bool,
        own: bool,
        queue: &mut VecDeque<(Level, bool)>,
    ) {
        let IndexAnswer::Found(descriptor) = self.index.class(name) else {
            self.complete = false;
            return;
        };
        if !descriptor.members_complete {
            self.complete = false;
        }
        let supers = descriptor.inherits.iter().chain(&descriptor.implements);
        for raw in supers {
            let raw = raw.as_written();
            if let Some(c) = builtin_supertype(self.index, true, &descriptor.imports, raw) {
                queue.push_back((Level::Builtin(c), true));
                continue;
            }
            match lookup_supertype(self.index, &descriptor.imports, raw) {
                SupertypeLookup::Class(n) => queue.push_back((Level::Workspace(n), true)),
                SupertypeLookup::Assembly | SupertypeLookup::Missing { .. } => {
                    self.complete = false
                }
            }
        }
        if descriptor.kind == ClassKind::Enum
            && let Some(base) = self.catalog.class("Progress.Lang.Enum")
        {
            queue.push_back((Level::Builtin(base), true));
        }
        if !include {
            return;
        }
        let IndexAnswer::Found(members) = self.index.class_members(name) else {
            self.complete = false;
            return;
        };
        for m in members.iter() {
            if member_namespace(m.kind).is_none() || !self.reaches_workspace(m, own) {
                continue;
            }
            self.push(SurfaceMember {
                name: m.name.as_written().to_string(),
                kind: m.kind,
                access: m.access,
                is_static: m.is_static,
                ty: m.ty.clone(),
                params: m.params.as_deref().map(workspace_params),
                declared_in: ClassOrigin::Workspace(name.clone()),
                origin: MemberOrigin::File(descriptor.file),
            });
        }
    }

    fn reaches_workspace(&self, m: &MemberDescriptor, own: bool) -> bool {
        self.reaches(m.access, m.is_static, own)
    }

    fn local(&mut self, sym: SymbolId, include: bool, queue: &mut VecDeque<(Level, bool)>) {
        let Some(sem) = self.sem else {
            self.complete = false;
            return;
        };
        if sem.symbols.has_incomplete_chain(sym) {
            self.complete = false;
        }
        for &parent in sem.symbols.resolved_supertypes(sym) {
            match sem.symbols.class_origin(parent) {
                Some(ClassOrigin::Builtin(c)) => queue.push_back((Level::Builtin(c), true)),
                Some(ClassOrigin::Workspace(n)) => queue.push_back((Level::Workspace(n), true)),
                Some(ClassOrigin::Local(s)) => queue.push_back((Level::Local(s), true)),
                None => self.complete = false,
            }
        }
        // A header name that resolved to nothing oxabl reads.
        let authored = sem
            .symbols
            .supertypes(sym)
            .map_or(0, |s| s.inherits.iter().count() + s.implements.len());
        if authored > sem.symbols.resolved_supertypes(sym).len() {
            self.complete = false;
        }
        if !include {
            return;
        }
        let declaration = sem.symbols.get(sym).declaration;
        let Some((_, scope)) = sem.scope_tree.iter().find(|(_, s)| {
            s.owner_node == declaration
                && declaration != NodeId::DUMMY
                && matches!(
                    s.kind,
                    crate::ScopeKind::Class | crate::ScopeKind::Interface
                )
        }) else {
            self.complete = false;
            return;
        };
        let mut own: Vec<SymbolId> = Vec::new();
        for ns in [
            NamespaceId::Functions,
            NamespaceId::Values,
            NamespaceId::Events,
        ] {
            for (_, member) in scope.bindings[ns.index()].iter() {
                match sem.symbols.method_overloads(member) {
                    Some(set) => own.extend_from_slice(set),
                    None => own.push(member),
                }
            }
        }
        own.sort_unstable_by_key(|s| sem.symbols.get(*s).name_span.start);
        for member in own {
            let symbol = sem.symbols.get(member);
            if member_namespace(symbol.kind).is_none() {
                continue;
            }
            let access = access_of(symbol.flags);
            let is_static = symbol.flags.contains(SymbolFlags::STATIC);
            if !self.reaches(access, is_static, true) {
                continue;
            }
            let ty = match &symbol.data_type {
                None => MemberType::Untyped,
                Some(ResolvedType::Class(c)) => match sem.symbols.builtin_class(*c) {
                    Some(b) => MemberType::Named(IndexName::new(b.name)),
                    None => MemberType::Named(IndexName::new(&sem.symbols.get(*c).name)),
                },
                Some(t) => {
                    PortableType::new(t.clone()).map_or(MemberType::Untyped, MemberType::Portable)
                }
            };
            let params = (symbol.kind == SymbolKind::Function)
                .then(|| {
                    sem.symbols
                        .method_param_decls(member)
                        .map(|p| workspace_params(p))
                })
                .flatten();
            self.push(SurfaceMember {
                name: symbol.name.to_string(),
                kind: symbol.kind,
                access,
                is_static,
                ty,
                params,
                declared_in: ClassOrigin::Local(sym),
                origin: MemberOrigin::Local(member),
            });
        }
    }
}

fn access_of(flags: SymbolFlags) -> AccessModifier {
    if flags.contains(SymbolFlags::PRIVATE) {
        AccessModifier::Private
    } else if flags.contains(SymbolFlags::PACKAGE_PRIVATE) && flags.contains(SymbolFlags::PROTECTED)
    {
        AccessModifier::PackageProtected
    } else if flags.contains(SymbolFlags::PACKAGE_PRIVATE) {
        AccessModifier::PackagePrivate
    } else if flags.contains(SymbolFlags::PROTECTED) {
        AccessModifier::Protected
    } else {
        AccessModifier::Public
    }
}

/// Whether two parameter lists are surely the same, by shape and type as
/// written.
fn same_params(a: Option<&[SurfaceParam]>, b: Option<&[SurfaceParam]>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => {
            a.len() == b.len()
                && a.iter().zip(b).all(|(x, y)| match (&x.shape, &y.shape) {
                    (
                        MemberParamShape::Value(MemberType::Named(n)),
                        MemberParamShape::Value(MemberType::Named(m)),
                    ) => {
                        n.as_atom() == m.as_atom()
                            || n.as_atom().rsplit('.').next() == m.as_atom().rsplit('.').next()
                    }
                    (x, y) => x == y,
                })
        }
        _ => false,
    }
}

impl BuiltinMemberRef {
    /// The documentation of the member: its title and the page of the type
    /// that declares it.
    pub fn doc(&self) -> MemberDoc {
        match self {
            BuiltinMemberRef::Member(m) => MemberDoc {
                title: format!("{}:{}", m.class().name, m.name()),
                url: m.class().docs_url(),
                summary: None,
            },
            BuiltinMemberRef::EnumMember(class, name) => MemberDoc {
                title: format!("{}:{name}", class.name),
                url: class.docs_url(),
                summary: None,
            },
        }
    }
}

/// The documentation of a built-in member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberDoc {
    pub title: String,
    pub url: &'static str,
    /// One line from the documentation, when the catalog has it (not yet).
    pub summary: Option<&'static str>,
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use oxabl_common::FileId;
    use oxabl_schema::Schema;

    use super::*;
    use crate::{
        AnalysisContext, ClassDescriptor, IndexRevision, NullIndex, PrimitiveTy, analyze_file,
    };

    /// An index with one workspace class, `app.Err INHERITS
    /// Progress.Lang.AppError`, with a public method and a private one.
    struct OneClass;

    impl WorkspaceIndex for OneClass {
        fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
            if name.as_atom().as_ref() != "app.err" {
                return IndexAnswer::NotFound;
            }
            IndexAnswer::Found(Arc::new(ClassDescriptor {
                name: IndexName::new("app.Err"),
                file: IndexedFileId::new(7),
                kind: ClassKind::Class,
                inherits: Some(IndexName::new("Progress.Lang.AppError")),
                implements: Vec::new(),
                imports: Arc::default(),
                members_complete: true,
                is_abstract: false,
            }))
        }
        fn class_members(&self, name: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
            if name.as_atom().as_ref() != "app.err" {
                return IndexAnswer::NotFound;
            }
            let member = |n: &str, access| MemberDescriptor {
                name: IndexName::new(n),
                kind: SymbolKind::Function,
                ty: MemberType::Portable(
                    PortableType::new(ResolvedType::Primitive(PrimitiveTy::Character)).unwrap(),
                ),
                access,
                is_static: false,
                record: None,
                params: Some(Arc::from([])),
            };
            IndexAnswer::Found(Arc::from([
                member("describe", AccessModifier::Public),
                member("secret", AccessModifier::Private),
            ]))
        }
        fn program(&self, _: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::NotFound
        }
        fn shared_producer(&self, _: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::NotFound
        }
        fn revision(&self) -> IndexRevision {
            IndexRevision::new(1)
        }
    }

    fn builtin(name: &str) -> ClassOrigin {
        ClassOrigin::Builtin(crate::builtin_class(name).unwrap())
    }

    /// The declaring type and name of each member, in order.
    fn rows(surface: &ClassSurface) -> Vec<(String, String)> {
        surface
            .members
            .iter()
            .map(|m| {
                let at = match &m.declared_in {
                    ClassOrigin::Builtin(c) => c.name.to_string(),
                    ClassOrigin::Workspace(n) => n.as_atom().to_string(),
                    ClassOrigin::Local(_) => "local".to_string(),
                };
                (at, m.name.clone())
            })
            .collect()
    }

    #[test]
    fn the_surface_of_app_error_is_its_chain_nearest_first() {
        let s = surface(
            &builtin("Progress.Lang.AppError"),
            &NullIndex,
            &crate::BUILTIN_CATALOG,
            SurfaceRole::Instance,
        );
        assert!(s.complete);
        let rows = rows(&s);
        let at = |class: &str, name: &str| {
            rows.iter()
                .position(|(c, n)| c == class && n == name)
                .unwrap_or_else(|| panic!("{class}:{name} in {rows:?}"))
        };
        let own = at("Progress.Lang.AppError", "AddMessage");
        let pro = at("Progress.Lang.ProError", "GetMessage");
        let object = at("Progress.Lang.Object", "ToString");
        assert!(own < pro && pro < object);
        // `GetMessage` of the interface `Progress.Lang.Error` is the one of
        // `ProError`: the nearer level has the same parameter list.
        assert!(
            !rows
                .iter()
                .any(|(c, n)| c == "Progress.Lang.Error" && n == "GetMessage")
        );
        let get = &s.members[pro];
        assert_eq!(get.kind, SymbolKind::Function);
        assert_eq!(get.params.as_ref().unwrap()[0].mode, None);
        assert_eq!(
            get.params.as_ref().unwrap()[0].name.as_deref(),
            Some("MessageIndex")
        );
        let MemberOrigin::Builtin(r) = get.origin else {
            panic!("a built-in origin");
        };
        assert_eq!(
            r.doc().url,
            "https://docs-be.progress.com/bundle/abl-reference/page/Progress.Lang.ProError-class.html"
        );
    }

    #[test]
    fn a_type_name_reaches_only_static_members() {
        let s = surface(
            &builtin("Progress.ApplicationServer.AdapterTypes"),
            &NullIndex,
            &crate::BUILTIN_CATALOG,
            SurfaceRole::TypeName,
        );
        assert!(s.members.iter().all(|m| m.is_static));
        assert!(s.members.iter().any(|m| m.name == "SOAP"));
    }

    #[test]
    fn an_incomplete_catalog_type_is_not_complete() {
        for name in [
            "Progress.Lang.Class",
            "Progress.Collections.List",
            "Progress.IO.ByteOrder",
            "OpenEdge.Net.UriSchemeEnum",
        ] {
            let s = surface(
                &builtin(name),
                &NullIndex,
                &crate::BUILTIN_CATALOG,
                SurfaceRole::Instance,
            );
            assert!(!s.complete, "{name}");
        }
    }

    #[test]
    fn a_workspace_class_reads_its_level_then_the_built_in_chain() {
        let origin = ClassOrigin::Workspace(IndexName::new("app.Err"));
        let s = surface(
            &origin,
            &OneClass,
            &crate::BUILTIN_CATALOG,
            SurfaceRole::Instance,
        );
        assert!(s.complete);
        let rows = rows(&s);
        assert_eq!(rows[0], ("app.err".to_string(), "describe".to_string()));
        assert!(!rows.iter().any(|(_, n)| n == "secret"));
        assert!(rows.iter().any(|(c, _)| c == "Progress.Lang.ProError"));
        let supers = supertypes(&origin, &OneClass, &crate::BUILTIN_CATALOG);
        assert_eq!(
            supers.parent,
            Some(SupertypeOrigin::Class(builtin("Progress.Lang.AppError")))
        );
        assert!(supers.header_known);
        // A class no file supplies is not complete.
        let missing = ClassOrigin::Workspace(IndexName::new("app.Missing"));
        assert!(
            !surface(
                &missing,
                &OneClass,
                &crate::BUILTIN_CATALOG,
                SurfaceRole::Instance
            )
            .complete
        );
    }

    #[test]
    fn a_local_class_that_inherits_a_built_in_class() {
        let source = "CLASS app.Local INHERITS Progress.Lang.AppError:\n\
                      METHOD PUBLIC CHARACTER Own():\n\
                      END METHOD.\n\
                      METHOD PRIVATE CHARACTER Hidden():\n\
                      END METHOD.\n\
                      END CLASS.\n";
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
        let sem = analyze_file(&program.statements, &ctx);
        let sym = sem
            .symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Class && s.name.as_ref() == "app.local")
            .map(|(id, _)| id)
            .unwrap();
        let origin = sem.symbols.class_origin(sym).unwrap();
        assert_eq!(origin, ClassOrigin::Local(sym));
        let outside = local_surface(&sem, &origin, &NullIndex, SurfaceRole::Instance);
        assert!(outside.complete);
        let names: Vec<_> = outside.members.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names[0], "own");
        assert!(!names.contains(&"hidden"));
        assert!(names.contains(&"AddMessage"));
        let inside = local_surface(&sem, &origin, &NullIndex, SurfaceRole::ThisObject);
        assert!(inside.members.iter().any(|m| m.name == "hidden"));
        let sup = local_surface(&sem, &origin, &NullIndex, SurfaceRole::Super);
        assert!(!sup.members.iter().any(|m| m.name == "own"));
        assert!(sup.members.iter().any(|m| m.name == "AddMessage"));
    }
}
