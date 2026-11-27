//! The members a class offers a receiver: its own and its supertypes', as the
//! role of the receiver reaches them. Code completion after `obj:` reads it.
//!
//! The rules are the ones of `docs/design/class-members.md` (access, static
//! and completeness), and the walk of the supertypes is the one of the resolve
//! pass ([`lookup_supertype`](crate::resolve::lookup_supertype) and a visited
//! set), so completion offers what go to definition resolves. The resolve pass
//! itself is not changed by this module.

use std::sync::Arc;

use oxabl_ast::{AccessModifier, NodeId};
use rustc_hash::FxHashSet;

use crate::resolve::{SupertypeLookup, lookup_supertype_in};
use crate::{
    ClassDescriptor, ClassKind, ImportContext, IndexAnswer, IndexName, IndexedFileId,
    MemberDescriptor, MemberParam, MemberType, NamespaceId, ResolvedType, ScopeKind, Semantic,
    SymbolFlags, SymbolId, SymbolKind, SymbolTable, WorkspaceIndex,
};

/// Classes and their own members, by qualified name. The workspace index is
/// one source ([`WorkspaceClasses`]); a catalog of the built-in classes is
/// another. A chain asks the workspace first.
pub trait ClassSource {
    /// The class or interface `name`.
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>>;
    /// The members `name` declares itself, in declaration order.
    fn class_members(&self, name: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>>;
    /// The classes, interfaces and enums whose qualified name starts with
    /// `package` (for example `"app.models."`, or `""` for the root), for type
    /// completion. `complete` is false when the source cannot list them all.
    fn classes_in(&self, package: &str) -> ClassList;
}

/// A class, interface or enum that a [`ClassSource`] lists.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassListing {
    /// The qualified name as the source spells it.
    pub name: String,
    pub kind: ClassKind,
    pub is_abstract: bool,
}

/// The answer of [`ClassSource::classes_in`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ClassList {
    pub classes: Vec<ClassListing>,
    /// False when the source cannot list every class of the package.
    pub complete: bool,
}

/// A [`ClassSource`] over a [`WorkspaceIndex`]. The index cannot list classes
/// yet, so [`ClassSource::classes_in`] answers an empty, incomplete list.
pub struct WorkspaceClasses<'a>(pub &'a dyn WorkspaceIndex);

impl ClassSource for WorkspaceClasses<'_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        self.0.class(name)
    }

    fn class_members(&self, name: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        self.0.class_members(name)
    }

    fn classes_in(&self, _package: &str) -> ClassList {
        ClassList::default()
    }
}

/// The class whose members are wanted.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassRef {
    /// A class or interface declared in the analysed file.
    Local(SymbolId),
    /// A class by qualified name, from a class source.
    Named(IndexName),
}

impl ClassRef {
    /// The class that class symbol `sym` stands for: a class of this file, or
    /// a class the resolve pass found in the index. `None` for any other
    /// symbol.
    pub fn of(symbols: &SymbolTable, sym: SymbolId) -> Option<ClassRef> {
        if let Some(name) = symbols.indexed_class_name(sym) {
            return Some(ClassRef::Named(name.clone()));
        }
        let symbol = symbols.get(sym);
        (symbol.declaration != NodeId::DUMMY
            && matches!(symbol.kind, SymbolKind::Class | SymbolKind::Interface))
        .then_some(ClassRef::Local(sym))
    }
}

/// How a receiver reaches its class. The same four roles as
/// `class-members.md`, "Receivers".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberReach {
    /// A value of the class: instance members.
    Instance,
    /// The type name: static members.
    TypeName,
    /// `THIS-OBJECT` in the class body: instance members, the own private ones
    /// included.
    ThisObject,
    /// `SUPER` in the class body: the instance members the class inherits.
    Super,
}

/// One member name as a receiver reaches it.
#[derive(Debug, Clone)]
pub struct SurfaceMember {
    /// The name as the declaring file spells it. For a member of a class of
    /// the analysed file, the folded name: its spelling is in the analysed
    /// text (see [`MemberOrigin::Local`]).
    pub name: String,
    /// `Function` for a method, `Property`, `Variable` (a data member),
    /// `Event`, `TempTable`, `Buffer`, `Dataset` or `DataSource`.
    pub kind: SymbolKind,
    /// The access of the nearest declaration.
    pub access: AccessModifier,
    pub is_static: bool,
    /// The nearest class that declares it, qualified.
    pub declared_in: String,
    /// Whether that class is an enum, so a static property is an enum member.
    pub declared_in_enum: bool,
    /// One entry for each overload of a method, one entry for any other
    /// member.
    pub signatures: Vec<MemberSignature>,
    /// Where the documentation of `completionItem/resolve` comes from.
    pub origin: MemberOrigin,
}

/// The type and parameters of one declaration of a member.
#[derive(Debug, Clone)]
pub struct MemberSignature {
    pub ty: MemberType,
    /// The parameters of a method. `None` for any other member and for a
    /// method whose list is not known.
    pub params: Option<Arc<[MemberParam]>>,
    /// The class `ty` names, when the analysis already resolved it (a member
    /// of a class of the analysed file).
    pub class: Option<ClassRef>,
}

/// Where a member is declared.
#[derive(Debug, Clone)]
pub enum MemberOrigin {
    /// A class of the analysed file. The symbol is the anchor of the overload
    /// set of a method.
    Local(SymbolId),
    /// A class file of the workspace, and the imports its types are written
    /// under.
    Workspace {
        file: IndexedFileId,
        imports: Arc<ImportContext>,
    },
}

/// The members a receiver reaches, nearest class first, and whether the list
/// is whole.
#[derive(Debug, Clone, Default)]
pub struct ClassSurface {
    pub members: Vec<SurfaceMember>,
    /// False when a member can be missing: a supertype that was not read, a
    /// level whose member list is not whole, a cycle in the inheritance, and
    /// every class until a source answers for `Progress.Lang.Object`, whose
    /// members every class inherits.
    pub complete: bool,
}

/// One declaration on a level of the walk.
struct Declared {
    name: String,
    folded: String,
    kind: SymbolKind,
    access: AccessModifier,
    is_static: bool,
    signature: MemberSignature,
    origin: MemberOrigin,
}

/// One class of the walk and the members it declares itself.
struct Level {
    name: String,
    is_enum: bool,
    members: Vec<Declared>,
}

/// What the walk reads for a class, before it reads the members.
struct Header {
    name: String,
    kind: ClassKind,
    is_abstract: bool,
    imports: Arc<ImportContext>,
    inherits: Option<String>,
    implements: Vec<String>,
}

impl Header {
    fn of(descriptor: &ClassDescriptor) -> Header {
        Header {
            name: descriptor.name.as_written().to_string(),
            kind: descriptor.kind,
            is_abstract: descriptor.is_abstract,
            imports: Arc::clone(&descriptor.imports),
            inherits: descriptor
                .inherits
                .as_ref()
                .map(|n| n.as_written().to_string()),
            implements: descriptor
                .implements
                .iter()
                .map(|n| n.as_written().to_string())
                .collect(),
        }
    }
}

/// The members `reach` gives for `class`, own and inherited, nearest first.
///
/// `inside` says that the cursor is in the body of `class` (a class of the
/// analysed file): its private members are then reachable through an instance
/// or the type name, as in the compiler. `THIS-OBJECT` and `SUPER` are inside
/// by their nature.
pub fn class_surface(
    semantic: &Semantic,
    source: &dyn ClassSource,
    class: ClassRef,
    reach: MemberReach,
    inside: bool,
) -> ClassSurface {
    let inside = inside || matches!(reach, MemberReach::ThisObject | MemberReach::Super);
    let symbols = &semantic.symbols;
    let mut surface = ClassSurface {
        members: Vec::new(),
        complete: true,
    };
    let mut levels: Vec<Level> = Vec::new();
    let mut visited: FxHashSet<String> = FxHashSet::default();
    // The supertype names still to read, with the imports of the file that
    // wrote each and whether it is on the `INHERITS` chain.
    let mut queue: std::collections::VecDeque<(String, Arc<ImportContext>, bool)> =
        std::collections::VecDeque::new();
    let mut chain: FxHashSet<String> = FxHashSet::default();

    let start = match &class {
        ClassRef::Local(sym) => local_header(symbols, *sym).map(|header| (header, None)),
        ClassRef::Named(name) => match source.class(name) {
            IndexAnswer::Found(descriptor) => {
                Some((Header::of(&descriptor), Some(Arc::clone(&descriptor))))
            }
            _ => None,
        },
    };
    let Some((header, descriptor)) = start else {
        surface.complete = false;
        return surface;
    };
    // A supertype of an interface or an abstract class that was not read can
    // hold members a concrete class declares itself.
    let needs_interfaces = header.kind == ClassKind::Interface || header.is_abstract;
    visited.insert(header.name.to_ascii_lowercase());
    chain.insert(header.name.to_ascii_lowercase());
    if let Some(parent) = &header.inherits {
        queue.push_back((parent.clone(), Arc::clone(&header.imports), true));
    }
    for parent in &header.implements {
        queue.push_back((parent.clone(), Arc::clone(&header.imports), false));
    }
    if reach != MemberReach::Super {
        match (&class, &descriptor) {
            (ClassRef::Local(sym), _) => {
                levels.push(local_level(symbols, &semantic.scope_tree, *sym));
            }
            (ClassRef::Named(name), Some(descriptor)) => {
                match named_level(source, name, descriptor, &header) {
                    Some((level, whole)) => {
                        surface.complete &= whole;
                        levels.push(level);
                    }
                    None => surface.complete = false,
                }
            }
            (ClassRef::Named(_), None) => surface.complete = false,
        }
    }

    while let Some((raw, imports, superclass)) = queue.pop_front() {
        let unread = |surface: &mut ClassSurface| {
            if superclass || needs_interfaces {
                surface.complete = false;
            }
        };
        let name = match lookup_supertype_in(source, &imports, &raw) {
            SupertypeLookup::Class(name) => name,
            SupertypeLookup::Assembly => {
                unread(&mut surface);
                continue;
            }
            SupertypeLookup::Missing { root_object, .. } => {
                if !root_object {
                    unread(&mut surface);
                }
                continue;
            }
        };
        let folded = name.as_str().to_string();
        if superclass && !chain.insert(folded.clone()) {
            // A cycle does not compile: every member is in doubt.
            surface.complete = false;
        }
        if !visited.insert(folded) {
            continue;
        }
        let IndexAnswer::Found(descriptor) = source.class(&name) else {
            unread(&mut surface);
            continue;
        };
        let parent = Header::of(&descriptor);
        if let Some(next) = &parent.inherits {
            queue.push_back((next.clone(), Arc::clone(&parent.imports), superclass));
        }
        for next in &parent.implements {
            queue.push_back((next.clone(), Arc::clone(&parent.imports), false));
        }
        match named_level(source, &name, &descriptor, &parent) {
            Some((level, whole)) => {
                surface.complete &= whole;
                levels.push(level);
            }
            None => unread(&mut surface),
        }
    }

    // Every class inherits the members of `Progress.Lang.Object`. Until a
    // source answers for it, no surface is whole.
    if !matches!(
        source.class(&IndexName::new("Progress.Lang.Object")),
        IndexAnswer::Found(_)
    ) {
        surface.complete = false;
    }

    surface.members = reachable(levels, reach, inside);
    surface
}

/// The class `member` evaluates to when it is read: a property or data
/// member, or a method called. `None` when its type is not a class, when the
/// declarations of a method do not all name the same class, and when the class
/// is not one the workspace supplies.
pub fn member_class(
    source: &dyn ClassSource,
    member: &SurfaceMember,
    call: bool,
) -> Option<ClassRef> {
    let wanted = if call {
        member.kind == SymbolKind::Function
    } else {
        matches!(member.kind, SymbolKind::Property | SymbolKind::Variable)
    };
    if !wanted {
        return None;
    }
    let mut found: Option<ClassRef> = None;
    for signature in &member.signatures {
        let class = match (&signature.class, &signature.ty, &member.origin) {
            (Some(class), _, _) => class.clone(),
            (None, MemberType::Named(raw), MemberOrigin::Workspace { imports, .. }) => {
                resolve_named(source, imports, raw.as_written())?
            }
            _ => return None,
        };
        match &found {
            Some(first) if *first != class => return None,
            _ => found = Some(class),
        }
    }
    found
}

/// The class the name `raw`, written in a file with `imports`, means: the
/// first candidate the source knows, in the order the AVM tries them.
pub fn resolve_named(
    source: &dyn ClassSource,
    imports: &ImportContext,
    raw: &str,
) -> Option<ClassRef> {
    match lookup_supertype_in(source, imports, raw) {
        SupertypeLookup::Class(name) => Some(ClassRef::Named(name)),
        SupertypeLookup::Assembly | SupertypeLookup::Missing { .. } => None,
    }
}

fn local_header(symbols: &SymbolTable, sym: SymbolId) -> Option<Header> {
    let symbol = symbols.get(sym);
    if symbol.declaration == NodeId::DUMMY {
        return None;
    }
    let supers = symbols.supertypes(sym);
    let written = |s: &crate::SupertypeRef| s.name.as_written().to_string();
    Some(Header {
        name: symbol.name.to_string(),
        kind: if symbol.kind == SymbolKind::Interface {
            ClassKind::Interface
        } else {
            ClassKind::Class
        },
        is_abstract: symbol.flags.contains(SymbolFlags::ABSTRACT),
        imports: symbols.imports().cloned().unwrap_or_default(),
        inherits: supers.and_then(|s| s.inherits.as_ref().map(written)),
        implements: supers
            .map(|s| s.implements.iter().map(written).collect())
            .unwrap_or_default(),
    })
}

/// The members a class of the analysed file declares itself, from its body
/// scope.
fn local_level(symbols: &SymbolTable, tree: &crate::ScopeTree, class: SymbolId) -> Level {
    let symbol = symbols.get(class);
    let kind = if symbol.kind == SymbolKind::Interface {
        ScopeKind::Interface
    } else {
        ScopeKind::Class
    };
    let mut level = Level {
        name: symbol.name.to_string(),
        is_enum: false,
        members: Vec::new(),
    };
    let Some(scope) = tree.child(symbol.declared_in, symbol.declaration, kind) else {
        return level;
    };
    let held = tree.get(scope);
    let mut bound: Vec<SymbolId> = Vec::new();
    for ns in [
        NamespaceId::Values,
        NamespaceId::Functions,
        NamespaceId::Events,
        NamespaceId::Buffers,
    ] {
        if let Some(map) = held.bindings.get(ns) {
            bound.extend(map.iter().map(|(_, sym)| sym));
        }
    }
    bound.sort_unstable_by_key(|sym| sym.raw());
    for anchor in bound {
        let anchor_symbol = symbols.get(anchor);
        if anchor_symbol.declaration == NodeId::DUMMY {
            continue;
        }
        match anchor_symbol.kind {
            SymbolKind::Function if symbols.is_method(anchor) => {
                let overloads = symbols
                    .method_overloads(anchor)
                    .map_or_else(|| vec![anchor], <[SymbolId]>::to_vec);
                for overload in overloads {
                    level
                        .members
                        .push(local_declared(symbols, overload, anchor));
                }
            }
            SymbolKind::Property
            | SymbolKind::Variable
            | SymbolKind::Event
            | SymbolKind::TempTable
            | SymbolKind::Buffer
            | SymbolKind::Dataset
            | SymbolKind::DataSource => level.members.push(local_declared(symbols, anchor, anchor)),
            _ => {}
        }
    }
    level
}

fn local_declared(symbols: &SymbolTable, sym: SymbolId, anchor: SymbolId) -> Declared {
    let symbol = symbols.get(sym);
    let flags = symbol.flags;
    let access = if flags.contains(SymbolFlags::PRIVATE) {
        AccessModifier::Private
    } else if flags.contains(SymbolFlags::PROTECTED) {
        if flags.contains(SymbolFlags::PACKAGE_PRIVATE) {
            AccessModifier::PackageProtected
        } else {
            AccessModifier::Protected
        }
    } else if flags.contains(SymbolFlags::PACKAGE_PRIVATE) {
        AccessModifier::PackagePrivate
    } else if flags.contains(SymbolFlags::PUBLIC) {
        AccessModifier::Public
    } else {
        // A data member that writes no access is private, as in ABL.
        symbols
            .member_access(sym)
            .unwrap_or(AccessModifier::Private)
    };
    let (ty, class) = match &symbol.data_type {
        None => (MemberType::Untyped, None),
        Some(ResolvedType::Class(class_sym)) => {
            let class = ClassRef::of(symbols, *class_sym);
            let name = match &class {
                Some(ClassRef::Named(name)) => name.clone(),
                _ => IndexName::new(&symbols.get(*class_sym).name),
            };
            (MemberType::Named(name), class)
        }
        Some(ty) => (
            MemberType::portable(ty.clone())
                .or_else(|| MemberType::portable(ResolvedType::Unknown))
                .unwrap_or(MemberType::Untyped),
            None,
        ),
    };
    let folded = symbol.name.to_string();
    Declared {
        name: folded.clone(),
        folded,
        kind: symbol.kind,
        access,
        is_static: flags.contains(SymbolFlags::STATIC),
        signature: MemberSignature {
            ty,
            params: symbols.method_param_decls(sym).cloned(),
            class,
        },
        origin: MemberOrigin::Local(anchor),
    }
}

/// The members the class `name` declares itself, and whether the list is
/// whole. `None` when the source cannot list them.
fn named_level(
    source: &dyn ClassSource,
    name: &IndexName,
    descriptor: &ClassDescriptor,
    header: &Header,
) -> Option<(Level, bool)> {
    let IndexAnswer::Found(members) = source.class_members(name) else {
        return None;
    };
    let level = Level {
        name: header.name.clone(),
        is_enum: header.kind == ClassKind::Enum,
        members: members
            .iter()
            .map(|member| Declared {
                name: member.name.as_written().to_string(),
                folded: member.name.as_str().to_string(),
                kind: member.kind,
                access: member.access,
                is_static: member.is_static,
                signature: MemberSignature {
                    ty: member.ty.clone(),
                    params: member.params.clone(),
                    class: None,
                },
                origin: MemberOrigin::Workspace {
                    file: descriptor.file,
                    imports: Arc::clone(&descriptor.imports),
                },
            })
            .collect(),
    };
    Some((level, descriptor.members_complete))
}

/// Whether a member declared with `access` on level `depth` is reachable.
fn is_reachable(access: AccessModifier, depth: usize, inside: bool) -> bool {
    match (inside, depth) {
        // The own members of the class the cursor is in.
        (true, 0) => true,
        // What a subclass inherits.
        (true, _) => matches!(
            access,
            AccessModifier::Public | AccessModifier::Protected | AccessModifier::PackageProtected
        ),
        (false, _) => access == AccessModifier::Public,
    }
}

/// The members of `levels` that `reach` reaches, one entry for each name (a
/// method gathers its overloads from every level).
fn reachable(levels: Vec<Level>, reach: MemberReach, inside: bool) -> Vec<SurfaceMember> {
    let mut out: Vec<SurfaceMember> = Vec::new();
    // Where each folded name is in `out`; a method and a value of one name are
    // two entries.
    let mut at: rustc_hash::FxHashMap<(bool, String), usize> = rustc_hash::FxHashMap::default();
    // `Super` skips the own level, so the first level it holds is a parent.
    let first_depth = usize::from(reach == MemberReach::Super);
    for (depth, level) in levels.into_iter().enumerate() {
        let depth = depth + first_depth;
        for declared in level.members {
            if declared.is_static != (reach == MemberReach::TypeName)
                || !is_reachable(declared.access, depth, inside)
            {
                continue;
            }
            let is_method = declared.kind == SymbolKind::Function;
            match at.get(&(is_method, declared.folded.clone())) {
                Some(&found) if is_method => {
                    let member = &mut out[found];
                    // A declaration with the parameter list of a nearer one is
                    // the method it overrides or implements.
                    let same = declared.signature.params.is_some()
                        && member
                            .signatures
                            .iter()
                            .any(|s| s.params == declared.signature.params);
                    if !same {
                        member.signatures.push(declared.signature);
                    }
                }
                // The nearest declaration of anything else wins.
                Some(_) => {}
                None => {
                    at.insert((is_method, declared.folded), out.len());
                    out.push(SurfaceMember {
                        name: declared.name,
                        kind: declared.kind,
                        access: declared.access,
                        is_static: declared.is_static,
                        declared_in: level.name.clone(),
                        declared_in_enum: level.is_enum,
                        signatures: vec![declared.signature],
                        origin: declared.origin,
                    });
                }
            }
        }
    }
    out
}
