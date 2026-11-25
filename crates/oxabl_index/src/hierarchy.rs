//! The inheritance graph of a workspace: the supertypes a class header names,
//! the reverse index of its subtypes, and the methods that override or
//! implement another.
//!
//! The forward facts are [`ClassDescriptor`] (`INHERITS`, `IMPLEMENTS` and the
//! imports of the declaring file). This module resolves each supertype name
//! the way the resolve pass does ([`ImportContext::resolve_class`]) and keeps
//! the answer, **including what did not resolve**: a supertype that oxabl
//! cannot read is a [`Supertype::Unresolved`], never a missing entry. A
//! consumer that lists supertypes or subtypes shows those entries, so a list
//! never looks whole when it is not.
//!
//! [`SubtypeIndex`] is built from the facts of the class files of a workspace
//! (`ClassFile`). It does not read files or know a database: the caller gives
//! the facts, which an incremental cache has memoized per file.
//!
//! Built-in Progress types have no source file. [`builtin_type`] is the one
//! place that says what oxabl knows about them, so a catalog of built-in
//! classes changes this function and nothing else.

use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::Arc;

use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassDescriptor, ClassKind, ClassResolution, IndexAnswer, IndexName, IndexRevision,
    IndexedFileId, LocateKind, Located, MemberDescriptor, MemberParam, MemberParamShape,
    MemberType, Signature, SymbolKind, WorkspaceIndex,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::FileFacts;

/// How a class header names a supertype.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Relation {
    /// `INHERITS`: the parent class, or an interface that extends another.
    Inherits,
    /// `IMPLEMENTS`: an interface of a class.
    Implements,
}

impl Relation {
    /// The keyword, as the header writes it.
    pub fn keyword(self) -> &'static str {
        match self {
            Relation::Inherits => "INHERITS",
            Relation::Implements => "IMPLEMENTS",
        }
    }
}

/// Why a supertype name did not resolve to a class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Unresolved {
    /// No file of the configured paths declares it.
    NotFound,
    /// No search path is configured, so nothing was looked at.
    NotSearched,
    /// A file that would declare it was found and oxabl cannot read it.
    Unusable,
    /// Two files can supply the name, and oxabl does not choose.
    Unknowable,
    /// The file may import more than oxabl read (an `{include}` before the
    /// header), so the name may mean a class that was not tried.
    UnseenImports,
}

impl Unresolved {
    /// The reason, in a sentence fragment.
    pub fn describe(self) -> &'static str {
        match self {
            Unresolved::NotFound => "no file on the include path declares it",
            Unresolved::NotSearched => "no include path is configured, so no file was searched",
            Unresolved::Unusable => "its file cannot be read",
            Unresolved::Unknowable => "more than one file can supply it",
            Unresolved::UnseenImports => {
                "the file may import more than oxabl read, so the name may mean another class"
            }
        }
    }
}

/// What oxabl knows about a built-in Progress type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinType {
    /// The documented qualified name.
    pub name: &'static str,
    pub kind: ClassKind,
    /// The supertypes the documentation gives it.
    pub supertypes: Vec<(Relation, &'static str)>,
}

/// The built-in type named `name`, or `None` when oxabl does not know it.
///
/// This is the seam for the catalog of built-in classes: a type that a catalog
/// knows is shown by name and kind with no file, and its own supertypes come
/// from here.
pub fn builtin_type(name: &str) -> Option<BuiltinType> {
    let class = oxabl_semantic::builtin_class(name)?;
    let kind = class.kind;
    let mut supertypes = Vec::new();
    if let Some(parent) = class.inherits {
        supertypes.push((Relation::Inherits, parent));
    }
    let implements = if kind == ClassKind::Interface {
        Relation::Inherits
    } else {
        Relation::Implements
    };
    supertypes.extend(class.implements.iter().map(|name| (implements, *name)));
    Some(BuiltinType {
        name: class.name,
        kind,
        supertypes,
    })
}

/// A supertype name of a class header, resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Supertype {
    /// A class or interface of the workspace.
    Class {
        name: IndexName,
        file: IndexedFileId,
        kind: ClassKind,
    },
    /// A built-in Progress type: it has a name and a kind and no file.
    Builtin { name: &'static str, kind: ClassKind },
    /// A type of a .NET assembly, which oxabl does not read.
    DotNet,
    /// A name that did not resolve.
    Unresolved(Unresolved),
}

/// One supertype of a class header, as written and as resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SupertypeRef {
    /// The name as the header writes it.
    pub written: IndexName,
    pub relation: Relation,
    pub resolved: Supertype,
}

/// The supertypes `class` names, in the order of its header: the parent, then
/// the interfaces. Each is resolved through the imports of the file that
/// declares `class`. An interface's `INHERITS` list is its supertypes, all
/// with [`Relation::Inherits`].
pub fn supertypes_of(class: &ClassDescriptor, index: &dyn WorkspaceIndex) -> Vec<SupertypeRef> {
    let interface = class.kind == ClassKind::Interface;
    let named = class
        .inherits
        .iter()
        .map(|name| (name, Relation::Inherits))
        .chain(class.implements.iter().map(|name| {
            (
                name,
                if interface {
                    Relation::Inherits
                } else {
                    Relation::Implements
                },
            )
        }));
    named
        .map(|(written, relation)| SupertypeRef {
            resolved: resolve_supertype(class, written, index),
            written: written.clone(),
            relation,
        })
        .collect()
}

fn resolve_supertype(
    class: &ClassDescriptor,
    written: &IndexName,
    index: &dyn WorkspaceIndex,
) -> Supertype {
    match class.imports.resolve_class(written.as_written(), index) {
        ClassResolution::Class(found) => Supertype::Class {
            name: found.name.clone(),
            file: found.file,
            kind: found.kind,
        },
        ClassResolution::Builtin(known) => Supertype::Builtin {
            name: known.name,
            kind: known.kind,
        },
        ClassResolution::Assembly => Supertype::DotNet,
        ClassResolution::Unusable => Supertype::Unresolved(Unresolved::Unusable),
        ClassResolution::Unknowable => Supertype::Unresolved(Unresolved::Unknowable),
        ClassResolution::NotFound if !index.searches_any_path() => {
            Supertype::Unresolved(Unresolved::NotSearched)
        }
        ClassResolution::NotFound if class.imports.has_unseen_imports() => {
            Supertype::Unresolved(Unresolved::UnseenImports)
        }
        ClassResolution::NotFound => Supertype::Unresolved(Unresolved::NotFound),
    }
}

/// The folded last segment of a qualified name: `Cache` for `app.Services.Cache`.
pub fn simple_name(name: &str) -> OxablAtom {
    let simple = name.rsplit('.').next().unwrap_or(name);
    IndexName::new(simple).as_atom().clone()
}

/// A class file of the workspace, with the facts read from it.
#[derive(Debug, Clone)]
pub struct ClassFile {
    pub path: PathBuf,
    pub facts: Arc<FileFacts>,
}

/// One class, interface or enum of the workspace, with its supertypes
/// resolved.
#[derive(Debug, Clone)]
pub struct ClassNode {
    pub name: IndexName,
    pub file: IndexedFileId,
    pub path: PathBuf,
    pub kind: ClassKind,
    /// Whether the member list is whole; see
    /// [`ClassDescriptor::members_complete`].
    pub members_complete: bool,
    /// The members the class declares itself.
    pub members: Arc<[MemberDescriptor]>,
    /// The supertypes the header names, resolved. Never leaves one out.
    pub supertypes: Vec<SupertypeRef>,
}

/// What a supertype is, as a key of the reverse index.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SuperKey {
    /// A class of the workspace, by its name and the file that declares it.
    Class(IndexName, IndexedFileId),
    /// A built-in type, by its folded name.
    Builtin(IndexName),
}

/// A supertype name that did not resolve, and the class that names it.
#[derive(Debug, Clone, Copy)]
pub struct UnresolvedHeader<'a> {
    pub class: &'a ClassNode,
    pub written: &'a IndexName,
    pub why: Unresolved,
}

/// The subtypes of every class of a workspace: the reverse of the supertypes
/// each header names.
#[derive(Debug, Default)]
pub struct SubtypeIndex {
    nodes: Vec<ClassNode>,
    direct: FxHashMap<SuperKey, Vec<(usize, Relation)>>,
    /// Supertype names that did not resolve, by the folded last segment of the
    /// name as written: each can be a supertype of any class of that name.
    unresolved: FxHashMap<OxablAtom, Vec<(usize, usize)>>,
    /// Class files that could not be read: each can hold a subtype.
    unreadable: Vec<PathBuf>,
}

impl SubtypeIndex {
    /// Build the index from the class files of a workspace. Each supertype
    /// name is resolved through `index`, which answers from the same
    /// workspace; each distinct class name is looked up once.
    pub fn build(files: impl IntoIterator<Item = ClassFile>, index: &dyn WorkspaceIndex) -> Self {
        let index = LookupMemo::new(index);
        let mut built = SubtypeIndex::default();
        for file in files {
            if !file.facts.parsed {
                built.unreadable.push(file.path);
                continue;
            }
            for class in &file.facts.classes {
                let descriptor = &class.descriptor;
                built.add(ClassNode {
                    name: descriptor.name.clone(),
                    file: file.facts.file,
                    path: file.path.clone(),
                    kind: descriptor.kind,
                    members_complete: descriptor.members_complete,
                    members: Arc::clone(&class.members),
                    supertypes: supertypes_of(descriptor, &index),
                });
            }
        }
        built.unreadable.sort();
        built
    }

    fn add(&mut self, node: ClassNode) {
        let at = self.nodes.len();
        for (position, supertype) in node.supertypes.iter().enumerate() {
            match &supertype.resolved {
                Supertype::Class { name, file, .. } => self
                    .direct
                    .entry(SuperKey::Class(name.clone(), *file))
                    .or_default()
                    .push((at, supertype.relation)),
                Supertype::Builtin { name, .. } => self
                    .direct
                    .entry(SuperKey::Builtin(IndexName::new(name)))
                    .or_default()
                    .push((at, supertype.relation)),
                Supertype::DotNet => {}
                Supertype::Unresolved(_) => self
                    .unresolved
                    .entry(simple_name(supertype.written.as_str()))
                    .or_default()
                    .push((at, position)),
            }
        }
        self.nodes.push(node);
    }

    /// Every class of the workspace the index read.
    pub fn nodes(&self) -> &[ClassNode] {
        &self.nodes
    }

    /// The class `name` that `file` declares.
    pub fn class(&self, name: &IndexName, file: IndexedFileId) -> Option<&ClassNode> {
        self.nodes
            .iter()
            .find(|node| node.file == file && node.name == *name)
    }

    /// The classes whose header names `target` as a supertype, each with the
    /// relation, in the order the files were given.
    pub fn direct_subtypes(&self, target: &SuperKey) -> Vec<(&ClassNode, Relation)> {
        self.direct
            .get(target)
            .map(|found| {
                found
                    .iter()
                    .map(|(at, relation)| (&self.nodes[*at], *relation))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The classes below `target` through any number of headers, nearest
    /// first, each once. A cycle ends the walk.
    pub fn all_subtypes(&self, target: &SuperKey) -> Vec<&ClassNode> {
        let mut seen: FxHashSet<usize> = FxHashSet::default();
        let mut found: Vec<&ClassNode> = Vec::new();
        let mut frontier = vec![target.clone()];
        while !frontier.is_empty() {
            let mut next = Vec::new();
            for key in &frontier {
                for (at, _) in self.direct.get(key).into_iter().flatten() {
                    if seen.insert(*at) {
                        let node = &self.nodes[*at];
                        found.push(node);
                        next.push(SuperKey::Class(node.name.clone(), node.file));
                    }
                }
            }
            frontier = next;
        }
        found
    }

    /// The headers that name a supertype that did not resolve and spell it
    /// with the last segment `simple` (folded): any of them can name the class
    /// of that name.
    pub fn unresolved_named(&self, simple: &OxablAtom) -> Vec<UnresolvedHeader<'_>> {
        self.unresolved
            .get(simple)
            .into_iter()
            .flatten()
            .filter_map(|(at, position)| {
                let class = &self.nodes[*at];
                let supertype = class.supertypes.get(*position)?;
                let Supertype::Unresolved(why) = supertype.resolved else {
                    return None;
                };
                Some(UnresolvedHeader {
                    class,
                    written: &supertype.written,
                    why,
                })
            })
            .collect()
    }

    /// Every header that names a supertype that did not resolve.
    pub fn unresolved_headers(&self) -> Vec<UnresolvedHeader<'_>> {
        let mut headers: Vec<UnresolvedHeader<'_>> = self
            .unresolved
            .values()
            .flatten()
            .filter_map(|(at, position)| {
                let class = &self.nodes[*at];
                let supertype = class.supertypes.get(*position)?;
                let Supertype::Unresolved(why) = supertype.resolved else {
                    return None;
                };
                Some(UnresolvedHeader {
                    class,
                    written: &supertype.written,
                    why,
                })
            })
            .collect();
        headers.sort_by(|a, b| {
            (&a.class.path, a.written.as_str()).cmp(&(&b.class.path, b.written.as_str()))
        });
        headers
    }

    /// The class files that could not be read, sorted.
    pub fn unreadable(&self) -> &[PathBuf] {
        &self.unreadable
    }
}

/// An index that remembers each class lookup, for the many headers that name
/// the same base class.
struct LookupMemo<'a> {
    inner: &'a dyn WorkspaceIndex,
    classes: RefCell<FxHashMap<OxablAtom, IndexAnswer<Arc<ClassDescriptor>>>>,
}

impl<'a> LookupMemo<'a> {
    fn new(inner: &'a dyn WorkspaceIndex) -> Self {
        LookupMemo {
            inner,
            classes: RefCell::default(),
        }
    }
}

impl WorkspaceIndex for LookupMemo<'_> {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        // The spelling, not the folded name: a path derives from it.
        let key = name.as_written_atom().clone();
        if let Some(known) = self.classes.borrow().get(&key) {
            return known.clone();
        }
        let answer = self.inner.class(name);
        self.classes.borrow_mut().insert(key, answer.clone());
        answer
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        self.inner.class_members(class)
    }

    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId> {
        self.inner.program(target)
    }

    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId> {
        self.inner.shared_producer(name)
    }

    fn located(&self, kind: LocateKind, name: &IndexName) -> Located {
        self.inner.located(kind, name)
    }

    fn signature(
        &self,
        program: &IndexName,
        procedure: Option<&IndexName>,
    ) -> Option<Arc<Signature>> {
        self.inner.signature(program, procedure)
    }

    fn revision(&self) -> IndexRevision {
        self.inner.revision()
    }

    fn searches_any_path(&self) -> bool {
        self.inner.searches_any_path()
    }
}

// ---------------------------------------------------------------------------
// Methods that override or implement another
// ---------------------------------------------------------------------------

/// Whether two method declarations have one parameter list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SameSignature {
    Yes,
    No,
    /// oxabl cannot tell: a list it did not read, a table or buffer
    /// parameter, a class written with another spelling.
    Unknown,
}

/// The methods of `members` named `name`, in declaration order. These are the
/// overloads of the name.
pub fn methods_named<'m>(
    members: &'m [MemberDescriptor],
    name: &IndexName,
) -> Vec<&'m MemberDescriptor> {
    members
        .iter()
        .filter(|member| member.kind == SymbolKind::Function && member.name == *name)
        .collect()
}

/// Whether `a` and `b`, two methods of one name, take the same parameters in
/// the same modes: what makes one an override or an implementation of the
/// other. Different lists are overloads.
pub fn same_signature(a: &MemberDescriptor, b: &MemberDescriptor) -> SameSignature {
    let (Some(a), Some(b)) = (&a.params, &b.params) else {
        return SameSignature::Unknown;
    };
    if a.len() != b.len() {
        return SameSignature::No;
    }
    let mut unknown = false;
    for (a, b) in a.iter().zip(b.iter()) {
        match same_param(a, b) {
            SameSignature::No => return SameSignature::No,
            SameSignature::Unknown => unknown = true,
            SameSignature::Yes => {}
        }
    }
    if unknown {
        SameSignature::Unknown
    } else {
        SameSignature::Yes
    }
}

fn same_param(a: &MemberParam, b: &MemberParam) -> SameSignature {
    if a.mode != b.mode {
        return SameSignature::No;
    }
    match (&a.shape, &b.shape) {
        (MemberParamShape::Value(a), MemberParamShape::Value(b)) => same_type(a, b),
        (MemberParamShape::Buffer, MemberParamShape::Buffer)
        | (MemberParamShape::Table, MemberParamShape::Table)
        | (MemberParamShape::Dataset, MemberParamShape::Dataset) => SameSignature::Unknown,
        (MemberParamShape::TableHandle, MemberParamShape::TableHandle)
        | (MemberParamShape::DatasetHandle, MemberParamShape::DatasetHandle) => SameSignature::Yes,
        _ => SameSignature::No,
    }
}

fn same_type(a: &MemberType, b: &MemberType) -> SameSignature {
    match (a, b) {
        // `LIKE` and a type an `&IF` chooses give `Unknown`: not comparable.
        (MemberType::Portable(a), MemberType::Portable(b)) => {
            if a.as_resolved().is_unknown() || b.as_resolved().is_unknown() {
                SameSignature::Unknown
            } else if a == b {
                SameSignature::Yes
            } else {
                SameSignature::No
            }
        }
        (MemberType::Named(a), MemberType::Named(b)) => {
            if a == b {
                SameSignature::Yes
            } else if simple_name(a.as_str()) == simple_name(b.as_str()) {
                // `Person` and `app.Person` can be one class.
                SameSignature::Unknown
            } else {
                SameSignature::No
            }
        }
        (MemberType::Untyped, MemberType::Untyped) => SameSignature::Unknown,
        _ => SameSignature::No,
    }
}

/// Whether a subclass or an implementing class can override `member`:
/// not `PRIVATE`, not `STATIC`.
pub fn overridable(member: &MemberDescriptor) -> bool {
    member.kind == SymbolKind::Function
        && !member.is_static
        && member.access != oxabl_ast::AccessModifier::Private
}

/// The overloads in `declared` that `wanted` can mean: their positions, and
/// whether every one of them is certain.
fn matching(declared: &[&MemberDescriptor], wanted: &[&MemberDescriptor]) -> (Vec<usize>, bool) {
    let mut ordinals = Vec::new();
    let mut certain = true;
    for (ordinal, method) in declared.iter().enumerate() {
        if !overridable(method) {
            continue;
        }
        let mut yes = false;
        let mut unknown = false;
        for wanted in wanted {
            match same_signature(wanted, method) {
                SameSignature::Yes => yes = true,
                SameSignature::Unknown => unknown = true,
                SameSignature::No => {}
            }
        }
        if yes || unknown {
            ordinals.push(ordinal);
            certain &= yes;
        }
    }
    (ordinals, certain)
}

/// One class that declares a method of the same name and parameters as a
/// wanted one.
#[derive(Debug, Clone)]
pub struct MethodMatch<'a> {
    pub class: &'a ClassNode,
    /// The position of each matching declaration among the methods of this
    /// name in the class, in declaration order.
    pub ordinals: Vec<usize>,
    /// How many methods of this name the class declares.
    pub total: usize,
    /// False when a match rests on a parameter list oxabl could not compare.
    pub certain: bool,
}

/// The classes below `target` that declare a method like one of `wanted`
/// (the overloads the cursor can mean), and what the answer can miss.
pub struct Overrides<'a> {
    pub found: Vec<MethodMatch<'a>>,
    /// Why the list can be short, one sentence each.
    pub gaps: Vec<String>,
}

/// The classes below `target` that override or implement one of `wanted`.
pub fn overrides_below<'a>(
    index: &'a SubtypeIndex,
    target: &SuperKey,
    wanted: &[&MemberDescriptor],
) -> Overrides<'a> {
    let mut found = Vec::new();
    let mut gaps = Vec::new();
    let Some(name) = wanted.first().map(|method| method.name.clone()) else {
        return Overrides { found, gaps };
    };
    for class in index.all_subtypes(target) {
        // An interface that extends another declares no body.
        if class.kind == ClassKind::Interface {
            continue;
        }
        let declared = methods_named(&class.members, &name);
        let (ordinals, certain) = matching(&declared, wanted);
        if !ordinals.is_empty() {
            if !certain {
                gaps.push(format!(
                    "`{}` declares `{}` with parameters oxabl cannot compare: it may be an \
                     overload",
                    class.name.as_written(),
                    name.as_written()
                ));
            }
            found.push(MethodMatch {
                class,
                ordinals,
                total: declared.len(),
                certain,
            });
        } else if !class.members_complete {
            gaps.push(format!(
                "`{}` has members that oxabl cannot read (an include or an &IF): it may \
                 declare `{}`",
                class.name.as_written(),
                name.as_written()
            ));
        }
    }
    Overrides { found, gaps }
}

/// A class above a method, found by [`super_methods`].
#[derive(Debug, Clone)]
pub struct SuperMatch {
    pub class: IndexName,
    pub file: IndexedFileId,
    /// As [`MethodMatch::ordinals`].
    pub ordinals: Vec<usize>,
    /// As [`MethodMatch::total`].
    pub total: usize,
    /// As [`MethodMatch::certain`].
    pub certain: bool,
}

/// The methods a method of `start` overrides or implements, in the nearest
/// supertypes that declare one.
pub struct SuperMethods {
    pub found: Vec<SuperMatch>,
    /// Why the answer can be short, one sentence each.
    pub gaps: Vec<String>,
}

/// Walk the supertypes of `start`, nearest level first, and return the
/// declarations like one of `wanted` at the first level that has any. A
/// supertype that does not resolve, or whose members oxabl cannot read, is a
/// gap when it lies at or before that level, or when nothing is found.
pub fn super_methods(
    index: &dyn WorkspaceIndex,
    start: &ClassDescriptor,
    wanted: &[&MemberDescriptor],
) -> SuperMethods {
    let mut found: Vec<SuperMatch> = Vec::new();
    let mut gaps: Vec<String> = Vec::new();
    let Some(name) = wanted.first().map(|method| method.name.clone()) else {
        return SuperMethods { found, gaps };
    };
    let mut seen: FxHashSet<(IndexName, IndexedFileId)> = FxHashSet::default();
    seen.insert((start.name.clone(), start.file));
    let mut level = supertypes_of(start, index)
        .into_iter()
        .map(|supertype| (start.name.clone(), supertype))
        .collect::<Vec<_>>();
    while !level.is_empty() {
        let mut next = Vec::new();
        for (child, supertype) in level {
            match supertype.resolved {
                Supertype::Class {
                    name: class,
                    file,
                    kind,
                } => {
                    if !seen.insert((class.clone(), file)) {
                        continue;
                    }
                    let IndexAnswer::Found(descriptor) = index.class(&class) else {
                        gaps.push(format!(
                            "`{}` cannot be read: the method may be there",
                            class.as_written()
                        ));
                        continue;
                    };
                    let IndexAnswer::Found(members) = index.class_members(&class) else {
                        gaps.push(format!(
                            "the members of `{}` cannot be read",
                            class.as_written()
                        ));
                        continue;
                    };
                    let declared = methods_named(&members, &name);
                    let (ordinals, certain) = matching(&declared, wanted);
                    if ordinals.is_empty() {
                        if !descriptor.members_complete {
                            gaps.push(format!(
                                "`{}` has members that oxabl cannot read (an include or an \
                                 &IF): it may declare `{}`",
                                class.as_written(),
                                name.as_written()
                            ));
                        }
                        if kind != ClassKind::Enum {
                            next.extend(
                                supertypes_of(&descriptor, index)
                                    .into_iter()
                                    .map(|up| (class.clone(), up)),
                            );
                        }
                    } else {
                        if !certain {
                            gaps.push(format!(
                                "`{}` declares `{}` with parameters oxabl cannot compare: it \
                                 may be an overload",
                                class.as_written(),
                                name.as_written()
                            ));
                        }
                        found.push(SuperMatch {
                            class,
                            file,
                            ordinals,
                            total: declared.len(),
                            certain,
                        });
                    }
                }
                Supertype::Builtin { name: builtin, .. } => gaps.push(format!(
                    "`{}` names the built-in type `{builtin}`: oxabl does not know its methods",
                    child.as_written()
                )),
                Supertype::DotNet => gaps.push(format!(
                    "`{}` names a .NET type: oxabl does not read assemblies",
                    child.as_written()
                )),
                Supertype::Unresolved(why) => gaps.push(format!(
                    "`{}` names `{}`, which oxabl cannot resolve ({}): the method may be there",
                    child.as_written(),
                    supertype.written.as_written(),
                    why.describe()
                )),
            }
        }
        if !found.is_empty() {
            break;
        }
        level = next;
    }
    if found.is_empty()
        && start.kind != ClassKind::Interface
        && OBJECT_METHODS
            .iter()
            .any(|known| name.as_str().eq_ignore_ascii_case(known))
    {
        gaps.push(format!(
            "`{}` can override a method of `Progress.Lang.Object`, which oxabl does not know",
            name.as_written()
        ));
    }
    SuperMethods { found, gaps }
}

/// The methods every class inherits from `Progress.Lang.Object`.
const OBJECT_METHODS: &[&str] = &["clone", "equals", "getclass", "tostring"];
