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
//! Built-in Progress types have no source file. [`builtin_type`] says what
//! oxabl knows about their names, kinds and supertypes, and
//! [`builtin_may_declare`] says whether it knows their methods. Other code
//! reads built-in types too: `ImportContext::resolve_class`, the built-in
//! type at the cursor (`Navigation::builtin_type_at`), the implicit parent
//! `Progress.Lang.Object` of the daemon, and `OBJECT_METHODS` here. A catalog
//! of built-in classes with members changes all of them; see
//! `docs/design/type-hierarchy.md`.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use oxabl_ast::Span;
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    ClassDescriptor, ClassKind, ClassResolution, ImportContext, IndexAnswer, IndexName,
    IndexRevision, IndexedFileId, LocateKind, Located, MemberDescriptor, MemberParam,
    MemberParamShape, MemberType, Signature, SymbolKind, WorkspaceIndex,
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

/// Whether the built-in type `type_name`, or a built-in type above it, may
/// declare a method named `method`. `false` only when oxabl knows the methods
/// and the name is not among them.
///
/// This is the seam for the members of built-in classes: today oxabl knows
/// only the methods of `Progress.Lang.Object`, and every other built-in type
/// may declare any method.
pub fn builtin_may_declare(type_name: &str, method: &IndexName) -> bool {
    if type_name.eq_ignore_ascii_case("Progress.Lang.Object") {
        return OBJECT_METHODS
            .iter()
            .any(|known| method.as_str().eq_ignore_ascii_case(known));
    }
    true
}

/// The methods every class inherits from `Progress.Lang.Object`.
const OBJECT_METHODS: &[&str] = &["clone", "equals", "getclass", "tostring"];

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
    /// The imports of the declaring file, which the class names in its
    /// parameters are written under.
    pub imports: Arc<ImportContext>,
    /// The supertypes the header names, resolved. Never leaves one out.
    pub supertypes: Vec<SupertypeRef>,
    /// Byte span of the type name in the unexpanded file, when known from the
    /// facts parse.
    pub name_span: Option<Span>,
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
    by_key: FxHashMap<(IndexName, IndexedFileId), usize>,
    direct: FxHashMap<SuperKey, Vec<(usize, Relation)>>,
    /// Supertype names that did not resolve, by the folded last segment of the
    /// name as written: each can be a supertype of any class of that name.
    unresolved: FxHashMap<OxablAtom, Vec<(usize, usize)>>,
    /// Class files that could not be read: each can hold a subtype.
    unreadable: Vec<PathBuf>,
}

/// What reads the classes of a workspace for the walks of this module.
///
/// A name in a class header means what the file that writes it says, so each
/// file has its own resolver: the search base of a class file depends on its
/// package.
pub trait ClassFiles {
    /// The class file the index minted `file` for.
    fn path_of(&self, file: IndexedFileId) -> Option<PathBuf>;
    /// The index that resolves the names the class file `path` writes.
    fn resolver(&self, path: &Path) -> Rc<dyn WorkspaceIndex + '_>;
    /// The facts of the class file `path`, an open buffer over the disk.
    fn facts(&self, path: &Path) -> Arc<FileFacts>;
}

impl SubtypeIndex {
    /// Build the index from the class files of a workspace. Each supertype
    /// name is resolved through `resolver` for the file that writes it.
    pub fn build<'a>(
        files: impl IntoIterator<Item = ClassFile>,
        resolver: &dyn Fn(&Path) -> Rc<dyn WorkspaceIndex + 'a>,
    ) -> Self {
        let mut built = SubtypeIndex::default();
        for file in files {
            if !file.facts.parsed {
                built.unreadable.push(file.path);
                continue;
            }
            if file.facts.classes.is_empty() {
                continue;
            }
            let index = resolver(&file.path);
            for class in &file.facts.classes {
                let descriptor = &class.descriptor;
                built.add(ClassNode {
                    name: descriptor.name.clone(),
                    file: file.facts.file,
                    path: file.path.clone(),
                    kind: descriptor.kind,
                    members_complete: descriptor.members_complete,
                    members: Arc::clone(&class.members),
                    imports: Arc::clone(&descriptor.imports),
                    supertypes: supertypes_of(descriptor, &*index),
                    name_span: class.name_span,
                });
            }
        }
        built.unreadable.sort();
        built
    }

    fn add(&mut self, node: ClassNode) {
        let at = self.nodes.len();
        self.by_key.insert((node.name.clone(), node.file), at);
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
        self.by_key
            .get(&(name.clone(), file))
            .map(|at| &self.nodes[*at])
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
    /// first, each once, never `target` itself. A cycle ends the walk.
    pub fn all_subtypes(&self, target: &SuperKey) -> Vec<&ClassNode> {
        let mut seen: FxHashSet<usize> = FxHashSet::default();
        if let SuperKey::Class(name, file) = target
            && let Some(at) = self.by_key.get(&(name.clone(), *file))
        {
            seen.insert(*at);
        }
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
pub struct LookupMemo<I> {
    inner: I,
    classes: RefCell<FxHashMap<OxablAtom, IndexAnswer<Arc<ClassDescriptor>>>>,
}

impl<I: WorkspaceIndex> LookupMemo<I> {
    pub fn new(inner: I) -> Self {
        LookupMemo {
            inner,
            classes: RefCell::default(),
        }
    }
}

impl<I: WorkspaceIndex> WorkspaceIndex for LookupMemo<I> {
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
    /// parameter, a class name that does not resolve the same way on both
    /// sides.
    Unknown,
}

/// The temp-table and buffer names of some classes: the names a
/// `LIKE table.field` parameter can mean instead of a database table.
#[derive(Debug, Clone)]
pub struct RecordNames {
    names: FxHashSet<IndexName>,
    /// False when a class added has members that oxabl cannot read, or a class
    /// that should be added cannot be found.
    known: bool,
}

impl Default for RecordNames {
    fn default() -> Self {
        RecordNames {
            names: FxHashSet::default(),
            known: true,
        }
    }
}

impl RecordNames {
    /// The names of one class: its members, and whether the list is whole.
    pub fn of(members: &[MemberDescriptor], complete: bool) -> Self {
        let mut records = RecordNames::default();
        records.add(members, complete);
        records
    }

    /// Names that oxabl cannot list: every `LIKE table.field` can mean a
    /// temp-table or a buffer.
    pub fn unknown() -> Self {
        RecordNames {
            names: FxHashSet::default(),
            known: false,
        }
    }

    pub fn add(&mut self, members: &[MemberDescriptor], complete: bool) {
        self.known &= complete;
        self.names.extend(
            members
                .iter()
                .filter(|m| matches!(m.kind, SymbolKind::TempTable | SymbolKind::Buffer))
                .map(|m| m.name.clone()),
        );
    }

    /// Whether `table` can be a temp-table or a buffer of these classes.
    fn may_name(&self, table: &IndexName) -> bool {
        !self.known || self.names.contains(table)
    }
}

/// Where a method is declared: the imports and the index that give a class
/// name in its parameters its meaning, and the temp-tables and buffers that a
/// `LIKE` in its parameters can name: those of its class, and of the classes
/// between it and the other method of a comparison.
#[derive(Clone, Copy)]
pub struct Declaring<'a> {
    imports: &'a ImportContext,
    index: &'a dyn WorkspaceIndex,
    records: &'a RecordNames,
}

impl<'a> Declaring<'a> {
    pub fn new(
        imports: &'a ImportContext,
        index: &'a dyn WorkspaceIndex,
        records: &'a RecordNames,
    ) -> Self {
        Declaring {
            imports,
            index,
            records,
        }
    }
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
/// other. Different lists are overloads. A class-typed parameter means the
/// class its own declaring file resolves the name to.
pub fn same_signature(
    a: &MemberDescriptor,
    a_in: &Declaring<'_>,
    b: &MemberDescriptor,
    b_in: &Declaring<'_>,
) -> SameSignature {
    let (Some(a), Some(b)) = (&a.params, &b.params) else {
        return SameSignature::Unknown;
    };
    if a.len() != b.len() {
        return SameSignature::No;
    }
    let mut unknown = false;
    for (a, b) in a.iter().zip(b.iter()) {
        match same_param(a, a_in, b, b_in) {
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

fn same_param(
    a: &MemberParam,
    a_in: &Declaring<'_>,
    b: &MemberParam,
    b_in: &Declaring<'_>,
) -> SameSignature {
    if a.mode != b.mode {
        return SameSignature::No;
    }
    match (&a.shape, &b.shape) {
        (MemberParamShape::Value(a), MemberParamShape::Value(b)) => same_type(a, a_in, b, b_in),
        (
            MemberParamShape::Like {
                source: a,
                extent: a_extent,
            },
            MemberParamShape::Like {
                source: b,
                extent: b_extent,
            },
        ) => {
            if a == b && a_extent == b_extent && same_like_target(a, a_in, b_in) {
                SameSignature::Yes
            } else {
                SameSignature::Unknown
            }
        }
        // `LIKE` can give the type the other parameter names.
        (MemberParamShape::Like { .. }, MemberParamShape::Value(_))
        | (MemberParamShape::Value(_), MemberParamShape::Like { .. }) => SameSignature::Unknown,
        (MemberParamShape::Buffer, MemberParamShape::Buffer)
        | (MemberParamShape::Table, MemberParamShape::Table)
        | (MemberParamShape::Dataset, MemberParamShape::Dataset) => SameSignature::Unknown,
        (MemberParamShape::TableHandle, MemberParamShape::TableHandle)
        | (MemberParamShape::DatasetHandle, MemberParamShape::DatasetHandle) => SameSignature::Yes,
        _ => SameSignature::No,
    }
}

/// Whether `LIKE source`, written alike in two declarations, names one field
/// in both: a `table.field` whose table no temp-table or buffer of either side
/// can be, so both read the database table. A name with no table can be a
/// variable of either class.
fn same_like_target(source: &IndexName, a_in: &Declaring<'_>, b_in: &Declaring<'_>) -> bool {
    let mut parts = source.as_str().rsplit('.');
    let (Some(_field), Some(table)) = (parts.next(), parts.next()) else {
        return false;
    };
    let table = IndexName::new(table);
    !a_in.records.may_name(&table) && !b_in.records.may_name(&table)
}

/// What a class name in a parameter means.
enum Named {
    Is(SuperKey),
    /// No class has the name.
    Absent,
    /// A lookup that oxabl cannot settle.
    Unclear,
}

fn resolve_named(name: &IndexName, within: &Declaring<'_>) -> Named {
    match within
        .imports
        .resolve_class(name.as_written(), within.index)
    {
        ClassResolution::Class(found) => Named::Is(SuperKey::Class(found.name.clone(), found.file)),
        ClassResolution::Builtin(known) => Named::Is(SuperKey::Builtin(IndexName::new(known.name))),
        ClassResolution::NotFound => Named::Absent,
        ClassResolution::Assembly | ClassResolution::Unusable | ClassResolution::Unknowable => {
            Named::Unclear
        }
    }
}

fn same_type(
    a: &MemberType,
    a_in: &Declaring<'_>,
    b: &MemberType,
    b_in: &Declaring<'_>,
) -> SameSignature {
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
            match (resolve_named(a, a_in), resolve_named(b, b_in)) {
                (Named::Is(a), Named::Is(b)) => {
                    if a == b {
                        SameSignature::Yes
                    } else {
                        SameSignature::No
                    }
                }
                // The same unresolved name under the same imports is one name.
                (Named::Absent, Named::Absent) if a == b && a_in.imports == b_in.imports => {
                    SameSignature::Yes
                }
                _ => SameSignature::Unknown,
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
fn matching(
    declared: &[&MemberDescriptor],
    declared_in: &Declaring<'_>,
    wanted: &[&MemberDescriptor],
    wanted_in: &Declaring<'_>,
) -> (Vec<usize>, bool) {
    let mut ordinals = Vec::new();
    let mut certain = true;
    for (ordinal, method) in declared.iter().enumerate() {
        if !overridable(method) {
            continue;
        }
        let mut yes = false;
        let mut unknown = false;
        for wanted in wanted {
            match same_signature(wanted, wanted_in, method, declared_in) {
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

/// What a class declares that `wanted` can mean.
struct Declared {
    ordinals: Vec<usize>,
    total: usize,
    certain: bool,
}

fn declared_in_node(
    node: &ClassNode,
    source: &dyn ClassFiles,
    records: &RecordNames,
    name: &IndexName,
    wanted: &[&MemberDescriptor],
    wanted_in: &Declaring<'_>,
) -> Declared {
    let declared = methods_named(&node.members, name);
    if declared.is_empty() {
        return Declared {
            ordinals: Vec::new(),
            total: 0,
            certain: true,
        };
    }
    let resolver = source.resolver(&node.path);
    let within = Declaring::new(&node.imports, &*resolver, records);
    let (ordinals, certain) = matching(&declared, &within, wanted, wanted_in);
    Declared {
        ordinals,
        total: declared.len(),
        certain,
    }
}

fn uncertain_gap(class: &IndexName, name: &IndexName) -> String {
    format!(
        "`{}` declares `{}` with parameters oxabl cannot compare: it may be an overload",
        class.as_written(),
        name.as_written()
    )
}

fn incomplete_gap(class: &IndexName, name: &IndexName) -> String {
    format!(
        "`{}` has members that oxabl cannot read (an include, an &IF or a preprocessor \
         reference): it may declare `{}`",
        class.as_written(),
        name.as_written()
    )
}

/// Keep the first of each gap, in order.
fn dedupe(gaps: &mut Vec<String>) {
    let mut seen: FxHashSet<String> = FxHashSet::default();
    gaps.retain(|gap| seen.insert(gap.clone()));
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
///
/// With `implements` (the method is an interface method), a class that
/// declares nothing of the name still implements it through the method it
/// inherits: the nearest declaration up its parent chain counts, and the
/// class that declares it can lie outside the subtypes of `target`.
pub fn overrides_below<'a>(
    index: &'a SubtypeIndex,
    source: &dyn ClassFiles,
    target: &SuperKey,
    implements: bool,
    wanted: &[&MemberDescriptor],
    wanted_in: &Declaring<'_>,
) -> Overrides<'a> {
    let mut found: Vec<MethodMatch<'a>> = Vec::new();
    let mut gaps = Vec::new();
    let Some(name) = wanted.first().map(|method| method.name.clone()) else {
        return Overrides { found, gaps };
    };
    let like = has_like(wanted);
    for class in index.all_subtypes(target) {
        // An interface that extends another declares no body.
        if class.kind == ClassKind::Interface {
            continue;
        }
        // Each class above `class` is above the declaration it matches too.
        let records = if like {
            closure_records(index, class)
        } else {
            RecordNames::unknown()
        };
        let mut at = class;
        let mut walked: FxHashSet<(IndexName, IndexedFileId)> = FxHashSet::default();
        loop {
            let mut declared = declared_in_node(at, source, &records, &name, wanted, wanted_in);
            if implements && !std::ptr::eq(at, class) {
                // An interface method is implemented by a public method only.
                let members = methods_named(&at.members, &name);
                declared.ordinals.retain(|ordinal| {
                    members
                        .get(*ordinal)
                        .is_some_and(|member| member.access == oxabl_ast::AccessModifier::Public)
                });
            }
            if !declared.ordinals.is_empty() {
                if !declared.certain {
                    gaps.push(uncertain_gap(&at.name, &name));
                }
                if !found
                    .iter()
                    .any(|known| known.class.name == at.name && known.class.file == at.file)
                {
                    found.push(MethodMatch {
                        class: at,
                        ordinals: declared.ordinals,
                        total: declared.total,
                        certain: declared.certain,
                    });
                }
                break;
            }
            if !at.members_complete {
                gaps.push(incomplete_gap(&at.name, &name));
            }
            if !implements {
                break;
            }
            let Some(parent) = at
                .supertypes
                .iter()
                .find(|up| up.relation == Relation::Inherits)
            else {
                break;
            };
            match &parent.resolved {
                Supertype::Class {
                    name: parent_name,
                    file,
                    ..
                } => {
                    if !walked.insert((parent_name.clone(), *file)) {
                        break;
                    }
                    match index.class(parent_name, *file) {
                        Some(node) => at = node,
                        None => {
                            gaps.push(format!(
                                "`{}` extends `{}`, which the index did not read: it may \
                                 declare `{}`",
                                at.name.as_written(),
                                parent_name.as_written(),
                                name.as_written()
                            ));
                            break;
                        }
                    }
                }
                Supertype::Builtin { name: builtin, .. } => {
                    if builtin_may_declare(builtin, &name) {
                        gaps.push(format!(
                            "`{}` extends the built-in type `{builtin}`: oxabl does not know \
                             its methods",
                            at.name.as_written()
                        ));
                    }
                    break;
                }
                Supertype::DotNet => {
                    gaps.push(format!(
                        "`{}` extends a .NET type: oxabl does not read assemblies",
                        at.name.as_written()
                    ));
                    break;
                }
                Supertype::Unresolved(why) => {
                    gaps.push(format!(
                        "`{}` extends `{}`, which oxabl cannot resolve ({}): it may declare `{}`",
                        at.name.as_written(),
                        parent.written.as_written(),
                        why.describe(),
                        name.as_written()
                    ));
                    break;
                }
            }
        }
    }
    dedupe(&mut gaps);
    Overrides { found, gaps }
}

/// Whether a method of `wanted` has a `LIKE` parameter: only then do the
/// temp-tables and buffers of the classes matter.
pub fn has_like(wanted: &[&MemberDescriptor]) -> bool {
    wanted.iter().any(|method| {
        method.params.as_deref().is_some_and(|params| {
            params
                .iter()
                .any(|p| matches!(p.shape, MemberParamShape::Like { .. }))
        })
    })
}

/// The temp-tables and buffers of `class` and of every class and interface
/// above it, through any number of headers. Each of them can give a name to
/// the table of a `LIKE` in a list of `class`. A supertype that the index does
/// not hold, or that does not resolve, makes them not known.
pub fn closure_records(index: &SubtypeIndex, class: &ClassNode) -> RecordNames {
    let mut records = RecordNames::of(&class.members, class.members_complete);
    let mut seen: FxHashSet<(&IndexName, IndexedFileId)> = FxHashSet::default();
    seen.insert((&class.name, class.file));
    let mut todo = vec![class];
    while let Some(at) = todo.pop() {
        for up in &at.supertypes {
            match &up.resolved {
                Supertype::Class { name, file, .. } => {
                    if !seen.insert((name, *file)) {
                        continue;
                    }
                    let Some(node) = index.class(name, *file) else {
                        return RecordNames::unknown();
                    };
                    records.add(&node.members, node.members_complete);
                    todo.push(node);
                }
                // A built-in or .NET type has no temp-table that ABL code names.
                Supertype::Builtin { .. } | Supertype::DotNet => {}
                Supertype::Unresolved(_) => return RecordNames::unknown(),
            }
        }
    }
    records
}

/// [`closure_records`] for the class `start` of the file `start_path`, read
/// through `source`: a class or interface that cannot be read makes them not
/// known.
fn source_closure_records(
    source: &dyn ClassFiles,
    start_path: &Path,
    start: &ClassDescriptor,
    start_members: &[MemberDescriptor],
) -> RecordNames {
    let mut records = RecordNames::of(start_members, start.members_complete);
    let mut seen: FxHashSet<(IndexName, IndexedFileId)> = FxHashSet::default();
    seen.insert((start.name.clone(), start.file));
    let mut todo = supertypes_of(start, &*source.resolver(start_path));
    while let Some(up) = todo.pop() {
        match up.resolved {
            Supertype::Class { name, file, .. } => {
                if !seen.insert((name.clone(), file)) {
                    continue;
                }
                let Some(path) = source.path_of(file) else {
                    return RecordNames::unknown();
                };
                let facts = source.facts(&path);
                let Some(read) = facts.class(&name).filter(|_| facts.parsed) else {
                    return RecordNames::unknown();
                };
                records.add(&read.members, read.descriptor.members_complete);
                todo.extend(supertypes_of(&read.descriptor, &*source.resolver(&path)));
            }
            Supertype::Builtin { .. } | Supertype::DotNet => {}
            Supertype::Unresolved(_) => return RecordNames::unknown(),
        }
    }
    records
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

/// The methods a method of `start` overrides or implements.
pub struct SuperMethods {
    pub found: Vec<SuperMatch>,
    /// Why the answer can be short, one sentence each.
    pub gaps: Vec<String>,
}

/// A supertype to look at, with the class that names it.
type Up = (IndexName, SupertypeRef);

enum Examined {
    /// The supertype declares the method.
    Matched,
    /// The walk cannot go on from here; a gap says why when it matters.
    Stopped,
    /// The supertype does not declare it: its own supertypes are next.
    Missed(Vec<Up>),
}

struct SuperWalk<'a> {
    source: &'a dyn ClassFiles,
    name: IndexName,
    wanted: &'a [&'a MemberDescriptor],
    wanted_in: Declaring<'a>,
    /// The temp-tables and buffers of the start and of every type above it:
    /// each type the walk reads is one of them.
    records: &'a RecordNames,
    seen: FxHashSet<(IndexName, IndexedFileId)>,
    found: Vec<SuperMatch>,
    gaps: Vec<String>,
}

impl SuperWalk<'_> {
    fn examine(&mut self, child: &IndexName, supertype: &SupertypeRef) -> Examined {
        match &supertype.resolved {
            Supertype::Class {
                name: class,
                file,
                kind,
            } => {
                if !self.seen.insert((class.clone(), *file)) {
                    return Examined::Stopped;
                }
                let Some(path) = self.source.path_of(*file) else {
                    self.gaps.push(format!(
                        "the file of `{}` is not known: the method may be there",
                        class.as_written()
                    ));
                    return Examined::Stopped;
                };
                let facts = self.source.facts(&path);
                let Some(read) = facts.class(class).filter(|_| facts.parsed) else {
                    self.gaps.push(format!(
                        "`{}` cannot be read: the method may be there",
                        class.as_written()
                    ));
                    return Examined::Stopped;
                };
                let resolver = self.source.resolver(&path);
                let within = Declaring::new(&read.descriptor.imports, &*resolver, self.records);
                let declared = methods_named(&read.members, &self.name);
                let (ordinals, certain) =
                    matching(&declared, &within, self.wanted, &self.wanted_in);
                if !ordinals.is_empty() {
                    if !certain {
                        self.gaps.push(uncertain_gap(class, &self.name));
                    }
                    self.found.push(SuperMatch {
                        class: class.clone(),
                        file: *file,
                        ordinals,
                        total: declared.len(),
                        certain,
                    });
                    return Examined::Matched;
                }
                if !read.descriptor.members_complete {
                    self.gaps.push(incomplete_gap(class, &self.name));
                }
                if *kind == ClassKind::Enum {
                    return Examined::Missed(Vec::new());
                }
                Examined::Missed(
                    supertypes_of(&read.descriptor, &*resolver)
                        .into_iter()
                        .map(|up| (class.clone(), up))
                        .collect(),
                )
            }
            Supertype::Builtin { name: builtin, .. } => {
                if builtin_may_declare(builtin, &self.name) {
                    self.gaps.push(format!(
                        "`{}` names the built-in type `{builtin}`: oxabl does not know its \
                         methods",
                        child.as_written()
                    ));
                }
                Examined::Stopped
            }
            Supertype::DotNet => {
                self.gaps.push(format!(
                    "`{}` names a .NET type: oxabl does not read assemblies",
                    child.as_written()
                ));
                Examined::Stopped
            }
            Supertype::Unresolved(why) => {
                self.gaps.push(format!(
                    "`{}` names `{}`, which oxabl cannot resolve ({}): the method may be there",
                    child.as_written(),
                    supertype.written.as_written(),
                    why.describe()
                ));
                Examined::Stopped
            }
        }
    }
}

/// The methods that a method of `start` (the class file `start_path`)
/// overrides or implements.
///
/// The parent chain and the interfaces are two searches, because a method can
/// override a class method and implement an interface method at once. The
/// chain gives the nearest class that declares a method like one of `wanted`;
/// each interface, those of `start` and those of the classes of the chain
/// before the match, gives its nearest declaration (an interface extends
/// others, one branch at a time). A supertype that does not resolve, or whose
/// members oxabl cannot read, is a gap where a walk reaches it before a match.
pub fn super_methods(
    source: &dyn ClassFiles,
    start_path: &Path,
    start: &ClassDescriptor,
    start_members: &[MemberDescriptor],
    wanted: &[&MemberDescriptor],
) -> SuperMethods {
    let Some(name) = wanted.first().map(|method| method.name.clone()) else {
        return SuperMethods {
            found: Vec::new(),
            gaps: Vec::new(),
        };
    };
    let resolver = source.resolver(start_path);
    // The whole closure, not only what the walk reads before a match: an
    // interface does not see a temp-table above the matched parent, and the
    // start does.
    let records = if has_like(wanted) {
        source_closure_records(source, start_path, start, start_members)
    } else {
        RecordNames::unknown()
    };
    let mut walk = SuperWalk {
        source,
        name: name.clone(),
        wanted,
        wanted_in: Declaring::new(&start.imports, &*resolver, &records),
        records: &records,
        seen: FxHashSet::default(),
        found: Vec::new(),
        gaps: Vec::new(),
    };
    walk.seen.insert((start.name.clone(), start.file));
    let mut chain: Option<Up> = None;
    let mut interfaces: Vec<Up> = Vec::new();
    for up in supertypes_of(start, &*resolver) {
        let up = (start.name.clone(), up);
        if start.kind != ClassKind::Interface && up.1.relation == Relation::Inherits {
            chain = Some(up);
        } else {
            interfaces.push(up);
        }
    }
    // A chain that ends on its own ends at `Progress.Lang.Object`.
    let mut reaches_object = start.kind == ClassKind::Class;
    while let Some((child, up)) = chain.take() {
        match walk.examine(&child, &up) {
            Examined::Matched | Examined::Stopped => {
                reaches_object = false;
                break;
            }
            Examined::Missed(ups) => {
                for up in ups {
                    if up.1.relation == Relation::Inherits {
                        chain = Some(up);
                    } else {
                        interfaces.push(up);
                    }
                }
            }
        }
    }
    let mut at = 0;
    while at < interfaces.len() {
        let (child, up) = interfaces[at].clone();
        at += 1;
        if let Examined::Missed(ups) = walk.examine(&child, &up) {
            interfaces.extend(ups);
        }
    }
    // Only the parent chain decides: an interface match does not rule out an
    // override of an `Object` method.
    if reaches_object && builtin_may_declare("Progress.Lang.Object", &name) {
        walk.gaps.push(format!(
            "`{}` can override a method of `Progress.Lang.Object`, which oxabl does not know",
            name.as_written()
        ));
    }
    dedupe(&mut walk.gaps);
    SuperMethods {
        found: walk.found,
        gaps: walk.gaps,
    }
}
