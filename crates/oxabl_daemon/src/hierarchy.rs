//! Go to implementation, go to super method and the type hierarchy across a
//! workspace. See `docs/design/type-hierarchy.md`.
//!
//! The unit that asks is analysed first, with go to definition, to find the
//! class or method at the cursor. The rest reads class files: the facts of
//! each file are memoized per file by the daemon, so a request reads again
//! only a file that changed, and it builds the reverse inheritance index
//! ([`SubtypeIndex`]) from them. Open buffers are read over the disk, and an
//! open class file that is not saved yet counts as a class of the workspace.
//! Each class header is resolved with the search paths of its own file, as go
//! to definition does.
//!
//! An answer that can be short says why in [`HierarchyAnswer::gaps`]. A
//! supertype that oxabl cannot resolve is an item of its own, never left out.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use oxabl_index::hierarchy::{
    ClassNode, Declaring, RecordNames, Relation, SubtypeIndex, SuperKey, Supertype, SupertypeRef,
    Unresolved, builtin_type, closure_records, has_like, methods_named, overridable,
    overrides_below, simple_name, super_methods, supertypes_of,
};
use oxabl_pipeline::{
    ClassDecl, Gap, MethodAt, MethodsAt, ReverseGraph, Span, Target, class_decl, class_name_span,
};
use oxabl_semantic::{ClassDescriptor, ClassKind, IndexName, MemberDescriptor};

use crate::SessionHost;
use crate::db::{HierarchyUnit, with_hierarchy};
use crate::references::{
    GraphAnswer, ReferenceError, analyse, graph_or_cancel_waiting, pass_counts,
};
use crate::subtypes::{ClassList, class_list, index_for};
pub use crate::subtypes::{
    NoProgress, Progress, build_subtype_index_here, forget_subtype_index, subtype_index_builds,
    wait_for_subtype_index, want_subtype_index,
};

/// The most class files the reverse index reads. A workspace with more is
/// cut, and the answer says so.
pub const MAX_CLASSES: usize = 20_000;

/// The most classes a list answer places: the subtypes of a type, and the
/// classes below it for go to implementation. A longer list is cut, and the
/// answer says so.
pub const MAX_LISTED: usize = 1_000;

/// What a type hierarchy item stands for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Form {
    /// A class, interface or enum of the workspace.
    Class,
    /// A built-in Progress type: it has no file.
    Builtin,
    /// A supertype name that did not resolve, and why. It has no supertypes
    /// or subtypes of its own.
    Unresolved(String),
}

/// What an item carries back to the server.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeKey {
    /// The unit analysed for the item.
    pub root: PathBuf,
    /// The class file, or for a type with no file, the file of the class that
    /// names it, where the item is placed.
    pub path: PathBuf,
    /// The qualified name.
    pub name: String,
    pub form: Form,
    /// For a built-in type, where `path` writes the name that the item stands
    /// for. Its supertypes are placed there.
    pub anchor: Option<Span>,
}

/// What a hierarchy request asks.
#[derive(Debug, Clone)]
pub enum HierarchyQuery {
    /// The class at byte `offset` of `file`, or the class that holds it.
    Prepare {
        file: PathBuf,
        offset: u32,
    },
    Supertypes(TypeKey),
    Subtypes(TypeKey),
    /// The classes that implement or extend the type, or override the method,
    /// at the cursor.
    Implementation {
        file: PathBuf,
        offset: u32,
    },
    /// The methods that the method at the cursor overrides or implements.
    SuperMethod {
        file: PathBuf,
        offset: u32,
    },
}

/// What a type hierarchy item is, for the client's icon.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemKind {
    Class,
    Interface,
    Enum,
    /// A name that did not resolve.
    Unresolved,
}

/// One item of the type hierarchy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeItem {
    pub key: TypeKey,
    pub kind: ItemKind,
    pub detail: Option<String>,
    /// The file the item is placed in, as `span` and `selection` measure it.
    pub path: PathBuf,
    pub span: Span,
    pub selection: Span,
}

/// The answer to a [`HierarchyQuery`], with the text of every file it places a
/// span in.
#[derive(Debug, Clone, Default)]
pub struct HierarchyAnswer {
    pub items: Vec<TypeItem>,
    pub locations: Vec<Target>,
    /// Why the answer can miss something. Empty for a complete answer.
    pub gaps: Vec<Gap>,
    /// For a request about a method: why the cursor is on none that oxabl
    /// reads. Such an answer says nothing about overrides.
    pub not_a_method: Option<String>,
    pub texts: HashMap<PathBuf, Arc<str>>,
}

/// Answer `query` for the unit `root` of the workspace at `session_root`.
///
/// Blocks until a workspace pass is current and the subtype index is built
/// when the query reads the subtypes, so callers keep it off the LSP thread.
/// `progress` hears about the wait. Stops with [`ReferenceError::Cancelled`]
/// once `cancel` is set.
pub fn answer_hierarchy(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    query: &HierarchyQuery,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<HierarchyAnswer, ReferenceError> {
    match query {
        HierarchyQuery::Prepare { file, offset } => {
            prepare(host, session_root, root, file, *offset)
        }
        HierarchyQuery::Supertypes(key) => supertypes(host, session_root, key),
        HierarchyQuery::Subtypes(key) => subtypes(host, session_root, key, cancel, progress),
        HierarchyQuery::Implementation { file, offset } => {
            implementation(host, session_root, root, file, *offset, cancel, progress)
        }
        HierarchyQuery::SuperMethod { file, offset } => {
            super_method(host, session_root, root, file, *offset)
        }
    }
}

/// Run `work` on a fresh snapshot of the session, with the open buffers over
/// the disk.
fn with_snapshot<T>(
    host: &SessionHost,
    session_root: &Path,
    work: impl FnOnce(&HierarchyUnit<'_>) -> T,
) -> Result<T, ReferenceError> {
    let taken = host.with(|sessions| {
        let session = sessions.get(session_root)?;
        Some((session.database().clone(), session.buffer_overlay()))
    });
    let Some((snapshot, overlay)) = taken else {
        return Err(ReferenceError::Failed("no session".to_string()));
    };
    with_hierarchy(&snapshot, overlay, work).ok_or(ReferenceError::Modified)
}

/// What go to definition finds at the cursor of `file`, read in the unit
/// `root`.
struct Cursor {
    types: Vec<oxabl_pipeline::TypeAt>,
    enclosing: Option<oxabl_pipeline::TypeAt>,
    methods: MethodsAt,
    /// A built-in type named at the cursor, and where the file writes it.
    builtin: Option<(&'static str, Span)>,
}

fn cursor(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<Cursor, ReferenceError> {
    cursors(host, session_root, root, file, &[offset])?
        .pop()
        .ok_or_else(|| ReferenceError::Failed("no cursor".to_string()))
}

/// [`cursor`] at each of `offsets`, from one analysis of `root`.
fn cursors(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offsets: &[u32],
) -> Result<Vec<Cursor>, ReferenceError> {
    analyse(host, session_root, root, |unit| {
        let file = unit.spelled(file);
        offsets
            .iter()
            .map(|&offset| Cursor {
                types: unit.navigation.types_at(&file, offset),
                enclosing: unit.navigation.enclosing_type(&file, offset),
                methods: unit.navigation.methods_at(&file, offset),
                builtin: unit.navigation.builtin_type_at(&file, offset),
            })
            .collect()
    })
    .ok_or(ReferenceError::Modified)?
    .map_err(ReferenceError::Failed)
}

fn prepare(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<HierarchyAnswer, ReferenceError> {
    let cursor = cursor(host, session_root, root, file, offset)?;
    let builtin = cursor.builtin;
    let found = if !cursor.types.is_empty() {
        cursor.types
    } else if builtin.is_some() {
        Vec::new()
    } else {
        cursor.enclosing.into_iter().collect()
    };
    with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        if found.is_empty()
            && let Some((name, at)) = builtin
        {
            match builtin_item(unit, root, file, name, at, &mut answer.texts) {
                Some(item) => answer.items.push(item),
                None => answer.gaps.push(Gap::of_referent(unreadable(file))),
            }
        }
        for found in &found {
            match class_item(unit, root, &found.path, None, &mut answer.texts) {
                Some(item) => answer.items.push(item),
                None => answer.gaps.push(Gap::of_referent(unreadable(&found.path))),
            }
        }
        answer
    })
}

/// The item of the built-in type `name`, which `file` writes at `at`.
fn builtin_item(
    unit: &HierarchyUnit<'_>,
    root: &Path,
    file: &Path,
    name: &str,
    at: Span,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<TypeItem> {
    let known = builtin_type(name)?;
    if !texts.contains_key(file) {
        texts.insert(file.to_path_buf(), unit.read(file)?);
    }
    Some(TypeItem {
        key: TypeKey {
            root: root.to_path_buf(),
            path: file.to_path_buf(),
            name: known.name.to_string(),
            form: Form::Builtin,
            anchor: Some(at),
        },
        kind: item_kind(known.kind),
        detail: Some("built-in type, no source file".to_string()),
        path: file.to_path_buf(),
        span: at,
        selection: at,
    })
}

fn supertypes(
    host: &SessionHost,
    session_root: &Path,
    key: &TypeKey,
) -> Result<HierarchyAnswer, ReferenceError> {
    with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        let refs = match &key.form {
            Form::Unresolved(_) => return answer,
            Form::Class => {
                let facts = unit.facts(&key.path);
                match facts.class(&IndexName::new(&key.name)) {
                    Some(class) if facts.parsed => {
                        let mut named =
                            supertypes_of(&class.descriptor, &*unit.resolver(&key.path));
                        implicit_object(&class.descriptor, &mut named);
                        if class.descriptor.kind == ClassKind::Enum {
                            answer.gaps.push(Gap::of_referent(format!(
                                "`{}` is an enum and extends a built-in enum type that oxabl \
                                 does not list",
                                key.name
                            )));
                        }
                        named
                    }
                    _ => {
                        answer.gaps.push(Gap::of_referent(format!(
                            "the header of `{}` cannot be read, so its supertypes are not known",
                            key.name
                        )));
                        return answer;
                    }
                }
            }
            Form::Builtin => match builtin_type(&key.name) {
                Some(known) => {
                    answer.gaps.push(Gap::of_referent(format!(
                        "oxabl lists the supertypes that its table of built-in types gives \
                         `{}`: the list can be short",
                        key.name
                    )));
                    known
                        .supertypes
                        .iter()
                        .map(|(relation, name)| SupertypeRef {
                            written: IndexName::new(name),
                            relation: *relation,
                            resolved: match builtin_type(name) {
                                Some(parent) => Supertype::Builtin {
                                    name: parent.name,
                                    kind: parent.kind,
                                },
                                None => Supertype::Unresolved(Unresolved::NotFound),
                            },
                        })
                        .collect()
                }
                None => {
                    answer.gaps.push(Gap::of_referent(format!(
                        "oxabl does not know the supertypes of the built-in type `{}`",
                        key.name
                    )));
                    return answer;
                }
            },
        };
        let Some(anchor) = place_anchor(unit, key, &mut answer.texts) else {
            answer.gaps.push(Gap::of_referent(unreadable(&key.path)));
            return answer;
        };
        for supertype in &refs {
            match supertype_item(unit, key, &anchor, supertype, &mut answer.texts) {
                Some(item) => answer.items.push(item),
                None => answer.gaps.push(Gap::of_referent(format!(
                    "the file of `{}` cannot be read, so it is not shown",
                    supertype.written.as_written()
                ))),
            }
        }
        answer
    })
}

/// The item of one supertype, placed at its own file or, for a type with no
/// file, at `anchor`, the class that names it.
fn supertype_item(
    unit: &HierarchyUnit<'_>,
    from: &TypeKey,
    anchor: &Anchor,
    supertype: &SupertypeRef,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<TypeItem> {
    let detail = Some(supertype.relation.keyword().to_string());
    let placed = |name: &str, form: Form, kind: ItemKind, detail: Option<String>| TypeItem {
        key: TypeKey {
            root: from.root.clone(),
            path: from.path.clone(),
            name: name.to_string(),
            form,
            anchor: from.anchor,
        },
        kind,
        detail,
        path: from.path.clone(),
        span: anchor.span,
        selection: anchor.selection,
    };
    match &supertype.resolved {
        Supertype::Class { file, .. } => {
            let path = unit.path_of(*file)?;
            class_item(unit, &from.root, &path, detail, texts)
        }
        Supertype::Builtin { name, kind } => Some(placed(
            name,
            Form::Builtin,
            item_kind(*kind),
            Some(format!(
                "{} (built-in type, no source file)",
                supertype.relation.keyword()
            )),
        )),
        Supertype::DotNet => {
            let reason = "a .NET type: oxabl does not read assemblies".to_string();
            Some(placed(
                supertype.written.as_written(),
                Form::Unresolved(reason.clone()),
                ItemKind::Unresolved,
                Some(format!("unresolved: {reason}")),
            ))
        }
        Supertype::Unresolved(why) => {
            let reason = why.describe().to_string();
            Some(placed(
                supertype.written.as_written(),
                Form::Unresolved(reason.clone()),
                ItemKind::Unresolved,
                Some(format!("unresolved: {reason}")),
            ))
        }
    }
}

/// Where the item of a type that has no file is placed: the class that names
/// it, or for a built-in type that the cursor named, the name there.
struct Anchor {
    span: Span,
    selection: Span,
}

fn place_anchor(
    unit: &HierarchyUnit<'_>,
    key: &TypeKey,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<Anchor> {
    if let Some(at) = key.anchor {
        if !texts.contains_key(&key.path) {
            texts.insert(key.path.clone(), unit.read(&key.path)?);
        }
        return Some(Anchor {
            span: at,
            selection: at,
        });
    }
    place_class(unit, &key.path, texts).map(|decl| Anchor {
        span: decl.span,
        selection: decl.selection,
    })
}

/// Every class inherits `Progress.Lang.Object` when its header names no
/// parent: add it as the parent.
fn implicit_object(class: &ClassDescriptor, named: &mut Vec<SupertypeRef>) {
    if class.kind != ClassKind::Class
        || named
            .iter()
            .any(|supertype| supertype.relation == Relation::Inherits)
    {
        return;
    }
    if let Some(object) = builtin_type(OBJECT) {
        named.insert(
            0,
            SupertypeRef {
                written: IndexName::new(object.name),
                relation: Relation::Inherits,
                resolved: Supertype::Builtin {
                    name: object.name,
                    kind: object.kind,
                },
            },
        );
    }
}

const OBJECT: &str = "Progress.Lang.Object";

fn is_object(name: &str) -> bool {
    name.eq_ignore_ascii_case(OBJECT)
}

fn subtypes(
    host: &SessionHost,
    session_root: &Path,
    key: &TypeKey,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<HierarchyAnswer, ReferenceError> {
    if matches!(key.form, Form::Unresolved(_)) {
        return Ok(HierarchyAnswer::default());
    }
    let found = class_files(host, session_root, cancel, progress)?;
    let mut gaps = Vec::new();
    let (mut answer, mut pass_gaps) = with_index(
        host,
        session_root,
        found,
        cancel,
        progress,
        |unit, index| {
            let mut answer = HierarchyAnswer::default();
            let target = match key.form {
                Form::Builtin => {
                    gaps.push(Gap::of_referent(format!(
                        "only the classes of the workspace are listed: the built-in types that \
                         extend `{}` are not",
                        key.name
                    )));
                    SuperKey::Builtin(IndexName::new(&key.name))
                }
                _ => {
                    let facts = unit.facts(&key.path);
                    if !facts.parsed {
                        gaps.push(Gap::of_referent(format!(
                            "the header of `{}` cannot be read, so its subtypes are not known",
                            key.name
                        )));
                    }
                    SuperKey::Class(IndexName::new(&key.name), facts.file)
                }
            };
            let mut direct: Vec<(&ClassNode, Relation)> = index.direct_subtypes(&target);
            if key.form == Form::Builtin && is_object(&key.name) {
                // A class with no parent in its header extends `Progress.Lang.Object`.
                direct.extend(index.nodes().iter().filter_map(|node| {
                    let no_parent = node.kind == ClassKind::Class
                        && !node
                            .supertypes
                            .iter()
                            .any(|up| up.relation == Relation::Inherits);
                    no_parent.then_some((node, Relation::Inherits))
                }));
            }
            if direct.len() > MAX_LISTED {
                gaps.push(Gap::of_referent(too_many(&key.name, direct.len())));
                direct.truncate(MAX_LISTED);
            }
            for (node, relation) in direct {
                check_cancel(cancel)?;
                match node_item(unit, &key.root, node, Some(relation), &mut answer.texts) {
                    Some(item) => answer.items.push(item),
                    None => answer.gaps.push(Gap::of_referent(unreadable(&node.path))),
                }
            }
            answer
                .gaps
                .extend(index_gaps(index, &[IndexName::new(&key.name)]));
            Ok(answer)
        },
    )?;
    pass_gaps.append(&mut gaps);
    pass_gaps.append(&mut answer.gaps);
    answer.gaps = pass_gaps;
    Ok(answer)
}

fn implementation(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<HierarchyAnswer, ReferenceError> {
    let cursor = cursor(host, session_root, root, file, offset)?;
    match cursor.methods {
        MethodsAt::Found(method) => {
            implementations_of_method(host, session_root, &method, cancel, progress)
        }
        MethodsAt::Unsupported(reason) => Ok(HierarchyAnswer {
            gaps: vec![Gap::of_referent(reason)],
            ..HierarchyAnswer::default()
        }),
        MethodsAt::None if !cursor.types.is_empty() => {
            let subjects = cursor.types.iter().map(Subject::Class).collect();
            implementations_below(host, session_root, subjects, cancel, progress)
        }
        MethodsAt::None => match cursor.builtin {
            Some((name, _)) => implementations_below(
                host,
                session_root,
                vec![Subject::Builtin(name)],
                cancel,
                progress,
            ),
            None => Ok(HierarchyAnswer {
                gaps: vec![Gap::of_referent(
                    "oxabl finds no class or method of the workspace at the cursor: a built-in \
                     member and a name that does not resolve have no implementations that it \
                     lists"
                        .to_string(),
                )],
                ..HierarchyAnswer::default()
            }),
        },
    }
}

/// A class or interface whose implementations are asked for.
enum Subject<'a> {
    Class(&'a oxabl_pipeline::TypeAt),
    Builtin(&'static str),
}

/// The classes that extend or implement each of `subjects`.
fn implementations_below(
    host: &SessionHost,
    session_root: &Path,
    subjects: Vec<Subject<'_>>,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<HierarchyAnswer, ReferenceError> {
    let found = class_files(host, session_root, cancel, progress)?;
    let mut gaps = Vec::new();
    let (mut answer, mut pass_gaps) = with_index(
        host,
        session_root,
        found,
        cancel,
        progress,
        |unit, index| {
            let mut answer = HierarchyAnswer::default();
            let mut visited: Vec<IndexName> = Vec::new();
            for subject in &subjects {
                let name: String;
                let below: Vec<&ClassNode> = match subject {
                    Subject::Class(found) => {
                        let facts = unit.facts(&found.path);
                        let Some(class) = facts.classes.first().filter(|_| facts.parsed) else {
                            answer.gaps.push(Gap::of_referent(unreadable(&found.path)));
                            continue;
                        };
                        name = class.descriptor.name.as_written().to_string();
                        visited.push(class.descriptor.name.clone());
                        index.all_subtypes(&SuperKey::Class(
                            class.descriptor.name.clone(),
                            facts.file,
                        ))
                    }
                    Subject::Builtin(builtin) => {
                        answer.gaps.push(Gap::of_referent(format!(
                            "only the classes of the workspace are listed: the built-in types \
                             that extend `{builtin}` are not"
                        )));
                        name = (*builtin).to_string();
                        visited.push(IndexName::new(builtin));
                        if is_object(builtin) {
                            index.nodes().iter().collect()
                        } else {
                            index.all_subtypes(&SuperKey::Builtin(IndexName::new(builtin)))
                        }
                    }
                };
                let classes = below
                    .iter()
                    .filter(|node| node.kind == ClassKind::Class)
                    .count();
                let mut placed = 0;
                for node in below {
                    visited.push(node.name.clone());
                    if node.kind != ClassKind::Class {
                        continue;
                    }
                    if placed == MAX_LISTED {
                        answer.gaps.push(Gap::of_referent(too_many(&name, classes)));
                        break;
                    }
                    check_cancel(cancel)?;
                    placed += 1;
                    match place_name(unit, node, &mut answer.texts) {
                        Some((_, selection)) => answer.locations.push(Target {
                            path: node.path.clone(),
                            span: Some(selection),
                        }),
                        None => answer.gaps.push(Gap::of_referent(unreadable(&node.path))),
                    }
                }
            }
            answer.gaps.extend(index_gaps(index, &visited));
            Ok(answer)
        },
    )?;
    pass_gaps.append(&mut gaps);
    pass_gaps.append(&mut answer.gaps);
    answer.gaps = pass_gaps;
    sort_locations(&mut answer.locations);
    Ok(answer)
}

/// The classes below the class of `method` that declare a method of the same
/// name and parameters.
fn implementations_of_method(
    host: &SessionHost,
    session_root: &Path,
    method: &MethodAt,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<HierarchyAnswer, ReferenceError> {
    let found = class_files(host, session_root, cancel, progress)?;
    let mut gaps = found.gaps.clone();
    // The class of the method can say at once that nothing overrides it. That
    // needs no index, so a request of that kind never waits for one.
    let known = with_snapshot(host, session_root, |unit| {
        let facts = unit.facts(&method.class_path);
        let class = facts.classes.first().filter(|_| facts.parsed)?;
        let (wanted, notes) = wanted_methods(&class.members, method);
        (!wanted.iter().any(|member| overridable(member))).then_some(notes)
    })?;
    if let Some(notes) = known {
        gaps.extend(notes.into_iter().map(Gap::of_referent));
        return Ok(HierarchyAnswer {
            gaps,
            ..HierarchyAnswer::default()
        });
    }
    let mut gaps = Vec::new();
    let (mut answer, mut pass_gaps) = with_index(
        host,
        session_root,
        found,
        cancel,
        progress,
        |unit, index| {
            let mut answer = HierarchyAnswer::default();
            let facts = unit.facts(&method.class_path);
            let Some(class) = facts.classes.first().filter(|_| facts.parsed) else {
                answer
                    .gaps
                    .push(Gap::of_referent(unreadable(&method.class_path)));
                return Ok(answer);
            };
            let (wanted, mut notes) = wanted_methods(&class.members, method);
            answer.gaps.extend(notes.drain(..).map(Gap::of_referent));
            let wanted: Vec<&MemberDescriptor> = wanted
                .into_iter()
                .filter(|member| overridable(member))
                .collect();
            if wanted.is_empty() {
                // A private or static method has no override.
                return Ok(answer);
            }
            let target = SuperKey::Class(class.descriptor.name.clone(), facts.file);
            let resolver = unit.resolver(&method.class_path);
            let records = match index.class(&class.descriptor.name, facts.file) {
                Some(node) if has_like(&wanted) => closure_records(index, node),
                _ => RecordNames::unknown(),
            };
            let wanted_in = Declaring::new(&class.descriptor.imports, &*resolver, &records);
            let below = overrides_below(
                index,
                unit,
                &target,
                class.descriptor.kind == ClassKind::Interface,
                &wanted,
                &wanted_in,
            );
            let mut visited = vec![class.descriptor.name.clone()];
            visited.extend(
                index
                    .all_subtypes(&target)
                    .iter()
                    .map(|node| node.name.clone()),
            );
            answer
                .gaps
                .extend(below.gaps.into_iter().map(Gap::of_referent));
            for found in &below.found {
                check_cancel(cancel)?;
                match method_spans(
                    unit,
                    &found.class.path,
                    &found.class.name,
                    &method.name,
                    &found.ordinals,
                    found.total,
                    &mut answer.texts,
                ) {
                    Some((spans, gap)) => {
                        answer.gaps.extend(gap.map(Gap::of_referent));
                        answer
                            .locations
                            .extend(spans.into_iter().map(|span| Target {
                                path: found.class.path.clone(),
                                span: Some(span),
                            }));
                    }
                    None => answer
                        .gaps
                        .push(Gap::of_referent(unreadable(&found.class.path))),
                }
            }
            answer.gaps.extend(index_gaps(index, &visited));
            Ok(answer)
        },
    )?;
    pass_gaps.append(&mut gaps);
    pass_gaps.append(&mut answer.gaps);
    answer.gaps = pass_gaps;
    sort_locations(&mut answer.locations);
    Ok(answer)
}

/// Whether the member at byte `offset` of `file` can override or implement a
/// member of a supertype, so that calls through the supertype reach it. Fail
/// closed: a method whose supertypes oxabl cannot fully read, and a property of
/// a class with a parent or an interface, say yes.
pub fn may_override(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<bool, ReferenceError> {
    may_override_each(host, session_root, root, file, &[offset])?
        .pop()
        .unwrap_or(Ok(true))
}

/// [`may_override`] for the member at each of `offsets` of `file`, with one
/// analysis of `root` for all of them. An `Err` of the whole is one of that
/// analysis.
pub fn may_override_each(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offsets: &[u32],
) -> Result<Vec<Result<bool, ReferenceError>>, ReferenceError> {
    let cursors = cursors(host, session_root, root, file, offsets)?;
    Ok(cursors
        .into_iter()
        .map(|cursor| may_override_at(host, session_root, cursor))
        .collect())
}

fn may_override_at(
    host: &SessionHost,
    session_root: &Path,
    cursor: Cursor,
) -> Result<bool, ReferenceError> {
    let enclosing = cursor.enclosing.clone();
    let answer = super_method_at(host, session_root, cursor)?;
    if answer.not_a_method.is_none() {
        return Ok(!answer.locations.is_empty() || !answer.gaps.is_empty());
    }
    // Not a method: a property. It has no super-member lookup, so a class
    // with a parent or an interface counts.
    let Some(class) = enclosing else {
        return Ok(true);
    };
    with_snapshot(host, session_root, |unit| {
        let facts = unit.facts(&class.path);
        facts
            .classes
            .first()
            .filter(|_| facts.parsed)
            .is_none_or(|class| {
                class.descriptor.inherits.is_some() || !class.descriptor.implements.is_empty()
            })
    })
}

fn super_method(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<HierarchyAnswer, ReferenceError> {
    let cursor = cursor(host, session_root, root, file, offset)?;
    super_method_at(host, session_root, cursor)
}

fn super_method_at(
    host: &SessionHost,
    session_root: &Path,
    cursor: Cursor,
) -> Result<HierarchyAnswer, ReferenceError> {
    let method = match cursor.methods {
        MethodsAt::Found(method) => method,
        MethodsAt::Unsupported(reason) => {
            return Ok(HierarchyAnswer {
                gaps: vec![Gap::of_referent(reason)],
                ..HierarchyAnswer::default()
            });
        }
        MethodsAt::None => {
            return Ok(HierarchyAnswer {
                not_a_method: Some(
                    "oxabl finds no method at the cursor: the name is not one that it \
                     resolves to a method of a class"
                        .to_string(),
                ),
                ..HierarchyAnswer::default()
            });
        }
    };
    with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        let facts = unit.facts(&method.class_path);
        let Some(class) = facts.classes.first().filter(|_| facts.parsed) else {
            answer
                .gaps
                .push(Gap::of_referent(unreadable(&method.class_path)));
            return answer;
        };
        let (wanted, notes) = wanted_methods(&class.members, &method);
        answer.gaps.extend(notes.into_iter().map(Gap::of_referent));
        let wanted: Vec<&MemberDescriptor> = wanted
            .into_iter()
            .filter(|member| overridable(member))
            .collect();
        if wanted.is_empty() {
            return answer;
        }
        let found = super_methods(
            unit,
            &method.class_path,
            &class.descriptor,
            &class.members,
            &wanted,
        );
        answer
            .gaps
            .extend(found.gaps.into_iter().map(Gap::of_referent));
        for matched in &found.found {
            let Some(path) = unit.path_of(matched.file) else {
                answer.gaps.push(Gap::of_referent(format!(
                    "the file of `{}` is not known",
                    matched.class.as_written()
                )));
                continue;
            };
            match method_spans(
                unit,
                &path,
                &matched.class,
                &method.name,
                &matched.ordinals,
                matched.total,
                &mut answer.texts,
            ) {
                Some((spans, gap)) => {
                    answer.gaps.extend(gap.map(Gap::of_referent));
                    answer
                        .locations
                        .extend(spans.into_iter().map(|span| Target {
                            path: path.clone(),
                            span: Some(span),
                        }));
                }
                None => answer.gaps.push(Gap::of_referent(unreadable(&path))),
            }
        }
        sort_locations(&mut answer.locations);
        answer
    })
}

/// The overloads the cursor means, as facts. When the class file and its
/// facts do not list the same number of methods of the name (an include or
/// an `&IF` adds some), the overloads cannot be told apart and all of them
/// count.
fn wanted_methods<'m>(
    members: &'m [MemberDescriptor],
    method: &MethodAt,
) -> (Vec<&'m MemberDescriptor>, Vec<String>) {
    let name = IndexName::new(&method.name);
    let declared = methods_named(members, &name);
    if declared.len() == method.total {
        let picked = method
            .ordinals
            .iter()
            .filter_map(|at| declared.get(*at).copied())
            .collect();
        (picked, Vec::new())
    } else {
        (
            declared,
            vec![format!(
                "the overloads of `{}` could not be told apart: each counts",
                method.name
            )],
        )
    }
}

/// The name spans of the methods `ordinals` of `name` of the class `class`
/// in the class file `path`, and why they are not exact. When the outline
/// lists more methods of the name than the facts, every method of the name is
/// shown; when it lists fewer, the class name is shown with them.
fn method_spans(
    unit: &HierarchyUnit<'_>,
    path: &Path,
    class: &IndexName,
    name: &str,
    ordinals: &[usize],
    total: usize,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<(Vec<Span>, Option<String>)> {
    let decl = place_class(unit, path, texts)?;
    let all: Vec<Span> = decl.methods_named(name).map(|m| m.selection).collect();
    if all.len() == total {
        let picked = ordinals
            .iter()
            .filter_map(|at| all.get(*at).copied())
            .collect();
        Some((picked, None))
    } else if all.is_empty() {
        Some((
            vec![decl.selection],
            Some(format!(
                "oxabl cannot place `{name}` in `{}` (an include or a preprocessor name writes \
                 it): the class is shown",
                class.as_written()
            )),
        ))
    } else if all.len() < total {
        // The match may be one the outline does not list.
        let mut spans = all;
        spans.push(decl.selection);
        Some((
            spans,
            Some(format!(
                "oxabl cannot place every `{name}` in `{}` (an include or a preprocessor name \
                 writes one): each it places is shown, and the class",
                class.as_written()
            )),
        ))
    } else {
        Some((
            all,
            Some(format!(
                "the overloads of `{name}` in `{}` could not be told apart: each is shown",
                class.as_written()
            )),
        ))
    }
}

/// The class files of the workspace, from the workspace pass, and what the
/// pass can miss.
struct FoundClasses {
    /// Why the list can be short, as gaps of the answer.
    gaps: Vec<Gap>,
    /// The graph the files come from, or `None` when no pass gave one.
    graph: Option<Arc<ReverseGraph>>,
}

/// The class files of the workspace, from the workspace pass, and what the
/// pass can miss. Waits for a current pass, and tells `progress` while it
/// waits.
fn class_files(
    host: &SessionHost,
    session_root: &Path,
    cancel: &AtomicBool,
    progress: &dyn Progress,
) -> Result<FoundClasses, ReferenceError> {
    check_cancel(cancel)?;
    let graph = graph_or_cancel_waiting(host, session_root, cancel, &|| {
        if let Some((indexed, total)) = pass_counts(host, session_root) {
            progress.waiting_for_pass(indexed, total);
        }
    })?;
    class_files_of(host, session_root, graph, cancel)
}

fn check_cancel(cancel: &AtomicBool) -> Result<(), ReferenceError> {
    if cancel.load(Ordering::Relaxed) {
        Err(ReferenceError::Cancelled)
    } else {
        Ok(())
    }
}

/// What a workspace pass says about the class files, and why the list can be
/// short: the pass is not current, a file could not be analysed, the pass
/// failed, or the list was cut.
fn class_files_of(
    host: &SessionHost,
    session_root: &Path,
    graph: GraphAnswer,
    cancel: &AtomicBool,
) -> Result<FoundClasses, ReferenceError> {
    let mut gaps = Vec::new();
    let mut kept = None;
    let mut count = 0;
    match graph {
        Ok(Ok(graph)) => {
            gaps.extend(graph.gaps.into_iter().map(Gap::of_referent));
            let unanalysed = graph.graph.unanalysed().len();
            if unanalysed > 0 {
                gaps.push(Gap::standing(format!(
                    "{unanalysed} file(s) of the workspace could not be analysed: each can \
                     hold a subtype"
                )));
            }
            count = class_list(host, session_root, &graph.graph).count;
            kept = Some(graph.graph);
        }
        Ok(Err(reason)) => gaps.push(Gap::of_referent(format!(
            "the classes of the workspace are not searched: {reason}"
        ))),
        Err(error) => gaps.push(Gap::of_referent(format!(
            "the classes of the workspace are not searched: {error}"
        ))),
    }
    check_cancel(cancel)?;
    if count > MAX_CLASSES {
        gaps.push(Gap::of_referent(format!(
            "the workspace has {count} class files and the index reads the first {MAX_CLASSES}"
        )));
    }
    Ok(FoundClasses { gaps, graph: kept })
}

/// How many times a request takes the graph and asks for the index before it
/// gives up: an edit stops a build, and a request that keeps meeting edits
/// must end.
const MAX_ATTEMPTS: u32 = 5;

/// Run `answer` over the subtype index of the workspace and a unit of the
/// state that the index describes. Gives the answer and the gaps of the pass
/// whose class files the index read.
///
/// The index is the one the session keeps, or one that the request waits for
/// (see `crate::subtypes`). Each attempt after the first takes the graph again,
/// so a request does not ask for an index of a pass that a newer one replaced.
/// With no graph from a pass, only the open buffers are classes, and the index
/// is built here and not kept.
fn with_index<T>(
    host: &SessionHost,
    session_root: &Path,
    first: FoundClasses,
    cancel: &AtomicBool,
    progress: &dyn Progress,
    answer: impl FnOnce(&HierarchyUnit<'_>, &SubtypeIndex) -> Result<T, ReferenceError>,
) -> Result<(T, Vec<Gap>), ReferenceError> {
    let mut found = first;
    for attempt in 0..MAX_ATTEMPTS {
        if attempt > 0 {
            found = class_files(host, session_root, cancel, progress)?;
        }
        let Some(graph) = &found.graph else {
            let answered = with_snapshot(host, session_root, |unit| {
                let none = ClassList::empty();
                let files = none.with_open_classes(unit, session_root);
                let index = unit
                    .subtype_index(&files, &|| cancel.load(Ordering::Relaxed), &|| {})
                    .ok_or(ReferenceError::Cancelled)?;
                answer(unit, &index)
            })??;
            return Ok((answered, found.gaps));
        };
        let Some(ready) = index_for(host, session_root, graph, cancel, progress)? else {
            continue;
        };
        let answered = with_hierarchy(&ready.captured.snapshot, ready.captured.overlay, |unit| {
            answer(unit, &ready.built.index)
        })
        .ok_or(ReferenceError::Modified)??;
        return Ok((answered, found.gaps));
    }
    Err(ReferenceError::Modified)
}

pub(crate) fn is_class_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("cls"))
}

/// What the reverse index can miss for the classes in `visited`.
///
/// A header that names a supertype oxabl cannot resolve, and spells it like a
/// visited class, can extend that class: a gap of this answer. Any other such
/// header, and any class file oxabl cannot read, can hold a subtype too: a
/// standing gap.
fn index_gaps(index: &SubtypeIndex, visited: &[IndexName]) -> Vec<Gap> {
    let mut gaps = Vec::new();
    let mut spelled: BTreeMap<String, Vec<(&ClassNode, &IndexName, Unresolved)>> = BTreeMap::new();
    let mut others = 0;
    let mut first_other = None;
    let names: Vec<_> = visited
        .iter()
        .map(|name| (simple_name(name.as_str()), name))
        .collect();
    for header in index.unresolved_headers() {
        let simple = simple_name(header.written.as_str());
        match names.iter().find(|(known, _)| *known == simple) {
            Some((_, class)) => spelled
                .entry(class.as_written().to_string())
                .or_default()
                .push((header.class, header.written, header.why)),
            None => {
                others += 1;
                first_other.get_or_insert((header.class, header.written, header.why));
            }
        }
    }
    for (class, headers) in &spelled {
        let (first, written, why) = headers[0];
        gaps.push(Gap::of_referent(if headers.len() == 1 {
            format!(
                "`{}` names `{}`, which oxabl cannot resolve ({}): it may extend `{class}`",
                first.name.as_written(),
                written.as_written(),
                why.describe()
            )
        } else {
            format!(
                "{} classes name a supertype spelled like `{class}` that oxabl cannot \
                 resolve, such as `{}` naming `{}` ({}): any may extend it",
                headers.len(),
                first.name.as_written(),
                written.as_written(),
                why.describe()
            )
        }));
    }
    if let Some((class, written, why)) = first_other {
        gaps.push(Gap::standing(format!(
            "{others} supertype name(s) in the workspace do not resolve, such as `{}` naming \
             `{}` ({}): a subtype that extends one of them can be missed",
            class.name.as_written(),
            written.as_written(),
            why.describe()
        )));
    }
    if let Some(first) = index.unreadable().first() {
        gaps.push(Gap::standing(format!(
            "{} class file(s) could not be read, such as {}: each can hold a subtype",
            index.unreadable().len(),
            first.display()
        )));
    }
    gaps
}

fn unreadable(path: &Path) -> String {
    format!("{} cannot be read, so it is not shown", path.display())
}

fn item_kind(kind: ClassKind) -> ItemKind {
    match kind {
        ClassKind::Class => ItemKind::Class,
        ClassKind::Interface => ItemKind::Interface,
        ClassKind::Enum => ItemKind::Enum,
    }
}

/// The declaration of the class in the class file `path`. The text it was
/// read from goes into `texts`.
fn place_class(
    unit: &HierarchyUnit<'_>,
    path: &Path,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<ClassDecl> {
    let text = match texts.get(path) {
        Some(text) => Arc::clone(text),
        None => {
            let text = unit.read(path)?;
            texts.insert(path.to_path_buf(), Arc::clone(&text));
            text
        }
    };
    class_decl(&text)
}

/// The item of the class that the class file `path` declares.
fn class_item(
    unit: &HierarchyUnit<'_>,
    root: &Path,
    path: &Path,
    detail: Option<String>,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<TypeItem> {
    let facts = unit.facts(path);
    let descriptor: &Arc<ClassDescriptor> = facts
        .classes
        .first()
        .filter(|_| facts.parsed)
        .map(|class| &class.descriptor)?;
    let decl = place_class(unit, path, texts)?;
    Some(TypeItem {
        key: TypeKey {
            root: root.to_path_buf(),
            path: path.to_path_buf(),
            name: descriptor.name.as_written().to_string(),
            form: Form::Class,
            anchor: None,
        },
        kind: item_kind(descriptor.kind),
        detail,
        path: path.to_path_buf(),
        span: decl.span,
        selection: decl.selection,
    })
}

/// The item of a class of the reverse index.
fn node_item(
    unit: &HierarchyUnit<'_>,
    root: &Path,
    node: &ClassNode,
    relation: Option<Relation>,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<TypeItem> {
    let (span, selection) = place_name(unit, node, texts)?;
    Some(TypeItem {
        key: TypeKey {
            root: root.to_path_buf(),
            path: node.path.clone(),
            name: node.name.as_written().to_string(),
            form: Form::Class,
            anchor: None,
        },
        kind: item_kind(node.kind),
        detail: relation.map(|relation| relation.keyword().to_string()),
        path: node.path.clone(),
        span,
        selection,
    })
}

/// Where the class of a node of the index is written: the span of its name
/// from the head of its file, which the index has already read, and for a
/// header that the head does not show plainly, the whole declaration and name
/// from the outline.
fn place_name(
    unit: &HierarchyUnit<'_>,
    node: &ClassNode,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<(Span, Span)> {
    let text = match texts.get(&node.path) {
        Some(text) => Arc::clone(text),
        None => {
            let text = unit.read(&node.path)?;
            texts.insert(node.path.clone(), Arc::clone(&text));
            text
        }
    };
    if let Some(name) = node.name_span {
        return Some((name, name));
    }
    match class_name_span(&text, node.name.as_written()) {
        Some(name) => Some((name, name)),
        None => class_decl(&text).map(|decl| (decl.span, decl.selection)),
    }
}

fn too_many(name: &str, found: usize) -> String {
    format!(
        "`{name}` has {found} classes below it and the answer lists the first {MAX_LISTED}: \
         the list is cut"
    )
}

fn sort_locations(locations: &mut Vec<Target>) {
    locations.sort_by(|a, b| {
        (&a.path, a.span.map(|s| s.start)).cmp(&(&b.path, b.span.map(|s| s.start)))
    });
    locations.dedup();
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use oxabl_pipeline::{LintPipeline, PipelineConfig, ReverseGraph};
    use oxabl_workspace::InMemoryFileSystem;

    use super::*;
    use crate::methods::ReferenceGraph;

    fn graph_with(gaps: &[&str]) -> GraphAnswer {
        let config = PipelineConfig::default();
        let fs = InMemoryFileSystem::new();
        let empty = ReverseGraph::build(&LintPipeline::new(&config, &fs), &[]);
        Ok(Ok(ReferenceGraph {
            graph: Arc::new(empty),
            gaps: gaps.iter().map(|gap| gap.to_string()).collect(),
        }))
    }

    fn reasons(gaps: &[Gap]) -> Vec<&str> {
        gaps.iter().map(|gap| gap.reason.as_str()).collect()
    }

    #[test]
    fn a_pass_that_is_not_current_is_a_gap_of_the_answer() {
        let cancel = AtomicBool::new(false);
        let found = class_files_of(
            &SessionHost::new(),
            Path::new("/no-session"),
            graph_with(&["the workspace index is not ready: 1 of 2 files are indexed"]),
            &cancel,
        )
        .unwrap();
        assert_eq!(
            reasons(&found.gaps),
            ["the workspace index is not ready: 1 of 2 files are indexed"]
        );
    }

    #[test]
    fn a_pass_that_cannot_run_is_a_gap_and_not_an_empty_workspace() {
        let cancel = AtomicBool::new(false);
        let found = class_files_of(
            &SessionHost::new(),
            Path::new("/no-session"),
            Ok(Err("no include path".to_string())),
            &cancel,
        )
        .unwrap();
        assert!(found.graph.is_none());
        assert_eq!(
            reasons(&found.gaps),
            ["the classes of the workspace are not searched: no include path"]
        );
    }

    #[test]
    fn a_current_pass_has_no_gap() {
        let cancel = AtomicBool::new(false);
        let found = class_files_of(
            &SessionHost::new(),
            Path::new("/no-session"),
            graph_with(&[]),
            &cancel,
        )
        .unwrap();
        assert!(found.gaps.is_empty());
    }
}
