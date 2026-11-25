//! Go to implementation, go to super method and the type hierarchy across a
//! workspace. See `docs/design/type-hierarchy.md`.
//!
//! The unit that asks is analysed first, with go to definition, to find the
//! class or method at the cursor. The rest reads class files: the facts of
//! each file are memoized per file by the daemon, so a request reads again
//! only a file that changed, and it builds the reverse inheritance index
//! ([`SubtypeIndex`]) from them. Open buffers are read over the disk.
//!
//! An answer that can be short says why in [`HierarchyAnswer::gaps`]. A
//! supertype that oxabl cannot resolve is an item of its own, never left out.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use oxabl_index::hierarchy::{
    ClassNode, Relation, SubtypeIndex, SuperKey, Supertype, SupertypeRef, Unresolved, builtin_type,
    methods_named, overridable, overrides_below, simple_name, super_methods, supertypes_of,
};
use oxabl_pipeline::{ClassDecl, Gap, MethodAt, MethodsAt, Span, Target, class_decl};
use oxabl_semantic::{ClassDescriptor, ClassKind, IndexName, MemberDescriptor};

use crate::SessionHost;
use crate::db::{HierarchyUnit, with_hierarchy};
use crate::references::{ReferenceError, analyse, graph_or_cancel};

/// The most class files the reverse index reads. A workspace with more is
/// cut, and the answer says so.
pub const MAX_CLASSES: usize = 20_000;

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
    pub texts: HashMap<PathBuf, Arc<str>>,
}

/// Answer `query` for the unit `root` of the workspace at `session_root`.
///
/// Blocks until a workspace pass is current when the query reads the
/// subtypes, so callers keep it off the LSP thread. Stops with
/// [`ReferenceError::Cancelled`] once `cancel` is set.
pub fn answer_hierarchy(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    query: &HierarchyQuery,
    cancel: &AtomicBool,
) -> Result<HierarchyAnswer, ReferenceError> {
    match query {
        HierarchyQuery::Prepare { file, offset } => {
            prepare(host, session_root, root, file, *offset)
        }
        HierarchyQuery::Supertypes(key) => supertypes(host, session_root, key),
        HierarchyQuery::Subtypes(key) => subtypes(host, session_root, key, cancel),
        HierarchyQuery::Implementation { file, offset } => {
            implementation(host, session_root, root, file, *offset, cancel)
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
}

fn cursor(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<Cursor, ReferenceError> {
    analyse(host, session_root, root, |unit| {
        let file = unit.spelled(file);
        Cursor {
            types: unit.navigation.types_at(&file, offset),
            enclosing: unit.navigation.enclosing_type(&file, offset),
            methods: unit.navigation.methods_at(&file, offset),
        }
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
    let found = if cursor.types.is_empty() {
        cursor.enclosing.into_iter().collect()
    } else {
        cursor.types
    };
    with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        for found in &found {
            match class_item(unit, root, &found.path, None, &mut answer.texts) {
                Some(item) => answer.items.push(item),
                None => answer.gaps.push(Gap::of_referent(unreadable(&found.path))),
            }
        }
        answer
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
                    Some(class) if facts.parsed => supertypes_of(&class.descriptor, unit.index()),
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
        let Some(anchor) = place_class(unit, &key.path, &mut answer.texts) else {
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
    anchor: &ClassDecl,
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

fn subtypes(
    host: &SessionHost,
    session_root: &Path,
    key: &TypeKey,
    cancel: &AtomicBool,
) -> Result<HierarchyAnswer, ReferenceError> {
    if matches!(key.form, Form::Unresolved(_)) {
        return Ok(HierarchyAnswer::default());
    }
    let (files, mut gaps) = class_files(host, session_root, cancel)?;
    let mut answer = with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        let index = unit.subtype_index(&files);
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
        for (node, relation) in index.direct_subtypes(&target) {
            match node_item(unit, &key.root, node, Some(relation), &mut answer.texts) {
                Some(item) => answer.items.push(item),
                None => answer.gaps.push(Gap::of_referent(unreadable(&node.path))),
            }
        }
        answer
            .gaps
            .extend(index_gaps(&index, &[IndexName::new(&key.name)]));
        answer
    })?;
    gaps.append(&mut answer.gaps);
    answer.gaps = gaps;
    Ok(answer)
}

fn implementation(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
    cancel: &AtomicBool,
) -> Result<HierarchyAnswer, ReferenceError> {
    let cursor = cursor(host, session_root, root, file, offset)?;
    match cursor.methods {
        MethodsAt::Found(method) => implementations_of_method(host, session_root, &method, cancel),
        MethodsAt::Unsupported(reason) => Ok(HierarchyAnswer {
            gaps: vec![Gap::of_referent(reason)],
            ..HierarchyAnswer::default()
        }),
        MethodsAt::None if cursor.types.is_empty() => Ok(HierarchyAnswer::default()),
        MethodsAt::None => implementations_of_types(host, session_root, &cursor.types, cancel),
    }
}

/// The classes that extend or implement each of `types`.
fn implementations_of_types(
    host: &SessionHost,
    session_root: &Path,
    types: &[oxabl_pipeline::TypeAt],
    cancel: &AtomicBool,
) -> Result<HierarchyAnswer, ReferenceError> {
    let (files, mut gaps) = class_files(host, session_root, cancel)?;
    let mut answer = with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        let index = unit.subtype_index(&files);
        let mut visited: Vec<IndexName> = Vec::new();
        for found in types {
            let facts = unit.facts(&found.path);
            let Some(class) = facts.classes.first().filter(|_| facts.parsed) else {
                answer.gaps.push(Gap::of_referent(unreadable(&found.path)));
                continue;
            };
            visited.push(class.descriptor.name.clone());
            let target = SuperKey::Class(class.descriptor.name.clone(), facts.file);
            for node in index.all_subtypes(&target) {
                visited.push(node.name.clone());
                if node.kind != ClassKind::Class {
                    continue;
                }
                match place_class(unit, &node.path, &mut answer.texts) {
                    Some(decl) => answer.locations.push(Target {
                        path: node.path.clone(),
                        span: Some(decl.selection),
                    }),
                    None => answer.gaps.push(Gap::of_referent(unreadable(&node.path))),
                }
            }
        }
        answer.gaps.extend(index_gaps(&index, &visited));
        answer
    })?;
    gaps.append(&mut answer.gaps);
    answer.gaps = gaps;
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
) -> Result<HierarchyAnswer, ReferenceError> {
    let (files, mut gaps) = class_files(host, session_root, cancel)?;
    let mut answer = with_snapshot(host, session_root, |unit| {
        let mut answer = HierarchyAnswer::default();
        let facts = unit.facts(&method.class_path);
        let Some(class) = facts.classes.first().filter(|_| facts.parsed) else {
            answer
                .gaps
                .push(Gap::of_referent(unreadable(&method.class_path)));
            return answer;
        };
        let (wanted, mut notes) = wanted_methods(&class.members, method);
        answer.gaps.extend(notes.drain(..).map(Gap::of_referent));
        let wanted: Vec<&MemberDescriptor> = wanted
            .into_iter()
            .filter(|member| overridable(member))
            .collect();
        if wanted.is_empty() {
            // A private or static method has no override.
            return answer;
        }
        let index = unit.subtype_index(&files);
        let target = SuperKey::Class(class.descriptor.name.clone(), facts.file);
        let below = overrides_below(&index, &target, &wanted);
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
            match method_spans(
                unit,
                &found.class.path,
                &method.name,
                &found.ordinals,
                found.total,
                &mut answer.texts,
            ) {
                Some((spans, exact)) => {
                    if !exact {
                        answer.gaps.push(Gap::of_referent(format!(
                            "the overloads of `{}` in `{}` could not be told apart: each is shown",
                            method.name,
                            found.class.name.as_written()
                        )));
                    }
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
        answer.gaps.extend(index_gaps(&index, &visited));
        answer
    })?;
    gaps.append(&mut answer.gaps);
    answer.gaps = gaps;
    sort_locations(&mut answer.locations);
    Ok(answer)
}

fn super_method(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    file: &Path,
    offset: u32,
) -> Result<HierarchyAnswer, ReferenceError> {
    let cursor = cursor(host, session_root, root, file, offset)?;
    let method = match cursor.methods {
        MethodsAt::Found(method) => method,
        MethodsAt::Unsupported(reason) => {
            return Ok(HierarchyAnswer {
                gaps: vec![Gap::of_referent(reason)],
                ..HierarchyAnswer::default()
            });
        }
        MethodsAt::None => return Ok(HierarchyAnswer::default()),
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
        let found = super_methods(unit.index(), &class.descriptor, &wanted);
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
                &method.name,
                &matched.ordinals,
                matched.total,
                &mut answer.texts,
            ) {
                Some((spans, exact)) => {
                    if !exact {
                        answer.gaps.push(Gap::of_referent(format!(
                            "the overloads of `{}` in `{}` could not be told apart: each is shown",
                            method.name,
                            matched.class.as_written()
                        )));
                    }
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

/// The name spans of the methods `ordinals` of `name` in the class file
/// `path`, and whether they are exact. When the file writes another number
/// of methods of the name than the facts list, every method of the name is
/// shown and the answer is not exact.
fn method_spans(
    unit: &HierarchyUnit<'_>,
    path: &Path,
    name: &str,
    ordinals: &[usize],
    total: usize,
    texts: &mut HashMap<PathBuf, Arc<str>>,
) -> Option<(Vec<Span>, bool)> {
    let decl = place_class(unit, path, texts)?;
    let all: Vec<Span> = decl.methods_named(name).map(|m| m.selection).collect();
    if all.len() == total {
        let picked = ordinals
            .iter()
            .filter_map(|at| all.get(*at).copied())
            .collect();
        Some((picked, true))
    } else {
        Some((all, false))
    }
}

/// The class files of the workspace, from the workspace pass, and what the
/// pass can miss. Waits for a current pass.
fn class_files(
    host: &SessionHost,
    session_root: &Path,
    cancel: &AtomicBool,
) -> Result<(Vec<PathBuf>, Vec<Gap>), ReferenceError> {
    let cancelled = || {
        if cancel.load(Ordering::Relaxed) {
            Err(ReferenceError::Cancelled)
        } else {
            Ok(())
        }
    };
    cancelled()?;
    let mut gaps = Vec::new();
    let mut files = Vec::new();
    match graph_or_cancel(host, session_root, cancel)? {
        Ok(Ok(graph)) => {
            gaps.extend(graph.gaps.into_iter().map(Gap::of_referent));
            let unanalysed = graph.graph.unanalysed().len();
            if unanalysed > 0 {
                gaps.push(Gap::standing(format!(
                    "{unanalysed} file(s) of the workspace could not be analysed: each can \
                     hold a subtype"
                )));
            }
            files = graph
                .graph
                .tracked_files()
                .into_iter()
                .filter(|path| is_class_file(path))
                .collect();
        }
        Ok(Err(reason)) => gaps.push(Gap::of_referent(format!(
            "the classes of the workspace are not searched: {reason}"
        ))),
        Err(error) => gaps.push(Gap::of_referent(format!(
            "the classes of the workspace are not searched: {error}"
        ))),
    }
    cancelled()?;
    if files.len() > MAX_CLASSES {
        gaps.push(Gap::of_referent(format!(
            "the workspace has {} class files and the index reads the first {MAX_CLASSES}",
            files.len()
        )));
        files.truncate(MAX_CLASSES);
    }
    Ok((files, gaps))
}

fn is_class_file(path: &Path) -> bool {
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
    let decl = place_class(unit, &node.path, texts)?;
    Some(TypeItem {
        key: TypeKey {
            root: root.to_path_buf(),
            path: node.path.clone(),
            name: node.name.as_written().to_string(),
            form: Form::Class,
        },
        kind: item_kind(node.kind),
        detail: relation.map(|relation| relation.keyword().to_string()),
        path: node.path.clone(),
        span: decl.span,
        selection: decl.selection,
    })
}

fn sort_locations(locations: &mut Vec<Target>) {
    locations.sort_by(|a, b| {
        (&a.path, a.span.map(|s| s.start)).cmp(&(&b.path, b.span.map(|s| s.start)))
    });
    locations.dedup();
}
