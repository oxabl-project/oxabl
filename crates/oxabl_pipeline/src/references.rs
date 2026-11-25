//! Find references and the call hierarchy.
//!
//! Both start from what go to definition finds at the cursor
//! ([`Navigation::referent_at`]): a reference is any name whose definition is
//! the same declaration, file or undeclared procedure. One compilation unit
//! answers for itself ([`Navigation::references`],
//! [`Navigation::incoming_calls`], [`Navigation::outgoing_calls`]). Which other
//! units can name the same thing comes from the workspace dependency graph
//! ([`Referent::search`]): the files that `RUN` a program, and the files that
//! name a class or, for a member, a class that can inherit it.
//!
//! An answer that can be short says why in its gaps. It is never shown as a
//! complete answer.

use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_common::VirtualSpan;
use oxabl_index::EdgeKind;
use oxabl_index::calls::{CallKind, CallSite, CallTarget, UnknownCall};
use oxabl_index::search;
use oxabl_semantic::{SiteTarget, SymbolFlags};
use oxabl_workspace::is_include_fragment;

use crate::navigate::{Key, is_member_kind};
use crate::{Navigation, OutlineKind, OutlineSymbol, ReverseGraph, Subject, Target};

/// What a references or call hierarchy request is about: every declaration,
/// file or undeclared procedure the name at the cursor refers to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Referent {
    keys: Vec<Key>,
    /// The name as declared, or the file name of a program.
    pub name: String,
    /// Where other files can name it. Empty when only the analysed unit can.
    pub reach: Vec<Reach>,
    /// What the unit that found it knows the search will miss.
    pub gaps: Vec<Gap>,
    /// Whether every use writes the name in full: a variable, a routine or a
    /// member, not a program path, a class name or a field or table name
    /// (which can be abbreviated).
    whole_name: bool,
}

/// Why an answer can miss something.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gap {
    pub reason: String,
    /// True for a gap that every request in the workspace has (the state of
    /// the index, a limit of the analysis), false for one of this referent.
    pub standing: bool,
    /// Set for a gap that one unit gives, so [`fold_gaps`] can join the gaps
    /// of many units into one.
    fold: Option<Fold>,
}

/// What a unit gap says, and in which file, for [`fold_gaps`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Fold {
    kind: FoldKind,
    /// The name of the file, for the text.
    file: String,
    /// What tells the files apart: one file in several units is one.
    path: PathBuf,
    count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FoldKind {
    ParseErrors,
    Unmodelled(String),
    Unresolved(String),
    Unplaced(String),
}

impl Gap {
    /// A gap of this referent or answer.
    pub fn of_referent(reason: String) -> Gap {
        Gap {
            reason,
            standing: false,
            fold: None,
        }
    }

    /// A gap that every request in the workspace has.
    pub fn standing(reason: String) -> Gap {
        Gap {
            reason,
            standing: true,
            fold: None,
        }
    }

    /// A gap of one file. `count` is how many names or places the file has.
    fn of_file(kind: FoldKind, path: PathBuf, count: usize) -> Gap {
        let fold = Fold {
            kind,
            file: file_name(&path),
            path,
            count,
        };
        Gap {
            reason: fold.reason(1),
            standing: false,
            fold: Some(fold),
        }
    }
}

impl Fold {
    /// The text for this gap when it covers `files` files; the file is one
    /// example.
    fn reason(&self, files: usize) -> String {
        let (file, count) = (&self.file, self.count);
        match (&self.kind, files) {
            (FoldKind::ParseErrors, 1) => {
                format!("{file} has parse errors: uses in the damaged statements can be missed")
            }
            (FoldKind::ParseErrors, _) => format!(
                "{files} file(s) have parse errors, such as {file}: uses in the damaged \
                 statements can be missed"
            ),
            (FoldKind::Unmodelled(name), 1) => format!(
                "`{name}` is named in a statement of {file} that oxabl does not model: uses \
                 there can be missed"
            ),
            (FoldKind::Unmodelled(name), _) => format!(
                "`{name}` is named in a statement that oxabl does not model in {files} \
                 file(s), such as {file}: uses there can be missed"
            ),
            (FoldKind::Unresolved(name), 1) => format!(
                "{count} name(s) `{name}` in {file} did not resolve, and any of them can mean it"
            ),
            (FoldKind::Unresolved(name), _) => format!(
                "{count} name(s) `{name}` in {files} file(s), such as {file}, did not \
                 resolve, and any of them can mean it"
            ),
            (FoldKind::Unplaced(name), 1) => format!(
                "{count} place(s) in {file} that name `{name}` come from text no file writes"
            ),
            (FoldKind::Unplaced(name), _) => format!(
                "{count} place(s) in {files} file(s), such as {file}, that name `{name}` \
                 come from text no file writes"
            ),
        }
    }
}

/// Join the gaps that many units give for one thing into one gap that counts
/// the files and names one, so the text of an answer does not grow with the
/// number of files. Other gaps stay as they are.
pub fn fold_gaps(gaps: Vec<Gap>) -> Vec<Gap> {
    let mut out: Vec<Gap> = Vec::new();
    let mut folded: Vec<(usize, Fold, Vec<PathBuf>)> = Vec::new();
    for gap in gaps {
        let Some(fold) = &gap.fold else {
            out.push(gap);
            continue;
        };
        match folded
            .iter_mut()
            .find(|(_, held, _)| held.kind == fold.kind)
        {
            Some((_, held, files)) => {
                if !files.contains(&fold.path) {
                    held.count += fold.count;
                    files.push(fold.path.clone());
                }
            }
            None => {
                folded.push((out.len(), fold.clone(), vec![fold.path.clone()]));
                out.push(gap);
            }
        }
    }
    for (at, fold, files) in folded {
        out[at] = Gap {
            reason: fold.reason(files.len()),
            standing: false,
            fold: None,
        };
    }
    out
}

/// Which other files can name a referent.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reach {
    /// A program: the files that `RUN` it.
    Program(PathBuf),
    /// An internal procedure or user-defined function of a program: the files
    /// that `RUN` the program, through a handle it sets.
    Routine { program: PathBuf },
    /// A class: the files that name it.
    Class(PathBuf),
    /// A member of a class: the files that name the class or a class that
    /// names it, which can inherit the member.
    Member { class: PathBuf },
    /// A declaration in an include file: each unit that expands the include,
    /// and for a routine or a member, the files that reach it there.
    Included { include: PathBuf, kind: OutlineKind },
    /// A database table or one of its fields: the files that read the table.
    Table(String),
}

/// One place that names a referent.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Occurrence {
    /// The bytes of the name, in the file that writes it.
    pub place: Target,
    /// Whether this is a declaration of the referent rather than a use.
    pub declaration: bool,
    expanded: VirtualSpan,
}

/// The references one unit holds.
#[derive(Debug, Clone, Default)]
pub struct UnitReferences {
    pub occurrences: Vec<Occurrence>,
    /// What this unit's answer can miss.
    pub gaps: Vec<Gap>,
    /// Calls of a routine of the referent's name that go through a handle, a
    /// super procedure or an object whose class is not known. Any of them can
    /// call the referent.
    pub unresolved_calls: usize,
}

/// What an item of the call hierarchy is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallItemKind {
    Program,
    Class,
    Procedure,
    Function,
    Method,
    Constructor,
    Destructor,
    Property,
    /// An `ON` trigger block.
    Trigger,
    /// A routine of a shared library (`PROCEDURE p EXTERNAL`).
    External,
    /// A call whose target oxabl does not know.
    Unresolved,
    /// A resolved target that is none of the above.
    Other,
}

/// One item of the call hierarchy: a routine, a program, or a call that does
/// not resolve.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallItem {
    pub name: String,
    pub kind: CallItemKind,
    pub detail: Option<String>,
    /// The file that writes the item.
    pub path: PathBuf,
    /// The whole item, in the bytes of `path`.
    pub span: Span,
    /// The name, in the bytes of `path`.
    pub selection: Span,
    /// The compilation unit analysed for the item's own calls: the program or
    /// class file, or for a routine written in an include, the unit it was
    /// found in.
    pub root: PathBuf,
    /// False for an internal procedure of a program whose text does not
    /// declare it. The item then stands at the start of the program.
    pub declared: bool,
    /// Why the item stands for a call oxabl cannot resolve. Such an item has
    /// no calls of its own.
    pub unresolved: Option<String>,
}

/// A caller of a call hierarchy item, and where it calls it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncomingCall {
    pub from: CallItem,
    /// In the bytes of `from.path`.
    pub spans: Vec<Span>,
}

/// A callee of a call hierarchy item, and where the item calls it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutgoingCall {
    pub to: CallItem,
    /// In the bytes of the caller's file.
    pub spans: Vec<Span>,
}

/// The callers one unit holds.
#[derive(Debug, Clone, Default)]
pub struct UnitIncoming {
    pub calls: Vec<IncomingCall>,
    /// What this unit's answer can miss.
    pub gaps: Vec<Gap>,
    /// As [`UnitReferences::unresolved_calls`].
    pub unresolved_calls: usize,
}

/// The calls of one item.
#[derive(Debug, Clone, Default)]
pub struct Outgoing {
    pub calls: Vec<OutgoingCall>,
    /// The unit has more calls than
    /// [`MAX_SITES`](oxabl_index::calls::MAX_SITES): the rest are not listed.
    pub truncated: bool,
    /// What else the answer can miss.
    pub gaps: Vec<Gap>,
}

/// The other files that can name a referent, and why that list can be short.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Search {
    /// Root files to analyse, sorted, without the unit that asked.
    pub units: Vec<PathBuf>,
    /// Why the answer can miss a reference. Empty when it cannot.
    pub gaps: Vec<Gap>,
}

impl Referent {
    /// Whether only the analysed unit can name it.
    pub fn is_local(&self) -> bool {
        self.reach.is_empty()
    }

    /// The declarations of the referent, each at its name.
    pub fn declarations(&self) -> Vec<Target> {
        self.keys
            .iter()
            .filter_map(|key| match key {
                Key::At(target) if target.span.is_some() => Some(target.clone()),
                _ => None,
            })
            .collect()
    }

    /// The units other than `origin` that can name the referent, by the
    /// workspace dependency graph, at most `limit` of them. The units that
    /// declare it come first, so a cut at `limit` keeps them.
    pub fn search(&self, graph: &ReverseGraph, origin: &Path, limit: usize) -> Search {
        let origin = search::normalize_lexically(origin);
        let mut own: Vec<PathBuf> = Vec::new();
        let mut units: BTreeSet<PathBuf> = BTreeSet::new();
        let mut unresolved = 0usize;
        let mut add = |subject: &Subject, kinds: &[EdgeKind], units: &mut BTreeSet<PathBuf>| {
            let dependents = graph.dependents(subject);
            let mut found = Vec::new();
            for dependent in dependents.all() {
                if kinds.contains(&dependent.kind) && units.insert(dependent.file.clone()) {
                    found.push(dependent.file.clone());
                }
            }
            unresolved += dependents
                .unresolved()
                .iter()
                .filter(|row| kinds.contains(&row.reference.kind))
                .count();
            found
        };
        let mut handles = false;
        let mut queue: VecDeque<Reach> = self.reach.iter().cloned().collect();
        let mut done: Vec<Reach> = Vec::new();
        while let Some(reach) = queue.pop_front() {
            if done.contains(&reach) {
                continue;
            }
            done.push(reach.clone());
            // The declaring file can name it too: a recursive call, a local one.
            if let Reach::Program(file)
            | Reach::Routine { program: file }
            | Reach::Class(file)
            | Reach::Member { class: file } = &reach
            {
                let file = search::normalize_lexically(file);
                if !own.contains(&file) {
                    own.push(file);
                }
            }
            match reach {
                Reach::Program(program) => {
                    add(
                        &Subject::file(&program),
                        &[EdgeKind::ProgramReference],
                        &mut units,
                    );
                }
                Reach::Routine { program } => {
                    handles = true;
                    add(
                        &Subject::file(&program),
                        &[EdgeKind::ProgramReference],
                        &mut units,
                    );
                }
                Reach::Class(class) => {
                    add(
                        &Subject::file(&class),
                        &[EdgeKind::ClassReference],
                        &mut units,
                    );
                }
                Reach::Member { class } => {
                    // A class that names the class can inherit the member, and
                    // so can a class that names that one.
                    let mut seen = BTreeSet::from([search::normalize_lexically(&class)]);
                    let mut classes = VecDeque::from([search::normalize_lexically(&class)]);
                    while let Some(class) = classes.pop_front() {
                        let found = add(
                            &Subject::file(&class),
                            &[EdgeKind::ClassReference],
                            &mut units,
                        );
                        for file in found {
                            if is_class_file(&file) && seen.insert(file.clone()) {
                                classes.push_back(file);
                            }
                        }
                    }
                }
                Reach::Included { include, kind } => {
                    let includers = add(
                        &Subject::file(&include),
                        &[EdgeKind::DirectInclude, EdgeKind::TransitiveInclude],
                        &mut units,
                    );
                    // A routine or member written in the include is the
                    // includer's own: search where the includer's are found.
                    for includer in includers {
                        let derived = match kind {
                            OutlineKind::Procedure | OutlineKind::Function
                                if !is_class_file(&includer) =>
                            {
                                Some(Reach::Routine { program: includer })
                            }
                            kind if is_member_kind(kind) && is_class_file(&includer) => {
                                Some(Reach::Member { class: includer })
                            }
                            _ => None,
                        };
                        queue.extend(derived);
                    }
                }
                Reach::Table(table) => {
                    add(
                        &Subject::table(&table),
                        &[EdgeKind::SchemaTable],
                        &mut units,
                    );
                }
            }
        }
        let mut ordered: Vec<PathBuf> = own
            .into_iter()
            .filter(|file| !is_include_fragment(file) && *file != origin)
            .collect();
        for unit in units {
            if unit != origin && !ordered.contains(&unit) {
                ordered.push(unit);
            }
        }
        let mut gaps = Vec::new();
        let found = ordered.len();
        if found > limit {
            ordered.truncate(limit);
            gaps.push(Gap::of_referent(format!(
                "the search stopped after {limit} of the {found} files that can name `{}`",
                self.name
            )));
        }
        if unresolved > 0 {
            gaps.push(Gap::of_referent(format!(
                "{unresolved} reference(s) in other files that may name `{}` did not resolve",
                self.name
            )));
        }
        if handles {
            gaps.push(Gap::standing(
                "a RUN ... IN a handle that a file gets from elsewhere (a parameter, a shared \
                 variable, SESSION) is not followed"
                    .to_string(),
            ));
        }
        let unanalysed = graph.unanalysed().len();
        if unanalysed > 0 {
            gaps.push(Gap::standing(format!(
                "{unanalysed} file(s) of the workspace could not be analysed"
            )));
        }
        let degraded = graph.degraded().len();
        if degraded > 0 {
            gaps.push(Gap::standing(format!(
                "{degraded} file(s) of the workspace have an incomplete dependency set"
            )));
        }
        Search {
            units: ordered,
            gaps,
        }
    }
}

/// What makes two call items one: the file, the name's bytes, the folded name
/// and the kind.
pub type ItemKey = (PathBuf, u32, u32, String, CallItemKind);

impl CallItem {
    /// Whether two items name one thing.
    fn same(&self, other: &CallItem) -> bool {
        self.key() == other.key()
    }

    /// The identity of the item, for grouping calls.
    pub fn key(&self) -> ItemKey {
        (
            self.path.clone(),
            self.selection.start,
            self.selection.end,
            self.name.to_ascii_lowercase(),
            self.kind,
        )
    }
}

/// Sort spans by position and drop repeats.
pub fn dedupe_spans(spans: &mut Vec<Span>) {
    spans.sort_unstable_by_key(|span| (span.start, span.end));
    spans.dedup();
}

impl Navigation<'_, '_> {
    /// What the name at byte `offset` of `file` refers to, for references and
    /// incoming calls. `None` when it names nothing, or names an include file.
    pub fn referent_at(&self, file: &Path, offset: u32) -> Option<Referent> {
        self.referent(self.keys_at(file, offset))
    }

    /// The referent a call hierarchy item stands for. `None` for an item of a
    /// call that does not resolve.
    pub fn referent_of(&self, item: &CallItem) -> Option<Referent> {
        if item.unresolved.is_some() {
            return None;
        }
        let path = search::normalize_lexically(&item.path);
        let key = match item.kind {
            CallItemKind::Program => Key::At(Target { path, span: None }),
            // The class as a whole: the places that name it.
            CallItemKind::Class if item.selection.end == 0 => {
                let span = self.class_span(&path);
                Key::At(Target { path, span })
            }
            CallItemKind::Procedure if !item.declared => Key::Undeclared {
                program: path,
                procedure: item.name.to_ascii_lowercase(),
            },
            _ => Key::At(Target {
                path,
                span: Some(item.selection),
            }),
        };
        self.referent(vec![key])
    }

    fn referent(&self, keys: Vec<Key>) -> Option<Referent> {
        let mut kept = Vec::new();
        let mut name = None;
        let mut reach = Vec::new();
        let mut gaps = Vec::new();
        let mut whole_name = true;
        for key in keys {
            whole_name &= match &key {
                Key::TableField { .. } | Key::At(Target { span: None, .. }) => false,
                Key::Undeclared { .. } | Key::Member { .. } => true,
                Key::At(Target {
                    path,
                    span: Some(span),
                }) => self.written_whole(path, *span),
            };
            let (named, reached) = match &key {
                Key::TableField { table, name, .. } => {
                    if let Some(span) = table.span
                        && let Some(shared) = self.shared_declared_at(&table.path, span)
                    {
                        gaps.push(Gap::of_referent(format!(
                            "`{name}` is a field of the shared temp-table `{shared}`: other \
                             programs that define it SHARED are not searched"
                        )));
                    }
                    // A temp-table of an include is each includer's own.
                    let reach = is_include_fragment(&table.path).then(|| Reach::Included {
                        include: table.path.clone(),
                        kind: OutlineKind::Field,
                    });
                    (name.clone(), reach)
                }
                Key::Undeclared { program, procedure } => (
                    procedure.clone(),
                    Some(Reach::Routine {
                        program: program.clone(),
                    }),
                ),
                Key::Member { class, name } => {
                    gaps.push(Gap::of_referent(format!(
                        "the declaration of `{name}` is not placed, and its uses in {} can \
                         be missed: neither the class nor its expansion shows it",
                        file_name(class)
                    )));
                    (
                        name.clone(),
                        Some(Reach::Member {
                            class: class.clone(),
                        }),
                    )
                }
                Key::At(Target { path, span: None }) => {
                    if is_include_fragment(path) {
                        continue;
                    }
                    if is_class_file(path) {
                        let stem = path
                            .file_stem()
                            .map(|stem| stem.to_string_lossy().into_owned())
                            .unwrap_or_default();
                        (stem, Some(Reach::Class(path.clone())))
                    } else {
                        (file_name(path), Some(Reach::Program(path.clone())))
                    }
                }
                Key::At(Target {
                    path,
                    span: Some(span),
                }) => {
                    if let Some(shared) = self.shared_declared_at(path, *span) {
                        gaps.push(Gap::of_referent(format!(
                            "`{shared}` is shared: other programs that define it SHARED are \
                             not searched"
                        )));
                    } else if let Some((field, table)) = self.shared_table_field_at(path, *span) {
                        gaps.push(Gap::of_referent(format!(
                            "`{field}` is a field of the shared temp-table `{table}`: other \
                             programs that define it SHARED are not searched"
                        )));
                    }
                    self.classify(path, *span)
                }
            };
            name.get_or_insert(named);
            if let Some(reached) = reached
                && !reach.contains(&reached)
            {
                reach.push(reached);
            }
            kept.push(key);
        }
        Some(Referent {
            keys: (!kept.is_empty()).then_some(kept)?,
            name: name?,
            reach,
            gaps,
            whole_name,
        })
    }

    /// Whether every use of the declaration at `span` of `path` writes its
    /// name in full. A field, a table, a buffer and a temp-table can be named
    /// by an abbreviation, a class by its unqualified name; so can anything
    /// the outline does not show.
    fn written_whole(&self, path: &Path, span: Span) -> bool {
        if self.schema_declared_at(path, span).is_some() {
            return false;
        }
        let outline = self.outline_of(path);
        let Some((symbol, _)) = outline
            .as_deref()
            .and_then(|symbols| find_selected(symbols, span))
        else {
            return false;
        };
        !matches!(
            symbol.kind,
            OutlineKind::Field
                | OutlineKind::TempTable
                | OutlineKind::Buffer
                | OutlineKind::Class
                | OutlineKind::Interface
                | OutlineKind::Enum
        )
    }

    /// The name of a shared variable, buffer, temp-table or other shared
    /// object this unit declares at `span` of `path`.
    fn shared_declared_at(&self, path: &Path, span: Span) -> Option<String> {
        let wanted = Target {
            path: path.to_path_buf(),
            span: Some(span),
        };
        let shared = SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED;
        self.model.semantic.symbols.iter().find_map(|(_, symbol)| {
            (symbol.declaration != oxabl_ast::NodeId::DUMMY
                && symbol.flags.intersects(shared)
                && self.place(symbol.name_span).as_ref() == Some(&wanted))
            .then(|| self.text(symbol.name_span).unwrap_or_default().to_string())
        })
    }

    /// The name of the field declared at `span` of `path` and of its
    /// temp-table, when this unit declares that temp-table shared.
    fn shared_table_field_at(&self, path: &Path, span: Span) -> Option<(String, String)> {
        let outline = self.outline_of(path)?;
        let (field, table) = field_and_table(&outline, span)?;
        let shared = SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED;
        self.model
            .semantic
            .symbols
            .iter()
            .any(|(_, symbol)| {
                symbol.declaration != oxabl_ast::NodeId::DUMMY
                    && symbol.kind == oxabl_semantic::SymbolKind::TempTable
                    && symbol.flags.intersects(shared)
                    && self
                        .text(symbol.name_span)
                        .is_some_and(|name| name.eq_ignore_ascii_case(&table.name))
            })
            .then(|| (field.name.clone(), table.name.clone()))
    }

    /// The database table whose `.df` declares the table or a field at `span`
    /// of `path`, and the name declared there.
    fn schema_declared_at(&self, path: &Path, span: Span) -> Option<(String, String)> {
        let schema = &self.run.config().schema;
        for (id, table) in schema.tables() {
            let Some((source, _)) = schema.table_source(id) else {
                continue;
            };
            if search::normalize_lexically(source) != path {
                continue;
            }
            if table.source.span == span {
                return Some((table.name.to_string(), table.name.to_string()));
            }
            if let Some(field) = table.fields.iter().find(|field| field.source.span == span) {
                return Some((table.name.to_string(), field.name.to_string()));
            }
        }
        None
    }

    /// The name declared at `span` of `path`, and which other files can name
    /// it.
    fn classify(&self, path: &Path, span: Span) -> (String, Option<Reach>) {
        if let Some((table, name)) = self.schema_declared_at(path, span) {
            return (name, Some(Reach::Table(table)));
        }
        let outline = self.outline_of(path);
        let symbol = outline
            .as_deref()
            .and_then(|symbols| find_selected(symbols, span));
        let name = match symbol {
            Some((symbol, _)) => symbol.name.clone(),
            None => self
                .run
                .file_system()
                .read(path)
                .ok()
                .and_then(|text| {
                    text.get(span.start as usize..span.end as usize)
                        .map(str::to_string)
                })
                .unwrap_or_default(),
        };
        let Some((symbol, parent)) = symbol else {
            return (name, None);
        };
        // A member is a direct child of its class; a variable of a method is not.
        let in_class = matches!(
            parent,
            Some(OutlineKind::Class | OutlineKind::Interface | OutlineKind::Enum)
        );
        let reach = if is_include_fragment(path) {
            // Only what an includer can reach by name: not a routine's own
            // variables.
            (parent.is_none() || in_class).then(|| Reach::Included {
                include: path.to_path_buf(),
                kind: symbol.kind,
            })
        } else {
            match symbol.kind {
                OutlineKind::Procedure | OutlineKind::Function
                    if parent.is_none() && !is_class_file(path) =>
                {
                    Some(Reach::Routine {
                        program: path.to_path_buf(),
                    })
                }
                OutlineKind::Class | OutlineKind::Interface | OutlineKind::Enum
                    if is_class_file(path) =>
                {
                    Some(Reach::Class(path.to_path_buf()))
                }
                kind if in_class && is_member_kind(kind) && is_class_file(path) => {
                    Some(Reach::Member {
                        class: path.to_path_buf(),
                    })
                }
                _ => None,
            }
        };
        (name, reach)
    }

    /// Every place in this unit that names `referent`: the uses, and the
    /// declarations this unit holds.
    pub fn references(&self, referent: &Referent) -> UnitReferences {
        let (occurrences, unplaced, unresolved_names) = self.occurrences(referent);
        let unresolved_calls = self.unresolved_calls(referent, &occurrences);
        let mut gaps = Vec::new();
        let root = self.root_path().unwrap_or_default();
        if unplaced > 0 {
            gaps.push(Gap::of_file(
                FoldKind::Unplaced(referent.name.clone()),
                root.clone(),
                unplaced,
            ));
        }
        if unresolved_names > 0 {
            gaps.push(Gap::of_file(
                FoldKind::Unresolved(referent.name.clone()),
                root.clone(),
                unresolved_names,
            ));
        }
        if let Some(gap) = self.recovered_gap() {
            gaps.push(gap);
        }
        let unmodelled = self.named_in_unmodelled_text(referent).or_else(|| {
            self.named_by_unmodelled_statement(referent)
                .then(|| root.clone())
        });
        if let Some(file) = unmodelled {
            gaps.push(Gap::of_file(
                FoldKind::Unmodelled(referent.name.clone()),
                file,
                1,
            ));
        }
        if referent.reach.iter().any(through_handles) && self.calls().truncated {
            gaps.push(Gap::of_referent(format!(
                "{} has more than {} calls: a call through a handle after that is not \
                 resolved",
                file_name(&root),
                oxabl_index::calls::MAX_SITES
            )));
        }
        UnitReferences {
            occurrences,
            gaps,
            unresolved_calls,
        }
    }

    /// The gap of a unit whose parse had errors: the sites of a damaged
    /// statement are not known.
    fn recovered_gap(&self) -> Option<Gap> {
        self.model
            .recovered
            .map(|at| Gap::of_file(FoldKind::ParseErrors, self.file_of_text(at), 1))
    }

    /// Whether a statement form that oxabl recognizes but does not model names
    /// the referent: its names are not sites, so the search misses them.
    fn named_by_unmodelled_statement(&self, referent: &Referent) -> bool {
        let symbols = &self.model.semantic.symbols;
        symbols.iter().any(|(id, symbol)| {
            symbol
                .flags
                .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
                && self
                    .symbol_keys(id)
                    .iter()
                    .any(|key| referent.keys.contains(key))
        })
    }

    /// The file that wrote `span` of the expanded text: an include, or the
    /// unit when no file did.
    fn file_of_text(&self, span: VirtualSpan) -> PathBuf {
        self.place_or_site(span)
            .map(|place| place.path)
            .or_else(|| self.root_path())
            .unwrap_or_default()
    }

    /// The file whose skipped text (not modelled by the parser) holds the name
    /// of the referent: the uses in it are not sites. For a referent that is
    /// not always written whole (a field, a table, a buffer), a word that is a
    /// prefix of the name, three letters or more, counts: a field can be
    /// abbreviated.
    fn named_in_unmodelled_text(&self, referent: &Referent) -> Option<PathBuf> {
        self.model
            .semantic
            .symbols
            .unmodelled_spans()
            .iter()
            .find(|span| {
                self.text(**span)
                    .is_some_and(|text| holds_name(text, &referent.name, !referent.whole_name))
            })
            .map(|span| self.file_of_text(*span))
    }

    /// The occurrences of `referent`, how many sites named it in text that
    /// has no place in a file, and how many names spelled like it did not
    /// resolve.
    fn occurrences(&self, referent: &Referent) -> (Vec<Occurrence>, usize, usize) {
        let keys = &referent.keys;
        let symbols = &self.model.semantic.symbols;
        let mut found: Vec<Occurrence> = Vec::new();
        let mut unplaced = 0;
        // Names spelled like the referent, by the place they name: whether
        // some site there resolved. A qualified name (`tt.field`) can have a
        // resolved site and an unresolved one for one place.
        let mut spelled: HashMap<(u32, u32), bool> = HashMap::new();
        for site in symbols.sites() {
            // A site whose text does not hold the name cannot name a referent
            // that is always written whole: no resolution needed.
            if referent.whole_name
                && !self
                    .text(site.span)
                    .is_some_and(|text| contains_folded(text, &referent.name))
            {
                continue;
            }
            let named = self.site_keys(&site.target, site.span);
            let resolved = named.as_ref().is_some_and(|keys| !keys.is_empty());
            // A resolved site of any kind marks its place; only a reference
            // that did not resolve can leave one unresolved.
            if (resolved || matches!(site.target, SiteTarget::Reference(_)))
                && self.spells(site.span, &referent.name)
            {
                let span = self.narrow(site.span, &referent.name);
                *spelled.entry((span.start, span.end)).or_default() |= resolved;
            }
            if !resolved {
                continue;
            }
            let Some(named) = named else {
                continue;
            };
            if !named.iter().any(|key| keys.contains(key)) {
                continue;
            }
            let span = if referent
                .reach
                .iter()
                .any(|reach| matches!(reach, Reach::Program(_)))
            {
                // A program is named by its path, which no narrowing keeps.
                site.span
            } else {
                self.narrow(site.span, &referent.name)
            };
            match self.place_or_site(span) {
                Some(place) => {
                    let declaration = keys.contains(&Key::At(place.clone()));
                    found.push(Occurrence {
                        place,
                        declaration,
                        expanded: span,
                    });
                }
                None => unplaced += 1,
            }
        }
        for (_, symbol) in symbols.iter() {
            if symbol.declaration == oxabl_ast::NodeId::DUMMY {
                continue;
            }
            if let Some(place) = self.place(symbol.name_span)
                && keys.contains(&Key::At(place.clone()))
            {
                found.push(Occurrence {
                    place,
                    declaration: true,
                    expanded: symbol.name_span,
                });
            }
        }
        found.sort_by(|a, b| {
            let key =
                |o: &Occurrence| (o.place.path.clone(), o.place.span.map(|s| (s.start, s.end)));
            key(a).cmp(&key(b)).then(b.declaration.cmp(&a.declaration))
        });
        found.dedup_by(|later, kept| later.place == kept.place);
        let unresolved = spelled.values().filter(|resolved| !**resolved).count();
        (found, unplaced, unresolved)
    }

    /// Whether the text at `span` is `name`, or a qualified name that ends in
    /// it (`x.name`, `x:name`), ignoring case.
    fn spells(&self, span: VirtualSpan, name: &str) -> bool {
        let Some(text) = self.text(span) else {
            return false;
        };
        let text = text.trim();
        let Some(at) = text.len().checked_sub(name.len()) else {
            return false;
        };
        if name.is_empty() || !text.is_char_boundary(at) {
            return false;
        }
        let (head, tail) = text.split_at(at);
        tail.eq_ignore_ascii_case(name) && (head.is_empty() || head.ends_with(['.', ':']))
    }

    /// A span of the expanded text in the file that wrote it, or for text a
    /// `{&name}` substitution spliced in, the reference in the root that brought
    /// it in.
    fn place_or_site(&self, span: VirtualSpan) -> Option<Target> {
        if let Some(place) = self.place(span) {
            return Some(place);
        }
        let site = self.expanded.resolve_include_site(Span {
            start: span.start,
            end: span.end,
        })?;
        Some(Target {
            path: self.root_path()?,
            span: Some(site),
        })
    }

    /// The name `name` within a site that is a call, a member access or a
    /// qualified name (`fn(x)`, `obj:Method(x)`, `table.field`), so a
    /// reference shows the name and not the whole expression. Any other site
    /// is kept whole.
    fn narrow(&self, span: VirtualSpan, name: &str) -> VirtualSpan {
        let Some(text) = self.text(span) else {
            return span;
        };
        if name.is_empty() || !text.contains([':', '(', '.']) {
            return span;
        }
        let bytes = text.as_bytes();
        let word = |at: usize| {
            bytes.get(at).is_some_and(|b| {
                b.is_ascii_alphanumeric() || matches!(b, b'_' | b'-' | b'#' | b'$' | b'%' | b'&')
            })
        };
        let mut first = None;
        let mut after_colon = None;
        let mut at = 0;
        while at + name.len() <= bytes.len() {
            let whole = text
                .get(at..at + name.len())
                .is_some_and(|found| found.eq_ignore_ascii_case(name))
                && (at == 0 || !word(at - 1))
                && !word(at + name.len());
            if whole {
                first.get_or_insert(at);
                if at > 0 && bytes[at - 1] == b':' && after_colon.is_none() {
                    after_colon = Some(at);
                }
            }
            at += 1;
        }
        match after_colon.or(first) {
            Some(at) => VirtualSpan {
                start: span.start + at as u32,
                end: span.start + (at + name.len()) as u32,
            },
            None => span,
        }
    }

    /// The calls in this unit of a routine named like `referent` whose target
    /// is not known and that no occurrence covers.
    fn unresolved_calls(&self, referent: &Referent, occurrences: &[Occurrence]) -> usize {
        let reasons: &[UnknownCall] = if referent.reach.iter().any(|reach| {
            matches!(reach, Reach::Member { .. })
                || matches!(reach, Reach::Included { kind, .. } if is_member_kind(*kind))
        }) {
            &[UnknownCall::Method]
        } else if referent.reach.iter().any(through_handles) {
            &[UnknownCall::Handle, UnknownCall::Super]
        } else {
            return 0;
        };
        let mut covered: Vec<(u32, u32)> = occurrences
            .iter()
            .map(|occurrence| (occurrence.expanded.start, occurrence.expanded.end))
            .collect();
        covered.sort_unstable();
        self.calls()
            .sites
            .iter()
            .filter(|site| match &site.target {
                CallTarget::Unknown {
                    name: Some(name),
                    reason,
                } => {
                    reasons.contains(reason)
                        && name
                            .trim_matches(['"', '\''])
                            .eq_ignore_ascii_case(&referent.name)
                }
                _ => false,
            })
            .filter(|site| {
                let from = covered.partition_point(|(start, _)| *start < site.at.start);
                !covered[from..]
                    .iter()
                    .take_while(|(start, _)| *start < site.at.end)
                    .any(|(_, end)| *end <= site.at.end)
            })
            .count()
    }

    /// The call hierarchy items at byte `offset` of `file`: the routines or
    /// programs the name there refers to, or else the routine that holds the
    /// offset, or else the analysed program or class itself.
    pub fn prepare_call_hierarchy(&self, file: &Path, offset: u32) -> Vec<CallItem> {
        let file = search::normalize_lexically(file);
        let mut items: Vec<CallItem> = Vec::new();
        for key in self.keys_at(&file, offset) {
            if let Some(item) = self.item_for(&key)
                && item.unresolved.is_none()
                && !items.iter().any(|known| known.same(&item))
            {
                items.push(item);
            }
        }
        if !items.is_empty() {
            return items;
        }
        let at = self.expanded_at(&file, offset).or_else(|| {
            offset
                .checked_sub(1)
                .and_then(|at| self.expanded_at(&file, at))
        });
        if let Some(virt) = at
            && let Some(routine) = self.enclosing(virt)
        {
            return self.routine_item(routine).into_iter().collect();
        }
        if self.root_path().as_deref() == Some(file.as_path()) {
            return self.unit_item().into_iter().collect();
        }
        Vec::new()
    }

    /// The callers in this unit of the thing `referent` names.
    pub fn incoming_calls(&self, referent: &Referent) -> UnitIncoming {
        let references = self.references(referent);
        let mut calls: Vec<IncomingCall> = Vec::new();
        let mut grouped: HashMap<ItemKey, usize> = HashMap::new();
        for occurrence in references.occurrences {
            if occurrence.declaration {
                continue;
            }
            let caller = match self.enclosing(occurrence.expanded.start) {
                Some(routine) => self.routine_item(routine),
                None => self.unit_item(),
            };
            let Some(caller) = caller else { continue };
            let Some(span) = self.span_in(&caller.path, occurrence.expanded) else {
                continue;
            };
            match grouped.entry(caller.key()) {
                Entry::Occupied(at) => calls[*at.get()].spans.push(span),
                Entry::Vacant(at) => {
                    at.insert(calls.len());
                    calls.push(IncomingCall {
                        from: caller,
                        spans: vec![span],
                    });
                }
            }
        }
        for call in &mut calls {
            dedupe_spans(&mut call.spans);
        }
        UnitIncoming {
            calls,
            gaps: references.gaps,
            unresolved_calls: references.unresolved_calls,
        }
    }

    /// The calls `item` makes. `Err` when this unit no longer declares it.
    pub fn outgoing_calls(&self, item: &CallItem) -> Result<Outgoing, String> {
        if item.unresolved.is_some() {
            return Ok(Outgoing::default());
        }
        if !item.declared {
            return Ok(Outgoing {
                gaps: vec![Gap::of_referent(format!(
                    "a super procedure of {} supplies `{}`: its calls are not known",
                    file_name(&item.path),
                    item.name
                ))],
                ..Outgoing::default()
            });
        }
        let root = self.root_path();
        let whole_unit = matches!(item.kind, CallItemKind::Program | CallItemKind::Class)
            && root.as_deref() == Some(search::normalize_lexically(&item.path).as_path());
        let routine = if whole_unit {
            None
        } else {
            let wanted = Target {
                path: search::normalize_lexically(&item.path),
                span: Some(item.selection),
            };
            let found = self.routines().iter().find(|routine| {
                self.place(virtual_span(routine.selection_span)).as_ref() == Some(&wanted)
            });
            match found {
                Some(routine) => Some(routine),
                None => {
                    return Err(format!(
                        "{} no longer declares `{}` there",
                        item.path.display(),
                        item.name
                    ));
                }
            }
        };
        let caller_path = search::normalize_lexically(&item.path);
        let calls = self.calls();
        let mut out: Vec<OutgoingCall> = Vec::new();
        let mut grouped: HashMap<ItemKey, usize> = HashMap::new();
        let mut items: HashMap<KeyId, CallItem> = HashMap::new();
        let mut unplaced = 0;
        for site in &calls.sites {
            let enclosing = self.enclosing(site.at.start);
            if enclosing.map(|r| r.span) != routine.map(|r| r.span) {
                continue;
            }
            let (keys, shown) = self.call_keys(site);
            let mut callees: Vec<CallItem> = Vec::new();
            for key in &keys {
                // Many calls name one callee: build its item once.
                let item = items
                    .entry(key_id(key))
                    .or_insert_with(|| self.item_for(key).unwrap_or_else(|| self.other_item(key)))
                    .clone();
                if !callees.iter().any(|known| known.same(&item)) {
                    callees.push(item);
                }
            }
            if callees.len() > 1 {
                let count = callees.len();
                for callee in &mut callees {
                    callee.detail = Some(format!(
                        "one of {count} targets the name can mean{}",
                        callee
                            .detail
                            .as_deref()
                            .map(|detail| format!(", {detail}"))
                            .unwrap_or_default()
                    ));
                }
            }
            if callees.is_empty() {
                callees.extend(self.unresolved_item(site));
            }
            let Some(span) = self.span_in(&caller_path, shown) else {
                unplaced += 1;
                continue;
            };
            for callee in callees {
                match grouped.entry(callee.key()) {
                    Entry::Occupied(at) => out[*at.get()].spans.push(span),
                    Entry::Vacant(at) => {
                        at.insert(out.len());
                        out.push(OutgoingCall {
                            to: callee,
                            spans: vec![span],
                        });
                    }
                }
            }
        }
        for call in &mut out {
            dedupe_spans(&mut call.spans);
        }
        let mut gaps: Vec<Gap> = self.recovered_gap().into_iter().collect();
        if unplaced > 0 {
            gaps.push(Gap::of_referent(format!(
                "{unplaced} call(s) of `{}` could not be placed in {}",
                item.name,
                file_name(&item.path)
            )));
        }
        Ok(Outgoing {
            calls: out,
            truncated: calls.truncated,
            gaps,
        })
    }

    /// What a call site calls, by the resolution go to definition uses, and
    /// the part of the expanded text to show for it.
    fn call_keys(&self, site: &CallSite) -> (Vec<Key>, VirtualSpan) {
        let whole = virtual_span(site.at);
        match &site.target {
            CallTarget::Unknown {
                reason:
                    UnknownCall::Dynamic
                    | UnknownCall::Event
                    | UnknownCall::Super
                    | UnknownCall::Unexpanded,
                ..
            }
            | CallTarget::Library(_) => return (Vec::new(), whole),
            _ => {}
        }
        match site.kind {
            CallKind::Run | CallKind::Persistent | CallKind::Remote => {
                let sites = self.model.semantic.symbols.sites();
                let runs = self.run_sites.get_or_init(|| {
                    let mut runs: Vec<usize> = (0..sites.len())
                        .filter(|at| matches!(sites[*at].target, SiteTarget::Run(_)))
                        .collect();
                    runs.sort_by_key(|at| sites[*at].span.start);
                    runs
                });
                let from = runs.partition_point(|at| sites[*at].span.start < site.at.start);
                let named = runs[from..]
                    .iter()
                    .map(|at| &sites[*at])
                    .take_while(|named| named.span.start < site.at.end)
                    .find(|named| named.span.end <= site.at.end);
                match named {
                    Some(named) => (
                        self.site_keys(&named.target, named.span)
                            .unwrap_or_default(),
                        named.span,
                    ),
                    None => (Vec::new(), whole),
                }
            }
            CallKind::Call => (self.keys_in_expanded(site.at.start), whole),
            CallKind::Publish => (Vec::new(), whole),
        }
    }

    /// The callee item for a key. `None` for a key that is no routine or
    /// program.
    fn item_for(&self, key: &Key) -> Option<CallItem> {
        match key {
            Key::TableField { .. } => None,
            Key::Undeclared { program, procedure } => Some(CallItem {
                name: procedure.clone(),
                kind: CallItemKind::Procedure,
                detail: Some(format!(
                    "not declared in {}: a super procedure of it can supply it",
                    file_name(program)
                )),
                path: program.clone(),
                span: Span { start: 0, end: 0 },
                selection: Span { start: 0, end: 0 },
                root: program.clone(),
                declared: false,
                unresolved: None,
            }),
            Key::At(Target { path, span: None }) => {
                if is_include_fragment(path) {
                    return None;
                }
                self.file_item(path)
            }
            Key::Member { class, name } => Some(CallItem {
                name: name.clone(),
                kind: CallItemKind::Method,
                detail: Some(format!(
                    "in {}, where its outline does not show it",
                    file_name(class)
                )),
                path: class.clone(),
                span: Span { start: 0, end: 0 },
                selection: Span { start: 0, end: 0 },
                root: class.clone(),
                declared: false,
                unresolved: None,
            }),
            Key::At(Target {
                path,
                span: Some(span),
            }) => {
                let outline = self.outline_of(path)?;
                let (symbol, _) = find_selected(&outline, *span)?;
                let kind = routine_kind(symbol.kind)?;
                let root = if is_include_fragment(path) {
                    self.root_path()?
                } else {
                    path.clone()
                };
                Some(CallItem {
                    name: symbol.name.clone(),
                    kind,
                    detail: symbol.detail.clone(),
                    path: path.clone(),
                    span: symbol.span,
                    selection: symbol.selection_span,
                    root,
                    declared: true,
                    unresolved: None,
                })
            }
        }
    }

    /// An item for a resolved callee that is no routine the outline shows.
    fn other_item(&self, key: &Key) -> CallItem {
        let target = key.clone().into_target();
        let span = target.span.unwrap_or(Span { start: 0, end: 0 });
        let name = self
            .run
            .file_system()
            .read(&target.path)
            .ok()
            .and_then(|text| {
                text.get(span.start as usize..span.end as usize)
                    .map(str::to_string)
            })
            .filter(|name| !name.is_empty())
            .unwrap_or_else(|| file_name(&target.path));
        CallItem {
            name,
            kind: CallItemKind::Other,
            detail: Some(file_name(&target.path)),
            root: target.path.clone(),
            path: target.path,
            span,
            selection: span,
            declared: true,
            unresolved: None,
        }
    }

    /// An item for a program or class file as a whole.
    fn file_item(&self, path: &Path) -> Option<CallItem> {
        let text = self.run.file_system().read(path).ok()?;
        Some(CallItem {
            name: file_name(path),
            kind: if is_class_file(path) {
                CallItemKind::Class
            } else {
                CallItemKind::Program
            },
            detail: path
                .parent()
                .map(|parent| parent.to_string_lossy().into_owned()),
            path: path.to_path_buf(),
            span: Span {
                start: 0,
                end: text.len() as u32,
            },
            selection: Span { start: 0, end: 0 },
            root: path.to_path_buf(),
            declared: true,
            unresolved: None,
        })
    }

    /// The analysed root as an item: the caller for code outside every routine.
    fn unit_item(&self) -> Option<CallItem> {
        let root = self.root_path()?;
        if is_include_fragment(&root) {
            return None;
        }
        self.file_item(&root)
    }

    /// The item for a routine of the expanded unit.
    fn routine_item(&self, routine: &OutlineSymbol) -> Option<CallItem> {
        let selection = self.place(virtual_span(routine.selection_span))?;
        let selection_span = selection.span?;
        let span = self
            .place(virtual_span(routine.span))
            .filter(|whole| whole.path == selection.path)
            .and_then(|whole| whole.span)
            .unwrap_or(selection_span);
        let root = self.root_path()?;
        Some(CallItem {
            name: routine.name.clone(),
            kind: routine_kind(routine.kind)?,
            detail: routine.detail.clone(),
            root: if is_include_fragment(&selection.path) {
                root
            } else {
                selection.path.clone()
            },
            path: selection.path,
            span,
            selection: selection_span,
            declared: true,
            unresolved: None,
        })
    }

    /// An item for a call whose target is not known.
    fn unresolved_item(&self, site: &CallSite) -> Option<CallItem> {
        let place = self.place(virtual_span(site.at))?;
        let span = place.span?;
        let (name, kind, why) = match &site.target {
            CallTarget::Library(name) => (
                name.clone(),
                CallItemKind::External,
                "a routine of a shared library".to_string(),
            ),
            CallTarget::Unknown { name, reason } => (
                name.clone().unwrap_or_else(|| self.call_text(site)),
                CallItemKind::Unresolved,
                unknown_reason(*reason).to_string(),
            ),
            CallTarget::Program(name) => (
                name.clone(),
                CallItemKind::Unresolved,
                "no file on the include path supplies it".to_string(),
            ),
            CallTarget::InProgram { procedure, .. } => (
                procedure.clone(),
                CallItemKind::Unresolved,
                "the program its handle runs is not found".to_string(),
            ),
            CallTarget::Procedure(name) | CallTarget::Function(name) | CallTarget::Method(name) => {
                (
                    name.clone(),
                    CallItemKind::Unresolved,
                    "its declaration did not resolve".to_string(),
                )
            }
        };
        Some(CallItem {
            name,
            kind,
            detail: Some(format!("unresolved: {why}")),
            root: self.root_path()?,
            path: place.path,
            span,
            selection: span,
            declared: true,
            unresolved: Some(why),
        })
    }

    /// A short text for a call that has no name: its first line.
    fn call_text(&self, site: &CallSite) -> String {
        let text = &self.expanded.text;
        let start = site.at.start as usize;
        let mut end = (site.at.end as usize).min(text.len());
        // A call site of an expression is its keyword; take its arguments too.
        if text.get(end..).is_some_and(|rest| rest.starts_with('(')) {
            let mut depth = 0i32;
            for (at, byte) in text.as_bytes()[end..].iter().enumerate().take(200) {
                match byte {
                    b'(' => depth += 1,
                    b')' => {
                        depth -= 1;
                        if depth == 0 {
                            end += at + 1;
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
        let line = text
            .get(start..end)
            .unwrap_or_default()
            .lines()
            .next()
            .unwrap_or_default()
            .trim()
            .trim_end_matches('.');
        let words: Vec<&str> = line.split_whitespace().collect();
        let mut shown = words.join(" ");
        if shown.chars().count() > 60 {
            shown = shown.chars().take(59).collect::<String>() + "…";
        }
        shown
    }

    /// The routines of the expanded unit, outlined once.
    pub(crate) fn routines(&self) -> &[OutlineSymbol] {
        &self.routine_tree().0
    }

    /// The routines sorted by start, and the parent of each.
    fn routine_tree(&self) -> &(Vec<OutlineSymbol>, Vec<Option<usize>>) {
        self.routines.get_or_init(|| {
            let mut routines = Vec::new();
            let outline = crate::outline(&self.expanded.text).unwrap_or_default();
            let mut stack: Vec<&OutlineSymbol> = outline.iter().collect();
            while let Some(symbol) = stack.pop() {
                if routine_kind(symbol.kind).is_some() {
                    let mut kept = symbol.clone();
                    kept.children = Vec::new();
                    routines.push(kept);
                }
                stack.extend(symbol.children.iter());
            }
            routines
                .sort_by_key(|routine| (routine.span.start, std::cmp::Reverse(routine.span.end)));
            let mut parents = Vec::with_capacity(routines.len());
            let mut open: Vec<usize> = Vec::new();
            for (at, routine) in routines.iter().enumerate() {
                while let Some(&last) = open.last()
                    && routines[last].span.end <= routine.span.start
                {
                    open.pop();
                }
                parents.push(open.last().copied());
                open.push(at);
            }
            (routines, parents)
        })
    }

    /// The innermost routine of the expanded unit that holds `virt`. Routines
    /// nest, so it is the last routine that starts at or before `virt`, or
    /// one that holds it.
    fn enclosing(&self, virt: u32) -> Option<&OutlineSymbol> {
        let (routines, parents) = self.routine_tree();
        let mut at = routines
            .partition_point(|routine| routine.span.start <= virt)
            .checked_sub(1);
        while let Some(index) = at {
            let routine = &routines[index];
            if virt < routine.span.end {
                return Some(routine);
            }
            at = parents[index];
        }
        None
    }

    /// The bytes of `file` that show the expanded span `virt`: the span
    /// itself when `file` writes it, else the `{...}` reference in `file`
    /// through which it arrived.
    fn span_in(&self, file: &Path, virt: VirtualSpan) -> Option<Span> {
        let place = self.place_or_site(virt)?;
        if place.path == file {
            return place.span;
        }
        let expansions = self.expanded.include_expansions();
        let mut at = expansions
            .iter()
            .enumerate()
            .filter(|(_, expansion)| {
                expansion.expanded.start <= virt.start && virt.start < expansion.expanded.end
            })
            .min_by_key(|(_, expansion)| expansion.expanded.end - expansion.expanded.start)
            .map(|(at, _)| at);
        while let Some(index) = at {
            let expansion = expansions.get(index)?;
            let parent = match expansion.parent {
                None => self.root_path()?,
                Some(parent) => search::normalize_lexically(expansions.get(parent)?.path),
            };
            if parent == file {
                return expansion.site;
            }
            at = expansion.parent;
        }
        None
    }
}

/// The call hierarchy kind of an outline kind, for the kinds that run code.
fn routine_kind(kind: OutlineKind) -> Option<CallItemKind> {
    Some(match kind {
        OutlineKind::Procedure => CallItemKind::Procedure,
        OutlineKind::Function => CallItemKind::Function,
        OutlineKind::Method => CallItemKind::Method,
        OutlineKind::Constructor => CallItemKind::Constructor,
        OutlineKind::Destructor => CallItemKind::Destructor,
        OutlineKind::Property => CallItemKind::Property,
        OutlineKind::Trigger => CallItemKind::Trigger,
        _ => return None,
    })
}

/// The symbol whose name is at `span`, at any depth, and the kind of the
/// symbol that holds it.
pub(crate) fn find_selected(
    symbols: &[OutlineSymbol],
    span: Span,
) -> Option<(&OutlineSymbol, Option<OutlineKind>)> {
    let mut stack: Vec<(&OutlineSymbol, Option<OutlineKind>)> =
        symbols.iter().map(|s| (s, None)).collect();
    while let Some((symbol, parent)) = stack.pop() {
        if symbol.selection_span == span {
            return Some((symbol, parent));
        }
        stack.extend(
            symbol
                .children
                .iter()
                .map(|child| (child, Some(symbol.kind))),
        );
    }
    None
}

/// A [`Key`] as a value a map can hold.
type KeyId = (u8, PathBuf, Option<(u32, u32)>, String);

fn key_id(key: &Key) -> KeyId {
    match key {
        Key::At(target) => (
            0,
            target.path.clone(),
            target.span.map(|span| (span.start, span.end)),
            String::new(),
        ),
        Key::Undeclared { program, procedure } => (1, program.clone(), None, procedure.clone()),
        Key::Member { class, name } => (2, class.clone(), None, name.clone()),
        Key::TableField { table, name, .. } => (
            3,
            table.path.clone(),
            table.span.map(|span| (span.start, span.end)),
            name.clone(),
        ),
    }
}

/// The field whose name is at `span`, and the temp-table that holds it.
fn field_and_table(
    symbols: &[OutlineSymbol],
    span: Span,
) -> Option<(&OutlineSymbol, &OutlineSymbol)> {
    let mut stack: Vec<&OutlineSymbol> = symbols.iter().collect();
    while let Some(symbol) = stack.pop() {
        if symbol.kind == OutlineKind::TempTable
            && let Some(field) = symbol
                .children
                .iter()
                .find(|child| child.kind == OutlineKind::Field && child.selection_span == span)
        {
            return Some((field, symbol));
        }
        stack.extend(symbol.children.iter());
    }
    None
}

/// Whether `text` holds `name`, ignoring ASCII case.
fn contains_folded(text: &str, name: &str) -> bool {
    let (text, name) = (text.as_bytes(), name.as_bytes());
    name.is_empty()
        || text
            .windows(name.len())
            .any(|window| window.eq_ignore_ascii_case(name))
}

/// Whether `text` has a word, outside string literals, that is `name` or,
/// with `abbreviated` and when `name` is one word, an abbreviation of it. A
/// name with a character that no word holds (a program path) is looked for as
/// text.
fn holds_name(text: &str, name: &str, abbreviated: bool) -> bool {
    let word_char = |c: char| c.is_alphanumeric() || matches!(c, '-' | '_' | '#' | '$' | '%' | '&');
    if !name.chars().all(word_char) {
        return contains_folded(text, name);
    }
    code_outside_strings(text).any(|code| {
        code.split(|c: char| !word_char(c)).any(|word| {
            word.eq_ignore_ascii_case(name)
                || (abbreviated
                    && word.len() >= 3
                    && name
                        .as_bytes()
                        .get(..word.len())
                        .is_some_and(|head| head.eq_ignore_ascii_case(word.as_bytes())))
        })
    })
}

/// The parts of `text` outside string literals (`"..."`, `'...'`, with `~` as
/// the escape). A quote with no closing quote opens no literal, so a stray
/// quote hides nothing.
fn code_outside_strings(text: &str) -> impl Iterator<Item = &str> {
    let bytes = text.as_bytes();
    let mut parts = Vec::new();
    let (mut start, mut at) = (0, 0);
    while at < bytes.len() {
        let quote = bytes[at];
        if quote != b'"' && quote != b'\'' {
            at += 1;
            continue;
        }
        let mut end = at + 1;
        while end < bytes.len() && bytes[end] != quote {
            end += if bytes[end] == b'~' { 2 } else { 1 };
        }
        if end >= bytes.len() {
            at += 1;
            continue;
        }
        parts.push(&text[start..at]);
        start = end + 1;
        at = start;
    }
    parts.push(&text[start..]);
    parts.into_iter()
}

/// Whether other files reach the referent through a handle.
fn through_handles(reach: &Reach) -> bool {
    matches!(reach, Reach::Routine { .. })
        || matches!(
            reach,
            Reach::Included {
                kind: OutlineKind::Procedure | OutlineKind::Function,
                ..
            }
        )
}

fn unknown_reason(reason: UnknownCall) -> &'static str {
    match reason {
        UnknownCall::Dynamic => "the target is computed at run time",
        UnknownCall::Handle => "it runs through a handle that oxabl cannot follow",
        UnknownCall::Super => "a super procedure supplies it at run time",
        UnknownCall::Event => "PUBLISH: the subscribers are known at run time",
        UnknownCall::Method => "a method of an object whose class oxabl does not know",
        UnknownCall::Unexpanded => "the name holds text the preprocessor did not expand",
        UnknownCall::NotFound => "no file on the include path supplies it",
        UnknownCall::Unusable => "the file that supplies it cannot be used",
        UnknownCall::Ambiguous => "several files supply it",
        UnknownCall::NoRoutine => "the program does not declare it",
    }
}

pub(crate) fn is_class_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("cls"))
}

fn file_name(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_default()
}

fn virtual_span(span: Span) -> VirtualSpan {
    VirtualSpan {
        start: span.start,
        end: span.end,
    }
}
