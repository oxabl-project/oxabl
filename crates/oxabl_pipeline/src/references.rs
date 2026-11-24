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

use std::collections::{BTreeSet, VecDeque};
use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_common::VirtualSpan;
use oxabl_index::EdgeKind;
use oxabl_index::calls::{CallKind, CallSite, CallTarget, UnknownCall};
use oxabl_index::search;
use oxabl_semantic::SiteTarget;
use oxabl_workspace::is_include_fragment;

use crate::navigate::Key;
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
    /// Calls of a routine of the referent's name that go through a handle, a
    /// super procedure or an object whose class is not known. Any of them can
    /// call the referent.
    pub unresolved_calls: usize,
}

/// What an item of the call hierarchy is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

/// The other files that can name a referent, and why that list can be short.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Search {
    /// Root files to analyse, sorted, without the unit that asked.
    pub units: Vec<PathBuf>,
    /// Why the answer can miss a reference. Empty when it cannot.
    pub gaps: Vec<String>,
}

impl Referent {
    /// Whether only the analysed unit can name it.
    pub fn is_local(&self) -> bool {
        self.reach.is_empty()
    }

    /// The units other than `origin` that can name the referent, by the
    /// workspace dependency graph, at most `limit` of them.
    pub fn search(&self, graph: &ReverseGraph, origin: &Path, limit: usize) -> Search {
        let origin = search::normalize_lexically(origin);
        let mut units: BTreeSet<PathBuf> = BTreeSet::new();
        let mut unresolved = 0usize;
        let mut add = |subject: &Subject, kind: EdgeKind, units: &mut BTreeSet<PathBuf>| {
            let dependents = graph.dependents(subject);
            let mut found = Vec::new();
            for dependent in dependents.of_kind(kind) {
                if units.insert(dependent.file.clone()) {
                    found.push(dependent.file.clone());
                }
            }
            unresolved += dependents
                .unresolved()
                .iter()
                .filter(|row| row.reference.kind == kind)
                .count();
            found
        };
        for reach in &self.reach {
            // The declaring file can name it too: a recursive call, a local one.
            let own = match reach {
                Reach::Program(file)
                | Reach::Routine { program: file }
                | Reach::Class(file)
                | Reach::Member { class: file } => search::normalize_lexically(file),
            };
            if !is_include_fragment(&own) {
                units.insert(own);
            }
            match reach {
                Reach::Program(program) | Reach::Routine { program } => {
                    add(
                        &Subject::file(program),
                        EdgeKind::ProgramReference,
                        &mut units,
                    );
                }
                Reach::Class(class) => {
                    add(&Subject::file(class), EdgeKind::ClassReference, &mut units);
                }
                Reach::Member { class } => {
                    // A class that names the class can inherit the member, and
                    // so can a class that names that one.
                    let mut seen = BTreeSet::from([search::normalize_lexically(class)]);
                    let mut queue = VecDeque::from([search::normalize_lexically(class)]);
                    while let Some(class) = queue.pop_front() {
                        let found =
                            add(&Subject::file(&class), EdgeKind::ClassReference, &mut units);
                        for file in found {
                            if is_class_file(&file) && seen.insert(file.clone()) {
                                queue.push_back(file);
                            }
                        }
                    }
                }
            }
        }
        units.remove(&origin);
        let mut gaps = Vec::new();
        let found = units.len();
        let mut units: Vec<PathBuf> = units.into_iter().collect();
        if found > limit {
            units.truncate(limit);
            gaps.push(format!(
                "the search stopped after {limit} of the {found} files that can name `{}`",
                self.name
            ));
        }
        if unresolved > 0 {
            gaps.push(format!(
                "{unresolved} reference(s) in other files that may name `{}` did not resolve",
                self.name
            ));
        }
        let unanalysed = graph.unanalysed().len();
        if unanalysed > 0 {
            gaps.push(format!(
                "{unanalysed} file(s) of the workspace could not be analysed"
            ));
        }
        let degraded = graph.degraded().len();
        if degraded > 0 {
            gaps.push(format!(
                "{degraded} file(s) of the workspace have an incomplete dependency set"
            ));
        }
        Search { units, gaps }
    }
}

impl CallItem {
    /// Whether two items name one thing.
    fn same(&self, other: &CallItem) -> bool {
        self.path == other.path
            && self.selection == other.selection
            && self.name.eq_ignore_ascii_case(&other.name)
            && self.kind == other.kind
    }
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
        for key in keys {
            let (named, reached) = match &key {
                Key::Undeclared { program, procedure } => (
                    procedure.clone(),
                    Some(Reach::Routine {
                        program: program.clone(),
                    }),
                ),
                Key::At(Target { path, span: None }) => {
                    if is_include_fragment(path) {
                        continue;
                    }
                    (file_name(path), Some(Reach::Program(path.clone())))
                }
                Key::At(Target {
                    path,
                    span: Some(span),
                }) => self.classify(path, *span),
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
        })
    }

    /// The name declared at `span` of `path`, and which other files can name
    /// it.
    fn classify(&self, path: &Path, span: Span) -> (String, Option<Reach>) {
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
        let Some((symbol, in_class)) = symbol else {
            return (name, None);
        };
        let reach = match symbol.kind {
            OutlineKind::Procedure | OutlineKind::Function
                if !in_class && !is_include_fragment(path) && !is_class_file(path) =>
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
            OutlineKind::Method
            | OutlineKind::Property
            | OutlineKind::Event
            | OutlineKind::Variable
            | OutlineKind::TempTable
            | OutlineKind::Buffer
            | OutlineKind::Dataset
            | OutlineKind::DataSource
                if in_class && is_class_file(path) =>
            {
                Some(Reach::Member {
                    class: path.to_path_buf(),
                })
            }
            _ => None,
        };
        (name, reach)
    }

    /// Every place in this unit that names `referent`: the uses, and the
    /// declarations this unit holds.
    pub fn references(&self, referent: &Referent) -> UnitReferences {
        let occurrences = self.occurrences(referent);
        let unresolved_calls = self.unresolved_calls(referent, &occurrences);
        UnitReferences {
            occurrences,
            unresolved_calls,
        }
    }

    fn occurrences(&self, referent: &Referent) -> Vec<Occurrence> {
        let keys = &referent.keys;
        let symbols = &self.semantic.symbols;
        let mut found: Vec<Occurrence> = Vec::new();
        for site in symbols.sites() {
            let Some(named) = self.site_keys(&site.target, site.span) else {
                continue;
            };
            if !named.iter().any(|key| keys.contains(key)) {
                continue;
            }
            let span = self.narrow(site.span, &referent.name);
            if let Some(place) = self.place(span) {
                let declaration = keys.contains(&Key::At(place.clone()));
                found.push(Occurrence {
                    place,
                    declaration,
                    expanded: span,
                });
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
        found
    }

    /// The name `name` within a site that is a call or a member access
    /// (`fn(x)`, `obj:Method(x)`), so a reference shows the name and not the
    /// whole expression. Any other site is kept whole.
    fn narrow(&self, span: VirtualSpan, name: &str) -> VirtualSpan {
        let Some(text) = self.text(span) else {
            return span;
        };
        if name.is_empty() || !(text.contains(':') || text.contains('(')) {
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
        let reasons: &[UnknownCall] = if referent
            .reach
            .iter()
            .any(|reach| matches!(reach, Reach::Member { .. }))
        {
            &[UnknownCall::Method]
        } else if referent
            .reach
            .iter()
            .any(|reach| matches!(reach, Reach::Routine { .. }))
        {
            &[UnknownCall::Handle, UnknownCall::Super]
        } else {
            return 0;
        };
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
                !occurrences.iter().any(|occurrence| {
                    site.at.start <= occurrence.expanded.start
                        && occurrence.expanded.end <= site.at.end
                })
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
            match calls.iter_mut().find(|call| call.from.same(&caller)) {
                Some(call) => {
                    if !call.spans.contains(&span) {
                        call.spans.push(span);
                    }
                }
                None => calls.push(IncomingCall {
                    from: caller,
                    spans: vec![span],
                }),
            }
        }
        UnitIncoming {
            calls,
            unresolved_calls: references.unresolved_calls,
        }
    }

    /// The calls `item` makes. `Err` when this unit no longer declares it.
    pub fn outgoing_calls(&self, item: &CallItem) -> Result<Outgoing, String> {
        if item.unresolved.is_some() {
            return Ok(Outgoing::default());
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
        for site in &calls.sites {
            let enclosing = self.enclosing(site.at.start);
            if enclosing.map(|r| r.span) != routine.map(|r| r.span) {
                continue;
            }
            let (keys, shown) = self.call_keys(site);
            let mut callees: Vec<CallItem> = Vec::new();
            for key in &keys {
                let item = self.item_for(key).unwrap_or_else(|| self.other_item(key));
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
                continue;
            };
            for callee in callees {
                match out.iter_mut().find(|call| call.to.same(&callee)) {
                    Some(call) => {
                        if !call.spans.contains(&span) {
                            call.spans.push(span);
                        }
                    }
                    None => out.push(OutgoingCall {
                        to: callee,
                        spans: vec![span],
                    }),
                }
            }
        }
        Ok(Outgoing {
            calls: out,
            truncated: calls.truncated,
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
                let named = self.semantic.symbols.sites().iter().find(|named| {
                    matches!(named.target, SiteTarget::Run(_))
                        && site.at.start <= named.span.start
                        && named.span.end <= site.at.end
                });
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
    fn routines(&self) -> &[OutlineSymbol] {
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
        })
    }

    /// The innermost routine of the expanded unit that holds `virt`.
    fn enclosing(&self, virt: u32) -> Option<&OutlineSymbol> {
        self.routines()
            .iter()
            .filter(|routine| routine.span.start <= virt && virt < routine.span.end)
            .min_by_key(|routine| routine.span.end - routine.span.start)
    }

    /// The bytes of `file` that show the expanded span `virt`: the span
    /// itself when `file` writes it, else the `{...}` reference in `file`
    /// through which it arrived.
    fn span_in(&self, file: &Path, virt: VirtualSpan) -> Option<Span> {
        let place = self.place(virt)?;
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

/// The symbol whose name is at `span`, at any depth, and whether a class or
/// interface holds it.
fn find_selected(symbols: &[OutlineSymbol], span: Span) -> Option<(&OutlineSymbol, bool)> {
    let mut stack: Vec<(&OutlineSymbol, bool)> = symbols.iter().map(|s| (s, false)).collect();
    while let Some((symbol, in_class)) = stack.pop() {
        if symbol.selection_span == span {
            return Some((symbol, in_class));
        }
        let class = matches!(symbol.kind, OutlineKind::Class | OutlineKind::Interface);
        stack.extend(
            symbol
                .children
                .iter()
                .map(|child| (child, in_class || class)),
        );
    }
    None
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

fn is_class_file(path: &Path) -> bool {
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
