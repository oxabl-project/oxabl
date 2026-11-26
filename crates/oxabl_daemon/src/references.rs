//! Find references and the call hierarchy across a workspace.
//!
//! The unit that asks is analysed first, on its own snapshot, and gives the
//! referent and the answer within that unit. When other files can name the
//! referent ([`Referent::is_local`]), the workspace dependency graph
//! ([`reference_graph`]) names the units to search, and each is analysed on a
//! worker thread with a fresh snapshot. No snapshot is held while the
//! workspace pass runs, so an edit is never kept waiting for it.
//!
//! Every reason the answer can be short goes into [`ReferenceAnswer::gaps`]:
//! a workspace with no include path, an index that is out of date, files that
//! could not be analysed, references that did not resolve, calls through a
//! handle or an object oxabl cannot follow, and a search that stopped at
//! [`MAX_UNITS`].

use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

use oxabl_index::calls::MAX_SITES;
use oxabl_pipeline::{
    CallItem, Gap, IncomingCall, ItemKey, Occurrence, OutgoingCall, ReferenceMemo, Referent,
    Target, UnitIncoming, UnitReferences, dedupe_spans, fold_gaps,
};

use crate::SessionHost;
use crate::db::{NavigationUnit, with_navigation};
use crate::methods::{ReferenceGraph, reference_graph};

/// The most other units one request analyses.
pub const MAX_UNITS: usize = 2_000;

/// How often a unit is analysed again after an edit cancelled its analysis.
const ATTEMPTS: usize = 3;

/// What a references or call hierarchy request asks.
#[derive(Debug, Clone)]
pub enum ReferenceQuery {
    /// The places that name what the name at byte `offset` of `file` refers
    /// to. `file` is the root or an include its expansion reads.
    References {
        file: PathBuf,
        offset: u32,
        include_declaration: bool,
    },
    /// The places that run the program of the unit that asks (a root file
    /// that is neither a class nor an include). A program has no declaration.
    Program,
    /// The call hierarchy items at byte `offset` of `file`.
    Prepare { file: PathBuf, offset: u32 },
    /// The callers of an item.
    Incoming(CallItem),
    /// The callees of an item.
    Outgoing(CallItem),
}

/// The answer to a [`ReferenceQuery`], with the text of every file it places a
/// span in.
#[derive(Debug, Clone, Default)]
pub struct ReferenceAnswer {
    pub locations: Vec<Target>,
    pub items: Vec<CallItem>,
    pub incoming: Vec<IncomingCall>,
    pub outgoing: Vec<OutgoingCall>,
    /// Why the answer can miss something. Empty for a complete answer.
    pub gaps: Vec<Gap>,
    /// The calls of the workspace that no search follows and that can reach the
    /// referent, as the short text a count shows ([`Search::counts`]).
    pub count_reasons: Vec<String>,
    pub texts: HashMap<PathBuf, Arc<str>>,
    /// Whether the name or item asked about refers to something. An empty
    /// `locations` of an answer that is not `resolved` is not a count of zero.
    pub resolved: bool,
    /// Whether a later request can give another answer with no edit between:
    /// the workspace pass was not ready, failed or was dated. A caller keeps
    /// no such answer.
    pub volatile: bool,
    /// Whether the referent is a member of a class: a supertype's callers can
    /// reach it through an override or an implementation.
    pub member: bool,
    /// The workspace pass the search read, for [`Pass::is_current`]. `None`
    /// when the answer read no pass.
    pub pass: Option<Pass>,
}

/// The identity of a workspace pass: it holds no part of the pass, and tells
/// whether the pass is still the one a later request reads.
#[derive(Debug, Clone)]
pub struct Pass(std::sync::Weak<oxabl_pipeline::ReverseGraph>);

impl Pass {
    /// Whether the workspace pass that a request now reads is this one and is
    /// current. Blocks as long as a request does for a pass that is not, so
    /// callers keep it off the LSP thread. `false` when `cancel` is set, or
    /// when the workspace cannot say.
    pub fn is_current(&self, host: &SessionHost, session_root: &Path, cancel: &AtomicBool) -> bool {
        Pass::current(host, session_root, &|| cancel.load(Ordering::Relaxed))
            .is_some_and(|current| current.same(self))
    }

    /// The workspace pass that a request now reads, when it is current. Blocks
    /// as [`is_current`](Self::is_current) does. `None` when `stop` says so,
    /// or when the workspace cannot say.
    pub fn current(
        host: &SessionHost,
        session_root: &Path,
        stop: &dyn Fn() -> bool,
    ) -> Option<Pass> {
        match graph_or_stop(host, session_root, stop) {
            Ok(Ok(Ok(graph))) if graph.gaps.is_empty() => Some(Pass(Arc::downgrade(&graph.graph))),
            _ => None,
        }
    }

    /// Whether both are one pass.
    pub fn same(&self, other: &Pass) -> bool {
        std::sync::Weak::ptr_eq(&self.0, &other.0)
    }
}

/// Why a request has no answer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceError {
    /// Edits cancelled the analysis of the unit that asked.
    Modified,
    /// The unit that asked gives no answer.
    Failed(String),
    /// The client cancelled the request.
    Cancelled,
}

/// What the unit that asked gives.
enum Local {
    /// The whole answer.
    Done(ReferenceAnswer),
    /// The referent, and what this unit holds of it, with the texts it read.
    Search(Referent, Part, HashMap<PathBuf, Arc<str>>),
}

/// Answer `query` for the unit `root` of the workspace at `session_root`.
///
/// Blocks until a workspace pass is current when other files can name the
/// referent, so callers keep it off the LSP thread. Stops with
/// [`ReferenceError::Cancelled`] once `cancel` is set.
pub fn answer_references(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    query: &ReferenceQuery,
    cancel: &AtomicBool,
) -> Result<ReferenceAnswer, ReferenceError> {
    let stop = || cancel.load(Ordering::Relaxed);
    answer_batch(host, session_root, root, std::slice::from_ref(query), &stop)
        .answers
        .pop()
        .unwrap_or(Err(ReferenceError::Cancelled))
}

/// The answers to several queries of one unit, from one search: the unit
/// that asks is analysed once, and so is each other file that one of the
/// referents can be named in. Each answer is the one [`answer_references`]
/// gives for its query alone.
#[derive(Debug, Default)]
pub struct BatchAnswer {
    /// One for each query, in their order.
    pub answers: Vec<Result<ReferenceAnswer, ReferenceError>>,
    /// The other files the search analysed, once for each analysis.
    pub analysed: Vec<PathBuf>,
}

/// Answer each of `queries` for the unit `root`, in one search. Stops, and
/// every answer is [`ReferenceError::Cancelled`], once `stop` says so; `stop`
/// is asked often while the search waits.
pub fn answer_batch(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    queries: &[ReferenceQuery],
    stop: &dyn Fn() -> bool,
) -> BatchAnswer {
    let every = |error: ReferenceError| BatchAnswer {
        answers: queries.iter().map(|_| Err(error.clone())).collect(),
        analysed: Vec::new(),
    };
    let locals = match analyse(host, session_root, root, |unit| {
        let (mut read, mut memo) = (HashMap::new(), ReferenceMemo::default());
        queries
            .iter()
            .map(|query| local_answer(unit, query, &mut read, &mut memo))
            .collect::<Vec<_>>()
    }) {
        None => return every(ReferenceError::Modified),
        Some(Err(reason)) => return every(ReferenceError::Failed(reason)),
        Some(Ok(locals)) => locals,
    };
    let mut answers: Vec<Option<Result<ReferenceAnswer, ReferenceError>>> =
        (0..queries.len()).map(|_| None).collect();
    let mut searches: Vec<Pending> = Vec::new();
    for (at, local) in locals.into_iter().enumerate() {
        match local {
            Local::Done(answer) => answers[at] = Some(Ok(answer)),
            Local::Search(referent, part, texts) => searches.push(Pending {
                at,
                incoming: matches!(part, Part::Incoming(..)),
                include_declaration: matches!(
                    queries[at],
                    ReferenceQuery::References {
                        include_declaration: true,
                        ..
                    }
                ),
                gaps: referent.gaps.clone(),
                referent,
                local: Some(part),
                texts,
                units: Vec::new(),
                volatile: false,
                pass: None,
                count_reasons: Vec::new(),
            }),
        }
    }
    if searches.is_empty() {
        return BatchAnswer {
            answers: answers
                .into_iter()
                .map(|answer| answer.unwrap_or(Err(ReferenceError::Cancelled)))
                .collect(),
            analysed: Vec::new(),
        };
    }
    let cancelled = || every(ReferenceError::Cancelled);
    if searches.iter().any(|search| !search.referent.is_local()) {
        if stop() {
            return cancelled();
        }
        // These leave out or date the whole search, so they are gaps of each
        // answer, not standing ones.
        let graph = match graph_or_stop(host, session_root, stop) {
            Ok(graph) => graph,
            Err(_) => return cancelled(),
        };
        for search in searches
            .iter_mut()
            .filter(|search| !search.referent.is_local())
        {
            match &graph {
                Ok(Ok(graph)) => {
                    search.volatile = !graph.gaps.is_empty();
                    search.pass = Some(Pass(Arc::downgrade(&graph.graph)));
                    search
                        .gaps
                        .extend(graph.gaps.iter().cloned().map(Gap::of_referent));
                    let found = search.referent.search(&graph.graph, root, MAX_UNITS);
                    search.gaps.extend(found.gaps);
                    search.count_reasons = found.counts;
                    search.units = found.units;
                }
                Ok(Err(reason)) => {
                    search.volatile = true;
                    search.gaps.push(Gap::of_referent(format!(
                        "other files are not searched: {reason}"
                    )));
                }
                Err(error) => {
                    search.volatile = true;
                    search.gaps.push(Gap::of_referent(format!(
                        "other files are not searched: {error}"
                    )));
                }
            }
        }
        if stop() {
            return cancelled();
        }
    }

    // Each file once, with the referents that can be named in it.
    let mut units: Vec<PathBuf> = Vec::new();
    let mut wanted: Vec<Vec<usize>> = Vec::new();
    let mut at_unit: HashMap<&Path, usize> = HashMap::new();
    let mut unit_of: Vec<Vec<usize>> = Vec::with_capacity(searches.len());
    for (s, search) in searches.iter().enumerate() {
        let mut mine = Vec::with_capacity(search.units.len());
        for unit in &search.units {
            let u = *at_unit.entry(unit.as_path()).or_insert_with(|| {
                units.push(unit.clone());
                wanted.push(Vec::new());
                units.len() - 1
            });
            wanted[u].push(s);
            mine.push(u);
        }
        unit_of.push(mine);
    }
    drop(at_unit);
    let referents: Arc<Vec<(Referent, bool)>> = Arc::new(
        searches
            .iter()
            .map(|search| (search.referent.clone(), search.incoming))
            .collect(),
    );
    let units = Arc::new(units);
    let remote = search_units(host, session_root, &units, stop, move |unit, at| {
        let (mut read, mut memo) = (HashMap::new(), ReferenceMemo::default());
        wanted[at]
            .iter()
            .map(|&s| {
                let (referent, incoming) = &referents[s];
                // A panic costs this referent in this unit, not the others.
                let part = oxabl_common::catch_panic(|| {
                    if oxabl_common::INJECTION_ENABLED {
                        oxabl_common::panic_if_injected(
                            &format!(
                                "{}:{}",
                                oxabl_common::panic_sites::REFERENCE_PART,
                                referent.name
                            ),
                            &unit.source,
                        );
                    }
                    unit_part(unit, referent, *incoming, &mut read, &mut memo)
                })
                .map_err(|panic| {
                    // What the panic left behind is not trusted.
                    (read, memo) = (HashMap::new(), ReferenceMemo::default());
                    format!("its analysis failed: {panic}")
                });
                (s, part)
            })
            .collect::<Vec<_>>()
    });
    if stop() {
        return BatchAnswer {
            analysed: remote.analysed,
            ..cancelled()
        };
    }
    let analysed = remote.analysed;
    // The parts of each unit, by search.
    let mut parts: Vec<Option<Result<Vec<Option<PartOutcome>>, String>>> =
        (0..units.len()).map(|_| None).collect();
    for (u, outcome) in remote.outcomes {
        parts[u] = Some(outcome.map(|found| {
            let mut by_search: Vec<Option<PartOutcome>> =
                (0..searches.len()).map(|_| None).collect();
            for (s, part) in found {
                by_search[s] = Some(part);
            }
            by_search
        }));
    }
    for (s, search) in searches.into_iter().enumerate() {
        let mut remote = Vec::with_capacity(unit_of[s].len());
        let mut failed: Vec<(PathBuf, String)> = Vec::new();
        for &u in &unit_of[s] {
            match &mut parts[u] {
                Some(Ok(by_search)) => match by_search[s].take() {
                    Some(Ok(part)) => remote.push(part),
                    Some(Err(reason)) => failed.push((units[u].clone(), reason)),
                    None => failed.push((units[u].clone(), "it was not searched".to_string())),
                },
                Some(Err(reason)) => failed.push((units[u].clone(), reason.clone())),
                None => failed.push((units[u].clone(), "no thread could analyse it".to_string())),
            }
        }
        let at = search.at;
        answers[at] = Some(Ok(search.finish(remote, failed)));
    }
    BatchAnswer {
        answers: answers
            .into_iter()
            .map(|answer| answer.unwrap_or(Err(ReferenceError::Cancelled)))
            .collect(),
        analysed,
    }
}

/// A query whose referent other files can name, while its search runs.
struct Pending {
    /// The index of the query.
    at: usize,
    referent: Referent,
    incoming: bool,
    include_declaration: bool,
    /// What the unit that asked holds of the referent.
    local: Option<Part>,
    texts: HashMap<PathBuf, Arc<str>>,
    gaps: Vec<Gap>,
    /// The other files to search, in their order.
    units: Vec<PathBuf>,
    volatile: bool,
    pass: Option<Pass>,
    count_reasons: Vec<String>,
}

impl Pending {
    /// The answer from the parts of the other files, in the order of
    /// [`units`](Self::units), and the files that gave none.
    fn finish(
        mut self,
        remote: Vec<TextedPart>,
        failed: Vec<(PathBuf, String)>,
    ) -> ReferenceAnswer {
        let referent = &self.referent;
        let include_declaration = self.include_declaration;
        let incoming = self.incoming;
        let mut texts = std::mem::take(&mut self.texts);
        let mut gaps = std::mem::take(&mut self.gaps);
        let mut answer = ReferenceAnswer::default();
        let mut unresolved = 0;
        let mut grouped: HashMap<ItemKey, usize> = HashMap::new();
        let parts = self
            .local
            .take()
            .map(|local| (local, HashMap::new()))
            .into_iter()
            .chain(remote);
        for (part, part_texts) in parts {
            // The first text of a file is the one its spans were measured on: a
            // later analysis can have read an edit.
            for (path, text) in part_texts {
                texts.entry(path).or_insert(text);
            }
            match part {
                Part::References(found) => {
                    unresolved += found.unresolved_calls;
                    gaps.extend(found.gaps);
                    answer
                        .locations
                        .extend(kept_locations(found.occurrences, include_declaration));
                }
                Part::Incoming(found) => {
                    unresolved += found.unresolved_calls;
                    gaps.extend(found.gaps);
                    merge_incoming(&mut answer.incoming, &mut grouped, found.calls);
                }
            }
        }
        if include_declaration && !incoming {
            answer.locations.extend(
                referent
                    .declarations()
                    .into_iter()
                    .filter(|target| texts.contains_key(&target.path)),
            );
        }
        if !failed.is_empty() {
            let (path, reason) = &failed[0];
            gaps.push(Gap::of_referent(format!(
                "{} file(s) that can name `{}` could not be analysed, such as {}: {reason}",
                failed.len(),
                referent.name,
                path.display()
            )));
        }
        if unresolved > 0 {
            gaps.push(Gap::of_referent(format!(
                "{unresolved} call(s) of a routine named `{}` go through a handle, a super \
                 procedure or an object whose class oxabl does not know, and can call it",
                referent.name
            )));
        }
        let mut gaps = fold_gaps(gaps);
        let mut seen = Vec::new();
        gaps.retain(|gap| {
            let new = !seen.contains(gap);
            if new {
                seen.push(gap.clone());
            }
            new
        });
        answer.locations.sort_by(|a, b| {
            (&a.path, a.span.map(|s| s.start)).cmp(&(&b.path, b.span.map(|s| s.start)))
        });
        answer.locations.dedup();
        finish_incoming(&mut answer.incoming);
        answer.gaps = gaps;
        answer.count_reasons = self.count_reasons;
        answer.texts = texts;
        answer.resolved = true;
        answer.volatile = self.volatile;
        answer.member = referent.is_member();
        answer.pass = self.pass;
        answer
    }
}

/// What one unit holds of `referent`, with the texts of the files it names.
/// `read` and `memo` keep what the unit has read and resolved, for the other
/// referents.
fn unit_part(
    unit: &NavigationUnit<'_, '_, '_>,
    referent: &Referent,
    incoming: bool,
    read: &mut HashMap<PathBuf, Option<Arc<str>>>,
    memo: &mut ReferenceMemo,
) -> TextedPart {
    let mut texts = HashMap::new();
    let part = if incoming {
        let found = unit.navigation.incoming_calls(referent);
        for call in &found.calls {
            read_into(unit, read, &mut texts, &call.from.path);
        }
        Part::Incoming(found)
    } else {
        let found = unit.navigation.references_with(referent, memo);
        for occurrence in &found.occurrences {
            read_into(unit, read, &mut texts, &occurrence.place.path);
        }
        for declaration in referent.declarations() {
            read_into(unit, read, &mut texts, &declaration.path);
        }
        Part::References(found)
    };
    (part, texts)
}

/// The answer within the unit that asked.
fn local_answer(
    unit: &NavigationUnit<'_, '_, '_>,
    query: &ReferenceQuery,
    read: &mut HashMap<PathBuf, Option<Arc<str>>>,
    memo: &mut ReferenceMemo,
) -> Local {
    let navigation = unit.navigation;
    let mut answer = ReferenceAnswer::default();
    match query {
        ReferenceQuery::Prepare { file, offset } => {
            answer.items = navigation.prepare_call_hierarchy(&unit.spelled(file), *offset);
            for item in &answer.items {
                read_into(unit, read, &mut answer.texts, &item.path);
            }
            Local::Done(answer)
        }
        ReferenceQuery::Outgoing(item) => {
            match navigation.outgoing_calls(item) {
                Ok(outgoing) => {
                    if outgoing.truncated {
                        answer.gaps.push(Gap::of_referent(format!(
                            "the unit has more than {MAX_SITES} calls: the rest are not listed"
                        )));
                    }
                    answer.gaps.extend(outgoing.gaps);
                    answer.outgoing = outgoing.calls;
                }
                Err(reason) => answer.gaps.push(Gap::of_referent(reason)),
            }
            read_into(unit, read, &mut answer.texts, &item.path);
            for call in &answer.outgoing {
                read_into(unit, read, &mut answer.texts, &call.to.path);
            }
            Local::Done(answer)
        }
        ReferenceQuery::References { file, offset, .. } => {
            match navigation.referent_at(&unit.spelled(file), *offset) {
                Some(referent) => {
                    let (part, texts) = unit_part(unit, &referent, false, read, memo);
                    Local::Search(referent, part, texts)
                }
                None => Local::Done(answer),
            }
        }
        ReferenceQuery::Program => match navigation.program_referent() {
            Some(referent) => {
                let (part, texts) = unit_part(unit, &referent, false, read, memo);
                Local::Search(referent, part, texts)
            }
            None => Local::Done(answer),
        },
        ReferenceQuery::Incoming(item) => match navigation.referent_of(item) {
            Some(referent) => {
                let (part, texts) = unit_part(unit, &referent, true, read, memo);
                Local::Search(referent, part, texts)
            }
            None => Local::Done(answer),
        },
    }
}

/// Put the text of `path` into `texts`, read once for the unit through `read`.
fn read_into(
    unit: &NavigationUnit<'_, '_, '_>,
    read: &mut HashMap<PathBuf, Option<Arc<str>>>,
    texts: &mut HashMap<PathBuf, Arc<str>>,
    path: &Path,
) {
    if texts.contains_key(path) {
        return;
    }
    let text = match read.get(path) {
        Some(text) => text.clone(),
        None => {
            let text = unit.read(path);
            read.insert(path.to_path_buf(), text.clone());
            text
        }
    };
    if let Some(text) = text {
        texts.insert(path.to_path_buf(), text);
    }
}

fn kept_locations(found: Vec<Occurrence>, include_declaration: bool) -> Vec<Target> {
    found
        .into_iter()
        .filter(|occurrence| include_declaration || !occurrence.declaration)
        .map(|occurrence| occurrence.place)
        .collect()
}

/// Add each unit's callers to `into`, one entry per caller. The spans of an
/// entry are sorted and without repeats at the end ([`finish_incoming`]).
fn merge_incoming(
    into: &mut Vec<IncomingCall>,
    grouped: &mut HashMap<ItemKey, usize>,
    calls: Vec<IncomingCall>,
) {
    for call in calls {
        match grouped.entry(call.from.key()) {
            Entry::Occupied(at) => into[*at.get()].spans.extend(call.spans),
            Entry::Vacant(at) => {
                at.insert(into.len());
                into.push(call);
            }
        }
    }
}

fn finish_incoming(calls: &mut [IncomingCall]) {
    for call in calls {
        dedupe_spans(&mut call.spans);
    }
}

pub(crate) type GraphAnswer = Result<Result<ReferenceGraph, String>, crate::MethodError>;

/// The workspace graph, from a thread of its own, so a cancel stops the wait
/// for a workspace pass. The pass goes on and is kept for later requests.
pub(crate) fn graph_or_cancel(
    host: &SessionHost,
    session_root: &Path,
    cancel: &AtomicBool,
) -> Result<GraphAnswer, ReferenceError> {
    graph_or_stop(host, session_root, &|| cancel.load(Ordering::Relaxed))
}

/// [`graph_or_cancel`], stopped when `stop` says so.
fn graph_or_stop(
    host: &SessionHost,
    session_root: &Path,
    stop: &dyn Fn() -> bool,
) -> Result<GraphAnswer, ReferenceError> {
    let (sender, receiver) = std::sync::mpsc::channel();
    let spawned = oxabl_common::stack::builder()
        .name("oxabl-reference-graph".to_string())
        .spawn({
            let host = host.clone();
            let session_root = session_root.to_path_buf();
            move || {
                let answer = oxabl_common::catch_panic(|| reference_graph(&host, &session_root))
                    .unwrap_or_else(|panic| Ok(Err(format!("the workspace pass failed: {panic}"))));
                let _ = sender.send(answer);
            }
        });
    if let Err(error) = spawned {
        return Ok(Ok(Err(format!("cannot start the workspace pass: {error}"))));
    }
    loop {
        if stop() {
            return Err(ReferenceError::Cancelled);
        }
        match receiver.recv_timeout(Duration::from_millis(50)) {
            Ok(answer) => return Ok(answer),
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                return Ok(Ok(Err("the workspace pass stopped".to_string())));
            }
        }
    }
}

/// What one unit gives of a referent, with the texts of the files it names.
type TextedPart = (Part, HashMap<PathBuf, Arc<str>>);

/// What one unit gives of a referent, or why it gives nothing.
type PartOutcome = Result<TextedPart, String>;

/// What one unit gives.
enum Part {
    References(UnitReferences),
    Incoming(UnitIncoming),
}

struct Searched<T> {
    /// What each unit gave, by its index, or why it gave nothing. A unit with
    /// no entry was not analysed.
    outcomes: Vec<(usize, Result<T, String>)>,
    /// Each unit analysed, once for each analysis.
    analysed: Vec<PathBuf>,
}

/// Analyse each of `units` with `answer`, which also gets the unit's index,
/// on the threads of the [`Pool`]. Stops between two units when `stop` says
/// so; the units not analysed then have no outcome.
fn search_units<T: Send + 'static>(
    host: &SessionHost,
    session_root: &Path,
    units: &Arc<Vec<PathBuf>>,
    stop: &dyn Fn() -> bool,
    answer: impl Fn(&NavigationUnit<'_, '_, '_>, usize) -> T + Send + Sync + 'static,
) -> Searched<T> {
    let mut searched = Searched {
        outcomes: Vec::with_capacity(units.len()),
        analysed: Vec::with_capacity(units.len()),
    };
    if units.is_empty() {
        return searched;
    }
    let (sender, results) = std::sync::mpsc::channel();
    let task = Arc::new(SearchTask {
        host: host.clone(),
        session_root: session_root.to_path_buf(),
        units: Arc::clone(units),
        next: AtomicUsize::new(0),
        stop: AtomicBool::new(false),
        answer: Box::new(answer),
        results: sender,
    });
    if !Pool::shared().submit(Arc::clone(&task) as Arc<dyn PoolTask>) {
        return searched;
    }
    while searched.outcomes.len() < units.len() {
        if stop() {
            break;
        }
        match results.recv_timeout(Duration::from_millis(50)) {
            Ok((at, outcome)) => {
                searched.analysed.push(units[at].clone());
                searched.outcomes.push((at, outcome));
            }
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => break,
        }
    }
    // The pool leaves the units that are left.
    task.stop.store(true, Ordering::Relaxed);
    searched.outcomes.sort_by_key(|(at, _)| *at);
    searched
}

/// The threads that analyse the other files of every search of the process,
/// [`available_parallelism`](std::thread::available_parallelism) of them. The
/// searches share them by turns, one unit at a time, so the work in progress
/// is bounded whatever the number of searches, and a short search is not stuck
/// behind a long one. A thread with no work for [`Pool::IDLE`] ends.
struct Pool {
    state: Mutex<PoolState>,
    work: Condvar,
}

#[derive(Default)]
struct PoolState {
    /// The searches with units left, in turn.
    tasks: std::collections::VecDeque<Arc<dyn PoolTask>>,
    threads: usize,
}

/// A search that the [`Pool`] works.
trait PoolTask: Send + Sync {
    /// Analyse the next unit. `false` when none is left.
    fn step(&self) -> bool;
}

impl Pool {
    /// Short, because the allocator keeps the memory that a thread freed until
    /// the thread ends: after a large search that is most of the memory of
    /// the process, and none of it is in use.
    const IDLE: Duration = Duration::from_secs(5);

    fn shared() -> &'static Pool {
        static POOL: std::sync::LazyLock<Pool> = std::sync::LazyLock::new(|| Pool {
            state: Mutex::new(PoolState::default()),
            work: Condvar::new(),
        });
        &POOL
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, PoolState> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Queue `task`, and start the threads that are missing. `false` when no
    /// thread runs and none can start.
    fn submit(&'static self, task: Arc<dyn PoolTask>) -> bool {
        let size = std::thread::available_parallelism().map_or(1, usize::from);
        let mut state = self.lock();
        state.tasks.push_back(Arc::clone(&task));
        while state.threads < size {
            let spawned = oxabl_common::stack::builder()
                .name("oxabl-references".to_string())
                .spawn(move || self.work());
            if spawned.is_err() {
                break;
            }
            state.threads += 1;
        }
        if state.threads == 0 {
            state.tasks.retain(|held| !Arc::ptr_eq(held, &task));
            return false;
        }
        self.work.notify_all();
        true
    }

    fn work(&self) {
        loop {
            let task = {
                let mut state = self.lock();
                loop {
                    if let Some(task) = state.tasks.pop_front() {
                        state.tasks.push_back(Arc::clone(&task));
                        break task;
                    }
                    let (held, waited) = self
                        .work
                        .wait_timeout(state, Pool::IDLE)
                        .unwrap_or_else(|poisoned| poisoned.into_inner());
                    state = held;
                    if waited.timed_out() && state.tasks.is_empty() {
                        state.threads -= 1;
                        return;
                    }
                }
            };
            // A step contains the panics of its unit; a panic past that costs
            // the unit ([`Unsent`]), not the thread or the search.
            let more = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| task.step()))
                .unwrap_or(true);
            if !more {
                self.lock().tasks.retain(|held| !Arc::ptr_eq(held, &task));
            }
        }
    }
}

/// One search, as the [`Pool`] works it.
struct SearchTask<T> {
    host: SessionHost,
    session_root: PathBuf,
    units: Arc<Vec<PathBuf>>,
    next: AtomicUsize,
    stop: AtomicBool,
    answer: Box<UnitAnswer<T>>,
    results: std::sync::mpsc::Sender<(usize, Result<T, String>)>,
}

/// What a search asks of each unit, with the index of the unit.
type UnitAnswer<T> = dyn Fn(&NavigationUnit<'_, '_, '_>, usize) -> T + Send + Sync;

/// Sends a failure for a unit whose analysis ended with no outcome, so that
/// the search does not wait for it.
struct Unsent<'a, T> {
    at: usize,
    results: &'a std::sync::mpsc::Sender<(usize, Result<T, String>)>,
    sent: bool,
}

impl<T> Drop for Unsent<'_, T> {
    fn drop(&mut self) {
        if !self.sent {
            let _ = self
                .results
                .send((self.at, Err("its analysis failed".to_string())));
        }
    }
}

impl<T: Send> PoolTask for SearchTask<T> {
    fn step(&self) -> bool {
        if self.stop.load(Ordering::Relaxed) {
            return false;
        }
        let at = self.next.fetch_add(1, Ordering::Relaxed);
        let Some(unit) = self.units.get(at) else {
            return false;
        };
        let mut unsent = Unsent {
            at,
            results: &self.results,
            sent: false,
        };
        let outcome = analyse_unit(&self.host, &self.session_root, unit, at, &*self.answer);
        unsent.sent = true;
        // The search may have stopped and gone.
        let _ = self.results.send((at, outcome));
        true
    }
}

/// What `answer` gives for `unit`, analysed again when an edit cancelled its
/// analysis. A panic in one file costs that file, not the request.
fn analyse_unit<T>(
    host: &SessionHost,
    session_root: &Path,
    unit: &Path,
    at: usize,
    answer: &dyn Fn(&NavigationUnit<'_, '_, '_>, usize) -> T,
) -> Result<T, String> {
    oxabl_common::catch_panic(|| {
        (0..ATTEMPTS)
            .find_map(|_| {
                analyse(host, session_root, unit, |unit| {
                    oxabl_common::hold_if_injected(
                        oxabl_common::panic_sites::REFERENCE_UNIT,
                        &unit.source,
                    );
                    answer(unit, at)
                })
            })
            .unwrap_or_else(|| Err("edits kept cancelling its analysis".to_string()))
    })
    .unwrap_or_else(|panic| Err(format!("its analysis failed: {panic}")))
}

/// Analyse `root` on a fresh snapshot of the session at `session_root`.
/// `None` when an edit cancelled the analysis.
pub(crate) fn analyse<T>(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    answer: impl FnOnce(&NavigationUnit<'_, '_, '_>) -> T,
) -> Option<Result<T, String>> {
    let taken = host.with(|sessions| {
        let session = sessions.get(session_root)?;
        Some((
            session.database().clone(),
            session.schema_handle(),
            session.buffer_overlay(),
        ))
    });
    let Some((snapshot, schema, overlay)) = taken else {
        return Some(Err("no session".to_string()));
    };
    with_navigation(&snapshot, schema, root, overlay, answer)
}
