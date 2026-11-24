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
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use oxabl_index::calls::MAX_SITES;
use oxabl_pipeline::{CallItem, IncomingCall, Occurrence, OutgoingCall, Referent, Target};

use crate::SessionHost;
use crate::db::{NavigationUnit, with_navigation};
use crate::methods::reference_graph;

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
    pub gaps: Vec<String>,
    pub texts: HashMap<PathBuf, Arc<str>>,
}

/// Why a request has no answer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceError {
    /// Edits cancelled the analysis of the unit that asked.
    Modified,
    /// The unit that asked gives no answer.
    Failed(String),
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
/// referent, so callers keep it off the LSP thread.
pub fn answer_references(
    host: &SessionHost,
    session_root: &Path,
    root: &Path,
    query: &ReferenceQuery,
) -> Result<ReferenceAnswer, ReferenceError> {
    let local = analyse(host, session_root, root, |unit| local_answer(unit, query))
        .ok_or(ReferenceError::Modified)?
        .map_err(ReferenceError::Failed)?;
    let (referent, local, mut texts) = match local {
        Local::Done(answer) => return Ok(answer),
        Local::Search(referent, part, texts) => (referent, part, texts),
    };
    let mut gaps = Vec::new();
    let mut units = Vec::new();
    if !referent.is_local() {
        match reference_graph(host, session_root) {
            Ok(Ok(graph)) => {
                gaps.extend(graph.gaps);
                let search = referent.search(&graph.graph, root, MAX_UNITS);
                gaps.extend(search.gaps);
                units = search.units;
            }
            Ok(Err(reason)) => gaps.push(format!("other files are not searched: {reason}")),
            Err(error) => gaps.push(format!("other files are not searched: {error}")),
        }
    }
    let incoming = matches!(local, Part::Incoming(..));
    let include_declaration = matches!(
        query,
        ReferenceQuery::References {
            include_declaration: true,
            ..
        }
    );
    let remote = search_units(host, session_root, &units, |unit| {
        unit_part(unit, &referent, incoming)
    });

    let mut answer = ReferenceAnswer::default();
    let mut unresolved = 0;
    let parts = std::iter::once((local, HashMap::new())).chain(remote.parts);
    for (part, part_texts) in parts {
        texts.extend(part_texts);
        match part {
            Part::References(found, unresolved_calls) => {
                unresolved += unresolved_calls;
                answer
                    .locations
                    .extend(kept_locations(found, include_declaration));
            }
            Part::Incoming(calls, unresolved_calls) => {
                unresolved += unresolved_calls;
                merge_incoming(&mut answer.incoming, calls);
            }
        }
    }
    if !remote.failed.is_empty() {
        let (path, reason) = &remote.failed[0];
        gaps.push(format!(
            "{} file(s) that can name `{}` could not be analysed, such as {}: {reason}",
            remote.failed.len(),
            referent.name,
            path.display()
        ));
    }
    if unresolved > 0 {
        gaps.push(format!(
            "{unresolved} call(s) of a routine named `{}` go through a handle, a super \
             procedure or an object whose class oxabl does not know, and can call it",
            referent.name
        ));
    }
    answer.locations.sort_by(|a, b| {
        (&a.path, a.span.map(|s| s.start)).cmp(&(&b.path, b.span.map(|s| s.start)))
    });
    answer.locations.dedup();
    answer.gaps = gaps;
    answer.texts = texts;
    Ok(answer)
}

/// What one unit holds of `referent`, with the texts of the files it names.
fn unit_part(
    unit: &NavigationUnit<'_, '_, '_>,
    referent: &Referent,
    incoming: bool,
) -> (Part, HashMap<PathBuf, Arc<str>>) {
    let mut texts = HashMap::new();
    let part = if incoming {
        let found = unit.navigation.incoming_calls(referent);
        for call in &found.calls {
            read_into(unit, &mut texts, &call.from.path);
        }
        Part::Incoming(found.calls, found.unresolved_calls)
    } else {
        let found = unit.navigation.references(referent);
        for occurrence in &found.occurrences {
            read_into(unit, &mut texts, &occurrence.place.path);
        }
        Part::References(found.occurrences, found.unresolved_calls)
    };
    (part, texts)
}

/// The answer within the unit that asked.
fn local_answer(unit: &NavigationUnit<'_, '_, '_>, query: &ReferenceQuery) -> Local {
    let navigation = unit.navigation;
    let mut answer = ReferenceAnswer::default();
    match query {
        ReferenceQuery::Prepare { file, offset } => {
            answer.items = navigation.prepare_call_hierarchy(&unit.spelled(file), *offset);
            for item in &answer.items {
                read_into(unit, &mut answer.texts, &item.path);
            }
            Local::Done(answer)
        }
        ReferenceQuery::Outgoing(item) => {
            match navigation.outgoing_calls(item) {
                Ok(outgoing) => {
                    if outgoing.truncated {
                        answer.gaps.push(format!(
                            "the unit has more than {MAX_SITES} calls: the rest are not listed"
                        ));
                    }
                    answer.outgoing = outgoing.calls;
                }
                Err(reason) => answer.gaps.push(reason),
            }
            read_into(unit, &mut answer.texts, &item.path);
            for call in &answer.outgoing {
                read_into(unit, &mut answer.texts, &call.to.path);
            }
            Local::Done(answer)
        }
        ReferenceQuery::References { file, offset, .. } => {
            match navigation.referent_at(&unit.spelled(file), *offset) {
                Some(referent) => {
                    let (part, texts) = unit_part(unit, &referent, false);
                    Local::Search(referent, part, texts)
                }
                None => Local::Done(answer),
            }
        }
        ReferenceQuery::Incoming(item) => match navigation.referent_of(item) {
            Some(referent) => {
                let (part, texts) = unit_part(unit, &referent, true);
                Local::Search(referent, part, texts)
            }
            None => Local::Done(answer),
        },
    }
}

fn read_into(
    unit: &NavigationUnit<'_, '_, '_>,
    texts: &mut HashMap<PathBuf, Arc<str>>,
    path: &Path,
) {
    if !texts.contains_key(path)
        && let Some(text) = unit.read(path)
    {
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

fn merge_incoming(into: &mut Vec<IncomingCall>, calls: Vec<IncomingCall>) {
    for call in calls {
        let known = into.iter_mut().find(|known| {
            known.from.path == call.from.path
                && known.from.selection == call.from.selection
                && known.from.kind == call.from.kind
        });
        match known {
            Some(known) => {
                for span in call.spans {
                    if !known.spans.contains(&span) {
                        known.spans.push(span);
                    }
                }
            }
            None => into.push(call),
        }
    }
}

/// What one other unit gives.
enum Part {
    References(Vec<Occurrence>, usize),
    Incoming(Vec<IncomingCall>, usize),
}

struct Searched<T> {
    parts: Vec<T>,
    /// The units that gave no answer, and why.
    failed: Vec<(PathBuf, String)>,
}

/// Analyse each of `units` with `answer`, on worker threads.
fn search_units<T: Send>(
    host: &SessionHost,
    session_root: &Path,
    units: &[PathBuf],
    answer: impl Fn(&NavigationUnit<'_, '_, '_>) -> T + Sync,
) -> Searched<T> {
    let jobs = std::thread::available_parallelism()
        .map_or(1, usize::from)
        .clamp(1, units.len().max(1));
    let next = AtomicUsize::new(0);
    let mut results: Vec<(usize, Result<T, String>)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..jobs)
            .map(|_| {
                let (next, answer) = (&next, &answer);
                oxabl_common::stack::builder()
                    .name("oxabl-references".to_string())
                    .spawn_scoped(scope, move || {
                        let mut mine = Vec::new();
                        loop {
                            let at = next.fetch_add(1, Ordering::Relaxed);
                            let Some(unit) = units.get(at) else { break };
                            // A panic in one file costs that file, not the request.
                            let outcome = oxabl_common::catch_panic(|| {
                                (0..ATTEMPTS)
                                    .find_map(|_| analyse(host, session_root, unit, answer))
                                    .unwrap_or_else(|| {
                                        Err("edits kept cancelling its analysis".to_string())
                                    })
                            })
                            .unwrap_or_else(|panic| Err(format!("its analysis failed: {panic}")));
                            mine.push((at, outcome));
                        }
                        mine
                    })
            })
            .collect();
        workers
            .into_iter()
            .flat_map(|worker| match worker {
                Ok(worker) => worker.join().unwrap_or_else(|panic| {
                    std::panic::resume_unwind(panic);
                }),
                Err(_) => Vec::new(),
            })
            .collect()
    });
    results.sort_by_key(|(at, _)| *at);
    let mut searched = Searched {
        parts: Vec::new(),
        failed: Vec::new(),
    };
    let mut seen = vec![false; units.len()];
    for (at, outcome) in results {
        seen[at] = true;
        match outcome {
            Ok(part) => searched.parts.push(part),
            Err(reason) => searched.failed.push((units[at].clone(), reason)),
        }
    }
    // A worker that could not start leaves its units unanalysed.
    for (at, seen) in seen.into_iter().enumerate() {
        if !seen {
            searched
                .failed
                .push((units[at].clone(), "no thread could analyse it".to_string()));
        }
    }
    searched
}

/// Analyse `root` on a fresh snapshot of the session at `session_root`.
/// `None` when an edit cancelled the analysis.
fn analyse<T>(
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
