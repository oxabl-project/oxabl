//! The end of one unit: its record, its events and the verification that
//! decides whether a cache may store its outputs.

use std::time::{SystemTime, UNIX_EPOCH};

use super::context::{Ctx, State, UnitState};
use super::types::{Hit, UnitDone};
use crate::cache::{ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, Origin};
use crate::events::Event;
use crate::observe::observe;
use crate::store::{COMPILED, Components, FAILED, LOCAL_CACHE, REMOTE_CACHE, ResultRow, SUCCESS};

pub fn seconds() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_secs() as i64)
}

/// V3 for a unit that compiled. `Err` is the note to record: the unit gets no
/// key, so it compiles again at each build.
fn check_reads(
    ctx: &Ctx<'_>,
    state: &mut State<'_>,
    done: &UnitDone,
    closure: &[u32],
) -> Result<(), String> {
    let Some(verify) = ctx.verify.as_ref() else {
        return Ok(());
    };
    let path = &ctx.infos[&done.unit].path;
    match done.reads.as_ref() {
        Some(Ok(xref)) => verify
            .reads
            .check(done.unit, closure, &xref.sources)
            .map_err(|file| {
                state.notes.push(format!(
                    "{path}: the compiler opened {file}, which is not in the closure of the unit; the unit is not cached and compiles at each build. This is a bug in the analysis of oxabl: please report it"
                ));
                file
            }),
        Some(Err(why)) => {
            state.notes.push(format!(
                "{path}: {why}; the unit is not cached and compiles at each build"
            ));
            Err(format!("unreadable XREF-XML: {why}"))
        }
        None => Err("no XREF-XML".to_string()),
    }
}

/// What the observation of one compile changes in its record.
enum Observation {
    /// The compiler read the closure the key holds.
    Same,
    /// It read another closure: the key made from it now, if any.
    Changed(Option<Box<([u8; 32], Components)>>),
    /// No closure can be taken from the compile.
    Refused,
}

/// Takes the closure that the compile of a unit oxabl cannot read reported,
/// and stores it when it differs from the one the unit's key holds. A key
/// made from an observed closure stands only when the compile is observed.
fn observe_compile(ctx: &Ctx<'_>, state: &mut State<'_>, done: &UnitDone) -> Option<Observation> {
    let info = &ctx.infos[&done.unit];
    if !done.ok || !info.observable {
        return None;
    }
    let Some(observer) = ctx.observer.as_ref() else {
        if !info.observed {
            return None;
        }
        state.recorder.batch().observed.push((done.unit, None));
        return Some(Observation::Refused);
    };
    let propath = &observer.base.propaths[info.propath];
    let entries: Vec<String> = propath.iter().map(|(entry, _)| entry.clone()).collect();
    let observed = match done.reads.as_ref() {
        Some(Ok(xref)) => observe(xref, &info.path, &entries, &observer.places),
        Some(Err(why)) => Err(why.clone()),
        None => Err("no XREF-XML".to_string()),
    };
    let closure = match observed {
        Ok(closure) => closure,
        Err(why) => {
            state.notes.push(format!(
                "{}: no key from what the compiler read: {why}; the unit compiles at each build",
                info.path
            ));
            state.recorder.batch().observed.push((done.unit, None));
            return Some(Observation::Refused);
        }
    };
    if info.observed && observer.stored(done.unit).as_ref() == Some(&closure) {
        return Some(Observation::Same);
    }
    let key = observer.key_after(done.unit, &info.name, info.propath, &closure);
    state
        .recorder
        .batch()
        .observed
        .push((done.unit, Some(closure)));
    Some(Observation::Changed(key.map(Box::new)))
}

/// Records the end of one unit, tells the events and offers the outputs.
pub fn complete(ctx: &Ctx<'_>, state: &mut State<'_>, done: UnitDone) {
    let info = &ctx.infos[&done.unit];
    state.failed.remove(&done.unit);
    let verifying = ctx.verify.is_some() && done.ok && info.key.is_some();
    let closure = if verifying || (done.ok && !state.failed.is_empty()) {
        super::closure_of(ctx.outcome, info, done.unit, &mut state.scratch)
    } else {
        Vec::new()
    };
    let tainted = done.ok && closure.iter().any(|node| state.failed.contains(node));
    if !done.ok {
        state.failed.insert(done.unit);
    }
    for message in &done.messages {
        state.emit(&Event::Message {
            path: &info.path,
            message,
        });
    }
    state.emit(&Event::UnitFinished {
        path: &info.path,
        ok: done.ok,
        ms: done.ms,
    });
    let mut key = info.key;
    let mut components = info.components;
    let mut note = None;
    let mut cacheable = verifying && !tainted && !ctx.cancel.is_cancelled();
    let rekeyed = match observe_compile(ctx, state, &done) {
        Some(Observation::Same) | None => false,
        Some(Observation::Changed(after)) => {
            key = after.as_ref().map(|after| after.0);
            components = after.map(|after| after.1);
            true
        }
        Some(Observation::Refused) => {
            key = None;
            components = None;
            true
        }
    };
    cacheable &= !rekeyed;
    if verifying
        && !rekeyed
        && let Err(why) = check_reads(ctx, state, &done, &closure)
    {
        key = None;
        components = None;
        note = Some(why);
        cacheable = false;
    }
    if done.ok && !info.observable && ctx.outcome.observed.contains_key(&done.unit) {
        state.recorder.batch().observed.push((done.unit, None));
    }
    let result = ResultRow {
        status: if done.ok { SUCCESS } else { FAILED },
        key,
        components,
        compiled_gen: ctx.generation,
        origin: COMPILED,
        duration_ms: done.ms,
        finished_at: seconds(),
        note,
    };
    if cacheable && let (Some(key), Some(release)) = (info.key, state.release.as_mut()) {
        let set = ArtifactSetRef {
            key: CompileKey(key),
            files: info
                .expected
                .iter()
                .zip(&done.outputs)
                .map(|(expected, row)| ArtifactFileRef {
                    path: ArtifactPath(expected.staged.clone()),
                    disk_path: ctx.root.join(&row.path),
                    size: row.size,
                    blake3: row.hash,
                })
                .collect(),
        };
        release.offer(done.unit, &info.path, set, done.finished);
    }
    state.recorder.push(done.unit, result, done.outputs);
    state.units.insert(
        done.unit,
        UnitState {
            ok: done.ok,
            ms: done.ms,
            messages: done.messages,
            note: done.note,
            tainted,
            lost: done.lost,
            fetched: None,
        },
    );
}

/// Records a unit that a cache gave: no compile ran.
pub fn fetched(ctx: &Ctx<'_>, state: &mut State<'_>, unit: u32, hit: Hit) {
    let info = &ctx.infos[&unit];
    let (cache, origin) = match &hit.origin {
        Origin::Local { .. } => ("local", LOCAL_CACHE),
        Origin::Remote(_) => ("remote", REMOTE_CACHE),
    };
    state.emit(&Event::UnitFetched {
        path: &info.path,
        cache,
    });
    state.failed.remove(&unit);
    let result = ResultRow {
        status: SUCCESS,
        key: info.key,
        components: info.components,
        compiled_gen: ctx.generation,
        origin,
        duration_ms: 0,
        finished_at: seconds(),
        note: None,
    };
    state.recorder.push(unit, result, hit.outputs);
    state.units.insert(
        unit,
        UnitState {
            ok: true,
            ms: 0,
            messages: Vec::new(),
            note: None,
            tainted: false,
            lost: false,
            fetched: Some(hit.origin),
        },
    );
}
