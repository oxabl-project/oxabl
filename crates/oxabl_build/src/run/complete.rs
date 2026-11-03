//! The end of one unit: its record, its events and the verification that
//! decides whether a cache may store its outputs.

use std::time::{SystemTime, UNIX_EPOCH};

use super::context::{Ctx, State, UnitState};
use super::types::{Hit, UnitDone};
use crate::cache::{ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, Origin};
use crate::events::Event;
use crate::store::{COMPILED, FAILED, LOCAL_CACHE, REMOTE_CACHE, ResultRow, SUCCESS};

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
        Some(Ok(names)) => verify
            .reads
            .check(done.unit, closure, names)
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

/// Records the end of one unit, tells the events and offers the outputs.
pub fn complete(ctx: &Ctx<'_>, state: &mut State<'_>, done: UnitDone) {
    let info = &ctx.infos[&done.unit];
    state.failed.remove(&done.unit);
    let verifying = ctx.verify.is_some() && done.ok && info.key.is_some();
    let closure = if verifying || (done.ok && !state.failed.is_empty()) {
        ctx.outcome.after.closure(done.unit, &mut state.scratch)
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
    if verifying && let Err(why) = check_reads(ctx, state, &done, &closure) {
        key = None;
        components = None;
        note = Some(why);
        cacheable = false;
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
