//! The fetch thread: asks the cache for every keyable unit of the rebuild set,
//! and installs each hit at its final paths as it arrives.

use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, channel};
use std::thread;
use std::time::Duration;

use oxabl_common::catch_panic;

use super::context::{Ctx, FetchPlan};
use super::types::{Hit, Msg};
use crate::cache::{
    ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, FetchOutcome, Origin,
};
use crate::output::promote;

/// Runs the cache's `fetch` and ends with `FetchEnded`. A panic inside the
/// cache ends the call: the keys it did not report are misses.
pub fn run(ctx: &Ctx<'_>, plan: &FetchPlan, tx: Sender<Msg>) {
    let _ = std::fs::create_dir_all(&plan.stage);
    let (ended, watching) = channel::<()>();
    thread::scope(|scope| {
        scope.spawn(move || cancel_when_asked(ctx, &watching));
        let _ = catch_panic(|| {
            ctx.cache
                .fetch(plan.requests.clone(), &plan.stage, &mut |outcome| {
                    deliver(ctx, plan, &tx, outcome)
                });
        });
        drop(ended);
    });
    let _ = tx.send(Msg::FetchEnded);
}

/// Tells the cache to end its fetch when the build is cancelled, until the
/// fetch has ended.
fn cancel_when_asked(ctx: &Ctx<'_>, ended: &Receiver<()>) {
    while let Err(RecvTimeoutError::Timeout) = ended.recv_timeout(Duration::from_millis(20)) {
        if ctx.cancel.is_cancelled() {
            let _ = catch_panic(|| ctx.cache.cancel_fetch());
            return;
        }
    }
}

fn deliver(ctx: &Ctx<'_>, plan: &FetchPlan, tx: &Sender<Msg>, outcome: FetchOutcome) {
    if ctx.cancel.is_cancelled() {
        return;
    }
    let (key, installed) = match outcome {
        FetchOutcome::Miss { key, .. } => (key, None),
        FetchOutcome::Installed { key, files, origin } => (key, Some((files, origin))),
    };
    let Some(&unit) = plan.unit_of.get(&key) else {
        return;
    };
    let hit = installed.and_then(|(files, origin)| install(ctx, plan, unit, key, files, origin));
    let _ = tx.send(Msg::Fetched { unit, hit });
}

/// Moves an installed set from the fetch stage to its final paths. A set whose
/// file list is not exactly the expected outputs is a miss.
fn install(
    ctx: &Ctx<'_>,
    plan: &FetchPlan,
    unit: u32,
    key: CompileKey,
    files: Vec<ArtifactPath>,
    origin: Origin,
) -> Option<Hit> {
    let info = &ctx.infos[&unit];
    let mut got: Vec<&str> = files.iter().map(|file| file.0.as_str()).collect();
    let mut want: Vec<&str> = info.expected.iter().map(|e| e.staged.as_str()).collect();
    got.sort_unstable();
    want.sort_unstable();
    if got != want {
        return None;
    }
    let outputs = promote::promote(ctx.root, &plan.stage, &info.expected).ok()?;
    if let Origin::Remote(remote) = &origin {
        let set = ArtifactSetRef {
            key,
            files: info
                .expected
                .iter()
                .zip(&outputs)
                .map(|(expected, row)| ArtifactFileRef {
                    path: ArtifactPath(expected.staged.clone()),
                    disk_path: ctx.root.join(&row.path),
                    size: row.size,
                    blake3: row.hash,
                })
                .collect(),
        };
        let _ = catch_panic(|| ctx.cache.fill(set, remote));
    }
    Some(Hit { outputs, origin })
}
