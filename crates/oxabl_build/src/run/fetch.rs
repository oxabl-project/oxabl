//! The fetch thread: asks the cache for every keyable unit of the rebuild set,
//! and installs each hit at its final paths as it arrives.

use std::sync::mpsc::Sender;

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
    let _ = catch_panic(|| {
        ctx.cache
            .fetch(plan.requests.clone(), &plan.stage, &mut |outcome| {
                deliver(ctx, plan, &tx, outcome)
            });
    });
    let _ = tx.send(Msg::FetchEnded);
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
