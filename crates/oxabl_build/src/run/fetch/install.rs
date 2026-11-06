//! The installer: moves fetched sets from the stage to their final paths, one
//! batch at a time, after one `syncfs` for the batch.

use std::fs::File;
use std::sync::mpsc::{Receiver, Sender};

use super::next_batch;
use crate::cache::{
    ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, Origin, RemoteOrigin,
};
use crate::output::promote;
use crate::refresh::parallel;
use crate::run::context::{Ctx, FetchPlan};
use crate::run::types::{Hit, Msg};

/// A set the cache installed in the stage with the expected files.
pub struct Arrival {
    pub unit: u32,
    pub key: CompileKey,
    pub origin: Origin,
}

/// Installs every batch from `arrived` and answers each of its units. A set
/// reaches its final paths only after its bytes are durable: a batch whose
/// `syncfs` fails is a miss for each unit. After a cancel, nothing more is
/// installed or answered.
pub fn run(
    ctx: &Ctx<'_>,
    plan: &FetchPlan,
    tx: &Sender<Msg>,
    arrived: &Receiver<Arrival>,
    fills: &Sender<(ArtifactSetRef, RemoteOrigin)>,
) {
    while let Some(batch) = next_batch(arrived) {
        if ctx.cancel.is_cancelled() {
            return;
        }
        let durable = File::open(&plan.stage).is_ok_and(|stage| rustix::fs::syncfs(&stage).is_ok());
        let installed = parallel::map(&batch, |arrival| {
            match (ctx.cancel.is_cancelled(), durable) {
                (true, _) => None,
                (false, true) => Some(install(ctx, plan, arrival)),
                (false, false) => Some(None),
            }
        });
        for (arrival, installed) in batch.iter().zip(installed) {
            let Some(installed) = installed else {
                continue;
            };
            let hit = installed.map(|(hit, fill)| {
                if let Some(fill) = fill {
                    let _ = fills.send(fill);
                }
                hit
            });
            let _ = tx.send(Msg::Fetched {
                unit: arrival.unit,
                hit,
            });
        }
    }
}

/// Moves one set to its final paths. A remote hit comes with what the
/// filler stores.
fn install(
    ctx: &Ctx<'_>,
    plan: &FetchPlan,
    arrival: &Arrival,
) -> Option<(Hit, Option<(ArtifactSetRef, RemoteOrigin)>)> {
    let info = &ctx.infos[&arrival.unit];
    let outputs = promote::promote(ctx.root, &plan.stage, &info.expected).ok()?;
    let fill = match &arrival.origin {
        Origin::Remote(remote) => Some((
            ArtifactSetRef {
                key: arrival.key,
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
            },
            remote.clone(),
        )),
        Origin::Local { .. } => None,
    };
    let origin = arrival.origin.clone();
    Some((Hit { outputs, origin }, fill))
}
