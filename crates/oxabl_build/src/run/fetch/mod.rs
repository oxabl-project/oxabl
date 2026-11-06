//! The fetch: one thread reads the cache's outcomes, one installs the hits at
//! their final paths in batches, and one stores the remote hits in the nearer
//! cache.

mod fill;
mod install;

use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, channel};
use std::thread;
use std::time::{Duration, Instant};

use oxabl_common::catch_panic;

use self::install::Arrival;
use super::context::{Ctx, FetchPlan};
use super::types::Msg;
use crate::cache::{ArtifactPath, FetchOutcome};

/// A batch is installed, or filled, when it holds this many sets.
const BATCH_SETS: usize = 500;
/// A batch is installed, or filled, when its first set is this old.
const BATCH_AGE: Duration = Duration::from_secs(2);

/// Runs the cache's `fetch` and ends with `FetchEnded`, once every set it
/// installed has an answer. A panic inside the cache, or in the installer,
/// ends the call: the keys it did not answer are misses.
pub fn run(ctx: &Ctx<'_>, plan: &FetchPlan, tx: Sender<Msg>) {
    let _ = std::fs::create_dir_all(&plan.stage);
    let (fills, filled) = channel();
    thread::scope(|scope| {
        scope.spawn(move || fill::run(ctx, &filled));
        let (arrivals, arrived) = channel();
        let (ended, watching) = channel::<()>();
        thread::scope(|scope| {
            scope.spawn(move || cancel_when_asked(ctx, &watching));
            let installer = tx.clone();
            scope.spawn(move || {
                let _ = catch_panic(|| install::run(ctx, plan, &installer, &arrived, &fills));
            });
            let _ = catch_panic(|| {
                ctx.cache
                    .fetch(plan.requests.clone(), &plan.stage, &mut |outcome| {
                        deliver(ctx, plan, &tx, &arrivals, outcome)
                    });
            });
            drop(arrivals);
            drop(ended);
        });
        let _ = tx.send(Msg::FetchEnded);
    });
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

/// Answers a miss at once and hands a hit to the installer. A set whose file
/// list is not exactly the expected outputs is a miss.
fn deliver(
    ctx: &Ctx<'_>,
    plan: &FetchPlan,
    tx: &Sender<Msg>,
    arrivals: &Sender<Arrival>,
    outcome: FetchOutcome,
) {
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
    match installed {
        Some((files, origin)) if expected(ctx, unit, &files) => {
            let _ = arrivals.send(Arrival { unit, key, origin });
        }
        _ => {
            let _ = tx.send(Msg::Fetched { unit, hit: None });
        }
    }
}

fn expected(ctx: &Ctx<'_>, unit: u32, files: &[ArtifactPath]) -> bool {
    let mut got: Vec<&str> = files.iter().map(|file| file.0.as_str()).collect();
    let mut want: Vec<&str> = ctx.infos[&unit]
        .expected
        .iter()
        .map(|e| e.staged.as_str())
        .collect();
    got.sort_unstable();
    want.sort_unstable();
    got == want
}

/// The next batch from `rx`: it waits for one item, then takes more until
/// the batch is full, its first item is `BATCH_AGE` old or the sender has
/// left. `None` when the sender has left and nothing is queued.
fn next_batch<T>(rx: &Receiver<T>) -> Option<Vec<T>> {
    let first = rx.recv().ok()?;
    let since = Instant::now();
    let mut batch = vec![first];
    while batch.len() < BATCH_SETS {
        let left = BATCH_AGE.saturating_sub(since.elapsed());
        match rx.recv_timeout(left) {
            Ok(next) => batch.push(next),
            Err(_) => break,
        }
    }
    Some(batch)
}
