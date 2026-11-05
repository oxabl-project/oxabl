//! Starting one worker process and its handshake.

use std::io::Write;

use super::cancel::Cancel;
use super::command::{self, Values};
use super::error::WorkerError;
use super::handshake::{self, Context, Failure};
use super::logs::Logs;
use super::pool::PoolSpec;
use super::program::Program;
use super::random;
use super::session::{Session, SpawnSpec};
use crate::key::Profile;
use std::time::Instant;

/// A worker that fails to start is started again at most this often.
pub(super) const RESTARTS_PER_SLOT: usize = 2;

pub(super) enum SlotFailure {
    Config(String),
    Infra { number: usize, message: String },
    Cancelled,
}

pub(super) type Slot = Result<(Session, Profile, Vec<String>), SlotFailure>;

fn try_start(
    spec: &PoolSpec,
    program: &Program,
    logs: &Logs,
    cancel: &Cancel,
    number: usize,
) -> Result<(Session, Profile, Vec<String>), Failure> {
    let argv = command::expand(
        &spec.command,
        &Values {
            program: &program.worker_path,
            root: &spec.worker_root,
            worker: number,
            build: &spec.build_id,
        },
    )
    .map_err(Failure::Config)?;
    let io =
        |error: std::io::Error| WorkerError::Spawn(format!("cannot open the worker log: {error}"));
    let stderr = logs.open(number).map_err(io)?;
    let noise = logs.open(number).map_err(io)?;
    let mut session = Session::spawn(
        SpawnSpec {
            argv: &argv,
            cwd: &spec.host_root,
            env: &spec.env,
            stderr,
            noise,
        },
        cancel,
    )?;
    let context = Context {
        worker_root: &spec.worker_root,
        preprocessor: &spec.preprocessor,
        schema_names: &spec.schema_names,
    };
    let deadline = Instant::now() + spec.start_timeout;
    match handshake::run(&mut session, &context, &random::token(), deadline) {
        Ok((environment, warnings)) => Ok((session, environment, warnings)),
        Err(failure) => {
            session.kill();
            Err(failure)
        }
    }
}

pub(super) fn start_slot(
    spec: &PoolSpec,
    program: &Program,
    logs: &Logs,
    cancel: &Cancel,
    number: usize,
) -> Slot {
    let mut last = String::new();
    for attempt in 1..=RESTARTS_PER_SLOT + 1 {
        if cancel.is_cancelled() {
            return Err(SlotFailure::Cancelled);
        }
        match try_start(spec, program, logs, cancel, number) {
            Ok((session, environment, warnings)) => {
                return Ok((session, environment, warnings));
            }
            Err(Failure::Config(message)) => return Err(SlotFailure::Config(message)),
            Err(Failure::Worker(WorkerError::Cancelled)) => return Err(SlotFailure::Cancelled),
            Err(Failure::Worker(error)) => {
                last = error.to_string();
                if let Ok(mut file) = logs.open(number) {
                    let _ = writeln!(
                        file,
                        "oxabl: start {attempt} of worker {number} failed: {last}"
                    );
                }
            }
        }
    }
    Err(SlotFailure::Infra {
        number,
        message: last,
    })
}
