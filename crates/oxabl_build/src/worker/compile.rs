//! `PROPATH`, `OPTIONS` and `COMPILE` requests.

use std::time::{Duration, Instant};

use super::error::WorkerError;
use super::frame::{CompileRequest, Reply, Request};
use super::session::{POLL, Session};

/// How long a compile may wait on the worker.
#[derive(Clone, Copy, Debug)]
pub struct Limits {
    /// From the `COMPILE` line, or from the previous `DONE`, to the next `DONE`.
    pub unit_timeout: Duration,
    /// How long a busy worker may run on after a cancel.
    pub cancel_grace: Duration,
}

/// One compiler message of a unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompilerMessage {
    /// `error` or `warning`.
    pub kind: String,
    pub number: String,
    pub file: String,
    pub row: String,
    pub column: String,
    pub text: String,
}

/// The `DONE` of one unit with the messages that came before it.
#[derive(Clone, Debug)]
pub struct CompiledUnit {
    pub name: String,
    pub ok: bool,
    pub ms: u64,
    pub messages: Vec<CompilerMessage>,
}

fn protocol(message: impl Into<String>) -> WorkerError {
    WorkerError::Protocol(message.into())
}

impl Session {
    fn expect_ok(&mut self, id: u64, deadline: Instant) -> Result<(), WorkerError> {
        match self.recv(deadline)? {
            Reply::Ok { id: got } if got == id.to_string() => Ok(()),
            other => Err(protocol(format!("expected OK {id}, got {other:?}"))),
        }
    }

    /// Defines PROPATH `id` on the worker.
    pub fn define_propath(
        &mut self,
        id: u64,
        entries: &[String],
        deadline: Instant,
    ) -> Result<(), WorkerError> {
        self.send(&Request::Propath { id, entries })?;
        self.expect_ok(id, deadline)
    }

    /// Defines the compile options `id` on the worker.
    pub fn define_options(
        &mut self,
        id: u64,
        pairs: &[(String, String)],
        deadline: Instant,
    ) -> Result<(), WorkerError> {
        self.send(&Request::Options { id, pairs })?;
        self.expect_ok(id, deadline)
    }

    fn decode(&self, bytes: &[u8]) -> Result<String, WorkerError> {
        self.codec.decode(bytes).map_err(protocol)
    }

    /// Sends one `COMPILE` request and reads its answers up to `END`, calling
    /// `on_done` as each unit completes. A cancel does not stop the wait for
    /// the unit in flight: it ends `cancel_grace` after the cancel.
    pub fn compile(
        &mut self,
        request: &CompileRequest<'_>,
        limits: Limits,
        on_done: &mut dyn FnMut(CompiledUnit),
    ) -> Result<(), WorkerError> {
        self.send(&Request::Compile(request))?;
        let req = request.req.to_string();
        let mut mark = Instant::now();
        let mut next = 0;
        let mut messages = Vec::new();
        loop {
            let mut deadline = mark + limits.unit_timeout;
            if let Some(at) = self.cancel.cancelled_at() {
                deadline = deadline.min(at + limits.cancel_grace);
            }
            let wait = deadline.saturating_duration_since(Instant::now());
            if wait.is_zero() {
                return Err(match self.cancel.is_cancelled() {
                    true => WorkerError::Cancelled,
                    false => WorkerError::Timeout("no answer before the unit timeout".into()),
                });
            }
            let Some(reply) = self.poll_reply(wait.min(POLL))? else {
                continue;
            };
            match reply {
                Reply::Msg(message) => {
                    let name = self.decode(&message.name)?;
                    if message.req != req || request.units.get(next) != Some(&name) {
                        return Err(protocol(format!("MSG for {name}, which is not in flight")));
                    }
                    messages.push(CompilerMessage {
                        kind: message.kind,
                        number: message.number,
                        file: self.decode(&message.file)?,
                        row: message.row,
                        column: message.column,
                        text: self.decode(&message.text)?,
                    });
                }
                Reply::Done {
                    req: got,
                    name,
                    status,
                    ms,
                } => {
                    let name = self.decode(&name)?;
                    if got != req || request.units.get(next) != Some(&name) {
                        return Err(protocol(format!("DONE for {name}, which is not in flight")));
                    }
                    let ok = match status.as_str() {
                        "ok" => true,
                        "fail" => false,
                        other => return Err(protocol(format!("bad DONE status `{other}`"))),
                    };
                    let ms = ms
                        .parse()
                        .map_err(|_| protocol(format!("bad DONE time `{ms}`")))?;
                    on_done(CompiledUnit {
                        name,
                        ok,
                        ms,
                        messages: std::mem::take(&mut messages),
                    });
                    next += 1;
                    mark = Instant::now();
                }
                Reply::End { req: got } => {
                    if got != req || next != request.units.len() {
                        return Err(protocol(format!(
                            "END after {next} of {} units",
                            request.units.len()
                        )));
                    }
                    return Ok(());
                }
                other => return Err(protocol(format!("unexpected {other:?} during a compile"))),
            }
        }
    }
}
