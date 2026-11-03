//! One worker process and the request/response exchange with it.

use std::collections::BTreeMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::{Child, ChildStdin, Command, ExitStatus, Stdio};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use std::os::unix::process::CommandExt;

use super::cancel::Cancel;
use super::codec::Codec;
use super::error::WorkerError;
use super::frame::{Frame, Reply, Request};
use super::kill;
use super::reader::{self, Event};
use crate::key::{FileDigest, Fingerprint};

/// How long a `QUIT` waits for `BYE` and the exit.
const QUIT_WAIT: Duration = Duration::from_secs(10);
/// How long a `SIGTERM` waits before `SIGKILL`.
const TERM_GRACE: Duration = Duration::from_secs(5);
/// The longest wait on the reader before oxabl looks at the cancel flag.
pub(super) const POLL: Duration = Duration::from_millis(100);

/// How to start one worker.
pub struct SpawnSpec<'a> {
    pub argv: &'a [String],
    pub cwd: &'a Path,
    pub env: &'a BTreeMap<String, String>,
    /// Receives the worker's stderr.
    pub stderr: File,
    /// Receives the worker's noise lines.
    pub noise: File,
}

/// A running worker.
pub struct Session {
    child: Child,
    stdin: Option<ChildStdin>,
    events: Receiver<Event>,
    reader: Option<JoinHandle<()>>,
    pub(super) cancel: Cancel,
    pub codec: Codec,
    next_id: u64,
}

fn is_hex64(text: &str) -> bool {
    text.len() == 64
        && text
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

impl Session {
    /// Starts the command in a new process group with stdin and stdout piped.
    pub fn spawn(spec: SpawnSpec<'_>, cancel: &Cancel) -> Result<Session, WorkerError> {
        let (program, args) = spec
            .argv
            .split_first()
            .ok_or_else(|| WorkerError::Spawn("the command is empty".into()))?;
        let mut child = Command::new(program)
            .args(args)
            .current_dir(spec.cwd)
            .envs(spec.env)
            .process_group(0)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::from(spec.stderr))
            .spawn()
            .map_err(|error| WorkerError::Spawn(format!("{program}: {error}")))?;
        cancel.register(child.id());
        let stdin = child.stdin.take();
        let stdout = child.stdout.take().expect("stdout is piped");
        let (sender, events) = channel();
        let reader = reader::spawn(stdout, spec.noise, sender);
        Ok(Session {
            child,
            stdin,
            events,
            reader: Some(reader),
            cancel: cancel.clone(),
            codec: Codec::utf8(),
            next_id: 1,
        })
    }

    pub fn pid(&self) -> u32 {
        self.child.id()
    }

    /// Writes one request.
    pub fn send(&mut self, request: &Request<'_>) -> Result<(), WorkerError> {
        let lines = request.lines(&self.codec).map_err(WorkerError::Protocol)?;
        let stdin = self
            .stdin
            .as_mut()
            .ok_or_else(|| WorkerError::Lost("stdin is closed".into()))?;
        for line in lines {
            stdin.write_all(&line).map_err(|error| {
                WorkerError::Lost(format!("cannot write to the worker: {error}"))
            })?;
        }
        stdin
            .flush()
            .map_err(|error| WorkerError::Lost(format!("cannot write to the worker: {error}")))
    }

    fn exit_status(&mut self) -> Option<ExitStatus> {
        let deadline = Instant::now() + Duration::from_millis(500);
        loop {
            if let Ok(Some(status)) = self.child.try_wait() {
                return Some(status);
            }
            if Instant::now() >= deadline {
                return None;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    fn lost(&mut self, what: &str) -> WorkerError {
        match self.exit_status() {
            Some(status) => WorkerError::Lost(format!("{what}; the process exited with {status}")),
            None => WorkerError::Lost(what.to_string()),
        }
    }

    /// Waits up to `wait` for one reply; `None` when nothing came. An `ERROR`
    /// reply is a protocol error.
    pub(super) fn poll_reply(&mut self, wait: Duration) -> Result<Option<Reply>, WorkerError> {
        match self.events.recv_timeout(wait) {
            Ok(Event::Line(line)) => {
                let reply = Frame::parse(&line)
                    .and_then(Reply::from_frame)
                    .map_err(WorkerError::Protocol)?;
                if let Reply::Error { text } = &reply {
                    return Err(WorkerError::Protocol(format!(
                        "the worker reported: {}",
                        String::from_utf8_lossy(text)
                    )));
                }
                Ok(Some(reply))
            }
            Ok(Event::TooLong) => Err(WorkerError::Protocol(
                "a protocol line is longer than 30,000 bytes".into(),
            )),
            Ok(Event::Eof) => Err(self.lost("its stdout ended")),
            Ok(Event::Failed(error)) => Err(self.lost(&format!("cannot read its stdout: {error}"))),
            Err(RecvTimeoutError::Timeout) => Ok(None),
            Err(RecvTimeoutError::Disconnected) => Err(self.lost("its reader stopped")),
        }
    }

    /// Reads the next reply; a cancel stops the wait.
    pub fn recv(&mut self, deadline: Instant) -> Result<Reply, WorkerError> {
        loop {
            if self.cancel.is_cancelled() {
                return Err(WorkerError::Cancelled);
            }
            let wait = deadline.saturating_duration_since(Instant::now());
            if wait.is_zero() {
                return Err(WorkerError::Timeout("no answer before the deadline".into()));
            }
            if let Some(reply) = self.poll_reply(wait.min(POLL))? {
                return Ok(reply);
            }
        }
    }

    pub(super) fn id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// Reads the GUID and the schema stamp of one connected database.
    pub fn fingerprint(
        &mut self,
        ldb: &str,
        deadline: Instant,
    ) -> Result<Fingerprint, WorkerError> {
        self.send(&Request::Fingerprint { ldb })?;
        let Reply::Fprint {
            ldb: got,
            guid,
            stamp,
        } = self.recv(deadline)?
        else {
            return Err(WorkerError::Protocol("expected FPRINT".into()));
        };
        let codec = self.codec;
        let decode = |bytes: &[u8]| codec.decode(bytes).map_err(WorkerError::Protocol);
        if decode(&got)? != ldb {
            return Err(WorkerError::Protocol(format!(
                "FPRINT for another database than {ldb}"
            )));
        }
        Ok(Fingerprint {
            ldb: ldb.to_string(),
            guid: decode(&guid)?,
            stamp: decode(&stamp)?,
        })
    }

    /// Hashes one worker-side file.
    pub fn hash(&mut self, path: &str, deadline: Instant) -> Result<FileDigest, WorkerError> {
        let id = self.id();
        self.send(&Request::Hash { id, path })?;
        let Reply::Hashed {
            id: got,
            kind,
            size,
            sha256,
        } = self.recv(deadline)?
        else {
            return Err(WorkerError::Protocol("expected HASHED".into()));
        };
        if got != id.to_string() {
            return Err(WorkerError::Protocol(format!(
                "HASHED for request {got}, expected {id}"
            )));
        }
        match kind.as_str() {
            "file" if is_hex64(&sha256) => Ok(FileDigest::File {
                size: size
                    .parse()
                    .map_err(|_| WorkerError::Protocol(format!("bad file size `{size}`")))?,
                sha256,
            }),
            "dir" if size == "-" && sha256 == "-" => Ok(FileDigest::Dir),
            "none" if size == "-" && sha256 == "-" => Ok(FileDigest::Absent),
            _ => Err(WorkerError::Protocol(format!(
                "bad HASHED answer `{kind} {size} {sha256}`"
            ))),
        }
    }

    /// Hashes a sorted set of worker-side files; the answer is one digest of the list.
    pub fn hash_set(&mut self, paths: &[String], deadline: Instant) -> Result<String, WorkerError> {
        let id = self.id();
        self.send(&Request::HashSet { id, paths })?;
        let Reply::HashsetDone { id: got, sha256 } = self.recv(deadline)? else {
            return Err(WorkerError::Protocol("expected HASHSET-DONE".into()));
        };
        if got != id.to_string() || !is_hex64(&sha256) {
            return Err(WorkerError::Protocol(format!(
                "bad HASHSET-DONE `{got} {sha256}`"
            )));
        }
        Ok(sha256)
    }

    /// Checks that the worker answers.
    pub fn ping(&mut self, deadline: Instant) -> Result<(), WorkerError> {
        let n = self.id();
        self.send(&Request::Ping { n })?;
        match self.recv(deadline)? {
            Reply::Pong { n: got } if got == n.to_string() => Ok(()),
            other => Err(WorkerError::Protocol(format!(
                "expected PONG {n}, got {other:?}"
            ))),
        }
    }

    /// Whether the process is still running.
    pub fn is_alive(&mut self) -> bool {
        matches!(self.child.try_wait(), Ok(None))
    }

    fn finish(&mut self) {
        self.cancel.unregister(self.child.id());
        drop(self.stdin.take());
        if let Some(reader) = self.reader.take() {
            let _ = reader.join();
        }
    }

    /// Sends `QUIT`, waits up to ten seconds for `BYE` and the exit, then
    /// stops the process group.
    pub fn quit(&mut self) {
        let deadline = Instant::now() + QUIT_WAIT;
        if self.send(&Request::Quit).is_ok() {
            while let Ok(reply) = self.recv(deadline) {
                if reply == Reply::Bye {
                    break;
                }
            }
        }
        drop(self.stdin.take());
        let remaining = deadline.saturating_duration_since(Instant::now());
        if !kill::wait_exit(&mut self.child, remaining) {
            kill::terminate(&mut self.child, TERM_GRACE);
        }
        self.finish();
    }

    /// `SIGTERM` to the process group, `SIGKILL` after five seconds, then reap.
    pub fn kill(&mut self) {
        kill::terminate(&mut self.child, TERM_GRACE);
        self.finish();
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        if matches!(self.child.try_wait(), Ok(None)) {
            kill::terminate(&mut self.child, Duration::from_millis(200));
        }
        self.cancel.unregister(self.child.id());
    }
}
