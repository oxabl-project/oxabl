//! The event stream of a running build.

use serde_json::{Value, json};

use crate::worker::compile::CompilerMessage;

/// The `format` number of the `build-started` event.
pub const FORMAT: u32 = 1;

/// Something that happened in a build.
#[derive(Debug)]
pub enum Event<'a> {
    BuildStarted {
        build: &'a str,
        units: usize,
        rebuild: usize,
        workers: usize,
    },
    UnitStarted {
        path: &'a str,
        worker: usize,
    },
    Message {
        path: &'a str,
        message: &'a CompilerMessage,
    },
    UnitFinished {
        path: &'a str,
        ok: bool,
        ms: u64,
    },
    WorkerLost {
        worker: usize,
        reason: &'a str,
        unit: Option<&'a str>,
    },
    Summary {
        compiled: usize,
        fetched: usize,
        failed: usize,
        skipped: usize,
        exit: u8,
    },
}

fn number(text: &str) -> u64 {
    text.parse().unwrap_or(0)
}

impl Event<'_> {
    /// The event as one JSON object.
    pub fn to_json(&self) -> Value {
        match self {
            Event::BuildStarted {
                build,
                units,
                rebuild,
                workers,
            } => json!({
                "event": "build-started",
                "format": FORMAT,
                "build": build,
                "units": units,
                "rebuild": rebuild,
                "workers": workers,
            }),
            Event::UnitStarted { path, worker } => {
                json!({ "event": "unit-started", "path": path, "worker": worker })
            }
            Event::Message { path, message } => json!({
                "event": "message",
                "path": path,
                "type": message.kind,
                "number": number(&message.number),
                "file": message.file,
                "row": number(&message.row),
                "column": number(&message.column),
                "text": message.text,
            }),
            Event::UnitFinished { path, ok, ms } => json!({
                "event": "unit-finished",
                "path": path,
                "status": if *ok { "ok" } else { "fail" },
                "ms": ms,
            }),
            Event::WorkerLost {
                worker,
                reason,
                unit,
            } => json!({
                "event": "worker-lost",
                "worker": worker,
                "reason": reason,
                "unit": unit,
            }),
            Event::Summary {
                compiled,
                fetched,
                failed,
                skipped,
                exit,
            } => json!({
                "event": "summary",
                "compiled": compiled,
                "fetched": fetched,
                "failed": failed,
                "skipped": skipped,
                "exit": exit,
            }),
        }
    }
}

/// Where events go.
pub type Sink<'a> = Box<dyn FnMut(&Event<'_>) + Send + 'a>;
