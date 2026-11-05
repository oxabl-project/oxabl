//! What the worker threads and the coordinating thread exchange.

use crate::output::Expected;
use crate::store::{Components, OutputRow};
use crate::worker::compile::CompilerMessage;

/// One unit of the build with what a compile and its record need.
#[derive(Debug)]
pub struct UnitInfo {
    pub path: String,
    pub name: String,
    pub propath: usize,
    pub key: Option<[u8; 32]>,
    pub components: Option<Components>,
    pub expected: Vec<Expected>,
}

/// The end of one unit's compile.
#[derive(Debug)]
pub struct UnitDone {
    pub job: usize,
    pub unit: u32,
    pub ok: bool,
    pub ms: u64,
    pub messages: Vec<CompilerMessage>,
    pub note: Option<String>,
    /// The outputs now in place, when the unit compiled.
    pub outputs: Vec<OutputRow>,
    /// The unit failed because its worker was lost twice.
    pub lost: bool,
}

/// A message from a worker thread to the coordinating thread.
#[derive(Debug)]
pub enum Msg {
    UnitStarted {
        unit: u32,
        worker: usize,
    },
    UnitDone(UnitDone),
    WorkerLost {
        worker: usize,
        reason: &'static str,
        detail: String,
        unit: Option<u32>,
    },
    SlotEnded {
        worker: usize,
        error: Option<String>,
    },
}
