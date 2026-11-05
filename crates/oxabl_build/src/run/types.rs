//! What the worker threads and the coordinating thread exchange.

use std::time::Instant;

use crate::cache::Origin;
use crate::key::SchemaPart;
use crate::observe::xref::Xref;
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
    /// Only oxabl's reading of the unit stands in the way of a key: what its
    /// compile reads is observed.
    pub observable: bool,
    /// The key holds the closure observed at the last compile.
    pub observed: bool,
    /// With `observed`: the files of that closure, classes included.
    pub observed_closure: Vec<u32>,
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
    /// When the worker reported the end of the unit.
    pub finished: Instant,
    /// What the compiler read, from its `XREF-XML`, when verification or
    /// the observed closure asks for it.
    pub reads: Option<Result<Xref, String>>,
}

/// A set the cache gave for one unit, installed at its final paths.
#[derive(Debug)]
pub struct Hit {
    pub outputs: Vec<OutputRow>,
    pub origin: Origin,
}

/// A message from a worker thread to the coordinating thread.
#[derive(Debug)]
pub enum Msg {
    UnitStarted {
        unit: u32,
        worker: usize,
    },
    UnitDone(UnitDone),
    /// V1 for a unit that compiled: `Err` names the file that changed.
    Rehashed {
        unit: u32,
        result: Result<(), String>,
    },
    /// One part of a schema check, from the worker that took it.
    Image {
        round: u64,
        part: usize,
        result: Result<Vec<SchemaPart>, String>,
    },
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
    /// The answer of the fetch for one unit: a hit, or a miss.
    Fetched {
        unit: u32,
        hit: Option<Hit>,
    },
    /// The fetch call returned: keys it did not report are misses.
    FetchEnded,
}
