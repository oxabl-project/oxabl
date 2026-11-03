//! What a build fixes before it compiles, and what it collects while it does.

use std::path::Path;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;

use rustc_hash::{FxHashMap, FxHashSet};

use super::record::Recorder;
use super::types::UnitInfo;
use crate::cache::ArtifactUploader;
use crate::events::{Event, Sink};
use crate::graph::Scratch;
use crate::refresh::Outcome;
use crate::worker::Cancel;
use crate::worker::compile::{CompilerMessage, Limits};

/// Fixed for the whole build.
pub struct Ctx<'a> {
    pub root: &'a Path,
    pub outcome: &'a Outcome,
    pub infos: FxHashMap<u32, UnitInfo>,
    /// The worker-side PROPATH entries, by PROPATH id.
    pub propaths: Vec<Vec<String>>,
    pub options: Vec<(String, String)>,
    pub xref: &'static str,
    pub debug_list: bool,
    pub listing: bool,
    pub limits: Limits,
    pub cancel: &'a Cancel,
    pub fail_fast: bool,
    pub generation: u64,
    /// The last compile time of each unit, for the priority of its job.
    pub weights: FxHashMap<u32, u64>,
    /// How often each unit lost its worker.
    pub lost: Mutex<FxHashMap<u32, u8>>,
    pub next_request: AtomicU64,
}

/// What became of one unit in this build.
#[derive(Debug, Clone)]
pub struct UnitState {
    pub ok: bool,
    pub ms: u64,
    pub messages: Vec<CompilerMessage>,
    pub note: Option<String>,
    /// A unit in its closure failed in this build.
    pub tainted: bool,
    /// The failure was a lost worker; a retry would only repeat it.
    pub lost: bool,
}

/// Changes while the build runs.
pub struct State<'a> {
    pub recorder: Recorder<'a>,
    pub sink: Option<Sink<'a>>,
    pub uploader: Option<Box<dyn ArtifactUploader>>,
    pub units: FxHashMap<u32, UnitState>,
    pub failed: FxHashSet<u32>,
    pub notes: Vec<String>,
    /// An error that stops the build: the store could not be written.
    pub fatal: Option<crate::error::BuildError>,
    pub scratch: Scratch,
}

impl State<'_> {
    pub fn emit(&mut self, event: &Event<'_>) {
        if let Some(sink) = &mut self.sink {
            sink(event);
        }
    }
}
