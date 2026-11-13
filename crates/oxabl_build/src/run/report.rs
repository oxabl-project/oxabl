//! What a build reports: per unit, in total, as text and as JSON.

use serde_json::{Value, json};

use crate::cache::{NotStoredReason, Origin};
use crate::verify::policy::Policy;
use crate::worker::compile::CompilerMessage;

/// The `format` number of the report document.
pub const FORMAT: u32 = 1;

/// How one unit of the rebuild set ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitStatus {
    Compiled,
    /// A cache gave the outputs; no compile ran.
    Fetched(Origin),
    Failed,
    /// The unit cannot compile where it is, and why.
    Skipped(String),
    /// The build stopped before the unit compiled.
    NotRun,
}

impl UnitStatus {
    pub fn name(&self) -> &'static str {
        match self {
            UnitStatus::Compiled | UnitStatus::Fetched(_) => "ok",
            UnitStatus::Failed => "fail",
            UnitStatus::Skipped(_) => "skipped",
            UnitStatus::NotRun => "not-run",
        }
    }
}

/// One unit of the rebuild set, and the skipped units.
#[derive(Debug, Clone)]
pub struct UnitReport {
    pub path: String,
    pub status: UnitStatus,
    /// Why the planner put the unit in the rebuild set.
    pub reason: String,
    pub duration_ms: u64,
    pub messages: Vec<CompilerMessage>,
    /// A unit in its closure failed in this build.
    pub tainted: bool,
    pub note: Option<String>,
}

/// The counts of a build.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Summary {
    pub compiled: usize,
    pub fetched: usize,
    pub failed: usize,
    pub skipped: usize,
    pub not_run: usize,
    pub tainted: usize,
    pub up_to_date: usize,
}

/// What the caches did in one build.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CacheReport {
    /// The label of the cache, `none` when no cache was used.
    pub label: String,
    /// Whether any cache stores sets, which turns verification on.
    pub writes: bool,
    /// Sets that passed verification and were pushed.
    pub offered: usize,
    pub stored: usize,
    pub already_present: usize,
    pub not_stored: Vec<(NotStoredReason, usize)>,
    /// Units whose set was not offered: a file changed during the build.
    pub changed_inputs: usize,
    /// Units whose set was not offered: the schema check failed.
    pub schema_dropped: usize,
    /// The verification of the build.
    pub policy: Option<Policy>,
    /// Schema checks that ended while the build compiled, the final one not
    /// counted.
    pub imaging_rounds: usize,
}

/// How the schema images of a build came about, by logical database name.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SchemaImages {
    pub reused: Vec<String>,
    pub imaged: Vec<String>,
}

/// Where the setup time went, each phase in milliseconds. The refresh runs at
/// the same time as the worker start and the imaging.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Setup {
    pub refresh_ms: u64,
    /// Starting the workers and their handshakes.
    pub workers_ms: u64,
    /// Imaging the databases whose stored image could not be reused.
    pub imaging_ms: u64,
    /// The raw digest passes that tell whether a stored image holds.
    pub raw_digest_ms: u64,
    /// The keys, the `$DLC` hash phase included.
    pub keys_ms: u64,
    /// The `$DLC` hash phase alone.
    pub dlc_hash_ms: u64,
    pub plan_ms: u64,
    /// Pruning the output directories and recording the environment.
    pub prune_ms: u64,
}

/// The outcome of `oxabl build`.
#[derive(Debug)]
pub struct BuildReport {
    /// 0 compiled, 1 a unit failed, 3 no worker was left, 130 cancelled.
    pub exit: u8,
    pub build: String,
    pub generation: u64,
    pub environment: Option<String>,
    pub units: Vec<UnitReport>,
    pub summary: Summary,
    pub notes: Vec<String>,
    pub cache: CacheReport,
    /// How long the refresh, the probe and the keys took before any compile.
    pub setup_ms: u64,
    pub setup: Setup,
    pub elapsed_ms: u64,
    /// The databases whose schema image the build reused, and the ones it
    /// imaged.
    pub schema_images: SchemaImages,
}

impl CacheReport {
    /// The summary line of the caches, when one was used.
    pub fn line(&self) -> Option<String> {
        if self.label == "none" {
            return None;
        }
        if !self.writes {
            return Some(format!("{}: read-only, 0 sets offered", self.label));
        }
        let mut parts = vec![format!("{} sets offered", self.offered)];
        if self.stored > 0 {
            parts.push(format!("{} stored", self.stored));
        }
        if self.already_present > 0 {
            parts.push(format!("{} already present", self.already_present));
        }
        for (reason, count) in &self.not_stored {
            parts.push(format!("{count} not stored ({reason:?})"));
        }
        if self.changed_inputs > 0 {
            parts.push(format!("{} dropped: a file changed", self.changed_inputs));
        }
        if self.schema_dropped > 0 {
            parts.push(format!("{} dropped: schema", self.schema_dropped));
        }
        Some(format!("{}: {}", self.label, parts.join(", ")))
    }
}

fn message(message: &CompilerMessage) -> Value {
    json!({
        "type": message.kind,
        "number": message.number.parse::<u64>().unwrap_or(0),
        "file": message.file,
        "row": message.row.parse::<u64>().unwrap_or(0),
        "column": message.column.parse::<u64>().unwrap_or(0),
        "text": message.text,
    })
}

impl BuildReport {
    /// The report as one JSON document.
    pub fn to_json(&self) -> Value {
        let units: Vec<Value> = self
            .units
            .iter()
            .map(|unit| {
                json!({
                    "path": unit.path,
                    "status": unit.status.name(),
                    "source": match &unit.status {
                        UnitStatus::Compiled | UnitStatus::Failed => json!("compiled"),
                        UnitStatus::Fetched(Origin::Local { .. }) => json!("local-cache"),
                        UnitStatus::Fetched(Origin::Remote(_)) => json!("remote-cache"),
                        _ => Value::Null,
                    },
                    "origin_unverified": match &unit.status {
                        UnitStatus::Fetched(Origin::Local { recorded: Some(remote) }) => {
                            json!(format!("remote {} {}", remote.tier, remote.signer))
                        }
                        _ => Value::Null,
                    },
                    "reason": unit.reason,
                    "ms": unit.duration_ms,
                    "tainted": unit.tainted,
                    "skipped": match &unit.status {
                        UnitStatus::Skipped(why) => json!(why),
                        _ => Value::Null,
                    },
                    "note": unit.note,
                    "messages": unit.messages.iter().map(message).collect::<Vec<_>>(),
                })
            })
            .collect();
        let summary = &self.summary;
        json!({
            "format": FORMAT,
            "exit": self.exit,
            "build": self.build,
            "store": { "generation": self.generation },
            "environment": self.environment,
            "summary": {
                "compiled": summary.compiled,
                "fetched": summary.fetched,
                "failed": summary.failed,
                "skipped": summary.skipped,
                "not_run": summary.not_run,
                "tainted": summary.tainted,
                "up_to_date": summary.up_to_date,
            },
            "cache": {
                "label": self.cache.label,
                "writes": self.cache.writes,
                "offered": self.cache.offered,
                "stored": self.cache.stored,
                "already_present": self.cache.already_present,
                "not_stored": self.cache.not_stored.iter().map(|(reason, count)| {
                    json!({ "reason": format!("{reason:?}"), "count": count })
                }).collect::<Vec<_>>(),
                "changed_inputs": self.cache.changed_inputs,
                "schema_dropped": self.cache.schema_dropped,
                "verification": self.cache.policy.map(|policy| {
                    Value::Object(
                        policy
                            .entries()
                            .into_iter()
                            .map(|(name, value)| (name.to_string(), value))
                            .collect(),
                    )
                }),
                "imaging_rounds": self.cache.imaging_rounds,
            },
            "timing": {
                "setup_ms": self.setup_ms,
                "elapsed_ms": self.elapsed_ms,
                "setup": {
                    "refresh_ms": self.setup.refresh_ms,
                    "workers_ms": self.setup.workers_ms,
                    "imaging_ms": self.setup.imaging_ms,
                    "raw_digest_ms": self.setup.raw_digest_ms,
                    "keys_ms": self.setup.keys_ms,
                    "dlc_hash_ms": self.setup.dlc_hash_ms,
                    "plan_ms": self.setup.plan_ms,
                    "prune_ms": self.setup.prune_ms,
                },
            },
            "schema_images": {
                "reused": self.schema_images.reused,
                "imaged": self.schema_images.imaged,
            },
            "notes": self.notes,
            "units": units,
        })
    }

    /// The report as text: each failed unit with its messages, then the counts.
    pub fn to_text(&self) -> String {
        let mut out = String::new();
        for unit in &self.units {
            match &unit.status {
                UnitStatus::Failed => out.push_str(&format!("failed    {}\n", unit.path)),
                UnitStatus::NotRun => out.push_str(&format!("not run   {}\n", unit.path)),
                _ => continue,
            }
            if let Some(note) = &unit.note {
                out.push_str(&format!("  {note}\n"));
            }
            for message in &unit.messages {
                out.push_str(&format!(
                    "  {} {} {}:{}:{}: {}\n",
                    message.kind,
                    message.number,
                    message.file,
                    message.row,
                    message.column,
                    message.text
                ));
            }
        }
        let s = &self.summary;
        out.push_str(&format!("compiled {}", s.compiled));
        if s.fetched > 0 {
            out.push_str(&format!(", fetched {}", s.fetched));
        }
        if s.failed > 0 {
            out.push_str(&format!(", failed {}", s.failed));
        }
        if s.tainted > 0 {
            out.push_str(&format!(" ({} tainted)", s.tainted));
        }
        if s.not_run > 0 {
            out.push_str(&format!(", not run {}", s.not_run));
        }
        if s.skipped > 0 {
            out.push_str(&format!(", skipped {}", s.skipped));
        }
        out.push_str(&format!(
            ", {} up to date in {:.1}s\n",
            s.up_to_date,
            self.elapsed_ms as f64 / 1000.0
        ));
        if let Some(line) = self.cache.line() {
            out.push_str(&line);
            out.push('\n');
        }
        out
    }
}
