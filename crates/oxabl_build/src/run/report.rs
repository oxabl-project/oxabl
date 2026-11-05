//! What a build reports: per unit, in total, as text and as JSON.

use serde_json::{Value, json};

use crate::worker::compile::CompilerMessage;

/// The `format` number of the report document.
pub const FORMAT: u32 = 1;

/// How one unit of the rebuild set ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitStatus {
    Compiled,
    Failed,
    /// The unit cannot compile where it is, and why.
    Skipped(String),
    /// The build stopped before the unit compiled.
    NotRun,
}

impl UnitStatus {
    pub fn name(&self) -> &'static str {
        match self {
            UnitStatus::Compiled => "ok",
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
    /// The sets of outputs offered to the cache.
    pub offered: usize,
    /// How long the refresh, the probe and the keys took before any compile.
    pub setup_ms: u64,
    pub elapsed_ms: u64,
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
                    "source": match unit.status {
                        UnitStatus::Compiled | UnitStatus::Failed => json!("compiled"),
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
            "timing": { "setup_ms": self.setup_ms, "elapsed_ms": self.elapsed_ms },
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
        out
    }
}
