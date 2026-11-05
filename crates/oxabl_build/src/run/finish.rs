//! The report of a finished build.

use rustc_hash::FxHashMap;

use super::context::UnitState;
use super::pass::PassEnd;
use super::report::{BuildReport, Summary, UnitReport, UnitStatus};
use crate::plan::Plan;

/// What the report holds besides the units.
pub struct Facts {
    pub build: String,
    pub environment: String,
    pub setup_ms: u64,
    pub elapsed_ms: u64,
}

/// The exit code: 130 after a cancel, 3 when no worker was left, 1 when a unit
/// failed, else 0.
fn exit_code(end: PassEnd, failed: usize) -> u8 {
    match end {
        PassEnd::Cancelled => 130,
        PassEnd::NoWorkers => 3,
        _ if failed > 0 => 1,
        _ => 0,
    }
}

/// Turns the final state of the units into the report.
pub fn report(
    plan: &Plan,
    id_of: &FxHashMap<&str, u32>,
    mut units: FxHashMap<u32, UnitState>,
    end: PassEnd,
    notes: Vec<String>,
    facts: Facts,
) -> BuildReport {
    let mut reports: Vec<UnitReport> = Vec::new();
    let mut summary = Summary {
        up_to_date: plan.unchanged.len(),
        skipped: plan.skipped.len(),
        ..Summary::default()
    };
    for entry in &plan.rebuild {
        let reason = entry
            .reasons
            .first()
            .map(|reason| reason.describe())
            .unwrap_or_default();
        let state = units.remove(&id_of[entry.path.as_str()]);
        let report = match state {
            Some(state) => UnitReport {
                path: entry.path.clone(),
                status: match (state.ok, state.fetched) {
                    (true, Some(origin)) => UnitStatus::Fetched(origin),
                    (true, None) => UnitStatus::Compiled,
                    (false, _) => UnitStatus::Failed,
                },
                reason,
                duration_ms: state.ms,
                messages: state.messages,
                tainted: state.tainted,
                note: state.note,
            },
            None => UnitReport {
                path: entry.path.clone(),
                status: UnitStatus::NotRun,
                reason,
                duration_ms: 0,
                messages: Vec::new(),
                tainted: false,
                note: None,
            },
        };
        match report.status {
            UnitStatus::Compiled => summary.compiled += 1,
            UnitStatus::Fetched(_) => summary.fetched += 1,
            UnitStatus::Failed => summary.failed += 1,
            UnitStatus::NotRun => summary.not_run += 1,
            UnitStatus::Skipped(_) => {}
        }
        summary.tainted += usize::from(report.tainted);
        reports.push(report);
    }
    let keys = plan.keys.as_ref();
    for path in &plan.skipped {
        let why = keys
            .and_then(|keys| keys.get(path))
            .and_then(|state| state.unkeyable.first().cloned())
            .unwrap_or_default();
        reports.push(UnitReport {
            path: path.clone(),
            status: UnitStatus::Skipped(why),
            reason: String::new(),
            duration_ms: 0,
            messages: Vec::new(),
            tainted: false,
            note: None,
        });
    }
    reports.sort_by(|a, b| a.path.cmp(&b.path));
    BuildReport {
        exit: exit_code(end, summary.failed),
        build: facts.build,
        generation: plan.generation,
        environment: Some(facts.environment),
        units: reports,
        summary,
        notes,
        cache: Default::default(),
        setup_ms: facts.setup_ms,
        elapsed_ms: facts.elapsed_ms,
    }
}
