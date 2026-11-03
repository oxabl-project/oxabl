use serde_json::{Value, json};

use super::decide::Plan;
use super::reasons::Reason;
use super::time::now_rfc3339;
use crate::key::compute::UnitKey;

/// The `format` number of the plan document.
pub const FORMAT: u32 = 1;

fn reason(reason: &Reason) -> Value {
    match reason {
        Reason::New | Reason::SourceChanged => json!({ "kind": reason.kind(), "paths": [] }),
        Reason::Named(named) => json!({
            "kind": reason.kind(),
            "paths": named.iter().map(|n| &n.file).collect::<Vec<_>>(),
            "via": named.iter().map(|n| &n.via).collect::<Vec<_>>(),
        }),
        Reason::Unkeyable(rules) => json!({
            "kind": reason.kind(),
            "paths": [],
            "details": rules,
        }),
        Reason::DependencyChanged(files) => json!({ "kind": reason.kind(), "paths": files }),
        Reason::ClosureChanged { gained, lost } => json!({
            "kind": reason.kind(),
            "paths": gained.iter().chain(lost).collect::<Vec<_>>(),
            "gained": gained,
            "lost": lost,
        }),
    }
}

fn hex(bytes: &[u8; 32]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// The reason no unit has a key when the environment was not probed.
const ENVIRONMENT_UNKNOWN: &str = "environment-unknown";

fn unit(plan: &Plan, path: &str, action: &str, reasons: Vec<Value>) -> Value {
    let state: Option<&UnitKey> = plan.keys.as_ref().and_then(|keys| keys.get(path));
    let unkeyable = match (&plan.keys, state) {
        (None, _) => vec![ENVIRONMENT_UNKNOWN.to_string()],
        (Some(_), Some(state)) => state.unkeyable.clone(),
        (Some(_), None) => Vec::new(),
    };
    let mut unit = json!({
        "path": path,
        "compile_name": state.and_then(|s| s.compile_name.as_deref()),
        "action": action,
        "reasons": reasons,
        "key": state.and_then(|s| s.key.as_ref()).map(hex),
        "unkeyable": unkeyable,
    });
    if plan.keys.is_some() {
        unit["propath"] = json!(state.and_then(|s| s.propath));
    }
    unit
}

fn environment(plan: &Plan) -> Value {
    match plan.keys.as_ref().and_then(|keys| keys.keyed.environment) {
        Some(identity) => json!({
            "source": "probe",
            "identity": identity.to_hex().to_string(),
            "at": now_rfc3339(),
        }),
        None => json!({ "source": "none", "identity": null, "at": null }),
    }
}

fn propaths(plan: &Plan) -> Vec<Value> {
    let Some(keys) = &plan.keys else {
        return Vec::new();
    };
    keys.keyed
        .propaths
        .iter()
        .map(|propath| {
            let entries: Vec<Value> = propath
                .iter()
                .map(|(entry, digest)| json!({ "entry": entry, "digest": digest }))
                .collect();
            Value::Array(entries)
        })
        .collect()
}

/// The plan as one JSON document, units sorted by path. Up-to-date units are
/// listed only with `all`.
pub fn render_json(plan: &Plan, committed: bool, all: bool) -> String {
    let mut units: Vec<(&str, Value)> = plan
        .rebuild
        .iter()
        .map(|entry| {
            let reasons = entry.reasons.iter().map(reason).collect();
            (
                entry.path.as_str(),
                unit(plan, &entry.path, "rebuild", reasons),
            )
        })
        .collect();
    units.extend(
        plan.skipped
            .iter()
            .map(|path| (path.as_str(), unit(plan, path, "skip", Vec::new()))),
    );
    if all {
        units.extend(
            plan.unchanged
                .iter()
                .map(|path| (path.as_str(), unit(plan, path, "up-to-date", Vec::new()))),
        );
    }
    units.sort_by(|a, b| a.0.cmp(b.0));
    let unkeyable = plan
        .rebuild
        .iter()
        .filter(|entry| {
            entry
                .reasons
                .iter()
                .any(|r| matches!(r, Reason::Unkeyable(_)))
        })
        .count();
    let units: Vec<Value> = units.into_iter().map(|(_, value)| value).collect();
    let mut document = json!({
        "format": FORMAT,
        "environment": environment(plan),
        "store": { "generation": plan.generation, "committed": committed },
        "units": units,
        "removed": plan.removed,
        "unknown_files": plan.unknown,
        "summary": {
            "rebuild": plan.rebuild.len(),
            "unkeyable": unkeyable,
            "up_to_date": plan.unchanged.len(),
            "skipped": plan.skipped.len(),
        },
    });
    if plan.keys.is_some() {
        document["propaths"] = Value::Array(propaths(plan));
    }
    let mut text = serde_json::to_string_pretty(&document).expect("a plan is valid JSON");
    text.push('\n');
    text
}
