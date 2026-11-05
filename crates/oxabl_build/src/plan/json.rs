use serde_json::{Value, json};

use super::decide::Plan;
use super::reasons::Reason;

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
        Reason::DependencyChanged(files) => json!({ "kind": reason.kind(), "paths": files }),
        Reason::ClosureChanged { gained, lost } => json!({
            "kind": reason.kind(),
            "paths": gained.iter().chain(lost).collect::<Vec<_>>(),
            "gained": gained,
            "lost": lost,
        }),
    }
}

fn unit(path: &str, action: &str, reasons: Vec<Value>) -> Value {
    json!({
        "path": path,
        "compile_name": null,
        "action": action,
        "reasons": reasons,
        "key": null,
        "unkeyable": [],
    })
}

/// The plan as one JSON document, units sorted by path. Up-to-date units are
/// listed only with `all`.
pub fn render_json(plan: &Plan, committed: bool, all: bool) -> String {
    let mut units: Vec<(&str, Value)> = plan
        .rebuild
        .iter()
        .map(|entry| {
            let reasons = entry.reasons.iter().map(reason).collect();
            (entry.path.as_str(), unit(&entry.path, "rebuild", reasons))
        })
        .collect();
    if all {
        units.extend(
            plan.unchanged
                .iter()
                .map(|path| (path.as_str(), unit(path, "up-to-date", Vec::new()))),
        );
        units.sort_by(|a, b| a.0.cmp(b.0));
    }
    let units: Vec<Value> = units.into_iter().map(|(_, value)| value).collect();
    let document = json!({
        "format": FORMAT,
        "environment": { "source": "none", "identity": null, "at": null },
        "store": { "generation": plan.generation, "committed": committed },
        "units": units,
        "removed": plan.removed,
        "unknown_files": plan.unknown,
        "summary": {
            "rebuild": plan.rebuild.len(),
            "unkeyable": 0,
            "up_to_date": plan.unchanged.len(),
            "skipped": 0,
        },
    });
    let mut text = serde_json::to_string_pretty(&document).expect("a plan is valid JSON");
    text.push('\n');
    text
}
