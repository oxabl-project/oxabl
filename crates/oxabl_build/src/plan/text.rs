use super::decide::Plan;
use super::reasons::Reason;

fn thousands(count: usize) -> String {
    let digits = count.to_string();
    let mut out = String::new();
    for (i, digit) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i).is_multiple_of(3) {
            out.push(',');
        }
        out.push(digit);
    }
    out
}

/// The plan as text: one line for each unit in the rebuild set, sorted by path
/// (and, with `all`, one for each other unit), then a summary line.
pub fn render(plan: &Plan, committed: bool, all: bool) -> String {
    let mut lines: Vec<(&str, &str, String)> = plan
        .rebuild
        .iter()
        .map(|entry| {
            let mut reason = entry
                .reasons
                .first()
                .map_or_else(String::new, |r| r.describe());
            let unkeyable = entry
                .reasons
                .iter()
                .skip(1)
                .find(|r| matches!(r, Reason::Unkeyable(_)));
            if let Some(unkeyable) = unkeyable {
                reason.push_str(&format!("; {}", unkeyable.describe()));
            }
            ("rebuild", entry.path.as_str(), reason)
        })
        .collect();
    lines.extend(plan.skipped.iter().map(|path| {
        let reason = plan
            .keys
            .as_ref()
            .and_then(|keys| keys.get(path))
            .and_then(|state| state.unkeyable.first().cloned())
            .unwrap_or_default();
        ("skip", path.as_str(), reason)
    }));
    if all {
        lines.extend(
            plan.unchanged
                .iter()
                .map(|path| ("unchanged", path.as_str(), String::new())),
        );
    }
    lines.sort_by(|a, b| a.1.cmp(b.1));
    let width = lines
        .iter()
        .map(|(_, path, _)| path.len())
        .max()
        .unwrap_or(0);
    let mut out = String::new();
    for (action, path, reason) in &lines {
        if reason.is_empty() {
            out.push_str(&format!("{action:<9}  {path}\n"));
        } else {
            out.push_str(&format!("{action:<9}  {path:<width$}  {reason}\n"));
        }
    }
    for path in &plan.unknown {
        out.push_str(&format!("unknown    {path}  unknown-file\n"));
    }
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
    let unkeyable = match unkeyable {
        0 => String::new(),
        count => format!(" ({} unkeyable)", thousands(count)),
    };
    out.push_str(&format!(
        "{} to rebuild{unkeyable}, {} unchanged",
        thousands(plan.rebuild.len()),
        thousands(plan.unchanged.len()),
    ));
    if !plan.skipped.is_empty() {
        out.push_str(&format!(", {} skipped", thousands(plan.skipped.len())));
    }
    if !plan.removed.is_empty() {
        out.push_str(&format!(", {} removed", thousands(plan.removed.len())));
    }
    out.push_str(&format!(", store generation {}", plan.generation));
    if !committed {
        out.push_str(" (not committed)");
    }
    out.push('\n');
    out
}
