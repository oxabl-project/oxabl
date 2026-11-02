use super::decide::Plan;

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
            let reason = entry
                .reasons
                .first()
                .map_or_else(String::new, |r| r.describe());
            ("rebuild", entry.path.as_str(), reason)
        })
        .collect();
    if all {
        lines.extend(
            plan.unchanged
                .iter()
                .map(|path| ("unchanged", path.as_str(), String::new())),
        );
        lines.sort_by(|a, b| a.1.cmp(b.1));
    }
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
    out.push_str(&format!(
        "{} to rebuild, {} unchanged",
        thousands(plan.rebuild.len()),
        thousands(plan.unchanged.len()),
    ));
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
