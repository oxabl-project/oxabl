use std::path::Path;

use serde_json::{Value, json};

use super::effective::{Entry, as_json};
use super::{Layers, OverlayOrigin};

fn overlay_line(layers: &Layers) -> String {
    match &layers.overlay_file {
        None => "none".to_string(),
        Some(overlay) => {
            let origin = match overlay.origin {
                OverlayOrigin::Flag => "--config-overlay",
                OverlayOrigin::Environment => "OXABL_BUILD_OVERLAY",
            };
            format!("{} ({origin})", overlay.path.display())
        }
    }
}

fn value_text(value: &Value) -> String {
    match value {
        Value::Null => "(unset)".to_string(),
        other => other.to_string(),
    }
}

/// The effective configuration as text: two header lines, then one
/// `key = value  # source` line for each setting.
pub fn render_config(root: &Path, layers: &Layers, entries: &[Entry]) -> String {
    let base = match layers.base {
        Some(_) => root.join("oxabl.toml").display().to_string(),
        None => "none".to_string(),
    };
    let mut text = format!(
        "# root: {}\n# oxabl.toml: {base}\n# overlay: {}\n",
        root.display(),
        overlay_line(layers)
    );
    let left: Vec<String> = entries
        .iter()
        .map(|entry| format!("{} = {}", entry.key, value_text(&entry.value)))
        .collect();
    let width = left.iter().map(String::len).max().unwrap_or(0);
    for (line, entry) in left.iter().zip(entries) {
        text.push_str(&format!("{line:width$}  # {}\n", entry.source.describe()));
    }
    text
}

/// The effective configuration as one JSON document.
pub fn render_config_json(root: &Path, layers: &Layers, entries: &[Entry]) -> String {
    let document = json!({
        "format": 1,
        "root": root.display().to_string(),
        "oxabl_toml": layers.base.is_some(),
        "overlay": layers.overlay_file.as_ref().map(|o| o.path.display().to_string()),
        "settings": as_json(entries),
    });
    let mut text = serde_json::to_string_pretty(&document).expect("a document is JSON");
    text.push('\n');
    text
}
