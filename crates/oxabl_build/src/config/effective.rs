use oxabl_workspace::WorkspaceConfig;
use serde_json::{Map, Value, json};
use toml::Table;

use super::Layers;

/// Where the effective value of a key came from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Source {
    Flag(&'static str),
    Environment(&'static str),
    Overlay,
    OxablToml,
    Default,
}

impl Source {
    pub fn describe(&self) -> String {
        match self {
            Source::Flag(flag) => format!("flag {flag}"),
            Source::Environment(variable) => format!("environment {variable}"),
            Source::Overlay => "overlay".to_string(),
            Source::OxablToml => "oxabl.toml".to_string(),
            Source::Default => "default".to_string(),
        }
    }
}

/// The settings that a flag or an environment variable overrides.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Overrides {
    pub workers: Option<u32>,
    pub no_cache: bool,
    pub no_remote_cache: bool,
    /// `OXABL_DLC_SOURCES`, when set and not empty.
    pub dlc_sources: Option<String>,
    /// `OXABL_BUILD_CACHE_DIR`, when set and not empty.
    pub cache_dir: Option<String>,
}

/// One effective setting: its dotted key, its value (`null` when unset) and
/// where the value came from.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub key: String,
    pub value: Value,
    pub source: Source,
}

fn sections(config: &WorkspaceConfig) -> Value {
    let workspace = &config.workspace;
    json!({
        "workspace": {
            "name": workspace.name,
            "sources": workspace.sources,
            "schema": workspace.schema,
            "preprocessor": workspace.preprocessor,
        },
        "build": config.build,
    })
}

fn flatten(prefix: &mut Vec<String>, value: &Value, out: &mut Vec<(Vec<String>, Value)>) {
    match value {
        Value::Object(map) if !map.is_empty() => {
            for (key, inner) in map {
                prefix.push(key.clone());
                flatten(prefix, inner, out);
                prefix.pop();
            }
        }
        other => out.push((prefix.clone(), other.clone())),
    }
}

/// The dotted name of `path`; a segment that holds a dot is quoted.
fn dotted(path: &[String]) -> String {
    let quoted: Vec<String> = path
        .iter()
        .map(|segment| match segment.contains('.') {
            true => format!("\"{segment}\""),
            false => segment.clone(),
        })
        .collect();
    quoted.join(".")
}

fn defined_in(table: Option<&Table>, path: &[String]) -> bool {
    let Some(mut current) = table else {
        return false;
    };
    for (index, key) in path.iter().enumerate() {
        match current.get(key) {
            Some(toml::Value::Table(inner)) if index + 1 < path.len() => current = inner,
            Some(_) => return index + 1 == path.len(),
            None => return false,
        }
    }
    false
}

fn source_of(layers: &Layers, path: &[String]) -> Source {
    if defined_in(layers.overlay.as_ref(), path) {
        Source::Overlay
    } else if defined_in(layers.base.as_ref(), path) {
        Source::OxablToml
    } else {
        Source::Default
    }
}

fn set(entries: &mut [Entry], key: &str, value: Value, source: Source) {
    let entry = entries
        .iter_mut()
        .find(|entry| entry.key == key)
        .unwrap_or_else(|| panic!("`{key}` is a setting"));
    entry.value = value;
    entry.source = source;
}

/// Every setting that `oxabl build` reads from `[workspace]` (`name`,
/// `sources`, `schema`, `preprocessor`) and `[build]`, sorted by key, each with the layer that decided it. A flag beats an
/// environment variable, an environment variable beats the overlay, the overlay
/// beats `oxabl.toml`, and `oxabl.toml` beats the default.
pub fn effective(config: &WorkspaceConfig, layers: &Layers, overrides: &Overrides) -> Vec<Entry> {
    let mut leaves = Vec::new();
    flatten(&mut Vec::new(), &sections(config), &mut leaves);
    let mut entries: Vec<Entry> = leaves
        .into_iter()
        .map(|(path, value)| Entry {
            key: dotted(&path),
            source: source_of(layers, &path),
            value,
        })
        .collect();
    if let Some(dlc) = &overrides.dlc_sources {
        let value = Value::String(dlc.clone());
        set(
            &mut entries,
            "workspace.sources.dlc",
            value,
            Source::Environment("OXABL_DLC_SOURCES"),
        );
    }
    if let Some(dir) = &overrides.cache_dir {
        let value = Value::String(dir.clone());
        set(
            &mut entries,
            "build.cache.local",
            value,
            Source::Environment("OXABL_BUILD_CACHE_DIR"),
        );
    }
    if let Some(workers) = overrides.workers {
        set(
            &mut entries,
            "build.workers.count",
            json!(workers),
            Source::Flag("--workers"),
        );
    }
    if overrides.no_cache {
        set(
            &mut entries,
            "build.cache.enabled",
            json!(false),
            Source::Flag("--no-cache"),
        );
    }
    if overrides.no_remote_cache {
        set(
            &mut entries,
            "build.cache.remote",
            json!(false),
            Source::Flag("--no-remote-cache"),
        );
    }
    entries
}

/// The verification that the build would run: not a key of `oxabl.toml`, but
/// what its `[build.verify]` and the cache mode make of it. The settings it
/// reads are entries of [`effective`] too.
pub fn verification(
    policy: &crate::verify::policy::Policy,
    environment: &crate::cache::Environment,
) -> Entry {
    let source = match (&environment.cache_mode, &environment.ci) {
        (Some(_), _) => Source::Environment("OXABL_CACHE_MODE"),
        (None, Some(_)) => Source::Environment("CI"),
        (None, None) => Source::Default,
    };
    Entry {
        key: "verification.mode".to_string(),
        value: Value::String(policy.mode.name().to_string()),
        source,
    }
}

/// The entries as one JSON object: the keys are the dotted names and each
/// value holds `value` and `source`.
pub fn as_json(entries: &[Entry]) -> Value {
    let mut map = Map::new();
    for entry in entries {
        map.insert(
            entry.key.clone(),
            json!({ "value": entry.value, "source": entry.source.describe() }),
        );
    }
    Value::Object(map)
}
