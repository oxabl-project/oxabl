use toml::{Table, Value};

use crate::error::BuildError;

/// The `[workspace]` keys an overlay may set.
const OVERLAY_WORKSPACE_KEYS: [&str; 4] = ["name", "sources", "preprocessor", "schema"];

/// The names of the remote cache's root of trust. Only `.oxabl/cache.toml` sets
/// them.
const TRUST_KEYS: [&str; 4] = ["tenant", "cache", "owner_keys", "min_roster_version"];

/// Name fragments of a key that holds a secret, compared without `-` and `_`.
const SECRET_FRAGMENTS: [&str; 8] = [
    "password",
    "passwd",
    "secret",
    "token",
    "apikey",
    "privatekey",
    "accesskey",
    "credential",
];

fn usage(message: String) -> BuildError {
    BuildError::Usage(message)
}

/// Refuse a key an overlay may not set: anything outside `[workspace]`
/// (`name`, `sources`, `preprocessor`, `schema`) and `[build]`.
pub fn check_overlay_keys(overlay: &Table, file: &str) -> Result<(), BuildError> {
    for (key, value) in overlay {
        match (key.as_str(), value) {
            ("build", _) => {}
            ("workspace", Value::Table(workspace)) => {
                for inner in workspace.keys() {
                    if !OVERLAY_WORKSPACE_KEYS.contains(&inner.as_str()) {
                        return Err(usage(format!(
                            "{file}: [workspace] `{inner}` is not a key an overlay may set; an overlay sets [workspace] name, sources, preprocessor and schema, and [build]"
                        )));
                    }
                }
            }
            _ => {
                return Err(usage(format!(
                    "{file}: `{key}` is not a key an overlay may set; an overlay sets [workspace] name, sources, preprocessor and schema, and [build]"
                )));
            }
        }
    }
    Ok(())
}

/// Refuse the keys of the remote cache's root of trust under `[build.cache]`,
/// at any depth of that table.
pub fn check_trust_keys(document: &Table, file: &str) -> Result<(), BuildError> {
    let Some(Value::Table(build)) = document.get("build") else {
        return Ok(());
    };
    let Some(Value::Table(cache)) = build.get("cache") else {
        return Ok(());
    };
    for key in cache.keys() {
        if TRUST_KEYS.contains(&key.as_str()) || key == "remote" && cache[key].is_table() {
            return Err(usage(format!(
                "{file}: [build.cache] `{key}` belongs to the remote cache's root of trust; only .oxabl/cache.toml sets it"
            )));
        }
    }
    Ok(())
}

/// Refuse a key whose name says it holds a secret, at any depth.
pub fn check_secret_keys(document: &Table, file: &str) -> Result<(), BuildError> {
    fn walk(table: &Table, path: &mut Vec<String>, file: &str) -> Result<(), BuildError> {
        for (key, value) in table {
            path.push(key.clone());
            let folded: String = key
                .chars()
                .filter(|c| !matches!(c, '-' | '_'))
                .collect::<String>()
                .to_ascii_lowercase();
            if SECRET_FRAGMENTS.iter().any(|f| folded.contains(f)) {
                return Err(usage(format!(
                    "{file}: `{}` looks like a secret; oxabl reads no secret from a file (put it in a .pf file, a wrapper script or the environment)",
                    path.join(".")
                )));
            }
            if let Value::Table(inner) = value {
                walk(inner, path, file)?;
            }
            path.pop();
        }
        Ok(())
    }
    walk(document, &mut Vec::new(), file)
}
