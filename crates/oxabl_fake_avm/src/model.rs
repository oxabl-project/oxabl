//! The environment a test describes in the JSON file of `FAKE_AVM_ENV`.

use std::collections::BTreeMap;
use std::path::Path;

use serde::Deserialize;

/// One startup parameter: a plain `-name value` item, or a `.pf` file that
/// the AVM expands in place.
#[derive(Deserialize)]
#[serde(untagged)]
pub enum Parameter {
    Plain(String),
    ParameterFile { pf: String, contents: Vec<String> },
}

#[derive(Deserialize)]
pub struct Database {
    pub ldb: String,
    #[serde(default = "progress")]
    pub dbtype: String,
    #[serde(default)]
    pub aliases: Vec<String>,
    #[serde(default)]
    pub schema: Vec<Vec<String>>,
    /// The `_Db-guid` the fake reports; default `guid-<ldb>`.
    pub guid: Option<String>,
    /// The `_DbStatus-CacheStamp` the fake reports; default `stamp-1`.
    pub stamp: Option<String>,
}

fn progress() -> String {
    "PROGRESS".to_string()
}

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct Model {
    /// Where `$DLC/...` paths resolve for `HASH`; beats `FAKE_AVM_DLC`.
    pub dlc_dir: Option<String>,
    pub env: BTreeMap<String, serde_json::Value>,
    pub parameters: Vec<Parameter>,
    pub databases: Vec<Database>,
}

const DEFAULTS: &[(&str, &str)] = &[
    ("program", "1"),
    ("dlc", "/usr/dlc"),
    (
        "dlc_version",
        "OpenEdge Release 12.8.9 as of Mon Jan  1 00:00:00 EST 2026",
    ),
    ("proversion", "12.8"),
    ("opsys", "UNIX"),
    ("window_system", "TTY"),
    ("batch_mode", "yes"),
    ("process_architecture", "64"),
    ("cpinternal", "UTF-8"),
    ("cpstream", "UTF-8"),
    ("cpcoll", "BASIC"),
    ("cpcase", "BASIC"),
    ("date_format", "mdy"),
    ("year_offset", "1950"),
    ("numeric_decimal_point", "."),
    ("numeric_separator", ","),
    ("propath", ""),
];

impl Model {
    pub fn load(path: Option<&Path>) -> Result<Model, String> {
        let Some(path) = path else {
            return Ok(Model::default());
        };
        let text = std::fs::read_to_string(path)
            .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
        serde_json::from_str(&text).map_err(|error| format!("bad {}: {error}", path.display()))
    }

    /// The value of one `ENV` field: the file's value, else the default.
    pub fn field(&self, name: &str) -> String {
        match self.env.get(name) {
            Some(serde_json::Value::String(text)) => text.clone(),
            Some(serde_json::Value::Array(items)) => items
                .iter()
                .filter_map(|item| item.as_str())
                .collect::<Vec<_>>()
                .join(","),
            Some(other) => other.to_string(),
            None => DEFAULTS
                .iter()
                .find(|(key, _)| *key == name)
                .map(|(_, value)| value.to_string())
                .unwrap_or_default(),
        }
    }

    /// `SESSION:STARTUP-PARAMETERS`, with each `.pf` expanded like the AVM does.
    pub fn startup_parameters(&self) -> String {
        let mut items = Vec::new();
        for parameter in &self.parameters {
            match parameter {
                Parameter::Plain(item) => items.push(item.clone()),
                Parameter::ParameterFile { pf, contents } => {
                    items.push(format!("-pf {pf}"));
                    items.extend(contents.iter().cloned());
                    items.push("(end .pf)".to_string());
                }
            }
        }
        items.join(",")
    }
}
