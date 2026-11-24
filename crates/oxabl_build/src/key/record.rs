//! The environment as a stored record, and the fields in which two differ.

use std::collections::BTreeMap;

use serde_json::Value;

use super::environment::Environment;

/// The facts that enter the environment identity, by the name the record shows.
const FACTS: [&str; 14] = [
    "proversion",
    "opsys",
    "window_system",
    "batch_mode",
    "process_architecture",
    "cpinternal",
    "cpstream",
    "cpcoll",
    "cpcase",
    "date_format",
    "year_offset",
    "numeric_decimal_point",
    "numeric_separator",
    "dlc_version",
];

fn fields(environment: &Environment) -> BTreeMap<String, String> {
    let mut fields = BTreeMap::new();
    for name in FACTS {
        fields.insert(
            name.to_string(),
            environment.facts.get(name).trim_end().to_string(),
        );
    }
    for parameter in &environment.parameters {
        fields.insert(
            format!("parameter: {}", parameter.name),
            parameter.value.clone(),
        );
    }
    for database in &environment.databases {
        let ldb = database.info.ldb.to_ascii_lowercase();
        let mut aliases: Vec<String> = database
            .info
            .aliases
            .iter()
            .map(|alias| alias.to_ascii_lowercase())
            .collect();
        aliases.sort();
        fields.insert(
            format!("database: {ldb}"),
            format!("{} {}", database.info.dbtype, aliases.join(",")),
        );
        fields.insert(
            format!("schema: {ldb}"),
            database.schema.identity().to_hex().to_string(),
        );
    }
    for library in &environment.libraries {
        fields.insert(
            format!("library: {}", library.entry),
            library.digest.word().to_string(),
        );
    }
    fields
}

/// The record of `environment`: a JSON object of readable fields, without
/// anything a startup parameter dropped for secrecy.
pub fn record(environment: &Environment) -> String {
    serde_json::to_string(&fields(environment)).expect("a record is valid JSON")
}

/// The names of the fields in which the stored `before` and the `now` record
/// differ, sorted. A record that cannot be read differs in every field.
pub fn differences(before: &str, now: &str) -> Vec<String> {
    let read = |text: &str| -> Option<BTreeMap<String, String>> {
        match serde_json::from_str::<Value>(text).ok()? {
            Value::Object(map) => map
                .into_iter()
                .map(|(key, value)| Some((key, value.as_str()?.to_string())))
                .collect(),
            _ => None,
        }
    };
    let (Some(before), Some(now)) = (read(before), read(now)) else {
        return vec!["environment".to_string()];
    };
    let mut names: Vec<&String> = before.keys().chain(now.keys()).collect();
    names.sort();
    names.dedup();
    names
        .into_iter()
        .filter(|name| before.get(*name) != now.get(*name))
        .cloned()
        .collect()
}
