//! Canonical startup parameters.

use std::collections::BTreeMap;

/// One startup parameter after canonicalisation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub value: String,
}

/// Parameters that name a connection, a file of this process, a log or a
/// secret. They never reach a hash, a store or an output.
const DROPPED: &[&str] = &[
    "-p",
    "-param",
    "-pf",
    "-b",
    "-q",
    "-T",
    "-db",
    "-ld",
    "-H",
    "-S",
    "-N",
    "-U",
    "-P",
    "-1",
    "-ct",
    "-dt",
    "-clientlog",
    "-logginglevel",
    "-logentrytypes",
    "-logthreshold",
    "-numlogfiles",
    "-basekey",
    "-keyvalue",
];

/// Splits `SESSION:STARTUP-PARAMETERS` into items: `(end .pf)` markers go,
/// and an item that does not start with `-` continues the value before it.
fn items(raw: &str) -> Vec<String> {
    let mut items: Vec<String> = Vec::new();
    for item in raw.split(',') {
        if item == "(end .pf)" || item.is_empty() {
            continue;
        }
        match items.last_mut() {
            Some(last) if !item.starts_with('-') => {
                last.push(',');
                last.push_str(item);
            }
            _ => items.push(item.to_string()),
        }
    }
    items
}

/// The parameters of `raw`, with the dropped ones removed and the last
/// occurrence of a repeated name kept, sorted by name. File values are still
/// as written.
pub fn parse(raw: &str) -> Vec<Parameter> {
    let mut by_name: BTreeMap<String, String> = BTreeMap::new();
    for item in items(raw) {
        let (name, value) = item.split_once(' ').unwrap_or((item.as_str(), ""));
        if DROPPED.contains(&name) {
            continue;
        }
        by_name.insert(name.to_string(), value.to_string());
    }
    by_name
        .into_iter()
        .map(|(name, value)| Parameter { name, value })
        .collect()
}
