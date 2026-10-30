//! The entry-point file of the `security` rules: the remotely reachable
//! entries (REST routes, WebSpeed programs, service methods) that
//! reachability starts from.
//!
//! ```json
//! {
//!   "version": 1,
//!   "entries": [
//!     { "kind": "REST", "name": "GET /orders/{id}",
//!       "program": "web/orders.p", "procedure": "get-order" },
//!     { "kind": "WebSpeed", "name": "/report.w", "program": "web/report.w" },
//!     { "kind": "REST", "name": "POST /orders",
//!       "class": "app.OrderService", "method": "Create", "auth": true }
//!   ]
//! }
//! ```
//!
//! Each entry names a `program` (as a `RUN` names it, optionally with one of
//! its internal procedures) or a `class` (optionally with one method). `kind`
//! and `name` only label findings. `auth` says that the framework authorises
//! the caller before the entry runs. An unknown key is an error.

use oxabl_common::{EntryPoint, EntryTarget};
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct File {
    version: u32,
    entries: Vec<Entry>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Entry {
    #[serde(default)]
    kind: String,
    #[serde(default)]
    name: String,
    program: Option<String>,
    procedure: Option<String>,
    class: Option<String>,
    method: Option<String>,
    #[serde(default)]
    auth: bool,
}

/// The entries of an entry-point file, or what is wrong with it.
pub fn parse_entry_points(text: &str) -> Result<Vec<EntryPoint>, String> {
    let file: File = serde_json::from_str(text).map_err(|e| e.to_string())?;
    if file.version != 1 {
        return Err(format!(
            "version {} is not supported; the version is 1",
            file.version
        ));
    }
    file.entries
        .into_iter()
        .enumerate()
        .map(|(i, e)| {
            let nonempty =
                |v: Option<String>| v.map(|s| s.trim().to_string()).filter(|s| !s.is_empty());
            let target = match (
                nonempty(e.program),
                nonempty(e.procedure),
                nonempty(e.class),
                nonempty(e.method),
            ) {
                (Some(program), procedure, None, None) => {
                    EntryTarget::Program { program, procedure }
                }
                (None, None, Some(class), method) => EntryTarget::Class { class, method },
                (Some(_), _, Some(_), _) => {
                    return Err(format!(
                        "entry {i}: name a `program` or a `class`, not both"
                    ));
                }
                (None, Some(_), _, _) => {
                    return Err(format!("entry {i}: `procedure` needs a `program`"));
                }
                (_, _, None, Some(_)) => {
                    return Err(format!("entry {i}: `method` needs a `class`"));
                }
                (None, None, None, None) => {
                    return Err(format!("entry {i}: name a `program` or a `class`"));
                }
            };
            Ok(EntryPoint {
                kind: e.kind,
                name: e.name,
                target,
                auth: e.auth,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_file_lists_program_procedure_and_class_entries() {
        let entries = parse_entry_points(
            r#"{
              "version": 1,
              "entries": [
                { "kind": "REST", "name": "GET /orders/{id}",
                  "program": "web/orders.p", "procedure": "get-order" },
                { "kind": "WebSpeed", "name": "/report.w", "program": "web/report.w" },
                { "kind": "REST", "name": "POST /orders",
                  "class": "app.OrderService", "method": "Create", "auth": true },
                { "class": "app.Health" }
              ]
            }"#,
        )
        .unwrap();
        assert_eq!(entries.len(), 4);
        assert_eq!(entries[0].label(), "REST GET /orders/{id}");
        assert_eq!(
            entries[0].target,
            EntryTarget::Program {
                program: "web/orders.p".into(),
                procedure: Some("get-order".into())
            }
        );
        assert!(!entries[0].auth);
        assert_eq!(entries[1].target.describe(), "web/report.w");
        assert_eq!(entries[2].target.describe(), "app.OrderService:Create");
        assert!(entries[2].auth);
        assert_eq!(
            entries[3].target,
            EntryTarget::Class {
                class: "app.Health".into(),
                method: None
            }
        );
    }

    #[test]
    fn a_wrong_entry_is_an_error_that_names_it() {
        for (text, needle) in [
            (r#"{"version": 2, "entries": []}"#, "version 2"),
            (
                r#"{"version": 1, "entries": [{"program": "a.p", "class": "B"}]}"#,
                "not both",
            ),
            (
                r#"{"version": 1, "entries": [{"procedure": "p"}]}"#,
                "needs a `program`",
            ),
            (
                r#"{"version": 1, "entries": [{"program": "a.p", "method": "m"}]}"#,
                "needs a `class`",
            ),
            (
                r#"{"version": 1, "entries": [{"kind": "REST"}]}"#,
                "entry 0",
            ),
            (
                r#"{"version": 1, "entries": [{"program": "a.p", "route": "x"}]}"#,
                "route",
            ),
            (r#"{"version": 1}"#, "entries"),
        ] {
            let err = parse_entry_points(text).unwrap_err();
            assert!(err.contains(needle), "{text}: {err}");
        }
    }
}
