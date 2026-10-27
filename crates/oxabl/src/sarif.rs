//! `oxabl check --format sarif`: the run as one SARIF 2.1.0 log.
//!
//! The log has one run. `tool.driver.rules` lists every rule of the registry,
//! plus a bare entry for any other code a result carries (parse errors, for
//! example). A suppressed finding stays in `results` with an `inSource`
//! suppression, so a viewer can show it with its reason.
//!
//! A security finding with a taint path carries it as one `codeFlows` thread:
//! each step in order, source first, ending at the sink. One that entry points
//! reach carries `properties.reachability` (the status, the entries, the
//! path) and a `relatedLocations` entry for each call on that path.
//!
//! Columns count UTF-16 code units, which is the SARIF default.

use oxabl_lint::{Registry, Rule};
use serde_json::{Map, Value, json};

const INFORMATION_URI: &str = "https://github.com/oxabl-project/oxabl";

/// A finding's location in the checked file. All numbers are 1-based; the end
/// column is the one after the last character.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Region {
    /// The region of byte span `start..end` in `source`.
    pub fn of_span(source: &str, start: u32, end: u32) -> Region {
        let (start_line, start_column) = locate(source, start as usize);
        let (end_line, end_column) = locate(source, (end as usize).max(start as usize));
        Region {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

/// 1-based line and UTF-16 column of byte offset `at`.
fn locate(source: &str, at: usize) -> (usize, usize) {
    let mut at = at.min(source.len());
    while !source.is_char_boundary(at) {
        at -= 1;
    }
    let before = &source[..at];
    let line = before.matches('\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let column = before[line_start..].encode_utf16().count() + 1;
    (line, column)
}

/// One `results[]` entry.
#[derive(Debug, Clone)]
pub struct Finding {
    pub rule_id: String,
    pub severity: &'static str,
    pub text: String,
    pub uri: String,
    pub region: Option<Region>,
    pub fingerprint: String,
    /// `new` or `unchanged`, set only when a baseline is in use.
    pub baseline_state: Option<&'static str>,
    /// The reason of the `oxabl-ignore` comment that suppresses the finding.
    pub suppressed: Option<String>,
    /// The steps of a taint path, source first, without the sink.
    pub flow: Vec<FlowStep>,
    /// How the entry points reach the finding.
    pub reach: Option<oxabl_common::Reachability>,
    /// The calls on that path.
    pub related: Vec<Related>,
}

/// A call on the reachability path of a finding.
#[derive(Debug, Clone)]
pub struct Related {
    pub uri: String,
    /// 1-based.
    pub line: usize,
    pub message: String,
}

/// One step of a taint path.
#[derive(Debug, Clone)]
pub struct FlowStep {
    pub region: Region,
    pub message: String,
}

/// The taint path of a security finding: its labels that sit in the checked
/// file, in order. Other findings have none: their labels are not a path.
pub fn flow_of(
    registry: &Registry,
    d: &oxabl_common::Diagnostic,
    source: &str,
    file: oxabl_common::FileId,
) -> Vec<FlowStep> {
    let security = registry
        .rule(d.code.0)
        .is_some_and(|r| r.meta().security_severity.is_some());
    if !security {
        return Vec::new();
    }
    d.labels
        .iter()
        .filter(|l| l.span.file == file)
        .map(|l| FlowStep {
            region: Region::of_span(source, l.span.span.start, l.span.span.end),
            message: l.message.clone(),
        })
        .collect()
}

/// A run-level problem that is not a finding: a file that failed, or one that
/// would be reformatted.
#[derive(Debug, Clone)]
pub struct Notification {
    pub level: &'static str,
    pub text: String,
}

/// The SARIF `level` for a diagnostic severity.
pub fn level(severity: &str) -> &'static str {
    match severity {
        "error" => "error",
        "warning" => "warning",
        _ => "note",
    }
}

/// A relative URI for a `/`-separated path: bytes outside the unreserved set
/// are percent-encoded.
pub fn uri(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    for b in path.replace('\\', "/").bytes() {
        if b.is_ascii_alphanumeric() || b"-._~/".contains(&b) {
            out.push(b as char);
        } else {
            out.push_str(&format!("%{b:02X}"));
        }
    }
    out
}

fn rule_descriptor(rule: &dyn Rule) -> Value {
    let meta = rule.meta();
    let mut tags = vec![Value::from(meta.group)];
    if meta.stability == oxabl_lint::Stability::Preview {
        tags.push(Value::from("preview"));
    }
    let mut properties = Map::new();
    properties.insert("tags".into(), Value::Array(tags));
    if let Some(severity) = meta.security_severity {
        properties.insert("security-severity".into(), Value::from(severity));
    }
    json!({
        "id": meta.code,
        "name": meta.name,
        "shortDescription": { "text": meta.description },
        "help": { "text": meta.help.unwrap_or(meta.description) },
        "defaultConfiguration": { "level": level(meta.default_severity.as_str()) },
        "properties": Value::Object(properties),
    })
}

fn region(r: Region) -> Value {
    json!({
        "startLine": r.start_line,
        "startColumn": r.start_column,
        "endLine": r.end_line,
        "endColumn": r.end_column,
    })
}

fn result(finding: &Finding) -> Value {
    let mut physical = Map::new();
    physical.insert(
        "artifactLocation".into(),
        json!({ "uri": finding.uri, "uriBaseId": "%SRCROOT%" }),
    );
    if let Some(r) = finding.region {
        physical.insert("region".into(), region(r));
    }
    let mut out = json!({
        "ruleId": finding.rule_id,
        "level": finding.severity,
        "message": { "text": finding.text },
        "locations": [{ "physicalLocation": Value::Object(physical.clone()) }],
        "partialFingerprints": { "oxablFinding/v1": finding.fingerprint },
    });
    if let Some(state) = finding.baseline_state {
        out["baselineState"] = state.into();
    }
    if let Some(reason) = &finding.suppressed {
        out["suppressions"] = json!([{ "kind": "inSource", "justification": reason }]);
    }
    if let Some(reach) = &finding.reach {
        out["properties"] = json!({ "reachability": reachability(reach) });
    }
    if !finding.related.is_empty() {
        out["relatedLocations"] = finding
            .related
            .iter()
            .enumerate()
            .map(|(i, r)| {
                json!({
                    "id": i,
                    "physicalLocation": {
                        "artifactLocation": { "uri": r.uri, "uriBaseId": "%SRCROOT%" },
                        "region": { "startLine": r.line },
                    },
                    "message": { "text": r.message },
                })
            })
            .collect();
    }
    if !finding.flow.is_empty() {
        let artifact = json!({ "uri": finding.uri, "uriBaseId": "%SRCROOT%" });
        let mut locations: Vec<Value> = finding
            .flow
            .iter()
            .map(|step| {
                json!({ "location": {
                    "physicalLocation": {
                        "artifactLocation": artifact,
                        "region": region(step.region),
                    },
                    "message": { "text": step.message },
                } })
            })
            .collect();
        locations.push(json!({ "location": {
            "physicalLocation": Value::Object(physical),
            "message": { "text": "sink" },
        } }));
        out["codeFlows"] = json!([{ "threadFlows": [{ "locations": locations }] }]);
    }
    out
}

/// `properties.reachability` of a result.
fn reachability(r: &oxabl_common::Reachability) -> Value {
    let entry = |e: &oxabl_common::ReachEntry| json!({ "label": e.label, "target": e.target });
    let mut out = json!({
        "status": r.status.as_str(),
        "entries": r.entries.iter().map(entry).collect::<Vec<_>>(),
    });
    if r.more_entries {
        out["moreEntries"] = true.into();
    }
    if !r.unknown.is_empty() {
        out["unknownCalls"] = r.unknown.clone().into();
    }
    if let Some(p) = &r.path {
        out["path"] = json!({
            "entry": entry(&p.entry),
            "calls": p.steps.iter().map(|s| s.routine.clone()).collect::<Vec<_>>(),
        });
    }
    out
}

/// The `file:` URI of the directory `root`, ending in `/`.
pub fn root_uri(root: &std::path::Path) -> String {
    let path = root.to_string_lossy().replace('\\', "/");
    let path = path.trim_end_matches('/');
    // A drive letter keeps its colon: `file:///C:/src/`.
    let (drive, rest) = match path.as_bytes() {
        [d, b':', ..] if d.is_ascii_alphabetic() => path.split_at(2),
        _ => ("", path),
    };
    let lead = if path.starts_with('/') { "" } else { "/" };
    format!("file://{lead}{drive}{}/", uri(rest))
}

/// The log for `findings`, with rules from `registry`. `root` is the
/// directory every result's `%SRCROOT%` URI is relative to.
pub fn document(
    registry: &Registry,
    findings: &[Finding],
    notifications: &[Notification],
    root: Option<&std::path::Path>,
) -> Value {
    let mut rules: Vec<Value> = registry.rules().map(rule_descriptor).collect();
    let mut extra: Vec<&str> = Vec::new();
    for f in findings {
        let known = registry.rules().any(|r| r.meta().code == f.rule_id);
        if !known && !extra.contains(&f.rule_id.as_str()) {
            extra.push(&f.rule_id);
        }
    }
    extra.sort_unstable();
    rules.extend(extra.into_iter().map(|id| {
        json!({
            "id": id,
            "shortDescription": { "text": id },
            "defaultConfiguration": { "level": "error" },
            "properties": { "tags": ["core"] },
        })
    }));

    let mut invocation = json!({
        "executionSuccessful": !notifications.iter().any(|n| n.level == "error"),
    });
    if !notifications.is_empty() {
        invocation["toolExecutionNotifications"] = notifications
            .iter()
            .map(|n| json!({ "level": n.level, "message": { "text": n.text } }))
            .collect();
    }

    let mut run = json!({
        "tool": { "driver": {
            "name": "oxabl",
            "version": env!("CARGO_PKG_VERSION"),
            "informationUri": INFORMATION_URI,
            "rules": rules,
        } },
        "columnKind": "utf16CodeUnits",
        "invocations": [invocation],
        "results": findings.iter().map(result).collect::<Vec<_>>(),
    });
    if let Some(root) = root {
        run["originalUriBaseIds"] = json!({ "%SRCROOT%": { "uri": root_uri(root) } });
    }
    json!({
        "$schema": "https://json.schemastore.org/sarif-2.1.0.json",
        "version": "2.1.0",
        "runs": [run],
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_common::Severity;
    use oxabl_lint::{FnRule, RuleMeta};

    #[test]
    fn the_root_uri_is_a_file_uri_of_a_directory() {
        assert_eq!(
            root_uri(std::path::Path::new("/w s/repo")),
            "file:///w%20s/repo/"
        );
        assert_eq!(
            root_uri(std::path::Path::new("C:\\src\\repo\\")),
            "file:///C:/src/repo/"
        );
    }

    fn none(
        _: &[oxabl_ast::Statement],
        _: &oxabl_semantic::Semantic,
        _: &oxabl_semantic::AnalysisContext,
    ) -> Vec<oxabl_common::Diagnostic> {
        Vec::new()
    }

    #[test]
    fn a_rule_with_a_security_severity_carries_it_and_others_do_not() {
        let mut registry = Registry::empty();
        registry
            .register(FnRule::new(
                RuleMeta::new("SEC001", "sec-rule", "default", "A security rule.")
                    .with_default_severity(Severity::Error)
                    .with_help("Use the safe pattern.")
                    .with_security_severity("9.1"),
                none,
            ))
            .unwrap();
        registry
            .register(FnRule::new(
                RuleMeta::new("PLAIN01", "plain-rule", "default", "A plain rule."),
                none,
            ))
            .unwrap();
        let doc = document(&registry, &[], &[], None);
        let rules = doc["runs"][0]["tool"]["driver"]["rules"]
            .as_array()
            .unwrap();
        assert_eq!(rules[0]["properties"]["security-severity"], "9.1");
        assert_eq!(rules[0]["help"]["text"], "Use the safe pattern.");
        assert_eq!(rules[0]["defaultConfiguration"]["level"], "error");
        assert!(rules[1]["properties"].get("security-severity").is_none());
        assert_eq!(rules[1]["help"]["text"], "A plain rule.");
    }

    #[test]
    fn a_code_outside_the_registry_gets_a_bare_rule() {
        let finding = Finding {
            rule_id: "PARSE001".into(),
            severity: "error",
            text: "bad".into(),
            uri: "a.p".into(),
            region: None,
            fingerprint: "x".into(),
            baseline_state: None,
            suppressed: None,
            flow: Vec::new(),
            reach: None,
            related: Vec::new(),
        };
        let doc = document(&Registry::empty(), &[finding], &[], None);
        assert!(doc["runs"][0]["results"][0].get("properties").is_none());
        let rules = doc["runs"][0]["tool"]["driver"]["rules"]
            .as_array()
            .unwrap();
        assert_eq!(rules[0]["id"], "PARSE001");
    }

    #[test]
    fn a_reachable_finding_carries_its_entries_path_and_related_calls() {
        use oxabl_common::{ReachEntry, ReachPath, ReachStatus, ReachStep, Reachability};
        let entry = ReachEntry {
            label: "REST GET /orders/{id}".into(),
            target: "web/orders.p:get-order".into(),
        };
        let mut reach = Reachability::of(ReachStatus::Reachable, Vec::new());
        reach.entries = vec![entry.clone()];
        reach.path = Some(ReachPath {
            entry,
            steps: vec![ReachStep {
                routine: "web/orders.p:get-order".into(),
                file: None,
                line: Some(9),
            }],
        });
        let finding = Finding {
            rule_id: "ABLSEC001".into(),
            severity: "error",
            text: "t".into(),
            uri: "lib/filter.p".into(),
            region: None,
            fingerprint: "x".into(),
            baseline_state: None,
            suppressed: None,
            flow: Vec::new(),
            reach: Some(reach),
            related: vec![Related {
                uri: "web/orders.p".into(),
                line: 9,
                message: "web/orders.p:get-order calls".into(),
            }],
        };
        let doc = document(&Registry::empty(), &[finding], &[], None);
        let result = &doc["runs"][0]["results"][0];
        let r = &result["properties"]["reachability"];
        assert_eq!(r["status"], "reachable");
        assert_eq!(r["entries"][0]["label"], "REST GET /orders/{id}");
        assert_eq!(r["entries"][0]["target"], "web/orders.p:get-order");
        assert_eq!(r["path"]["calls"][0], "web/orders.p:get-order");
        assert!(r.get("unknownCalls").is_none());
        let related = &result["relatedLocations"][0]["physicalLocation"];
        assert_eq!(related["artifactLocation"]["uri"], "web/orders.p");
        assert_eq!(related["region"]["startLine"], 9);
    }

    #[test]
    fn a_taint_path_becomes_one_thread_flow_ending_at_the_sink() {
        let at = |line| Region {
            start_line: line,
            start_column: 1,
            end_line: line,
            end_column: 5,
        };
        let finding = Finding {
            rule_id: "ABLSEC001".into(),
            severity: "error",
            text: "t".into(),
            uri: "a.p".into(),
            region: Some(at(5)),
            fingerprint: "x".into(),
            baseline_state: None,
            suppressed: None,
            flow: vec![
                FlowStep {
                    region: at(1),
                    message: "untrusted source: input parameter `pc`".into(),
                },
                FlowStep {
                    region: at(3),
                    message: "`c` takes the untrusted value".into(),
                },
            ],
            reach: None,
            related: Vec::new(),
        };
        let doc = document(&Registry::empty(), &[finding], &[], None);
        let locations =
            &doc["runs"][0]["results"][0]["codeFlows"][0]["threadFlows"][0]["locations"];
        let lines: Vec<u64> = locations
            .as_array()
            .unwrap()
            .iter()
            .map(|l| {
                l["location"]["physicalLocation"]["region"]["startLine"]
                    .as_u64()
                    .unwrap()
            })
            .collect();
        assert_eq!(lines, [1, 3, 5]);
        assert_eq!(
            locations[0]["location"]["message"]["text"],
            "untrusted source: input parameter `pc`"
        );
    }

    #[test]
    fn columns_count_utf16_units_and_lines_start_at_one() {
        let src = "a\nb\u{1F600}c\n";
        let at = src.find('c').unwrap() as u32;
        let r = Region::of_span(src, at, at + 1);
        assert_eq!((r.start_line, r.start_column), (2, 4));
        assert_eq!((r.end_line, r.end_column), (2, 5));
    }

    #[test]
    fn a_uri_is_percent_encoded() {
        assert_eq!(uri("src/my file#1.p"), "src/my%20file%231.p");
        assert_eq!(uri("src\\a.p"), "src/a.p");
    }
}
