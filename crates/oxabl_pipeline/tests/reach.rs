//! Entry-point reachability over a synthetic web application
//! (`tests/reach_corpus`): the labels every security finding gets, and the
//! rules that need the graph (ABLSEC011, ABLSEC012) with ABLSEC010.
//!
//! Each line where one of those three rules reports in some configuration
//! carries a mark with the rule's code: `/* reach: real ABLSEC011 */` when the
//! finding is right, `/* reach: safe ABLSEC011 */` when the code is safe and a
//! rule that ignored authorisation checks and resets would report it. A
//! finding on a line with no mark for its rule fails the test.

use std::path::{Path, PathBuf};

use oxabl_common::{Diagnostic, ReachStatus, SecurityMode, SecurityOptions, Severity};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::{InMemoryFileSystem, parse_entry_points};

const RULES: [&str; 3] = ["ABLSEC010", "ABLSEC011", "ABLSEC012"];

fn corpus() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/reach_corpus")
}

/// Every file of the corpus, as `/src/...` paths, with its text.
fn files() -> Vec<(PathBuf, String)> {
    fn walk(dir: &Path, rel: &Path, out: &mut Vec<(PathBuf, String)>) {
        let mut entries: Vec<_> = std::fs::read_dir(dir)
            .unwrap()
            .map(|e| e.unwrap().path())
            .collect();
        entries.sort();
        for p in entries {
            let name = rel.join(p.file_name().unwrap());
            if p.is_dir() {
                walk(&p, &name, out);
            } else {
                out.push((
                    Path::new("/src").join(&name),
                    std::fs::read_to_string(&p).unwrap(),
                ));
            }
        }
    }
    let mut out = Vec::new();
    walk(&corpus().join("src"), Path::new(""), &mut out);
    out
}

fn entries() -> SecurityOptions {
    let text = std::fs::read_to_string(corpus().join("entry-points.json")).unwrap();
    let mut options = SecurityOptions {
        mode: SecurityMode::Taint,
        ..SecurityOptions::default()
    };
    options.reach.configured = true;
    options.reach.entries = parse_entry_points(&text).unwrap();
    options.reach.auth_routines = vec![
        "auth/check-access.p".into(),
        "auth/guard.i".into(),
        "require-role".into(),
    ];
    options.reach.sensitive_calls = vec!["post-ledger".into()];
    options
}

fn config(options: SecurityOptions) -> PipelineConfig {
    let mut config = PipelineConfig {
        include_paths: vec!["/src".into()],
        ..PipelineConfig::default()
    };
    config.lint_severities.base_mut().enable_group("security");
    config.lint_severities.set_security(options);
    config
}

/// The security findings of every `.p` and `.cls` file, by file.
fn run(options: SecurityOptions) -> Vec<(PathBuf, String, Vec<Diagnostic>)> {
    let files = files();
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in &files {
        fs.insert(path.clone(), text.as_str());
    }
    let config = config(options);
    let lint = LintPipeline::new(&config, &fs);
    let reach = std::sync::Arc::new(lint.build_security_reach(None));
    let lint = lint.with_security_reach(reach);
    files
        .into_iter()
        .filter(|(p, _)| p.extension().is_some_and(|e| e == "p" || e == "cls"))
        .map(|(path, text)| {
            let result = lint.with_file(&path).run(&text);
            assert!(!result.failed_run(), "{}", path.display());
            let diags = result
                .all()
                .filter(|d| d.diagnostic.code.0.starts_with("ABLSEC"))
                .map(|d| d.diagnostic.clone())
                .collect();
            (path, text, diags)
        })
        .collect()
}

fn line(text: &str, d: &Diagnostic) -> usize {
    text[..d.span.span.start as usize].matches('\n').count() + 1
}

fn find<'a>(
    out: &'a [(PathBuf, String, Vec<Diagnostic>)],
    file: &str,
    code: &str,
) -> Vec<&'a Diagnostic> {
    out.iter()
        .filter(|(p, ..)| p.ends_with(file))
        .flat_map(|(_, _, d)| d.iter().filter(|d| d.code.0 == code))
        .collect()
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Score {
    tp: usize,
    fp: usize,
    fn_: usize,
}

fn ratio(a: usize, b: usize) -> f64 {
    if b == 0 { 1.0 } else { a as f64 / b as f64 }
}

/// Precision and recall of one rule against the marks: `/* reach: real CODE */`
/// and `/* reach: safe CODE */`.
fn score(out: &[(PathBuf, String, Vec<Diagnostic>)], code: &str) -> Score {
    let real = format!("/* reach: real {code} */");
    let safe = format!("/* reach: safe {code} */");
    let mut s = Score::default();
    for (path, text, diags) in out {
        let found: Vec<usize> = diags
            .iter()
            .filter(|d| d.code.0 == code)
            .map(|d| line(text, d))
            .collect();
        for &l in &found {
            let t = text.lines().nth(l - 1).unwrap_or("");
            if t.contains(&real) {
                s.tp += 1;
            } else if t.contains(&safe) {
                println!("FP {}:{l} {code}", path.display());
                s.fp += 1;
            } else {
                panic!(
                    "{}:{l}: {code} on a line with no mark for it",
                    path.display()
                );
            }
        }
        for (i, t) in text.lines().enumerate() {
            if t.contains(&real) && !found.contains(&(i + 1)) {
                println!("FN {}:{} {code}", path.display(), i + 1);
                s.fn_ += 1;
            }
        }
    }
    s
}

#[test]
fn the_rules_meet_the_marks() {
    let out = run(entries());
    let mut table = String::from("rule       |  TP  FP  FN  precision  recall\n");
    for code in RULES {
        let s = score(&out, code);
        table.push_str(&format!(
            "{code}  | {:>3} {:>3} {:>3}  {:>9.2}  {:>6.2}\n",
            s.tp,
            s.fp,
            s.fn_,
            ratio(s.tp, s.tp + s.fp),
            ratio(s.tp, s.tp + s.fn_)
        ));
        assert_eq!(s.fp, 0, "{code}\n{table}");
        assert_eq!(s.fn_, 0, "{code}\n{table}");
    }
    // The same corpus with no authorisation routine and no reset seen: what a
    // rule that ignores the gates reports.
    let mut ungated = entries();
    ungated.reach.auth_routines.clear();
    for e in &mut ungated.reach.entries {
        e.auth = false;
    }
    let s = score(&run(ungated), "ABLSEC011");
    table.push_str(&format!(
        "ABLSEC011 without gates | {:>3} {:>3} {:>3}  {:>9.2}  {:>6.2}\n",
        s.tp,
        s.fp,
        s.fn_,
        ratio(s.tp, s.tp + s.fp),
        ratio(s.tp, s.tp + s.fn_)
    ));
    assert!(s.fp > 0, "{table}");
    println!("{table}");
}

#[test]
fn a_finding_names_the_entries_that_reach_it_and_a_path() {
    let out = run(entries());
    let q = find(&out, "lib/filter.p", "ABLSEC001");
    assert_eq!(q.len(), 1, "{q:?}");
    let r = q[0].reach.as_deref().expect("a reach label");
    assert_eq!(r.status, ReachStatus::Reachable);
    let labels: Vec<&str> = r.entries.iter().map(|e| e.label.as_str()).collect();
    assert_eq!(labels, ["REST GET /orders/{id}"]);
    assert!(
        q[0].message
            .ends_with("(reachable from REST GET /orders/{id})"),
        "{}",
        q[0].message
    );
    let path = r.path.as_ref().unwrap();
    let steps: Vec<&str> = path.steps.iter().map(|s| s.routine.as_str()).collect();
    assert_eq!(steps, ["web/orders.p:get-order"]);
    assert_eq!(path.steps[0].line, Some(9));

    let shell = find(&out, "web/pages/home.p", "ABLSEC003");
    let r = shell[0].reach.as_deref().unwrap();
    assert_eq!(
        r.status,
        ReachStatus::Unknown,
        "only a RUN VALUE reaches it"
    );
    assert_eq!(r.unknown, ["dynamic"]);
    assert!(
        shell[0]
            .message
            .contains("(reachability unknown: dynamic call)")
    );

    let xss = find(&out, "web/me.p", "ABLSEC006");
    assert_eq!(xss.len(), 1, "the JSON object carries the name to the page");
    assert_eq!(
        xss[0].reach.as_deref().unwrap().entries[0].label,
        "REST GET /api/me"
    );
}

#[test]
fn class_entries_reach_through_new_and_typed_method_calls() {
    let out = run(entries());
    let audit = find(&out, "app/AuditLog.cls", "ABLSEC011");
    assert_eq!(audit.len(), 1, "{audit:?}");
    assert!(
        audit[0].message.contains("app/InvoiceService.cls:Create"),
        "{}",
        audit[0].message
    );
    let status = find(&out, "app/StatusService.cls", "ABLSEC011");
    assert!(
        status.is_empty(),
        "an entry marked auth is authorised: {status:?}"
    );
}

#[test]
fn without_a_dynamic_call_an_unreached_finding_is_unreachable_and_can_be_lowered() {
    let mut options = entries();
    options
        .reach
        .entries
        .retain(|e| !e.target.describe().starts_with("web/front.p"));
    options.reach.lower_unreachable = true;
    options.reach.raise_reachable = true;
    let out = run(options);
    let purge = find(&out, "batch/purge.p", "ABLSEC003");
    let r = purge[0].reach.as_deref().unwrap();
    assert_eq!(r.status, ReachStatus::Unreachable);
    assert_eq!(purge[0].severity, Severity::Warning, "lowered from error");
    let export = find(&out, "web/export.p", "ABLSEC005");
    assert_eq!(export[0].severity, Severity::Error, "raised from warning");
}

#[test]
fn without_entry_points_nothing_is_labelled_and_the_graph_rules_are_silent() {
    let mut options = entries();
    options.reach = Default::default();
    let out = run(options);
    for (path, _, diags) in &out {
        for d in diags {
            assert!(d.reach.is_none(), "{}: {d:?}", path.display());
            assert!(
                !matches!(d.code.0, "ABLSEC011" | "ABLSEC012"),
                "{}: {d:?}",
                path.display()
            );
        }
    }
    assert!(!find(&out, "web/orders.p", "ABLSEC010").is_empty());
}

#[test]
fn an_entry_that_does_not_resolve_leaves_unreached_findings_unlabelled() {
    let mut options = entries();
    options
        .reach
        .entries
        .retain(|e| !e.target.describe().starts_with("web/front.p"));
    let missing = parse_entry_points(
        r#"{"version": 1, "entries": [{"kind": "REST", "name": "GET /gone", "program": "web/gone.p"}]}"#,
    )
    .unwrap();
    options.reach.entries.extend(missing);
    options.reach.lower_unreachable = true;
    let out = run(options);
    let purge = find(&out, "batch/purge.p", "ABLSEC003");
    assert!(purge[0].reach.is_none(), "{:?}", purge[0]);
    assert_eq!(purge[0].severity, Severity::Error, "not lowered");
    let q = find(&out, "lib/filter.p", "ABLSEC001");
    assert_eq!(
        q[0].reach.as_deref().map(|r| r.status),
        Some(ReachStatus::Reachable)
    );
}

#[test]
fn when_no_entry_resolves_nothing_is_labelled_unreachable() {
    let mut options = entries();
    options.reach.entries = parse_entry_points(
        r#"{"version": 1, "entries": [{"kind": "REST", "name": "GET /a", "program": "web/aa.p"}]}"#,
    )
    .unwrap();
    options.reach.lower_unreachable = true;
    for (path, _, diags) in &run(options) {
        for d in diags {
            assert!(d.reach.is_none(), "{}: {d:?}", path.display());
        }
    }
}
