//! The fixture corpus under `tests/security_corpus`.
//!
//! Each rule has a directory with `vulnerable/` and `safe/` programs. A
//! vulnerable program starts with `/* expect: N */` and must give its own rule
//! exactly N findings. A safe program must give none from any security rule.
//! Programs are preprocessed against the shared `include/` directory, so the
//! include-file and `&GLOBAL-DEFINE` cases run the way a client runs them.
//! All fixtures are synthetic.

use std::path::{Path, PathBuf};

use oxabl_common::FileId;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::RealFileSystem;

use super::{RULES, run_text};

const MIN_PER_KIND: usize = 4;

fn corpus_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/security_corpus")
}

fn programs(dir: &Path) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("{}: {e}", dir.display()))
        .map(|e| e.unwrap().path())
        .filter(|p| {
            p.extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| e == "p" || e == "cls")
        })
        .collect();
    out.sort();
    out
}

fn expected(source: &str, path: &Path) -> usize {
    let first = source.lines().next().unwrap_or("");
    first
        .strip_prefix("/* expect: ")
        .and_then(|rest| rest.strip_suffix(" */"))
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| panic!("{} needs a first line `/* expect: N */`", path.display()))
}

/// Findings of every rule over the expansion of `path`.
fn findings(path: &Path, source: &str) -> Vec<usize> {
    let fs = RealFileSystem;
    let include_paths = [corpus_root().join("include")];
    let expanded = Preprocessor::new(&fs, &include_paths)
        .process(FileId::new(1), source)
        .unwrap_or_else(|d| panic!("{}: {d:?}", path.display()));
    let text = expanded.to_text();
    RULES
        .iter()
        .map(|(_, rule)| run_text(*rule, &text).len())
        .collect()
}

#[test]
fn every_rule_has_enough_vulnerable_and_safe_programs() {
    for (i, (code, _)) in RULES.iter().enumerate() {
        let dir = corpus_root().join(format!("ablsec{:03}", i + 1));
        for kind in ["vulnerable", "safe"] {
            let n = programs(&dir.join(kind)).len();
            assert!(n >= MIN_PER_KIND, "{code} has {n} {kind} programs");
        }
    }
}

#[test]
fn vulnerable_programs_fire_their_rule() {
    for (i, (code, _)) in RULES.iter().enumerate() {
        let dir = corpus_root().join(format!("ablsec{:03}/vulnerable", i + 1));
        for path in programs(&dir) {
            let source = std::fs::read_to_string(&path).unwrap();
            let want = expected(&source, &path);
            let got = findings(&path, &source)[i];
            assert_eq!(got, want, "{code} on {}", path.display());
        }
    }
}

#[test]
fn safe_programs_fire_no_security_rule() {
    for (i, (code, _)) in RULES.iter().enumerate() {
        let dir = corpus_root().join(format!("ablsec{:03}/safe", i + 1));
        for path in programs(&dir) {
            let source = std::fs::read_to_string(&path).unwrap();
            let got = findings(&path, &source);
            assert!(
                got.iter().all(|n| *n == 0),
                "{code} safe program {} fired {got:?}",
                path.display()
            );
        }
    }
}
