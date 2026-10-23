//! The fixture corpus under `tests/security_corpus`.
//!
//! Each rule has a directory with `vulnerable/` and `safe/` programs. A
//! vulnerable program starts with `/* expect: N */` and must give its own rule
//! exactly N findings. A safe program must give none from any security rule.
//! Programs are preprocessed against the shared `include/` directory, so the
//! include-file and `&GLOBAL-DEFINE` cases run the way a client runs them.
//! All fixtures are synthetic. A program must parse cleanly, unless its name
//! ends in `_unparsed`.

use std::path::{Path, PathBuf};

use oxabl_common::FileId;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::RealFileSystem;

use super::{RULES, check_loose, run_text};

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

/// A program whose file name ends in `_unparsed` holds a construct that the
/// parser rejects, so it is read through the tokens. Every other program must
/// parse cleanly.
fn unparsed(path: &Path) -> bool {
    path.file_stem()
        .and_then(|s| s.to_str())
        .is_some_and(|s| s.ends_with("_unparsed"))
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
        .map(|(_, rule)| {
            if unparsed(path) {
                check_loose(*rule, &text).len()
            } else {
                run_text(*rule, &text).len()
            }
        })
        .collect()
}

#[test]
fn every_rule_has_enough_vulnerable_and_safe_programs() {
    for (code, _) in RULES {
        let dir = corpus_root().join(code.to_ascii_lowercase());
        for kind in ["vulnerable", "safe"] {
            let n = programs(&dir.join(kind)).len();
            assert!(n >= MIN_PER_KIND, "{code} has {n} {kind} programs");
        }
    }
}

#[test]
fn vulnerable_programs_fire_their_rule() {
    for (i, (code, _)) in RULES.iter().enumerate() {
        let dir = corpus_root().join(format!("{}/vulnerable", code.to_ascii_lowercase()));
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
    for (code, _) in RULES {
        let dir = corpus_root().join(format!("{}/safe", code.to_ascii_lowercase()));
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
