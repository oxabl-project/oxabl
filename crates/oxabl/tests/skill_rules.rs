//! `references/rules.md` of the skill must list exactly the lint rules that
//! oxabl has, with the group and the default severity it states. The rules
//! come from the program's own registry (`oxabl::lint::Registry::builtin`).
//! The codes of the tables of non-lint diagnostics have no registry, so they are
//! checked in both directions against the string literals of the non-test
//! workspace sources: a code in the table that no source names, and a code a
//! source names that the table lacks.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use oxabl::lint::{Registry, Stability};

/// One table row: the cells, trimmed, with backticks removed.
fn rows(section: &str) -> Vec<Vec<String>> {
    section
        .lines()
        .filter(|line| line.starts_with('|'))
        .map(|line| {
            line.trim_matches('|')
                .split('|')
                .map(|cell| cell.trim().replace('`', ""))
                .collect::<Vec<_>>()
        })
        .filter(|cells| {
            // Drops the header and the `|---|---|` rule.
            cells
                .first()
                .is_some_and(|c| c.chars().next().is_some_and(|ch| ch.is_ascii_uppercase()))
                && cells[0] != "Code"
        })
        .collect()
}

/// The text under the `## <title>` heading that starts with `title`, up to the next `## `.
fn section<'a>(markdown: &'a str, title: &str) -> Option<&'a str> {
    let start = markdown
        .lines()
        .scan(0usize, |offset, line| {
            let at = *offset;
            *offset += line.len() + 1;
            Some((at, line))
        })
        .find(|(_, line)| line.starts_with("## ") && line[3..].starts_with(title))?
        .0;
    let rest = &markdown[start..];
    let end = rest[3..].find("\n## ").map_or(rest.len(), |i| i + 3);
    Some(&rest[..end])
}

/// What rules.md states for one lint rule.
#[derive(Debug, PartialEq)]
struct Stated {
    name: String,
    group: String,
    severity: String,
}

/// Problems of `markdown` against the registry, one message each.
fn problems(markdown: &str) -> Vec<String> {
    let mut problems = Vec::new();
    let registry = Registry::builtin();
    let mut stated: BTreeMap<String, Stated> = BTreeMap::new();

    for title in ["Core rules", "Strict rules"] {
        let Some(text) = section(markdown, title) else {
            problems.push(format!("rules.md has no `## {title}` section"));
            continue;
        };
        for cells in rows(text) {
            let (code, name) = (cells[0].clone(), cells[1].clone());
            let entry = if title == "Core rules" {
                // | Code | Name | Default | Reports |
                Stated {
                    name,
                    group: "default".into(),
                    severity: cells[2].clone(),
                }
            } else {
                // | Code | Name | Group | Reports |, all default to warning
                Stated {
                    name,
                    group: cells[2].clone(),
                    severity: "warning".into(),
                }
            };
            if stated.insert(code.clone(), entry).is_some() {
                problems.push(format!("rules.md lists {code} twice"));
            }
        }
    }

    let known: BTreeSet<&str> = registry.rules().map(|r| r.meta().code).collect();
    for rule in registry.rules() {
        let meta = rule.meta();
        match stated.get(meta.code) {
            None => problems.push(format!(
                "rules.md does not list the rule {} ({})",
                meta.code, meta.name
            )),
            Some(s) => {
                if s.name != meta.name {
                    problems.push(format!(
                        "rules.md names {} `{}`, oxabl names it `{}`",
                        meta.code, s.name, meta.name
                    ));
                }
                if s.group != meta.group {
                    problems.push(format!(
                        "rules.md puts {} in `{}`, oxabl in `{}`",
                        meta.code, s.group, meta.group
                    ));
                }
                if s.severity != meta.default_severity.as_str() {
                    problems.push(format!(
                        "rules.md gives {} the default severity {}, oxabl {}",
                        meta.code,
                        s.severity,
                        meta.default_severity.as_str()
                    ));
                }
                let strict = meta.group.starts_with("strict.");
                if strict && meta.stability != Stability::Preview {
                    problems.push(format!(
                        "rules.md calls the strict rules preview, but {} is not",
                        meta.code
                    ));
                }
            }
        }
    }
    for code in stated.keys() {
        if !known.contains(code.as_str()) {
            problems.push(format!(
                "rules.md lists {code}, which oxabl has no rule for"
            ));
        }
    }

    match section(markdown, "Other codes") {
        None => problems.push("rules.md has no `## Other codes` section".into()),
        Some(text) => {
            let sources = workspace_sources();
            let mut listed = BTreeSet::new();
            for cells in rows(text) {
                let code = &cells[0];
                listed.insert(code.clone());
                if known.contains(code.as_str()) {
                    problems.push(format!(
                        "rules.md lists the lint rule {code} as a non-lint code"
                    ));
                } else if !sources.contains(&format!("\"{code}\"")) {
                    problems.push(format!(
                        "rules.md lists {code}, which no oxabl source names"
                    ));
                }
            }
            for code in diagnostic_codes(&sources) {
                if !listed.contains(&code) && !known.contains(code.as_str()) {
                    problems.push(format!(
                        "oxabl names the code {code}, which rules.md does not list"
                    ));
                }
            }
        }
    }
    problems
}

/// Every `"PARSE001"`, `"PREPROC007"`, `"SEM0001"` or `"SCHEMA0010"` literal.
fn diagnostic_codes(sources: &str) -> BTreeSet<String> {
    let mut codes = BTreeSet::new();
    // A part between two quotes that is code-shaped is a literal: no source
    // puts such a word between two strings.
    let parts: Vec<&str> = sources.split('"').collect();
    for literal in &parts[1..parts.len().saturating_sub(1)] {
        for prefix in ["PARSE", "PREPROC", "SEM", "SCHEMA"] {
            if let Some(digits) = literal.strip_prefix(prefix)
                && !digits.is_empty()
                && digits.bytes().all(|b| b.is_ascii_digit())
            {
                codes.insert((*literal).to_string());
            }
        }
    }
    codes
}

/// The text of every `.rs` file under the `src` of the workspace crates,
/// without the code that only tests compile.
fn workspace_sources() -> String {
    let crates = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = Vec::new();
    for entry in fs::read_dir(&crates).unwrap() {
        let src = entry.unwrap().path().join("src");
        if src.is_dir() {
            walk(&src, &mut files);
        }
    }
    assert!(
        files.len() > 50,
        "found {} source files under {}: this test needs the workspace checkout",
        files.len(),
        crates.display()
    );
    let texts: Vec<(PathBuf, String)> = files
        .into_iter()
        .map(|path| {
            let text = fs::read_to_string(&path).unwrap_or_default();
            (path, text)
        })
        .collect();
    let test_files: BTreeSet<PathBuf> = texts
        .iter()
        .flat_map(|(path, text)| test_modules(path, text))
        .collect();
    let mut sources = String::new();
    for (path, text) in &texts {
        let in_test_module = test_files.iter().any(|root| path.starts_with(root));
        if !in_test_module && !is_test_file(path) {
            sources.push_str(&without_test_code(text));
        }
    }
    sources
}

fn walk(dir: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path: PathBuf = entry.unwrap().path();
        if path.is_dir() {
            walk(&path, files);
        } else if path.extension().is_some_and(|e| e == "rs") {
            files.push(path);
        }
    }
}

fn is_test_file(path: &Path) -> bool {
    path.file_stem()
        .is_some_and(|stem| stem == "tests" || stem == "test_support")
}

/// Whether an attribute line gates code on `test`.
fn is_test_cfg(line: &str) -> bool {
    let line = line.trim();
    line.starts_with("#[cfg(") && line.contains("test") && !line.contains("not(test")
}

/// The paths of the files and directories of the `mod name;` items that a
/// test-only attribute gates in `path`.
fn test_modules(path: &Path, text: &str) -> Vec<PathBuf> {
    let dir = path.parent().unwrap();
    let stem = path.file_stem().unwrap();
    let nested = if matches!(stem.to_str(), Some("lib" | "main" | "mod")) {
        dir.to_path_buf()
    } else {
        dir.join(stem)
    };
    let lines: Vec<&str> = text.lines().collect();
    let mut found = Vec::new();
    for (index, line) in lines.iter().enumerate() {
        if !is_test_cfg(line) {
            continue;
        }
        let Some(next) = lines.get(index + 1) else {
            continue;
        };
        let item = next.trim().trim_start_matches("pub ");
        if let Some(name) = item
            .strip_prefix("mod ")
            .and_then(|rest| rest.strip_suffix(';'))
        {
            found.push(nested.join(format!("{name}.rs")));
            found.push(nested.join(name));
        }
    }
    found
}

/// `text` without the items that a test-only attribute gates: a `mod name;`
/// line, or a braced item up to the closing brace at the attribute's indent.
fn without_test_code(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let mut kept = String::new();
    let mut index = 0;
    while index < lines.len() {
        let line = lines[index];
        if !is_test_cfg(line) {
            kept.push_str(line);
            kept.push('\n');
            index += 1;
            continue;
        }
        let indent = line.len() - line.trim_start().len();
        index += 1;
        let Some(first) = lines.get(index) else {
            break;
        };
        index += 1;
        if first.trim_end().ends_with(';') && !first.contains('{') {
            continue;
        }
        while index < lines.len() {
            let item = lines[index];
            index += 1;
            if item.len() - item.trim_start().len() == indent && item.trim_start().starts_with('}')
            {
                break;
            }
        }
    }
    kept
}

fn rules_md() -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("skill/references/rules.md");
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

#[test]
fn rules_md_lists_exactly_the_rules_oxabl_has() {
    let problems = problems(&rules_md());

    assert!(
        problems.is_empty(),
        "skill/references/rules.md differs from the oxabl rule registry:\n{}",
        problems.join("\n")
    );
}

#[test]
fn a_rule_missing_from_rules_md_is_reported() {
    let md = rules_md();
    let edited: String = md
        .lines()
        .filter(|line| !line.starts_with("| LK003 "))
        .map(|line| format!("{line}\n"))
        .collect();

    let problems = problems(&edited);

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(
        problems[0].contains("does not list the rule LK003"),
        "{problems:?}"
    );
}

#[test]
fn a_bogus_code_in_rules_md_is_reported() {
    let md = rules_md().replace(
        "| LK003 |",
        "| LK099 | `no-such-rule` | `strict.locks` | Invented. |\n| LK003 |",
    );

    let problems = problems(&md);

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(
        problems[0].contains("lists LK099, which oxabl has no rule for"),
        "{problems:?}"
    );
}

#[test]
fn a_bogus_non_lint_code_is_reported() {
    let md = rules_md().replace("| SEM0003 |", "| SEM0999 | Invented. |\n| SEM0003 |");

    let problems = problems(&md);

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(problems[0].contains("SEM0999"), "{problems:?}");
}

#[test]
fn a_wrong_group_name_or_severity_is_reported() {
    let md = rules_md()
        .replace(
            "| LINT0002 | `unused-variable` | warning |",
            "| LINT0002 | `unused-variable` | error |",
        )
        .replace(
            "`strict.locks` | A database read",
            "`strict.access` | A database read",
        )
        .replace("`find-in-loop`", "`find-loops`");

    let problems = problems(&md);

    assert_eq!(problems.len(), 3, "{problems:?}");
    assert!(
        problems
            .iter()
            .any(|p| p.contains("LINT0002") && p.contains("severity"))
    );
    assert!(
        problems
            .iter()
            .any(|p| p.contains("LK001") && p.contains("strict.access"))
    );
    assert!(
        problems
            .iter()
            .any(|p| p.contains("DB001") && p.contains("find-loops"))
    );
}

#[test]
fn a_non_lint_code_missing_from_rules_md_is_reported() {
    let md = rules_md();
    let edited: String = md
        .lines()
        .filter(|line| !line.starts_with("| SEM0003 ") && !line.starts_with("| PREPROC010 "))
        .map(|line| format!("{line}\n"))
        .collect();

    let problems = problems(&edited);

    assert_eq!(problems.len(), 2, "{problems:?}");
    assert!(
        problems
            .iter()
            .any(|p| p.contains("names the code SEM0003, which rules.md does not list")),
        "{problems:?}"
    );
    assert!(
        problems.iter().any(|p| p.contains("PREPROC010")),
        "{problems:?}"
    );
}
