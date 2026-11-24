//! `references/rules.md` of the skill must list exactly the lint rules that
//! oxabl has, with the group and the default severity it states. The rules
//! come from the program's own registry (`oxabl::lint::Registry::builtin`).
//! The codes of the tables of non-lint diagnostics have no registry, so they are
//! checked against the string literals of the workspace sources.

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
            for cells in rows(text) {
                let code = &cells[0];
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
        }
    }
    problems
}

/// The text of every `.rs` file under the `src` of the workspace crates.
fn workspace_sources() -> String {
    let crates = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut text = String::new();
    let mut files = 0;
    for entry in fs::read_dir(&crates).unwrap() {
        let src = entry.unwrap().path().join("src");
        if src.is_dir() {
            files += walk(&src, &mut text);
        }
    }
    assert!(
        files > 50,
        "found {files} source files under {}: this test needs the workspace checkout",
        crates.display()
    );
    text
}

fn walk(dir: &Path, text: &mut String) -> usize {
    let mut files = 0;
    for entry in fs::read_dir(dir).unwrap() {
        let path: PathBuf = entry.unwrap().path();
        if path.is_dir() {
            files += walk(&path, text);
        } else if path.extension().is_some_and(|e| e == "rs") {
            files += 1;
            text.push_str(&fs::read_to_string(&path).unwrap_or_default());
        }
    }
    files
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
