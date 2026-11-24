//! Every `oxabl` command line in the skill's `sh` blocks must name a
//! subcommand and flags that the binary has. The binary's own `--help` is the
//! authority, so a renamed flag or command fails here, not in an agent's hands.

use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// What one command's `--help` says: its subcommands and its flags.
#[derive(Default)]
struct Help {
    subcommands: BTreeSet<String>,
    long: BTreeSet<String>,
    short: BTreeSet<char>,
}

fn oxabl_help(path: &[String]) -> Help {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(path)
        .arg("--help")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "oxabl {} --help failed",
        path.join(" ")
    );
    parse_help(&String::from_utf8_lossy(&output.stdout))
}

/// Reads clap's help layout: a `Commands:` list and option lines that start
/// with two spaces (`  -I, --include-path`) or six (`      --json`).
fn parse_help(text: &str) -> Help {
    let mut help = Help::default();
    let mut in_commands = false;
    for line in text.lines() {
        if line.trim_end() == "Commands:" {
            in_commands = true;
            continue;
        }
        if in_commands {
            if line.trim().is_empty() {
                in_commands = false;
            } else if let Some(name) = line
                .strip_prefix("  ")
                .and_then(|l| l.split_whitespace().next())
            {
                help.subcommands.insert(name.to_string());
            }
            continue;
        }
        let option = if let Some(rest) = line.strip_prefix("      --") {
            Some(rest)
        } else if let Some(rest) = line.strip_prefix("  -") {
            if let Some((short, rest)) = rest.split_once(", --") {
                if let Some(c) = short.chars().next().filter(|_| short.len() == 1) {
                    help.short.insert(c);
                }
                Some(rest)
            } else {
                // A short flag with no long name.
                rest.chars().next().inspect(|c| {
                    help.short.insert(*c);
                });
                None
            }
        } else {
            None
        };
        if let Some(name) = option.and_then(|rest| {
            rest.split(|c: char| !(c.is_ascii_alphanumeric() || c == '-'))
                .next()
        }) && !name.is_empty()
        {
            help.long.insert(name.to_string());
        }
    }
    help
}

/// The lines of a shell snippet that start an `oxabl` command, with the line
/// number of the first line and any `\` continuation joined.
fn commands_in(markdown: &str) -> Vec<(usize, String)> {
    let mut found = Vec::new();
    let mut in_sh = false;
    let mut pending: Option<(usize, String)> = None;
    for (index, line) in markdown.lines().enumerate() {
        let number = index + 1;
        let trimmed = line.trim_start();
        if let Some(fence) = trimmed.strip_prefix("```") {
            if in_sh {
                in_sh = false;
            } else {
                in_sh = fence.trim() == "sh";
            }
            pending = None;
            continue;
        }
        if !in_sh {
            continue;
        }
        let (start, mut joined) = pending.take().unwrap_or_else(|| (number, String::new()));
        joined.push_str(trimmed);
        if let Some(without) = joined.strip_suffix('\\') {
            pending = Some((start, format!("{} ", without.trim_end())));
            continue;
        }
        if joined.split_whitespace().next() == Some("oxabl") {
            found.push((start, joined));
        }
    }
    found
}

/// Splits a command line like a shell would for quotes, and stops at the first
/// operator or comment.
fn words(line: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    let mut quote: Option<char> = None;
    let mut started = false;
    for c in line.chars() {
        match (quote, c) {
            (Some(q), c) if c == q => quote = None,
            (Some(_), c) => current.push(c),
            (None, '\'' | '"') => {
                quote = Some(c);
                started = true;
            }
            (None, c) if c.is_whitespace() => {
                if started || !current.is_empty() {
                    words.push(std::mem::take(&mut current));
                    started = false;
                }
            }
            (None, '#') if current.is_empty() => break,
            (None, c) => current.push(c),
        }
    }
    if started || !current.is_empty() {
        words.push(current);
    }
    let end = words
        .iter()
        .position(|w| {
            matches!(
                w.as_str(),
                "|" | "||" | "&&" | ";" | "&" | ">" | ">>" | "<" | "2>&1"
            ) || w.starts_with(['>', '<'])
                || w.starts_with("2>")
        })
        .unwrap_or(words.len());
    words.truncate(end);
    words
}

struct Tree {
    cache: HashMap<Vec<String>, Help>,
}

impl Tree {
    fn help(&mut self, path: &[String]) -> &Help {
        self.cache
            .entry(path.to_vec())
            .or_insert_with(|| oxabl_help(path))
    }
}

/// The problems of one command line, each naming the file, line and flag.
fn check_line(tree: &mut Tree, file: &str, line: usize, text: &str) -> Vec<String> {
    let mut problems = Vec::new();
    let mut path: Vec<String> = Vec::new();
    let mut resolving = true;
    for word in words(text).iter().skip(1) {
        if let Some(long) = word.strip_prefix("--") {
            resolving = false;
            if long.is_empty() {
                break;
            }
            let name = long.split('=').next().unwrap();
            let help = tree.help(&path);
            if !help.long.contains(name) && name != "help" && name != "version" {
                problems.push(format!(
                    "{file}:{line}: `oxabl{}{}` has no flag --{name}: {text}",
                    if path.is_empty() { "" } else { " " },
                    path.join(" "),
                ));
            }
        } else if word.len() > 1 && word.starts_with('-') {
            resolving = false;
            let short = word.chars().nth(1).unwrap();
            let help = tree.help(&path);
            if !help.short.contains(&short) && !matches!(short, 'h' | 'V') {
                problems.push(format!(
                    "{file}:{line}: `oxabl{}{}` has no flag -{short}: {text}",
                    if path.is_empty() { "" } else { " " },
                    path.join(" "),
                ));
            }
        } else if resolving {
            let help = tree.help(&path);
            if help.subcommands.is_empty() {
                resolving = false;
            } else if help.subcommands.contains(word) {
                path.push(word.clone());
            } else {
                problems.push(format!(
                    "{file}:{line}: `oxabl{}{}` has no subcommand {word:?} (has: {}): {text}",
                    if path.is_empty() { "" } else { " " },
                    path.join(" "),
                    help.subcommands
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
                break;
            }
        }
    }
    problems
}

fn check_markdown(tree: &mut Tree, file: &str, markdown: &str) -> Vec<String> {
    commands_in(markdown)
        .into_iter()
        .flat_map(|(line, text)| check_line(tree, file, line, &text))
        .collect()
}

fn new_tree() -> Tree {
    Tree {
        cache: HashMap::new(),
    }
}

fn markdown_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            markdown_files(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "md") {
            out.push(path);
        }
    }
}

#[test]
fn every_oxabl_command_in_the_skill_exists() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("skill");
    let mut files = Vec::new();
    markdown_files(&root, &mut files);
    files.sort();
    assert!(!files.is_empty(), "no skill files under {}", root.display());
    let mut tree = new_tree();
    let mut checked = 0;
    let mut problems = Vec::new();
    for path in &files {
        let name = path
            .strip_prefix(&root)
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let markdown = fs::read_to_string(path).unwrap();
        checked += commands_in(&markdown).len();
        problems.extend(check_markdown(&mut tree, &name, &markdown));
    }
    assert!(
        checked > 0,
        "the skill holds no `oxabl` command in a sh block"
    );
    assert!(
        problems.is_empty(),
        "the skill names commands or flags that oxabl does not have:\n{}",
        problems.join("\n")
    );
}

#[test]
fn the_check_reads_the_help_of_the_real_binary() {
    let mut tree = new_tree();

    let check = tree.help(&["check".to_string()]);
    assert!(check.long.contains("json") && check.long.contains("include-path"));
    assert!(check.short.contains(&'I') && check.short.contains(&'j'));
    let skill = tree.help(&["skill".to_string()]);
    assert!(skill.subcommands.contains("install") && skill.subcommands.contains("show"));
    let install = tree.help(&["skill".to_string(), "install".to_string()]);
    assert!(install.long.contains("refresh") && install.long.contains("user"));
}

#[test]
fn a_bogus_flag_is_reported_with_file_line_and_flag() {
    let mut tree = new_tree();
    let markdown = "text\n\n```sh\noxabl check src/ --json\noxabl check src/ --no-such-flag\n```\n";

    let problems = check_markdown(&mut tree, "references/x.md", markdown);

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(
        problems[0].starts_with("references/x.md:5:") && problems[0].contains("--no-such-flag"),
        "{problems:?}"
    );
}

#[test]
fn a_bogus_short_flag_is_reported() {
    let mut tree = new_tree();

    let problems = check_markdown(&mut tree, "SKILL.md", "```sh\noxabl check -Q src/\n```\n");

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(problems[0].contains("-Q"), "{problems:?}");
}

#[test]
fn a_bogus_subcommand_is_reported() {
    let mut tree = new_tree();

    let problems = check_markdown(
        &mut tree,
        "SKILL.md",
        "```sh\noxabl skill nope --user\n```\n",
    );

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(
        problems[0].starts_with("SKILL.md:2:") && problems[0].contains("\"nope\""),
        "{problems:?}"
    );
    let top = check_markdown(&mut tree, "SKILL.md", "```sh\noxabl lint src/\n```\n");
    assert!(top[0].contains("\"lint\""), "{top:?}");
}

#[test]
fn flags_are_checked_against_the_subcommand_they_follow() {
    let mut tree = new_tree();

    let problems = check_markdown(
        &mut tree,
        "SKILL.md",
        "```sh\noxabl skill install --user --refresh\noxabl check --refresh src/\n```\n",
    );

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(problems[0].contains("oxabl check") && problems[0].contains("--refresh"));
}

#[test]
fn only_sh_blocks_and_oxabl_lines_are_read() {
    let mut tree = new_tree();
    let markdown = "```text\noxabl check --bogus\n```\n```sh\ncd src\ngrep --bogus x\noxabl format src/ \\\n  --check # comment --bogus\noxabl check . | head --bogus\n```\nrun `oxabl check --bogus`\n";

    let problems = check_markdown(&mut tree, "SKILL.md", markdown);

    assert!(problems.is_empty(), "{problems:?}");
    let joined = "```sh\noxabl format src/ \\\n  --bogus\n```\n";
    let problems = check_markdown(&mut tree, "SKILL.md", joined);
    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(problems[0].starts_with("SKILL.md:2:"), "{problems:?}");
}
