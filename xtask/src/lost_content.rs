//! `cargo xtask lost-content <old-head> <new-head> [--base <ref>]`.
//!
//! After a rebase, every word that the new head removes (against its merge-base with the base)
//! must also be removed by the old head (against its own merge-base). Any other removed word
//! was lost when a conflict was resolved.

use std::collections::BTreeMap;
use std::process::{Command, ExitCode};

pub const USAGE: &str = "usage: cargo xtask lost-content <old-head> <new-head> [--base <ref>]";

const DEFAULT_BASE: &str = "origin/master";
const SHOWN_WORDS: usize = 24;

/// Words removed from one hunk, with the old-side line of the first line that lost a word.
/// The old-side line of a removed line, and the words it lost.
type Hunk = (u32, Vec<String>);

struct Loss {
    line: u32,
    words: Vec<String>,
}

pub fn run(arguments: &[String]) -> ExitCode {
    let mut refs: Vec<&str> = Vec::new();
    let mut base = DEFAULT_BASE;
    let mut iter = arguments.iter();
    while let Some(argument) = iter.next() {
        match argument.as_str() {
            "--base" => match iter.next() {
                Some(value) => base = value,
                None => return usage_error("--base needs a ref"),
            },
            flag if flag.starts_with('-') => return usage_error(&format!("unknown flag {flag}")),
            name => refs.push(name),
        }
    }
    let [old, new] = refs[..] else {
        return usage_error("two heads are needed");
    };
    match lost(old, new, base) {
        Ok(losses) if losses.is_empty() => {
            println!("lost-content: nothing lost between {old} and {new}");
            ExitCode::SUCCESS
        }
        Ok(losses) => {
            for (file, loss) in &losses {
                let mut text = loss
                    .words
                    .iter()
                    .take(SHOWN_WORDS)
                    .cloned()
                    .collect::<Vec<_>>();
                if loss.words.len() > SHOWN_WORDS {
                    text.push(format!("... ({} words)", loss.words.len()));
                }
                println!("{file}:{}: lost: {}", loss.line, text.join(" "));
            }
            eprintln!("lost-content: {} place(s) lost content", losses.len());
            ExitCode::FAILURE
        }
        Err(message) => {
            eprintln!("lost-content: {message}");
            ExitCode::from(2)
        }
    }
}

fn usage_error(message: &str) -> ExitCode {
    eprintln!("{message}\n{USAGE}");
    ExitCode::from(2)
}

fn git(arguments: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(arguments)
        .output()
        .map_err(|error| format!("cannot run git: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "git {} failed: {}",
            arguments.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn commit_of(name: &str) -> Result<String, String> {
    let spec = format!("{name}^{{commit}}");
    Ok(git(&["rev-parse", "--verify", "--quiet", &spec])
        .map_err(|_| format!("{name} is not a commit"))?
        .trim()
        .to_string())
}

fn lost(old: &str, new: &str, base: &str) -> Result<Vec<(String, Loss)>, String> {
    let old = commit_of(old)?;
    let new = commit_of(new)?;
    let base = commit_of(base)?;
    let old_removed = removed(&old, &base)?;
    let new_removed = removed(&new, &base)?;
    let mut losses = Vec::new();
    for (file, hunks) in new_removed.hunks {
        let mut allowed = old_removed.totals.get(&file).cloned().unwrap_or_default();
        for (line, words) in hunks {
            let mut lost_words = Vec::new();
            for word in words {
                match allowed.get_mut(&word) {
                    Some(count) if *count > 0 => *count -= 1,
                    _ => lost_words.push(word),
                }
            }
            if !lost_words.is_empty() {
                losses.push((
                    file.clone(),
                    Loss {
                        line,
                        words: lost_words,
                    },
                ));
            }
        }
    }
    Ok(losses)
}

#[derive(Default)]
struct Removed {
    /// Per file: the removed words of each hunk, with the old-side line of the hunk's first loss.
    hunks: Vec<(String, Vec<Hunk>)>,
    /// Per file: how many times each word is removed.
    totals: BTreeMap<String, BTreeMap<String, u32>>,
}

/// The words that `head` removes against its merge-base with `base`. In one hunk a word that the
/// added lines bring back is not removed (an edit of a line keeps its other words).
fn removed(head: &str, base: &str) -> Result<Removed, String> {
    let merge_base = git(&["merge-base", base, head])?.trim().to_string();
    let range = format!("{merge_base}..{head}");
    let diff = git(&["diff", "--no-renames", "--no-color", "-U0", &range])?;
    let mut result = Removed::default();
    let mut file = String::new();
    let mut gone: Vec<Hunk> = Vec::new();
    let mut added: Vec<String> = Vec::new();
    let mut line = 0u32;

    let flush =
        |file: &str, gone: &mut Vec<Hunk>, added: &mut Vec<String>, result: &mut Removed| {
            let mut back: BTreeMap<String, u32> = BTreeMap::new();
            for word in added.drain(..) {
                *back.entry(word).or_default() += 1;
            }
            let mut hunk: Vec<Hunk> = Vec::new();
            for (at, words) in gone.drain(..) {
                let mut kept = Vec::new();
                for word in words {
                    match back.get_mut(&word) {
                        Some(count) if *count > 0 => *count -= 1,
                        _ => kept.push(word),
                    }
                }
                if !kept.is_empty() {
                    hunk.push((at, kept));
                }
            }
            if hunk.is_empty() {
                return;
            }
            for (_, words) in &hunk {
                for word in words {
                    *result
                        .totals
                        .entry(file.to_string())
                        .or_default()
                        .entry(word.clone())
                        .or_default() += 1;
                }
            }
            match result.hunks.last_mut() {
                Some((name, hunks)) if name == file => hunks.extend(hunk),
                _ => result.hunks.push((file.to_string(), hunk)),
            }
        };

    let mut in_header = false;
    for text in diff.lines() {
        if text.starts_with("diff --git ") {
            flush(&file, &mut gone, &mut added, &mut result);
            in_header = true;
        } else if in_header && !text.starts_with("@@") {
            // `--- a/x` names the file; a new file has `--- /dev/null` and `+++ b/x`.
            if let Some(name) = text
                .strip_prefix("--- a/")
                .or_else(|| text.strip_prefix("+++ b/"))
            {
                file = name.to_string();
            }
        } else if text.starts_with("@@") {
            flush(&file, &mut gone, &mut added, &mut result);
            in_header = false;
            line = old_start(text).ok_or_else(|| format!("bad hunk header: {text}"))?;
        } else if let Some(content) = text.strip_prefix('-') {
            gone.push((line, content.split_whitespace().map(String::from).collect()));
            line += 1;
        } else if let Some(content) = text.strip_prefix('+') {
            added.extend(content.split_whitespace().map(String::from));
        }
    }
    flush(&file, &mut gone, &mut added, &mut result);
    Ok(result)
}

/// The first old-side line of a hunk header `@@ -12,3 +12,4 @@`.
fn old_start(header: &str) -> Option<u32> {
    let rest = header.strip_prefix("@@ -")?;
    let number: String = rest.chars().take_while(char::is_ascii_digit).collect();
    number.parse().ok()
}
