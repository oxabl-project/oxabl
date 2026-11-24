//! Every `toml` block of the skill must be an `oxabl.toml` that oxabl loads:
//! an agent copies these blocks into a project, and a config error is exit 2.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;

/// The `toml` blocks of a Markdown file: the line of the opening fence and
/// the text.
fn toml_blocks(markdown: &str) -> Vec<(usize, String)> {
    let mut blocks = Vec::new();
    // The open block: its opening line, whether it is toml, and its text.
    let mut open: Option<(usize, bool, String)> = None;
    for (index, line) in markdown.lines().enumerate() {
        if let Some(info) = line.trim_start().strip_prefix("```") {
            match open.take() {
                Some((line, true, text)) => blocks.push((line, text)),
                Some(_) => {}
                None => open = Some((index + 1, info.trim() == "toml", String::new())),
            }
        } else if let Some((_, _, text)) = open.as_mut() {
            text.push_str(line);
            text.push('\n');
        }
    }
    blocks
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

/// Runs oxabl on a project whose `oxabl.toml` is `toml`; the problems found.
fn load_problems(toml: &str) -> Vec<String> {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("oxabl.toml"), toml).unwrap();
    fs::write(
        dir.path().join("a.p"),
        "DEFINE VARIABLE i AS INTEGER NO-UNDO.\ni = 1.\n",
    )
    .unwrap();
    // `check` exits 1 for a finding; only a config error (2) fails the block.
    let mut commands = vec![(vec!["check", "."], 1)];
    if toml.contains("[build") {
        commands.push((vec!["build", "--print-config"], 0));
    }
    let mut problems = Vec::new();
    for (args, highest_ok) in commands {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .args(&args)
            .current_dir(dir.path())
            .output()
            .unwrap();
        if output.status.code().is_none_or(|code| code > highest_ok) {
            problems.push(format!(
                "`oxabl {}` fails ({}): {}",
                args.join(" "),
                output.status,
                String::from_utf8_lossy(&output.stderr).trim()
            ));
        }
    }
    problems
}

fn skill_problems(files: &[(String, String)]) -> Vec<String> {
    let mut problems = Vec::new();
    for (name, markdown) in files {
        for (line, toml) in toml_blocks(markdown) {
            for problem in load_problems(&toml) {
                problems.push(format!("{name}:{line}: {problem}"));
            }
        }
    }
    problems
}

#[test]
fn every_toml_block_of_the_skill_loads() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("skill");
    let mut paths = Vec::new();
    markdown_files(&root, &mut paths);
    paths.sort();
    let files: Vec<(String, String)> = paths
        .iter()
        .map(|path| {
            (
                path.strip_prefix(&root)
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
                fs::read_to_string(path).unwrap(),
            )
        })
        .collect();
    let blocks: usize = files.iter().map(|(_, text)| toml_blocks(text).len()).sum();
    assert!(blocks >= 2, "the skill holds {blocks} toml blocks");

    let problems = skill_problems(&files);

    assert!(
        problems.is_empty(),
        "the skill holds a toml block that oxabl does not load:\n{}",
        problems.join("\n")
    );
}

#[test]
fn a_block_without_the_workspace_name_is_reported_with_file_and_line() {
    let markdown = "text\n\n```toml\n[workspace.lint]\nenable = [\"strict\"]\n```\n";

    let problems = skill_problems(&[("references/x.md".to_string(), markdown.to_string())]);

    assert_eq!(problems.len(), 1, "{problems:?}");
    assert!(
        problems[0].starts_with("references/x.md:3:") && problems[0].contains("name"),
        "{problems:?}"
    );
}

#[test]
fn a_bad_build_section_is_reported() {
    let markdown = "```toml\n[workspace]\nname = \"a\"\n\n[build]\nrcode = \"nowhere\"\n```\n";

    let problems = skill_problems(&[("SKILL.md".to_string(), markdown.to_string())]);

    assert!(
        problems
            .iter()
            .any(|p| p.starts_with("SKILL.md:1:") && p.contains("build --print-config")),
        "{problems:?}"
    );
}

#[test]
fn blocks_of_other_tags_are_not_read() {
    let markdown = "```sh\nnot toml\n```\n```text\n[bad\n```\n";

    assert!(toml_blocks(markdown).is_empty());
}
