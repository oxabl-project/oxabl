//! Checks that read the workflows or the dependency tree. Each one returns its log as `Ok` when
//! it passes and as `Err` when it fails. Each one also proves that it read something.

use crate::Context;
use std::path::Path;
use std::process::Command;

pub type Outcome = Result<String, String>;

const ACTION_DIRECTORY_DEPTH_MAX: u32 = 8;
const ACTION_FILE_COUNT_MAX: usize = 1024;

/// Every workflow and local action uses only first-party actions, local ones, or the few named
/// here. A third-party action can hang or change under us.
pub fn workflows_use_allowed_actions(context: &Context) -> Outcome {
    let mut files = Vec::new();
    for directory in [".github/workflows", ".github/actions"] {
        collect_yaml(&context.root.join(directory), 0, &mut files)?;
    }
    let mut uses: Vec<String> = Vec::new();
    for file in &files {
        let text = std::fs::read_to_string(file)
            .map_err(|error| format!("cannot read {}: {error}", file.display()))?;
        uses.extend(text.lines().filter_map(used_action).map(str::to_owned));
    }
    uses.sort();
    uses.dedup();
    if !uses
        .iter()
        .any(|used| used == "./.github/actions/setup-rust")
    {
        return Err("no step uses ./.github/actions/setup-rust, so the check read nothing".into());
    }
    let bad: Vec<&String> = uses.iter().filter(|used| !action_allowed(used)).collect();
    if bad.is_empty() {
        Ok(format!("{} distinct actions, all allowed\n", uses.len()))
    } else {
        Err(format!(
            "actions not on the allowlist in this job:\n{bad:#?}\n"
        ))
    }
}

/// The action named by a `uses:` line, with an optional list dash before the key.
pub fn used_action(line: &str) -> Option<&str> {
    let line = line.trim_start();
    let line = line.strip_prefix("- ").map_or(line, str::trim_start);
    let value = line.strip_prefix("uses:")?.trim_start();
    let end = value
        .find(|c: char| c.is_whitespace() || c == '#')
        .unwrap_or(value.len());
    let value = &value[..end];
    if value.is_empty() { None } else { Some(value) }
}

pub fn action_allowed(used: &str) -> bool {
    const THIRD_PARTY: [&str; 3] = [
        "CodSpeedHQ/action@",
        "pnpm/action-setup@",
        "googleapis/release-please-action@",
    ];
    if used.starts_with("./.github/actions/") || THIRD_PARTY.iter().any(|p| used.starts_with(p)) {
        return true;
    }
    let Some(rest) = used.strip_prefix("actions/") else {
        return false;
    };
    let Some((name, _version)) = rest.split_once('@') else {
        return false;
    };
    let segments: Vec<&str> = name.split('/').collect();
    let segment_ok = |segment: &&str| {
        !segment.is_empty() && segment.chars().all(|c| matches!(c, 'a'..='z' | '-'))
    };
    (1..=2).contains(&segments.len()) && segments.iter().all(segment_ok)
}

fn collect_yaml(
    directory: &Path,
    depth: u32,
    files: &mut Vec<std::path::PathBuf>,
) -> Result<(), String> {
    assert!(
        depth <= ACTION_DIRECTORY_DEPTH_MAX,
        "workflow directories nest this deep at most"
    );
    let entries = std::fs::read_dir(directory)
        .map_err(|error| format!("cannot read {}: {error}", directory.display()))?;
    for entry in entries {
        let path = entry.map_err(|error| error.to_string())?.path();
        if path.is_dir() {
            if depth < ACTION_DIRECTORY_DEPTH_MAX {
                collect_yaml(&path, depth + 1, files)?;
            }
        } else if matches!(
            path.extension().and_then(|e| e.to_str()),
            Some("yml" | "yaml")
        ) {
            files.push(path);
        }
        if files.len() > ACTION_FILE_COUNT_MAX {
            return Err("too many workflow files".into());
        }
    }
    Ok(())
}

/// The fake AVM stands in for Progress, so it must not share a codec with oxabl.
pub fn fake_avm_has_no_oxabl_dependency(context: &Context) -> Outcome {
    let tree = cargo_tree(context, &["-p", "oxabl_fake_avm", "--prefix", "none"])?;
    // The first line is the package itself.
    let offenders: Vec<&str> = tree
        .lines()
        .skip(1)
        .filter(|l| l.starts_with("oxabl"))
        .collect();
    if tree.lines().count() < 2 {
        return Err(format!(
            "cargo tree listed no dependency, so the check read nothing:\n{tree}"
        ));
    }
    verdict(
        &offenders,
        "oxabl_fake_avm must not depend on an oxabl crate",
    )
}

/// The remote cache sits behind the `remote-cache` feature of the umbrella crate, so that no
/// other build carries the network and crypto stack.
pub fn only_the_feature_reaches_the_cache_client(context: &Context) -> Outcome {
    let build = cargo_tree(context, &["-p", "oxabl_build", "--prefix", "none"])?;
    let plain = cargo_tree(
        context,
        &["-p", "oxabl", "-e", "normal,build", "--prefix", "none"],
    )?;
    for tree in [&build, &plain] {
        require_listing(tree, "oxabl_")?;
    }
    let mut offenders: Vec<&str> = Vec::new();
    offenders.extend(build.lines().filter(|l| l.starts_with("oxabl-cache")));
    offenders.extend(plain.lines().filter(|l| l.starts_with("oxabl-cache")));
    verdict(
        &offenders,
        "no build without the remote-cache feature may depend on oxabl-cache",
    )
}

/// LSP counts only LF, CRLF and CR as line breaks. `ropey`'s `unicode_lines` also counts
/// U+2028, NEL and FF, and any crate that enables it turns it on for the language server too.
pub fn no_build_turns_on_unicode_lines(context: &Context) -> Outcome {
    let tree = cargo_tree(
        context,
        &[
            "--workspace",
            "--all-features",
            "-e",
            "features",
            "-i",
            "ropey",
        ],
    )?;
    if !tree.lines().any(|line| line.starts_with("ropey v")) {
        return Err("cargo tree did not list ropey, so the gate checked nothing".into());
    }
    let offenders: Vec<&str> = tree
        .lines()
        .filter(|l| l.contains("unicode_lines"))
        .collect();
    verdict(
        &offenders,
        "ropey's unicode_lines moves every LSP position after a U+2028, NEL or FF",
    )
}

/// The `self-update` feature of the umbrella crate is the only way to the HTTP and TLS stack
/// besides the remote cache, so a plain `cargo install oxabl` carries no network or crypto code.
pub fn only_the_features_reach_the_network_stack(context: &Context) -> Outcome {
    const STACK: [&str; 11] = [
        "reqwest",
        "hyper",
        "hyper-rustls",
        "hyper-util",
        "rustls",
        "rustls-webpki",
        "tokio",
        "tokio-rustls",
        "ring",
        "aws-lc-rs",
        "aws-lc-sys",
    ];
    let in_stack = |line: &str| {
        line.split_once(' ')
            .is_some_and(|(name, _)| STACK.contains(&name))
    };
    let plain = cargo_tree(
        context,
        &["-p", "oxabl", "-e", "normal,build", "--prefix", "none"],
    )?;
    let build = cargo_tree(
        context,
        &[
            "-p",
            "oxabl_build",
            "-e",
            "normal,build",
            "--prefix",
            "none",
        ],
    )?;
    let offenders: Vec<&str> = plain
        .lines()
        .chain(build.lines())
        .filter(|l| in_stack(l))
        .collect();
    verdict(
        &offenders,
        "oxabl and oxabl_build must not depend on an HTTP or TLS crate",
    )?;
    // Positive control: the check above would pass for any pattern.
    let with = cargo_tree(
        context,
        &[
            "-p",
            "oxabl",
            "--features",
            "self-update",
            "-e",
            "normal,build",
            "--prefix",
            "none",
        ],
    )?;
    if with.lines().any(|line| line.starts_with("reqwest ")) {
        Ok("no HTTP or TLS crate without the features; reqwest with self-update\n".into())
    } else {
        Err("with self-update, cargo tree did not list reqwest, so the check proves nothing".into())
    }
}

fn require_listing(tree: &str, prefix: &str) -> Result<(), String> {
    if tree.lines().any(|line| line.starts_with(prefix)) {
        Ok(())
    } else {
        Err(format!(
            "cargo tree listed no `{prefix}` crate, so the check read nothing:\n{tree}"
        ))
    }
}

fn verdict(offenders: &[&str], message: &str) -> Outcome {
    if offenders.is_empty() {
        Ok("none found\n".into())
    } else {
        Err(format!("{message}\n{}\n", offenders.join("\n")))
    }
}

fn cargo_tree(context: &Context, arguments: &[&str]) -> Result<String, String> {
    let output = Command::new("cargo")
        .arg("tree")
        .args(arguments)
        .current_dir(&context.root)
        .output()
        .map_err(|error| format!("cannot run cargo tree: {error}"))?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        Err(format!(
            "cargo tree {} failed:\n{}",
            arguments.join(" "),
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_uses_lines() {
        assert_eq!(
            used_action("      - uses: actions/checkout@v6"),
            Some("actions/checkout@v6")
        );
        assert_eq!(
            used_action("  uses: ./.github/actions/setup-rust # c"),
            Some("./.github/actions/setup-rust")
        );
        assert_eq!(used_action("      - run: echo uses: x"), None);
    }

    #[test]
    fn allows_first_party_and_named_actions_only() {
        assert!(action_allowed("actions/cache@v6"));
        assert!(action_allowed("actions/upload-artifact@v4"));
        assert!(action_allowed("./.github/actions/setup-rust"));
        assert!(action_allowed("pnpm/action-setup@v4"));
        assert!(!action_allowed("moonrepo/setup-rust@v1"));
        assert!(!action_allowed("actions/cache"));
        assert!(!action_allowed("actions/Evil@v1"));
        assert!(!action_allowed("actionsx/cache@v1"));
    }
}
