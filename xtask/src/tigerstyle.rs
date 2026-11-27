//! TigerStyle lint ratchet across every workspace crate (`docs/TIGER_STYLE.md`).
//!
//! Runs `cargo clippy --workspace --lib --bins --message-format=json` with
//! `CLIPPY_CONF_DIR` pointing at `xtask/tigerstyle` and `-W` for every lint in
//! the target TigerStyle set, then compares the per-(crate, lint) counts
//! against `tigerstyle-ratchet.txt`. Any count above the committed baseline
//! fails the check; `--update` writes the current counts back to the baseline.

use crate::Context;
use crate::checks::Outcome;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

const RATCHET_FILE: &str = "tigerstyle-ratchet.txt";
const CLIPPY_CONF_SUBDIR: &str = "xtask/tigerstyle";
const USAGE: &str = "usage: cargo xtask tigerstyle [--update]";

/// Every lint tracked by the TigerStyle ratchet (`docs/TIGER_STYLE.md`).
/// Stored using the exact `code.code` string emitted by rustc / clippy.
const TRACKED_LINTS: [&str; 17] = [
    "unsafe_code",
    "unused_must_use",
    "clippy::too_many_lines",
    "clippy::too_many_arguments",
    "clippy::unwrap_used",
    "clippy::expect_used",
    "clippy::let_underscore_must_use",
    "clippy::cast_possible_truncation",
    "clippy::cast_sign_loss",
    "clippy::cast_possible_wrap",
    "clippy::disallowed_methods",
    "clippy::undocumented_unsafe_blocks",
    "clippy::todo",
    "clippy::unimplemented",
    "clippy::dbg_macro",
    "clippy::large_types_passed_by_value",
    "clippy::fn_params_excessive_bools",
];

type Counts = BTreeMap<(String, String), u64>;

/// CLI entry point for `cargo xtask tigerstyle [--update]`.
pub fn run(arguments: &[String]) -> ExitCode {
    let mut update = false;
    for arg in arguments {
        match arg.as_str() {
            "--update" => update = true,
            _ => {
                eprintln!("unknown argument {arg}\n{USAGE}");
                return ExitCode::from(2);
            }
        }
    }
    let root = workspace_root();
    let result = if update {
        update_ratchet(&root)
    } else {
        check_ratchet_at(&root)
    };
    match result {
        Ok(report) => {
            print!("{report}");
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprint!("{err}");
            ExitCode::FAILURE
        }
    }
}

/// CI check entry point called from `xtask/src/jobs.rs`.
pub fn check_ratchet(context: &Context) -> Outcome {
    check_ratchet_at(&context.root)
}

fn workspace_root() -> PathBuf {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    root.canonicalize().unwrap_or(root)
}

fn update_ratchet(root: &Path) -> Outcome {
    let current = collect_counts(root)?;
    let path = root.join(RATCHET_FILE);
    let rendered = format_ratchet(&current);
    std::fs::write(&path, rendered)
        .map_err(|err| format!("failed to write {}: {err}\n", path.display()))?;
    let total: u64 = current.values().sum();
    Ok(format!(
        "updated {} ({} entries, {total} total findings)\n",
        path.display(),
        current.len()
    ))
}

fn check_ratchet_at(root: &Path) -> Outcome {
    let path = root.join(RATCHET_FILE);
    let baseline_text = std::fs::read_to_string(&path).map_err(|err| {
        format!(
            "cannot read {}: {err} (run `cargo xtask tigerstyle --update` to generate)\n",
            path.display()
        )
    })?;
    let baseline = parse_ratchet(&baseline_text)?;
    let current = collect_counts(root)?;
    compare_counts(&baseline, &current)
}

fn collect_counts(root: &Path) -> Result<Counts, String> {
    let conf_dir = root.join(CLIPPY_CONF_SUBDIR);
    let mut command = Command::new("cargo");
    command
        .current_dir(root)
        .env("CLIPPY_CONF_DIR", &conf_dir)
        .args([
            "clippy",
            "--workspace",
            "--lib",
            "--bins",
            "--message-format=json",
            "--",
        ]);
    for lint in TRACKED_LINTS {
        command.arg("-W").arg(lint.replace('_', "-"));
    }
    let output = command
        .output()
        .map_err(|err| format!("failed to run cargo clippy: {err}\n"))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "cargo clippy exited with {}:\n{stderr}\n",
            output.status
        ));
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(count_diagnostics(&stdout))
}

fn count_diagnostics(json_lines: &str) -> Counts {
    let mut counts: Counts = BTreeMap::new();
    for line in json_lines.lines() {
        let Some((package, lint)) = parse_diagnostic_line(line) else {
            continue;
        };
        if TRACKED_LINTS.contains(&lint.as_str()) {
            *counts.entry((package, lint)).or_insert(0) += 1;
        }
    }
    counts
}

fn parse_diagnostic_line(line: &str) -> Option<(String, String)> {
    if !line.contains("\"reason\":\"compiler-message\"") {
        return None;
    }
    let package_id = extract_json_string(line, "\"package_id\":\"")?;
    let package = package_name_from_id(&package_id)?;
    let code_section = line.split("\"code\":{\"code\":\"").nth(1)?;
    let lint = read_json_string_body(code_section)?;
    Some((package, lint))
}

fn package_name_from_id(package_id: &str) -> Option<String> {
    if let Some((before_hash, after_hash)) = package_id.split_once('#') {
        if let Some((name, _version)) = after_hash.split_once('@') {
            return (!name.is_empty()).then(|| name.to_owned());
        }
        let dir = before_hash.rsplit('/').next()?;
        return (!dir.is_empty()).then(|| dir.to_owned());
    }
    let name = package_id.split_whitespace().next()?;
    (!name.is_empty()).then(|| name.to_owned())
}

fn extract_json_string(haystack: &str, key_prefix: &str) -> Option<String> {
    let rest = haystack.split(key_prefix).nth(1)?;
    read_json_string_body(rest)
}

fn read_json_string_body(rest: &str) -> Option<String> {
    let mut out = String::new();
    let mut chars = rest.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => return Some(out),
            '\\' => out.push(chars.next()?),
            _ => out.push(c),
        }
    }
    None
}

fn format_ratchet(counts: &Counts) -> String {
    let mut out = String::from(
        "# TigerStyle lint ratchet (see docs/TIGER_STYLE.md).\n\
         # Regenerate after lowering counts with: cargo xtask tigerstyle --update\n",
    );
    for ((package, lint), count) in counts {
        if *count > 0 {
            out.push_str(&format!("{package} {lint} {count}\n"));
        }
    }
    out
}

fn parse_ratchet(text: &str) -> Result<Counts, String> {
    let mut counts: Counts = BTreeMap::new();
    for (idx, raw_line) in text.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.split_whitespace();
        let (Some(package), Some(lint), Some(count_str), None) =
            (parts.next(), parts.next(), parts.next(), parts.next())
        else {
            return Err(format!(
                "{RATCHET_FILE}:{}: expected `<crate> <lint> <count>`\n",
                idx + 1
            ));
        };
        let count: u64 = count_str.parse().map_err(|err| {
            format!(
                "{RATCHET_FILE}:{}: invalid count `{count_str}`: {err}\n",
                idx + 1
            )
        })?;
        counts.insert((package.to_owned(), lint.to_owned()), count);
    }
    Ok(counts)
}

fn compare_counts(baseline: &Counts, current: &Counts) -> Outcome {
    let keys: BTreeSet<&(String, String)> = baseline.keys().chain(current.keys()).collect();
    let mut regressions = Vec::new();
    let mut improvements = Vec::new();

    for key in keys {
        let base = baseline.get(key).copied().unwrap_or(0);
        let now = current.get(key).copied().unwrap_or(0);
        if now > base {
            regressions.push(format!(
                "  {} {}: {base} -> {now} (+{})",
                key.0,
                key.1,
                now - base
            ));
        } else if now < base {
            improvements.push(format!(
                "  {} {}: {base} -> {now} (-{})",
                key.0,
                key.1,
                base - now
            ));
        }
    }

    if !regressions.is_empty() {
        let mut message =
            String::from("TigerStyle ratchet regression(s) detected (see docs/TIGER_STYLE.md):\n");
        for line in &regressions {
            message.push_str(line);
            message.push('\n');
        }
        return Err(message);
    }

    let total: u64 = current.values().sum();
    let mut summary =
        format!("tigerstyle ratchet ok ({total} tracked findings across workspace)\n");
    if !improvements.is_empty() {
        summary.push_str(
            "Improvements available (run `cargo xtask tigerstyle --update` to lock in):\n",
        );
        for line in &improvements {
            summary.push_str(line);
            summary.push('\n');
        }
    }
    Ok(summary)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_package_id_variants() {
        assert_eq!(
            package_name_from_id("path+file:///repo/crates/oxabl_parser#6.0.0").as_deref(),
            Some("oxabl_parser")
        );
        assert_eq!(
            package_name_from_id("path+file:///repo/crates/oxabl#oxabl@0.1.0").as_deref(),
            Some("oxabl")
        );
        assert_eq!(
            package_name_from_id("oxabl_lexer 3.0.0 (path+file:///repo/crates/oxabl_lexer)")
                .as_deref(),
            Some("oxabl_lexer")
        );
    }

    #[test]
    fn parses_compiler_message_line() {
        let line = r#"{"reason":"compiler-message","package_id":"path+file:///repo/crates/oxabl_parser#6.0.0","message":{"level":"warning","code":{"code":"clippy::too_many_lines","explanation":null}}}"#;
        let parsed = parse_diagnostic_line(line);
        assert_eq!(
            parsed,
            Some((
                "oxabl_parser".to_owned(),
                "clippy::too_many_lines".to_owned()
            ))
        );
    }

    #[test]
    fn ratchet_roundtrip_and_regression_detection() {
        let mut baseline = Counts::new();
        baseline.insert(("oxabl_parser".into(), "clippy::too_many_lines".into()), 10);
        let text = format_ratchet(&baseline);
        let reparsed = parse_ratchet(&text).expect("valid ratchet");
        assert_eq!(reparsed, baseline);

        let mut regressed = baseline.clone();
        regressed.insert(("oxabl_parser".into(), "clippy::too_many_lines".into()), 11);
        assert!(compare_counts(&baseline, &regressed).is_err());

        let mut improved = baseline.clone();
        improved.insert(("oxabl_parser".into(), "clippy::too_many_lines".into()), 8);
        let report = compare_counts(&baseline, &improved).expect("improvement passes");
        assert!(report.contains("10 -> 8"));
    }
}
