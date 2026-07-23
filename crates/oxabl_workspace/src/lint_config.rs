//! `[workspace.lint]` resolution: discover `oxabl.toml`, read its lint table,
//! and apply CLI overrides — mirroring [`resolved_include_paths`] (nearest
//! ancestor + CLI-first precedence: CLI > `oxabl.toml` > default, R15).
//!
//! Lives in the workspace crate (not the `oxabl` binary) so it is unit-testable
//! and reusable by the LSP.

use std::path::{Path, PathBuf};

use crate::config::{LintConfig, LintSeverity, WorkspaceConfig};
use crate::include_paths::find_workspace_root;

fn absolutize(p: &Path) -> PathBuf {
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(p))
            .unwrap_or_else(|_| p.to_path_buf())
    }
}

/// Resolve the effective [`LintConfig`] for analyzing `target`.
///
/// Discovers `oxabl.toml` by walking up from `target`, reads its
/// `[workspace.lint]` table (or the safe default if there is no config), then
/// applies `cli_overrides` (rule kebab-name → severity) on top so an explicit
/// CLI flag beats the file which beats the default. A malformed/unreadable
/// `oxabl.toml` degrades to the default table and surfaces the error string.
///
/// Unknown CLI override rule names are ignored (they never match a real rule);
/// they cannot come from `oxabl.toml`, where `deny_unknown_fields` rejects them.
pub fn resolved_lint_config(
    target: &Path,
    cli_overrides: &[(String, LintSeverity)],
) -> (LintConfig, Option<String>) {
    let abs_target = absolutize(target);
    let start_dir = if abs_target.is_dir() {
        abs_target.clone()
    } else {
        abs_target
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or(abs_target)
    };

    let mut config = LintConfig::default();
    let mut error = None;

    if let Some(root) = find_workspace_root(&start_dir) {
        match WorkspaceConfig::from_path(&root) {
            Ok(cfg) => config = cfg.workspace.lint,
            Err(e) => error = Some(e),
        }
    }

    // CLI overrides win over the resolved file/default.
    for (rule, severity) in cli_overrides {
        config.set_by_name(rule, *severity);
    }

    (config, error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::LintConfig;
    use oxabl_common::Severity;
    use std::fs;
    use tempfile::TempDir;

    fn write(dir: &Path, name: &str, contents: &str) {
        fs::write(dir.join(name), contents).unwrap();
    }

    #[test]
    fn default_when_no_config() {
        let tmp = TempDir::new().unwrap();
        let target = tmp.path().join("main.p");
        write(tmp.path(), "main.p", "");
        let (cfg, err) = resolved_lint_config(&target, &[]);
        assert!(err.is_none());
        assert_eq!(cfg, LintConfig::default());
        // Default map: undefined-symbol = error, the other three warn.
        let map = cfg.to_severity_map();
        assert_eq!(map.get("LINT0001"), Some(Some(Severity::Error)));
        assert_eq!(map.get("LINT0002"), Some(Some(Severity::Warning)));
        assert_eq!(map.get("LINT0003"), Some(Some(Severity::Warning)));
        assert_eq!(map.get("LINT0004"), Some(Some(Severity::Warning)));
    }

    #[test]
    fn reads_lint_table_from_oxabl_toml() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.lint]\nunused-variable = \"info\"\nundefined-symbol = \"off\"\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let (cfg, err) = resolved_lint_config(&target, &[]);
        assert!(err.is_none());
        assert_eq!(cfg.unused_variable, LintSeverity::Info);
        assert_eq!(cfg.undefined_symbol, LintSeverity::Off);
        // Unspecified keys keep the default.
        assert_eq!(cfg.type_mismatch_assignment, LintSeverity::Warn);
        let map = cfg.to_severity_map();
        assert_eq!(map.get("LINT0001"), Some(None)); // off
        assert_eq!(map.get("LINT0002"), Some(Some(Severity::Info)));
    }

    #[test]
    fn cli_override_beats_file() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.lint]\nunused-variable = \"info\"\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let cli = vec![("unused-variable".to_string(), LintSeverity::Error)];
        let (cfg, _) = resolved_lint_config(&target, &cli);
        assert_eq!(cfg.unused_variable, LintSeverity::Error);
    }

    #[test]
    fn nearest_ancestor_resolves_lint_table() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.lint]\ntype-mismatch-assignment = \"off\"\n",
        );
        let deep = root.join("a").join("b");
        fs::create_dir_all(&deep).unwrap();
        let target = deep.join("deep.p");
        write(&deep, "deep.p", "");
        let (cfg, err) = resolved_lint_config(&target, &[]);
        assert!(err.is_none());
        assert_eq!(cfg.type_mismatch_assignment, LintSeverity::Off);
    }

    #[test]
    fn unknown_key_is_hard_error() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.lint]\nnonexistent-rule = \"warn\"\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let (cfg, err) = resolved_lint_config(&target, &[]);
        // Degrades to default and surfaces the parse error (deny_unknown_fields).
        assert!(err.is_some(), "unknown [lint] key must surface an error");
        assert_eq!(cfg, LintConfig::default());
    }
}
