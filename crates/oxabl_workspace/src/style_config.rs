//! `[workspace.style]` resolution: discover `oxabl.toml`, read its style table,
//! and apply a CLI `--style` override — mirroring [`resolved_lint_config`] and
//! [`resolved_include_paths`] (nearest ancestor + CLI-wins precedence).
//!
//! Precedence is CLI `--style` > `oxabl.toml [workspace.style]` >
//! [`StyleGuide::default_base`] (KTD1). `--style` is a *whole-guide* selection
//! (a named preset or a TOML file resolved by the caller), so when present it
//! wins outright — it does not merge with the discovered table.
//!
//! Lives in the workspace crate (not the `oxabl` binary) so it is unit-testable
//! and reusable by the LSP.
//!
//! [`resolved_lint_config`]: crate::resolved_lint_config
//! [`resolved_include_paths`]: crate::resolved_include_paths

use std::path::{Path, PathBuf};

use oxabl_style::StyleGuide;

use crate::config::WorkspaceConfig;
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

/// Resolve the effective [`StyleGuide`] for formatting `target`.
///
/// Precedence (KTD1):
/// 1. `cli_style` — when `Some`, it wins outright (a whole-guide selection),
///    returned unchanged with no config error.
/// 2. `oxabl.toml [workspace.style]` — discovered by walking up from `target`;
///    the parsed table is already merged onto [`StyleGuide::default_base`] by
///    serde `#[serde(default)]`.
/// 3. [`StyleGuide::default_base`] — the safe, non-mangling fallback when there
///    is no workspace root.
///
/// A malformed/unreadable `oxabl.toml` (including an unknown `[workspace.style]`
/// key, which `deny_unknown_fields` rejects) degrades to `default_base()` and
/// surfaces the error string in the returned `Option<String>` rather than
/// aborting.
pub fn resolved_style(
    target: &Path,
    cli_style: Option<StyleGuide>,
) -> (StyleGuide, Option<String>) {
    // CLI wins wholesale (KTD1 tier 1): no discovery, no config error.
    if let Some(style) = cli_style {
        return (style, None);
    }

    let abs_target = absolutize(target);
    let start_dir = if abs_target.is_dir() {
        abs_target.clone()
    } else {
        abs_target
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or(abs_target)
    };

    let mut style = StyleGuide::default_base();
    let mut error = None;

    if let Some(root) = find_workspace_root(&start_dir) {
        match WorkspaceConfig::from_path(&root) {
            Ok(cfg) => style = cfg.workspace.style,
            Err(e) => error = Some(e),
        }
    }

    (style, error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_style::IndentStyle;
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
        let (style, err) = resolved_style(&target, None);
        assert!(err.is_none());
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    #[test]
    fn reads_style_table_and_fills_defaults() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 2\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let (style, err) = resolved_style(&target, None);
        assert!(err.is_none());
        // The one set field is overridden.
        assert_eq!(style.indent_size, 2);
        // Everything else falls back to default_base via serde default fill.
        assert_eq!(style.indent_style, IndentStyle::Spaces);
        assert_eq!(style.max_line_length, 120);
    }

    #[test]
    fn unknown_key_is_hard_error_and_degrades_to_default() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nnonexistent_rule = 3\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let (style, err) = resolved_style(&target, None);
        assert!(err.is_some(), "unknown [workspace.style] key must surface");
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    #[test]
    fn nearest_ancestor_wins() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 8\n",
        );
        let deep = root.join("a").join("b");
        fs::create_dir_all(&deep).unwrap();
        write(
            &deep,
            "oxabl.toml",
            "[workspace]\nname = \"child\"\n[workspace.style]\nindent_size = 2\n",
        );
        let target = deep.join("deep.p");
        write(&deep, "deep.p", "");
        let (style, err) = resolved_style(&target, None);
        assert!(err.is_none());
        assert_eq!(style.indent_size, 2, "child oxabl.toml must shadow parent");
    }

    #[test]
    fn cli_style_wins_and_ignores_table() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 2\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");
        let (style, err) = resolved_style(&target, Some(StyleGuide::oestandards()));
        assert!(err.is_none());
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::oestandards().to_toml().unwrap(),
            "CLI --style must win wholesale, ignoring the discovered table"
        );
    }

    #[test]
    fn file_target_starts_walk_at_parent_dir() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 3\n",
        );
        let target = root.join("bare.p");
        write(root, "bare.p", "");
        let (style, err) = resolved_style(&target, None);
        assert!(err.is_none());
        assert_eq!(style.indent_size, 3);
    }
}
