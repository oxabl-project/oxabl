//! Include-path resolution: auto-discover `oxabl.toml`, merge its
//! `[workspace.sources].include_paths` with CLI `-I` flags, and hand a single
//! absolute, first-match-ordered PROPATH to the preprocessor.
//!
//! Lives here (not in the `oxabl` binary) so it is unit-testable and reusable
//! by the future LSP — a binary target's private helpers cannot be imported by
//! an integration test crate.

use std::path::{Path, PathBuf};

use crate::config::WorkspaceConfig;

/// Make a path absolute without touching the filesystem.
///
/// Relative paths are joined against the current working directory. We do
/// **not** canonicalize (that errors on not-yet-existing dirs) and do **not**
/// collapse `..` (there is no allocation-free std normalizer, and
/// `resolve_include`'s `dir.join(name)` + `exists()` handle embedded `..`
/// fine). A configured-but-absent dir simply never matches.
fn absolutize(p: &Path) -> PathBuf {
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(p))
            .unwrap_or_else(|_| p.to_path_buf())
    }
}

/// Walk upward from `start` to the filesystem root, returning the nearest
/// ancestor directory that contains an `oxabl.toml`, or `None`.
///
/// `start` should be absolute; [`resolved_include_paths`] absolutizes before
/// calling so the walk actually reaches the root.
pub fn find_workspace_root(start: &Path) -> Option<PathBuf> {
    let mut current = Some(start);
    while let Some(dir) = current {
        if dir.join("oxabl.toml").is_file() {
            return Some(dir.to_path_buf());
        }
        current = dir.parent();
    }
    None
}

/// Compute the effective include search path for analyzing `target`.
///
/// Discovers `oxabl.toml` by walking up from `target` (its parent directory if
/// `target` is a file), loads its `[workspace.sources].include_paths`, and
/// merges them with the CLI `-I` flags. The merged list is:
///
/// ```text
/// [<all CLI flags, in order>, <all config paths, in file order>]
/// ```
///
/// CLI-first means an explicit `-I` shadows a config entry for the same include
/// name (first-match-wins), while config entries still fill in everything the
/// user didn't type. CLI paths are anchored to the cwd; config paths are
/// anchored to the workspace root (the dir containing `oxabl.toml`). All entries
/// are absolutized, and exact duplicates are removed while preserving order.
///
/// Returns the merged paths plus an optional error message: a malformed or
/// unreadable `oxabl.toml` degrades to flags-only behavior and surfaces the
/// error string rather than aborting.
///
/// Callers resolve this once per invocation from the check/analyze target
/// (`check <dir>` anchors on the directory, `analyze <file>` on the file). A
/// single workspace per run is assumed: nested projects with their own
/// `oxabl.toml` under a directory target are not discovered per-file.
///
/// The ancestor walk has no stop boundary below the filesystem root, so an
/// `oxabl.toml` in an unexpected ancestor would be picked up (the nearest one
/// wins). This matches how `cargo`/`rustfmt`/`tsc` locate their config.
pub fn resolved_include_paths(target: &Path, cli: &[PathBuf]) -> (Vec<PathBuf>, Option<String>) {
    let abs_target = absolutize(target);
    let start_dir = if abs_target.is_dir() {
        abs_target.clone()
    } else {
        abs_target
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or(abs_target)
    };

    let mut merged: Vec<PathBuf> = cli.iter().map(|p| absolutize(p)).collect();
    let mut error = None;

    if let Some(root) = find_workspace_root(&start_dir) {
        match WorkspaceConfig::from_path(&root) {
            Ok(config) => {
                for p in &config.workspace.sources.include_paths {
                    // Config-relative paths anchor to the workspace root; absolute
                    // config paths are used verbatim.
                    let anchored = if p.is_absolute() {
                        p.clone()
                    } else {
                        root.join(p)
                    };
                    merged.push(anchored);
                }
            }
            Err(e) => error = Some(e),
        }
    }

    // Order-preserving dedup so a dir named by both a flag and the config isn't
    // stat'd twice per include.
    let mut seen = std::collections::HashSet::new();
    merged.retain(|p| seen.insert(p.clone()));

    (merged, error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write(dir: &Path, name: &str, contents: &str) {
        fs::write(dir.join(name), contents).unwrap();
    }

    #[test]
    fn config_include_paths_loaded_when_no_flags() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");

        let (paths, err) = resolved_include_paths(&target, &[]);
        assert!(err.is_none());
        assert_eq!(paths, vec![root.join("inc")]);
    }

    #[test]
    fn cli_flag_and_config_paths_merge_cli_first() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\"/cfg\"]\n",
        );
        let target = root.join("main.p");
        write(root, "main.p", "");

        let (paths, _) = resolved_include_paths(&target, &[PathBuf::from("/cli")]);
        assert_eq!(paths, vec![PathBuf::from("/cli"), PathBuf::from("/cfg")]);
    }

    #[test]
    fn config_relative_paths_anchored_to_workspace_root() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
        );
        let sub = root.join("a").join("b");
        fs::create_dir_all(&sub).unwrap();
        let target = sub.join("deep.p");
        write(&sub, "deep.p", "");

        let (paths, _) = resolved_include_paths(&target, &[]);
        // Anchored to the root that holds oxabl.toml, not the file's subdir.
        assert_eq!(paths, vec![root.join("inc")]);
    }

    #[test]
    fn find_workspace_root_walks_ancestors() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "oxabl.toml", "[workspace]\nname = \"p\"\n");
        let deep = root.join("x").join("y").join("z");
        fs::create_dir_all(&deep).unwrap();

        assert_eq!(find_workspace_root(&deep), Some(root.to_path_buf()));
    }

    #[test]
    fn missing_oxabl_toml_returns_flags_only() {
        let tmp = TempDir::new().unwrap();
        let target = tmp.path().join("lonely.p");
        write(tmp.path(), "lonely.p", "");

        let cli = vec![PathBuf::from("/only/this")];
        let (paths, err) = resolved_include_paths(&target, &cli);
        assert!(err.is_none());
        assert_eq!(paths, cli);
    }

    #[test]
    fn malformed_oxabl_toml_surfaces_error_not_panic() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "oxabl.toml", "this is not valid toml {{{");
        let target = root.join("main.p");
        write(root, "main.p", "");

        let cli = vec![PathBuf::from("/flag")];
        let (paths, err) = resolved_include_paths(&target, &cli);
        assert!(err.is_some(), "malformed config must surface an error");
        // Degrades to flags-only rather than aborting.
        assert_eq!(paths, cli);
    }

    #[test]
    fn absolutize_makes_relative_paths_absolute_and_leaves_absolute_paths() {
        // A relative CLI/config path must be joined against the cwd so the
        // ancestor walk doesn't short-circuit on an empty parent; an absolute
        // path must pass through untouched.
        let abs = PathBuf::from("/already/absolute");
        assert_eq!(absolutize(&abs), abs);

        let rel = Path::new("some/rel/dir");
        let out = absolutize(rel);
        assert!(out.is_absolute(), "relative path must be absolutized");
        assert!(out.ends_with("some/rel/dir"));
    }

    #[test]
    fn file_target_starts_walk_at_parent_dir() {
        // A file target (not a dir) must begin the ancestor walk at its parent,
        // finding a sibling oxabl.toml — the non-vacuous file-vs-dir check.
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
        );
        let target = root.join("bare.p");
        write(root, "bare.p", "");
        let (paths, err) = resolved_include_paths(&target, &[]);
        assert!(err.is_none());
        assert_eq!(paths, vec![root.join("inc")]);
    }
}
