//! Workspace-root discovery: walk up from a target to the nearest ancestor
//! holding an `oxabl.toml`.
//!
//! Include-path *resolution* used to live here too, as `resolved_include_paths`.
//! It was a line-for-line duplicate of the derivation
//! `oxabl_pipeline::resolve_from_config` performs — same CLI-first merge, same
//! root anchoring, same order-preserving dedup — so the two could drift while
//! looking identical, and its last caller (the `conformance` walk) moved onto the
//! shared resolution. One derivation now, in the crate that owns configuration.
//!
//! What remains is the discovery step that resolution calls into. It lives here
//! rather than in `oxabl_pipeline` because it is bound to the real filesystem and
//! predates that crate.

use std::path::{Path, PathBuf};

/// Walk upward from `start` to the filesystem root, returning the nearest
/// ancestor directory that contains an `oxabl.toml`, or `None`.
///
/// `start` must be absolute — a relative path's ancestor walk short-circuits on
/// an empty parent long before reaching the root, so callers absolutize first.
///
/// The walk has no stop boundary below the filesystem root, so an `oxabl.toml` in
/// an unexpected ancestor would be picked up (the nearest one wins). This matches
/// how `cargo`, `rustfmt`, and `tsc` locate their config.
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn find_workspace_root_walks_ancestors() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::write(root.join("oxabl.toml"), "[workspace]\nname = \"p\"\n").unwrap();
        let deep = root.join("x").join("y").join("z");
        fs::create_dir_all(&deep).unwrap();

        assert_eq!(find_workspace_root(&deep), Some(root.to_path_buf()));
    }

    /// Nothing under the fixture matches. Asserted that way rather than as a flat
    /// `is_none()` because the walk deliberately has no stop boundary: a real
    /// `oxabl.toml` somewhere above the temp directory would be a correct hit, and
    /// a test that failed on it would be flaky rather than right.
    #[test]
    fn find_workspace_root_matches_nothing_inside_a_config_free_tree() {
        let tmp = TempDir::new().unwrap();
        let deep = tmp.path().join("x").join("y");
        fs::create_dir_all(&deep).unwrap();

        match find_workspace_root(&deep) {
            None => {}
            Some(found) => assert!(
                !found.starts_with(tmp.path()),
                "no oxabl.toml exists under the fixture, but {} matched",
                found.display()
            ),
        }
    }
}
