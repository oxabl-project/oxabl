use std::path::Path;
use std::sync::Arc;

use oxabl_common::FileSet;

use crate::config::WorkspaceConfig;
use crate::discovery::{is_root_file, walk_directory};
use crate::file_system::{FileSystem, InMemoryFileSystem, RealFileSystem};

/// Top-level handle combining configuration, file identity, and file I/O.
///
/// Constructed via [`Workspace::from_path`] (real disk) or
/// [`Workspace::in_memory`] (tests / LSP).
pub struct Workspace {
    pub config: WorkspaceConfig,
    pub file_set: FileSet,
    fs: Arc<dyn FileSystem>,
}

impl std::fmt::Debug for Workspace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Workspace")
            .field("config", &self.config)
            .field("file_set", &self.file_set)
            .finish_non_exhaustive()
    }
}

impl Workspace {
    /// Discover a workspace rooted at `root`.
    ///
    /// Reads `oxabl.toml` from `root`, then walks all declared source
    /// directories under the shared root policy ([`is_root_file`]: `.p`, `.w`,
    /// `.cls`, `.v`, case-insensitive, `.i` excluded), assigning each discovered
    /// file a [`FileId`](oxabl_common::FileId).
    ///
    /// [`is_root_file`]: crate::is_root_file
    pub fn from_path(root: &Path) -> Result<Self, String> {
        let config = WorkspaceConfig::from_path(root)?;
        let fs = Arc::new(RealFileSystem);
        let file_set = discover_files(root, &config);
        Ok(Workspace {
            config,
            file_set,
            fs,
        })
    }

    /// Create a workspace from an in-memory file system.
    ///
    /// The caller provides the config and the pre-populated
    /// [`InMemoryFileSystem`]. Paths that are ABL roots under the shared
    /// policy are registered in the [`FileSet`].
    ///
    /// This deliberately uses the same predicate as the on-disk walk (R8): an
    /// in-memory workspace must register the same root set a walked one would,
    /// or tests and the LSP would be reasoning about a file set the CLI never
    /// produces.
    pub fn in_memory(config: WorkspaceConfig, mem_fs: InMemoryFileSystem) -> Self {
        let mut file_set = FileSet::new();
        for path in mem_fs.paths() {
            if is_root_file(path) {
                file_set.insert(path.to_path_buf());
            }
        }
        Workspace {
            config,
            file_set,
            fs: Arc::new(mem_fs),
        }
    }

    /// Access the underlying [`FileSystem`].
    pub fn fs(&self) -> &dyn FileSystem {
        &*self.fs
    }
}

/// Walk declared source directories and collect ABL roots into a [`FileSet`].
///
/// Keeps its own per-declared-directory loop rather than calling
/// [`crate::discover_path`] (KTD10): this shape resolves each configured
/// directory against `root`, silently skips ones that are absent, and folds
/// every walk into one `FileSet` — a different job from resolving a single
/// user-supplied path. The shared piece is the walk primitive and the extension
/// policy, not the top-level signature.
fn discover_files(root: &Path, config: &WorkspaceConfig) -> FileSet {
    let mut file_set = FileSet::new();

    for dir in &config.workspace.sources.directories {
        let abs_dir = if dir.is_absolute() {
            dir.clone()
        } else {
            root.join(dir)
        };

        // A configured-but-absent directory is not an error; the walk of a
        // missing path would return empty anyway, but checking keeps the intent
        // explicit.
        if !abs_dir.is_dir() {
            continue;
        }

        for path in walk_directory(&abs_dir) {
            file_set.insert(path);
        }
    }

    file_set
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    fn test_config(dirs: &[&str], include_paths: &[&str]) -> WorkspaceConfig {
        let toml = format!(
            r#"
[workspace]
name = "test"

[workspace.sources]
directories = [{}]
include_paths = [{}]
"#,
            dirs.iter()
                .map(|d| format!("\"{d}\""))
                .collect::<Vec<_>>()
                .join(", "),
            include_paths
                .iter()
                .map(|d| format!("\"{d}\""))
                .collect::<Vec<_>>()
                .join(", "),
        );
        WorkspaceConfig::from_toml(&toml).unwrap()
    }

    #[test]
    fn in_memory_discovers_abl_files() {
        let config = test_config(&[], &[]);
        let mut mem_fs = InMemoryFileSystem::new();
        mem_fs.insert(PathBuf::from("/src/main.p"), "");
        mem_fs.insert(PathBuf::from("/src/window.w"), "");
        mem_fs.insert(PathBuf::from("/src/Report.cls"), "");
        mem_fs.insert(PathBuf::from("/src/legacy.v"), "");
        mem_fs.insert(PathBuf::from("/src/shared.i"), ""); // R9: not a root
        mem_fs.insert(PathBuf::from("/src/readme.txt"), ""); // not ABL

        let ws = Workspace::in_memory(config, mem_fs);
        assert_eq!(ws.file_set.len(), 4);
        assert!(ws.file_set.id(Path::new("/src/shared.i")).is_none());
    }

    /// R8: `in_memory` must register exactly the roots a disk walk would —
    /// same extension set, same case-insensitivity, `.i` excluded.
    #[test]
    fn in_memory_matches_the_shared_root_policy() {
        let config = test_config(&[], &[]);
        let mut mem_fs = InMemoryFileSystem::new();
        mem_fs.insert(PathBuf::from("/src/lower.p"), "");
        mem_fs.insert(PathBuf::from("/src/UPPER.P"), "");
        mem_fs.insert(PathBuf::from("/src/fragment.i"), "");

        let ws = Workspace::in_memory(config, mem_fs);
        assert_eq!(ws.file_set.len(), 2);
        assert!(ws.file_set.id(Path::new("/src/lower.p")).is_some());
        assert!(ws.file_set.id(Path::new("/src/UPPER.P")).is_some());
        assert!(ws.file_set.id(Path::new("/src/fragment.i")).is_none());
    }

    #[test]
    fn in_memory_empty() {
        let config = test_config(&[], &[]);
        let mem_fs = InMemoryFileSystem::new();
        let ws = Workspace::in_memory(config, mem_fs);
        assert!(ws.file_set.is_empty());
    }

    #[test]
    fn resolve_include_through_workspace() {
        let config = test_config(&[], &["/inc1", "/inc2"]);
        let mut mem_fs = InMemoryFileSystem::new();
        mem_fs.insert(PathBuf::from("/inc2/shared.i"), "/* shared */");

        let ws = Workspace::in_memory(config, mem_fs);
        let resolved = ws
            .fs()
            .resolve_include(&ws.config.workspace.sources.include_paths, "shared.i");
        assert_eq!(resolved, Some(PathBuf::from("/inc2/shared.i")));
    }

    #[test]
    fn from_path_missing_config() {
        let err = Workspace::from_path(Path::new("/nonexistent/path")).unwrap_err();
        assert!(err.contains("failed to read"));
    }

    #[test]
    fn in_memory_file_set_assigns_unique_ids() {
        let config = test_config(&[], &[]);
        let mut mem_fs = InMemoryFileSystem::new();
        mem_fs.insert(PathBuf::from("/a.p"), "");
        mem_fs.insert(PathBuf::from("/b.p"), "");

        let ws = Workspace::in_memory(config, mem_fs);
        let id_a = ws.file_set.id(Path::new("/a.p")).unwrap();
        let id_b = ws.file_set.id(Path::new("/b.p")).unwrap();
        assert_ne!(id_a, id_b);
    }

    #[test]
    fn in_memory_fs_readable_through_workspace() {
        let config = test_config(&[], &[]);
        let mut mem_fs = InMemoryFileSystem::new();
        mem_fs.insert(
            PathBuf::from("/src/main.p"),
            "DEFINE VARIABLE x AS INTEGER.",
        );

        let ws = Workspace::in_memory(config, mem_fs);
        let content = ws.fs().read(Path::new("/src/main.p")).unwrap();
        assert_eq!(&*content, "DEFINE VARIABLE x AS INTEGER.");
    }

    #[test]
    fn from_path_discovers_files_on_disk() {
        let tmp = std::env::temp_dir().join("oxabl_workspace_test_discover");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(tmp.join("src/sub")).unwrap();
        std::fs::create_dir_all(tmp.join("procs")).unwrap();

        // Write ABL files
        std::fs::write(tmp.join("src/main.p"), "").unwrap();
        std::fs::write(tmp.join("src/sub/nested.w"), "").unwrap();
        std::fs::write(tmp.join("procs/run.p"), "").unwrap();
        std::fs::write(tmp.join("procs/include.i"), "").unwrap();
        std::fs::write(tmp.join("procs/Report.cls"), "").unwrap();
        // Uppercase extension: a root under the shared case-insensitive policy.
        std::fs::write(tmp.join("procs/LEGACY.V"), "").unwrap();
        // Non-ABL file
        std::fs::write(tmp.join("src/readme.md"), "").unwrap();

        // Write oxabl.toml
        std::fs::write(
            tmp.join("oxabl.toml"),
            r#"
[workspace]
name = "disk-test"

[workspace.sources]
directories = ["src/", "procs/"]
"#,
        )
        .unwrap();

        let ws = Workspace::from_path(&tmp).unwrap();
        assert_eq!(ws.config.workspace.name, "disk-test");
        // main.p, nested.w, run.p, Report.cls, LEGACY.V — the `.i` fragment is
        // not a root (R9) and readme.md is not ABL.
        assert_eq!(ws.file_set.len(), 5);
        assert!(ws.file_set.id(&tmp.join("procs/include.i")).is_none());
        assert!(ws.file_set.id(&tmp.join("procs/LEGACY.V")).is_some());

        // Clean up
        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn from_path_skips_nonexistent_source_dir() {
        let tmp = std::env::temp_dir().join("oxabl_workspace_test_skip");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(tmp.join("src")).unwrap();

        std::fs::write(tmp.join("src/a.p"), "").unwrap();
        std::fs::write(
            tmp.join("oxabl.toml"),
            r#"
[workspace]
name = "skip-test"

[workspace.sources]
directories = ["src/", "nonexistent/"]
"#,
        )
        .unwrap();

        let ws = Workspace::from_path(&tmp).unwrap();
        assert_eq!(ws.file_set.len(), 1);

        let _ = std::fs::remove_dir_all(&tmp);
    }
}
