use std::path::Path;
use std::sync::Arc;

use oxabl_common::FileSet;

use crate::config::WorkspaceConfig;
use crate::file_system::{FileSystem, InMemoryFileSystem, RealFileSystem};

/// ABL file extensions that the workspace scanner collects.
const ABL_EXTENSIONS: &[&str] = &["p", "w", "cls", "i"];

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
    /// directories to discover `.p`, `.w`, `.cls`, and `.i` files, assigning
    /// each a [`FileId`](oxabl_common::FileId).
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
    /// [`InMemoryFileSystem`]. All paths present in the file system that
    /// match ABL extensions are registered in the [`FileSet`].
    pub fn in_memory(config: WorkspaceConfig, mem_fs: InMemoryFileSystem) -> Self {
        let mut file_set = FileSet::new();
        // Register all files in the in-memory FS that have ABL extensions.
        // We need to iterate the keys — expose them via a helper.
        for path in mem_fs.paths() {
            if is_abl_file(path) {
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

/// Walk declared source directories and collect ABL files into a [`FileSet`].
fn discover_files(root: &Path, config: &WorkspaceConfig) -> FileSet {
    let mut file_set = FileSet::new();

    for dir in &config.workspace.sources.directories {
        let abs_dir = if dir.is_absolute() {
            dir.clone()
        } else {
            root.join(dir)
        };

        if !abs_dir.is_dir() {
            continue;
        }

        for entry in walkdir::WalkDir::new(&abs_dir)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.is_file() && is_abl_file(path) {
                file_set.insert(path.to_path_buf());
            }
        }
    }

    file_set
}

/// Check if a path has an ABL source file extension.
fn is_abl_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ABL_EXTENSIONS.contains(&ext))
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
        mem_fs.insert(PathBuf::from("/src/Customer.cls"), "");
        mem_fs.insert(PathBuf::from("/src/shared.i"), "");
        mem_fs.insert(PathBuf::from("/src/readme.txt"), ""); // not ABL

        let ws = Workspace::in_memory(config, mem_fs);
        assert_eq!(ws.file_set.len(), 4);
    }

    #[test]
    fn in_memory_empty() {
        let config = test_config(&[], &[]);
        let mem_fs = InMemoryFileSystem::new();
        let ws = Workspace::in_memory(config, mem_fs);
        assert!(ws.file_set.is_empty());
    }

    #[test]
    fn is_abl_file_checks() {
        assert!(is_abl_file(Path::new("main.p")));
        assert!(is_abl_file(Path::new("window.w")));
        assert!(is_abl_file(Path::new("Customer.cls")));
        assert!(is_abl_file(Path::new("shared.i")));
        assert!(!is_abl_file(Path::new("readme.txt")));
        assert!(!is_abl_file(Path::new("Makefile")));
        assert!(!is_abl_file(Path::new("no_extension")));
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
        std::fs::write(tmp.join("procs/Customer.cls"), "").unwrap();
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
        assert_eq!(ws.file_set.len(), 5); // main.p, nested.w, run.p, include.i, Customer.cls

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
