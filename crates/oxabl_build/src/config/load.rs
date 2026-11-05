use std::path::{Path, PathBuf};

use oxabl_workspace::{WorkspaceConfig, find_workspace_root};

use super::BuildSettings;
use crate::error::BuildError;

/// A workspace as `oxabl build` sees it: the parsed `oxabl.toml` and the
/// settings lowered from it.
#[derive(Debug)]
pub struct BuildWorkspace {
    pub config: WorkspaceConfig,
    pub settings: BuildSettings,
}

impl BuildWorkspace {
    /// Open the workspace at `root`, or at the nearest ancestor of the current
    /// directory that holds an `oxabl.toml`.
    pub fn open(root: Option<&Path>) -> Result<Self, BuildError> {
        let root = match root {
            Some(root) => absolute(root)?,
            None => {
                let here = absolute(Path::new("."))?;
                find_workspace_root(&here).ok_or_else(|| {
                    BuildError::Usage(
                        "no oxabl.toml found; run oxabl build in a workspace or pass --root"
                            .to_string(),
                    )
                })?
            }
        };
        let config = WorkspaceConfig::from_path(&root).map_err(BuildError::Usage)?;
        let settings = BuildSettings::lower(&config, &root)?;
        Ok(BuildWorkspace { config, settings })
    }
}

fn absolute(path: &Path) -> Result<PathBuf, BuildError> {
    std::path::absolute(path)
        .map_err(|e| BuildError::Usage(format!("cannot resolve {}: {e}", path.display())))
}
