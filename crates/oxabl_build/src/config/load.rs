use std::path::{Path, PathBuf};

use oxabl_workspace::{WorkspaceConfig, find_workspace_root};

use super::{BuildSettings, Layers, Overlay};
use crate::error::BuildError;

/// A workspace as `oxabl build` sees it: the parsed `oxabl.toml` (with the
/// overlay merged over it) and the settings lowered from it.
#[derive(Debug)]
pub struct BuildWorkspace {
    pub config: WorkspaceConfig,
    pub settings: BuildSettings,
    pub layers: Layers,
}

impl BuildWorkspace {
    /// Open the workspace at `root`, or at the nearest ancestor of the current
    /// directory that holds an `oxabl.toml`.
    pub fn open(root: Option<&Path>) -> Result<Self, BuildError> {
        Self::open_with(root, None)
    }

    /// Like [`BuildWorkspace::open`], with `overlay` merged over `oxabl.toml`.
    /// With an overlay, `oxabl.toml` may be absent.
    pub fn open_with(root: Option<&Path>, overlay: Option<&Overlay>) -> Result<Self, BuildError> {
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
        let layers = Layers::read(&root, overlay)?;
        let config = layers.parse()?;
        let settings = BuildSettings::lower(&config, &root)?;
        Ok(BuildWorkspace {
            config,
            settings,
            layers,
        })
    }
}

fn absolute(path: &Path) -> Result<PathBuf, BuildError> {
    std::path::absolute(path)
        .map_err(|e| BuildError::Usage(format!("cannot resolve {}: {e}", path.display())))
}
