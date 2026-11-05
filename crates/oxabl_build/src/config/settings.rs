use std::path::{Component, Path, PathBuf};

use oxabl_workspace::{BuildConfig, WorkspaceConfig, dlc_sources};

use super::Excludes;
use crate::error::BuildError;

/// The settings of `oxabl build` that the store and the planner use, lowered
/// from the `[build]` and `[workspace.sources]` tables.
#[derive(Debug)]
pub struct BuildSettings {
    pub root: PathBuf,
    /// `<state>`: the lock, stages and logs.
    pub state_dir: PathBuf,
    /// The directory that holds `state.db`.
    pub store_dir: PathBuf,
    /// The host directory of the OpenEdge include sources.
    pub dlc: Option<PathBuf>,
    /// The directories walked for compile units, absolute.
    pub unit_directories: Vec<PathBuf>,
    pub excludes: Excludes,
    /// The `[build.workers].command`, empty when unset.
    pub worker_command: Vec<String>,
}

impl BuildSettings {
    /// Lower `config` for the workspace at the absolute path `root`.
    pub fn lower(config: &WorkspaceConfig, root: &Path) -> Result<Self, BuildError> {
        let build = &config.build;
        let state_dir = under_root(root, "[build].state", &build.state)?;
        let store_dir = match BuildConfig::directory(&build.store) {
            Some(store) if store.is_absolute() => store.to_path_buf(),
            Some(store) => root.join(store),
            None => state_dir.clone(),
        };
        for (key, directory) in [
            ("[build].output", &build.output),
            ("[build.outputs].xref", &build.outputs.xref),
            ("[build.outputs].debug_list", &build.outputs.debug_list),
            ("[build.outputs].listing", &build.outputs.listing),
        ] {
            if let Some(directory) = BuildConfig::directory(directory) {
                under_root(root, key, directory)?;
            }
        }
        let excludes = Excludes::new(&build.exclude).map_err(BuildError::Usage)?;
        let sources = &config.workspace.sources;
        let unit_directories = if sources.directories.is_empty() {
            vec![root.to_path_buf()]
        } else {
            sources.directories.iter().map(|d| root.join(d)).collect()
        };
        Ok(BuildSettings {
            root: root.to_path_buf(),
            state_dir,
            store_dir,
            dlc: dlc_sources(sources.dlc.as_deref(), root),
            unit_directories,
            excludes,
            worker_command: build.workers.command.clone(),
        })
    }
}

/// `path` as a directory under `root`: relative and never leaving it.
fn under_root(root: &Path, key: &str, path: &Path) -> Result<PathBuf, BuildError> {
    let relative = if path.is_absolute() {
        path.strip_prefix(root).map_err(|_| outside(key, path))?
    } else {
        path
    };
    let mut resolved = root.to_path_buf();
    for component in relative.components() {
        match component {
            Component::Normal(part) => resolved.push(part),
            Component::CurDir => {}
            _ => return Err(outside(key, path)),
        }
    }
    Ok(resolved)
}

fn outside(key: &str, path: &Path) -> BuildError {
    BuildError::Usage(format!(
        "{key} = \"{}\" must be a directory under the workspace root",
        path.display()
    ))
}
