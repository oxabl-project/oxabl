use std::path::{Path, PathBuf};

use oxabl_workspace::WorkspaceConfig;
use toml::{Table, Value};

use super::rules::{check_overlay_keys, check_secret_keys, check_trust_keys};
use crate::error::BuildError;

/// Where the overlay file was named.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverlayOrigin {
    /// `--config-overlay`.
    Flag,
    /// `OXABL_BUILD_OVERLAY`.
    Environment,
}

/// An overlay file that the command line or the environment named.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Overlay {
    pub path: PathBuf,
    pub origin: OverlayOrigin,
}

/// `oxabl.toml` and the overlay as two documents, merged key by key.
#[derive(Debug)]
pub struct Layers {
    pub base: Option<Table>,
    pub overlay: Option<Table>,
    pub overlay_file: Option<Overlay>,
    base_text: Option<String>,
}

fn parse_table(text: &str, what: &str) -> Result<Table, BuildError> {
    text.parse::<Table>()
        .map_err(|e| BuildError::Usage(format!("failed to parse {what}: {e}")))
}

fn read(path: &Path) -> Result<String, BuildError> {
    std::fs::read_to_string(path)
        .map_err(|e| BuildError::Usage(format!("failed to read {}: {e}", path.display())))
}

impl Layers {
    /// Read `<root>/oxabl.toml` and the overlay. A missing `oxabl.toml` is an
    /// error unless an overlay is given.
    pub fn read(root: &Path, overlay: Option<&Overlay>) -> Result<Layers, BuildError> {
        let base_path = root.join("oxabl.toml");
        let base_text = match (base_path.is_file(), overlay) {
            (true, _) => Some(read(&base_path)?),
            (false, None) => return Err(read(&base_path).unwrap_err()),
            (false, Some(_)) => None,
        };
        let base = base_text
            .as_deref()
            .map(|text| parse_table(text, "oxabl.toml"))
            .transpose()?;
        if let Some(base) = &base {
            check_trust_keys(base, "oxabl.toml")?;
            check_secret_keys(base, "oxabl.toml")?;
        }
        let overlay_table = match overlay {
            None => None,
            Some(overlay) => {
                let file = overlay.path.display().to_string();
                let table = parse_table(&read(&overlay.path)?, &format!("overlay {file}"))?;
                check_overlay_keys(&table, &file)?;
                check_trust_keys(&table, &file)?;
                check_secret_keys(&table, &file)?;
                Some(table)
            }
        };
        Ok(Layers {
            base,
            overlay: overlay_table,
            overlay_file: overlay.cloned(),
            base_text,
        })
    }

    /// The overlay merged over `oxabl.toml`: tables merge key by key; an array
    /// or a scalar in the overlay replaces the value.
    pub fn merged(&self) -> Table {
        let mut merged = self.base.clone().unwrap_or_default();
        if let Some(overlay) = &self.overlay {
            merge(&mut merged, overlay);
        }
        merged
    }

    /// The strictly parsed configuration.
    pub fn parse(&self) -> Result<WorkspaceConfig, BuildError> {
        let Some(overlay) = &self.overlay_file else {
            let text = self.base_text.as_deref().unwrap_or_default();
            return WorkspaceConfig::from_toml(text).map_err(BuildError::Usage);
        };
        let merged = self.merged();
        let named = merged
            .get("workspace")
            .and_then(Value::as_table)
            .is_some_and(|workspace| workspace.contains_key("name"));
        if !named {
            return Err(BuildError::Usage(format!(
                "invalid configuration: set [workspace] name in oxabl.toml or in the overlay {}",
                overlay.path.display()
            )));
        }
        Value::Table(merged)
            .try_into::<WorkspaceConfig>()
            .map_err(|e| {
                let name = match self.base {
                    Some(_) => "oxabl.toml merged with the overlay",
                    None => "the overlay",
                };
                BuildError::Usage(format!(
                    "invalid configuration ({name} {}): {e}",
                    overlay.path.display()
                ))
            })
    }
}

fn merge(base: &mut Table, overlay: &Table) {
    for (key, value) in overlay {
        match (base.get_mut(key), value) {
            (Some(Value::Table(base)), Value::Table(overlay)) => merge(base, overlay),
            _ => {
                base.insert(key.clone(), value.clone());
            }
        }
    }
}
