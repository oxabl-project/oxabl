use std::path::{Path, PathBuf};

use serde::Deserialize;

/// Top-level configuration deserialized from `oxabl.toml`.
#[derive(Debug, Clone, Deserialize)]
pub struct WorkspaceConfig {
    pub workspace: WorkspaceSection,
}

/// The `[workspace]` section of `oxabl.toml`.
#[derive(Debug, Clone, Deserialize)]
pub struct WorkspaceSection {
    /// Human-readable project name.
    pub name: String,

    /// Source file locations and include paths.
    #[serde(default)]
    pub sources: SourcesConfig,

    /// Optional schema (`.df`) file declarations.
    #[serde(default)]
    pub schema: SchemaConfig,
}

/// The `[workspace.sources]` section.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct SourcesConfig {
    /// Directories containing ABL source files (`.p`, `.w`, `.cls`, `.i`).
    #[serde(default)]
    pub directories: Vec<PathBuf>,

    /// Include search paths, searched in order (PROPATH equivalent).
    #[serde(default)]
    pub include_paths: Vec<PathBuf>,
}

/// The `[workspace.schema]` section.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct SchemaConfig {
    /// Paths to `.df` schema dump files.
    #[serde(default)]
    pub files: Vec<PathBuf>,
}

impl WorkspaceConfig {
    /// Parse an `oxabl.toml` string into a [`WorkspaceConfig`].
    ///
    /// Returns a human-readable error string on parse failure rather than
    /// panicking, so callers can surface it as a diagnostic.
    pub fn from_toml(source: &str) -> Result<Self, String> {
        toml::from_str(source).map_err(|e| format!("failed to parse oxabl.toml: {e}"))
    }

    /// Read and parse `oxabl.toml` from a project root directory.
    pub fn from_path(root: &Path) -> Result<Self, String> {
        let config_path = root.join("oxabl.toml");
        let source = std::fs::read_to_string(&config_path)
            .map_err(|e| format!("failed to read {}: {e}", config_path.display()))?;
        Self::from_toml(&source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_minimal_config() {
        let toml = r#"
[workspace]
name = "my-project"
"#;
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert_eq!(config.workspace.name, "my-project");
        assert!(config.workspace.sources.directories.is_empty());
        assert!(config.workspace.sources.include_paths.is_empty());
        assert!(config.workspace.schema.files.is_empty());
    }

    #[test]
    fn parse_full_config() {
        let toml = r#"
[workspace]
name = "sports2000"

[workspace.sources]
directories = ["src/", "procedures/"]
include_paths = ["src/include/", "/shared/abl/"]

[workspace.schema]
files = ["schema/sports2000.df"]
"#;
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert_eq!(config.workspace.name, "sports2000");
        assert_eq!(config.workspace.sources.directories.len(), 2);
        assert_eq!(config.workspace.sources.include_paths.len(), 2);
        assert_eq!(config.workspace.schema.files.len(), 1);
        assert_eq!(
            config.workspace.schema.files[0],
            PathBuf::from("schema/sports2000.df")
        );
    }

    #[test]
    fn parse_error_produces_readable_message() {
        let bad_toml = "this is not valid toml {{{";
        let err = WorkspaceConfig::from_toml(bad_toml).unwrap_err();
        assert!(err.contains("failed to parse oxabl.toml"));
    }

    #[test]
    fn missing_workspace_section_errors() {
        let toml = r#"
[other]
name = "bad"
"#;
        let err = WorkspaceConfig::from_toml(toml).unwrap_err();
        assert!(err.contains("failed to parse oxabl.toml"));
    }

    #[test]
    fn missing_name_errors() {
        let toml = r#"
[workspace]
"#;
        let err = WorkspaceConfig::from_toml(toml).unwrap_err();
        assert!(err.contains("failed to parse oxabl.toml"));
    }
}
