use std::path::{Path, PathBuf};

use oxabl_common::{LintSeverityMap, Severity};
use oxabl_style::StyleGuide;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Top-level configuration deserialized from `oxabl.toml`.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct WorkspaceConfig {
    pub workspace: WorkspaceSection,
}

/// The `[workspace]` section of `oxabl.toml`.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct WorkspaceSection {
    /// Human-readable project name.
    pub name: String,

    /// Source file locations and include paths.
    #[serde(default)]
    pub sources: SourcesConfig,

    /// Optional schema (`.df`) file declarations.
    #[serde(default)]
    pub schema: SchemaConfig,

    /// Per-rule lint severity surface (`[workspace.lint]`).
    #[serde(default)]
    pub lint: LintConfig,

    /// Formatter style rules (`[workspace.style]`).
    ///
    /// Embeds [`StyleGuide`] directly (no wrapper type): it already derives
    /// `Deserialize` with `#[serde(default, deny_unknown_fields)]`, so a partial
    /// table fills unspecified fields from [`StyleGuide::default_base`], an
    /// absent section yields `default_base()`, and an unknown key is a hard
    /// parse error.
    #[serde(default)]
    pub style: StyleGuide,
}

/// A user-facing severity level for a lint rule (`[workspace.lint]`).
///
/// Five levels: `off` disables the rule; the other four map 1:1 to
/// [`oxabl_common::Severity`] and hence to LSP `DiagnosticSeverity`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum LintSeverity {
    Off,
    Hint,
    Info,
    Warn,
    Error,
}

impl LintSeverity {
    /// Lower to the application form: `None` for `off`, else the mapped
    /// [`Severity`].
    pub fn to_severity(self) -> Option<Severity> {
        match self {
            LintSeverity::Off => None,
            LintSeverity::Hint => Some(Severity::Hint),
            LintSeverity::Info => Some(Severity::Info),
            LintSeverity::Warn => Some(Severity::Warning),
            LintSeverity::Error => Some(Severity::Error),
        }
    }
}

/// The `[workspace.lint]` table: one severity per v1 lint rule (kebab keys).
///
/// Mirrors [`oxabl_style::StyleGuide`]'s serde idiom — container-level
/// `#[serde(default)]` so partial tables fall back per-field, plus
/// `deny_unknown_fields` so a misspelled rule name is a hard error rather than
/// silently ignored. The safe default (no table) is all-on with
/// `undefined-symbol = error`, `block-var-used-outside = info`, and the rest
/// `warn` (R15).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct LintConfig {
    pub undefined_symbol: LintSeverity,
    pub unused_variable: LintSeverity,
    pub unknown_table_or_field: LintSeverity,
    pub type_mismatch_assignment: LintSeverity,
    pub block_var_used_outside: LintSeverity,
}

impl Default for LintConfig {
    fn default() -> Self {
        LintConfig {
            undefined_symbol: LintSeverity::Error,
            unused_variable: LintSeverity::Warn,
            unknown_table_or_field: LintSeverity::Warn,
            type_mismatch_assignment: LintSeverity::Warn,
            block_var_used_outside: LintSeverity::Info,
        }
    }
}

impl LintConfig {
    /// Lower to the pipeline's [`LintSeverityMap`] (code → optional severity),
    /// the leaf application form consumed by `oxabl_lint::lint_file` (KTD6).
    pub fn to_severity_map(&self) -> LintSeverityMap {
        let mut map = LintSeverityMap::new();
        map.set("LINT0001", self.undefined_symbol.to_severity());
        map.set("LINT0002", self.unused_variable.to_severity());
        map.set("LINT0003", self.unknown_table_or_field.to_severity());
        map.set("LINT0004", self.type_mismatch_assignment.to_severity());
        map.set("LINT0005", self.block_var_used_outside.to_severity());
        map
    }

    /// Override one rule by its kebab name (used to apply CLI overrides on top
    /// of the resolved table). Returns `false` for an unknown rule name.
    pub fn set_by_name(&mut self, rule: &str, severity: LintSeverity) -> bool {
        match rule {
            "undefined-symbol" => self.undefined_symbol = severity,
            "unused-variable" => self.unused_variable = severity,
            "unknown-table-or-field" => self.unknown_table_or_field = severity,
            "type-mismatch-assignment" => self.type_mismatch_assignment = severity,
            "block-var-used-outside" => self.block_var_used_outside = severity,
            _ => return false,
        }
        true
    }
}

/// The `[workspace.sources]` section.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
pub struct SourcesConfig {
    /// Directories containing ABL source files (`.p`, `.w`, `.cls`, `.i`).
    #[serde(default)]
    pub directories: Vec<PathBuf>,

    /// Include search paths, searched in order (PROPATH equivalent).
    #[serde(default)]
    pub include_paths: Vec<PathBuf>,
}

/// The `[workspace.schema]` section.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
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
