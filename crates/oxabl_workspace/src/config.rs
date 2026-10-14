use std::collections::BTreeMap;
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

    /// The compile environment the preprocessor reports through its predefined
    /// names (`[workspace.preprocessor]`).
    #[serde(default)]
    pub preprocessor: PreprocessorConfig,
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
    pub assigned_but_never_read: LintSeverity,
}

impl Default for LintConfig {
    fn default() -> Self {
        LintConfig {
            undefined_symbol: LintSeverity::Error,
            unused_variable: LintSeverity::Warn,
            unknown_table_or_field: LintSeverity::Warn,
            type_mismatch_assignment: LintSeverity::Warn,
            block_var_used_outside: LintSeverity::Info,
            assigned_but_never_read: LintSeverity::Warn,
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
        map.set("LINT0006", self.assigned_but_never_read.to_severity());
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
            "assigned-but-never-read" => self.assigned_but_never_read = severity,
            _ => return false,
        }
        true
    }
}

/// The `[workspace.preprocessor]` table: the compile environment behind the
/// AVM's predefined preprocessor names.
///
/// The AVM answers `{&OPSYS}`, `{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`,
/// `{&PROCESS-ARCHITECTURE}` and `{&PROVERSION}` from the machine it compiles
/// on. A static analyzer has no such machine, so the environment is stated here
/// and every `&IF` over one of these names is decided against it. The defaults
/// describe a 64-bit Unix batch compile with a character (TTY) UI; set the keys
/// to describe the environment your code is really compiled in.
///
/// `{&FILE-NAME}`, `{&LINE-NUMBER}` and `{&SEQUENCE}` are computed from the
/// source and take no configuration.
///
/// The `DBTYPE`, `CONNECTED` and `LDBNAME` functions of an `&IF` ask which
/// databases are connected, which only a compile knows. `databases` states them;
/// without it the `[workspace.schema]` file names stand in, as a partial list.
///
/// Container-level `#[serde(default)]` lets a partial table fall back per key,
/// and `deny_unknown_fields` makes a misspelled key a hard error rather than a
/// silently wrong branch.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(default, deny_unknown_fields, rename_all = "kebab-case")]
pub struct PreprocessorConfig {
    /// Value of `{&OPSYS}`: `UNIX` or `WIN32`. Default `UNIX`.
    pub opsys: String,

    /// Value of `{&WINDOW-SYSTEM}`: `TTY` or `MS-WINDOWS`. Default `TTY`.
    pub window_system: String,

    /// Whether the compile is a batch compile; `{&BATCH-MODE}` expands to `yes`
    /// or `no`. Default `true`.
    pub batch_mode: bool,

    /// Value of `{&PROCESS-ARCHITECTURE}`: `32` or `64`. Default `64`.
    pub process_architecture: u32,

    /// Value of `{&PROVERSION}` and of the `PROVERSION` function, for example
    /// `"12.8"`. There is no default: the version is not something a tool can
    /// guess. While it is unset, an `&IF` condition that reads either form
    /// raises `PREPROC009` instead of silently deciding against an empty string.
    pub proversion: Option<String>,

    /// The logical names of the databases connected when the code compiles, in
    /// the order of connection. The `DBTYPE`, `CONNECTED` and `LDBNAME` functions
    /// of an `&IF` answer from this list, so `DBTYPE("ledger") NE ?` is true when
    /// `ledger` is listed. The list is complete: a name that is not in it is not
    /// connected, so `DBTYPE` answers `?` and `CONNECTED` answers false. Use `[]`
    /// for a compile with no database connected. `DICTDB`, the alias the AVM gives
    /// the first connected database, answers as the first name in the list.
    ///
    /// When the key is absent, the `[workspace.schema]` files name the databases:
    /// the file name without `.df` is the logical name, so `schema/ledger.df`
    /// connects `ledger`. A file name can differ from the logical name, and the
    /// files are in the order of `[workspace.schema]`, not of connection. So this
    /// list is not complete and not ordered: a name that is not in it raises
    /// `PREPROC010` instead of deciding that the database is absent, and so do
    /// `LDBNAME("DICTDB")` and `DBTYPE("DICTDB")`. `CONNECTED("DICTDB")` is true
    /// when the list is not empty. Set the key when a logical name
    /// differs from its file name. While neither a list nor a schema is given, an
    /// `&IF` that calls one of these functions raises `PREPROC010` instead of
    /// deciding on a guess.
    pub databases: Option<Vec<String>>,

    /// The logical names that the `[workspace.schema]` files stand for, which the
    /// database functions use when `databases` is absent. Set when the workspace
    /// is resolved, never from `oxabl.toml`.
    #[serde(skip)]
    #[schemars(skip)]
    pub inferred_databases: Option<Vec<String>>,

    /// The `DBTYPE` of a connected database, by logical name, for example
    /// `{ ledger = "ORACLE" }`. A database that is not named here is `PROGRESS`.
    pub database_types: BTreeMap<String, String>,
}

impl Default for PreprocessorConfig {
    fn default() -> Self {
        PreprocessorConfig {
            opsys: "UNIX".to_string(),
            window_system: "TTY".to_string(),
            batch_mode: true,
            process_architecture: 64,
            proversion: None,
            databases: None,
            inferred_databases: None,
            database_types: BTreeMap::new(),
        }
    }
}

/// The `[workspace.sources]` section.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct SourcesConfig {
    /// Directories to walk for ABL *root* files — `.p`, `.w`, `.cls`, and `.v`,
    /// matched case-insensitively. `.i` is deliberately not among them: an
    /// include fragment is never a root, and `is_root_file` is the one policy
    /// that decides.
    #[serde(default)]
    pub directories: Vec<PathBuf>,

    /// Include search paths, searched in order (PROPATH equivalent).
    #[serde(default)]
    pub include_paths: Vec<PathBuf>,

    /// Extensions tried, in order, for a `RUN` target written without one
    /// (`RUN post-order`), written without the dot. Default `["p", "w"]`.
    ///
    /// A leading dot is ignored, and an entry that is empty or holds a path
    /// separator is dropped with a warning. Each extension is tried as written and
    /// then in lower case, against the name as written and then folded.
    ///
    /// The first extension that finds a file wins. A list that omits an
    /// extension never links a bare name to such a file; an empty list links no
    /// bare name at all.
    #[serde(default = "default_run_extensions")]
    pub run_extensions: Vec<String>,
}

/// The extensions an extensionless `RUN` target is tried with.
pub fn default_run_extensions() -> Vec<String> {
    vec!["p".to_string(), "w".to_string()]
}

/// The usable entries of a `run_extensions` list, and the ones that were not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunExtensions {
    /// Entries in configured order, without a leading dot, each once.
    pub extensions: Vec<String>,
    /// Entries dropped because no file name can end in them: empty, or holding a
    /// path separator. As written.
    pub rejected: Vec<String>,
}

/// One `run_extensions` entry as a file extension, or `None` when no file name
/// can end in it.
///
/// Surrounding whitespace and a leading dot are stripped (` .p ` means `p`).
/// Case is kept: the lookup tries the configured spelling and the lower-case one.
pub fn normalize_run_extension(entry: &str) -> Option<&str> {
    let extension = entry.trim().trim_start_matches('.');
    (!extension.is_empty() && !extension.contains(['/', '\\'])).then_some(extension)
}

/// Normalize a `run_extensions` list as the search needs it: each entry through
/// [`normalize_run_extension`], repeats dropped.
pub fn normalize_run_extensions(entries: &[String]) -> RunExtensions {
    let mut extensions: Vec<String> = Vec::new();
    let mut rejected = Vec::new();
    for entry in entries {
        match normalize_run_extension(entry) {
            None => rejected.push(entry.clone()),
            Some(extension) if !extensions.iter().any(|seen| seen == extension) => {
                extensions.push(extension.to_string());
            }
            Some(_) => {}
        }
    }
    RunExtensions {
        extensions,
        rejected,
    }
}

impl Default for SourcesConfig {
    fn default() -> Self {
        SourcesConfig {
            directories: Vec::new(),
            include_paths: Vec::new(),
            run_extensions: default_run_extensions(),
        }
    }
}

/// The `[workspace.schema]` section.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
pub struct SchemaConfig {
    /// Paths to `.df` schema dump files.
    #[serde(default)]
    pub files: Vec<PathBuf>,
}

impl WorkspaceConfig {
    /// The configuration of a project with no `oxabl.toml` — every section at its
    /// documented default, and an empty name.
    ///
    /// Exists so a caller that resolves configuration from a parsed value can
    /// take the same derivation path whether or not a config file was found: the
    /// "no config" and "malformed config" cases become *this value* rather than a
    /// second, separately-maintained set of fallbacks. `name` is required by the
    /// deserializer (a real config must name its project) and is empty here
    /// because there is no project file to name one.
    pub fn defaults() -> Self {
        WorkspaceConfig {
            workspace: WorkspaceSection {
                name: String::new(),
                sources: SourcesConfig::default(),
                schema: SchemaConfig::default(),
                lint: LintConfig::default(),
                style: StyleGuide::default_base(),
                preprocessor: PreprocessorConfig::default(),
            },
        }
    }

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
    fn preprocessor_table_defaults_to_a_unix_tty_batch_compile() {
        let config = WorkspaceConfig::from_toml("[workspace]\nname = \"p\"\n").unwrap();
        let pp = &config.workspace.preprocessor;
        assert_eq!(pp.opsys, "UNIX");
        assert_eq!(pp.window_system, "TTY");
        assert!(pp.batch_mode);
        assert_eq!(pp.process_architecture, 64);
        assert_eq!(pp.proversion, None);
        assert_eq!(pp.databases, None);
        assert_eq!(pp.inferred_databases, None);
        assert!(pp.database_types.is_empty());
    }

    /// The names a schema file stands for are set by the resolver, so a key for
    /// them is a misspelling like any other.
    #[test]
    fn the_inferred_databases_are_not_a_config_key() {
        for key in ["inferred-databases", "inferred_databases"] {
            let toml =
                format!("[workspace]\nname = \"p\"\n[workspace.preprocessor]\n{key} = [\"a\"]\n");
            assert!(WorkspaceConfig::from_toml(&toml).is_err(), "{key}");
        }
    }

    #[test]
    fn preprocessor_table_lists_the_connected_databases() {
        let toml = r#"
[workspace]
name = "p"

[workspace.preprocessor]
databases = ["ledger", "stock"]
database-types = { stock = "ORACLE" }
"#;
        let pp = WorkspaceConfig::from_toml(toml)
            .unwrap()
            .workspace
            .preprocessor;
        assert_eq!(
            pp.databases.as_deref(),
            Some(&["ledger".into(), "stock".into()][..])
        );
        assert_eq!(pp.database_types["stock"], "ORACLE");

        let toml = "[workspace]\nname = \"p\"\n[workspace.preprocessor]\ndatabases = []\n";
        let pp = WorkspaceConfig::from_toml(toml)
            .unwrap()
            .workspace
            .preprocessor;
        assert_eq!(pp.databases.as_deref(), Some(&[][..]));
    }

    #[test]
    fn preprocessor_table_overrides_per_key() {
        let toml = r#"
[workspace]
name = "p"

[workspace.preprocessor]
opsys = "WIN32"
batch-mode = false
proversion = "12.8"
"#;
        let pp = WorkspaceConfig::from_toml(toml)
            .unwrap()
            .workspace
            .preprocessor;
        assert_eq!(pp.opsys, "WIN32");
        assert_eq!(pp.window_system, "TTY");
        assert!(!pp.batch_mode);
        assert_eq!(pp.proversion.as_deref(), Some("12.8"));
    }

    #[test]
    fn preprocessor_table_rejects_unknown_keys() {
        let toml = "[workspace]\nname = \"p\"\n[workspace.preprocessor]\nopsis = \"UNIX\"\n";
        assert!(WorkspaceConfig::from_toml(toml).is_err());
    }

    #[test]
    fn run_extensions_default_to_p_then_w_and_override_as_a_list() {
        let config = WorkspaceConfig::from_toml("[workspace]\nname = \"p\"\n").unwrap();
        assert_eq!(config.workspace.sources.run_extensions, ["p", "w"]);

        let toml = "[workspace]\nname = \"p\"\n[workspace.sources]\nrun_extensions = [\"w\", \"p\", \"pp\"]\n";
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert_eq!(config.workspace.sources.run_extensions, ["w", "p", "pp"]);
    }

    #[test]
    fn run_extensions_lose_a_leading_dot_and_repeats() {
        let entries = |list: &[&str]| list.iter().map(|e| e.to_string()).collect::<Vec<_>>();
        let normalized = normalize_run_extensions(&entries(&[".p", "w", "p", " .W "]));
        assert_eq!(normalized.extensions, ["p", "w", "W"]);
        assert!(normalized.rejected.is_empty());
    }

    #[test]
    fn run_extensions_reject_what_no_file_name_ends_in() {
        let entries = |list: &[&str]| list.iter().map(|e| e.to_string()).collect::<Vec<_>>();
        let normalized = normalize_run_extensions(&entries(&["", ".", "a/b", "a\\b", "p"]));
        assert_eq!(normalized.extensions, ["p"]);
        assert_eq!(normalized.rejected, ["", ".", "a/b", "a\\b"]);
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
