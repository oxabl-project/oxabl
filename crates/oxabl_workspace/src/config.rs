use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use oxabl_common::{LintLayer, LintSeverityMap, Severity};
use oxabl_style::StyleGuide;
use schemars::JsonSchema;
use schemars::r#gen::SchemaGenerator;
use schemars::schema::{InstanceType, Metadata, Schema, SchemaObject};
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

    /// Lint rule groups, severities and per-directory overrides
    /// (`[workspace.lint]`).
    #[serde(default)]
    #[schemars(schema_with = "lint_config_schema")]
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
/// `off` disables the rule; the other levels map 1:1 to
/// [`oxabl_common::Severity`] and hence to LSP `DiagnosticSeverity`. `warning`
/// is accepted as another spelling of `warn`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum LintSeverity {
    Off,
    Hint,
    Info,
    #[serde(alias = "warning")]
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

impl JsonSchema for LintSeverity {
    fn schema_name() -> String {
        "LintSeverity".to_string()
    }

    // Hand-written so the `warning` alias is offered too; the derive lists
    // variant names only.
    fn json_schema(_: &mut SchemaGenerator) -> Schema {
        SchemaObject {
            metadata: Some(Box::new(Metadata {
                description: Some("Severity of a lint rule; `off` disables it.".to_string()),
                ..Default::default()
            })),
            instance_type: Some(InstanceType::String.into()),
            enum_values: Some(
                ["off", "hint", "info", "warn", "warning", "error"]
                    .into_iter()
                    .map(|v| serde_json::Value::String(v.to_string()))
                    .collect(),
            ),
            ..Default::default()
        }
        .into()
    }
}

/// The `[workspace.lint]` schema, with every key not otherwise declared typed
/// as a [`LintSeverity`].
///
/// The rule keys are a flattened map, and schemars 0.8 drops a flattened map's
/// `additionalProperties` when it merges it into the parent, so it is put back
/// on the two generated definitions here.
fn lint_config_schema(generator: &mut SchemaGenerator) -> Schema {
    let schema = generator.subschema_for::<LintConfig>();
    let severity = generator.subschema_for::<LintSeverity>();
    for name in ["LintConfig", "LintOverride"] {
        if let Some(Schema::Object(definition)) = generator.definitions_mut().get_mut(name) {
            definition.object().additional_properties = Some(Box::new(severity.clone()));
        }
    }
    schema
}

/// Rule and group settings shared by `[workspace.lint]` and each of its
/// `[[workspace.lint.overrides]]` entries.
///
/// Every key other than `enable`, `disable` (and `files` / `overrides`) names a
/// rule, by code (`LINT0002`, `CL001`) or kebab name (`unused-variable`),
/// case-insensitively. Names the rule registry does not know are reported as
/// config warnings by the pipeline, not rejected here.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(default, rename_all = "kebab-case")]
pub struct LintRuleSettings {
    /// Rule groups to turn on, for example `["strict.calls"]`. A dotted prefix
    /// selects every group under it: `"strict"` turns on every `strict.*` group.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub enable: Vec<String>,

    /// Rule groups to turn off. Applied after `enable` in the same table.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub disable: Vec<String>,

    /// Per-rule severity, keyed by code or kebab name. A severity turns the rule
    /// on even when its group is off; `off` turns it off.
    #[serde(flatten)]
    pub rules: BTreeMap<String, LintSeverity>,
}

impl LintRuleSettings {
    fn to_layer(&self) -> LintLayer {
        let mut layer = LintLayer::new();
        for group in &self.enable {
            layer.enable_group(group);
        }
        for group in &self.disable {
            layer.disable_group(group);
        }
        for (rule, severity) in &self.rules {
            layer.set_rule(rule, severity.to_severity());
        }
        layer
    }
}

/// One `[[workspace.lint.overrides]]` entry: settings for the files that match
/// `files`. An entry with no `files` applies to nothing.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(default, rename_all = "kebab-case")]
pub struct LintOverride {
    /// Glob patterns relative to the directory holding `oxabl.toml`, written
    /// with `/` and matched case-insensitively. `*` stays inside one directory,
    /// `**` crosses directories, and a pattern that matches a directory selects
    /// every file under it.
    pub files: Vec<String>,

    /// The settings for matching files.
    #[serde(flatten)]
    pub settings: LintRuleSettings,
}

/// The `[workspace.lint]` table: which rule groups run, per-rule severities,
/// and per-directory overrides.
///
/// Precedence, lowest first: each rule's default, then this table, then every
/// override whose `files` match, in file order — a later override wins over an
/// earlier one, so list broad patterns before narrow ones. Within one table a
/// rule's own setting beats a group toggle, and `disable` beats `enable`. A
/// group toggle turns rules on or off but keeps the severity an earlier table
/// chose.
///
/// A group toggle in an override replaces an explicit setting for a rule in
/// `[workspace.lint]` (an override is more specific than the global table),
/// but not one made by the same or an earlier override: overrides rank equal,
/// so a rule's own setting there stands against any later group toggle. A
/// later rule setting always wins. For example, with `CL001 = "off"` in
/// `[workspace.lint]`, an override that enables `strict.calls` turns `CL001`
/// on for its files; with `CL001 = "off"` in an override for `a/**`, a later
/// override for `**/*.p` that enables `strict.calls` leaves `CL001` off.
///
/// An override with no `files` patterns, or only invalid ones, applies to no
/// file and is reported as a warning.
///
/// The `default` group (the `LINT` rules) is always on: it cannot be toggled as
/// a group, but each of its rules can be set to `off`. The `strict.*` groups
/// are off until enabled.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
#[serde(default, rename_all = "kebab-case")]
pub struct LintConfig {
    /// Global settings.
    #[serde(flatten)]
    pub settings: LintRuleSettings,

    /// Per-directory or per-file settings, applied in order after the global
    /// ones.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub overrides: Vec<LintOverride>,
}

/// A problem in the `files` of a `[[workspace.lint.overrides]]` entry that makes
/// the entry, or one pattern of it, apply to nothing: a pattern that is not a
/// valid glob, or an entry with no pattern at all.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RejectedLintPattern {
    /// The pattern as written. Empty when the entry has no `files` patterns
    /// or the pattern is itself empty.
    pub pattern: String,
    /// Why it was rejected.
    pub reason: String,
}

impl LintConfig {
    /// Lower to the pipeline's [`LintSeverityMap`] with override patterns
    /// anchored at `root`, the directory holding `oxabl.toml`. Invalid patterns
    /// are dropped and returned. An entry with no `files` patterns applies to
    /// no file; it is dropped and returned with an empty `pattern`.
    pub fn to_severity_map_at(&self, root: &Path) -> (LintSeverityMap, Vec<RejectedLintPattern>) {
        let mut map = LintSeverityMap::new();
        *map.base_mut() = self.settings.to_layer();
        let mut rejected = Vec::new();
        for entry in &self.overrides {
            if entry.files.is_empty() {
                rejected.push(RejectedLintPattern {
                    pattern: String::new(),
                    reason: "the override has no `files` patterns, so it applies to no file"
                        .to_string(),
                });
            }
            rejected.extend(
                map.add_scope(root, &entry.files, entry.settings.to_layer())
                    .into_iter()
                    .map(|(pattern, reason)| RejectedLintPattern { pattern, reason }),
            );
        }
        (map, rejected)
    }

    /// [`to_severity_map_at`](Self::to_severity_map_at) with no root: override
    /// patterns match the path exactly as a caller passes it.
    pub fn to_severity_map(&self) -> LintSeverityMap {
        self.to_severity_map_at(Path::new("")).0
    }

    /// Set one rule by code or kebab name in the global settings.
    pub fn set_rule(&mut self, rule: &str, severity: LintSeverity) {
        self.settings.rules.insert(rule.to_string(), severity);
    }

    /// Every rule selector written anywhere in the table, in file order.
    pub fn rule_selectors(&self) -> impl Iterator<Item = &str> {
        std::iter::once(&self.settings)
            .chain(self.overrides.iter().map(|o| &o.settings))
            .flat_map(|s| s.rules.keys().map(String::as_str))
    }

    /// Every group selector written anywhere in the table (`enable` and
    /// `disable`), in file order.
    pub fn group_selectors(&self) -> impl Iterator<Item = &str> {
        std::iter::once(&self.settings)
            .chain(self.overrides.iter().map(|o| &o.settings))
            .flat_map(|s| s.enable.iter().chain(&s.disable).map(String::as_str))
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

    /// Search paths resolved against the directory of each root file and
    /// searched before `include_paths`, in order. Default empty.
    ///
    /// For a shop that compiles each program with its own directory, or fixed
    /// subdirectories of it, first on the PROPATH: `[".", "rf"]` searches
    /// `<dir>` and `<dir>/rf` for a root in `<dir>`. Includes, classes and `RUN`
    /// targets are all searched this way; an indexed class or program reads its
    /// own includes with the paths of its own directory.
    #[serde(default)]
    pub root_relative_include_paths: Vec<PathBuf>,

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

    /// When a class or `RUN` target is not found under the spelling the source
    /// used or its lower-case fold, look for a file whose path matches ignoring
    /// case. Default `false`.
    ///
    /// Meant for a case-sensitive filesystem holding a tree written for a
    /// case-insensitive one (`RUN Sub/Prog.p` stored as `sub/prog.p`). It lists
    /// directories on every miss, so it costs more than the default lookup. Two
    /// distinct matches still leave the name unresolved.
    #[serde(default)]
    pub case_insensitive_lookup: bool,
    /// Extensions that make a file an ABL *root* when a directory is walked,
    /// written without the dot and matched case-insensitively. Default
    /// `["p", "w", "cls", "v"]`.
    ///
    /// Use it for shop conventions such as `.pp` or `.cgi`. It decides which files
    /// `oxabl check` and the other directory walks pick up; a file named on the
    /// command line is always accepted. A leading dot is ignored. An entry that is
    /// empty, holds a path separator, or is `i` is dropped with a warning: an
    /// include fragment is never a root. The list replaces the default, so keep
    /// the defaults you still need.
    #[serde(default = "default_root_extensions")]
    pub root_extensions: Vec<String>,
}

/// The extensions that make a walked file an ABL root.
pub fn default_root_extensions() -> Vec<String> {
    crate::ROOT_EXTENSIONS
        .iter()
        .map(|ext| (*ext).to_string())
        .collect()
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

/// Normalize a `root_extensions` list: each entry through
/// [`normalize_run_extension`], repeats (ignoring case) dropped, and `i` rejected
/// because an include fragment is never a root.
pub fn normalize_root_extensions(entries: &[String]) -> RunExtensions {
    let mut extensions: Vec<String> = Vec::new();
    let mut rejected = Vec::new();
    for entry in entries {
        match normalize_run_extension(entry) {
            Some(extension) if !extension.eq_ignore_ascii_case("i") => {
                if !extensions
                    .iter()
                    .any(|seen| seen.eq_ignore_ascii_case(extension))
                {
                    extensions.push(extension.to_string());
                }
            }
            _ => rejected.push(entry.clone()),
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
            root_relative_include_paths: Vec::new(),
            run_extensions: default_run_extensions(),
            case_insensitive_lookup: false,
            root_extensions: default_root_extensions(),
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
    use oxabl_common::LintRuleRef;

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
    fn case_insensitive_lookup_defaults_off_and_can_be_enabled() {
        let config = WorkspaceConfig::from_toml("[workspace]\nname = \"p\"\n").unwrap();
        assert!(!config.workspace.sources.case_insensitive_lookup);
        let toml =
            "[workspace]\nname = \"p\"\n[workspace.sources]\ncase_insensitive_lookup = true\n";
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert!(config.workspace.sources.case_insensitive_lookup);
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

    fn lint_of(toml: &str) -> LintConfig {
        WorkspaceConfig::from_toml(&format!("[workspace]\nname = \"p\"\n{toml}"))
            .unwrap()
            .workspace
            .lint
    }

    fn rule<'a>(code: &'a str, name: &'a str, group: Option<&'a str>) -> LintRuleRef<'a> {
        LintRuleRef {
            code,
            name,
            group,
            enabled_by_default: group.is_none(),
            default_severity: Severity::Warning,
        }
    }

    #[test]
    fn lint_table_defaults_to_nothing_configured() {
        let lint = lint_of("");
        assert_eq!(lint, LintConfig::default());
        assert!(lint.to_severity_map().is_empty());
    }

    #[test]
    fn lint_table_keeps_the_original_kebab_keys() {
        let lint = lint_of(
            "[workspace.lint]\nundefined-symbol = \"warn\"\nunused-variable = \"off\"\n\
             block-var-used-outside = \"warning\"\n",
        );
        let map = lint.to_severity_map();
        assert_eq!(map.get("undefined-symbol"), Some(Some(Severity::Warning)));
        assert_eq!(map.get("unused-variable"), Some(None));
        assert_eq!(
            map.get("block-var-used-outside"),
            Some(Some(Severity::Warning)),
            "`warning` spells `warn`"
        );
        assert_eq!(
            map.severity_for(&rule("LINT0002", "unused-variable", None)),
            None
        );
    }

    #[test]
    fn lint_table_takes_codes_and_group_lists() {
        let lint = lint_of(
            "[workspace.lint]\nenable = [\"strict.calls\"]\ndisable = [\"strict.nulls\"]\n\
             CL005 = \"error\"\n",
        );
        assert_eq!(lint.settings.enable, ["strict.calls"]);
        assert_eq!(lint.settings.disable, ["strict.nulls"]);
        let map = lint.to_severity_map();
        assert_eq!(
            map.severity_for(&rule("CL001", "run-value", Some("strict.calls"))),
            Some(Severity::Warning)
        );
        assert_eq!(
            map.severity_for(&rule("CL005", "shared-state", Some("strict.other"))),
            Some(Severity::Error)
        );
        assert_eq!(
            map.severity_for(&rule("NL001", "x", Some("strict.nulls"))),
            None
        );
    }

    #[test]
    fn an_unknown_rule_name_parses_and_is_left_to_the_pipeline_to_report() {
        let lint = lint_of("[workspace.lint]\nfuture-rule = \"off\"\n");
        assert_eq!(lint.rule_selectors().collect::<Vec<_>>(), ["future-rule"]);
    }

    #[test]
    fn a_rule_key_must_hold_a_severity() {
        let toml = "[workspace]\nname = \"p\"\n[workspace.lint]\nunused-variable = \"loud\"\n";
        assert!(WorkspaceConfig::from_toml(toml).is_err());
        let toml = "[workspace]\nname = \"p\"\n[workspace.lint]\nenable = \"strict\"\n";
        assert!(
            WorkspaceConfig::from_toml(toml).is_err(),
            "enable is a list"
        );
    }

    #[test]
    fn lint_overrides_apply_per_directory_in_file_order() {
        let lint = lint_of(
            r#"
[workspace.lint]
enable = ["strict"]
unused-variable = "info"

[[workspace.lint.overrides]]
files = ["legacy/**", "old.p"]
disable = ["strict"]
LINT0002 = "off"

[[workspace.lint.overrides]]
files = ["legacy/kept"]
enable = ["strict.calls"]
"#,
        );
        assert_eq!(lint.overrides.len(), 2);
        assert_eq!(lint.overrides[0].files, ["legacy/**", "old.p"]);
        assert_eq!(
            lint.group_selectors().collect::<Vec<_>>(),
            ["strict", "strict", "strict.calls"]
        );

        let (map, rejected) = lint.to_severity_map_at(Path::new("/ws"));
        assert!(rejected.is_empty());
        let unused = rule("LINT0002", "unused-variable", None);
        let calls = rule("CL001", "run-value", Some("strict.calls"));

        let src = map.for_path(Path::new("/ws/src/a.p"));
        assert_eq!(src.severity_for(&unused), Some(Severity::Info));
        assert_eq!(src.severity_for(&calls), Some(Severity::Warning));

        for file in ["/ws/legacy/a.p", "/ws/old.p"] {
            let legacy = map.for_path(Path::new(file));
            assert_eq!(legacy.severity_for(&unused), None, "{file}");
            assert_eq!(legacy.severity_for(&calls), None, "{file}");
        }

        let kept = map.for_path(Path::new("/ws/legacy/kept/a.p"));
        assert_eq!(kept.severity_for(&calls), Some(Severity::Warning));
        assert_eq!(kept.severity_for(&unused), None);
    }

    #[test]
    fn an_invalid_override_pattern_is_returned() {
        let lint = lint_of(
            "[[workspace.lint.overrides]]\nfiles = [\"a/[\", \"b/**\"]\nLINT0002 = \"off\"\n",
        );
        let (_, rejected) = lint.to_severity_map_at(Path::new("/ws"));
        assert_eq!(rejected.len(), 1);
        assert_eq!(rejected[0].pattern, "a/[");
    }

    #[test]
    fn an_override_with_no_files_is_returned() {
        for toml in [
            "[[workspace.lint.overrides]]\nLINT0002 = \"off\"\n",
            "[[workspace.lint.overrides]]\nfiles = []\nLINT0002 = \"off\"\n",
        ] {
            let (map, rejected) = lint_of(toml).to_severity_map_at(Path::new("/ws"));
            assert_eq!(rejected.len(), 1, "{toml}");
            assert!(rejected[0].pattern.is_empty());
            assert!(rejected[0].reason.contains("no `files`"), "{rejected:?}");
            let unused = rule("LINT0002", "unused-variable", None);
            let resolved = map.for_path(Path::new("/ws/a.p"));
            assert_eq!(resolved.severity_for(&unused), Some(Severity::Warning));
        }
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

    #[test]
    fn root_extensions_default_and_override() {
        let config = WorkspaceConfig::from_toml("[workspace]\nname = \"p\"\n").unwrap();
        assert_eq!(
            config.workspace.sources.root_extensions,
            ["p", "w", "cls", "v"]
        );

        let toml =
            "[workspace]\nname = \"p\"\n[workspace.sources]\nroot_extensions = [\"p\", \".pp\"]\n";
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert_eq!(config.workspace.sources.root_extensions, ["p", ".pp"]);
    }

    #[test]
    fn root_extensions_are_normalized_and_include_is_rejected() {
        let list: Vec<String> = [".pp", "PP", "i", ".I", "", "a/b"]
            .iter()
            .map(|e| e.to_string())
            .collect();
        let normalized = normalize_root_extensions(&list);
        assert_eq!(normalized.extensions, ["pp"]);
        assert_eq!(normalized.rejected, ["i", ".I", "", "a/b"]);
    }

    #[test]
    fn root_relative_include_paths_default_empty_and_parse_as_a_list() {
        let config = WorkspaceConfig::from_toml("[workspace]\nname = \"p\"\n").unwrap();
        assert!(
            config
                .workspace
                .sources
                .root_relative_include_paths
                .is_empty()
        );
        let toml = "[workspace]\nname = \"p\"\n[workspace.sources]\nroot_relative_include_paths = [\".\", \"rf\"]\n";
        let config = WorkspaceConfig::from_toml(toml).unwrap();
        assert_eq!(
            config.workspace.sources.root_relative_include_paths,
            [PathBuf::from("."), PathBuf::from("rf")]
        );
    }
}
