//! One resolution of `oxabl.toml` into everything a pipeline run needs (R6),
//! with non-fatal problems handed back as data rather than printed or dropped
//! (R7).
//!
//! # Why this replaces three helpers (KTD3)
//!
//! `oxabl_workspace` grew one `resolved_*` helper per configuration surface —
//! `resolved_include_paths`, `resolved_lint_config`, `resolved_style` — and each
//! one independently absolutizes its anchor, walks up for `oxabl.toml`, and
//! parses the file again. A client that wants two surfaces parses twice; today
//! `analyze --preprocess` does exactly that. Worse, each helper reports a
//! malformed config through its own `Option<String>`, so the *same* parse error
//! is surfaced once per helper the client happens to call — and the LSP, which
//! calls them and ignores the error slot, surfaces it zero times while the CLI
//! prints it as `warning:`.
//!
//! [`PipelineConfig::resolve`] finds the root once, reads the file once, and
//! derives every surface from that single parsed value. Whether a warning is
//! printed to stderr, published as an LSP diagnostic, or logged to the browser
//! console then becomes a client's decision, not an accident of which helper it
//! reached for.
//!
//! # The parse/derive split
//!
//! [`PipelineConfig::resolve`] does the I/O — root discovery, file read, TOML
//! parse — and then delegates every derivation to [`resolve_from_config`], which
//! takes an already-parsed [`WorkspaceConfig`]. Re-parsing is therefore
//! impossible *by construction* rather than by discipline, and the derivation
//! rules are testable without a config file at all.
//!
//! The split also sidesteps a dependency this unit has no business widening:
//! `find_workspace_root` and `WorkspaceConfig::from_path` are bound to the real
//! filesystem and are called by both the CLI and the LSP, so threading a
//! `FileSystem` through them is a change with its own blast radius. Keeping the
//! I/O in the thin outer layer leaves that for another day.

use std::borrow::Cow;
use std::path::{Path, PathBuf};

use oxabl_common::{Diagnostic, LintSeverityMap};
use oxabl_schema::{Schema, SchemaLoader};
use oxabl_style::StyleGuide;
use oxabl_workspace::{
    EncodedFileSystem, LintConfig, PreprocessorConfig, SourceDecoding, WorkspaceConfig,
    default_root_extensions, default_run_extensions, find_workspace_root,
    normalize_root_extensions, normalize_run_extensions,
};

/// A non-fatal problem hit while resolving configuration.
///
/// Returned as data (R7) so each client decides how — and whether — to surface
/// it. None of these stops a run: every one degrades to a documented default.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum ConfigWarning {
    /// An `oxabl.toml` was found but could not be read or parsed (including an
    /// unknown key in `[workspace.style]` or `[workspace.preprocessor]`, which
    /// use `deny_unknown_fields`, or a `[workspace.lint]` value that is not a
    /// severity). Every configured surface degrades to its
    /// default; caller overrides still apply.
    ///
    /// Exactly one of these is produced per resolution, however many surfaces
    /// the bad file would have fed.
    Config(String),

    /// A `.df` schema file produced a load diagnostic. The schema is still used:
    /// a partially-loaded schema drives resolution better than none.
    Schema(Diagnostic),

    /// A schema *directory* yielded nothing to load — it holds no `.df` file, or
    /// it could not be read at all.
    ///
    /// Unlike [`Schema`](Self::Schema) this one comes with `schema_loaded =
    /// false`, because there is no schema: the overwhelmingly likely cause is a
    /// typo in `--schema`, and treating the miss as an empty-but-present schema
    /// makes every table reference in the tree an `undefined-symbol` error and
    /// every field an `unknown-table-or-field` — a flood of findings about
    /// correct code with nothing naming the real cause.
    ///
    /// An explicitly *named* `.df` file that happens to declare no tables is a
    /// different thing and stays loaded: that is stated user intent, and it is the
    /// distinction `schema_loaded` exists to keep.
    SchemaPath(String),

    /// A `[workspace.sources].run_extensions` entry that no file name can end in
    /// (empty, or holding a path separator). The entry is dropped; without this
    /// warning a bare `RUN` name would simply stop linking.
    RunExtension(String),

    /// A `[workspace.sources].root_extensions` entry that cannot name a root
    /// (empty, holding a path separator, or `i`: an include fragment is never a
    /// root). The entry is dropped.
    RootExtension(String),

    /// A `[workspace.lint]` entry that cannot take effect: a rule or group name
    /// the rule registry does not know, a toggle of the always-on `default`
    /// group, or an override pattern that is not a valid glob. The entry is
    /// ignored and the rest of the table applies.
    Lint(String),
}

impl std::fmt::Display for ConfigWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigWarning::Config(msg) => write!(f, "{msg}"),
            ConfigWarning::Schema(d) => write!(f, "schema: [{}] {}", d.code.0, d.message),
            ConfigWarning::SchemaPath(msg) => write!(f, "schema: {msg}"),
            ConfigWarning::RunExtension(entry) => write!(
                f,
                "run_extensions: ignoring `{entry}`, which cannot be a file extension"
            ),
            ConfigWarning::RootExtension(entry) => write!(
                f,
                "root_extensions: ignoring `{entry}`, which cannot be a root file extension"
            ),
            ConfigWarning::Lint(msg) => write!(f, "lint: {msg}"),
        }
    }
}

/// Caller-supplied values that outrank `oxabl.toml`.
///
/// These are exactly what the CLI flags supply today — `-I`, `--schema`,
/// `--style` — modelled as a struct rather than a positional list so a new flag
/// is an added field, not a signature break. This is the shape the umbrella's
/// `AnalyzeOptions` already uses for the same reason.
#[derive(Debug, Clone, Default)]
pub struct ConfigOverrides {
    /// Include search directories from `-I` flags, in the order given. These
    /// precede the config's own paths (first-match-wins PROPATH), so an explicit
    /// flag shadows a config entry for the same include name.
    pub include_paths: Vec<PathBuf>,

    /// A `--schema` path: a single `.df` file, or a directory whose `.df` files
    /// are all loaded. When present it replaces `[workspace.schema].files`
    /// outright rather than adding to it.
    pub schema_path: Option<PathBuf>,

    /// A `--style` selection. `--style` names a *whole* guide (a preset or a
    /// TOML file, resolved by the caller), so when present it wins outright and
    /// does not merge with `[workspace.style]`.
    pub style: Option<StyleGuide>,
}

/// Everything a pipeline run needs, resolved once.
///
/// Construct with [`PipelineConfig::resolve`]; the fields are public so a client
/// with an unusual source of truth (a test, the browser, which has no
/// filesystem to discover a config on) can build one directly.
#[derive(Debug, Clone)]
pub struct PipelineConfig {
    /// Absolute include search directories, first-match-wins, duplicate-free.
    pub include_paths: Vec<PathBuf>,

    /// Search directories resolved against each root file's directory and
    /// searched before [`include_paths`](Self::include_paths)
    /// (`[workspace.sources].root_relative_include_paths`), as written. Default
    /// empty. See [`include_paths_for`](Self::include_paths_for).
    pub root_relative_include_paths: Vec<PathBuf>,

    /// Lint settings lowered from `[workspace.lint]`: rule severities, group
    /// toggles and per-directory overrides anchored at the workspace root.
    /// [`LintPipeline`](crate::LintPipeline) resolves them per file.
    pub lint_severities: LintSeverityMap,

    /// The resolved formatter style.
    pub style: StyleGuide,

    /// The schema driving schema-backed resolution.
    pub schema: Schema,

    /// Whether a schema was actually loaded.
    ///
    /// Kept **explicit** rather than derived from [`Schema::is_empty`] so a
    /// syntactically valid but empty `.df` still reads as loaded to
    /// schema-dependent diagnostics: "the user gave us a schema and it declares
    /// no tables" is a different fact from "the user gave us no schema", and only
    /// the second should keep `unknown-table-or-field` silent.
    pub schema_loaded: bool,

    /// The compile environment the preprocessor answers its predefined names
    /// from (`[workspace.preprocessor]`).
    ///
    /// Every client that builds a config without an `oxabl.toml` gets
    /// [`PreprocessorConfig::default`], so an `&IF` over `{&OPSYS}` takes the
    /// same branch in the browser as in the CLI.
    pub preprocessor: PreprocessorConfig,

    /// Extensions an extensionless `RUN` target is tried with, in order and
    /// without the dot (`[workspace.sources].run_extensions`). Default `p`, `w`.
    pub run_extensions: Vec<String>,

    /// Resolve classes and `RUN` targets ignoring case when the exact spellings
    /// miss (`[workspace.sources].case_insensitive_lookup`). Default `false`.
    pub case_insensitive_lookup: bool,

    /// How source files read from disk become text
    /// (`[workspace.sources].encoding` and `fallback_encoding`). Default UTF-8.
    /// [`file_system`](Self::file_system) builds the filesystem that honours it.
    pub decoding: SourceDecoding,

    /// Extensions that make a file in a walked directory an ABL root, without the
    /// dot (`[workspace.sources].root_extensions`). Default `p`, `w`, `cls`, `v`.
    pub root_extensions: Vec<String>,
}

impl Default for PipelineConfig {
    /// The configuration of a project with no `oxabl.toml`: no include paths, the
    /// default lint settings, the safe non-mangling base style, and no schema.
    ///
    /// # One table of default severities (R19)
    ///
    /// Every client must give the same answer for the same input, so default
    /// severities live in one place: each rule's registry entry in `oxabl_lint`.
    /// Both this value and [`resolve`](PipelineConfig::resolve) with no config
    /// file lower [`LintConfig::default`], which configures nothing, so both
    /// report each rule at its registry default.
    fn default() -> Self {
        PipelineConfig {
            include_paths: Vec::new(),
            root_relative_include_paths: Vec::new(),
            lint_severities: LintConfig::default().to_severity_map(),
            style: StyleGuide::default_base(),
            schema: Schema::empty(),
            schema_loaded: false,
            preprocessor: PreprocessorConfig::default(),
            run_extensions: default_run_extensions(),
            case_insensitive_lookup: false,
            decoding: SourceDecoding::default(),
            root_extensions: default_root_extensions(),
        }
    }
}

impl PipelineConfig {
    /// The disk filesystem for this configuration: every file it reads is decoded
    /// as [`decoding`](Self::decoding).
    pub fn file_system(&self) -> EncodedFileSystem {
        EncodedFileSystem::new(self.decoding)
    }

    /// The search paths of the file at `file`: each root-relative path resolved
    /// against its directory, then [`include_paths`](Self::include_paths), each
    /// once. The include paths alone with no file or no root-relative path.
    pub fn include_paths_for(&self, file: Option<&Path>) -> Cow<'_, [PathBuf]> {
        match file {
            Some(file) if !self.root_relative_include_paths.is_empty() => {
                Cow::Owned(oxabl_index::search::root_include_paths(
                    file,
                    &self.root_relative_include_paths,
                    &self.include_paths,
                ))
            }
            _ => Cow::Borrowed(&self.include_paths),
        }
    }

    /// Whether any file has a search path: an include path or a root-relative
    /// one.
    pub fn searches_any_path(&self) -> bool {
        !self.include_paths.is_empty() || !self.root_relative_include_paths.is_empty()
    }

    /// Resolve configuration for a run anchored at `anchor`, which may be the
    /// file or the directory the client was pointed at — both resolve
    /// identically, since a file anchor starts the ancestor walk at its parent.
    ///
    /// Reads `oxabl.toml` **once**: the nearest ancestor of `anchor` holding one
    /// wins (the same discovery rule `cargo`, `rustfmt`, and `tsc` use), and the
    /// parsed value feeds every surface via [`resolve_from_config`]. With no
    /// `oxabl.toml` anywhere above `anchor`, the overrides are applied to
    /// [`PipelineConfig::default`] and no warning is produced — an
    /// unconfigured project is not a misconfigured one.
    ///
    /// Precedence throughout is caller-overrides-first (R6): include paths are
    /// `[flags…, config…]`, `--schema` replaces the configured `.df` set, and
    /// `--style` replaces the configured style guide.
    ///
    /// Warnings are non-fatal by construction — a malformed `oxabl.toml` yields
    /// defaults plus one [`ConfigWarning::Config`], never an error.
    pub fn resolve(anchor: &Path, overrides: &ConfigOverrides) -> (Self, Vec<ConfigWarning>) {
        resolve_at(anchor, overrides, SchemaLoading::Load)
    }

    /// Resolve everything except the schema, which stays [`Schema::empty`] with
    /// `schema_loaded = false` — no `.df` is opened and no schema warning is
    /// produced (D2).
    ///
    /// For a client whose pipeline cannot consume a schema at all.
    /// [`FormatPipeline`](crate::FormatPipeline) takes a [`StyleGuide`] and
    /// nothing else, so under [`resolve`](Self::resolve) `oxabl format` parsed
    /// every configured `.df` per invocation and printed `warning: schema:` lines
    /// about problems it had no way to act on — on the command most likely to be
    /// wired to save-on-write.
    ///
    /// This is *not* a lighter `resolve` to reach for when the schema is merely
    /// unlikely to be needed: a caller that might read `schema`/`schema_loaded`
    /// would silently see "no schema configured" here. The name says style
    /// because that is the surface it is for.
    pub fn resolve_style_only(
        anchor: &Path,
        overrides: &ConfigOverrides,
    ) -> (Self, Vec<ConfigWarning>) {
        resolve_at(anchor, overrides, SchemaLoading::Skip)
    }
}

/// Whether a resolution reads the `.df` files its configuration names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SchemaLoading {
    Load,
    Skip,
}

/// The shared body of both public resolvers: find the root once, read the file
/// once, hand the parsed value to the derivation.
///
/// Both entry points funnel through here so the one-parse invariant (KTD3) holds
/// for every caller — a second resolver with its own discovery-and-read would be
/// exactly the duplication [`resolve_from_config`] exists to prevent.
fn resolve_at(
    anchor: &Path,
    overrides: &ConfigOverrides,
    schema: SchemaLoading,
) -> (PipelineConfig, Vec<ConfigWarning>) {
    let start_dir = start_dir(anchor);

    let root = find_workspace_root(&start_dir);
    let parsed = root.as_ref().map(|r| (r, WorkspaceConfig::from_path(r)));

    match parsed {
        // A config exists and parsed: derive everything from that one value.
        Some((root, Ok(config))) => derive(&config, root, overrides, schema),
        // A config exists but is unreadable or malformed. Degrade every
        // configured surface to its default and report once — not once per
        // surface, which is what the three `resolved_*` helpers do today.
        Some((root, Err(message))) => {
            let (config, mut warnings) =
                derive(&WorkspaceConfig::defaults(), root, overrides, schema);
            warnings.insert(0, ConfigWarning::Config(message));
            (config, warnings)
        }
        // No config anywhere above the anchor: overrides only, silently.
        None => derive(&WorkspaceConfig::defaults(), &start_dir, overrides, schema),
    }
}

/// Derive every configuration surface from one already-parsed `config`.
///
/// `root` anchors the config's relative paths — its include paths and its `.df`
/// files are joined against the directory holding `oxabl.toml`, not against the
/// process's working directory, so moving the anchor deeper into the tree does
/// not move what the config means. Caller-supplied relative paths are anchored
/// to the working directory instead, because that is where the user typed them.
///
/// This function performs **no** config I/O, which is the point (KTD3): with the
/// read confined to [`PipelineConfig::resolve`], there is no code path through
/// which one resolution parses `oxabl.toml` twice. It does read the `.df` files
/// the resolved configuration names — schema content is not part of
/// `oxabl.toml`.
pub fn resolve_from_config(
    config: &WorkspaceConfig,
    root: &Path,
    overrides: &ConfigOverrides,
) -> (PipelineConfig, Vec<ConfigWarning>) {
    derive(config, root, overrides, SchemaLoading::Load)
}

/// [`resolve_from_config`]'s body, with the schema step made optional so
/// [`PipelineConfig::resolve_style_only`] can skip it (D2).
///
/// Deliberately one function rather than two: every non-schema surface is derived
/// identically either way, and a second copy would be a second set of precedence
/// rules to keep in step.
fn derive(
    config: &WorkspaceConfig,
    root: &Path,
    overrides: &ConfigOverrides,
    schema_loading: SchemaLoading,
) -> (PipelineConfig, Vec<ConfigWarning>) {
    let mut warnings = Vec::new();

    // Include paths: [CLI flags in order, config paths in file order], then an
    // order-preserving dedup so a directory named by both a flag and the config
    // is not stat'd twice per include lookup.
    let mut include_paths: Vec<PathBuf> = overrides.include_paths.iter().map(absolutize).collect();
    include_paths.extend(
        config
            .workspace
            .sources
            .include_paths
            .iter()
            .map(|p| anchor_to(root, p)),
    );
    let mut seen = std::collections::HashSet::new();
    include_paths.retain(|p| seen.insert(p.clone()));

    // Style: `--style` is a whole-guide selection, so it wins outright. Absent
    // it, serde's per-field `default` has already merged `[workspace.style]`
    // onto `StyleGuide::default_base()`.
    let style = overrides
        .style
        .clone()
        .unwrap_or_else(|| config.workspace.style.clone());

    let (mut lint_severities, rejected_patterns) = config.workspace.lint.to_severity_map_at(root);
    if let Some(file) = &config.workspace.lint.security.reach.entry_points {
        let file = anchor_to(root, file);
        let read = std::fs::read_to_string(&file)
            .map_err(|e| e.to_string())
            .and_then(|text| oxabl_workspace::parse_entry_points(&text));
        match read {
            Ok(entries) => {
                let mut security = lint_severities.security().clone();
                security.reach.entries = entries;
                lint_severities.set_security(security);
            }
            Err(e) => warnings.push(ConfigWarning::Lint(format!(
                "entry points `{}`: {e}",
                file.display()
            ))),
        }
    }
    warnings.extend(rejected_patterns.into_iter().map(|r| {
        ConfigWarning::Lint(if r.pattern.is_empty() {
            format!("ignoring override: {}", r.reason)
        } else {
            format!("ignoring override pattern `{}`: {}", r.pattern, r.reason)
        })
    }));
    warnings.extend(lint_name_warnings(&config.workspace.lint));
    let mut preprocessor = config.workspace.preprocessor.clone();
    let run_extensions = normalize_run_extensions(&config.workspace.sources.run_extensions);
    warnings.extend(
        run_extensions
            .rejected
            .into_iter()
            .map(ConfigWarning::RunExtension),
    );
    let run_extensions = run_extensions.extensions;
    let root_extensions = normalize_root_extensions(&config.workspace.sources.root_extensions);
    warnings.extend(
        root_extensions
            .rejected
            .into_iter()
            .map(ConfigWarning::RootExtension),
    );
    let root_extensions = root_extensions.extensions;
    let decoding = config.workspace.sources.decoding();

    let schema_path = match schema_loading {
        SchemaLoading::Load => overrides.schema_path.as_ref(),
        // No `.df` is opened and `[workspace.schema]` is not consulted: the
        // resolution reports exactly what it did, which is nothing.
        SchemaLoading::Skip => {
            return (
                PipelineConfig {
                    include_paths,
                    root_relative_include_paths: config
                        .workspace
                        .sources
                        .root_relative_include_paths
                        .clone(),
                    lint_severities,
                    style,
                    schema: Schema::empty(),
                    schema_loaded: false,
                    preprocessor,
                    run_extensions,
                    case_insensitive_lookup: config.workspace.sources.case_insensitive_lookup,
                    decoding,
                    root_extensions,
                },
                warnings,
            );
        }
    };

    // `--schema` replaces the configured set rather than adding to it; absent it,
    // `[workspace.schema].files` is the set. Either way the entries go through the
    // same expansion, so a directory means the same thing wherever it is written.
    let entries: Vec<PathBuf> = match schema_path {
        Some(path) => vec![path.clone()],
        None => config
            .workspace
            .schema
            .files
            .iter()
            .map(|f| anchor_to(root, f))
            .collect(),
    };
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in &entries {
        files.extend(expand_schema_entry(entry, &mut warnings));
    }

    // Nothing to open — no entries at all, or every entry was a directory that
    // yielded nothing (already warned about) — reads as no schema, so
    // schema-dependent diagnostics stay off. Otherwise load diagnostics are
    // non-fatal: a partially-loaded schema drives resolution better than none,
    // and an intentionally empty `.df` is a schema the user supplied.
    let (schema, schema_loaded) = if files.is_empty() {
        (Schema::empty(), false)
    } else {
        // The databases the schema describes are the ones connected, unless the
        // config says which are. A file name is only a guess at the logical name,
        // so this list stays apart from the one the config states.
        if preprocessor.databases.is_none() {
            preprocessor.inferred_databases = Some(logical_database_names(&files));
        }
        let (schema, diags) = SchemaLoader::load_files(&files, &EncodedFileSystem::new(decoding));
        warnings.extend(diags.into_iter().map(ConfigWarning::Schema));
        (schema, true)
    };

    (
        PipelineConfig {
            include_paths,
            root_relative_include_paths: config
                .workspace
                .sources
                .root_relative_include_paths
                .clone(),
            lint_severities,
            style,
            schema,
            schema_loaded,
            preprocessor,
            run_extensions,
            case_insensitive_lookup: config.workspace.sources.case_insensitive_lookup,
            decoding,
            root_extensions,
        },
        warnings,
    )
}

/// A warning for each `[workspace.lint]` name the built-in rule registry does
/// not know, and for each attempt to toggle an always-on group.
fn lint_name_warnings(lint: &LintConfig) -> Vec<ConfigWarning> {
    let registry = oxabl_lint::Registry::builtin_shared();
    let mut warnings = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for rule in lint.rule_selectors() {
        if registry.rule(rule).is_none() && seen.insert(("rule", rule)) {
            warnings.push(ConfigWarning::Lint(format!("unknown rule `{rule}`")));
        }
    }
    for group in lint.group_selectors() {
        let selected = registry.groups_selected_by(group);
        let message = if selected.is_empty() {
            format!("unknown rule group `{group}`")
        } else if selected.iter().any(|g| !g.toggleable) {
            format!(
                "rule group `{group}` is always on and cannot be enabled or disabled; \
                 set its rules to `off` one by one"
            )
        } else {
            continue;
        };
        if seen.insert(("group", group)) {
            warnings.push(ConfigWarning::Lint(message));
        }
    }
    let table = "`[workspace.lint.security.modes]`";
    for rule in lint.security.modes.keys() {
        match registry.rule(rule) {
            None => warnings.push(ConfigWarning::Lint(format!(
                "unknown rule `{rule}` in {table}"
            ))),
            Some(r) if r.meta().group != oxabl_lint::groups::SECURITY => warnings.push(
                ConfigWarning::Lint(format!("rule `{rule}` in {table} is not a security rule")),
            ),
            Some(_) => {}
        }
    }
    for v in &lint.security.validators {
        for kind in &v.protects {
            if oxabl_lint::security::taint::SinkKind::mask_of(kind.trim()).is_none() {
                warnings.push(ConfigWarning::Lint(format!(
                    "unknown sink kind `{kind}` for validator `{}`",
                    v.name
                )));
            }
        }
    }
    warnings
}

/// The logical database names that `.df` files stand for: each file name without
/// its extension, once per name however it is cased.
fn logical_database_names(files: &[PathBuf]) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for stem in files.iter().filter_map(|f| f.file_stem()) {
        let stem = stem.to_string_lossy();
        if !names.iter().any(|n| n.eq_ignore_ascii_case(&stem)) {
            names.push(stem.into_owned());
        }
    }
    names
}

/// Expand one schema entry — a `--schema` path or a `[workspace.schema].files`
/// entry — into the `.df` files it names.
///
/// A named path expands to itself, unread: a missing or unreadable `.df` the user
/// pointed at is a schema they supplied, so it reads as loaded and its load
/// diagnostic is the report.
///
/// A *directory* is the case that can silently yield **nothing** — no match, or no
/// read at all — and both had been resolving as a loaded-but-empty schema (A2).
/// Neither does: each gets its own warning and expands to nothing, which leaves
/// `schema_loaded` false so schema-dependent diagnostics stay off exactly as they
/// do with no schema at all. Shared by both entry sources because a directory in
/// `oxabl.toml` had kept the old behaviour — `SchemaLoader` read it as a file and
/// reported `Is a directory`, then the empty schema flooded the run with
/// undefined-symbol findings.
fn expand_schema_entry(path: &Path, warnings: &mut Vec<ConfigWarning>) -> Vec<PathBuf> {
    if !path.is_dir() {
        return vec![path.to_path_buf()];
    }
    match oxabl_schema::df_files_in_dir(path) {
        Err(e) => {
            warnings.push(ConfigWarning::SchemaPath(format!(
                "cannot read schema directory `{}`: {e}; schema-backed \
                 resolution is off for this run",
                path.display()
            )));
            Vec::new()
        }
        Ok(files) if files.is_empty() => {
            warnings.push(ConfigWarning::SchemaPath(format!(
                "`{}` matched no .df files; schema-backed resolution is off \
                 for this run",
                path.display()
            )));
            Vec::new()
        }
        Ok(files) => files,
    }
}

/// Where the ancestor walk for `oxabl.toml` begins: the anchor itself when it is
/// a directory, otherwise its parent. This is what makes a file anchor and its
/// containing directory resolve identically.
fn start_dir(anchor: &Path) -> PathBuf {
    let abs = absolutize(anchor);
    if abs.is_dir() {
        abs
    } else {
        abs.parent().map(Path::to_path_buf).unwrap_or(abs)
    }
}

/// Make a path absolute without touching the filesystem.
///
/// Relative paths are joined against the working directory. We deliberately do
/// **not** canonicalize (that errors on a not-yet-existing directory) and do not
/// collapse `..` (there is no allocation-free std normalizer, and include
/// resolution's `dir.join(name)` + existence check handles an embedded `..`
/// fine). A configured-but-absent directory simply never matches.
fn absolutize(p: impl AsRef<Path>) -> PathBuf {
    let p = p.as_ref();
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(p))
            .unwrap_or_else(|_| p.to_path_buf())
    }
}

/// Anchor a config-supplied path to the workspace root, leaving an absolute
/// config path verbatim.
fn anchor_to(root: &Path, p: &Path) -> PathBuf {
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        root.join(p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_common::Severity;
    use oxabl_style::IndentStyle;
    use std::fs;
    use tempfile::TempDir;

    fn write(dir: &Path, name: &str, contents: &str) {
        fs::write(dir.join(name), contents).unwrap();
    }

    /// The severity a built-in rule resolves to under `config`'s global lint
    /// settings, or `None` when it is off.
    fn rule_severity(config: &PipelineConfig, code: &str) -> Option<Severity> {
        let registry = oxabl_lint::Registry::builtin_shared();
        registry.severity_for(registry.rule(code).unwrap(), &config.lint_severities)
    }

    fn lint_warnings(toml: &str) -> Vec<String> {
        let parsed =
            WorkspaceConfig::from_toml(&format!("[workspace]\nname = \"p\"\n{toml}")).unwrap();
        let (_, warnings) =
            resolve_from_config(&parsed, Path::new("/ws"), &ConfigOverrides::default());
        warnings
            .into_iter()
            .map(|w| {
                assert!(matches!(w, ConfigWarning::Lint(_)), "{w:?}");
                w.to_string()
            })
            .collect()
    }

    #[test]
    fn known_lint_names_resolve_without_warnings() {
        let warnings = lint_warnings(
            "[workspace.lint]\nenable = [\"strict\", \"strict.calls\"]\n\
             LINT0002 = \"off\"\nUnused-Variable = \"info\"\n\
             CL001 = \"error\"\nrun-value = \"error\"\ndynamic-invoke = \"off\"\n\
             cl005 = \"warn\"\nmissing-no-undo = \"info\"\nTX006 = \"hint\"\n\
             [[workspace.lint.overrides]]\nfiles = [\"legacy\"]\ndisable = [\"strict.nulls\"]\n",
        );
        assert!(warnings.is_empty(), "{warnings:?}");
    }

    #[test]
    fn an_override_without_a_usable_pattern_is_a_warning() {
        let warnings = lint_warnings(
            "[[workspace.lint.overrides]]\nLINT0002 = \"off\"\n\
             [[workspace.lint.overrides]]\nfiles = []\nLINT0001 = \"off\"\n\
             [[workspace.lint.overrides]]\nfiles = [\"\"]\nLINT0003 = \"off\"\n",
        );
        assert_eq!(
            warnings,
            [
                "lint: ignoring override: the override has no `files` patterns, so it applies \
                 to no file",
                "lint: ignoring override: the override has no `files` patterns, so it applies \
                 to no file",
                "lint: ignoring override: empty pattern",
            ]
        );
    }

    #[test]
    fn security_options_name_known_rules_and_sink_kinds() {
        let warnings = lint_warnings(
            "[workspace.lint.security]\nmode = \"taint\"\n\
             [workspace.lint.security.modes]\nABLSEC005 = \"sink\"\nno-such-rule = \"sink\"\n\
             unused-variable = \"taint\"\n\
             [[workspace.lint.security.validators]]\nname = \"ok\"\nprotects = [\"query\", \"ALL\"]\n\
             [[workspace.lint.security.validators]]\nname = \"v\"\nprotects = [\"sqli\"]\n",
        );
        assert_eq!(
            warnings,
            [
                "lint: unknown rule `no-such-rule` in `[workspace.lint.security.modes]`",
                "lint: rule `unused-variable` in `[workspace.lint.security.modes]` is not a \
                 security rule",
                "lint: unknown sink kind `sqli` for validator `v`",
            ]
        );
    }

    #[test]
    fn the_entry_point_file_is_read_relative_to_the_config() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir(root.join("security")).unwrap();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.lint.security.reach]\nentry-points = \"security/entries.json\"\n",
        );
        write(
            root,
            "security/entries.json",
            r#"{"version": 1, "entries": [{"kind": "REST", "name": "GET /a", "program": "web/a.p"}]}"#,
        );
        write(root, "main.p", "");
        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        assert!(warnings.is_empty(), "{warnings:?}");
        let reach = &config.lint_severities.security().reach;
        assert!(reach.configured);
        assert_eq!(reach.entries.len(), 1);
        assert_eq!(reach.entries[0].label(), "REST GET /a");

        write(
            root,
            "security/entries.json",
            r#"{"version": 1, "entries": [{}]}"#,
        );
        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        let warnings: Vec<String> = warnings.iter().map(|w| w.to_string()).collect();
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(warnings[0].contains("entries.json"), "{warnings:?}");
        assert!(warnings[0].contains("entry 0"), "{warnings:?}");
        assert!(config.lint_severities.security().reach.configured);
        assert!(config.lint_severities.security().reach.entries.is_empty());
    }

    #[test]
    fn unknown_lint_names_are_warnings_not_errors() {
        let warnings = lint_warnings(
            "[workspace.lint]\nenable = [\"strict.speed\"]\nno-such-rule = \"off\"\n\
             [[workspace.lint.overrides]]\nfiles = [\"a/[\"]\nno-such-rule = \"error\"\n\
             disable = [\"default\"]\n",
        );
        assert_eq!(
            warnings,
            [
                "lint: ignoring override pattern `a/[`: Pattern syntax error near position 2: \
                 invalid range pattern",
                "lint: unknown rule `no-such-rule`",
                "lint: unknown rule group `strict.speed`",
                "lint: rule group `default` is always on and cannot be enabled or disabled; \
                 set its rules to `off` one by one",
            ]
        );
    }

    /// One call resolves all three configured surfaces — the whole point of
    /// collapsing three helpers into one resolution (R6, KTD3).
    #[test]
    fn one_call_resolves_include_paths_lint_and_style() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"inc\"]\n\
             [workspace.lint]\nunused-variable = \"off\"\n\
             [workspace.style]\nindent_size = 2\n",
        );
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());

        assert!(warnings.is_empty(), "{warnings:?}");
        assert_eq!(config.include_paths, vec![root.join("inc")]);
        assert_eq!(rule_severity(&config, "LINT0002"), None, "off");
        assert_eq!(
            rule_severity(&config, "LINT0001"),
            Some(Severity::Error),
            "unspecified rules keep their default"
        );
        assert_eq!(config.style.indent_size, 2);
        assert_eq!(
            config.style.indent_style,
            IndentStyle::Spaces,
            "unspecified style fields fall back to default_base"
        );
    }

    /// CLI-first, order-preserving, duplicate-free — the PROPATH precedence the
    /// three helpers established and this collapse must preserve exactly.
    #[test]
    fn cli_include_paths_precede_config_paths_dedup_preserving_order() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"/shared\", \"/cfg\"]\n",
        );
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            // `/shared` is named by both a flag and the config: it must appear
            // once, in the flag's earlier position.
            include_paths: vec![PathBuf::from("/first"), PathBuf::from("/shared")],
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        assert_eq!(
            config.include_paths,
            vec![
                PathBuf::from("/first"),
                PathBuf::from("/shared"),
                PathBuf::from("/cfg"),
            ]
        );
    }

    /// A relative `-I` is absolutized against the working directory — where the
    /// user typed it — so the preprocessor never has to resolve a path whose
    /// meaning depends on where the process happens to be by then.
    #[test]
    fn relative_cli_include_paths_are_absolutized_against_the_cwd() {
        let tmp = TempDir::new().unwrap();
        write(tmp.path(), "lonely.p", "");

        let overrides = ConfigOverrides {
            include_paths: vec![PathBuf::from("some/rel/inc")],
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&tmp.path().join("lonely.p"), &overrides);

        assert_eq!(config.include_paths.len(), 1);
        let resolved = &config.include_paths[0];
        assert!(resolved.is_absolute(), "got {}", resolved.display());
        assert!(
            resolved.ends_with("some/rel/inc"),
            "got {}",
            resolved.display()
        );
    }

    /// Config-relative include paths anchor to the directory holding
    /// `oxabl.toml`, not to the anchor's own subdirectory.
    #[test]
    fn config_relative_include_paths_anchor_to_workspace_root() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\"inc\"]\n",
        );
        let deep = root.join("a").join("b");
        fs::create_dir_all(&deep).unwrap();
        write(&deep, "deep.p", "");

        let (config, _) =
            PipelineConfig::resolve(&deep.join("deep.p"), &ConfigOverrides::default());
        assert_eq!(config.include_paths, vec![root.join("inc")]);
    }

    /// A malformed config is a warning, never an error, and is reported *once* —
    /// not once per surface it would have fed, which is the multiplication the
    /// three `resolved_*` helpers produce today.
    #[test]
    fn malformed_config_yields_defaults_and_exactly_one_warning() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "oxabl.toml", "this is not valid toml {{{");
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            include_paths: vec![PathBuf::from("/flag")],
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(matches!(warnings[0], ConfigWarning::Config(_)));
        assert!(warnings[0].to_string().contains("oxabl.toml"));

        // Overrides survive; every configured surface degrades to its default.
        assert_eq!(config.include_paths, vec![PathBuf::from("/flag")]);
        assert_eq!(rule_severity(&config, "LINT0001"), Some(Severity::Error));
        assert_eq!(
            config.style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
        assert!(!config.schema_loaded);
    }

    /// An unknown `[workspace.lint]` key is a hard parse error
    /// (`deny_unknown_fields`), so it takes the same single-warning path.
    #[test]
    fn unknown_lint_key_is_one_warning_not_three() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.lint]\nnonexistent-rule = \"warn\"\n",
        );
        write(root, "main.p", "");

        let (_, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        assert_eq!(warnings.len(), 1, "{warnings:?}");
    }

    /// An unconfigured project is not a misconfigured one: defaults, no warning.
    #[test]
    fn missing_config_yields_defaults_and_no_warnings() {
        let tmp = TempDir::new().unwrap();
        write(tmp.path(), "lonely.p", "");

        let (config, warnings) =
            PipelineConfig::resolve(&tmp.path().join("lonely.p"), &ConfigOverrides::default());

        assert!(warnings.is_empty(), "{warnings:?}");
        assert!(config.include_paths.is_empty());
        assert!(!config.schema_loaded);
        assert_eq!(
            config.style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
        assert_eq!(rule_severity(&config, "LINT0005"), Some(Severity::Info));
    }

    /// `check <dir>` and `analyze <dir>/file.p` must see the same configuration.
    #[test]
    fn file_anchor_and_parent_dir_anchor_resolve_identically() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"inc\"]\n\
             [workspace.style]\nindent_size = 3\n",
        );
        write(root, "main.p", "");

        let (from_file, _) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        let (from_dir, _) = PipelineConfig::resolve(root, &ConfigOverrides::default());

        assert_eq!(from_file.include_paths, from_dir.include_paths);
        assert_eq!(from_file.style.indent_size, from_dir.style.indent_size);
        assert_eq!(from_file.style.indent_size, 3);
    }

    /// `--style` names a whole guide, so it replaces the configured table rather
    /// than merging with it — while the *other* surfaces still come from the
    /// same single parse.
    #[test]
    fn style_override_wins_wholesale_without_losing_other_surfaces() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"inc\"]\n\
             [workspace.style]\nindent_size = 2\n",
        );
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            style: Some(StyleGuide::oestandards()),
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        assert_eq!(
            config.style.to_toml().unwrap(),
            StyleGuide::oestandards().to_toml().unwrap(),
            "an explicit --style must ignore the discovered table"
        );
        assert_eq!(
            config.include_paths,
            vec![root.join("inc")],
            "a style override must not suppress the rest of the resolution"
        );
    }

    /// The distinction `schema_loaded` exists to preserve: an empty `.df` is a
    /// loaded schema, so schema-dependent diagnostics stay on.
    #[test]
    fn empty_df_reads_as_loaded_and_no_schema_path_does_not() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "empty.df", "");
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            schema_path: Some(root.join("empty.df")),
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &overrides);
        assert!(
            config.schema.is_empty(),
            "an empty .df declares no tables..."
        );
        assert!(
            config.schema_loaded,
            "...but it is still a schema the user supplied"
        );

        let (config, _) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        assert!(!config.schema_loaded, "no schema path supplied");
    }

    fn preprocessor_of(table: &str, overrides: &ConfigOverrides) -> PreprocessorConfig {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir_all(root.join("schema")).unwrap();
        for name in ["ledger.df", "Stock.DF", "STOCK.df", "notes.txt"] {
            write(&root.join("schema"), name, "");
        }
        write(root, "main.p", "");
        write(
            root,
            "oxabl.toml",
            &format!("[workspace]\nname = \"p\"\n{table}"),
        );
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), overrides);
        config.preprocessor
    }

    const SCHEMA_FILES: &str = "[workspace.schema]\nfiles = [\"schema/ledger.df\", \"schema/Stock.DF\", \"schema/STOCK.df\"]\n";

    /// The schema files are the databases the code compiles against, so each
    /// one's name is a connected logical database, once however it is cased. The
    /// names stay apart from the stated list, as a file name is only a guess.
    #[test]
    fn databases_default_to_the_names_of_the_schema_files() {
        let pp = preprocessor_of(SCHEMA_FILES, &ConfigOverrides::default());
        assert_eq!(pp.inferred_databases.unwrap(), ["ledger", "Stock"]);
        assert_eq!(pp.databases, None);
    }

    #[test]
    fn a_schema_directory_names_a_database_per_df_file() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir_all(root.join("schema")).unwrap();
        write(&root.join("schema"), "ledger.df", "");
        write(&root.join("schema"), "stock.df", "");
        write(&root.join("schema"), "readme.txt", "");
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            schema_path: Some(root.join("schema")),
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &overrides);
        assert_eq!(
            config.preprocessor.inferred_databases.unwrap(),
            ["ledger", "stock"]
        );

        let overrides = ConfigOverrides {
            schema_path: Some(root.join("schema/stock.df")),
            ..Default::default()
        };
        let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &overrides);
        assert_eq!(config.preprocessor.inferred_databases.unwrap(), ["stock"]);
    }

    #[test]
    fn a_configured_database_list_replaces_the_schema_names() {
        let table = format!("{SCHEMA_FILES}[workspace.preprocessor]\ndatabases = [\"main\"]\n");
        let pp = preprocessor_of(&table, &ConfigOverrides::default());
        assert_eq!(pp.databases.unwrap(), ["main"]);
        assert_eq!(pp.inferred_databases, None);
        let table = format!("{SCHEMA_FILES}[workspace.preprocessor]\ndatabases = []\n");
        let pp = preprocessor_of(&table, &ConfigOverrides::default());
        assert_eq!(pp.databases.unwrap(), Vec::<String>::new());
        assert_eq!(pp.inferred_databases, None);
    }

    #[test]
    fn no_schema_and_no_list_leave_the_databases_unknown() {
        let pp = preprocessor_of("", &ConfigOverrides::default());
        assert_eq!((pp.databases, pp.inferred_databases), (None, None));

        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "main.p", "");
        write(
            root,
            "oxabl.toml",
            &format!("[workspace]\nname = \"p\"\n{SCHEMA_FILES}"),
        );
        let (config, _) =
            PipelineConfig::resolve_style_only(&root.join("main.p"), &ConfigOverrides::default());
        assert_eq!(config.preprocessor.databases, None);
        assert_eq!(config.preprocessor.inferred_databases, None);
    }

    /// A `--schema` *directory* holding no `.df` file is a misconfiguration, not
    /// a request for schemaless resolution (A2).
    ///
    /// Hard-coding `schema_loaded = true` here turned a typo'd `--schema` path
    /// into an `undefined-symbol` error on every table reference in the tree plus
    /// a `LINT0003` on every field — a flood of findings about correct code, with
    /// nothing anywhere saying the schema had not loaded. The distinction that
    /// matters is *directory with no matches* versus *a named file*: an
    /// explicitly named empty `.df` is stated user intent and stays loaded.
    #[test]
    fn schema_dir_with_no_df_files_warns_and_does_not_read_as_loaded() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        let schema_dir = root.join("schema");
        fs::create_dir_all(&schema_dir).unwrap();
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            schema_path: Some(schema_dir),
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        assert!(
            !config.schema_loaded,
            "a directory with no .df must not read as a loaded schema"
        );
        assert!(config.schema.is_empty());
        assert!(
            warnings
                .iter()
                .any(|w| w.to_string().contains("no .df files")),
            "the misconfiguration must be reported, got: {warnings:?}"
        );
    }

    /// An unreadable `--schema` directory reports the I/O error itself, so the
    /// message names the real cause rather than the empty result it produced.
    #[cfg(unix)]
    #[test]
    fn unreadable_schema_dir_warns_with_the_io_error_and_does_not_read_as_loaded() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        let schema_dir = root.join("schema");
        fs::create_dir_all(&schema_dir).unwrap();
        write(&schema_dir, "s.df", "ADD TABLE \"Customer\"\n");
        write(root, "main.p", "");
        fs::set_permissions(&schema_dir, fs::Permissions::from_mode(0o000)).unwrap();

        if fs::read_dir(&schema_dir).is_ok() {
            eprintln!(
                "skipping: this process can list a 0o000 directory (running privileged), \
                 so there is no unreadable-directory case to observe"
            );
            return;
        }

        let overrides = ConfigOverrides {
            schema_path: Some(schema_dir.clone()),
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        // Restore before the assertions so a failure still lets TempDir clean up.
        fs::set_permissions(&schema_dir, fs::Permissions::from_mode(0o755)).unwrap();

        assert!(!config.schema_loaded);
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w, ConfigWarning::SchemaPath(_))),
            "expected a schema-path warning, got: {warnings:?}"
        );
    }

    /// `--schema` replaces `[workspace.schema].files` rather than adding to it,
    /// matching the whole-selection semantics of `--style`.
    #[test]
    fn schema_override_replaces_configured_files() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.schema]\nfiles = [\"absent.df\"]\n",
        );
        write(root, "empty.df", "");
        write(root, "main.p", "");

        let overrides = ConfigOverrides {
            schema_path: Some(root.join("empty.df")),
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve(&root.join("main.p"), &overrides);

        assert!(config.schema_loaded);
        assert!(
            warnings.is_empty(),
            "the configured (missing) file must not be read at all: {warnings:?}"
        );
    }

    /// `resolve_style_only` reads the style from the same single parse but opens no
    /// `.df` and reports no schema problem (D2) — the surface a style-only client
    /// like [`FormatPipeline`](crate::FormatPipeline) can actually act on.
    #[test]
    fn resolve_style_only_reads_the_style_and_no_schema() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.schema]\nfiles = [\"absent.df\"]\n\
             [workspace.style]\nindent_size = 2\n",
        );
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve_style_only(&root.join("main.p"), &ConfigOverrides::default());

        assert_eq!(config.style.indent_size, 2, "the style still resolves");
        assert!(!config.schema_loaded);
        assert!(config.schema.is_empty());
        assert!(
            warnings.is_empty(),
            "an unreadable .df that was never opened cannot warn: {warnings:?}"
        );

        // …and the same tree through the full resolver does warn, so the
        // difference is the schema step and not the fixture.
        let (_, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w, ConfigWarning::Schema(_))),
            "{warnings:?}"
        );
    }

    /// A schema-free resolution still reports a config it could not parse: the
    /// skipped step is the schema, not the honesty about the file.
    #[test]
    fn resolve_style_only_still_reports_an_unusable_config() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(root, "oxabl.toml", "this is not valid toml {{{");
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve_style_only(&root.join("main.p"), &ConfigOverrides::default());

        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(matches!(warnings[0], ConfigWarning::Config(_)));
        assert_eq!(
            config.style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    /// A configured `.df` that cannot be read is a warning, not a failure — and
    /// the resolution still reports the schema as loaded, since the user asked
    /// for one.
    #[test]
    fn unreadable_configured_schema_file_warns_without_failing() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.schema]\nfiles = [\"absent.df\"]\n",
        );
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());

        assert!(config.schema_loaded);
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w, ConfigWarning::Schema(_))),
            "{warnings:?}"
        );
    }

    /// A *directory* in `[workspace.schema].files` is the same misconfiguration as
    /// a `--schema` directory holding no `.df`, and gets the same answer. It used
    /// to reach `SchemaLoader` as a file, which reported `Is a directory` and left
    /// an empty schema reading as loaded — every symbol in the run then came back
    /// undefined.
    #[test]
    fn configured_schema_dir_with_no_df_files_warns_and_does_not_read_as_loaded() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir_all(root.join("sch")).unwrap();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.schema]\nfiles = [\"sch\"]\n",
        );
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());

        assert!(
            !config.schema_loaded,
            "a configured directory with no .df must not read as a loaded schema"
        );
        assert!(config.schema.is_empty());
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w, ConfigWarning::SchemaPath(_))),
            "expected a schema-path warning, got: {warnings:?}"
        );
    }

    /// The other half: a configured directory that *does* hold a `.df` loads it,
    /// so routing configured entries through the directory handling did not cost
    /// the capability.
    #[test]
    fn configured_schema_dir_holding_a_df_loads_it() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        let schema_dir = root.join("sch");
        fs::create_dir_all(&schema_dir).unwrap();
        write(&schema_dir, "s.df", "ADD TABLE \"Customer\"\n");
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.schema]\nfiles = [\"sch\"]\n",
        );
        write(root, "main.p", "");

        let (config, warnings) =
            PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());

        assert!(config.schema_loaded, "{warnings:?}");
        assert!(!config.schema.is_empty());
        assert!(
            !warnings
                .iter()
                .any(|w| matches!(w, ConfigWarning::SchemaPath(_))),
            "a directory that matched should not warn: {warnings:?}"
        );
    }

    /// `resolve_from_config` derives all three surfaces from one already-parsed
    /// value, with no config I/O of its own — so no path exists through which a
    /// single resolution reads `oxabl.toml` twice (KTD3). The proof is that it
    /// works at all against a root directory holding **no** `oxabl.toml`: a
    /// re-reading implementation would find nothing there and lose the values.
    #[test]
    fn resolve_from_config_derives_every_surface_without_reading_a_file() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        assert!(
            !root.join("oxabl.toml").exists(),
            "the fixture must not have a config file on disk"
        );

        let parsed = WorkspaceConfig::from_toml(
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"inc\"]\n\
             [workspace.lint]\nundefined-symbol = \"off\"\n\
             [workspace.style]\nindent_size = 7\n",
        )
        .unwrap();

        let (config, warnings) = resolve_from_config(&parsed, root, &ConfigOverrides::default());

        assert!(warnings.is_empty(), "{warnings:?}");
        assert_eq!(config.include_paths, vec![root.join("inc")]);
        assert_eq!(rule_severity(&config, "LINT0001"), None);
        assert_eq!(config.style.indent_size, 7);
    }

    #[test]
    fn the_source_encoding_reaches_the_config_and_the_schema_files() {
        use oxabl_workspace::SourceEncoding;
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::write(
            root.join("ledger.df"),
            b"ADD TABLE \"cust\"\n  DESCRIPTION \"caf\xe9\"\nADD FIELD \"id\" OF \"cust\" AS integer\n",
        )
        .unwrap();
        let toml = |sources: &str| {
            format!(
                "[workspace]\nname = \"p\"\n[workspace.sources]\n{sources}\
                 [workspace.schema]\nfiles = [\"ledger.df\"]\n"
            )
        };

        fs::write(root.join("oxabl.toml"), toml("")).unwrap();
        let (config, warnings) = PipelineConfig::resolve(root, &ConfigOverrides::default());
        assert_eq!(config.decoding, SourceDecoding::default());
        assert!(!warnings.is_empty(), "invalid UTF-8 is a schema warning");
        assert!(config.schema.is_empty());

        fs::write(
            root.join("oxabl.toml"),
            toml("encoding = \"windows-1252\"\n"),
        )
        .unwrap();
        let (config, warnings) = PipelineConfig::resolve(root, &ConfigOverrides::default());
        assert_eq!(config.decoding.encoding, SourceEncoding::Windows1252);
        assert_eq!(warnings, []);
        assert!(!config.schema.is_empty());
    }

    #[test]
    fn root_extensions_are_normalized_and_a_bad_entry_warns() {
        let tmp = TempDir::new().unwrap();
        fs::write(
            tmp.path().join("oxabl.toml"),
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\nroot_extensions = [\".p\", \"i\", \"pp\"]\n",
        )
        .unwrap();
        let (config, warnings) = PipelineConfig::resolve(tmp.path(), &ConfigOverrides::default());
        assert_eq!(config.root_extensions, ["p", "pp"]);
        assert_eq!(warnings, [ConfigWarning::RootExtension("i".to_string())]);
        assert_eq!(
            PipelineConfig::default().root_extensions,
            ["p", "w", "cls", "v"]
        );
    }

    #[test]
    fn run_extensions_are_normalized_and_a_bad_entry_warns() {
        let parsed = WorkspaceConfig::from_toml(
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\nrun_extensions = [\".p\", \"\", \"a/b\", \"w\"]\n",
        )
        .unwrap();

        let (config, warnings) = resolve_from_config(
            &parsed,
            Path::new("/nonexistent-root"),
            &ConfigOverrides::default(),
        );

        assert_eq!(config.run_extensions, ["p", "w"]);
        assert_eq!(
            warnings,
            [
                ConfigWarning::RunExtension(String::new()),
                ConfigWarning::RunExtension("a/b".to_string()),
            ]
        );
    }

    /// The nearest `oxabl.toml` wins, as with every ancestor-walking tool.
    #[test]
    fn nearest_ancestor_config_shadows_a_parent() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 8\n",
        );
        let deep = root.join("a").join("b");
        fs::create_dir_all(&deep).unwrap();
        write(
            &deep,
            "oxabl.toml",
            "[workspace]\nname = \"child\"\n[workspace.style]\nindent_size = 2\n",
        );
        write(&deep, "deep.p", "");

        let (config, _) =
            PipelineConfig::resolve(&deep.join("deep.p"), &ConfigOverrides::default());
        assert_eq!(config.style.indent_size, 2);
    }

    #[test]
    fn config_warning_renders_readably() {
        let w = ConfigWarning::Config("failed to parse oxabl.toml: boom".to_string());
        assert_eq!(w.to_string(), "failed to parse oxabl.toml: boom");
    }

    #[test]
    fn root_relative_paths_come_first_resolved_against_the_files_directory() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            root,
            "oxabl.toml",
            "[workspace]\nname = \"p\"\n\
             [workspace.sources]\ninclude_paths = [\"inc\", \"app/rf\"]\n\
             root_relative_include_paths = [\".\", \"rf\"]\n",
        );
        let (config, _) = PipelineConfig::resolve(root, &ConfigOverrides::default());
        assert_eq!(
            config.root_relative_include_paths,
            [".", "rf"].map(PathBuf::from)
        );
        assert_eq!(
            config.include_paths,
            [root.join("inc"), root.join("app/rf")]
        );

        let file = root.join("app/main.p");
        assert_eq!(
            config.include_paths_for(Some(&file)).into_owned(),
            [root.join("app"), root.join("app/rf"), root.join("inc")]
        );
        assert_eq!(
            config.include_paths_for(None).into_owned(),
            config.include_paths
        );
        assert!(config.searches_any_path());
    }

    #[test]
    fn without_root_relative_paths_every_file_searches_the_include_paths() {
        let config = PipelineConfig {
            include_paths: vec![PathBuf::from("/inc")],
            ..PipelineConfig::default()
        };
        assert!(matches!(
            config.include_paths_for(Some(Path::new("/app/main.p"))),
            Cow::Borrowed(paths) if paths == [PathBuf::from("/inc")]
        ));
        let none = PipelineConfig {
            root_relative_include_paths: vec![PathBuf::from(".")],
            ..PipelineConfig::default()
        };
        assert!(none.searches_any_path());
        assert!(!PipelineConfig::default().searches_any_path());
    }
}
