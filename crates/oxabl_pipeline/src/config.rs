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

use std::path::{Path, PathBuf};

use oxabl_common::{Diagnostic, LintSeverityMap};
use oxabl_schema::{Schema, SchemaLoader};
use oxabl_style::StyleGuide;
use oxabl_workspace::{LintConfig, RealFileSystem, WorkspaceConfig, find_workspace_root};

/// A non-fatal problem hit while resolving configuration.
///
/// Returned as data (R7) so each client decides how — and whether — to surface
/// it. None of these stops a run: every one degrades to a documented default.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum ConfigWarning {
    /// An `oxabl.toml` was found but could not be read or parsed (including an
    /// unknown key in `[workspace.lint]` or `[workspace.style]`, both of which
    /// use `deny_unknown_fields`). Every configured surface degrades to its
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
}

impl std::fmt::Display for ConfigWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigWarning::Config(msg) => write!(f, "{msg}"),
            ConfigWarning::Schema(d) => write!(f, "schema: [{}] {}", d.code.0, d.message),
            ConfigWarning::SchemaPath(msg) => write!(f, "schema: {msg}"),
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

    /// Per-rule lint severities, already lowered from `[workspace.lint]`'s
    /// user-facing five-level surface to the application form.
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
}

impl Default for PipelineConfig {
    /// The configuration of a project with no `oxabl.toml`: no include paths, the
    /// **documented default** lint severities, the safe non-mangling base style,
    /// and no schema.
    ///
    /// # Why the severity map is not simply empty (R19)
    ///
    /// An empty [`LintSeverityMap`] is not "the defaults" — it means *no rule has
    /// a configured severity*, so each diagnostic keeps whatever severity
    /// `oxabl_lint` happens to construct it with. For two rules
    /// (`unknown-table-or-field` and `type-mismatch-assignment`) that built-in
    /// value is `Error`, while `[workspace.lint]`'s documented default is `Warn`.
    ///
    /// [`resolve`](PipelineConfig::resolve) materializes the documented defaults
    /// even when no config file exists, so leaving this map empty gave the two
    /// clients that build a config directly — the browser, and any embedding
    /// caller — a *different answer for the same input* than every
    /// filesystem-backed client. That is precisely what R19 forbids: the client
    /// is not allowed to be a variable in the answer. The cross-client parity
    /// suite caught it, and the fix is that both paths now read one table.
    ///
    /// Deriving from [`LintConfig::default`] rather than restating the severities
    /// here is deliberate: a restatement is a second table that can drift, which
    /// is the bug this doc comment exists to explain.
    fn default() -> Self {
        PipelineConfig {
            include_paths: Vec::new(),
            lint_severities: LintConfig::default().to_severity_map(),
            style: StyleGuide::default_base(),
            schema: Schema::empty(),
            schema_loaded: false,
        }
    }
}

impl PipelineConfig {
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
        let start_dir = start_dir(anchor);

        let root = find_workspace_root(&start_dir);
        let parsed = root.as_ref().map(|r| (r, WorkspaceConfig::from_path(r)));

        match parsed {
            // A config exists and parsed: derive everything from that one value.
            Some((root, Ok(config))) => resolve_from_config(&config, root, overrides),
            // A config exists but is unreadable or malformed. Degrade every
            // configured surface to its default and report once — not once per
            // surface, which is what the three `resolved_*` helpers do today.
            Some((root, Err(message))) => {
                let (config, mut warnings) =
                    resolve_from_config(&WorkspaceConfig::defaults(), root, overrides);
                warnings.insert(0, ConfigWarning::Config(message));
                (config, warnings)
            }
            // No config anywhere above the anchor: overrides only, silently.
            None => resolve_from_config(&WorkspaceConfig::defaults(), &start_dir, overrides),
        }
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

    let lint_severities = config.workspace.lint.to_severity_map();

    let (schema, schema_loaded) = match &overrides.schema_path {
        // `--schema` replaces the configured set. A directory loads every `.df`
        // inside it; a path loads that one file.
        //
        // A directory is the case that can silently yield *nothing* — no match, or
        // no read at all — and both had been resolving as a loaded-but-empty
        // schema (A2). Neither is: they get their own warning and leave
        // `schema_loaded` false, so schema-dependent diagnostics stay off exactly
        // as they do with no `--schema` at all.
        Some(path) if path.is_dir() => match oxabl_schema::df_files_in_dir(path) {
            Err(e) => {
                warnings.push(ConfigWarning::SchemaPath(format!(
                    "cannot read schema directory `{}`: {e}; schema-backed \
                     resolution is off for this run",
                    path.display()
                )));
                (Schema::empty(), false)
            }
            Ok(files) if files.is_empty() => {
                warnings.push(ConfigWarning::SchemaPath(format!(
                    "`{}` matched no .df files; schema-backed resolution is off \
                     for this run",
                    path.display()
                )));
                (Schema::empty(), false)
            }
            Ok(files) => {
                let (schema, diags) = SchemaLoader::load_files(&files, &RealFileSystem);
                warnings.extend(diags.into_iter().map(ConfigWarning::Schema));
                (schema, true)
            }
        },
        // A *named* path: load that one file. Load diagnostics are non-fatal — a
        // partially-loaded schema still drives resolution — and an intentionally
        // empty `.df` is a schema the user supplied, so it reads as loaded.
        Some(path) => {
            let (schema, diags) =
                SchemaLoader::load_files(std::slice::from_ref(path), &RealFileSystem);
            warnings.extend(diags.into_iter().map(ConfigWarning::Schema));
            (schema, true)
        }
        None => {
            let files: Vec<PathBuf> = config
                .workspace
                .schema
                .files
                .iter()
                .map(|f| anchor_to(root, f))
                .collect();
            if files.is_empty() {
                (Schema::empty(), false)
            } else {
                let (schema, diags) = SchemaLoader::load_files(&files, &RealFileSystem);
                warnings.extend(diags.into_iter().map(ConfigWarning::Schema));
                (schema, true)
            }
        }
    };

    (
        PipelineConfig {
            include_paths,
            lint_severities,
            style,
            schema,
            schema_loaded,
        },
        warnings,
    )
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
        assert_eq!(config.lint_severities.get("LINT0002"), Some(None), "off");
        assert_eq!(
            config.lint_severities.get("LINT0001"),
            Some(Some(Severity::Error)),
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
        assert_eq!(
            config.lint_severities.get("LINT0001"),
            Some(Some(Severity::Error))
        );
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
        assert_eq!(
            config.lint_severities.get("LINT0005"),
            Some(Some(Severity::Info))
        );
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
        assert_eq!(config.lint_severities.get("LINT0001"), Some(None));
        assert_eq!(config.style.indent_size, 7);
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
}
