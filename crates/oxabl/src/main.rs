use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::Instant;

use clap::Parser as ClapParser;
use indicatif::{ProgressBar, ProgressStyle};
use oxabl_analyze::{CollectedDiagnostics, dump_json_with_diagnostics, dump_text_with_diagnostics};
use oxabl_common::{Diagnostic, FileId, SourceMap, SourceResolver, render_diagnostics};
use oxabl_pipeline::{
    ConfigOverrides, ConfigWarning, FormatPipeline, LintPipeline, PipelineConfig, ROOT_FILE_ID,
    position,
};
use oxabl_preprocessor::Preprocessor;
use oxabl_style::StyleGuide;
use oxabl_workspace::{RealFileSystem, discover_path};
use serde::Serialize;

#[derive(ClapParser)]
#[command(name = "oxabl", about = "High-performance tooling for Progress ABL")]
enum Cli {
    /// Lint ABL source and report formatting drift — the one pre-commit gate.
    ///
    /// Two channels, deliberately not merged (KTD7): lint diagnostics are
    /// span-anchored findings, while format drift is a per-file boolean. Folding
    /// the second into the first would mean inventing spans that do not exist,
    /// so `--json` carries two findings keys instead of one array.
    ///
    /// Exit codes (R15): `0` with neither a lint finding nor drift, `1` when
    /// either is present (or a file could not be read or analyzed), `2` on a
    /// usage or config error. `--json` adds `6` for a serialization failure,
    /// matching `analyze`.
    ///
    /// A config error means an `oxabl.toml` this run could not **use** — which is
    /// broader than malformed TOML. Parsing is strict (unknown fields are
    /// rejected), so a syntactically valid file naming a key oxabl does not know —
    /// a misspelled or not-yet-released `[workspace.lint]` rule, say — is a config
    /// error too. Alone among the subcommands, `check` refuses to run on one rather
    /// than degrading to defaults: those defaults would drop the project's
    /// `[workspace.lint]` severities, `off` included, so the gate would report
    /// findings for switched-off rules and could still exit 0 — a wrong answer
    /// under a green light.
    /// Non-fatal problems (a schema file that would not load, a `--schema`
    /// directory that matched nothing) stay `warning:` lines and do not move the
    /// exit code.
    ///
    /// `oxabl format --check` remains the format pipeline's granular dry-run —
    /// the *same* pipeline this command calls, so the two cannot diverge.
    Check {
        /// Path to a directory or single file to check
        path: PathBuf,

        /// Output results as JSON
        #[arg(long)]
        json: bool,

        /// Skip preprocessing: do not expand includes or evaluate `&IF`.
        ///
        /// Preprocessing is **on by default** here, unlike `conformance` and
        /// `analyze` (R19). A lint gate that does not expand includes cannot see
        /// the symbols an include declares, so it reports every one of them as
        /// `undefined-symbol` — a flood of findings about the caller's own
        /// correct code. The language server always preprocesses, so leaving it
        /// off by default would make the gate and the editor disagree on any
        /// project that uses an include, which is exactly what the shared
        /// pipeline exists to prevent.
        ///
        /// Skipping it is still useful for a fast structural pass over a tree
        /// whose include paths are not configured; expect undefined-symbol noise
        /// if the source relies on includes.
        #[arg(long = "no-preprocess")]
        no_preprocess: bool,

        /// Accepted and ignored: preprocessing is the default for `check`.
        ///
        /// Kept so an invocation written against the flag's earlier opt-in form
        /// keeps working and keeps meaning the same thing. Hidden because it is
        /// now a no-op.
        #[arg(long, hide = true)]
        preprocess: bool,

        /// Include search paths (can be specified multiple times)
        #[arg(long = "include-path", short = 'I')]
        include_paths: Vec<PathBuf>,

        /// Path to a `.df` schema file (or a directory of them) driving
        /// schema-backed resolution — field validation, field types, and the
        /// unknown-table/field rule.
        #[arg(long = "schema")]
        schema: Option<PathBuf>,

        /// Suppress lint findings; parse/semantic errors still gate (R16).
        ///
        /// A **filter on the reported set**, not a skipped run — the same meaning
        /// `analyze --no-lint` has always had. The pipeline that produces lint
        /// findings is also the only source of `PARSE001` and the semantic
        /// diagnostics, so skipping it would let a file oxabl cannot even parse
        /// pass the gate with a green exit code.
        #[arg(long)]
        no_lint: bool,

        /// Suppress the format-drift channel: report lint findings only (R16).
        #[arg(long)]
        no_format: bool,
    },
    /// Walk a tree and report the parser's conformance: how many files parse,
    /// which ones fail and where, and the ranked error patterns behind the
    /// failures.
    ///
    /// This is the parser-refinement instrument, not a user-facing check — it
    /// answers "how much ABL can oxabl parse today?", which is a question about
    /// oxabl rather than about the caller's source. It is deliberately
    /// **hidden** from `--help` (R17/R23): it stays reachable and documented for
    /// the corpus loop and the A/B gate without growing the advertised CLI
    /// surface.
    #[command(hide = true)]
    Conformance {
        /// Path to a directory or single file to walk
        path: PathBuf,

        /// Output results as JSON
        #[arg(long)]
        json: bool,

        /// Enable preprocessing (include expansion, &IF evaluation)
        #[arg(long)]
        preprocess: bool,

        /// Include search paths (can be specified multiple times)
        #[arg(long = "include-path", short = 'I')]
        include_paths: Vec<PathBuf>,

        /// Show AST context on parse failure (single-file mode only)
        #[arg(long)]
        debug: bool,
    },
    /// Parse + semantic-analyze a single ABL file and dump the resolved model.
    ///
    /// This is the semantic layer's introspection tool, not a gate: it answers
    /// "what did oxabl understand about this file?", so it **exits 0 whatever it
    /// finds** (KTD9). A file full of lint findings and a clean one are the same
    /// success here; `oxabl check` is the command whose exit code means something.
    ///
    /// Deliberately **hidden** from `--help` (R23), alongside `conformance`: it is
    /// a debugging instrument for oxabl's own model, so it earns its own command
    /// but not a place in the advertised surface. It stays fully reachable and
    /// `oxabl analyze --help` works as normal.
    ///
    /// Exit codes: `0` on a successful dump regardless of diagnostics, `2` when
    /// the file cannot be read, `3` on a fatal preprocessing failure (no model to
    /// dump), `4` on a contained internal panic, `6` on a serialization failure,
    /// `7` on an unrecognized `--format`. Single-file by design, which is why a
    /// panic aborts here rather than being reported-and-skipped as `check` does
    /// (R24): there is no "rest of the walk" to protect.
    #[command(hide = true)]
    Analyze {
        /// Path to the ABL source file to analyze.
        path: PathBuf,

        /// Output format: `json` (stable, versioned) or `text` (human-oriented).
        #[arg(long, default_value = "json")]
        format: String,

        /// Drop lint findings from the dump (semantic-layer diagnostics only).
        ///
        /// A **filter on the result**, not a skipped pass: the envelope still
        /// wants the semantic model, so the pipeline runs either way and the
        /// lint-sourced diagnostics are removed from the reported set. `check
        /// --no-lint` means the same thing, for the same reason.
        #[arg(long)]
        no_lint: bool,

        /// Enable preprocessing (include expansion, &IF evaluation).
        #[arg(long)]
        preprocess: bool,

        /// Include search paths (can be specified multiple times).
        #[arg(long = "include-path", short = 'I')]
        include_paths: Vec<PathBuf>,

        /// Path to a `.df` schema file driving schema-backed resolution
        /// (field validation, field types, unknown-table/field lint).
        #[arg(long = "schema")]
        schema: Option<PathBuf>,
    },
    /// Format ABL source: fix layout (indentation, blank-line runs, comment
    /// placement) in place, or check/print without writing.
    ///
    /// v1 is layout-only and no-movement. It never renames identifiers, reorders
    /// statements, or rewrites comment bodies. Notably, long lines are left
    /// exactly as written: the resolved style's `wrap_long_lines` /
    /// `max_line_length` fields are read but deliberately NOT enforced in v1 — a
    /// 200-column line stays 200 columns. Reflow is deferred to a future version.
    ///
    /// Style resolution precedence: --style <preset|path> > oxabl.toml
    /// [workspace.style] > the safe non-mangling default. Any file that cannot be
    /// formatted faithfully (parse errors, or output that would alter the token
    /// stream) is left byte-for-byte unchanged and the reason is reported.
    Format {
        /// Path to a single ABL file or a directory to format
        /// (`.p`/`.w`/`.cls`/`.v` under a directory).
        path: PathBuf,

        /// CI mode: exit non-zero if any file would change; write nothing.
        #[arg(long, conflicts_with = "stdout")]
        check: bool,

        /// Print the formatted result to stdout; leave the file on disk
        /// unchanged. Single file only.
        #[arg(long)]
        stdout: bool,

        /// Style to format with: a named preset (`oestandards`,
        /// `consultingwerk`) or a path to a `.toml` style file. Overrides any
        /// discovered `oxabl.toml [workspace.style]` wholesale.
        #[arg(long)]
        style: Option<String>,
    },
    /// Run the language server over stdio (LSP), publishing live diagnostics.
    Lsp,
    /// Serve one workspace through the shared oxabl daemon.
    #[command(hide = true)]
    Daemon {
        /// Workspace root this daemon owns.
        workspace_root: PathBuf,
    },
    /// Emit the JSON Schema for `oxabl.toml` to stdout.
    ///
    /// The schema is derived directly from the config structs
    /// (`WorkspaceConfig` and everything it embeds) via `schemars`, so it is
    /// the single source of truth and auto-covers every style + lint rule with
    /// no hand-maintained mirror. Editors (e.g. Even Better TOML) consume it for
    /// autocomplete/validation inside `oxabl.toml`.
    Schema,
}

enum FileResult {
    Success,
    ParseError {
        path: PathBuf,
        line: usize,
        col: usize,
        message: String,
    },
    IoError {
        path: PathBuf,
        error: String,
    },
    LexerPanic {
        path: PathBuf,
    },
}

#[derive(Serialize)]
struct JsonReport {
    total: usize,
    passed: usize,
    failed: usize,
    io_errors: usize,
    lexer_panics: usize,
    success_rate: f64,
    elapsed_secs: f64,
    files_per_sec: f64,
    failures: Vec<JsonFailure>,
    error_patterns: Vec<JsonErrorPattern>,
}

#[derive(Serialize)]
struct JsonFailure {
    path: String,
    line: usize,
    col: usize,
    message: String,
}

#[derive(Serialize)]
struct JsonErrorPattern {
    pattern: String,
    count: usize,
}

/// Surface *loud* preprocessor diagnostics — errors plus selected warnings —
/// to stderr via the shared [`render_diagnostics`] renderer, and return them as
/// [`Diagnostic`]s for machine-readable output.
///
/// Always-loud warning codes:
/// - `PREPROC007` unresolvable include (symbol loss)
/// - `PREPROC002` unclosed `&IF` (inline/`skip_to_eol` regressions; #65 gate)
///
/// Rendering is delegated to [`render_diagnostics`] with a [`SourceResolver`]
/// over the root file: root-file diagnostics get a `path:line:col` position and
/// a snippet; diagnostics from a nested include (a different `FileId`) render
/// without a misleading root-relative position.
fn surface_preproc_diagnostics(
    path: &Path,
    root_source: &str,
    root_file_id: FileId,
    diagnostics: &[Diagnostic],
) -> Vec<Diagnostic> {
    // Share the surfacing rule with the collector so the two never drift (U4).
    let is_loud = oxabl_analyze::is_loud;
    let loud: Vec<Diagnostic> = diagnostics.iter().filter(|d| is_loud(d)).cloned().collect();
    if loud.is_empty() {
        return loud;
    }
    let resolver = SourceResolver::new(root_file_id, path.display().to_string(), root_source);
    eprint!("{}", render_diagnostics(&loud, &resolver));
    loud
}

/// Surface loud, root-origin preprocessor diagnostics from the shared collector
/// to stderr via [`render_diagnostics`].
///
/// The collector already filtered to the loud set and dropped include-origin
/// diagnostics (R8), so every entry here is root-relative (root [`FileId`] is
/// [`ROOT_FILE_ID`]) and gets a concrete position + snippet.
///
/// The machine-readable side is built separately by each caller from the same
/// `collected` set, because the two clients wrap the rows differently: `check`
/// walks a tree and must attribute each row to its file, while `analyze` is
/// single-file and its envelope names the path once.
fn surface_collected_preproc(path: &Path, source: &str, collected: &CollectedDiagnostics) {
    let loud: Vec<Diagnostic> = collected
        .by_source(oxabl_analyze::DiagnosticSource::Preproc)
        .map(|c| c.diagnostic.clone())
        .collect();
    if loud.is_empty() {
        return;
    }
    let resolver = SourceResolver::new(ROOT_FILE_ID, path.display().to_string(), source);
    eprint!("{}", render_diagnostics(&loud, &resolver));
}

/// Report how many symbols the count-gated lint rules could not fully judge,
/// and return the count for the machine-readable channel.
///
/// A file where `unused-variable`, `assigned-but-never-read` and
/// `block-var-used-outside` went partly blind should say so rather than looking
/// clean. This follows the `PREPROC007` precedent: one honest line at the true
/// cause, printed to stderr, rather than a per-site flood or a silent gap.
///
/// Deliberately *not* a diagnostic with a code — it is not a finding about the
/// source, it is a statement about coverage. And deliberately silent at zero: a
/// line that always appears is a line users learn to skip.
fn surface_unjudged_symbols(sem: &oxabl_semantic::Semantic) -> usize {
    let n = oxabl_analyze::unjudged_symbol_count(sem);
    if n > 0 {
        let plural = if n == 1 { "symbol" } else { "symbols" };
        eprintln!(
            "note: {n} {plural} could not be fully checked — {} named inside statement forms \
             oxabl recognizes but does not model, so the unused-variable, dead-store and \
             block-variable rules stayed silent for {}.",
            if n == 1 { "it is" } else { "they are" },
            if n == 1 { "it" } else { "them" },
        );
    }
    n
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli {
        Cli::Check {
            path,
            json,
            no_preprocess,
            // Accepted and ignored — preprocessing is the default now.
            preprocess: _,
            include_paths,
            schema,
            no_lint,
            no_format,
        } => run_check(
            &path,
            json,
            // On by default (R19): the language server always preprocesses, and a
            // gate that does not would report every include-declared symbol as
            // undefined.
            !no_preprocess,
            &include_paths,
            schema.as_deref(),
            no_lint,
            no_format,
        ),
        Cli::Conformance {
            path,
            json,
            preprocess,
            include_paths,
            debug,
        } => run_conformance(&path, json, preprocess, &include_paths, debug),
        Cli::Analyze {
            path,
            format,
            no_lint,
            preprocess,
            include_paths,
            schema,
        } => run_analyze(
            &path,
            &format,
            no_lint,
            preprocess,
            &include_paths,
            schema.as_deref(),
        ),
        Cli::Format {
            path,
            check,
            stdout,
            style,
        } => run_format(&path, check, stdout, style.as_deref()),
        Cli::Lsp => run_lsp(),
        Cli::Daemon { workspace_root } => run_daemon(&workspace_root),
        Cli::Schema => run_schema(),
    }
}

/// `oxabl schema`: serialize the `oxabl.toml` JSON Schema (derived from
/// `WorkspaceConfig` via `schemars`) to stdout. The build step for the VS Code
/// extension consumes this into `clients/vscode/schemas/oxabl.schema.json`.
fn run_schema() -> ExitCode {
    let schema = schemars::schema_for!(oxabl_workspace::WorkspaceConfig);
    let mut value = match serde_json::to_value(&schema) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: json serialize: {e}");
            return ExitCode::from(6);
        }
    };
    // Self-document the artifact as generated. `$comment` is a standard JSON
    // Schema keyword (JSON has no comment syntax), so this is the equivalent of
    // the codegen "DO NOT EDIT" header for a JSON artifact.
    if let serde_json::Value::Object(ref mut map) = value {
        map.insert(
            "$comment".to_string(),
            serde_json::Value::String(
                "GENERATED by `oxabl schema` from the oxabl config structs — DO NOT EDIT. \
                 Regenerate with `cargo run -p oxabl -- schema`."
                    .to_string(),
            ),
        );
    }
    match serde_json::to_string_pretty(&value) {
        Ok(s) => {
            println!("{s}");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("error: json serialize: {e}");
            ExitCode::from(6)
        }
    }
}

/// Launch the stdio LSP server. A clean shutdown returns success; any protocol
/// error or an `exit` without a prior `shutdown` maps to a failing exit code.
fn run_lsp() -> ExitCode {
    match oxabl_lsp::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("oxabl lsp: {e}");
            ExitCode::FAILURE
        }
    }
}

/// Launch the shared daemon on the Unix socket registered for `workspace_root`.
///
/// This is an umbrella subcommand, like `oxabl lsp`. It uses the same executable
/// discovery that editor clients already have, so clients do not need to locate a
/// second binary.
#[cfg(unix)]
fn run_daemon(workspace_root: &Path) -> ExitCode {
    let listener = match oxabl_daemon::Listener::bind(workspace_root) {
        Ok(listener) => listener,
        Err(error) => {
            eprintln!("oxabl daemon: {error}");
            return ExitCode::FAILURE;
        }
    };
    let dispatch = std::sync::Arc::new(oxabl_daemon::default_dispatch());
    let host = std::sync::Arc::new(oxabl_daemon::SessionHost::new());
    let serve = std::sync::Arc::new(
        move |connection: &lsp_server::Connection, host: &oxabl_daemon::SessionHost| {
            let Ok(first) = connection.receiver.recv() else {
                return;
            };
            match first {
                lsp_server::Message::Request(request) if request.method == "initialize" => {
                    if let Err(error) = oxabl_lsp::serve_with_first(
                        connection,
                        oxabl_lsp::debounce::DEFAULT_WINDOW,
                        request,
                        host.clone(),
                    ) {
                        eprintln!("oxabl daemon: LSP client failed: {error}");
                    }
                }
                first => {
                    oxabl_daemon::serve_with_first(connection, &dispatch, host, first);
                }
            }
        },
    );
    match listener.accept_loop_with(serve, host) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("oxabl daemon: {error}");
            ExitCode::FAILURE
        }
    }
}

#[cfg(not(unix))]
fn run_daemon(_workspace_root: &Path) -> ExitCode {
    eprintln!("oxabl daemon: Unix domain sockets are not supported on this platform");
    ExitCode::FAILURE
}

/// Resolve a `--style` value to a [`StyleGuide`] (KTD2): a known preset name
/// first, otherwise a path to a `.toml` file. Anything that is neither is a
/// hard usage error — an unresolvable style never silently falls back.
fn resolve_style_arg(value: &str) -> Result<StyleGuide, String> {
    if let Some(guide) = StyleGuide::from_preset_name(value) {
        return Ok(guide);
    }
    let content = std::fs::read_to_string(value).map_err(|e| {
        format!("--style `{value}` is not a known preset (oestandards, consultingwerk) and cannot be read as a file: {e}")
    })?;
    StyleGuide::from_toml(&content)
        .map_err(|e| format!("--style `{value}`: invalid style TOML: {e}"))
}

/// `oxabl format`: resolve configuration once, then render the shared
/// [`FormatPipeline`]'s decision for each discovered file per the selected mode.
///
/// This function formats nothing itself (R12): it reads bytes, hands them to the
/// pipeline, and acts on the [`FormatOutcome`](oxabl_pipeline::FormatOutcome) —
/// the same handle `oxabl check` drives for its drift channel, which is what
/// stops the two from disagreeing about whether a file conforms.
///
/// # The per-mode exit-code contract
///
/// Three rules, unchanged:
///
/// * **`--check` exits 1 if any file would change**, and writes nothing. Drift is
///   the *only* thing that mode reports, so this is its whole product.
/// * **A did-not-format outcome is neutral in every mode.** The formatter
///   declining is expected behavior on some inputs (parse errors above all), not a
///   failure of the command: the file is left byte-for-byte unchanged, the reason
///   goes to stderr, and the exit code does not move. This is deliberately unlike
///   `check`, which treats a *contained panic* — an oxabl bug rather than a
///   property of the input — as a failure; here even that stays neutral, because
///   `format`'s job is to leave unformattable files alone and one such file must
///   never fail a batch reformat.
/// * **Only [`Reformatted`](oxabl_pipeline::FormatOutcome::Reformatted) writes.**
///   No other arm carries bytes, so no arm can leave a half-written file.
///
/// I/O failure — a file that cannot be read or written — is the one non-usage
/// failure, and it counts in both modes.
fn run_format(path: &Path, check: bool, stdout: bool, style: Option<&str>) -> ExitCode {
    // Resolve the style once, up front (KTD1/KTD2). An unresolvable --style is a
    // usage error (exit 2) before any file is touched.
    let cli_style = match style {
        Some(s) => match resolve_style_arg(s) {
            Ok(guide) => Some(guide),
            Err(msg) => {
                eprintln!("error: {msg}");
                return ExitCode::from(2);
            }
        },
        None => None,
    };

    // Style is the *only* surface this command has (D2), so the resolution is the
    // schema-free one: a `.df` that would not load is a problem `format` cannot
    // act on, and printing a `warning: schema:` line about it — on the command
    // most likely to be wired to save-on-write — is noise, not information.
    //
    // `--style` names a whole guide, so it leaves nothing in `oxabl.toml` for the
    // run to need: it short-circuits discovery entirely rather than resolving a
    // config only to override it. That is what makes `--style` work in a tree
    // whose `oxabl.toml` cannot be parsed. Without it, a malformed config still
    // degrades to defaults with one `warning:` line (R7) — `format` rewrites
    // layout rather than answering a pass/fail question, so unlike `check` (A3) it
    // has nothing to report wrongly.
    let (config, warnings) = match cli_style {
        Some(style) => (
            PipelineConfig {
                style,
                ..PipelineConfig::default()
            },
            Vec::new(),
        ),
        None => PipelineConfig::resolve_style_only(path, &ConfigOverrides::default()),
    };
    for warning in &warnings {
        eprintln!("warning: {warning}");
    }

    // Discovery through the shared walker (exit 2 on path-not-found / no ABL
    // files, matching `check`).
    let files = match discover_path(path) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };
    if files.is_empty() {
        eprintln!("error: no ABL files found in {}", path.display());
        return ExitCode::from(2);
    }

    // --stdout is single-file only; a directory + --stdout is a usage error.
    if stdout && path.is_dir() {
        eprintln!("error: --stdout requires a single file, not a directory");
        return ExitCode::from(2);
    }

    // Built from the resolved style and nothing else, so it cannot see expanded
    // macro text however this run is configured (R4/KTD4) — the formatter's spans
    // must be real byte offsets into the bytes on disk.
    let pipeline = FormatPipeline::new(config.style.clone());

    let mut any_would_change = false;
    let mut any_io_error = false;

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error: cannot read {}: {e}", file.display());
                any_io_error = true;
                continue;
            }
        };

        let outcome = pipeline.format(&source);
        if let Some(formatted) = outcome.output() {
            if check {
                eprintln!("{}: would reformat", file.display());
                any_would_change = true;
            } else if stdout {
                print!("{formatted}");
            } else if let Err(e) = std::fs::write(file, formatted) {
                eprintln!("error: cannot write {}: {e}", file.display());
                any_io_error = true;
            }
        } else {
            // Unchanged or a refusal: nothing to write in any mode, and --stdout
            // still emits the original bytes so a pipeline never loses the file.
            if let Some(not_formatted) = outcome.not_formatted() {
                // A contained panic keeps its own wording — the reason alone
                // reads as an ordinary refusal, and an oxabl bug should not
                // (R5).
                if not_formatted.is_internal_panic() {
                    eprintln!(
                        "{}: internal panic while formatting; left unchanged ({})",
                        file.display(),
                        not_formatted.reason()
                    );
                } else {
                    eprintln!("{}: {}", file.display(), not_formatted.reason());
                }
            }
            if stdout {
                print!("{source}");
            }
        }
    }

    if check {
        // Exit 0 iff no file would change and none failed to read.
        if any_would_change || any_io_error {
            ExitCode::from(1)
        } else {
            ExitCode::SUCCESS
        }
    } else if any_io_error {
        // Write/stdout: I/O failure is the only non-usage failure.
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

/// `oxabl check`'s machine-readable report — **two findings keys**, not one
/// merged array (KTD7).
///
/// `diagnostics` is span-anchored; `format` is a per-file boolean rolled up into
/// a path list plus its total. Merging them would mean synthesizing a span for a
/// fact that has none, so they stay apart here exactly as they do in the text
/// output.
///
/// `version` is bumped when a key's meaning changes, so a consumer can pin. The
/// remaining keys are deliberately *not* findings:
///
/// * `preproc` — the loud `PREPROC007`-family coverage warnings, same channel
///   and same key name `analyze`'s envelope gives them. They never move the exit
///   code. Rows carry a `path` like the findings do (D1): `check` walks a tree,
///   and without one, N files missing the same include produced N entries a
///   consumer could not tell apart.
/// * `unjudged_symbols` — the count-gated rules' coverage note (R26). Also never
///   moves the exit code.
/// * `failures` — per-file internal failures (an unreadable file, or a contained
///   panic). These *do* move the exit code to 1 (R24), and they get their own key
///   rather than a fabricated diagnostic: without it a contained panic would be
///   machine-indistinguishable from an unused variable, which would undo the
///   loud-diagnosis posture the panic guards exist for.
#[derive(Serialize)]
struct CheckJsonReport {
    version: u32,
    files_checked: usize,
    /// Whether lint findings are reported (`false` under `--no-lint`), so an
    /// empty `diagnostics` cannot be misread as "clean". The pipeline itself runs
    /// either way — parse and semantic diagnostics are never suppressed (A1).
    lint_enabled: bool,
    diagnostics: Vec<CheckJsonDiagnostic>,
    /// Whether the format channel ran at all (`false` under `--no-format`).
    format_enabled: bool,
    format: CheckJsonFormat,
    preproc: Vec<CheckJsonDiagnostic>,
    unjudged_symbols: usize,
    failures: Vec<CheckJsonFailure>,
}

/// One lint finding, carrying the pipeline's **byte** span and, when the span is
/// root-origin, the derived line/column.
///
/// Both representations are present because they answer different questions and
/// have different owners: `span` is the pipeline's own coordinate space (KTD5),
/// which is what a cross-client parity test must compare, while `start`/`end`
/// are this client's rendering of it through the shared position helper (R13).
/// A diagnostic belonging to an *include* file has no honest position in the
/// root buffer, so those two are omitted rather than fabricated — the same rule
/// the text renderer applies.
#[derive(Serialize)]
struct CheckJsonDiagnostic {
    path: String,
    code: String,
    severity: &'static str,
    source: &'static str,
    message: String,
    /// The remediation line, when the finding carries one. Present since
    /// version 3: `undefined-symbol` reporting a name absent from the configured
    /// search paths puts the whole fix in its help line, and a JSON consumer that
    /// could not see it would be reading half a diagnostic.
    #[serde(skip_serializing_if = "Option::is_none")]
    help: Option<String>,
    span: CheckJsonSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<CheckJsonPosition>,
    #[serde(skip_serializing_if = "Option::is_none")]
    end: Option<CheckJsonPosition>,
}

#[derive(Serialize)]
struct CheckJsonSpan {
    start: u32,
    end: u32,
}

#[derive(Serialize)]
struct CheckJsonPosition {
    byte: u32,
    line: usize,
    column: usize,
}

impl From<position::Position> for CheckJsonPosition {
    fn from(p: position::Position) -> Self {
        CheckJsonPosition {
            byte: p.byte,
            line: p.line,
            column: p.column,
        }
    }
}

/// The format channel: the drifting paths, with the count as a trailing total
/// (R14). A count alone would force a second `format --check` run just to learn
/// *which* files, which is the friction a single gate exists to remove.
#[derive(Serialize)]
struct CheckJsonFormat {
    drifted: Vec<String>,
    drifted_count: usize,
}

/// A per-file internal failure: the walk reported it and kept going (R24).
#[derive(Serialize)]
struct CheckJsonFailure {
    path: String,
    reason: String,
}

/// `oxabl check`: lint every discovered file and report formatting drift, in two
/// channels (R14, KTD7).
///
/// Everything here is a *render* of the shared pipelines — this function
/// tokenizes nothing, parses nothing, and collects nothing itself (R12). File
/// discovery goes through the shared walker, configuration is resolved **once**
/// up front rather than per file, and the two pipelines are stood up once and
/// reused across the walk.
///
/// # Streams
///
/// Findings — the command's product — go to **stdout**, and `--json` replaces
/// them there with the report document. The two coverage channels
/// (`PREPROC007`-family preprocessor warnings, and the unjudged-symbol note) go
/// to **stderr** in both modes, which is exactly how `analyze` splits them: they
/// qualify the run rather than being results of it.
///
/// A run with nothing to report prints a one-line summary naming the file count
/// instead of nothing at all (D5), so a pass is distinguishable from a path that
/// silently matched almost nothing. Text mode only — `--json` carries the same
/// count as `files_checked`.
///
/// # A per-file failure never aborts the walk (R24)
///
/// A file that cannot be read, or whose analysis hits a contained panic, is
/// reported against that path and the walk continues to the remaining files —
/// the same posture `format` already takes. It counts toward exit 1 and never
/// toward `analyze`'s exit 4: `analyze` is single-file, so aborting is right
/// there and wrong here.
///
/// The lint run therefore goes through [`LintPipeline::run`], the *guarded*
/// composition, rather than the raw `expand`/`collect` phases. Those two are
/// unguarded on purpose so the language server's cancellation can travel as an
/// unwind; a non-incremental caller like this one wants the guard, and it is
/// what makes "report and continue" possible at all.
fn run_check(
    path: &Path,
    json_output: bool,
    preprocess: bool,
    include_paths: &[PathBuf],
    schema_path: Option<&Path>,
    no_lint: bool,
    no_format: bool,
) -> ExitCode {
    // Discovery through the shared walker: a named file is checked regardless of
    // extension, a directory is walked under the root-extension policy. A
    // missing path and an empty tree are distinct facts but the same usage
    // error (exit 2).
    let files = match discover_path(path) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };
    if files.is_empty() {
        eprintln!("error: no ABL files found in {}", path.display());
        return ExitCode::from(2);
    }

    // Config once, not once per file: a resolution reads `oxabl.toml` and every
    // `.df` it names, so doing it inside the loop would re-parse the schema for
    // every file in the tree.
    let overrides = ConfigOverrides {
        include_paths: include_paths.to_vec(),
        schema_path: schema_path.map(Path::to_path_buf),
        style: None,
    };
    let (config, warnings) = PipelineConfig::resolve(path, &overrides);

    // The one place the gate hardens where every other command degrades (A3): an
    // `oxabl.toml` the resolver could not use is a **config error**, exit 2.
    //
    // `ConfigWarning::Config` means the whole file was discarded, so the run would
    // proceed on defaults — and defaults are not what the user wrote. A dropped
    // `[workspace.lint]` table takes every `off` with it, so the gate reports
    // findings for rules the project switched off, and reports them under a green
    // exit code if nothing else fires. `format`, `analyze`, `conformance` and the
    // LSP keep degrading with a warning: none of them answers a pass/fail
    // question, so none of them can answer it wrongly.
    //
    // "Could not use" is broader than "malformed TOML": `LintConfig` and
    // `StyleGuide` deny unknown fields, so a syntactically valid file naming a key
    // oxabl does not know — a typo'd or not-yet-released rule name — lands here
    // too. That is the intended answer, for the same reason: the gate would
    // otherwise run on a configuration the user never wrote.
    //
    // Every other warning stays a warning. A `.df` that would not load, or a
    // `--schema` directory that matched nothing (A2), leaves the rest of the
    // configuration intact, so the run still means what it says.
    if let Some(msg) = warnings.iter().find_map(|w| match w {
        ConfigWarning::Config(msg) => Some(msg),
        _ => None,
    }) {
        eprintln!("error: {msg}");
        return ExitCode::from(2);
    }
    for warning in &warnings {
        eprintln!("warning: {warning}");
    }

    // Absolute spellings of the discovered files, for the index alone. The
    // display paths below stay exactly as the user typed them; these exist
    // because the index keys files by path, and a name lookup spells a candidate
    // by joining an *absolutized* include path — so a walk that handed over
    // `./src/a.cls` would look like a second, different file from the one a class
    // lookup finds, splitting one file into two identities.
    let indexed: Vec<PathBuf> = files
        .iter()
        .map(|f| std::path::absolute(f).unwrap_or_else(|_| f.clone()))
        .collect();

    let fs = RealFileSystem;
    // One run handle, so one cross-file index spans the whole walk: files sharing
    // a parent class read it once, not once each. The per-file handle below is a
    // pointer copy off this one.
    let lint = LintPipeline::new(&config, &fs)
        .with_preprocess(preprocess)
        // The walk's own file list — no directory scan — which is what lets a
        // `DEFINE SHARED` consumer find its producer even when no `RUN` names the
        // producing file.
        .with_known_files(&indexed);
    // The format pipeline is built from the resolved style and nothing else, so
    // it *cannot* see expanded macro text however this walk is configured (R4).
    let format = FormatPipeline::new(config.style.clone());

    let mut diagnostics: Vec<CheckJsonDiagnostic> = Vec::new();
    let mut preproc: Vec<CheckJsonDiagnostic> = Vec::new();
    let mut failures: Vec<CheckJsonFailure> = Vec::new();
    let mut drifted: Vec<String> = Vec::new();
    let mut unjudged = 0usize;

    for (file, indexed_path) in files.iter().zip(&indexed) {
        let display = file.display().to_string();
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                // Report and continue (R24): one unreadable file must not cost
                // the caller every other file's findings.
                eprintln!("error: cannot read {display}: {e}");
                failures.push(CheckJsonFailure {
                    path: display,
                    reason: format!("cannot read: {e}"),
                });
                continue;
            }
        };

        // The pipeline runs whatever `--no-lint` says (A1): it is the only source
        // of parse and semantic diagnostics too, and those gate regardless.
        //
        // Identity, so this file is excluded from its own cross-file lookups:
        // the bytes just read are what is being analysed, and resolving a name
        // to the same file on disk would attribute it to itself as a foreign
        // dependency.
        let result = lint.with_file(indexed_path).run(&source);
        if let Some(panic) = result.failure() {
            eprintln!("error: analysis failed on {display}: {panic}");
            failures.push(CheckJsonFailure {
                path: display.clone(),
                reason: panic.to_string(),
            });
        } else {
            // Preprocessor diagnostics keep their own channel — surfaced by the
            // same helper `analyze` uses, so the two cannot drift. They are
            // coverage warnings, not findings, and never move the exit code: an
            // elided include still parses.
            surface_collected_preproc(file, &source, result.diagnostics());
            // Machine-readable, through the *same* row builder the findings use
            // (D1): an entry names its file, so two files losing coverage on
            // different includes are two distinguishable entries.
            //
            // Only `--json` reads these rows, and most files have no preproc
            // diagnostic at all — so the `SourceMap` (a scan of the whole file) is
            // built behind both conditions, the way the findings rows below build
            // theirs only once there is a finding.
            if json_output {
                let mut collected = result
                    .by_source(oxabl_analyze::DiagnosticSource::Preproc)
                    .peekable();
                if collected.peek().is_some() {
                    let map = SourceMap::new(&source);
                    preproc.extend(collected.map(|c| check_json_diagnostic(&display, &map, c)));
                }
            }

            // `--no-lint` is a **filter on the reported set**, exactly as it is
            // for `analyze`: the lint-sourced entries go, the parse and semantic
            // ones stay. Two chained calls to the one shared filter rather than a
            // second hand-rolled predicate.
            let reported = result.excluding_source(oxabl_analyze::DiagnosticSource::Preproc);
            let reported = if no_lint {
                reported.excluding_source(oxabl_analyze::DiagnosticSource::Lint)
            } else {
                reported
            };
            if !reported.diagnostics.is_empty() {
                if !json_output {
                    let resolver =
                        SourceResolver::new(ROOT_FILE_ID, display.clone(), source.as_str());
                    let rendered: Vec<Diagnostic> =
                        reported.all().map(|c| c.diagnostic.clone()).collect();
                    print!("{}", render_diagnostics(&rendered, &resolver));
                }
                let map = SourceMap::new(&source);
                for collected in reported.all() {
                    diagnostics.push(check_json_diagnostic(&display, &map, collected));
                }
            }

            // The unjudged-symbol note is a statement about the *count-gated lint
            // rules'* coverage, so it has nothing to qualify when those findings
            // are suppressed.
            if !no_lint && let Some(sem) = result.semantic() {
                unjudged += surface_unjudged_symbols(sem);
            }
        }

        if !no_format {
            // The same `FormatPipeline` `oxabl format` drives, so `check`'s
            // drift answer and `format --check`'s cannot disagree (KTD7).
            let outcome = format.format(&source);
            if outcome.changed() {
                drifted.push(display.clone());
            } else if let Some(not_formatted) = outcome.not_formatted() {
                if not_formatted.is_internal_panic() {
                    // An oxabl bug, not a property of the input (R5): worth a
                    // failure entry and exit 1.
                    eprintln!(
                        "error: formatting failed on {display}: {}",
                        not_formatted.reason()
                    );
                    failures.push(CheckJsonFailure {
                        path: display,
                        reason: not_formatted.reason(),
                    });
                } else {
                    // A deliberate bail is expected behavior on some inputs and
                    // is *not* drift, so it stays neutral — the same call
                    // `format --check` makes.
                    eprintln!("{display}: {}", not_formatted.reason());
                }
            }
        }
    }

    if json_output {
        let report = CheckJsonReport {
            // 2: `preproc_diagnostics` became `preproc`, matching `analyze`'s
            // envelope, and its rows gained a `path` (D1).
            // 3: a diagnostic row carries its `help` line when it has one.
            version: 3,
            files_checked: files.len(),
            lint_enabled: !no_lint,
            diagnostics,
            format_enabled: !no_format,
            format: CheckJsonFormat {
                drifted_count: drifted.len(),
                drifted,
            },
            preproc,
            unjudged_symbols: unjudged,
            failures,
        };
        match serde_json::to_string_pretty(&report) {
            Ok(s) => println!("{s}"),
            Err(e) => {
                eprintln!("error: json serialize: {e}");
                return ExitCode::from(6);
            }
        }
        // The report owns every count now, so recompute the exit from it.
        return check_exit_code(
            report.diagnostics.len(),
            report.format.drifted_count,
            report.failures.len(),
        );
    }

    // The drift channel: the paths first, the count as a trailing total (R14),
    // pointing at the command that fixes them.
    if !drifted.is_empty() {
        println!();
        println!("Files that would be reformatted:");
        for file in &drifted {
            println!("  {file}");
        }
        let plural = if drifted.len() == 1 { "file" } else { "files" };
        println!(
            "{} {plural} would be reformatted — run `oxabl format` to fix.",
            drifted.len()
        );
    }

    // A passing run says what it checked (D5). Silence on success reads the same
    // as silence on a mistyped path that happened to resolve to one clean file,
    // and a green light that might mean "I checked nothing you cared about" is not
    // one CI can trust. Printed only when the gate actually passed, so it is
    // unambiguously the pass message rather than a header over a list of problems.
    //
    // Text mode only: `--json` already carries `files_checked`, and a prose line
    // beside the document would make stdout unparseable.
    if diagnostics.is_empty() && drifted.is_empty() && failures.is_empty() {
        let plural = if files.len() == 1 { "file" } else { "files" };
        let lint_channel = if no_lint {
            "lint suppressed"
        } else {
            "no findings"
        };
        let format_channel = if no_format {
            "format suppressed"
        } else {
            "no drift"
        };
        println!(
            "checked {} {plural}: {lint_channel}, {format_channel}",
            files.len()
        );
    }

    check_exit_code(diagnostics.len(), drifted.len(), failures.len())
}

/// `check`'s exit code (R15): `1` when either channel produced something or a
/// file failed, `0` otherwise.
///
/// The coverage note is deliberately *not* an input here (R26): "this file was
/// partly unjudgeable" is not "this file has a problem", and a gate that failed
/// on it would punish the honesty signal.
fn check_exit_code(findings: usize, drifted: usize, failures: usize) -> ExitCode {
    if findings > 0 || drifted > 0 || failures > 0 {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

/// Render one collected diagnostic into `check`'s JSON entry, deriving line and
/// column through the shared position helper (R13) rather than a second
/// hand-rolled `SourceMap::lookup` pair.
///
/// An include-origin span resolves against the wrong text, so its position is
/// omitted rather than fabricated — the byte span still travels.
fn check_json_diagnostic(
    path: &str,
    map: &SourceMap,
    collected: &oxabl_analyze::CollectedDiagnostic,
) -> CheckJsonDiagnostic {
    let d = &collected.diagnostic;
    let resolved = (d.span.file == ROOT_FILE_ID).then(|| position::resolve_diagnostic(map, d));
    CheckJsonDiagnostic {
        path: path.to_string(),
        code: d.code.0.to_string(),
        severity: d.severity.as_str(),
        source: collected.source.as_str(),
        message: d.message.clone(),
        help: d.help.clone(),
        span: CheckJsonSpan {
            start: d.span.span.start,
            end: d.span.span.end,
        },
        start: resolved.map(|r| r.start.into()),
        end: resolved.map(|r| r.end.into()),
    }
}

/// `oxabl analyze`: dump one file's resolved semantic model, in the versioned
/// envelope or as text.
///
/// Like `check`, this orchestrates nothing itself (R12) — it resolves
/// configuration once, drives [`LintPipeline`], and renders. Unlike `check` it is
/// single-file, and it **exits 0 whatever it finds** (KTD9): introspection, not a
/// gate. See the subcommand's own docs for the full exit-code list.
///
/// # Two channels, and why the envelope no longer omits one
///
/// The preprocessor coverage warnings and the unjudged-symbol note still go to
/// **stderr** — they qualify the run rather than being results of it, and a human
/// reading a piped dump needs them out of the way of the document. They are also
/// sections of the document now (`preproc` and `coverage`), emitted by the library
/// like every other section. Before, the CLI spliced both keys into the finished
/// JSON, which made them invisible to `--format text` and unversioned; the stderr
/// lines are the human channel and were never a substitute for that.
fn run_analyze(
    path: &Path,
    format: &str,
    no_lint: bool,
    preprocess: bool,
    include_paths: &[PathBuf],
    schema_path: Option<&Path>,
) -> ExitCode {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read {}: {e}", path.display());
            return ExitCode::from(2);
        }
    };

    // One resolution for every configured surface — include paths, lint
    // severities, and the schema — instead of the three independent `resolved_*`
    // calls this used to make, which parsed `oxabl.toml` once each and could each
    // report the same malformed file. Warnings are non-fatal data (R7): an
    // unreadable `.df` and a broken config both degrade to a default and say so
    // without moving the exit code.
    let overrides = ConfigOverrides {
        include_paths: include_paths.to_vec(),
        schema_path: schema_path.map(Path::to_path_buf),
        style: None,
    };
    let (config, warnings) = PipelineConfig::resolve(path, &overrides);
    for warning in &warnings {
        eprintln!("warning: {warning}");
    }

    let fs = RealFileSystem;
    // Guarded (R20): a panic in the shared pipeline is reported as this file's
    // failure, not an unwind out of the subcommand with a raw backtrace. Aborting
    // is right here — there is exactly one file — which is why `check`'s
    // continue-the-walk rule (R24) is `check`'s alone.
    let run = LintPipeline::new(&config, &fs).with_preprocess(preprocess);
    // The analysed file's identity, absolutized for the same reason `check`'s is:
    // it must be spelled the way a name lookup spells a candidate, or the file
    // fails to exclude itself.
    let analysed = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    let file_run = run.with_file(analysed);
    // The two phases by hand rather than `run`, so the dependency section is built
    // from *this* expansion and model instead of costing a second analysis. The
    // guard `run` would have applied is applied here.
    let phases = oxabl_common::catch_panic(|| {
        let expansion = file_run.expand(&source);
        let collected = file_run.collect(&expansion);
        (expansion, collected)
    });
    let (expansion, result) = match phases {
        Ok(phases) => phases,
        Err(panic) => {
            eprintln!("error: analysis failed on {}: {panic}", path.display());
            return ExitCode::from(4);
        }
    };
    if let Some(panic) = result.failure() {
        eprintln!("error: analysis failed on {}: {panic}", path.display());
        return ExitCode::from(4);
    }

    // `--no-lint` is a **filter on the result**, not a skipped run: the envelope
    // still wants the model, so the shared `excluding_source` drops the
    // lint-sourced entries and leaves parse, semantic, and preprocessor ones
    // alone. `check --no-lint` means the same thing by the same mechanism (A1): it
    // also runs the pipeline — the only source of the parse and semantic
    // diagnostics that gate regardless — and filters the reported set.
    let collected = if no_lint {
        result.excluding_source(oxabl_analyze::DiagnosticSource::Lint)
    } else {
        result.diagnostics().clone()
    };

    let sem = match result.semantic() {
        Some(sem) => sem,
        None => {
            // Fatal preprocessing failure: no model to dump. Not reachable from
            // any ABL input today — the preprocessor only fails fatally when it
            // emits an error *and* produces an empty span tree — but the arm is
            // real and reported distinctly rather than collapsed into success.
            let msg = collected
                .by_source(oxabl_analyze::DiagnosticSource::Preproc)
                .next()
                .map(|c| c.diagnostic.message.as_str())
                .unwrap_or("unknown");
            eprintln!("error: preprocessing failed: {msg}");
            return ExitCode::from(3);
        }
    };

    // The human channel, unchanged: loud unresolvable-include / preprocessor
    // diagnostics rendered with a position and a snippet, and the coverage note
    // saying how much of the file the count-gated rules could not judge. The
    // envelope carries both facts as well; neither replaces the other.
    surface_collected_preproc(path, &source, &collected);
    surface_unjudged_symbols(sem);

    // The typed dependency edges for this file. An edge set the analysis could not
    // produce yields an empty section rather than a missing key: `index revision:
    // 0` is how the document says nothing was looked at.
    let dependencies = file_run
        .edges_of(&expansion, &result)
        .map(|edges| oxabl_pipeline::dependency_section(&edges, sem.index_revision.raw()))
        .unwrap_or_default();

    match format {
        "json" => {
            let v = dump_json_with_diagnostics(sem, &collected, &dependencies);
            match serde_json::to_string_pretty(&v) {
                Ok(s) => println!("{s}"),
                Err(e) => {
                    eprintln!("error: json serialize: {e}");
                    return ExitCode::from(6);
                }
            }
        }
        "text" => {
            print!(
                "{}",
                dump_text_with_diagnostics(sem, &collected, &dependencies)
            );
        }
        other => {
            eprintln!("error: unsupported format `{other}` (use `json` or `text`)");
            return ExitCode::from(7);
        }
    }

    ExitCode::SUCCESS
}

/// The parse-conformance walk: discover ABL roots under `path`, parse each one
/// (optionally through the preprocessor), and render the pass/fail report.
///
/// Exit codes: `0` when every file parsed, `1` when any file failed (parse
/// error, I/O error, or a contained lexer panic), `2` for a usage problem — a
/// path that does not exist or a directory with no ABL roots in it. `--json`
/// adds `6` for a serialization failure, matching `analyze`'s contract.
fn run_conformance(
    path: &Path,
    json_output: bool,
    preprocess: bool,
    include_paths: &[PathBuf],
    debug: bool,
) -> ExitCode {
    // Discovery through the shared walker, the same one `check` and `format` use:
    // an explicitly named file is accepted whatever its extension, a directory is
    // walked under the root-extension policy. That is exactly what this walk
    // wants — it accepts a single named root the same way — so it has no reason to
    // keep a private copy.
    let files = match discover_path(path) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("Error: {e}");
            return ExitCode::from(2);
        }
    };

    if files.is_empty() {
        eprintln!("No ABL files found in {}", path.display());
        return ExitCode::from(2);
    }

    // Merge CLI `-I` flags with any auto-discovered `oxabl.toml` include paths —
    // through the shared resolution (D3), which is where that derivation now lives
    // for every client. `oxabl_workspace::resolved_include_paths` reimplemented it
    // line-for-line, so the two could disagree about PROPATH order or anchoring
    // while looking identical; it is gone, and this was its last caller.
    //
    // The schema-free resolution because a parse-conformance walk has no use for a
    // `.df`, and a malformed config degrades to flags-only with one warning, as
    // before: this command reports what oxabl can parse, not whether the caller's
    // configuration is right.
    let effective_paths: Vec<PathBuf> = if preprocess {
        let overrides = ConfigOverrides {
            include_paths: include_paths.to_vec(),
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve_style_only(path, &overrides);
        for warning in &warnings {
            eprintln!("warning: {warning}");
        }
        config.include_paths
    } else {
        Vec::new()
    };

    if !json_output {
        eprintln!("Found {} ABL files", files.len());
        if preprocess {
            eprintln!(
                "Preprocessing enabled with {} include path(s)",
                effective_paths.len()
            );
        }
    }

    // Debug mode: use parse_program() with error recovery and show AST context
    if debug {
        let real_fs = RealFileSystem;
        for file in &files {
            run_debug_parse(file, preprocess, &real_fs, &effective_paths);
        }
        return ExitCode::SUCCESS;
    }

    // Set up progress bar
    let progress = ProgressBar::new(files.len() as u64);
    if json_output {
        progress.set_draw_target(indicatif::ProgressDrawTarget::hidden());
    } else {
        progress.set_style(
            ProgressStyle::default_bar()
                .template("[{bar:40}] {pos}/{len}")
                .expect("valid progress template")
                .progress_chars("=> "),
        );
    }

    // Set up filesystem for preprocessing
    let real_fs = RealFileSystem;

    // Parse all files
    let start = Instant::now();
    let mut results = Vec::with_capacity(files.len());

    for file in &files {
        if preprocess {
            results.push(parse_file_with_preprocess(file, &real_fs, &effective_paths));
        } else {
            results.push(parse_file(file));
        }
        progress.inc(1);
    }

    progress.finish_and_clear();
    let elapsed = start.elapsed();

    // Render report
    if json_output {
        // A serialization failure is reported and propagated, not panicked on —
        // same exit code (6) `analyze` uses for the same failure.
        if let Err(code) = render_json_report(&results, elapsed.as_secs_f64()) {
            return code;
        }
    } else {
        render_human_report(&results, elapsed.as_secs_f64());
    }

    // Exit code
    let has_failures = results.iter().any(|r| !matches!(r, FileResult::Success));
    if has_failures {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn parse_file(path: &Path) -> FileResult {
    // Read the file
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            return FileResult::IoError {
                path: path.to_path_buf(),
                error: e.to_string(),
            };
        }
    };

    // Parse through the shared guard. `oxabl::try_parse` folds tokenize + parser
    // construction together; a panic anywhere in it is reported as a lexer
    // panic (the historical failure mode) rather than unwinding the walk.
    let program = match oxabl::try_parse(&source) {
        Ok(program) => program,
        Err(_) => {
            return FileResult::LexerPanic {
                path: path.to_path_buf(),
            };
        }
    };

    // Fail-fast reporting: surface the first recovered error, matching the
    // previous `parse_statements` contract.
    match program.first_error() {
        None => FileResult::Success,
        Some(e) => {
            let source_map = SourceMap::new(&source);
            let (line, col) = source_map.lookup(e.span.start as usize);
            FileResult::ParseError {
                path: path.to_path_buf(),
                line,
                col,
                message: e.message.clone(),
            }
        }
    }
}

fn parse_file_with_preprocess(
    path: &Path,
    fs: &RealFileSystem,
    include_paths: &[PathBuf],
) -> FileResult {
    // Read the file
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            return FileResult::IoError {
                path: path.to_path_buf(),
                error: e.to_string(),
            };
        }
    };

    // Preprocess
    let preprocessor = Preprocessor::new(fs, include_paths);
    let file_id = ROOT_FILE_ID;
    let preprocessed = match preprocessor.process(file_id, &source) {
        Ok(pf) => pf,
        Err(diags) => {
            // Use the first error diagnostic as the failure message
            let msg = diags
                .first()
                .map(|d| d.message.clone())
                .unwrap_or_else(|| "preprocessing failed".to_string());
            let source_map = SourceMap::new(&source);
            let span_start = diags
                .first()
                .map(|d| d.span.span.start as usize)
                .unwrap_or(0);
            let (line, col) = source_map.lookup(span_start);
            return FileResult::ParseError {
                path: path.to_path_buf(),
                line,
                col,
                message: format!("[preprocess] {msg}"),
            };
        }
    };

    // Surface loud preprocessor diagnostics — errors plus PREPROC007
    // (unresolvable include) and PREPROC002 (unclosed &IF) — so true
    // preproc problems aren't hidden. Other warnings stay quiet.
    surface_preproc_diagnostics(path, &source, file_id, &preprocessed.diagnostics);

    // Get the expanded source text
    let expanded = preprocessed.to_text();

    // Parse the preprocessed source through the shared guard (see `parse_file`).
    let program = match oxabl::try_parse(&expanded) {
        Ok(program) => program,
        Err(_) => {
            return FileResult::LexerPanic {
                path: path.to_path_buf(),
            };
        }
    };

    // Fail-fast reporting: surface the first recovered error.
    match program.first_error() {
        None => FileResult::Success,
        Some(e) => {
            // Resolve the virtual offset back to the real source location
            let real_span = preprocessed.resolve(e.span.start);
            let real_file = real_span.file;

            // If the error is in the original file, use the original source for line/col
            if real_file == file_id {
                let source_map = SourceMap::new(&source);
                let (line, col) = source_map.lookup(real_span.span.start as usize);
                FileResult::ParseError {
                    path: path.to_path_buf(),
                    line,
                    col,
                    message: e.message.clone(),
                }
            } else {
                // Error is inside an included file — use expanded source for position
                let source_map = SourceMap::new(&expanded);
                let (line, col) = source_map.lookup(e.span.start as usize);
                FileResult::ParseError {
                    path: path.to_path_buf(),
                    line,
                    col,
                    message: format!("[in include] {}", e.message),
                }
            }
        }
    }
}

fn render_human_report(results: &[FileResult], elapsed_secs: f64) {
    let total = results.len();
    let mut passed = 0usize;
    let mut failed = 0usize;
    let mut io_errors = 0usize;
    let mut lexer_panics = 0usize;
    let mut failures: Vec<(&PathBuf, usize, usize, &str)> = Vec::new();
    let mut error_counts: HashMap<&str, usize> = HashMap::new();

    for result in results {
        match result {
            FileResult::Success => passed += 1,
            FileResult::ParseError {
                path,
                line,
                col,
                message,
            } => {
                failed += 1;
                failures.push((path, *line, *col, message));
                *error_counts.entry(message.as_str()).or_insert(0) += 1;
            }
            FileResult::IoError { .. } => io_errors += 1,
            FileResult::LexerPanic { .. } => lexer_panics += 1,
        }
    }

    let success_rate = if total > 0 {
        (passed as f64 / total as f64) * 100.0
    } else {
        0.0
    };
    let files_per_sec = if elapsed_secs > 0.0 {
        total as f64 / elapsed_secs
    } else {
        0.0
    };

    // Summary
    println!();
    println!(
        "Results: {} passed, {} failed ({:.1}% success rate)",
        passed, failed, success_rate
    );
    if io_errors > 0 {
        println!("  I/O errors: {io_errors}");
    }
    if lexer_panics > 0 {
        println!("  Lexer panics: {lexer_panics}");
    }

    // Failure list
    if !failures.is_empty() {
        println!();
        println!("Failures:");
        for (path, line, col, message) in &failures {
            println!("  {}:{}:{}  {}", path.display(), line, col, message);
        }
    }

    // I/O errors
    let io_error_list: Vec<_> = results
        .iter()
        .filter_map(|r| match r {
            FileResult::IoError { path, error } => Some((path, error.as_str())),
            _ => None,
        })
        .collect();
    if !io_error_list.is_empty() {
        println!();
        println!("I/O Errors:");
        for (path, error) in &io_error_list {
            println!("  {}  {}", path.display(), error);
        }
    }

    // Lexer panics
    let panic_list: Vec<_> = results
        .iter()
        .filter_map(|r| match r {
            FileResult::LexerPanic { path } => Some(path),
            _ => None,
        })
        .collect();
    if !panic_list.is_empty() {
        println!();
        println!("Lexer Panics:");
        for path in &panic_list {
            println!("  {}", path.display());
        }
    }

    // Top error patterns
    if !error_counts.is_empty() {
        let mut patterns: Vec<_> = error_counts.into_iter().collect();
        patterns.sort_by_key(|p| std::cmp::Reverse(p.1));

        println!();
        println!("Top error patterns:");
        for (pattern, count) in patterns.iter().take(10) {
            println!("  {:>5}  {}", count, pattern);
        }
        if patterns.len() > 10 {
            let other_count: usize = patterns.iter().skip(10).map(|(_, c)| c).sum();
            println!("  {:>5}  (other)", other_count);
        }
    }

    // Timing
    println!();
    println!(
        "Total time: {:.1}s ({:.0} files/sec)",
        elapsed_secs, files_per_sec
    );
}

/// Render the conformance report as JSON on stdout.
///
/// Returns `Err(ExitCode)` rather than panicking when the report will not
/// serialize: the caller propagates it so a serialize failure is an exit code
/// (6, as in `analyze`) instead of a backtrace.
fn render_json_report(results: &[FileResult], elapsed_secs: f64) -> Result<(), ExitCode> {
    let total = results.len();
    let mut passed = 0usize;
    let mut failed = 0usize;
    let mut io_errors = 0usize;
    let mut lexer_panics = 0usize;
    let mut failures = Vec::new();
    let mut error_counts: HashMap<String, usize> = HashMap::new();

    for result in results {
        match result {
            FileResult::Success => passed += 1,
            FileResult::ParseError {
                path,
                line,
                col,
                message,
            } => {
                failed += 1;
                failures.push(JsonFailure {
                    path: path.display().to_string(),
                    line: *line,
                    col: *col,
                    message: message.clone(),
                });
                *error_counts.entry(message.clone()).or_insert(0) += 1;
            }
            FileResult::IoError { .. } => io_errors += 1,
            FileResult::LexerPanic { .. } => lexer_panics += 1,
        }
    }

    let success_rate = if total > 0 {
        ((passed as f64 / total as f64) * 1000.0).round() / 10.0
    } else {
        0.0
    };
    let files_per_sec = if elapsed_secs > 0.0 {
        (total as f64 / elapsed_secs * 10.0).round() / 10.0
    } else {
        0.0
    };

    let mut patterns: Vec<_> = error_counts.into_iter().collect();
    patterns.sort_by_key(|p| std::cmp::Reverse(p.1));

    let report = JsonReport {
        total,
        passed,
        failed,
        io_errors,
        lexer_panics,
        success_rate,
        elapsed_secs: (elapsed_secs * 100.0).round() / 100.0,
        files_per_sec,
        failures,
        error_patterns: patterns
            .into_iter()
            .map(|(pattern, count)| JsonErrorPattern { pattern, count })
            .collect(),
    };

    match serde_json::to_string_pretty(&report) {
        Ok(s) => {
            println!("{s}");
            Ok(())
        }
        Err(e) => {
            eprintln!("error: json serialize: {e}");
            Err(ExitCode::from(6))
        }
    }
}

fn run_debug_parse(path: &Path, preprocess: bool, fs: &RealFileSystem, include_paths: &[PathBuf]) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("I/O error: {} — {}", path.display(), e);
            return;
        }
    };

    let parse_source: String = if preprocess {
        let preprocessor = Preprocessor::new(fs, include_paths);
        let file_id = ROOT_FILE_ID;
        match preprocessor.process(file_id, &source) {
            Ok(pf) => {
                if !pf.diagnostics.is_empty() {
                    let source_map = SourceMap::new(&source);
                    println!("--- Preprocessor diagnostics ---");
                    for d in &pf.diagnostics {
                        let (line, col) = source_map.lookup(d.span.span.start as usize);
                        println!("  {}:{} [preprocess] {}", line, col, d.message);
                    }
                    println!();
                }
                pf.to_text().to_string()
            }
            Err(diags) => {
                eprintln!(
                    "Preprocess error: {} — {}",
                    path.display(),
                    diags
                        .first()
                        .map(|d| d.message.as_str())
                        .unwrap_or("unknown")
                );
                return;
            }
        }
    } else {
        source.clone()
    };

    // In debug mode with preprocessing, dump the expanded source
    if preprocess {
        println!("--- Preprocessed source ---");
        for (i, line) in parse_source.lines().enumerate() {
            println!("{:>5} | {}", i + 1, line);
        }
        println!("--- End preprocessed source ---");
        println!();
    }

    let program = match oxabl::try_parse(&parse_source) {
        Ok(program) => program,
        Err(panic) => {
            eprintln!("Lexer panic: {}: {panic}", path.display());
            return;
        }
    };

    if program.is_ok() {
        println!("OK: {}", path.display());
        return;
    }

    let source_map = SourceMap::new(&parse_source);

    println!("=== {} ===", path.display());
    println!();

    // Show the last N AST statements parsed before the first error
    let show_count = 10;
    let total_stmts = program.statements.len();
    println!(
        "--- Parsed {} statement(s) before error (showing last {}) ---",
        total_stmts,
        show_count.min(total_stmts)
    );
    let start_idx = total_stmts.saturating_sub(show_count);
    if start_idx > 0 {
        println!("  ... ({} earlier statements omitted)", start_idx);
    }
    for (i, stmt) in program.statements.iter().enumerate().skip(start_idx) {
        let debug_str = format!("{:?}", stmt);
        if debug_str.len() > 200 {
            println!("  [{}] {}...", i, &debug_str[..200]);
        } else {
            println!("  [{}] {}", i, debug_str);
        }
    }

    println!();
    println!("--- {} error(s) ---", program.errors.len());
    for (i, err) in program.errors.iter().enumerate() {
        let (line, col) = source_map.lookup(err.span.start as usize);
        println!("  Error {}: {}:{} — {}", i + 1, line, col, err.message);

        // Show source context around the error (±5 lines)
        let lines: Vec<&str> = parse_source.lines().collect();
        let start_line = line.saturating_sub(6); // 0-indexed, line is 1-indexed
        let end_line = (line + 4).min(lines.len());
        println!();
        for (l, src_line) in lines.iter().enumerate().take(end_line).skip(start_line) {
            let marker = if l + 1 == line { ">>>" } else { "   " };
            println!("  {} {:>5} | {}", marker, l + 1, src_line);
        }
        println!();
    }
}
