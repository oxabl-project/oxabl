use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

mod baseline;
mod explain;
mod sarif;
mod watch;

use clap::{Parser as ClapParser, ValueEnum};
use indicatif::{ProgressBar, ProgressStyle};
use oxabl_analyze::{
    CollectedDiagnostic, CollectedDiagnostics, dump_json_with_diagnostics,
    dump_text_with_diagnostics,
};
use oxabl_common::{
    Diagnostic, DiagnosticCode, FileId, FileSpan, LintSeverityMap, SourceMap, SourceResolver,
    render_diagnostics,
};
use oxabl_lint::{Registry, suppress};
use oxabl_pipeline::{
    ConfigOverrides, ConfigWarning, FormatPipeline, LintPipeline, PipelineConfig, ROOT_FILE_ID,
    position,
};
use oxabl_preprocessor::Preprocessor;
use oxabl_style::StyleGuide;
use oxabl_workspace::{
    EncodedFileSystem, FileSystem, SourceDecoding, discover_path, discover_path_in,
    walk_include_fragments,
};
use serde::Serialize;

/// How `check` writes its result.
#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CheckFormat {
    /// Human-readable findings (the default).
    Text,
    /// The versioned report document, same as `--json`.
    Json,
    /// One SARIF 2.1.0 log, for code review tools.
    Sarif,
}

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
    /// a misspelled `[workspace.style]` key, say — is a config error too. An
    /// unknown `[workspace.lint]` rule or group name is only a warning: that one
    /// entry is ignored and the rest of the table still applies. Alone among the subcommands, `check` refuses to run on one rather
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

        /// Output format: `text`, `json` (same as `--json`) or `sarif`.
        ///
        /// `sarif` writes one SARIF 2.1.0 log to stdout. Findings that an
        /// `oxabl-ignore` comment suppresses stay in it, marked as suppressed;
        /// with `--baseline`, each result also has a `baselineState`. The format
        /// changes what is printed and nothing else: the exit code is the same.
        #[arg(long, value_enum, conflicts_with = "json")]
        format: Option<CheckFormat>,

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

        /// Do not report findings recorded in this baseline file, so the gate
        /// fails only on new ones.
        ///
        /// A finding matches by rule code, workspace-relative path and a hash
        /// of the flagged source line, so edits elsewhere in the file do not
        /// unmatch it. Counts of new, baselined and stale (fixed) entries go to
        /// stderr. A missing or unreadable file is a usage error (exit 2).
        #[arg(long, value_name = "FILE")]
        baseline: Option<PathBuf>,

        /// Write every current finding to the `--baseline` file, then report
        /// none of them.
        ///
        /// Entries for files outside this run are kept, and entries for fixed
        /// findings are removed. Format drift and per-file failures still set
        /// the exit code.
        #[arg(long, requires = "baseline", conflicts_with = "no_lint")]
        update_baseline: bool,

        /// Also report the semantic and lint findings that sit in an include.
        ///
        /// Each one is a finding about **one root after expansion**, never about
        /// the workspace: "unused" means unused in that root's compilation unit,
        /// and another root may read the variable. It is reported at the include
        /// file's own path and line, with the root and the include chain that
        /// produced it. An include that several roots include is reported once
        /// per root; an include that one root expands several times is reported
        /// once per distinct finding, listing every expansion.
        ///
        /// Off by default, so a plain run keeps its findings, ordering and exit
        /// code. The findings count toward the exit code when it is on. Not
        /// combinable with `--baseline`. `--json` adds an `include_diagnostics`
        /// array.
        #[arg(long, conflicts_with = "baseline")]
        include_findings: bool,

        /// Number of files analysed at once. Defaults to the available cores;
        /// `1` analyses one file at a time.
        ///
        /// Output, exit code and baseline file are the same at every value:
        /// results are reported in file order.
        #[arg(long, short = 'j', value_name = "N", value_parser = clap::value_parser!(u32).range(1..))]
        jobs: Option<u32>,

        /// Check once, then watch the tree and the include paths, and re-check
        /// the files that changed and the files that depend on them.
        ///
        /// Only files whose findings changed are printed. A change to
        /// `oxabl.toml` or a `.df` file re-checks everything. Stop with Ctrl-C
        /// (exit 0). Not combinable with `--json` or `--baseline`.
        #[arg(long, conflicts_with_all = ["json", "format", "baseline"])]
        watch: bool,
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

        /// Number of files parsed at once. Defaults to the available cores;
        /// `1` parses one file at a time.
        ///
        /// Output and exit code are the same at every value: results are
        /// reported in file order. `--debug` always runs on one thread.
        #[arg(long, short = 'j', value_name = "N", value_parser = clap::value_parser!(u32).range(1..))]
        jobs: Option<u32>,
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
    /// Show direct database effects and transitive summaries of each unit.
    Explain {
        /// ABL file, optionally followed by a one-based root line number.
        file: String,
        /// Output a versioned JSON document.
        #[arg(long)]
        json: bool,
        /// Include search path (repeatable).
        #[arg(long = "include-path", short = 'I')]
        include_paths: Vec<PathBuf>,
        /// Schema file or directory.
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

        /// Number of files formatted at once. Defaults to the available cores;
        /// `1` formats one file at a time.
        ///
        /// Output and exit code are the same at every value: results are
        /// reported in file order.
        #[arg(long, short = 'j', value_name = "N", value_parser = clap::value_parser!(u32).range(1..))]
        jobs: Option<u32>,
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

/// Render *loud* preprocessor diagnostics — errors plus selected warnings —
/// for stderr via the shared [`render_diagnostics`] renderer.
///
/// Always-loud warning codes:
/// - `PREPROC007` unresolvable include (symbol loss)
/// - `PREPROC008` unterminated include reference
/// - `PREPROC009` predefined name without a value in an `&IF` condition
/// - `PREPROC010` part of an `&IF` condition the preprocessor does not evaluate
/// - `PREPROC002` unclosed `&IF` (inline/`skip_to_eol` regressions; #65 gate)
///
/// Rendering is delegated to [`render_diagnostics`] with a [`SourceResolver`]
/// over the root file: root-file diagnostics get a `path:line:col` position and
/// a snippet; diagnostics from a nested include (a different `FileId`) render
/// without a misleading root-relative position.
fn render_preproc_diagnostics(
    path: &Path,
    root_source: &str,
    root_file_id: FileId,
    diagnostics: &[Diagnostic],
) -> String {
    // Share the surfacing rule with the collector so the two never drift (U4).
    let is_loud = oxabl_analyze::is_loud;
    let loud: Vec<Diagnostic> = diagnostics.iter().filter(|d| is_loud(d)).cloned().collect();
    if loud.is_empty() {
        return String::new();
    }
    let resolver = SourceResolver::new(root_file_id, path.display().to_string(), root_source);
    render_diagnostics(&loud, &resolver)
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
fn surface_collected_preproc(
    path: &Path,
    source: &str,
    collected: &CollectedDiagnostics,
    output: &mut Output,
) {
    let loud: Vec<Diagnostic> = collected
        .by_source(oxabl_analyze::DiagnosticSource::Preproc)
        .map(|c| c.diagnostic.clone())
        .collect();
    if loud.is_empty() {
        return;
    }
    let secrets = baseline::secrets(source, collected.secrets_in(None, source));
    let shown = baseline::masked(source, &secrets);
    let resolver = SourceResolver::new(ROOT_FILE_ID, path.display().to_string(), &shown);
    output.err(render_diagnostics(&loud, &resolver));
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
fn surface_unjudged_symbols(sem: &oxabl_semantic::Semantic, output: &mut Output) -> usize {
    let n = oxabl_analyze::unjudged_symbol_count(sem);
    if n > 0 {
        let plural = if n == 1 { "symbol" } else { "symbols" };
        output.err(format!(
            "note: {n} {plural} could not be fully checked — {} named inside statement forms \
             oxabl recognizes but does not model, so the unused-variable, dead-store and \
             block-variable rules stayed silent for {}.\n",
            if n == 1 { "it is" } else { "they are" },
            if n == 1 { "it" } else { "them" },
        ));
    }
    n
}

/// Say when root-fragment analysis withheld whole-unit lint questions.
///
/// This is coverage, not a finding: it never changes the exit code. The note is
/// emitted only for an explicitly analyzed `.i`, so ordinary project walks do
/// not grow a line users would learn to ignore.
fn surface_source_context(sem: &oxabl_semantic::Semantic, output: &mut Output) -> bool {
    if !sem.source_context.is_include_fragment() {
        return false;
    }
    output.err(
        "note: include fragment analyzed without an includer — unresolved local names were \
         treated as external, and unused-variable, dead-store and block-variable rules stayed silent.\n"
            .to_string(),
    );
    true
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli {
        Cli::Check {
            path,
            json,
            format,
            no_preprocess,
            // Accepted and ignored — preprocessing is the default now.
            preprocess: _,
            include_paths,
            schema,
            no_lint,
            no_format,
            baseline,
            update_baseline,
            include_findings,
            jobs,
            watch,
        } => {
            let jobs = job_count(jobs);
            if watch {
                return watch::run(&watch::Options {
                    path: &path,
                    preprocess: !no_preprocess,
                    include_paths: &include_paths,
                    schema_path: schema.as_deref(),
                    no_lint,
                    no_format,
                    include_findings,
                    jobs,
                });
            }
            run_check(
                &path,
                match format {
                    Some(f) => f,
                    None if json => CheckFormat::Json,
                    None => CheckFormat::Text,
                },
                // On by default (R19): the language server always preprocesses, and a
                // gate that does not would report every include-declared symbol as
                // undefined.
                !no_preprocess,
                &include_paths,
                schema.as_deref(),
                no_lint,
                no_format,
                baseline.as_deref().map(|file| (file, update_baseline)),
                include_findings,
                jobs,
            )
        }
        Cli::Conformance {
            path,
            json,
            preprocess,
            include_paths,
            debug,
            jobs,
        } => run_conformance(
            &path,
            json,
            preprocess,
            &include_paths,
            debug,
            job_count(jobs),
        ),
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
        Cli::Explain {
            file,
            json,
            include_paths,
            schema,
        } => explain::run(&file, json, &include_paths, schema.as_deref()),
        Cli::Format {
            path,
            check,
            stdout,
            style,
            jobs,
        } => run_format(&path, check, stdout, style.as_deref(), job_count(jobs)),
        Cli::Lsp => run_lsp(),
        Cli::Daemon { workspace_root } => run_daemon(&workspace_root),
        Cli::Schema => run_schema(),
    }
}

/// The `-j` value, or the available cores when it is absent.
fn job_count(jobs: Option<u32>) -> usize {
    jobs.map_or_else(
        || std::thread::available_parallelism().map_or(1, usize::from),
        |n| n as usize,
    )
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
///
/// Files are formatted on `jobs` threads and reported in file order. Each
/// discovered file is a distinct file, so no two threads write the same one.
fn run_format(
    path: &Path,
    check: bool,
    stdout: bool,
    style: Option<&str>,
    jobs: usize,
) -> ExitCode {
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
    //
    // The source encoding is the exception: `--style` still has to read the files
    // the way the tree is stored, so it takes the encoding from the config and
    // stays silent about a config it otherwise ignores.
    let (config, warnings) = match cli_style {
        Some(style) => (
            PipelineConfig {
                style,
                decoding: PipelineConfig::resolve_style_only(path, &ConfigOverrides::default())
                    .0
                    .decoding,
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
    let files = match discover_path_in(path, &config.root_extensions) {
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
    in_task_order(
        jobs.min(files.len()),
        &files,
        (),
        || (),
        |(), file| format_one(&pipeline, config.decoding, file, check, stdout),
        |formatted| {
            eprint!("{}", formatted.messages);
            any_would_change |= formatted.would_change;
            any_io_error |= formatted.io_error;
            let written = formatted.stdout.map(|bytes| {
                bytes.and_then(|bytes| std::io::Write::write_all(&mut std::io::stdout(), &bytes))
            });
            if let Some(Err(e)) = written {
                eprintln!("error: cannot write {}: {e}", formatted.file.display());
                any_io_error = true;
            }
        },
    );

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

/// One file's `format` result, reported by the calling thread.
struct FormattedFile<'a> {
    file: &'a Path,
    /// The stderr lines for this file.
    messages: String,
    /// The bytes `--stdout` prints, or why they could not be encoded.
    stdout: Option<std::io::Result<Vec<u8>>>,
    would_change: bool,
    io_error: bool,
}

/// Read, format and (in write mode) rewrite one file for `run_format`.
fn format_one<'a>(
    pipeline: &FormatPipeline,
    decoding: SourceDecoding,
    file: &'a Path,
    check: bool,
    stdout: bool,
) -> FormattedFile<'a> {
    use std::fmt::Write as _;
    let mut result = FormattedFile {
        file,
        messages: String::new(),
        stdout: None,
        would_change: false,
        io_error: false,
    };
    let messages = &mut result.messages;
    let stored = match decoding.read_file(file) {
        Ok(stored) => stored,
        Err(e) => {
            let _ = writeln!(messages, "error: cannot read {}: {e}", file.display());
            result.io_error = true;
            return result;
        }
    };
    let source = &stored.text;
    let encoded = |text: &str| {
        let mut bytes = Vec::new();
        write_stored(&stored, text, &mut bytes).map(|()| bytes)
    };

    let outcome = pipeline.format(source);
    if let Some(formatted) = outcome.output() {
        if check {
            let _ = writeln!(messages, "{}: would reformat", file.display());
            result.would_change = true;
        } else if stdout {
            result.stdout = Some(encoded(formatted));
        } else if let Err(e) = stored
            .encode(formatted)
            .and_then(|bytes| std::fs::write(file, bytes))
        {
            let _ = writeln!(messages, "error: cannot write {}: {e}", file.display());
            result.io_error = true;
        }
    } else {
        // Unchanged or a refusal: nothing to write in any mode, and --stdout
        // still emits the original bytes so a pipeline never loses the file.
        if let Some(not_formatted) = outcome.not_formatted() {
            // A contained panic keeps its own wording — the reason alone
            // reads as an ordinary refusal, and an oxabl bug should not
            // (R5).
            if not_formatted.is_internal_panic() {
                let _ = writeln!(
                    messages,
                    "{}: internal panic while formatting; left unchanged ({})",
                    file.display(),
                    not_formatted.reason()
                );
            } else {
                let _ = writeln!(messages, "{}: {}", file.display(), not_formatted.reason());
            }
        }
        if stdout {
            result.stdout = Some(encoded(source));
        }
    }
    result
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
    /// Include files (`.i`) of a walked directory that were checked for
    /// context-free rules, and are not in `files_checked`. Omitted when none
    /// was, so ordinary walks keep their JSON.
    #[serde(skip_serializing_if = "is_zero")]
    include_files_checked: usize,
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
    /// Explicit include-fragment roots whose whole-unit lint rules were
    /// withheld. Omitted for ordinary walks to keep their JSON stable.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    fragment_roots: Vec<String>,
    /// Findings in an include, each for one root's compilation unit. Present only
    /// with `--include-findings` and when there are any; they count toward the
    /// exit code.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    include_diagnostics: Vec<CheckJsonIncludeFinding>,
    failures: Vec<CheckJsonFailure>,
    /// Present only with `--baseline`; `diagnostics` then holds new findings
    /// only.
    #[serde(skip_serializing_if = "Option::is_none")]
    baseline: Option<baseline::Report>,
    /// Findings that an `oxabl-ignore` comment suppressed. Omitted when none
    /// was.
    #[serde(skip_serializing_if = "is_zero")]
    suppressed: usize,
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
    /// How the entry points reach a security finding, when entry points are
    /// configured.
    #[serde(skip_serializing_if = "Option::is_none")]
    reach: Option<oxabl_common::Reachability>,
    /// Other places of the same finding, in this file (the further decisions
    /// of a `client-identity` finding). Present since version 3, when not
    /// empty.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    related: Vec<CheckJsonRelated>,
}

/// One other place of a finding.
#[derive(Serialize)]
struct CheckJsonRelated {
    message: String,
    span: CheckJsonSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<CheckJsonPosition>,
    #[serde(skip_serializing_if = "Option::is_none")]
    end: Option<CheckJsonPosition>,
}

/// One finding in an include, as a result for one root's compilation unit.
///
/// `path` is the physical include file and `span`/`start`/`end` are in its own
/// bytes. `root` is the checked file whose expansion produced it, which is what
/// separates this from a claim about the workspace: a declaration unused here
/// can be read by another root. `scope` says so in the data.
#[derive(Serialize)]
struct CheckJsonIncludeFinding {
    path: String,
    root: String,
    scope: &'static str,
    code: String,
    severity: &'static str,
    source: &'static str,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    help: Option<String>,
    span: CheckJsonSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<CheckJsonPosition>,
    #[serde(skip_serializing_if = "Option::is_none")]
    end: Option<CheckJsonPosition>,
    expansions: Vec<CheckJsonExpansion>,
}

/// One expansion of the include that produced a finding.
#[derive(Serialize)]
struct CheckJsonExpansion {
    /// This expansion's place among the root's expansions of that file, from 1.
    occurrence: u32,
    occurrences: u32,
    /// Root first. Each step is a file and the reference that included it.
    chain: Vec<CheckJsonChainStep>,
}

#[derive(Serialize)]
struct CheckJsonChainStep {
    path: String,
    /// The reference, in the bytes of the file that wrote it. Omitted when the
    /// reference sits in `{&name}` substitution text.
    #[serde(skip_serializing_if = "Option::is_none")]
    site: Option<CheckJsonSpan>,
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    column: Option<usize>,
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
#[allow(clippy::too_many_arguments)]
fn run_check(
    path: &Path,
    format_choice: CheckFormat,
    preprocess: bool,
    include_paths: &[PathBuf],
    schema_path: Option<&Path>,
    no_lint: bool,
    no_format: bool,
    baseline_file: Option<(&Path, bool)>,
    include_findings: bool,
    jobs: usize,
) -> ExitCode {
    let json_output = format_choice == CheckFormat::Json;
    let sarif_output = format_choice == CheckFormat::Sarif;

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
    // "Could not use" is broader than "malformed TOML": `StyleGuide` denies
    // unknown fields, so a syntactically valid file naming a key oxabl does not
    // know lands here too. An unknown lint rule or group name does not: it is a
    // `ConfigWarning::Lint`, and the rest of `[workspace.lint]` still applies.
    //
    // Every other warning stays a warning. A `.df` that would not load, or a
    // `--schema` directory that matched nothing (A2), leaves the rest of the
    // configuration intact, so the run still means what it says.
    //
    // An entry-point file that does not load is one too: the run would gate on
    // security findings without the reachability the project configured.
    if let Some(msg) = warnings.iter().find_map(|w| match w {
        ConfigWarning::Config(msg) => Some(msg.clone()),
        ConfigWarning::EntryPoints(_) => Some(w.to_string()),
        _ => None,
    }) {
        eprintln!("error: {msg}");
        return ExitCode::from(2);
    }
    // The warnings that qualify the run, for a SARIF log as well as stderr.
    let mut run_warnings: Vec<String> = Vec::new();
    for warning in &warnings {
        eprintln!("warning: {warning}");
        run_warnings.push(warning.to_string());
    }

    // Discovery through the shared walker: a named file is checked regardless of
    // extension, a directory is walked under the configured root extensions
    // (`[workspace.sources].root_extensions`). A missing path and an empty tree
    // are distinct facts but the same usage error (exit 2).
    let files = match discover_path_in(path, &config.root_extensions) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };

    let baseline = match baseline_file.map(|(file, update)| baseline::Run::open(file, update, path))
    {
        None => None,
        Some(Ok(run)) => Some(run),
        Some(Err(msg)) => {
            eprintln!("error: {msg}");
            return ExitCode::from(2);
        }
    };

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

    let fs = config.file_system();
    // One run handle, so one cross-file index spans the whole walk: files sharing
    // a parent class read it once, not once each. The per-file handle below is a
    // pointer copy off this one.
    // The walk's own file list — no directory scan — scanned once, on every
    // core, which is what lets a `DEFINE SHARED` consumer find its producer even
    // when no `RUN` names the producing file.
    let producers = LintPipeline::new(&config, &fs).shared_producers_parallel(&indexed, jobs);
    let lint = LintPipeline::new(&config, &fs)
        .with_preprocess(preprocess)
        .with_shared_producers(&producers);
    // The format pipeline is built from the resolved style and nothing else, so
    // it *cannot* see expanded macro text however this walk is configured (R4).
    let format = FormatPipeline::new(config.style.clone());

    // Include files in a walked directory. They count as ABL files here: a
    // directory of nothing else is checked, not a usage error.
    let fragments: Vec<(PathBuf, PathBuf)> = if !no_lint && path.is_dir() {
        walk_include_fragments(path)
            .into_iter()
            .map(|file| {
                let indexed_path = std::path::absolute(&file).unwrap_or_else(|_| file.clone());
                (file, indexed_path)
            })
            .filter(|(_, indexed_path)| lint.with_file(indexed_path).context_free_rules_enabled())
            .collect()
    } else {
        Vec::new()
    };
    if files.is_empty() && fragments.is_empty() {
        eprintln!("error: no ABL files found in {}", path.display());
        return ExitCode::from(2);
    }

    let pass = CheckPass {
        output: format_choice,
        root: baseline::workspace_root(path, None),
        severities: &config.lint_severities,
        no_lint,
        no_format,
        include_findings,
        baseline: baseline.as_ref(),
        format: &format,
        decoding: config.decoding,
    };
    // Files first, then include fragments, so the output order is the serial
    // order whatever the thread count.
    let tasks: Vec<Task<'_>> = files
        .iter()
        .zip(&indexed)
        .map(|(file, indexed_path)| Task {
            file,
            indexed_path,
            fragment: false,
        })
        .chain(fragments.iter().map(|(file, indexed_path)| Task {
            file,
            indexed_path,
            fragment: true,
        }))
        .collect();

    // TX004 asks for the summaries of its callees. Built once over the walk and
    // shared, as each thread's handle would otherwise build its own.
    let summaries = (!no_lint && lint.callee_summaries_enabled())
        .then(|| std::sync::Arc::new(lint.build_workspace_effect_summaries(&indexed)));
    let lint = match &summaries {
        Some(s) => lint.with_effect_summaries(std::sync::Arc::clone(s)),
        None => lint,
    };
    // The security rules ask how the entry points reach each file: one graph
    // for the walk.
    let reach = (!no_lint && lint.security_reach_enabled()).then(|| {
        let reach = lint.build_security_reach(None);
        for label in &reach.unresolved {
            let text = format!("entry point `{label}`: its target was not found");
            eprintln!("warning: {text}");
            run_warnings.push(text);
        }
        std::sync::Arc::new(reach)
    });
    let lint = match &reach {
        Some(r) => lint.with_security_reach(std::sync::Arc::clone(r)),
        None => lint,
    };

    let mut totals = Totals::default();
    run_ordered(
        jobs.min(tasks.len()),
        config.decoding,
        &lint,
        || {
            let handle = LintPipeline::new(&config, &fs)
                .with_preprocess(preprocess)
                .with_shared_producers(&producers);
            let handle = match &summaries {
                Some(s) => handle.with_effect_summaries(std::sync::Arc::clone(s)),
                None => handle,
            };
            match &reach {
                Some(r) => handle.with_security_reach(std::sync::Arc::clone(r)),
                None => handle,
            }
        },
        &tasks,
        |lint, texts, task| pass.run(lint, texts, task),
        |unit| totals.absorb(unit),
    );
    let Totals {
        diagnostics,
        preproc,
        mut failures,
        drifted,
        unjudged,
        fragment_roots,
        include_diagnostics,
        include_files_checked,
        sarif_results,
        suppressed,
    } = totals;
    if suppressed > 0 {
        eprintln!("suppressed: {suppressed} finding(s) by oxabl-ignore comments");
    }

    let baseline = match baseline.map(|run| run.finish(!no_lint)) {
        None => None,
        Some(Ok(report)) => {
            surface_baseline(&report);
            Some(report)
        }
        Some(Err(msg)) => {
            eprintln!("error: {msg}");
            failures.push(CheckJsonFailure {
                path: baseline_file.map_or_else(String::new, |(f, _)| f.display().to_string()),
                reason: msg,
            });
            None
        }
    };

    if sarif_output {
        let mut notifications: Vec<sarif::Notification> = failures
            .iter()
            .map(|f| sarif::Notification {
                level: "error",
                text: format!("{}: {}", f.path, f.reason),
            })
            .collect();
        notifications.extend(run_warnings.iter().map(|w| sarif::Notification {
            level: "warning",
            text: w.clone(),
        }));
        notifications.extend(drifted.iter().map(|d| sarif::Notification {
            level: "warning",
            text: format!("{d}: would be reformatted; run `oxabl format`"),
        }));
        let log = sarif::document(
            Registry::builtin_shared(),
            &sarif_results,
            &notifications,
            Some(&baseline::workspace_root(path, None)),
        );
        match serde_json::to_string_pretty(&log) {
            Ok(s) => println!("{s}"),
            Err(e) => {
                eprintln!("error: json serialize: {e}");
                return ExitCode::from(6);
            }
        }
        return check_exit_code(
            diagnostics.len() + include_diagnostics.len(),
            drifted.len(),
            failures.len(),
        );
    }

    if json_output {
        let report = CheckJsonReport {
            // 2: `preproc_diagnostics` became `preproc`, matching `analyze`'s
            // envelope, and its rows gained a `path` (D1).
            // 3: a diagnostic row carries its `help` line when it has one.
            version: 3,
            files_checked: files.len(),
            include_files_checked,
            lint_enabled: !no_lint,
            diagnostics,
            format_enabled: !no_format,
            format: CheckJsonFormat {
                drifted_count: drifted.len(),
                drifted,
            },
            preproc,
            unjudged_symbols: unjudged,
            fragment_roots,
            include_diagnostics,
            failures,
            baseline,
            suppressed,
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
            report.diagnostics.len() + report.include_diagnostics.len(),
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
    if diagnostics.is_empty()
        && include_diagnostics.is_empty()
        && drifted.is_empty()
        && failures.is_empty()
    {
        let plural = if files.len() == 1 { "file" } else { "files" };
        let includes = match include_files_checked {
            0 => String::new(),
            1 => " and 1 include file".to_string(),
            n => format!(" and {n} include files"),
        };
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
            "checked {} {plural}{includes}: {lint_channel}, {format_channel}",
            files.len()
        );
    }

    check_exit_code(
        diagnostics.len() + include_diagnostics.len(),
        drifted.len(),
        failures.len(),
    )
}

/// The baseline counts, on stderr: they qualify the run like the coverage notes
/// do, so `--json` stdout stays one document.
fn surface_baseline(report: &baseline::Report) {
    let stale = report.stale.unwrap_or(0);
    if report.updated {
        eprintln!(
            "baseline: updated {} ({} added, {stale} removed)",
            report.path, report.new
        );
        if let Some(v) = report.old_version {
            eprintln!("baseline: rewrote version {v} as version 3");
        }
        if report.legacy > 0 {
            eprintln!(
                "baseline: {} entries of files this run did not check stay at version 2 until \
                 an update checks them",
                report.legacy
            );
        }
        if report.outside_root > 0 {
            eprintln!("{}", outside_root_note(report));
        }
        return;
    }
    let stale_note = match report.stale {
        None => "stale not counted under --no-lint".to_string(),
        Some(0) => "0 stale".to_string(),
        Some(n) => format!("{n} stale (fixed; run with --update-baseline to remove them)"),
    };
    eprintln!(
        "baseline: {} new, {} baselined, {stale_note}",
        report.new, report.baselined
    );
    if let Some(v) = report.old_version {
        eprintln!(
            "baseline: {} is version {v}; run --update-baseline over the whole tree to rewrite \
             it as version 3",
            report.path
        );
    } else if report.legacy > 0 {
        eprintln!(
            "baseline: {} entries of {} are version 2; run --update-baseline over the whole \
             tree to rewrite them as version 3",
            report.legacy, report.path
        );
    }
    if report.outside_root > 0 {
        eprintln!("{}", outside_root_note(report));
    }
}

/// The note for findings in files outside the baseline root.
fn outside_root_note(report: &baseline::Report) -> String {
    format!(
        "baseline: {} finding(s) in files outside the baseline root were reported, not recorded",
        report.outside_root
    )
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

fn is_zero(n: &usize) -> bool {
    *n == 0
}

/// What one unit of `check` work printed, in order, per stream.
///
/// A worker cannot print: another worker's lines would land between its own. It
/// records instead, and the run replays each unit in file order, so stdout and
/// stderr read the same at any thread count.
#[derive(Default)]
struct Output(Vec<(Stream, String)>);

#[derive(Clone, Copy, PartialEq)]
enum Stream {
    Out,
    Err,
}

impl Output {
    fn out(&mut self, text: String) {
        self.push(Stream::Out, text);
    }

    fn err(&mut self, text: String) {
        self.push(Stream::Err, text);
    }

    fn push(&mut self, stream: Stream, text: String) {
        if text.is_empty() {
            return;
        }
        match self.0.last_mut() {
            Some((last, buffer)) if *last == stream => buffer.push_str(&text),
            _ => self.0.push((stream, text)),
        }
    }

    fn flush(self) {
        for (stream, text) in self.0 {
            match stream {
                Stream::Out => print!("{text}"),
                Stream::Err => eprint!("{text}"),
            }
        }
    }
}

/// One file's contribution to the run: what it printed and what it adds to the
/// report.
#[derive(Default)]
struct Unit {
    output: Output,
    diagnostics: Vec<CheckJsonDiagnostic>,
    preproc: Vec<CheckJsonDiagnostic>,
    failures: Vec<CheckJsonFailure>,
    include_diagnostics: Vec<CheckJsonIncludeFinding>,
    drifted: Option<String>,
    unjudged: usize,
    fragment_root: Option<String>,
    include_file_checked: bool,
    /// Every finding, for `--format sarif`.
    sarif_results: Vec<sarif::Finding>,
    /// Findings that an `oxabl-ignore` comment suppressed.
    suppressed: usize,
}

/// The run's report, built from its units in file order.
#[derive(Default)]
struct Totals {
    diagnostics: Vec<CheckJsonDiagnostic>,
    preproc: Vec<CheckJsonDiagnostic>,
    failures: Vec<CheckJsonFailure>,
    drifted: Vec<String>,
    unjudged: usize,
    fragment_roots: Vec<String>,
    include_diagnostics: Vec<CheckJsonIncludeFinding>,
    include_files_checked: usize,
    sarif_results: Vec<sarif::Finding>,
    suppressed: usize,
}

impl Totals {
    fn absorb(&mut self, unit: Unit) {
        unit.output.flush();
        self.diagnostics.extend(unit.diagnostics);
        self.preproc.extend(unit.preproc);
        self.failures.extend(unit.failures);
        self.drifted.extend(unit.drifted);
        self.unjudged += unit.unjudged;
        self.fragment_roots.extend(unit.fragment_root);
        self.include_diagnostics.extend(unit.include_diagnostics);
        self.include_files_checked += usize::from(unit.include_file_checked);
        self.sarif_results.extend(unit.sarif_results);
        self.suppressed += unit.suppressed;
    }
}

/// One file `check` analyses: a root of the walk, or an include fragment that
/// only the context-free rules read.
struct Task<'a> {
    file: &'a Path,
    indexed_path: &'a Path,
    fragment: bool,
}

/// The settings of one `check` run, read by every worker.
struct CheckPass<'a> {
    output: CheckFormat,
    /// The workspace root, for the paths of SARIF results.
    root: PathBuf,
    severities: &'a LintSeverityMap,
    no_lint: bool,
    no_format: bool,
    include_findings: bool,
    baseline: Option<&'a baseline::Run>,
    format: &'a FormatPipeline,
    decoding: SourceDecoding,
}

impl CheckPass<'_> {
    fn run(&self, lint: &LintPipeline<'_>, texts: &mut IncludeTexts, task: &Task<'_>) -> Unit {
        if task.fragment {
            self.fragment(lint, task.file, task.indexed_path)
        } else {
            self.root(lint, texts, task.file, task.indexed_path)
        }
    }

    fn root(
        &self,
        lint: &LintPipeline<'_>,
        texts: &mut IncludeTexts,
        file: &Path,
        indexed_path: &Path,
    ) -> Unit {
        let mut unit = Unit::default();
        let display = file.display().to_string();
        let source = match read_source(file, self.decoding) {
            Ok(s) => s,
            Err(e) => {
                // Report and continue (R24): one unreadable file must not cost
                // the caller every other file's findings.
                unit.output
                    .err(format!("error: cannot read {display}: {e}\n"));
                unit.failures.push(CheckJsonFailure {
                    path: display,
                    reason: format!("cannot read: {e}"),
                });
                return unit;
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
            unit.output
                .err(format!("error: analysis failed on {display}: {panic}\n"));
            unit.failures.push(CheckJsonFailure {
                path: display.clone(),
                reason: panic.to_string(),
            });
        } else {
            // Preprocessor diagnostics keep their own channel — surfaced by the
            // same helper `analyze` uses, so the two cannot drift. They are
            // coverage warnings, not findings, and never move the exit code: an
            // elided include still parses.
            surface_collected_preproc(file, &source, result.diagnostics(), &mut unit.output);
            // Machine-readable, through the *same* row builder the findings use
            // (D1): an entry names its file, so two files losing coverage on
            // different includes are two distinguishable entries.
            //
            // Only `--json` reads these rows, and most files have no preproc
            // diagnostic at all — so the `SourceMap` (a scan of the whole file) is
            // built behind both conditions, the way the findings rows below build
            // theirs only once there is a finding.
            if self.output == CheckFormat::Json {
                let mut collected = result
                    .by_source(oxabl_analyze::DiagnosticSource::Preproc)
                    .peekable();
                if collected.peek().is_some() {
                    let map = SourceMap::new(&source);
                    unit.preproc
                        .extend(collected.map(|c| check_json_diagnostic(&display, &map, c)));
                }
            }

            // `--no-lint` is a **filter on the reported set**, exactly as it is
            // for `analyze`: the lint-sourced entries go, the parse and semantic
            // ones stay. Two chained calls to the one shared filter rather than a
            // second hand-rolled predicate.
            let reported = result.excluding_source(oxabl_analyze::DiagnosticSource::Preproc);
            let reported = if self.no_lint {
                reported.excluding_source(oxabl_analyze::DiagnosticSource::Lint)
            } else {
                reported
            };
            let fragment_root = result
                .semantic()
                .is_some_and(|sem| sem.source_context.is_include_fragment());
            report_findings(
                &display,
                indexed_path,
                &source,
                reported,
                fragment_root,
                self,
                &mut unit,
            );

            if self.include_findings {
                let mut shown = result.include_origin().to_vec();
                if self.no_lint {
                    shown.retain(|d| d.source != oxabl_analyze::DiagnosticSource::Lint);
                }
                report_include_findings(
                    &display,
                    &source,
                    &shown,
                    result.diagnostics(),
                    texts,
                    self,
                    &mut unit,
                );
            }

            // The unjudged-symbol note is a statement about the *count-gated lint
            // rules'* coverage, so it has nothing to qualify when those findings
            // are suppressed.
            if !self.no_lint
                && let Some(sem) = result.semantic()
            {
                if surface_source_context(sem, &mut unit.output) {
                    unit.fragment_root = Some(display.clone());
                } else {
                    unit.unjudged += surface_unjudged_symbols(sem, &mut unit.output);
                }
            }
        }

        if !self.no_format {
            // The same `FormatPipeline` `oxabl format` drives, so `check`'s
            // drift answer and `format --check`'s cannot disagree (KTD7).
            let outcome = self.format.format(&source);
            if outcome.changed() {
                unit.drifted = Some(display);
            } else if let Some(not_formatted) = outcome.not_formatted() {
                if not_formatted.is_internal_panic() {
                    // An oxabl bug, not a property of the input (R5): worth a
                    // failure entry and exit 1.
                    unit.output.err(format!(
                        "error: formatting failed on {display}: {}\n",
                        not_formatted.reason()
                    ));
                    unit.failures.push(CheckJsonFailure {
                        path: display,
                        reason: not_formatted.reason(),
                    });
                } else {
                    // A deliberate bail is expected behavior on some inputs and
                    // is *not* drift, so it stays neutral — the same call
                    // `format --check` makes.
                    unit.output
                        .err(format!("{display}: {}\n", not_formatted.reason()));
                }
            }
        }
        unit
    }

    /// An include file in a walked directory: only the context-free rules
    /// report there, once, in the file itself. A root's own run drops findings
    /// that come from an include, so nothing is reported twice. Nothing else
    /// about an include is checked: it often does not parse or resolve without
    /// its includer.
    fn fragment(&self, lint: &LintPipeline<'_>, file: &Path, indexed_path: &Path) -> Unit {
        let mut unit = Unit::default();
        let display = file.display().to_string();
        let source = match read_source(file, self.decoding) {
            Ok(s) => s,
            Err(e) => {
                unit.output
                    .err(format!("error: cannot read {display}: {e}\n"));
                unit.failures.push(CheckJsonFailure {
                    path: display,
                    reason: format!("cannot read: {e}"),
                });
                return unit;
            }
        };
        let result = lint.with_file(indexed_path).run_context_free(&source);
        if let Some(panic) = result.failure() {
            unit.output
                .err(format!("error: analysis failed on {display}: {panic}\n"));
            unit.failures.push(CheckJsonFailure {
                path: display,
                reason: panic.to_string(),
            });
            return unit;
        }
        unit.include_file_checked = true;
        report_findings(
            &display,
            indexed_path,
            &source,
            result.into_diagnostics(),
            true,
            self,
            &mut unit,
        );
        unit
    }
}

/// Run `work` over `tasks` on `jobs` threads and hand every unit to `absorb`
/// in task order.
///
/// Each worker analyses through an index of its own, built by `own_lint`, and
/// keeps its own cache of include texts. One index shared by every worker
/// serialises them on its lock: at 24 threads it ran 2.4x slower. `jobs <= 1`
/// runs on the calling thread through `run_lint` with no thread at all.
fn run_ordered<'a>(
    jobs: usize,
    decoding: SourceDecoding,
    run_lint: &LintPipeline<'a>,
    own_lint: impl Fn() -> LintPipeline<'a> + Sync,
    tasks: &[Task<'_>],
    work: impl Fn(&LintPipeline<'_>, &mut IncludeTexts, &Task<'_>) -> Unit + Sync,
    absorb: impl FnMut(Unit),
) {
    // The calling thread's pipeline is not `Sync`, so it travels in the serial
    // state rather than in `work`.
    in_task_order(
        jobs,
        tasks,
        (Err(run_lint), IncludeTexts::new(decoding)),
        || (Ok(own_lint()), IncludeTexts::new(decoding)),
        |(lint, texts), task| {
            let lint = match lint {
                Ok(own) => &*own,
                Err(run) => *run,
            };
            work(lint, texts, task)
        },
        absorb,
    );
}

/// Run `work` over `tasks` on `jobs` threads and hand every result to
/// `absorb`, on the calling thread, in task order.
///
/// Each thread works on a state of its own from `thread_state`. `jobs <= 1`
/// runs on the calling thread with `serial_state` and no thread at all.
fn in_task_order<'t, T: Sync, S, R: Send>(
    jobs: usize,
    tasks: &'t [T],
    mut serial_state: S,
    thread_state: impl Fn() -> S + Sync,
    work: impl Fn(&mut S, &'t T) -> R + Sync,
    mut absorb: impl FnMut(R),
) {
    if jobs <= 1 {
        for task in tasks {
            absorb(work(&mut serial_state, task));
        }
        return;
    }
    let next = AtomicUsize::new(0);
    let (sender, receiver) = std::sync::mpsc::channel::<(usize, R)>();
    std::thread::scope(|scope| {
        for _ in 0..jobs {
            let sender = sender.clone();
            let (next, thread_state, work) = (&next, &thread_state, &work);
            scope.spawn(move || {
                let mut state = thread_state();
                loop {
                    let i = next.fetch_add(1, Ordering::Relaxed);
                    let Some(task) = tasks.get(i) else { break };
                    let result = work(&mut state, task);
                    if sender.send((i, result)).is_err() {
                        break;
                    }
                }
            });
        }
        drop(sender);
        let mut waiting: HashMap<usize, R> = HashMap::new();
        let mut want = 0;
        for (i, result) in receiver {
            waiting.insert(i, result);
            while let Some(result) = waiting.remove(&want) {
                absorb(result);
                want += 1;
            }
        }
    });
}

/// Apply suppressions and the baseline to one file's findings, then print them
/// (text mode) and add them to the report.
///
/// `context_free_only` says that the file ran only the context-free rules, so
/// a suppression of any other rule cannot be judged unused.
fn report_findings(
    display: &str,
    indexed_path: &Path,
    source: &str,
    reported: CollectedDiagnostics,
    context_free_only: bool,
    pass: &CheckPass,
    unit: &mut Unit,
) {
    let sarif_output = pass.output == CheckFormat::Sarif;
    let (reported, suppressed) = if pass.no_lint {
        (reported, Vec::new())
    } else {
        apply_suppressions(source, reported, indexed_path, pass, context_free_only)
    };
    unit.suppressed += suppressed.len();
    // Every hard-coded secret of the file, so no shown line and no hash holds
    // one, whatever finding covers it.
    let secrets = baseline::secrets(source, reported.secrets_in(None, source));
    let has_baseline = pass.baseline.is_some();
    let (reported, covered) = match pass.baseline {
        Some(run) => run.split(indexed_path, source, reported),
        None => (reported, Vec::new()),
    };
    if reported.diagnostics.is_empty() && covered.is_empty() && suppressed.is_empty() {
        return;
    }
    if pass.output == CheckFormat::Text && !reported.diagnostics.is_empty() {
        let shown = baseline::masked(source, &secrets);
        let resolver = SourceResolver::new(ROOT_FILE_ID, display.to_string(), &shown);
        let rendered: Vec<Diagnostic> = reported.all().map(|c| c.diagnostic.clone()).collect();
        unit.output.out(render_diagnostics(&rendered, &resolver));
    }
    let map = SourceMap::new(source);
    for collected in reported.all() {
        unit.diagnostics
            .push(check_json_diagnostic(display, &map, collected));
    }
    if !sarif_output {
        return;
    }

    // One fingerprint pass over every finding of the file, so the occurrence
    // numbers do not move when a finding is suppressed or baselined.
    let mut all: Vec<(&CollectedDiagnostic, Option<&'static str>, Option<&str>)> = Vec::new();
    let state = |s| has_baseline.then_some(s);
    all.extend(reported.all().map(|c| (c, state("new"), None)));
    all.extend(covered.iter().map(|c| (c, state("unchanged"), None)));
    all.extend(
        suppressed
            .iter()
            .map(|(c, why)| (c, None, Some(why.as_str()))),
    );
    let path = baseline::relative_path(&pass.root, indexed_path);
    let key = path.clone().unwrap_or_else(|| display.to_string());
    let entries = baseline::fingerprint(
        &key,
        source,
        all.iter().map(|(c, _, _)| &c.diagnostic),
        &secrets,
    );
    let uri = sarif::uri(&key);
    for ((c, baseline_state, reason), entry) in all.into_iter().zip(&entries) {
        let d = &c.diagnostic;
        let region = (d.span.file == ROOT_FILE_ID)
            .then(|| sarif::Region::of_span(source, d.span.span.start, d.span.span.end));
        unit.sarif_results.push(sarif::Finding {
            rule_id: d.code.0.to_string(),
            severity: sarif::level(d.severity.as_str()),
            text: match &d.help {
                Some(help) => format!("{}\n{help}", d.message),
                None => d.message.clone(),
            },
            uri: uri.clone(),
            region,
            fingerprint: baseline::finding_fingerprint(entry),
            baseline_state,
            suppressed: reason.map(str::to_string),
            flow: sarif::flow_of(Registry::builtin_shared(), d, source, ROOT_FILE_ID),
            reach: d.reach.as_deref().cloned(),
            related: related_locations(d, &pass.root)
                .into_iter()
                .chain(decision_locations(d, source, &uri))
                .collect(),
        });
    }
}

/// The calls on the reachability path of a finding, as SARIF related
/// locations: each in its own file, by line.
fn related_locations(d: &Diagnostic, root: &Path) -> Vec<sarif::Related> {
    let Some(path) = d.reach.as_deref().and_then(|r| r.path.as_ref()) else {
        return Vec::new();
    };
    path.steps
        .iter()
        .filter_map(|s| {
            let file = s.file.as_ref()?;
            let line = s.line?;
            let key =
                baseline::relative_path(root, file).unwrap_or_else(|| file.display().to_string());
            Some(sarif::Related {
                uri: sarif::uri(&key),
                line: line as usize,
                message: format!("{} calls on the way from {}", s.routine, path.entry.label),
            })
        })
        .collect()
}

/// The other places of a finding in the checked file (`Diagnostic::related`),
/// as SARIF related locations.
fn decision_locations(d: &Diagnostic, source: &str, uri: &str) -> Vec<sarif::Related> {
    d.related()
        .iter()
        .filter(|l| l.span.file == ROOT_FILE_ID)
        .map(|l| sarif::Related {
            uri: uri.to_string(),
            line: sarif::Region::of_span(source, l.span.span.start, l.span.span.end).start_line,
            message: l.message.clone(),
        })
        .collect()
}

/// Split `reported` into the findings that stay and those that an
/// `oxabl-ignore` comment suppresses (with its reason). The stay set gains a
/// LINT0007 for each comment that cannot suppress and a LINT0008 for each valid
/// one that matched nothing.
fn apply_suppressions(
    source: &str,
    mut reported: CollectedDiagnostics,
    indexed_path: &Path,
    pass: &CheckPass,
    context_free_only: bool,
) -> (CollectedDiagnostics, Vec<(CollectedDiagnostic, String)>) {
    let registry = Registry::builtin_shared();
    let directives = suppress::scan(source, registry);
    if directives.is_empty() {
        return (reported, Vec::new());
    }
    let severities = pass.severities.for_path(indexed_path);
    let severity_of = |code: &str| {
        registry
            .rule(code)
            .and_then(|rule| registry.severity_for(rule, &severities))
    };
    let mut used = vec![false; directives.len()];
    let mut suppressed = Vec::new();
    let mut kept = Vec::new();
    for collected in std::mem::take(&mut reported.diagnostics) {
        let d = &collected.diagnostic;
        let hit = (collected.source == oxabl_analyze::DiagnosticSource::Lint
            && d.span.file == ROOT_FILE_ID)
            .then(|| suppress::suppressor(&directives, source, d.code.0, d.span.span.start))
            .flatten();
        match hit {
            Some(i) => {
                used[i] = true;
                let suppress::DirectiveKind::Valid { reason, .. } = &directives[i].kind else {
                    unreachable!("only a valid directive covers a finding");
                };
                suppressed.push((collected, reason.clone()));
            }
            None => kept.push(collected),
        }
    }
    let span = |d: &suppress::Directive| {
        FileSpan::from((
            ROOT_FILE_ID,
            oxabl_ast::Span {
                start: d.start,
                end: d.end,
            },
        ))
    };
    for (i, d) in directives.iter().enumerate() {
        let found = match &d.kind {
            suppress::DirectiveKind::Invalid(why) => {
                let Some(severity) = severity_of(suppress::LINT0007) else {
                    continue;
                };
                let message = match why {
                    suppress::Invalid::NoCode => "`oxabl-ignore` names no rule".to_string(),
                    suppress::Invalid::NoReason(code) => {
                        format!("`oxabl-ignore {code}` has no reason, so it suppresses nothing")
                    }
                    suppress::Invalid::UnknownCode(code) => {
                        format!(
                            "`oxabl-ignore` names unknown rule `{code}`, so it suppresses nothing"
                        )
                    }
                };
                diagnostic(severity, suppress::LINT0007, message, span(d)).with_help(
                    "Write `oxabl-ignore <CODE>: <reason>` with a rule code and a reason."
                        .to_string(),
                )
            }
            suppress::DirectiveKind::Valid { code, .. } if !used[i] => {
                let Some(severity) = severity_of(suppress::LINT0008) else {
                    continue;
                };
                let judged = severity_of(code).is_some()
                    && (!context_free_only
                        || registry.rule(code).is_some_and(|r| r.meta().context_free));
                if !judged {
                    continue;
                }
                diagnostic(
                    severity,
                    suppress::LINT0008,
                    format!(
                        "`oxabl-ignore {code}` suppresses no finding on line {}",
                        d.target_line
                    ),
                    span(d),
                )
            }
            suppress::DirectiveKind::Valid { .. } => continue,
        };
        kept.push(CollectedDiagnostic {
            diagnostic: found,
            source: oxabl_analyze::DiagnosticSource::Lint,
        });
    }
    reported.diagnostics = kept;
    (reported, suppressed)
}

fn diagnostic(
    severity: oxabl_common::Severity,
    code: &'static str,
    message: String,
    span: FileSpan,
) -> Diagnostic {
    Diagnostic {
        severity,
        code: DiagnosticCode(code),
        message,
        span,
        labels: Vec::new(),
        help: None,
        reach: None,
        related: None,
    }
}
/// Include files read for `--include-findings`, each at most once per worker.
struct IncludeTexts {
    decoding: SourceDecoding,
    texts: HashMap<PathBuf, Option<String>>,
}

impl IncludeTexts {
    fn new(decoding: SourceDecoding) -> Self {
        IncludeTexts {
            decoding,
            texts: HashMap::new(),
        }
    }

    fn get(&mut self, path: &Path) -> Option<&str> {
        self.texts
            .entry(path.to_path_buf())
            .or_insert_with(|| read_source(path, self.decoding).ok())
            .as_deref()
    }
}

/// Write `text` to `out` in the encoding `stored` was read in, so `--stdout`
/// hands back the bytes a file would hold.
fn write_stored(
    stored: &oxabl_workspace::DecodedSource,
    text: &str,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    out.write_all(&stored.encode(text)?)
}

/// The text of the source file at `path`, decoded as `decoding`.
fn read_source(path: &Path, decoding: SourceDecoding) -> std::io::Result<String> {
    decoding.read_file(path).map(|source| source.text)
}

/// `path:line:col` of a reference at `at` in `text`, or just `path` when the
/// text is not known.
fn site_label(path: &str, text: Option<&str>, at: Option<oxabl_ast::Span>) -> String {
    match (text, at) {
        (Some(text), Some(at)) => {
            let (line, column) = SourceMap::new(text).lookup(at.start as usize);
            format!("{path}:{line}:{column}")
        }
        (_, None) => format!("{path} (`{{&...}}` substitution)"),
        _ => path.to_string(),
    }
}

/// Print (text mode) and collect (JSON rows) the findings in includes that
/// `root`'s expansion produced.
///
/// Positions come from the physical include file, read from disk. A file that
/// cannot be read keeps its byte span and loses line and column. Every
/// hard-coded secret of an include (`collected.secrets_in`) is masked in its
/// shown lines and in its hashes, whatever finding covers it.
fn report_include_findings(
    root: &str,
    root_source: &str,
    found: &[oxabl_analyze::IncludeOriginDiagnostic],
    collected: &CollectedDiagnostics,
    texts: &mut IncludeTexts,
    pass: &CheckPass,
    unit: &mut Unit,
) {
    for finding in found {
        let display = finding.path.display().to_string();
        let mut diagnostic = finding.diagnostic.clone();
        diagnostic.span.file = ROOT_FILE_ID;
        let text = texts.get(&finding.path).map(str::to_owned);
        let secrets = text.as_deref().map_or_else(Vec::new, |text| {
            baseline::secrets(text, collected.secrets_in(Some(&finding.path), text))
        });
        if pass.output == CheckFormat::Sarif {
            let key = baseline::relative_path(&pass.root, &finding.path)
                .unwrap_or_else(|| display.clone());
            let region = text.as_deref().map(|text| {
                sarif::Region::of_span(text, diagnostic.span.span.start, diagnostic.span.span.end)
            });
            // The root is part of the identity: one include reached from two
            // roots is two findings.
            let text = text.as_deref().unwrap_or("");
            let entries =
                baseline::fingerprint(&format!("{key}\0{root}"), text, [&diagnostic], &secrets);
            unit.sarif_results.push(sarif::Finding {
                rule_id: diagnostic.code.0.to_string(),
                severity: sarif::level(diagnostic.severity.as_str()),
                text: match &diagnostic.help {
                    Some(help) => format!("{}\n{help}", diagnostic.message),
                    None => diagnostic.message.clone(),
                },
                uri: sarif::uri(&key),
                region,
                fingerprint: baseline::finding_fingerprint(&entries[0]),
                baseline_state: None,
                suppressed: None,
                // The labels of a finding in an include point into other files.
                flow: Vec::new(),
                reach: diagnostic.reach.as_deref().cloned(),
                related: related_locations(&diagnostic, &pass.root),
            });
        }
        if pass.output == CheckFormat::Text {
            let shown = baseline::masked(text.as_deref().unwrap_or(""), &secrets);
            let resolver = SourceResolver::new(
                if text.is_some() {
                    ROOT_FILE_ID
                } else {
                    FileId::UNKNOWN
                },
                display.clone(),
                &shown,
            );
            unit.output
                .out(render_diagnostics(&[diagnostic.clone()], &resolver));
            for expansion in &finding.expansions {
                // Each site is in the file that wrote it: the root, then the
                // include before it in the chain.
                let mut previous = (root.to_string(), Some(root_source.to_string()));
                let mut via = Vec::new();
                for step in &expansion.chain {
                    via.push(site_label(&previous.0, previous.1.as_deref(), step.site));
                    previous = (
                        step.path.display().to_string(),
                        texts.get(&step.path).map(str::to_owned),
                    );
                }
                let nth = if expansion.occurrences > 1 {
                    format!(
                        " (expansion {} of {})",
                        expansion.occurrence, expansion.occurrences
                    )
                } else {
                    String::new()
                };
                unit.output.out(format!(
                    "  note: in the compilation unit of {root}, not a workspace-wide result; \
                     reached through {}{nth}\n",
                    via.join(" -> ")
                ));
            }
        }
        let resolved = text
            .as_deref()
            .map(|text| position::resolve_diagnostic(&SourceMap::new(text), &diagnostic));
        let expansions = finding
            .expansions
            .iter()
            .map(|expansion| {
                let mut previous = Some(root_source.to_string());
                let chain = expansion
                    .chain
                    .iter()
                    .map(|step| {
                        let position = match (&previous, step.site) {
                            (Some(text), Some(at)) => {
                                Some(SourceMap::new(text).lookup(at.start as usize))
                            }
                            _ => None,
                        };
                        previous = texts.get(&step.path).map(str::to_owned);
                        CheckJsonChainStep {
                            path: step.path.display().to_string(),
                            site: step.site.map(|at| CheckJsonSpan {
                                start: at.start,
                                end: at.end,
                            }),
                            line: position.map(|(line, _)| line),
                            column: position.map(|(_, column)| column),
                        }
                    })
                    .collect();
                CheckJsonExpansion {
                    occurrence: expansion.occurrence,
                    occurrences: expansion.occurrences,
                    chain,
                }
            })
            .collect();
        unit.include_diagnostics.push(CheckJsonIncludeFinding {
            path: display,
            root: root.to_string(),
            scope: "compilation-unit",
            code: diagnostic.code.0.to_string(),
            severity: diagnostic.severity.as_str(),
            source: finding.source.as_str(),
            message: diagnostic.message.clone(),
            help: diagnostic.help.clone(),
            span: CheckJsonSpan {
                start: diagnostic.span.span.start,
                end: diagnostic.span.span.end,
            },
            start: resolved.map(|r| r.start.into()),
            end: resolved.map(|r| r.end.into()),
            expansions,
        });
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
        reach: d.reach.as_deref().cloned(),
        related: d
            .related()
            .iter()
            .map(|l| {
                let resolved = (l.span.file == ROOT_FILE_ID)
                    .then(|| position::resolve_file_span(map, &l.span));
                CheckJsonRelated {
                    message: l.message.clone(),
                    span: CheckJsonSpan {
                        start: l.span.span.start,
                        end: l.span.span.end,
                    },
                    start: resolved.map(|r| r.start.into()),
                    end: resolved.map(|r| r.end.into()),
                }
            })
            .collect(),
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

    let source = match read_source(path, config.decoding) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read {}: {e}", path.display());
            return ExitCode::from(2);
        }
    };

    let fs = config.file_system();
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
    let mut notes = Output::default();
    surface_collected_preproc(path, &source, &collected, &mut notes);
    if !surface_source_context(sem, &mut notes) {
        surface_unjudged_symbols(sem, &mut notes);
    }
    notes.flush();

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
    jobs: usize,
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
    // The source encoding is configuration too, so the walk resolves it with or
    // without preprocessing.
    let path_config = {
        let overrides = ConfigOverrides {
            include_paths: if preprocess {
                include_paths.to_vec()
            } else {
                Vec::new()
            },
            ..Default::default()
        };
        let (config, warnings) = PipelineConfig::resolve_style_only(path, &overrides);
        for warning in &warnings {
            eprintln!("warning: {warning}");
        }
        config
    };
    let effective_paths = &path_config.include_paths;

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
        let real_fs = path_config.file_system();
        for file in &files {
            run_debug_parse(
                file,
                preprocess,
                &real_fs,
                &path_config.include_paths_for(Some(file)),
            );
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
    let real_fs = path_config.file_system();

    // Parse all files, on `jobs` threads, reported in file order.
    let start = Instant::now();
    let mut results = Vec::with_capacity(files.len());
    in_task_order(
        jobs.min(files.len()),
        &files,
        (),
        || (),
        |(), file| {
            if preprocess {
                parse_file_with_preprocess(
                    file,
                    &real_fs,
                    &path_config.include_paths_for(Some(file)),
                )
            } else {
                (parse_file(file, path_config.decoding), String::new())
            }
        },
        |(result, messages)| {
            eprint!("{messages}");
            results.push(result);
            progress.inc(1);
        },
    );

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

fn parse_file(path: &Path, decoding: SourceDecoding) -> FileResult {
    // Read the file
    let source = match read_source(path, decoding) {
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

/// Preprocess and parse one root for `conformance`. The text is the root's
/// rendered loud preprocessor diagnostics, for stderr.
fn parse_file_with_preprocess(
    path: &Path,
    fs: &EncodedFileSystem,
    include_paths: &[PathBuf],
) -> (FileResult, String) {
    // Read the file
    let source = match fs.read(path).map(|text| text.to_string()) {
        Ok(s) => s,
        Err(e) => {
            let result = FileResult::IoError {
                path: path.to_path_buf(),
                error: e.to_string(),
            };
            return (result, String::new());
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
            let result = FileResult::ParseError {
                path: path.to_path_buf(),
                line,
                col,
                message: format!("[preprocess] {msg}"),
            };
            return (result, String::new());
        }
    };

    // Surface loud preprocessor diagnostics — errors plus PREPROC007
    // (unresolvable include), PREPROC008 (unterminated include), PREPROC009
    // (valueless predefined name), PREPROC010 (unevaluated condition) and PREPROC002 (unclosed &IF) — so true
    // preproc problems aren't hidden. Other warnings stay quiet.
    let messages = render_preproc_diagnostics(path, &source, file_id, &preprocessed.diagnostics);

    // Get the expanded source text
    let expanded = preprocessed.to_text();

    // Parse the preprocessed source through the shared guard (see `parse_file`).
    let program = match oxabl::try_parse(&expanded) {
        Ok(program) => program,
        Err(_) => {
            let result = FileResult::LexerPanic {
                path: path.to_path_buf(),
            };
            return (result, messages);
        }
    };

    // Fail-fast reporting: surface the first recovered error.
    let result = match program.first_error() {
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
    };
    (result, messages)
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
        // Ties by text, so the report does not follow the map's random order.
        patterns.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(b.0)));

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
    // Ties by text, so the report does not follow the map's random order.
    patterns.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));

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

fn run_debug_parse(
    path: &Path,
    preprocess: bool,
    fs: &EncodedFileSystem,
    include_paths: &[PathBuf],
) {
    let source = match fs.read(path).map(|text| text.to_string()) {
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
