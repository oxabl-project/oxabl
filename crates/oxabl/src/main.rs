use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::Instant;

use clap::Parser as ClapParser;
use indicatif::{ProgressBar, ProgressStyle};
use oxabl_analyze::{
    CollectedDiagnostics, collect_with_model, dump_json_with_diagnostics,
    dump_text_with_diagnostics,
};
use oxabl_common::{Diagnostic, FileId, SourceMap};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::{Schema, SchemaLoader};
use oxabl_style::StyleGuide;
use oxabl_workspace::{
    RealFileSystem, resolved_include_paths, resolved_lint_config, resolved_style,
};
use serde::Serialize;
use walkdir::WalkDir;

/// ABL file extensions to scan for (lowercase)
const ABL_EXTENSIONS: &[&str] = &["p", "w", "cls", "v"];

#[derive(ClapParser)]
#[command(name = "oxabl", about = "High-performance tooling for Progress ABL")]
enum Cli {
    /// Parse ABL files and report what succeeds and what fails
    Check {
        /// Path to a directory or single file to check
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
    Analyze {
        /// Path to the ABL source file to analyze.
        path: PathBuf,

        /// Output format: `json` (stable, versioned) or `text` (human-oriented).
        #[arg(long, default_value = "json")]
        format: String,

        /// Skip the lint pass (semantic-layer diagnostics only).
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

/// A preprocessor diagnostic surfaced to machine-readable output.
#[derive(Serialize)]
struct JsonDiagnostic {
    file: String,
    code: String,
    severity: String,
    /// Absent for diagnostics that originate in a nested include: their offset
    /// is meaningful only against the include's own source, not the root file,
    /// so we omit a position rather than emit a misleading `0`.
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    col: Option<usize>,
    message: String,
}

/// Surface *loud* preprocessor diagnostics — errors plus selected warnings —
/// to stderr and collect them for machine-readable output.
///
/// Always-loud warning codes:
/// - `PREPROC007` unresolvable include (symbol loss)
/// - `PREPROC002` unclosed `&IF` (inline/`skip_to_eol` regressions; #65 gate)
///
/// Line/col is computed against the root source only when the diagnostic
/// belongs to the root file (`d.span.file == root_file_id`). Diagnostics from a
/// nested include carry a different `FileId`; resolving their offset against the
/// root `SourceMap` would print a garbage position, so those are reported
/// without a (misleading) root-relative line/col.
fn surface_preproc_diagnostics(
    path: &Path,
    root_source: &str,
    root_file_id: FileId,
    diagnostics: &[Diagnostic],
) -> Vec<JsonDiagnostic> {
    let mut out = Vec::new();
    // Share the surfacing rule with the collector so the two never drift (U4).
    let is_loud = oxabl_analyze::is_loud;
    // Only build a SourceMap if there's actually a loud diagnostic to place.
    if !diagnostics.iter().any(is_loud) {
        return out;
    }
    let source_map = SourceMap::new(root_source);
    for d in diagnostics.iter().filter(|d| is_loud(d)) {
        let severity = format!("{:?}", d.severity).to_lowercase();
        if d.span.file == root_file_id {
            let (line, col) = source_map.lookup(d.span.span.start as usize);
            eprintln!(
                "{}:{}:{} [preprocess {}] {}",
                path.display(),
                line,
                col,
                d.code.0,
                d.message
            );
            out.push(JsonDiagnostic {
                file: path.display().to_string(),
                code: d.code.0.to_string(),
                severity,
                line: Some(line),
                col: Some(col),
                message: d.message.clone(),
            });
        } else {
            eprintln!(
                "{}: [preprocess {}] in included file: {}",
                path.display(),
                d.code.0,
                d.message
            );
            out.push(JsonDiagnostic {
                file: path.display().to_string(),
                code: d.code.0.to_string(),
                severity,
                line: None,
                col: None,
                message: d.message.clone(),
            });
        }
    }
    out
}

/// Surface loud, root-origin preprocessor diagnostics from the shared
/// collector to stderr, and collect them for the `analyze` JSON channel.
///
/// The collector already filtered to the loud set and dropped include-origin
/// diagnostics (R8), so every entry here is root-relative and gets a concrete
/// line/col from the root source.
fn surface_collected_preproc(
    path: &Path,
    source: &str,
    collected: &CollectedDiagnostics,
) -> Vec<JsonDiagnostic> {
    let mut out = Vec::new();
    let mut source_map: Option<SourceMap> = None;
    for c in collected.by_source(oxabl_analyze::DiagnosticSource::Preproc) {
        let d = &c.diagnostic;
        let sm = source_map.get_or_insert_with(|| SourceMap::new(source));
        let (line, col) = sm.lookup(d.span.span.start as usize);
        let severity = format!("{:?}", d.severity).to_lowercase();
        eprintln!(
            "{}:{}:{} [preprocess {}] {}",
            path.display(),
            line,
            col,
            d.code.0,
            d.message
        );
        out.push(JsonDiagnostic {
            file: path.display().to_string(),
            code: d.code.0.to_string(),
            severity,
            line: Some(line),
            col: Some(col),
            message: d.message.clone(),
        });
    }
    out
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli {
        Cli::Check {
            path,
            json,
            preprocess,
            include_paths,
            debug,
        } => run_check(&path, json, preprocess, &include_paths, debug),
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

/// The pure per-file formatting decision, separated from the write/print/exit
/// shell so it is trivially testable (KTD6).
enum FormatOutcome {
    /// `format()` produced output byte-identical to the input.
    Unchanged,
    /// `format()` produced different output (the new bytes).
    Reformatted(String),
    /// The file could not be formatted faithfully; leave it unchanged. Carries
    /// the human-readable reason (a `FormatBail` or a lexer panic).
    Bailed(String),
}

/// Parse `source` raw (preprocessing OFF, per KTD4/R8 so spans are real byte
/// offsets) and run the formatter, classifying the outcome. The raw
/// `tokenize`/`parse_program` is wrapped in `catch_unwind` — the lexer can panic
/// on some inputs, and one bad file must not unwind the whole directory walk
/// after earlier files were already rewritten (R7.1b). A panic is treated as a
/// bail: the file is reported and left unchanged.
fn format_one(source: &str, style: &StyleGuide) -> FormatOutcome {
    let parsed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let tokens = tokenize(source);
        Parser::new(&tokens, source).parse_program()
    }));
    let program = match parsed {
        Ok(p) => p,
        Err(_) => return FormatOutcome::Bailed("lexer panicked; left unchanged".to_string()),
    };
    match oxabl_formatter::format(source, &program, style) {
        Ok(formatted) if formatted == source => FormatOutcome::Unchanged,
        Ok(formatted) => FormatOutcome::Reformatted(formatted),
        Err(bail) => FormatOutcome::Bailed(bail.to_string()),
    }
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

/// `oxabl format`: resolve a [`StyleGuide`], then format each discovered file
/// per the selected mode. See KTD5 for the per-mode exit-code contract.
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
    let (style_guide, cfg_err) = resolved_style(path, cli_style);
    if let Some(err) = cfg_err {
        eprintln!("warning: {err}");
    }

    // Discover files (exit 2 on path-not-found / no ABL files, matching `check`).
    let files = match discover_files(path) {
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

    // --stdout is single-file only; a directory + --stdout is a usage error.
    if stdout && path.is_dir() {
        eprintln!("error: --stdout requires a single file, not a directory");
        return ExitCode::from(2);
    }

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

        match format_one(&source, &style_guide) {
            FormatOutcome::Unchanged => {
                // In --stdout mode we still emit the (unchanged) content.
                if stdout {
                    print!("{source}");
                }
            }
            FormatOutcome::Bailed(reason) => {
                // R7.1b: file left byte-for-byte unchanged; reason to stderr; not
                // a failure (write) and counted as "no change" (--check).
                eprintln!("{}: {reason}", file.display());
                if stdout {
                    print!("{source}");
                }
            }
            FormatOutcome::Reformatted(formatted) => {
                if check {
                    eprintln!("{}: would reformat", file.display());
                    any_would_change = true;
                } else if stdout {
                    print!("{formatted}");
                } else if let Err(e) = std::fs::write(file, &formatted) {
                    eprintln!("error: cannot write {}: {e}", file.display());
                    any_io_error = true;
                }
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

    // Resolve include paths (only meaningful when preprocessing).
    let paths = if preprocess {
        let (paths, cfg_err) = resolved_include_paths(path, include_paths);
        if let Some(err) = cfg_err {
            eprintln!("warning: {err}");
        }
        paths
    } else {
        Vec::new()
    };

    // Load the schema when `--schema` was passed. Load diagnostics are
    // reported but non-fatal — a partially-loaded schema still drives
    // resolution. `schema_loaded` is set explicitly (not derived from
    // `Schema::is_empty`) so an intentionally empty `.df` still reads as
    // "loaded" to schema-dependent diagnostics.
    let (schema, schema_loaded) = match schema_path {
        Some(p) => {
            let fs = RealFileSystem;
            let (schema, diags) = SchemaLoader::load_files(&[p.to_path_buf()], &fs);
            for d in &diags {
                eprintln!("schema: [{}] {}", d.code.0, d.message);
            }
            (schema, true)
        }
        None => (Schema::empty(), false),
    };

    // Diagnostics come from the shared collector so the CLI and the LSP can
    // never drift (R7). The collector uses `parse_program` error recovery, so
    // `analyze` now surfaces semantic/lint diagnostics even on a parse error
    // instead of aborting — a deliberate behavior change (U4).
    let fs = RealFileSystem;
    let root = FileId::new(1);
    // Resolve the `[workspace.lint]` severity surface (CLI has no lint flags yet,
    // so overrides are empty): CLI > oxabl.toml > default (R15).
    let (lint_config, lint_err) = resolved_lint_config(path, &[]);
    if let Some(err) = lint_err {
        eprintln!("warning: {err}");
    }
    let lint_severities = lint_config.to_severity_map();
    let (sem_opt, collected) = collect_with_model(
        root,
        &source,
        &fs,
        &paths,
        &schema,
        schema_loaded,
        &lint_severities,
        preprocess,
    );

    let sem = match sem_opt {
        Some(sem) => sem,
        None => {
            // Fatal preprocessing failure: no model to dump.
            let msg = collected
                .by_source(oxabl_analyze::DiagnosticSource::Preproc)
                .next()
                .map(|c| c.diagnostic.message.as_str())
                .unwrap_or("unknown");
            eprintln!("error: preprocessing failed: {msg}");
            return ExitCode::from(3);
        }
    };

    // Honor `--no-lint` by dropping the lint-sourced diagnostics.
    let collected = if no_lint {
        oxabl_analyze::CollectedDiagnostics {
            diagnostics: collected
                .diagnostics
                .into_iter()
                .filter(|c| c.source != oxabl_analyze::DiagnosticSource::Lint)
                .collect(),
        }
    } else {
        collected
    };

    // CLI-owned channel (not part of the versioned analyze envelope): surface
    // unresolvable-include / preprocessor diagnostics to stderr so machine and
    // human consumers see the same "loud, not silent" signal.
    let preproc_diags = surface_collected_preproc(path, &source, &collected);

    match format {
        "json" => {
            let mut v = dump_json_with_diagnostics(&sem, &collected);
            if let serde_json::Value::Object(ref mut map) = v {
                map.insert(
                    "preproc_diagnostics".to_string(),
                    serde_json::to_value(&preproc_diags).unwrap_or(serde_json::Value::Null),
                );
            }
            match serde_json::to_string_pretty(&v) {
                Ok(s) => println!("{s}"),
                Err(e) => {
                    eprintln!("error: json serialize: {e}");
                    return ExitCode::from(6);
                }
            }
        }
        "text" => {
            print!("{}", dump_text_with_diagnostics(&sem, &collected));
        }
        other => {
            eprintln!("error: unsupported format `{other}` (use `json` or `text`)");
            return ExitCode::from(7);
        }
    }

    ExitCode::SUCCESS
}

fn run_check(
    path: &Path,
    json_output: bool,
    preprocess: bool,
    include_paths: &[PathBuf],
    debug: bool,
) -> ExitCode {
    // Discover files
    let files = match discover_files(path) {
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

    // Merge CLI `-I` flags with any auto-discovered `oxabl.toml` include paths.
    let effective_paths: Vec<PathBuf> = if preprocess {
        let (merged, cfg_err) = resolved_include_paths(path, include_paths);
        if let Some(err) = cfg_err {
            eprintln!("warning: {err}");
        }
        merged
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
        render_json_report(&results, elapsed.as_secs_f64());
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

fn discover_files(path: &Path) -> Result<Vec<PathBuf>, String> {
    if !path.exists() {
        return Err(format!("Path does not exist: {}", path.display()));
    }

    // Single file mode
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }

    if !path.is_dir() {
        return Err(format!(
            "Path is not a file or directory: {}",
            path.display()
        ));
    }

    let mut files = Vec::new();

    for entry in WalkDir::new(path).follow_links(true) {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue, // skip unreadable directories
        };

        if !entry.file_type().is_file() {
            continue;
        }

        if let Some(ext) = entry.path().extension() {
            let ext_lower = ext.to_string_lossy().to_lowercase();
            if ABL_EXTENSIONS.contains(&ext_lower.as_str()) {
                files.push(entry.into_path());
            }
        }
    }

    files.sort();
    Ok(files)
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

    // Tokenize with panic catching
    let tokens = match std::panic::catch_unwind(|| tokenize(&source)) {
        Ok(tokens) => tokens,
        Err(_) => {
            return FileResult::LexerPanic {
                path: path.to_path_buf(),
            };
        }
    };

    // Parse
    let mut parser = Parser::new(&tokens, &source);
    match parser.parse_statements() {
        Ok(_) => FileResult::Success,
        Err(e) => {
            let source_map = SourceMap::new(&source);
            let (line, col) = source_map.lookup(e.span.start as usize);
            FileResult::ParseError {
                path: path.to_path_buf(),
                line,
                col,
                message: e.message,
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
    let file_id = FileId::new(1);
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

    // Tokenize with panic catching
    let tokens =
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| tokenize(&expanded))) {
            Ok(tokens) => tokens,
            Err(_) => {
                return FileResult::LexerPanic {
                    path: path.to_path_buf(),
                };
            }
        };

    // Parse the preprocessed token stream
    let mut parser = Parser::new(&tokens, &expanded);
    match parser.parse_statements() {
        Ok(_) => FileResult::Success,
        Err(e) => {
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
                    message: e.message,
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

fn render_json_report(results: &[FileResult], elapsed_secs: f64) {
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

    println!(
        "{}",
        serde_json::to_string_pretty(&report).expect("JSON serialization failed")
    );
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
        let file_id = FileId::new(1);
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

    let tokens =
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| tokenize(&parse_source))) {
            Ok(tokens) => tokens,
            Err(_) => {
                eprintln!("Lexer panic: {}", path.display());
                return;
            }
        };

    let mut parser = Parser::new(&tokens, &parse_source);
    let program = parser.parse_program();

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
