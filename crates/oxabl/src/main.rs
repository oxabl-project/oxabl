use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::Instant;

use clap::Parser as ClapParser;
use indicatif::{ProgressBar, ProgressStyle};
use oxabl_common::{FileId, SourceMap};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::RealFileSystem;
use serde::Serialize;
use walkdir::WalkDir;

/// ABL file extensions to scan for (lowercase)
const ABL_EXTENSIONS: &[&str] = &["p", "w", "i", "cls", "v"];

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
    },
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

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli {
        Cli::Check {
            path,
            json,
            preprocess,
            include_paths,
        } => run_check(&path, json, preprocess, &include_paths),
    }
}

fn run_check(
    path: &Path,
    json_output: bool,
    preprocess: bool,
    include_paths: &[PathBuf],
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

    if !json_output {
        eprintln!("Found {} ABL files", files.len());
        if preprocess {
            eprintln!(
                "Preprocessing enabled with {} include path(s)",
                include_paths.len()
            );
        }
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
            results.push(parse_file_with_preprocess(file, &real_fs, include_paths));
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
        patterns.sort_by(|a, b| b.1.cmp(&a.1));

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
    patterns.sort_by(|a, b| b.1.cmp(&a.1));

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
