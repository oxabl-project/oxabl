//! scope_corpus — run `oxabl_scope` over every ABL file under a tree and
//! report buffers by scope kind, transaction blocks by cause, record reads by
//! lock, Unknown answers by reason, panics and time.
//!
//! Every `.p`, `.w` and `.cls` file (also `.v` and `.i` with
//! `OXABL_SCOPE_INCLUDES=1`; an include analysed alone lacks its includer's
//! definitions) is expanded the way
//! `oxabl check` expands it (the tree's `oxabl.toml`, its schema, preprocessing
//! on), then lexed, parsed and analysed. Each step runs under a panic guard.
//!
//! # Usage
//! ```text
//! OXABL_SCOPE_CORPUS=<dir-or-file> cargo run -p oxabl --release --example scope_corpus
//! ```
//! With `OXABL_SCOPE_DUMP=1` and a single file, print that file's dump;
//! `OXABL_SCOPE_EXPANDED=<path>` also writes the expanded text there.
//! `OXABL_SCOPE_SAMPLES=<n>` prints `n` sampled answers (every k-th, spread
//! over the tree) with `file:line`, for reading against the code. Lines are in
//! the root file; a site inside an include shows the `{...}` line, marked
//! `via`. `OXABL_SCOPE_VERBOSE=1` lists the files that panicked or failed.
//! `OXABL_SCOPE_EXPLAIN=<reason>` prints the text at every Unknown with that
//! reason (`ambiguous-field`, `dynamic-buffer`, ...), for triage.

use std::collections::BTreeMap;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use oxabl_analyze::ExpandedFile;
use oxabl_ast::Span;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_scope::{FileScopes, Lock, ScopeOptions, TxKind};
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const UNITS: &[&str] = &["p", "w", "cls"];
const INCLUDES: &[&str] = &["v", "i"];

#[derive(Default)]
struct Report {
    files: usize,
    unreadable: usize,
    preprocess_failed: usize,
    pipeline_panics: usize,
    parse_recovered: usize,
    scope_panics: usize,
    units: usize,
    blocks: usize,
    buffers: usize,
    buffers_by_kind: BTreeMap<&'static str, usize>,
    buffers_unreferenced: usize,
    buffers_external: usize,
    buffers_known: usize,
    buffers_unknown: usize,
    unknown_buffers_by_reason: BTreeMap<&'static str, usize>,
    scopes_by_kind: BTreeMap<&'static str, usize>,
    raised_to_routine: usize,
    raised_across_units: usize,
    capable_blocks: usize,
    tx_explicit: usize,
    tx_implicit_by_cause: BTreeMap<&'static str, usize>,
    tx_unknown_by_reason: BTreeMap<&'static str, usize>,
    tx_routine: usize,
    tx_routine_by_unit: BTreeMap<&'static str, usize>,
    tx_nested: usize,
    skipped_forms: BTreeMap<String, usize>,
    unknown_tables: BTreeMap<String, usize>,
    reads_by_kind_lock: BTreeMap<(&'static str, &'static str), usize>,
    default_reads_written: usize,
    default_reads_maybe_written: usize,
    samples: Vec<String>,
    expand_parse_time: Duration,
    scope_time: Duration,
    failed: Vec<(PathBuf, &'static str)>,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_SCOPE_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_SCOPE_CORPUS to a directory or file");
        std::process::exit(2);
    };
    let samples: usize = std::env::var("OXABL_SCOPE_SAMPLES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(0);
    let verbose = std::env::var_os("OXABL_SCOPE_VERBOSE").is_some();
    let files = discover(&root);
    if files.is_empty() {
        eprintln!("no ABL files under {}", root.display());
        std::process::exit(2);
    }
    let (config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let schema = config.schema_loaded.then_some(&config.schema);
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);

    if std::env::var_os("OXABL_SCOPE_DUMP").is_some() {
        let file = &files[0];
        let source = std::fs::read_to_string(file).expect("readable file");
        let expansion = pipeline.with_file(file).expand(&source);
        let text = expansion.text().unwrap_or(&source);
        if let Some(out) = std::env::var_os("OXABL_SCOPE_EXPANDED") {
            std::fs::write(out, text).expect("write the expanded text");
        }
        let tokens = oxabl_lexer::tokenize(text);
        let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
        let options = ScopeOptions {
            parse_recovered: !program.errors.is_empty(),
            ..ScopeOptions::default()
        };
        print!(
            "{}",
            oxabl_scope::analyze_file_with(&program.statements, text, schema, &options).dump(text)
        );
        return;
    }

    let mut r = Report::default();
    let started = Instant::now();
    for file in &files {
        run_file(&pipeline, schema, file, &mut r);
    }
    let wall = started.elapsed();
    print_report(&r, wall, samples, verbose);
}

fn discover(root: &Path) -> Vec<PathBuf> {
    if root.is_file() {
        return vec![std::path::absolute(root).unwrap_or_else(|_| root.to_path_buf())];
    }
    let includes = std::env::var_os("OXABL_SCOPE_INCLUDES").is_some();
    let wanted = |x: &str| {
        UNITS.iter().any(|k| k.eq_ignore_ascii_case(x))
            || includes && INCLUDES.iter().any(|k| k.eq_ignore_ascii_case(x))
    };
    let mut files: Vec<PathBuf> = WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            e.depth() == 0
                || !(e.file_type().is_dir() && e.file_name().to_string_lossy().starts_with('.'))
        })
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            e.path()
                .extension()
                .and_then(|x| x.to_str())
                .is_some_and(wanted)
        })
        .map(|e| std::path::absolute(e.path()).unwrap_or_else(|_| e.into_path()))
        .collect();
    files.sort();
    files
}

fn run_file(
    pipeline: &LintPipeline,
    schema: Option<&oxabl_schema::Schema>,
    file: &Path,
    r: &mut Report,
) {
    r.files += 1;
    let Ok(bytes) = std::fs::read(file) else {
        r.unreadable += 1;
        r.failed.push((file.to_path_buf(), "unreadable"));
        return;
    };
    let source = String::from_utf8_lossy(&bytes).into_owned();

    let t = Instant::now();
    let parsed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let expansion = pipeline.with_file(file).expand(&source);
        let expanded = expansion.expanded()?.clone();
        let text = expanded.text.to_string();
        let tokens = oxabl_lexer::tokenize(&text);
        let program = oxabl_parser::Parser::new(&tokens, &text).parse_program();
        Some((expanded, text, tokens, program))
    }));
    r.expand_parse_time += t.elapsed();
    let (expanded, text, tokens, program) = match parsed {
        Ok(Some(p)) => p,
        Ok(None) => {
            r.preprocess_failed += 1;
            r.failed.push((file.to_path_buf(), "preprocess failed"));
            return;
        }
        Err(_) => {
            r.pipeline_panics += 1;
            r.failed.push((file.to_path_buf(), "pipeline panic"));
            return;
        }
    };
    let recovered = !program.errors.is_empty();
    if recovered {
        r.parse_recovered += 1;
    }
    let options = ScopeOptions {
        parse_recovered: recovered,
        ..ScopeOptions::default()
    };

    let t = Instant::now();
    let analysed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        oxabl_scope::analyze_tokens(&program.statements, &text, &tokens, schema, &options)
    }));
    r.scope_time += t.elapsed();
    let scopes = match analysed {
        Ok(s) => s,
        Err(_) => {
            r.scope_panics += 1;
            r.failed.push((file.to_path_buf(), "scope panic"));
            return;
        }
    };
    let lines = Lines::new(&source);
    if let Some(reason) = std::env::var_os("OXABL_SCOPE_EXPLAIN") {
        explain(
            &scopes,
            &text,
            file,
            &expanded,
            &lines,
            &reason.to_string_lossy(),
        );
    }
    count(&scopes, &text, file, &expanded, &lines, r);
}

/// Print the text at every Unknown with `reason`.
fn explain(
    fs: &FileScopes,
    text: &str,
    file: &Path,
    expanded: &ExpandedFile,
    lines: &Lines,
    reason: &str,
) {
    let mut seen = std::collections::BTreeSet::new();
    let unknowns = fs
        .buffers()
        .iter()
        .flat_map(|b| b.unknown.iter().copied())
        .chain(fs.units().iter().flat_map(|u| {
            u.blocks.iter().filter_map(|b| match b.transaction.kind {
                TxKind::Unknown(un) => Some(un),
                _ => None,
            })
        }));
    for u in unknowns {
        if u.reason.as_str() != reason || !seen.insert(u.at.start) {
            continue;
        }
        let start = text[..u.at.start as usize].rfind('\n').map_or(0, |i| i + 1);
        let end = text[u.at.end as usize..]
            .find('\n')
            .map_or(text.len(), |i| u.at.end as usize + i);
        eprintln!(
            "{}:{}: {}",
            file.display(),
            site(expanded, lines, u.at),
            text[start..end].trim()
        );
    }
}

fn count(
    fs: &FileScopes,
    text: &str,
    file: &Path,
    expanded: &ExpandedFile,
    lines: &Lines,
    r: &mut Report,
) {
    r.units += fs.units().len();
    for u in fs.units() {
        r.blocks += u.blocks.len();
        for b in &u.blocks {
            if b.transaction_capable {
                r.capable_blocks += 1;
            }
            match b.transaction.kind {
                TxKind::None => {}
                TxKind::Explicit => r.tx_explicit += 1,
                TxKind::Implicit(up) => {
                    *r.tx_implicit_by_cause.entry(up.kind.as_str()).or_default() += 1;
                    if b.parent.is_none() {
                        r.tx_routine += 1;
                        *r.tx_routine_by_unit.entry(u.kind.as_str()).or_default() += 1;
                    }
                }
                TxKind::Unknown(un) => {
                    *r.tx_unknown_by_reason
                        .entry(un.reason.as_str())
                        .or_default() += 1;
                    if un.reason == oxabl_scope::UnknownReason::SkippedStatement {
                        let form = text
                            .get(un.at.start as usize..)
                            .map(statement_word)
                            .unwrap_or_default();
                        *r.skipped_forms.entry(form).or_default() += 1;
                    }
                }
            }
            if b.transaction.kind.is_transaction() && b.transaction.within.is_some() {
                r.tx_nested += 1;
            }
            if b.transaction.kind.is_transaction() {
                r.samples.push(format!(
                    "{}:{} tx {} {}{}",
                    file.display(),
                    site(expanded, lines, b.span),
                    b.kind.as_str(),
                    match b.transaction.kind {
                        TxKind::Explicit => "explicit".to_string(),
                        TxKind::Implicit(up) => format!(
                            "implicit {} at {}",
                            up.kind.as_str(),
                            site(expanded, lines, up.at)
                        ),
                        _ => String::new(),
                    },
                    b.transaction
                        .within
                        .map(|w| format!(" within {}", site(expanded, lines, u.block(w).span)))
                        .unwrap_or_default()
                ));
            }
        }
    }
    for b in fs.buffers() {
        r.buffers += 1;
        *r.buffers_by_kind.entry(b.kind.as_str()).or_default() += 1;
        if b.references == 0 {
            r.buffers_unreferenced += 1;
        }
        if let oxabl_scope::Table::Unknown(name) = &b.table {
            *r.unknown_tables
                .entry(name.to_ascii_lowercase())
                .or_default() += 1;
        }
        if b.is_external() {
            r.buffers_external += 1;
            continue;
        }
        if !b.unknown.is_empty() {
            r.buffers_unknown += 1;
            for u in &b.unknown {
                *r.unknown_buffers_by_reason
                    .entry(u.reason.as_str())
                    .or_default() += 1;
            }
            continue;
        }
        r.buffers_known += 1;
        for s in &b.seen {
            *r.scopes_by_kind.entry(s.kind.as_str()).or_default() += 1;
            let unit = fs.unit(s.unit);
            if s.kind == oxabl_scope::ScopeKind::Raised {
                if s.block == oxabl_flow::BlockId::ROUTINE {
                    r.raised_to_routine += 1;
                }
                if s.unit == b.owner && s.absorbed.is_empty() {
                    r.raised_across_units += 1;
                }
            }
            let absorbed: Vec<String> = s
                .absorbed
                .iter()
                .map(|a| site(expanded, lines, *a))
                .collect();
            r.samples.push(format!(
                "{}:{} scope {} -> {} {} at {} ({}){}",
                file.display(),
                site(expanded, lines, s.at),
                b.name,
                unit.block(s.block).kind.as_str(),
                site(expanded, lines, unit.block(s.block).span),
                s.kind.as_str(),
                s.references,
                if absorbed.is_empty() {
                    String::new()
                } else {
                    format!(" absorbed {}", absorbed.join(","))
                }
            ));
        }
    }
    for read in fs.reads() {
        let lock = match read.lock {
            Lock::Explicit(m) => m.as_str(),
            Lock::Default => "default",
            Lock::TempTable => "temp-table",
            Lock::Unknown(_) => "unknown",
        };
        *r.reads_by_kind_lock
            .entry((read.kind.as_str(), lock))
            .or_default() += 1;
        if read.lock == Lock::Default {
            match read.written {
                oxabl_scope::Written::Yes => r.default_reads_written += 1,
                oxabl_scope::Written::Unknown(_) => r.default_reads_maybe_written += 1,
                oxabl_scope::Written::No => {}
            }
        }
    }
}

/// The first word of a statement, lowercased.
fn statement_word(text: &str) -> String {
    text.split(|c: char| c.is_whitespace() || c == '.' || c == '(')
        .find(|w| !w.is_empty())
        .unwrap_or("")
        .to_ascii_lowercase()
}

/// The root-file line of an expanded span, or the include site's line.
fn site(expanded: &ExpandedFile, lines: &Lines, span: Span) -> String {
    if let Some(s) = expanded.resolve_root_span(span) {
        lines.line(s.start).to_string()
    } else if let Some(s) = expanded.resolve_include_site(span) {
        format!("via{}", lines.line(s.start))
    } else {
        "?".into()
    }
}

struct Lines(Vec<u32>);

impl Lines {
    fn new(src: &str) -> Self {
        let mut v = vec![0];
        v.extend(
            src.bytes()
                .enumerate()
                .filter(|(_, b)| *b == b'\n')
                .map(|(i, _)| i as u32 + 1),
        );
        Lines(v)
    }

    fn line(&self, at: u32) -> u32 {
        self.0.partition_point(|&s| s <= at) as u32
    }
}

fn print_map<K: std::fmt::Display>(title: &str, map: &BTreeMap<K, usize>) {
    println!("{title}");
    for (k, n) in map {
        println!("  {k:<28} {n}");
    }
}

fn print_report(r: &Report, wall: Duration, samples: usize, verbose: bool) {
    println!("files                 {}", r.files);
    println!("unreadable            {}", r.unreadable);
    println!("preprocess failed     {}", r.preprocess_failed);
    println!("pipeline panics       {}", r.pipeline_panics);
    println!("parse recovered       {}", r.parse_recovered);
    println!("scope panics          {}", r.scope_panics);
    println!("units                 {}", r.units);
    println!("blocks                {}", r.blocks);
    println!("buffers               {}", r.buffers);
    print_map("buffers by kind", &r.buffers_by_kind);
    println!(
        "buffers: external {}, unreferenced {}, known {}, unknown {}",
        r.buffers_external, r.buffers_unreferenced, r.buffers_known, r.buffers_unknown
    );
    print_map("unknown buffers by reason", &r.unknown_buffers_by_reason);
    print_map("scopes of known buffers by kind", &r.scopes_by_kind);
    println!(
        "raised scopes: at a routine block {}, only from another unit {}",
        r.raised_to_routine, r.raised_across_units
    );
    println!("transaction-capable blocks {}", r.capable_blocks);
    println!("transactions: explicit {}", r.tx_explicit);
    print_map("transactions: implicit by cause", &r.tx_implicit_by_cause);
    println!(
        "implicit transactions at a routine block {}, transaction blocks nested in another {}",
        r.tx_routine, r.tx_nested
    );
    print_map(
        "implicit transactions at a routine block, by unit",
        &r.tx_routine_by_unit,
    );
    print_map("transactions: unknown by reason", &r.tx_unknown_by_reason);
    print_map(
        "skipped statements behind an unknown transaction",
        &r.skipped_forms,
    );
    println!("record reads by kind and lock");
    for ((k, l), n) in &r.reads_by_kind_lock {
        println!("  {k:<12} {l:<16} {n}");
    }
    println!(
        "default-lock reads with a write after them in scope {}, maybe written by a call {}",
        r.default_reads_written, r.default_reads_maybe_written
    );
    println!("expand+parse time     {:.2?}", r.expand_parse_time);
    println!("scope time            {:.2?}", r.scope_time);
    println!("wall time             {wall:.2?}");
    if verbose {
        let mut tables: Vec<_> = r.unknown_tables.iter().collect();
        tables.sort_by(|a, b| b.1.cmp(a.1));
        eprintln!(
            "tables the schema does not have (top 30 of {})",
            tables.len()
        );
        for (t, n) in tables.iter().take(30) {
            eprintln!("  {t:<28} {n}");
        }
        for (path, why) in &r.failed {
            eprintln!("{why}: {}", path.display());
        }
    }
    if samples > 0 {
        eprintln!("sampled answers");
        let step = r.samples.len().checked_div(samples).unwrap_or(0).max(1);
        for s in r.samples.iter().step_by(step).take(samples) {
            eprintln!("{s}");
        }
    }
}
