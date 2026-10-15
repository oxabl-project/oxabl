//! lk_corpus — run the `strict.locks` rules (LK001-LK003) over a tree the way
//! `oxabl check <dir>` does, and print the finding counts per rule per
//! top-level directory.
//!
//! The tree's `oxabl.toml` is read as `check` reads it (its schema included);
//! the group is switched on in memory. Roots go through `LintPipeline::run`,
//! and the `.i` files of the tree through `run_context_free`, as in
//! `hd_corpus`.
//!
//! # Usage
//! ```text
//! OXABL_LK_CORPUS=<dir> cargo run -p oxabl --release --example lk_corpus
//! ```
//! `OXABL_LK_FINDINGS=<file>` also writes every finding as a tab-separated line:
//! rule, `path:line`, whether it is at a use site, the message and the help.

use std::collections::{BTreeMap, HashSet};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_analyze::DiagnosticSource;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path, walk_include_fragments};

const RULES: [&str; 3] = ["LK001", "LK002", "LK003"];

struct Finding {
    rule: &'static str,
    path: PathBuf,
    line: usize,
    use_site: bool,
    message: String,
    help: String,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_LK_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_LK_CORPUS to a directory");
        std::process::exit(2);
    };
    let started = Instant::now();
    let (mut config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    config
        .lint_severities
        .base_mut()
        .enable_group("strict.locks");

    let roots = discover_path(&root).unwrap_or_else(|e| {
        eprintln!("error: {e}");
        std::process::exit(2);
    });
    let abs = |p: &Path| std::path::absolute(p).unwrap_or_else(|_| p.to_path_buf());
    let indexed: Vec<PathBuf> = roots.iter().map(|f| abs(f)).collect();
    let fs = RealFileSystem;
    let lint = LintPipeline::new(&config, &fs)
        .with_preprocess(true)
        .with_known_files(&indexed);
    let fragments: Vec<PathBuf> = walk_include_fragments(&root)
        .into_iter()
        .filter(|f| lint.with_file(abs(f)).context_free_rules_enabled())
        .collect();

    let mut findings: Vec<Finding> = Vec::new();
    let mut unreadable = 0usize;
    let mut panics: Vec<String> = Vec::new();
    let mut seen: HashSet<(PathBuf, &'static str, u32)> = HashSet::new();
    for (file, is_fragment) in roots
        .iter()
        .map(|f| (f, false))
        .chain(fragments.iter().map(|f| (f, true)))
    {
        let Ok(bytes) = std::fs::read(file) else {
            unreadable += 1;
            continue;
        };
        let source = String::from_utf8_lossy(&bytes).into_owned();
        let handle = lint.with_file(abs(file));
        let result = if is_fragment {
            handle.run_context_free(&source)
        } else {
            handle.run(&source)
        };
        if let Some(panic) = result.failure() {
            panics.push(format!("{}: {panic:?}", file.display()));
            continue;
        }
        let starts: Vec<usize> = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        for found in result.by_source(DiagnosticSource::Lint) {
            let d = &found.diagnostic;
            let Some(rule) = RULES.iter().find(|r| **r == d.code.0) else {
                continue;
            };
            if !seen.insert((file.clone(), rule, d.span.span.start)) {
                continue;
            }
            let help = d.help.as_deref().unwrap_or("");
            findings.push(Finding {
                rule,
                path: file.clone(),
                line: starts.partition_point(|&s| s <= d.span.span.start as usize),
                use_site: help.ends_with(oxabl_lint::fragment::USE_SITE_NOTE),
                message: d.message.clone(),
                help: help.to_string(),
            });
        }
    }
    let wall = started.elapsed();

    let top = |p: &Path| -> String {
        let rel = p.strip_prefix(&root).unwrap_or(p);
        let mut parts = rel.components();
        match (parts.next(), parts.next()) {
            (Some(first), Some(_)) => first.as_os_str().to_string_lossy().into_owned(),
            _ => ".".to_string(),
        }
    };
    let mut table: BTreeMap<String, [usize; 3]> = BTreeMap::new();
    let mut totals = [0usize; 3];
    let mut use_site = [0usize; 3];
    for f in &findings {
        let i = RULES.iter().position(|r| *r == f.rule).unwrap();
        table.entry(top(&f.path)).or_default()[i] += 1;
        totals[i] += 1;
        use_site[i] += usize::from(f.use_site);
    }
    println!("root                  {}", root.display());
    println!("roots checked         {}", roots.len());
    println!("include files checked {}", fragments.len());
    println!("unreadable            {unreadable}");
    println!("panics                {}", panics.len());
    for p in panics.iter().take(10) {
        println!("  {p}");
    }
    println!();
    let row = |name: &str, c: &[usize; 3]| {
        println!(
            "{:<24}{:>8}{:>8}{:>8}{:>9}",
            name,
            c[0],
            c[1],
            c[2],
            c.iter().sum::<usize>()
        );
    };
    println!(
        "{:<24}{:>8}{:>8}{:>8}{:>9}",
        "directory", "LK001", "LK002", "LK003", "total"
    );
    for (dir, counts) in &table {
        row(dir, counts);
    }
    row("TOTAL", &totals);
    row("at a use site", &use_site);
    println!("wall time             {wall:.2?}");

    if let Some(out) = std::env::var_os("OXABL_LK_FINDINGS") {
        findings.sort_by(|a, b| (a.rule, &a.path, a.line).cmp(&(b.rule, &b.path, b.line)));
        let mut w = std::io::BufWriter::new(std::fs::File::create(out).expect("create findings"));
        for f in &findings {
            writeln!(
                w,
                "{}\t{}:{}\t{}\t{}\t{}",
                f.rule,
                f.path.display(),
                f.line,
                if f.use_site { "use-site" } else { "-" },
                f.message,
                f.help
            )
            .expect("write findings");
        }
    }
}
