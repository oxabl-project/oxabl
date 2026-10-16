//! nulls_corpus — run the `strict.nulls` rules over `oxabl_nulls` (NL003) over a
//! tree the way `oxabl check <dir>` does, and print the finding counts per
//! rule per top-level directory, with the wall time.
//!
//! The tree's `oxabl.toml` is read as `check` reads it (its schema included);
//! the group is switched on in memory. Roots go through `LintPipeline::run`,
//! and the `.i` files of the tree through `run_context_free`, as in
//! `lk_corpus`. `OXABL_NL_GROUP=off` leaves the group as configured, and
//! `OXABL_NL_ALSO=<group>` switches another group on, to time the same run
//! with and without the failed-find analysis (`strict.locks` shares its scope
//! analysis).
//!
//! `OXABL_NL_FALLBACKS=1` also analyses every root once more, over its
//! expanded text and the tree's schema, and counts the units whose dataflow ran
//! past its work budget (`FileNulls::exhausted`): those units are Unknown, with
//! no finding.
//!
//! # Usage
//! ```text
//! OXABL_NL_CORPUS=<dir> cargo run -p oxabl --release --example nulls_corpus
//! ```
//! `OXABL_NL_FINDINGS=<file>` also writes every finding as a tab-separated line:
//! rule, `path:line`, whether it is at a use site, the message and the help.

use std::collections::{BTreeMap, HashSet};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_analyze::DiagnosticSource;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path, walk_include_fragments};

const RULES: [&str; 3] = ["NL001", "NL002", "NL003"];
const N: usize = RULES.len();

struct Finding {
    rule: &'static str,
    path: PathBuf,
    line: usize,
    use_site: bool,
    message: String,
    help: String,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_NL_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_NL_CORPUS to a directory");
        std::process::exit(2);
    };
    let started = Instant::now();
    let (mut config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    if let Ok(also) = std::env::var("OXABL_NL_ALSO") {
        config.lint_severities.base_mut().enable_group(&also);
    }
    if std::env::var("OXABL_NL_GROUP").is_ok_and(|v| v == "off") {
        eprintln!("strict.nulls left as configured");
    } else {
        config
            .lint_severities
            .base_mut()
            .enable_group("strict.nulls");
    }

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
    let fallbacks =
        std::env::var_os("OXABL_NL_FALLBACKS").map(|_| count_fallbacks(&roots, &lint, &config));

    let top = |p: &Path| -> String {
        let rel = p.strip_prefix(&root).unwrap_or(p);
        let mut parts = rel.components();
        match (parts.next(), parts.next()) {
            (Some(first), Some(_)) => first.as_os_str().to_string_lossy().into_owned(),
            _ => ".".to_string(),
        }
    };
    let mut table: BTreeMap<String, [usize; N]> = BTreeMap::new();
    let mut totals = [0usize; N];
    let mut use_site = [0usize; N];
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
    let row = |name: &str, c: &[usize; N]| {
        let cells: String = c.iter().map(|n| format!("{n:>7}")).collect();
        println!("{name:<24}{cells}{:>9}", c.iter().sum::<usize>());
    };
    let head: String = RULES.iter().map(|r| format!("{r:>7}")).collect();
    println!("{:<24}{head}{:>9}", "directory", "total");
    for (dir, counts) in &table {
        row(dir, counts);
    }
    row("TOTAL", &totals);
    row("at a use site", &use_site);
    println!("wall time             {wall:.2?}");
    if let Some((units, files)) = fallbacks {
        println!("units over the budget {units} in {files} files");
    }

    if let Some(out) = std::env::var_os("OXABL_NL_FINDINGS") {
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

/// The units past the work budget over `roots`, and the files they are in.
fn count_fallbacks(
    roots: &[PathBuf],
    lint: &LintPipeline<'_>,
    config: &PipelineConfig,
) -> (usize, usize) {
    let schema = config.schema_loaded.then_some(&config.schema);
    let (mut units, mut files) = (0, 0);
    for file in roots {
        let Ok(bytes) = std::fs::read(file) else {
            continue;
        };
        let source = String::from_utf8_lossy(&bytes).into_owned();
        let abs = std::path::absolute(file).unwrap_or_else(|_| file.clone());
        let expansion = lint.with_file(abs).expand(&source);
        let text = expansion.text().unwrap_or(&source);
        let tokens = oxabl_lexer::tokenize(text);
        let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
        let options = oxabl_scope::ScopeOptions {
            parse_recovered: !program.errors.is_empty(),
            ..Default::default()
        };
        let flow = oxabl_flow::build_file_tokens(&program.statements, text, &tokens, &options.flow);
        let scopes =
            oxabl_scope::analyze_flow(&program.statements, text, &tokens, &flow, schema, &options);
        let nulls = oxabl_nulls::analyze_file(
            text,
            &tokens,
            &flow,
            &scopes,
            &oxabl_nulls::NullOptions::default(),
        )
        .with_values(
            text,
            &tokens,
            &program.statements,
            &flow,
            &oxabl_nulls::NullOptions::default(),
        );
        let n = nulls.exhausted().len();
        if n > 0 {
            println!("over the budget: {} ({n} units)", file.display());
            units += n;
            files += 1;
        }
    }
    (units, files)
}
