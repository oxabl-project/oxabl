//! hd_corpus — run the `strict.handles` rules (HD001-HD005) over a tree the
//! way `oxabl check <dir>` does, and print the finding counts per rule per
//! top-level directory.
//!
//! The tree's `oxabl.toml` is read as `check` reads it; the group is switched
//! on in memory. Roots go through `LintPipeline::run`, and the `.i` files of the
//! tree through `run_context_free`: the routing is the one `check` has, so a
//! finding in an include is reported once, in the include, unless the include
//! cannot show it alone. Then it is reported in the root, at the `{...}` site.
//!
//! # Usage
//! ```text
//! OXABL_HD_CORPUS=<dir> cargo run -p oxabl --release --example hd_corpus
//! ```
//! `OXABL_HD_FINDINGS=<file>` also writes every finding as a tab-separated line:
//! rule, `path:line`, whether it is lower confidence or at a use site, and the message.

use std::collections::{BTreeMap, HashSet};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_analyze::DiagnosticSource;
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path, walk_include_fragments};

const RULES: [&str; 5] = ["HD001", "HD002", "HD003", "HD004", "HD005"];

struct Finding {
    rule: &'static str,
    path: PathBuf,
    line: usize,
    lowered: bool,
    use_site: bool,
    message: String,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_HD_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_HD_CORPUS to a directory");
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
        .enable_group("strict.handles");

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
    let mut failures = 0usize;
    let mut seen: HashSet<(PathBuf, &'static str, u32)> = HashSet::new();
    for (file, is_fragment) in roots
        .iter()
        .map(|f| (f, false))
        .chain(fragments.iter().map(|f| (f, true)))
    {
        let Ok(bytes) = std::fs::read(file) else {
            failures += 1;
            continue;
        };
        let source = String::from_utf8_lossy(&bytes).into_owned();
        let handle = lint.with_file(abs(file));
        let result = if is_fragment {
            handle.run_context_free(&source)
        } else {
            handle.run(&source)
        };
        if result.failure().is_some() {
            failures += 1;
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
                lowered: help.ends_with(oxabl_lint::fragment::FRAGMENT_NOTE)
                    || help.ends_with(oxabl_lint::fragment::INCLUDER_NOTE),
                use_site: help.ends_with(oxabl_lint::fragment::USE_SITE_NOTE),
                message: d.message.clone(),
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
    let mut table: BTreeMap<String, [usize; 5]> = BTreeMap::new();
    let mut totals = [0usize; 5];
    let mut lowered = [0usize; 5];
    let mut use_site = [0usize; 5];
    for f in &findings {
        let i = RULES.iter().position(|r| *r == f.rule).unwrap();
        table.entry(top(&f.path)).or_default()[i] += 1;
        totals[i] += 1;
        lowered[i] += usize::from(f.lowered);
        use_site[i] += usize::from(f.use_site);
    }
    println!("root                  {}", root.display());
    println!("roots checked         {}", roots.len());
    println!("include files checked {}", fragments.len());
    println!("failures              {failures}");
    println!();
    println!(
        "{:<24}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}",
        "directory", "HD001", "HD002", "HD003", "HD004", "HD005", "total"
    );
    for (dir, counts) in &table {
        println!(
            "{:<24}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}",
            dir,
            counts[0],
            counts[1],
            counts[2],
            counts[3],
            counts[4],
            counts.iter().sum::<usize>()
        );
    }
    println!(
        "{:<24}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}",
        "TOTAL",
        totals[0],
        totals[1],
        totals[2],
        totals[3],
        totals[4],
        totals.iter().sum::<usize>()
    );
    println!(
        "{:<24}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}",
        "lower confidence",
        lowered[0],
        lowered[1],
        lowered[2],
        lowered[3],
        lowered[4],
        lowered.iter().sum::<usize>()
    );
    println!(
        "{:<24}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}",
        "at a use site",
        use_site[0],
        use_site[1],
        use_site[2],
        use_site[3],
        use_site[4],
        use_site.iter().sum::<usize>()
    );
    println!("wall time             {wall:.2?}");

    if let Some(out) = std::env::var_os("OXABL_HD_FINDINGS") {
        findings.sort_by(|a, b| (a.rule, &a.path, a.line).cmp(&(b.rule, &b.path, b.line)));
        let mut w = std::io::BufWriter::new(std::fs::File::create(out).expect("create findings"));
        for f in &findings {
            writeln!(
                w,
                "{}\t{}:{}\t{}\t{}",
                f.rule,
                f.path.display(),
                f.line,
                if f.use_site {
                    "use-site"
                } else if f.lowered {
                    "lowered"
                } else {
                    "-"
                },
                f.message
            )
            .expect("write findings");
        }
    }
}
