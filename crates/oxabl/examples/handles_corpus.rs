//! handles_corpus — run `oxabl_handles` over every ABL file under a tree and
//! report the dynamic objects it finds: counts by kind and verdict, leaks by
//! path, escapes and opaque reasons, panics and time.
//!
//! Every `.p`, `.w`, `.cls`, `.v` and `.i` file is expanded the way
//! `oxabl check` expands it (the tree's `oxabl.toml`, preprocessing on), then
//! lexed, parsed and analysed. Each step runs under a panic guard.
//!
//! # Usage
//! ```text
//! OXABL_HANDLES_CORPUS=<dir-or-file> cargo run -p oxabl --release --example handles_corpus
//! ```
//! With `OXABL_HANDLES_DUMP=1` and a single file, print that file's dump;
//! `OXABL_HANDLES_EXPANDED=<path>` also writes the expanded text there.
//! The report lists leaking objects per directory, normal-path leaks apart from
//! those on abnormal paths only. `OXABL_HANDLES_SITES=<path>` writes the distinct
//! normal-path leak sites there.
//! Set `OXABL_HANDLES_VERBOSE=<n>` to print `n` sampled leaking objects (every
//! k-th, spread over the tree) with `file:line` and their path lines, and the
//! files that panicked or failed. Lines are in the root file; a site inside an
//! include shows the line of the `{...}` that brought it in, marked `via`.

use std::collections::BTreeMap;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use oxabl_analyze::ExpandedFile;
use oxabl_ast::Span;
use oxabl_handles::{LeakKind, Placement, Verdict};
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const EXTENSIONS: &[&str] = &["p", "w", "cls", "v", "i"];

#[derive(Default)]
struct Report {
    files: usize,
    files_with_objects: usize,
    unreadable: usize,
    preprocess_failed: usize,
    pipeline_panics: usize,
    parse_recovered: usize,
    handles_panics: usize,
    units: usize,
    objects: usize,
    by_kind_verdict: BTreeMap<(&'static str, &'static str), usize>,
    by_verdict: BTreeMap<&'static str, usize>,
    leak_objects_normal: usize,
    leak_objects_abnormal_only: usize,
    leaks_by_kind: BTreeMap<&'static str, usize>,
    escapes_by_kind: BTreeMap<&'static str, usize>,
    opaque_by_reason: BTreeMap<&'static str, usize>,
    placement: BTreeMap<&'static str, usize>,
    /// Leaking objects by directory: on a normal path, and on abnormal paths only.
    leaks_by_dir: BTreeMap<String, [usize; 2]>,
    /// Leaking objects by distinct site: `file:line` in a root file, else the
    /// unit name and create text of an include (counted once for every root
    /// that includes it).
    distinct_normal: std::collections::BTreeSet<String>,
    distinct_abnormal: std::collections::BTreeSet<String>,
    distinct_all: std::collections::BTreeSet<String>,
    unowned_outer: usize,
    samples_normal: BTreeMap<String, String>,
    samples_abnormal: BTreeMap<String, String>,
    expand_parse_time: Duration,
    handles_time: Duration,
    failed: Vec<(PathBuf, &'static str)>,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_HANDLES_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_HANDLES_CORPUS to a directory or file");
        std::process::exit(2);
    };
    let verbose: usize = std::env::var("OXABL_HANDLES_VERBOSE")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(0);
    let files = discover(&root);
    if files.is_empty() {
        eprintln!("no ABL files under {}", root.display());
        std::process::exit(2);
    }
    let (config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);

    if std::env::var_os("OXABL_HANDLES_DUMP").is_some() {
        let file = &files[0];
        let source = std::fs::read_to_string(file).expect("readable file");
        let expansion = pipeline.with_file(file).expand(&source);
        let text = expansion.text().unwrap_or(&source);
        if let Some(out) = std::env::var_os("OXABL_HANDLES_EXPANDED") {
            std::fs::write(out, text).expect("write the expanded text");
        }
        let tokens = oxabl_lexer::tokenize(text);
        let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
        print!(
            "{}",
            oxabl_handles::analyze_file(&program.statements, text).dump(text)
        );
        return;
    }

    let mut r = Report::default();
    let started = Instant::now();
    for file in &files {
        run_file(&pipeline, &root, file, &mut r);
    }
    let wall = started.elapsed();
    print_report(&r, wall, verbose);
}

fn discover(root: &Path) -> Vec<PathBuf> {
    if root.is_file() {
        return vec![std::path::absolute(root).unwrap_or_else(|_| root.to_path_buf())];
    }
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
                .is_some_and(|x| EXTENSIONS.iter().any(|k| k.eq_ignore_ascii_case(x)))
        })
        .map(|e| std::path::absolute(e.path()).unwrap_or_else(|_| e.into_path()))
        .collect();
    files.sort();
    files
}

fn run_file(pipeline: &LintPipeline, root: &Path, file: &Path, r: &mut Report) {
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
        Some((expanded, text, program))
    }));
    r.expand_parse_time += t.elapsed();
    let (expanded, text, program) = match parsed {
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
    if !program.errors.is_empty() {
        r.parse_recovered += 1;
    }

    let t = Instant::now();
    let analysed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        oxabl_handles::analyze_file(&program.statements, &text)
    }));
    r.handles_time += t.elapsed();
    let handles = match analysed {
        Ok(h) => h,
        Err(_) => {
            r.handles_panics += 1;
            r.failed.push((file.to_path_buf(), "handles panic"));
            return;
        }
    };
    if handles.units().is_empty() {
        return;
    }
    r.files_with_objects += 1;
    r.units += handles.units().len();
    let dir = file
        .parent()
        .and_then(|p| p.strip_prefix(root).ok())
        .map(|p| p.display().to_string())
        .unwrap_or_default();
    let lines = Lines::new(&source);
    for (unit, o) in handles.objects() {
        r.objects += 1;
        let key = match expanded.resolve_root_span(o.create.span) {
            Some(s) => format!("{}:{}", file.display(), lines.line(s.start)),
            None => format!(
                "include {} {}",
                unit.name.as_deref().unwrap_or("main"),
                text.get(o.create.span.start as usize..o.create.span.end as usize)
                    .unwrap_or("")
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ")
                    .to_ascii_lowercase()
            ),
        };
        if !o.kind.is_collected() {
            r.distinct_all.insert(key.clone());
        }
        *r.by_verdict.entry(o.verdict.as_str()).or_default() += 1;
        *r.by_kind_verdict
            .entry((o.kind.as_str(), o.verdict.as_str()))
            .or_default() += 1;
        if o.kind.uses_widget_pool() {
            let p = match o.placement {
                Placement::NotPooled => "none",
                Placement::UnitPool => "unit",
                Placement::FilePool => "file",
                Placement::ClassPool => "class",
                Placement::NamedPool { .. } => "named",
                Placement::ForeignPool { .. } => "foreign",
                Placement::Unscoped => "unscoped",
            };
            *r.placement.entry(p).or_default() += 1;
        }
        for e in &o.escapes {
            *r.escapes_by_kind.entry(e.kind.as_str()).or_default() += 1;
        }
        if o.verdict == Verdict::Escapes
            && !o.escapes.is_empty()
            && o.escapes.iter().all(|e| {
                matches!(
                    e.kind,
                    oxabl_handles::EscapeKind::OuterVariable | oxabl_handles::EscapeKind::Member
                ) && !e.freed_elsewhere
            })
        {
            r.unowned_outer += 1;
        }
        if o.verdict == Verdict::Unknown {
            for x in &o.opaque {
                *r.opaque_by_reason.entry(x.reason.as_str()).or_default() += 1;
            }
        }
        if o.verdict != Verdict::Leaks {
            continue;
        }
        r.leaks_by_dir.entry(dir.clone()).or_default()[usize::from(!o.leaks_on_normal_path())] += 1;
        if o.leaks_on_normal_path() {
            r.leak_objects_normal += 1;
            r.distinct_normal.insert(key.clone());
        } else {
            r.leak_objects_abnormal_only += 1;
            r.distinct_abnormal.insert(key.clone());
        }
        for l in &o.leaks {
            let k = match (l.kind, l.normal) {
                (LeakKind::Overwrite, true) => "overwrite (normal)",
                (LeakKind::Overwrite, false) => "overwrite (abnormal)",
                (LeakKind::Exit(_), true) => "exit (normal)",
                (LeakKind::Exit(_), false) => "exit (abnormal)",
            };
            *r.leaks_by_kind.entry(k).or_default() += 1;
        }
        let first = o.leaks.iter().find(|l| l.normal).unwrap_or(&o.leaks[0]);
        let path: Vec<String> = first
            .path
            .iter()
            .map(|s| site(&expanded, &lines, s.span))
            .collect();
        let mut path_d = path.clone();
        path_d.dedup();
        let samples = if o.leaks_on_normal_path() {
            &mut r.samples_normal
        } else {
            &mut r.samples_abnormal
        };
        samples.entry(key).or_insert_with(|| {
            format!(
                "{}:{} {} {} [{}] leak {:?} normal={} via={} path {}",
                file.display(),
                site(&expanded, &lines, o.create.span),
                o.kind.as_str(),
                o.holder.as_deref().unwrap_or("-"),
                if o.leaks_on_normal_path() {
                    "normal"
                } else {
                    "abnormal-only"
                },
                first.kind,
                first.normal,
                first
                    .via
                    .map(|v| format!("{:?}@{}", v.edge, site(&expanded, &lines, v.span)))
                    .unwrap_or_default(),
                path_d.join(">")
            )
        });
    }
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

fn print_report(r: &Report, wall: Duration, verbose: usize) {
    println!("files                 {}", r.files);
    println!("files with objects    {}", r.files_with_objects);
    println!("unreadable            {}", r.unreadable);
    println!("preprocess failed     {}", r.preprocess_failed);
    println!("pipeline panics       {}", r.pipeline_panics);
    println!("parse recovered       {}", r.parse_recovered);
    println!("handles panics        {}", r.handles_panics);
    println!("units with objects    {}", r.units);
    println!("objects               {}", r.objects);
    for (v, n) in &r.by_verdict {
        println!("  {v:<20} {n}");
    }
    println!("by kind and verdict");
    for ((k, v), n) in &r.by_kind_verdict {
        println!("  {k:<22} {v:<12} {n}");
    }
    println!(
        "leaking objects       normal path {}, abnormal path only {}",
        r.leak_objects_normal, r.leak_objects_abnormal_only
    );
    println!(
        "distinct sites        objects {}, leaking: normal path {}, abnormal path only {}",
        r.distinct_all.len(),
        r.distinct_normal.len(),
        r.distinct_abnormal.len()
    );
    if let Some(path) = std::env::var_os("OXABL_HANDLES_SITES") {
        let lines: Vec<&str> = r.distinct_normal.iter().map(String::as_str).collect();
        let _ = std::fs::write(path, lines.join("\n"));
    }
    println!(
        "escapes only to a member or outer variable no other unit frees: {}",
        r.unowned_outer
    );
    println!("leaks by path");
    for (k, n) in &r.leaks_by_kind {
        println!("  {k:<22} {n}");
    }
    println!("escapes by kind");
    for (k, n) in &r.escapes_by_kind {
        println!("  {k:<22} {n}");
    }
    println!("unknown objects by opaque reason");
    for (k, n) in &r.opaque_by_reason {
        println!("  {k:<22} {n}");
    }
    println!("pool placement (poolable kinds)");
    for (k, n) in &r.placement {
        println!("  {k:<22} {n}");
    }
    println!("leaking objects by directory (top 20 by normal path)");
    println!("  {:>8} {:>8}  directory", "normal", "abnormal");
    let mut dirs: Vec<_> = r.leaks_by_dir.iter().collect();
    dirs.sort_by(|a, b| b.1.cmp(a.1));
    for (d, [normal, abnormal]) in dirs.iter().take(20) {
        println!("  {normal:>8} {abnormal:>8}  {d}");
    }
    println!("expand+parse time     {:.2?}", r.expand_parse_time);
    println!("handles time          {:.2?}", r.handles_time);
    println!("wall time             {wall:.2?}");
    if verbose > 0 {
        for (path, why) in &r.failed {
            eprintln!("{why}: {}", path.display());
        }
        for (title, samples) in [
            ("normal-path leaks", &r.samples_normal),
            ("abnormal-path-only leaks", &r.samples_abnormal),
        ] {
            eprintln!("sampled {title}");
            let step = samples.len().checked_div(verbose).unwrap_or(0).max(1);
            for s in samples.values().step_by(step).take(verbose) {
                eprintln!("{s}");
            }
        }
    }
}
