//! flow_corpus — build the `oxabl_flow` CFGs of every ABL file under a tree and
//! report counts: files, CFGs, blocks, opaque units by reason, time.
//!
//! Every `.p`, `.w`, `.cls`, `.v` and `.i` file is expanded the way
//! `oxabl check` expands it (the tree's `oxabl.toml`, preprocessing on), then
//! lexed, parsed, and handed to `oxabl_flow::build_file`. Each step runs under a
//! panic guard, so the report counts panics instead of stopping at the first.
//! The dominator trees and a reachability dataflow are also computed for every
//! CFG, to exercise the query layer.
//!
//! # Usage
//! ```text
//! OXABL_FLOW_CORPUS=<dir-or-file> cargo run -p oxabl --release --example flow_corpus
//! ```
//! With `OXABL_FLOW_DUMP=1` and a single file, print that file's CFG dump.
//! Set `OXABL_FLOW_VERBOSE=1` to list the files that panicked or failed, and
//! the first dead statement, exitless unit and opaque node of each unit.

use std::collections::BTreeMap;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use oxabl_flow::dataflow::{Analysis, BitSet, Direction, solve};
use oxabl_flow::{Cfg, NodeIx, NodeKind};
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::RealFileSystem;
use walkdir::WalkDir;

const EXTENSIONS: &[&str] = &["p", "w", "cls", "v", "i"];

#[derive(Default)]
struct Report {
    files: usize,
    by_ext: BTreeMap<String, usize>,
    unreadable: usize,
    preprocess_failed: usize,
    pipeline_panics: usize,
    parse_recovered: usize,
    flow_panics: usize,
    query_panics: usize,
    units: usize,
    units_by_kind: BTreeMap<&'static str, usize>,
    blocks: usize,
    blocks_by_kind: BTreeMap<&'static str, usize>,
    nodes: usize,
    edges: usize,
    max_nodes: usize,
    opaque_units: usize,
    opaque_units_by_reason: BTreeMap<&'static str, usize>,
    opaque_nodes_by_reason: BTreeMap<&'static str, usize>,
    units_with_dead_code: usize,
    units_without_exit: usize,
    findings: Vec<String>,
    expand_parse_time: Duration,
    build_time: Duration,
    query_time: Duration,
    failed: Vec<(PathBuf, &'static str)>,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_FLOW_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_FLOW_CORPUS to a directory or file");
        std::process::exit(2);
    };
    let verbose = std::env::var_os("OXABL_FLOW_VERBOSE").is_some();
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

    if std::env::var_os("OXABL_FLOW_DUMP").is_some() {
        let file = &files[0];
        let source = std::fs::read_to_string(file).expect("readable file");
        let expansion = pipeline.with_file(file).expand(&source);
        let text = expansion.text().unwrap_or(&source);
        let tokens = oxabl_lexer::tokenize(text);
        let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
        print!(
            "{}",
            oxabl_flow::build_file(&program.statements, text).dump(text)
        );
        return;
    }

    let mut r = Report::default();
    let started = Instant::now();
    for file in &files {
        run_file(&pipeline, file, &mut r, verbose);
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

fn run_file(pipeline: &LintPipeline, file: &Path, r: &mut Report, verbose: bool) {
    r.files += 1;
    let ext = file
        .extension()
        .and_then(|x| x.to_str())
        .unwrap_or("")
        .to_ascii_lowercase();
    *r.by_ext.entry(ext).or_default() += 1;
    let Ok(bytes) = std::fs::read(file) else {
        r.unreadable += 1;
        r.failed.push((file.to_path_buf(), "unreadable"));
        return;
    };
    let source = String::from_utf8_lossy(&bytes).into_owned();

    let t = Instant::now();
    let parsed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let expansion = pipeline.with_file(file).expand(&source);
        let text = expansion.text()?.to_string();
        let tokens = oxabl_lexer::tokenize(&text);
        let program = oxabl_parser::Parser::new(&tokens, &text).parse_program();
        Some((text, program))
    }));
    r.expand_parse_time += t.elapsed();
    let (text, program) = match parsed {
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
    let built = std::panic::catch_unwind(AssertUnwindSafe(|| {
        oxabl_flow::build_file(&program.statements, &text)
    }));
    r.build_time += t.elapsed();
    let flow = match built {
        Ok(flow) => flow,
        Err(_) => {
            r.flow_panics += 1;
            r.failed.push((file.to_path_buf(), "flow panic"));
            return;
        }
    };

    for cfg in flow.units() {
        r.units += 1;
        *r.units_by_kind.entry(cfg.unit().kind.as_str()).or_default() += 1;
        r.blocks += cfg.block_count();
        for (_, b) in cfg.blocks() {
            *r.blocks_by_kind.entry(b.kind.as_str()).or_default() += 1;
        }
        r.nodes += cfg.node_count();
        r.edges += cfg.edges().len();
        r.max_nodes = r.max_nodes.max(cfg.node_count());
        let mut reasons: Vec<&'static str> = cfg.opaque_nodes().map(|(_, o)| o.as_str()).collect();
        for reason in &reasons {
            *r.opaque_nodes_by_reason.entry(reason).or_default() += 1;
        }
        reasons.sort_unstable();
        reasons.dedup();
        if !reasons.is_empty() {
            r.opaque_units += 1;
        }
        for reason in reasons {
            *r.opaque_units_by_reason.entry(reason).or_default() += 1;
        }

        let t = Instant::now();
        let queried = std::panic::catch_unwind(AssertUnwindSafe(|| query(cfg)));
        r.query_time += t.elapsed();
        match queried {
            Ok((dead, exit)) => {
                r.units_with_dead_code += usize::from(dead);
                r.units_without_exit += usize::from(!exit);
                if verbose {
                    note(r, file, cfg, &text, dead, exit);
                }
            }
            Err(_) => {
                r.query_panics += 1;
                r.failed.push((file.to_path_buf(), "query panic"));
            }
        }
    }
}

fn note(r: &mut Report, file: &Path, cfg: &Cfg, text: &str, dead: bool, exit: bool) {
    let snippet = |n: NodeIx| {
        let span = cfg.node(n).span;
        let s = text
            .get(span.start as usize..span.end as usize)
            .unwrap_or("");
        s.split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
            .chars()
            .take(70)
            .collect::<String>()
    };
    let unit = cfg.unit().kind.as_str();
    if dead && let Some(&n) = cfg.unreachable_stmts().first() {
        r.findings
            .push(format!("dead {unit} {}: {}", file.display(), snippet(n)));
    }
    if !exit {
        r.findings
            .push(format!("no-exit {unit} {}", file.display()));
    }
    for (n, reason) in cfg.opaque_nodes() {
        if reason != oxabl_flow::OpaqueReason::SkippedStatement {
            r.findings.push(format!(
                "opaque {} {unit} {}: {}",
                reason.as_str(),
                file.display(),
                snippet(n)
            ));
        }
    }
}

/// Reachability as a forward may-analysis, checked against the graph walk.
struct Reach;

impl Analysis for Reach {
    type Fact = bool;

    fn direction(&self) -> Direction {
        Direction::Forward
    }

    fn initial(&self, _: &Cfg) -> bool {
        false
    }

    fn boundary(&self, _: &Cfg) -> bool {
        true
    }

    fn join(&self, acc: &mut bool, other: &bool) {
        *acc |= *other;
    }

    fn transfer(&self, _: &Cfg, _: NodeIx, _: &mut bool) {}
}

/// Run the query layer over one CFG; returns (has dead code, can exit).
fn query(cfg: &Cfg) -> (bool, bool) {
    let reachable: BitSet = cfg.reachable();
    let solved = solve(cfg, &Reach);
    for (n, _) in cfg.nodes() {
        assert_eq!(*solved.before(n), reachable.contains(n.index()));
    }
    let dom = cfg.dominators();
    let pdom = cfg.post_dominators();
    for (n, _) in cfg.nodes() {
        if reachable.contains(n.index()) {
            assert!(dom.dominates(cfg.entry(), n));
        }
        let _ = pdom.immediate(n);
    }
    let dead = !cfg.unreachable_stmts().is_empty();
    let exit = cfg
        .find_path(
            cfg.entry(),
            |n| matches!(cfg.node(n).kind, NodeKind::Exit(_)),
            |_| false,
        )
        .is_some();
    (dead, exit)
}

fn print_report(r: &Report, wall: Duration, verbose: bool) {
    println!("files                 {}", r.files);
    for (ext, n) in &r.by_ext {
        println!("  .{ext:<19} {n}");
    }
    println!("unreadable            {}", r.unreadable);
    println!("preprocess failed     {}", r.preprocess_failed);
    println!("pipeline panics       {}", r.pipeline_panics);
    println!("parse recovered       {}", r.parse_recovered);
    println!("flow panics           {}", r.flow_panics);
    println!("query panics          {}", r.query_panics);
    println!("CFGs (units)          {}", r.units);
    for (k, n) in &r.units_by_kind {
        println!("  {k:<20} {n}");
    }
    println!("blocks                {}", r.blocks);
    for (k, n) in &r.blocks_by_kind {
        println!("  {k:<20} {n}");
    }
    println!("nodes                 {}", r.nodes);
    println!("edges                 {}", r.edges);
    println!("largest CFG (nodes)   {}", r.max_nodes);
    println!("opaque units          {}", r.opaque_units);
    for (k, n) in &r.opaque_units_by_reason {
        println!("  {k:<20} {n} units");
    }
    for (k, n) in &r.opaque_nodes_by_reason {
        println!("  {k:<20} {n} nodes");
    }
    println!("units with dead code  {}", r.units_with_dead_code);
    println!("units with no exit    {}", r.units_without_exit);
    println!("expand+parse time     {:.2?}", r.expand_parse_time);
    println!("CFG build time        {:.2?}", r.build_time);
    println!("query time            {:.2?}", r.query_time);
    println!("wall time             {wall:.2?}");
    if verbose {
        for (path, why) in &r.failed {
            eprintln!("{why}: {}", path.display());
        }
        for f in &r.findings {
            eprintln!("{f}");
        }
    }
}
