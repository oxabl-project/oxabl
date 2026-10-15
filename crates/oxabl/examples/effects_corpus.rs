//! effects_corpus — run `oxabl_effects` and the call graph over every
//! compilation unit of a tree, solve the transitive summaries, and print:
//! call-graph coverage (resolved and Unknown call sites, by reason), the
//! Unknown rate of index choices and of unit effects, summary completeness,
//! and how many `TX004` findings a callee summary with no database write
//! would silence.
//!
//! Each `.p`, `.w` and `.cls` file is expanded as `oxabl check` expands it,
//! with the tree's `oxabl.toml` and schema. `RUN` targets are resolved over the
//! configured search path through the run index (`BatchIndex::program`).
//!
//! # Usage
//! ```text
//! OXABL_EFFECTS_CORPUS=<dir> cargo run -p oxabl --release --example effects_corpus
//! ```
//! `OXABL_EFFECTS_EXPLAIN=<reason>` prints the line of every search whose index
//! is Unknown for that reason (`multi-index`, `of-join`, ...), or of every call
//! with that Unknown reason (`call:handle`, `call:method`, ...).
//! `OXABL_EFFECTS_TX004=<file>` writes every TX004 finding with what its
//! callee's summary says, tab-separated.

use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use oxabl_common::FileId;
use oxabl_effects::summary::{self, Node, Partial};
use oxabl_effects::{Access, EffectOptions, FileEffects};
use oxabl_flow::UnitKind;
use oxabl_index::BatchIndex;
use oxabl_index::calls::{CallKind, CallTarget, Callee, FileCalls, ProgramAnswer, UnknownCall};
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_schema::Schema;
use oxabl_scope::{ScopeOptions, Table};
use oxabl_semantic::AnalysisContext;
use oxabl_workspace::{RealFileSystem, discover_path};

const EXTENSIONS: &[&str] = &["p", "w", "cls"];

struct FileResult {
    path: PathBuf,
    effects: FileEffects,
    lines: Vec<usize>,
    /// TX004 findings: the start of each `RUN` statement in the expanded
    /// text, and its line in the file.
    tx004: Vec<(u32, usize)>,
}

#[derive(Default)]
struct Times {
    expand_parse: Duration,
    scope: Duration,
    effects: Duration,
}

fn main() {
    let Some(root) = std::env::var_os("OXABL_EFFECTS_CORPUS").map(PathBuf::from) else {
        eprintln!("set OXABL_EFFECTS_CORPUS to a directory");
        std::process::exit(2);
    };
    let root = std::path::absolute(&root).unwrap_or(root);
    let explain = std::env::var("OXABL_EFFECTS_EXPLAIN").ok();
    let started = Instant::now();
    let (config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in &warnings {
        eprintln!("warning: {w}");
    }
    let schema = config.schema_loaded.then_some(&config.schema);
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_preprocess(true);
    let index = BatchIndex::new(&fs, &config.include_paths)
        .with_run_extensions(&config.run_extensions)
        .with_preprocessor(&config.preprocessor);

    let roots: Vec<PathBuf> = discover_path(&root)
        .unwrap_or_else(|e| {
            eprintln!("error: {e}");
            std::process::exit(2);
        })
        .into_iter()
        .filter(|p| {
            p.extension()
                .and_then(|x| x.to_str())
                .is_some_and(|x| EXTENSIONS.iter().any(|k| k.eq_ignore_ascii_case(x)))
        })
        .map(|p| std::path::absolute(&p).unwrap_or(p))
        .collect();

    let mut times = Times::default();
    let mut files: Vec<FileResult> = Vec::new();
    let (mut failed, mut panics, mut recovered) = (0usize, 0usize, 0usize);
    for path in &roots {
        let Ok(bytes) = std::fs::read(path) else {
            failed += 1;
            continue;
        };
        let source = String::from_utf8_lossy(&bytes).into_owned();
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            analyse(&pipeline, &config, schema, path, &source, &mut times)
        }));
        match result {
            Ok(Some((r, rec))) => {
                recovered += usize::from(rec);
                files.push(r);
            }
            Ok(None) => failed += 1,
            Err(_) => {
                panics += 1;
                eprintln!("panic: {}", path.display());
            }
        }
    }

    // Nodes: every unit of every file.
    let mut base: Vec<u32> = Vec::with_capacity(files.len());
    let mut n = 0u32;
    for f in &files {
        base.push(n);
        n += f.effects.units().len() as u32;
    }
    let by_path: HashMap<PathBuf, usize> = files
        .iter()
        .enumerate()
        .map(|(i, f)| (canonical(&f.path), i))
        .collect();
    let mut program_cache: HashMap<String, Result<usize, UnknownCall>> = HashMap::new();
    let mut resolve_program = |name: &str| -> Result<usize, UnknownCall> {
        if let Some(r) = program_cache.get(name) {
            return *r;
        }
        let r = match oxabl_index::calls::resolve_program(&index, name) {
            ProgramAnswer::Found(id) => index
                .indexed_path(id)
                .and_then(|p| by_path.get(&canonical(&p)).copied())
                .ok_or(UnknownCall::NotFound),
            ProgramAnswer::Unknown(u) => Err(u),
        };
        program_cache.insert(name.to_string(), r);
        r
    };
    let find_unit = |file: usize, kinds: &[UnitKind], name: &str| -> Option<u32> {
        files[file]
            .effects
            .units()
            .iter()
            .find(|u| {
                kinds.contains(&u.kind)
                    && u.name
                        .as_deref()
                        .is_some_and(|n| n.eq_ignore_ascii_case(name))
            })
            .map(|u| base[file] + u.unit.index() as u32)
    };

    // Call-graph coverage.
    let mut sites: BTreeMap<String, usize> = BTreeMap::new();
    let mut run_resolved = 0usize;
    let mut run_unknown = 0usize;
    let mut nodes: Vec<Node> = Vec::with_capacity(n as usize);
    let mut site_callee: HashMap<(usize, u32), Option<Callee>> = HashMap::new();
    for (fi, f) in files.iter().enumerate() {
        for u in f.effects.units() {
            let node = Node::of_unit(&f.effects, u, |c| {
                let callee = match &c.target {
                    CallTarget::Procedure(p) => Some(
                        find_unit(fi, &[UnitKind::Procedure], p)
                            .map_or(Callee::Unknown(UnknownCall::NoRoutine), Callee::Node),
                    ),
                    CallTarget::Function(p) => Some(
                        find_unit(fi, &[UnitKind::Function], p)
                            .map_or(Callee::Unknown(UnknownCall::NoRoutine), Callee::Node),
                    ),
                    CallTarget::Method(p) => Some(
                        find_unit(fi, &[UnitKind::Method], p)
                            .map_or(Callee::Unknown(UnknownCall::NoRoutine), Callee::Node),
                    ),
                    CallTarget::Program(p) => Some(match resolve_program(p) {
                        Ok(file) => Callee::Node(base[file]),
                        Err(u) => Callee::Unknown(u),
                    }),
                    CallTarget::InProgram { program, procedure } => {
                        Some(match resolve_program(program) {
                            Ok(file) => find_unit(file, &[UnitKind::Procedure], procedure)
                                .map_or(Callee::Unknown(UnknownCall::NoRoutine), Callee::Node),
                            Err(u) => Callee::Unknown(u),
                        })
                    }
                    CallTarget::Library(_) => None,
                    CallTarget::Unknown { reason, .. } => Some(Callee::Unknown(*reason)),
                };
                let class = match (&c.target, callee) {
                    (CallTarget::Library(_), _) => "library".to_string(),
                    (_, Some(Callee::Unknown(r))) => format!("unknown:{}", r.as_str()),
                    (CallTarget::Procedure(_), _) => "internal-procedure".to_string(),
                    (CallTarget::Function(_), _) => "function".to_string(),
                    (CallTarget::Method(_), _) => "method".to_string(),
                    (CallTarget::Program(_), _) => "program".to_string(),
                    (CallTarget::InProgram { .. }, _) => "in-persistent-program".to_string(),
                    (CallTarget::Unknown { .. }, _) => "unknown".to_string(),
                };
                if matches!(
                    c.kind,
                    CallKind::Run | CallKind::Persistent | CallKind::Remote
                ) {
                    if matches!(callee, Some(Callee::Unknown(_))) {
                        run_unknown += 1;
                    } else {
                        run_resolved += 1;
                    }
                }
                if let (Some(want), Some(Callee::Unknown(r))) = (&explain, callee)
                    && want.strip_prefix("call:") == Some(r.as_str())
                {
                    println!(
                        "explain {}:{} {:?}",
                        f.path.display(),
                        line(&f.lines, c.at.start),
                        c.target
                    );
                }
                *sites.entry(format!("{:?} {class}", c.kind)).or_default() += 1;
                site_callee.insert((fi, c.at.start), callee);
                callee
            });
            nodes.push(node);
        }
    }
    let t = Instant::now();
    let graph = summary::graph_of(&nodes);
    let summaries = summary::summarize(&nodes, &graph);
    let solve_time = t.elapsed();

    // Searches.
    let mut searches: BTreeMap<String, usize> = BTreeMap::new();
    let (mut db_searches, mut db_unknown, mut db_whole) = (0usize, 0usize, 0usize);
    let mut units_total = 0usize;
    let mut unit_unknown: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut units_with_unknown = 0usize;
    for f in &files {
        for e in f.effects.effects() {
            let Some(a) = &e.access else {
                continue;
            };
            let db = matches!(e.table, Table::Database(_));
            let key = match a {
                Access::Index(c) if c.whole => format!("index whole ({})", c.reason.as_str()),
                Access::Index(c) => format!("index bracketed ({})", c.reason.as_str()),
                Access::TableScan => "table-scan".to_string(),
                Access::Rowid => "rowid".to_string(),
                Access::Unknown(u) => format!("unknown {}", u.as_str()),
            };
            let scope = match &e.table {
                Table::Database(_) => "db",
                Table::TempTable { .. } => "tt",
                Table::Unknown(_) => "?",
            };
            *searches.entry(format!("{scope} {key}")).or_default() += 1;
            if db {
                db_searches += 1;
                db_unknown += usize::from(matches!(a, Access::Unknown(_)));
                db_whole += usize::from(matches!(a, Access::Index(c) if c.whole));
            }
            if let (Some(want), Access::Unknown(u)) = (&explain, a)
                && want == u.as_str()
            {
                println!(
                    "explain {}:{}",
                    f.path.display(),
                    line(&f.lines, e.at.start)
                );
            }
        }
        for u in f.effects.units() {
            units_total += 1;
            units_with_unknown += usize::from(!u.unknown.is_empty());
            for (r, _) in &u.unknown {
                *unit_unknown.entry(r.as_str()).or_default() += 1;
            }
        }
    }

    // Summaries.
    let complete = summaries.iter().filter(|s| s.partial.is_empty()).count();
    let recursive = summaries.iter().filter(|s| s.recursive).count();
    let writes = summaries.iter().filter(|s| s.writes()).count();
    let mut partial: BTreeMap<String, usize> = BTreeMap::new();
    for s in &summaries {
        for p in &s.partial {
            *partial.entry(p.as_str()).or_default() += 1;
        }
    }

    // TX004: would a callee summary silence the finding?
    let mut tx: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut tx_rows: Vec<String> = Vec::new();
    for (fi, f) in files.iter().enumerate() {
        for &(at, root_line) in &f.tx004 {
            let class = match site_callee.get(&(fi, at)).copied().flatten() {
                None => "no call site",
                Some(Callee::Unknown(_)) => "callee unknown",
                Some(Callee::Node(m)) => {
                    let s = &summaries[m as usize];
                    if s.known_no_writes() {
                        "silenced: no database write"
                    } else if s.writes() {
                        "callee writes"
                    } else {
                        "callee summary partial"
                    }
                }
            };
            *tx.entry(class).or_default() += 1;
            let detail = match site_callee.get(&(fi, at)).copied().flatten() {
                Some(Callee::Node(m)) => summaries[m as usize]
                    .partial
                    .iter()
                    .map(|p| p.as_str())
                    .collect::<Vec<_>>()
                    .join(","),
                Some(Callee::Unknown(u)) => u.as_str().to_string(),
                None => String::new(),
            };
            tx_rows.push(format!(
                "{}:{root_line}\t{class}\t{detail}",
                f.path.display()
            ));
        }
    }

    let pct = |a: usize, b: usize| {
        if b == 0 {
            0.0
        } else {
            100.0 * a as f64 / b as f64
        }
    };
    println!("root                  {}", root.display());
    println!("files analysed        {} of {}", files.len(), roots.len());
    println!("failed                {failed}");
    println!("panics                {panics}");
    println!("parse recovered       {recovered}");
    println!("units                 {units_total}");
    println!();
    println!("call sites by kind and target:");
    for (k, v) in &sites {
        println!("  {k:<48}{v:>9}");
    }
    println!(
        "RUN sites             resolved {run_resolved}, unknown {run_unknown} ({:.1} % unknown)",
        pct(run_unknown, run_resolved + run_unknown)
    );
    println!();
    println!("searches (FIND, FOR, CAN-FIND, OPEN QUERY, PRESELECT):");
    for (k, v) in &searches {
        println!("  {k:<48}{v:>9}");
    }
    println!(
        "database searches     {db_searches}: index unknown {db_unknown} ({:.1} %), whole-index {db_whole} ({:.1} %)",
        pct(db_unknown, db_searches),
        pct(db_whole, db_searches)
    );
    println!();
    println!(
        "units with Unknown effects {units_with_unknown} of {units_total} ({:.1} %):",
        pct(units_with_unknown, units_total)
    );
    for (k, v) in &unit_unknown {
        println!("  {k:<48}{v:>9}");
    }
    println!();
    println!(
        "summaries             complete {complete} of {} ({:.1} %), recursive {recursive}, with writes {writes}",
        summaries.len(),
        pct(complete, summaries.len())
    );
    for (k, v) in &partial {
        println!("  partial {k:<40}{v:>9}");
    }
    let _ = Partial::Bound;
    println!();
    println!("TX004 findings by callee summary:");
    for (k, v) in &tx {
        println!("  {k:<48}{v:>9}");
    }
    println!();
    println!("time expand+parse     {:.2?}", times.expand_parse);
    println!("time scope            {:.2?}", times.scope);
    println!("time effects+calls    {:.2?}", times.effects);
    println!("time summaries        {solve_time:.2?}");
    println!("wall time             {:.2?}", started.elapsed());

    if let Some(out) = std::env::var_os("OXABL_EFFECTS_TX004") {
        tx_rows.sort();
        let mut w = std::io::BufWriter::new(std::fs::File::create(out).expect("create"));
        for r in &tx_rows {
            let _ = writeln!(w, "{r}");
        }
    }
}

fn canonical(p: &Path) -> PathBuf {
    std::fs::canonicalize(p).unwrap_or_else(|_| p.to_path_buf())
}

fn line(starts: &[usize], at: u32) -> usize {
    starts.partition_point(|&s| s <= at as usize)
}

/// Expand, parse and analyse one file. Lines are of the expanded text.
fn analyse(
    pipeline: &LintPipeline,
    config: &PipelineConfig,
    schema: Option<&Schema>,
    path: &Path,
    source: &str,
    times: &mut Times,
) -> Option<(FileResult, bool)> {
    let t = Instant::now();
    let expansion = pipeline.with_file(path).expand(source);
    let expanded = expansion.expanded()?;
    let text = expanded.text.as_str();
    let tokens = oxabl_lexer::tokenize(text);
    let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
    times.expand_parse += t.elapsed();
    let recovered = !program.errors.is_empty();
    let elided = expanded.elided_offsets();
    let elided_includes = expanded.elided_include_offsets();
    let options = ScopeOptions {
        parse_recovered: recovered,
        elided,
        elided_includes,
        ..ScopeOptions::default()
    };
    let t = Instant::now();
    let flow = oxabl_flow::build_file_with(&program.statements, text, &options.flow);
    let scopes =
        oxabl_scope::analyze_flow(&program.statements, text, &tokens, &flow, schema, &options);
    times.scope += t.elapsed();
    let t = Instant::now();
    let calls = FileCalls::extract(&program.statements, text, &tokens);
    let effects = oxabl_effects::analyze_file(
        &program.statements,
        text,
        &tokens,
        &flow,
        &scopes,
        &calls,
        schema,
        &EffectOptions { elided },
    );
    times.effects += t.elapsed();

    let empty = Schema::empty();
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, text, schema.unwrap_or(&empty))
        .with_tokens(&tokens)
        .with_elided(elided)
        .with_elided_includes(elided_includes)
        .with_parse_recovered(recovered);
    ctx.schema_loaded = config.schema_loaded;
    // Only findings in the file's own text: one in an include is reported
    // in the include, or at the `{...}` site, by the lint run.
    let root_lines: Vec<usize> = std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect();
    let tx004 = oxabl_lint::transaction_spans_call::check(&scopes, &program.statements, &ctx)
        .iter()
        .filter_map(|d| {
            let root = expanded.resolve_root_span(d.span.span)?;
            Some((d.span.span.start, line(&root_lines, root.start)))
        })
        .collect();
    let lines = std::iter::once(0)
        .chain(text.match_indices('\n').map(|(i, _)| i + 1))
        .collect();
    Some((
        FileResult {
            path: path.to_path_buf(),
            effects,
            lines,
            tx004,
        },
        recovered,
    ))
}
