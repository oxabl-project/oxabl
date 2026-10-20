//! Compute the dependency edge set of every compile root of a tree, as a build
//! tool asks for it, and time it.
//!
//! ```text
//! cargo run --release -p oxabl_pipeline --example edge_walk -- \
//!     --root path/to/project [--include DIR]... [--own-dir-first all|none|PATH-PART] \
//!     [--schema] [--dlc DIR] [--threads N] [--api old|shared] [--fs-cache on|off] \
//!     [--dump FILE] [--compare FILE]
//! ```
//!
//! The settings are a build tool's: compile roots are `.p`, `.cls`, and `.t`
//! under a `tr` or `trigger` directory; sources are Windows-1252; names resolve
//! ignoring case; every edge set carries its conditional include closure; the
//! `SHARED` producers come from one map over every ABL file; the predefined
//! names are a 64-bit Linux batch compile. A root compiled own directory first
//! searches `.` and `rf` below its directory ahead of the include paths.
//! `--schema` loads the `.df` files of the tree's `oxabl.toml`. `--dlc` adds
//! the `psc/dlc/{tty,src,.,bin}` directories of a copy of an OpenEdge install
//! to the include paths.
//!
//! `--dump` writes every root's whole edge set, targets as paths, so two runs
//! (thread counts, builds) can be compared; `--compare` diffs against a dump.

use std::collections::BTreeMap;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use oxabl_pipeline::{
    ConfigOverrides, DependencyEdges, EdgeTarget, IncludeContext, LintPipeline, PipelineConfig,
    SharedProducers, SharedRun, definitions_in,
};
use oxabl_workspace::{
    CachedFileSystem, EncodedFileSystem, FileSystem, SourceDecoding, SourceEncoding,
};

const ROOT_EXTENSIONS: [&str; 3] = ["p", "cls", "t"];
const SOURCE_EXTENSIONS: [&str; 6] = ["p", "cls", "t", "i", "v", "w"];
const OWN_DIR_SEARCH: [&str; 2] = [".", "rf"];

/// Which roots search their own directory first: all, none, or those whose
/// path contains a part (matched ignoring case).
#[derive(Clone, PartialEq)]
enum OwnDir {
    All,
    Marked(String),
    None,
}

struct Args {
    root: PathBuf,
    includes: Vec<PathBuf>,
    own_dir: OwnDir,
    schema: bool,
    threads: usize,
    dump: Option<PathBuf>,
    compare: Option<PathBuf>,
    limit: Option<usize>,
    stride: usize,
    shared: bool,
    cache_fs: bool,
    dlc: Option<PathBuf>,
    slowest: usize,
    only: Option<String>,
}

fn args() -> Args {
    let mut out = Args {
        root: PathBuf::new(),
        includes: Vec::new(),
        own_dir: OwnDir::None,
        schema: false,
        threads: 8,
        dump: None,
        compare: None,
        limit: None,
        stride: 1,
        shared: true,
        cache_fs: true,
        dlc: None,
        slowest: 0,
        only: None,
    };
    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        let mut value = || it.next().expect("missing value");
        match arg.as_str() {
            "--root" => out.root = PathBuf::from(value()),
            "--include" => out.includes.push(PathBuf::from(value())),
            "--own-dir-first" => {
                out.own_dir = match value().as_str() {
                    "all" => OwnDir::All,
                    "none" => OwnDir::None,
                    part => OwnDir::Marked(part.to_ascii_lowercase()),
                }
            }
            "--schema" => out.schema = true,
            "--threads" => out.threads = value().parse().expect("threads"),
            "--dump" => out.dump = Some(PathBuf::from(value())),
            "--compare" => out.compare = Some(PathBuf::from(value())),
            "--limit" => out.limit = Some(value().parse().expect("limit")),
            "--stride" => out.stride = value().parse().expect("stride"),
            "--api" => out.shared = value() != "old",
            "--fs-cache" => out.cache_fs = value() != "off",
            "--dlc" => out.dlc = Some(PathBuf::from(value())),
            "--slowest" => out.slowest = value().parse().expect("slowest"),
            "--only" => out.only = Some(value()),
            other => panic!("unknown argument {other}"),
        }
    }
    out.root = std::fs::canonicalize(&out.root).expect("root");
    if out.includes.is_empty() {
        out.includes.push(out.root.clone());
    }
    out.includes = out
        .includes
        .iter()
        .map(|p| {
            if p.is_absolute() {
                p.clone()
            } else {
                out.root.join(p)
            }
        })
        .collect();
    if let Some(dlc) = &out.dlc {
        for dir in ["psc/dlc/tty", "psc/dlc/src", "psc/dlc", "psc/dlc/bin"] {
            let dir = dlc.join(dir);
            if dir.is_dir() {
                out.includes.push(dir);
            }
        }
    }
    out
}

/// Windows-1252 text, names resolved ignoring case, as a build tool reads.
struct BuildFs {
    inner: EncodedFileSystem,
}

impl FileSystem for BuildFs {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn resolve_include(&self, include_paths: &[PathBuf], name: &str) -> Option<PathBuf> {
        include_paths.iter().find_map(|dir| {
            let exact = dir.join(name);
            if exact.is_file() {
                return Some(exact);
            }
            let relative = Path::new(name);
            if relative.is_absolute() {
                return None;
            }
            self.inner
                .find_ignoring_case(dir, relative)
                .into_iter()
                .find(|p| p.is_file())
        })
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        self.inner.find_ignoring_case(dir, relative)
    }
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(kind) = entry.file_type() else {
            continue;
        };
        if kind.is_dir() {
            if !entry.file_name().to_string_lossy().starts_with('.') {
                walk(&path, out);
            }
        } else if kind.is_file() && has_extension(&path, &SOURCE_EXTENSIONS) {
            out.push(path);
        }
    }
}

fn has_extension(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|e| extensions.iter().any(|x| x.eq_ignore_ascii_case(e)))
}

fn is_root(path: &Path) -> bool {
    if !has_extension(path, &ROOT_EXTENSIONS) {
        return false;
    }
    if !has_extension(path, &["t"]) {
        return true;
    }
    path.parent().is_some_and(|dir| {
        dir.components().any(|c| {
            let c = c.as_os_str().to_string_lossy();
            c.eq_ignore_ascii_case("tr") || c.eq_ignore_ascii_case("trigger")
        })
    })
}

fn own_dir_first(mode: &OwnDir, path: &Path) -> bool {
    match mode {
        OwnDir::All => true,
        OwnDir::None => false,
        OwnDir::Marked(part) => path.to_string_lossy().to_ascii_lowercase().contains(part),
    }
}

fn config(args: &Args) -> PipelineConfig {
    let mut config = if args.schema {
        let (resolved, _) = PipelineConfig::resolve(&args.root, &ConfigOverrides::default());
        PipelineConfig {
            schema: resolved.schema,
            schema_loaded: resolved.schema_loaded,
            ..PipelineConfig::default()
        }
    } else {
        PipelineConfig::default()
    };
    config.include_paths = args.includes.clone();
    config.root_extensions = ROOT_EXTENSIONS.iter().map(|e| e.to_string()).collect();
    config.case_insensitive_lookup = true;
    config.decoding = SourceDecoding::new(SourceEncoding::Windows1252, None);
    // The batch compile in the container.
    config.preprocessor.opsys = "UNIX".to_string();
    config.preprocessor.window_system = "TTY".to_string();
    config.preprocessor.batch_mode = true;
    config.preprocessor.process_architecture = 64;
    config.preprocessor.proversion = Some("12.8".to_string());
    config
}

#[derive(Default, Clone, Copy)]
struct Stages {
    read: Duration,
    expand: Duration,
    analyze: Duration,
    edges: Duration,
}

impl Stages {
    fn add(&mut self, other: &Stages) {
        self.read += other.read;
        self.expand += other.expand;
        self.analyze += other.analyze;
        self.edges += other.edges;
    }
}

/// `target` as a path, or a name for a table.
fn target(handle: &LintPipeline<'_>, target: &EdgeTarget) -> String {
    match target {
        EdgeTarget::IncludePath(path) => format!("file:{}", path.display()),
        EdgeTarget::IndexedFile { file, name } => match handle.indexed_path(*file) {
            Some(path) => format!("file:{}", path.display()),
            None => format!("unnamed:{}", name.as_str()),
        },
        EdgeTarget::SchemaTable(name) => format!("table:{}", name.as_str()),
        #[allow(unreachable_patterns)]
        other => format!("{other:?}"),
    }
}

/// Every fact of `edges`, one sorted line each, with paths for index ids.
fn lines(handle: &LintPipeline<'_>, edges: &DependencyEdges) -> Vec<String> {
    let mut out = Vec::new();
    let t = |x: &Option<EdgeTarget>| x.as_ref().map(|x| target(handle, x));
    for e in edges.edges() {
        out.push(format!(
            "edge {:?} {} {:?} {:?}",
            e.kind,
            target(handle, &e.target),
            e.span,
            e.include_site
        ));
    }
    for u in edges.unresolved() {
        out.push(format!("unresolved {u:?}"));
    }
    for c in edges.conditional_includes() {
        out.push(format!("conditional-include {c:?}"));
    }
    for r in edges.conditional_references() {
        out.push(format!(
            "conditional-ref {:?} {:?} {} {:?} {:?} {:?} {:?}",
            r.kind,
            r.syntax,
            r.name,
            t(&r.target),
            r.span,
            r.include_site,
            r.branches
        ));
    }
    for c in edges.conditional_include_closure() {
        let refs: Vec<String> = c
            .references
            .iter()
            .map(|r| {
                format!(
                    "{:?}/{}/{}/{:?}",
                    r.kind,
                    r.conditional,
                    r.name,
                    t(&r.target)
                )
            })
            .collect();
        out.push(format!(
            "closure {} {:?} {:?} {:?} {:?} {} {:?}",
            c.name, c.via, c.path, c.reaches, c.unresolved, c.complete, refs
        ));
    }
    out.push(format!("completeness {:?}", edges.completeness()));
    out.sort();
    out
}

type Outcome = Result<Vec<String>, String>;

fn main() {
    let args = args();
    let started = Instant::now();
    let mut files = Vec::new();
    walk(&args.root, &mut files);
    files.sort();
    let mut roots: Vec<PathBuf> = files
        .iter()
        .filter(|p| is_root(p))
        .filter(|p| {
            args.only
                .as_ref()
                .is_none_or(|only| p.to_string_lossy().contains(only.as_str()))
        })
        .step_by(args.stride.max(1))
        .cloned()
        .collect();
    if let Some(limit) = args.limit {
        roots.truncate(limit);
    }
    let walked = started.elapsed();

    let build_fs = || BuildFs {
        inner: EncodedFileSystem::new(SourceDecoding::new(SourceEncoding::Windows1252, None)),
    };
    let (plain_fs, cached_fs);
    let fs: &dyn FileSystem = if args.cache_fs {
        cached_fs = CachedFileSystem::new(build_fs());
        &cached_fs
    } else {
        plain_fs = build_fs();
        &plain_fs
    };
    let config = config(&args);
    let own_dir_config = PipelineConfig {
        root_relative_include_paths: OWN_DIR_SEARCH.iter().map(PathBuf::from).collect(),
        ..config.clone()
    };
    let config_loaded = started.elapsed() - walked;

    let producers_started = Instant::now();
    let producers = shared_producers(&args, &config, &own_dir_config, fs, &files);
    let producers_took = producers_started.elapsed();
    if let Ok(names) = std::env::var("EDGE_WALK_PRODUCERS_OF") {
        for name in names.split(',') {
            let of = producers.producers_of(&oxabl_semantic::IndexName::new(name));
            println!("producers of {name}: {}", of.len());
            for path in of.iter().take(5) {
                println!("  {}", path.display());
            }
        }
    }

    let cursor = AtomicUsize::new(0);
    let results: Mutex<BTreeMap<PathBuf, Outcome>> = Mutex::default();
    let times: Mutex<Vec<(Duration, PathBuf)>> = Mutex::default();
    let stages = Mutex::new(Stages::default());
    let analysis_started = Instant::now();
    let shared = |config| SharedRun::new(config, fs).with_shared_producers(&producers);
    let (shared_plain, shared_own) = (shared(&config), shared(&own_dir_config));
    std::thread::scope(|scope| {
        for _ in 0..args.threads {
            scope.spawn(|| {
                let (plain, own) = (
                    run_handle(args.shared, &config, fs, &producers, &shared_plain),
                    run_handle(args.shared, &own_dir_config, fs, &producers, &shared_own),
                );
                let mut local = Stages::default();
                let mut done = Vec::new();
                let mut took = Vec::new();
                loop {
                    let index = cursor.fetch_add(1, Ordering::Relaxed);
                    let Some(path) = roots.get(index) else {
                        break;
                    };
                    let run = if own_dir_first(&args.own_dir, path) {
                        &own
                    } else {
                        &plain
                    };
                    let started = Instant::now();
                    done.push((path.clone(), one(run, path, &mut local)));
                    took.push((started.elapsed(), path.clone()));
                }
                results.lock().unwrap().extend(done);
                times.lock().unwrap().extend(took);
                stages.lock().unwrap().add(&local);
            });
        }
    });
    let analysis = analysis_started.elapsed();
    let results = results.into_inner().unwrap();
    let stages = stages.into_inner().unwrap();

    let failed = results.values().filter(|r| r.is_err()).count();
    let edge_lines: usize = results
        .values()
        .filter_map(|r| r.as_ref().ok())
        .map(Vec::len)
        .sum();
    println!(
        "{} roots {} files {} threads {} schema {} | walk {:.2?} config {:.2?} producers {} in {:.2?} | analysis {:.2?} | total {:.2?}",
        match (args.shared, args.cache_fs) {
            (true, true) => "shared+fscache",
            (true, false) => "shared",
            (false, true) => "old+fscache",
            (false, false) => "old",
        },
        roots.len(),
        files.len(),
        args.threads,
        config.schema_loaded,
        walked,
        config_loaded,
        producers.len(),
        producers_took,
        analysis,
        started.elapsed()
    );
    println!(
        "thread-summed: read {:.2?} expand {:.2?} analyze {:.2?} edges {:.2?} | failed {failed} lines {edge_lines}",
        stages.read, stages.expand, stages.analyze, stages.edges
    );

    println!("process: {}", process_usage());
    let mut times = times.into_inner().unwrap();
    times.sort_by_key(|a| std::cmp::Reverse(a.0));
    for (took, path) in times.iter().take(args.slowest) {
        println!("slow: {took:.2?} {}", path.display());
    }

    if let Some(path) = &args.dump {
        let mut out = io::BufWriter::new(std::fs::File::create(path).expect("dump"));
        for (root, outcome) in &results {
            match outcome {
                Ok(lines) => {
                    writeln!(out, "== {}", root.display()).unwrap();
                    for line in lines {
                        writeln!(out, "{line}").unwrap();
                    }
                }
                Err(e) => writeln!(out, "== {} ERR {e}", root.display()).unwrap(),
            }
        }
    }
    if let Some(path) = &args.compare {
        compare(path, &results);
    }
}

/// CPU time and peak memory of this process, from `/proc` (Linux only).
fn process_usage() -> String {
    let stat = std::fs::read_to_string("/proc/self/stat").unwrap_or_default();
    // The fields after the parenthesised command name; utime and stime are
    // the 14th and 15th fields of the line, in clock ticks of 1/100 s.
    let fields: Vec<&str> = stat
        .rsplit_once(')')
        .map_or(Vec::new(), |(_, rest)| rest.split_whitespace().collect());
    let ticks = |i: usize| {
        fields
            .get(i)
            .and_then(|f| f.parse::<f64>().ok())
            .unwrap_or(0.0)
            / 100.0
    };
    let peak = std::fs::read_to_string("/proc/self/status")
        .unwrap_or_default()
        .lines()
        .find_map(|l| l.strip_prefix("VmHWM:").map(|v| v.trim().to_string()))
        .unwrap_or_default();
    format!(
        "user {:.1}s sys {:.1}s peak rss {peak}",
        ticks(11),
        ticks(12)
    )
}

fn run_handle<'a>(
    shared: bool,
    config: &'a PipelineConfig,
    fs: &'a dyn FileSystem,
    producers: &'a SharedProducers,
    run: &'a SharedRun<'a>,
) -> LintPipeline<'a> {
    if shared {
        run.handle().with_conditional_include_closure(true)
    } else {
        LintPipeline::new(config, fs)
            .with_shared_producers(producers)
            .with_conditional_include_closure(true)
    }
}

fn one(run: &LintPipeline<'_>, path: &Path, stages: &mut Stages) -> Outcome {
    let t0 = Instant::now();
    let source = run
        .file_system()
        .read(path)
        .map_err(|e| format!("unreadable: {e}"))?;
    let handle = run.with_file(path.to_path_buf());
    let t1 = Instant::now();
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let expansion = handle.expand(&source);
        let t2 = Instant::now();
        let result = handle.analyze(&expansion);
        let t3 = Instant::now();
        let edges = handle.edges_of(&expansion, &result);
        let t4 = Instant::now();
        stages.expand += t2 - t1;
        stages.analyze += t3 - t2;
        stages.edges += t4 - t3;
        edges.map(|e| lines(&handle, &e))
    }));
    stages.read += t1 - t0;
    outcome.unwrap_or_else(|_| Err("panicked".into()))
}

fn shared_producers(
    args: &Args,
    config: &PipelineConfig,
    own_dir_config: &PipelineConfig,
    fs: &dyn FileSystem,
    files: &[PathBuf],
) -> SharedProducers {
    let (plain, own) = (context(config, fs), context(own_dir_config, fs));
    let cursor = AtomicUsize::new(0);
    let found = Mutex::new(Vec::new());
    std::thread::scope(|scope| {
        for _ in 0..args.threads {
            scope.spawn(|| {
                let mut local = Vec::new();
                loop {
                    let index = cursor.fetch_add(1, Ordering::Relaxed);
                    let Some(path) = files.get(index) else {
                        break;
                    };
                    let includes = if own_dir_first(&args.own_dir, path) {
                        &own
                    } else {
                        &plain
                    };
                    let started = Instant::now();
                    if let Ok(names) =
                        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            definitions_in(path, includes)
                        }))
                    {
                        local.push((path.clone(), names));
                    }
                    if args.slowest > 0 {
                        let took = started.elapsed();
                        if took > Duration::from_millis(200) {
                            println!("slow producer: {took:.2?} {}", path.display());
                        }
                    }
                }
                found.lock().unwrap().extend(local);
            });
        }
    });
    let mut found = found.into_inner().unwrap();
    found.sort_by(|a, b| a.0.cmp(&b.0));
    SharedProducers::from_definitions(found)
}

fn context<'a>(config: &'a PipelineConfig, fs: &'a dyn FileSystem) -> IncludeContext<'a> {
    IncludeContext {
        fs,
        include_paths: &config.include_paths,
        root_relative_paths: &config.root_relative_include_paths,
        predefined: &config.preprocessor,
    }
}

fn compare(path: &Path, results: &BTreeMap<PathBuf, Outcome>) {
    let text = std::fs::read_to_string(path).expect("compare file");
    let mut before: BTreeMap<PathBuf, Vec<String>> = BTreeMap::new();
    let mut current: Option<PathBuf> = None;
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("== ") {
            let (root, err) = match rest.split_once(" ERR ") {
                Some((root, err)) => (root, Some(err)),
                None => (rest, None),
            };
            let root = PathBuf::from(root);
            let entry = before.entry(root.clone()).or_default();
            if let Some(err) = err {
                entry.push(format!("ERR {err}"));
            }
            current = Some(root);
        } else if let Some(root) = &current {
            before.get_mut(root).unwrap().push(line.to_string());
        }
    }
    let mut differing = 0;
    let mut shown = 0;
    for (root, outcome) in results {
        let now = match outcome {
            Ok(lines) => lines.clone(),
            Err(e) => vec![format!("ERR {e}")],
        };
        let was = before
            .get(root)
            .cloned()
            .unwrap_or_else(|| vec!["<absent>".into()]);
        if now != was {
            differing += 1;
            if shown < 20 {
                shown += 1;
                println!("DIFF {}", root.display());
                for line in was.iter().filter(|l| !now.contains(l)).take(10) {
                    println!("  - {line}");
                }
                for line in now.iter().filter(|l| !was.contains(l)).take(10) {
                    println!("  + {line}");
                }
            }
        }
    }
    let missing = before.keys().filter(|k| !results.contains_key(*k)).count();
    println!(
        "compare: {differing} of {} roots differ, {missing} roots only in {}",
        results.len(),
        path.display()
    );
}
