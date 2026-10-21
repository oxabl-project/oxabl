//! `oxabl check --watch`: check once, then re-check what each edit can change.
//!
//! A cycle re-checks the files that changed and the files that depend on them
//! (the reverse include and class graph), and prints a file's findings only
//! when they differ from the last cycle.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::time::Duration;

use notify::RecursiveMode;
use notify_debouncer_full::new_debouncer;
use oxabl_pipeline::{
    ConfigOverrides, ConfigWarning, FormatPipeline, LintPipeline, PipelineConfig, ReverseGraph,
    Subject,
};
use oxabl_workspace::{discover_path_in, walk_include_fragments};

use crate::{CheckPass, Task};

const DEBOUNCE: Duration = Duration::from_millis(200);
const POLL: Duration = Duration::from_millis(100);

/// Extensions that can change a result besides the configured root extensions:
/// includes, the schema, and the config file.
const RELEVANT: [&str; 4] = ["i", "df", "toml", "t"];

static STOP: AtomicBool = AtomicBool::new(false);

pub struct Options<'a> {
    pub path: &'a Path,
    pub preprocess: bool,
    pub include_paths: &'a [PathBuf],
    pub schema_path: Option<&'a Path>,
    pub no_lint: bool,
    pub no_format: bool,
    pub include_findings: bool,
    pub jobs: usize,
}

/// What one file printed last cycle, keyed by its canonical path.
type Seen = HashMap<PathBuf, String>;

pub fn run(opts: &Options<'_>) -> ExitCode {
    if let Err(e) = ctrlc::set_handler(|| STOP.store(true, Ordering::SeqCst)) {
        eprintln!("error: cannot install the Ctrl-C handler: {e}");
        return ExitCode::from(2);
    }

    let (sender, receiver) = mpsc::channel();
    let mut debouncer = match new_debouncer(DEBOUNCE, None, move |result| {
        let _ = sender.send(result);
    }) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error: cannot start the file watcher: {e}");
            return ExitCode::from(2);
        }
    };

    let mut seen = Seen::new();
    let config = match cycle(opts, &mut seen, None) {
        Ok(config) => config,
        Err(msg) => {
            eprintln!("error: {msg}");
            return ExitCode::from(2);
        }
    };

    let mut roots = vec![opts.path.to_path_buf()];
    roots.extend(config.include_paths.iter().cloned());
    let mut watched: Vec<PathBuf> = Vec::new();
    for root in roots {
        let Ok(root) = std::fs::canonicalize(&root) else {
            continue;
        };
        let (target, mode) = if root.is_dir() {
            (root, RecursiveMode::Recursive)
        } else {
            let parent = root.parent().map_or(root.clone(), Path::to_path_buf);
            (parent, RecursiveMode::NonRecursive)
        };
        if let Err(e) = debouncer.watch(&target, mode) {
            eprintln!("warning: cannot watch {}: {e}", target.display());
        } else {
            watched.push(target);
        }
    }
    println!("watching for changes, Ctrl-C to stop");

    while !STOP.load(Ordering::SeqCst) {
        let events = match receiver.recv_timeout(POLL) {
            Ok(Ok(events)) => events,
            Ok(Err(errors)) => {
                for e in errors {
                    eprintln!("warning: watcher: {e}");
                }
                continue;
            }
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break,
        };
        let mut changed: Vec<PathBuf> = events
            .into_iter()
            .filter(|event| !event.kind.is_access())
            .flat_map(|event| event.event.paths)
            .filter(|path| relevant(path, &watched, &config.root_extensions))
            .map(|path| canonical(&path))
            .collect();
        changed.sort();
        changed.dedup();
        if changed.is_empty() {
            continue;
        }
        if let Err(msg) = cycle(opts, &mut seen, Some(&changed)) {
            eprintln!("error: {msg}");
        }
    }
    ExitCode::SUCCESS
}

/// Whether a change to `path` can alter a result. Hidden directories (`.git`)
/// below a watched root are skipped, as discovery skips them.
fn relevant(path: &Path, watched: &[PathBuf], root_extensions: &[String]) -> bool {
    let inside = watched
        .iter()
        .find_map(|root| path.strip_prefix(root).ok())
        .unwrap_or(path);
    let hidden = inside.components().any(|c| {
        c.as_os_str()
            .to_str()
            .is_some_and(|name| name.starts_with('.') && name.len() > 1)
    });
    if hidden {
        return false;
    }
    path.extension()
        .and_then(|e| e.to_str())
        .is_some_and(|ext| {
            RELEVANT.iter().any(|known| ext.eq_ignore_ascii_case(known))
                || root_extensions
                    .iter()
                    .any(|known| ext.eq_ignore_ascii_case(known))
        })
}

/// Canonical when the path exists; otherwise its canonical parent plus the name,
/// so a deleted file still matches the key it had.
fn canonical(path: &Path) -> PathBuf {
    if let Ok(path) = std::fs::canonicalize(path) {
        return path;
    }
    match (path.parent(), path.file_name()) {
        (Some(parent), Some(name)) => std::fs::canonicalize(parent)
            .unwrap_or_else(|_| parent.to_path_buf())
            .join(name),
        _ => path.to_path_buf(),
    }
}

/// One pass: everything when `changed` is `None`, otherwise the changed files
/// and their dependents. Returns the config the pass ran under.
fn cycle(
    opts: &Options<'_>,
    seen: &mut Seen,
    changed: Option<&[PathBuf]>,
) -> Result<PipelineConfig, String> {
    let overrides = ConfigOverrides {
        include_paths: opts.include_paths.to_vec(),
        schema_path: opts.schema_path.map(Path::to_path_buf),
        style: None,
    };
    let (config, warnings) = PipelineConfig::resolve(opts.path, &overrides);
    if let Some(msg) = warnings.iter().find_map(|w| match w {
        ConfigWarning::Config(msg) => Some(msg),
        _ => None,
    }) {
        return Err(msg.clone());
    }
    if changed.is_none() {
        for warning in &warnings {
            eprintln!("warning: {warning}");
        }
    }

    let files = discover_path_in(opts.path, &config.root_extensions)?;
    let fs = config.file_system();
    let indexed: Vec<PathBuf> = files.iter().map(|f| canonical(f)).collect();
    let producers = LintPipeline::new(&config, &fs).shared_producers_parallel(&indexed, opts.jobs);
    let lint = LintPipeline::new(&config, &fs)
        .with_preprocess(opts.preprocess)
        .with_shared_producers(&producers);
    // TX004 asks for the summaries of its callees. Build them once for the
    // whole walk and share them, as each handle would otherwise build its own
    // from the one file it happens to see first. Rebuilt every pass, so the
    // next pass sees the edit.
    let summaries = (!opts.no_lint && lint.callee_summaries_enabled()).then(|| {
        std::sync::Arc::new(lint.build_workspace_effect_summaries_parallel(&indexed, opts.jobs))
    });
    let lint = match &summaries {
        Some(s) => lint.with_effect_summaries(std::sync::Arc::clone(s)),
        None => lint,
    };

    let fragments: Vec<(PathBuf, PathBuf)> = if !opts.no_lint && opts.path.is_dir() {
        walk_include_fragments(opts.path)
            .into_iter()
            .map(|file| {
                let key = canonical(&file);
                (file, key)
            })
            .filter(|(_, key)| lint.with_file(key).context_free_rules_enabled())
            .collect()
    } else {
        Vec::new()
    };

    // A configuration change can alter every result.
    let everything = changed.is_none_or(|paths| {
        paths.iter().any(|p| {
            p.extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| e.eq_ignore_ascii_case("toml") || e.eq_ignore_ascii_case("df"))
        })
    });
    let affected: Option<HashSet<PathBuf>> = if everything {
        None
    } else {
        let (graph, _) = ReverseGraph::build_with(
            &indexed,
            opts.jobs,
            || {
                let handle = LintPipeline::new(&config, &fs)
                    .with_preprocess(opts.preprocess)
                    .with_shared_producers(&producers);
                match &summaries {
                    Some(s) => handle.with_effect_summaries(std::sync::Arc::clone(s)),
                    None => handle,
                }
            },
            |_, _, _| (),
        );
        let mut set: HashSet<PathBuf> = HashSet::new();
        for path in changed.unwrap_or_default() {
            set.extend(graph.rebuild_set(&Subject::file(path)));
            set.insert(path.clone());
        }
        Some(set)
    };
    let wanted = |key: &PathBuf| affected.as_ref().is_none_or(|set| set.contains(key));

    let format = FormatPipeline::new(config.style.clone());
    let pass = CheckPass {
        json_output: false,
        no_lint: opts.no_lint,
        no_format: opts.no_format,
        include_findings: opts.include_findings,
        baseline: None,
        format: &format,
        decoding: config.decoding,
    };
    let tasks: Vec<Task<'_>> = files
        .iter()
        .zip(&indexed)
        .map(|(file, key)| (file, key, false))
        .chain(fragments.iter().map(|(file, key)| (file, key, true)))
        .filter(|(_, key, _)| wanted(key))
        .map(|(file, key, fragment)| Task {
            file,
            indexed_path: key,
            fragment,
        })
        .collect();

    let live: HashSet<&PathBuf> = indexed
        .iter()
        .chain(fragments.iter().map(|(_, key)| key))
        .collect();
    seen.retain(|key, _| live.contains(key));

    let first = changed.is_none();
    let mut changed_files = 0usize;
    let mut with_findings = 0usize;
    let mut results: Vec<String> = Vec::with_capacity(tasks.len());
    crate::run_ordered(
        opts.jobs.min(tasks.len()),
        config.decoding,
        &lint,
        || {
            let handle = LintPipeline::new(&config, &fs)
                .with_preprocess(opts.preprocess)
                .with_shared_producers(&producers);
            match &summaries {
                Some(s) => handle.with_effect_summaries(std::sync::Arc::clone(s)),
                None => handle,
            }
        },
        &tasks,
        |lint, texts, task| pass.run(lint, texts, task),
        |unit| {
            let mut text = String::new();
            for (_, part) in &unit.output.0 {
                text.push_str(part);
            }
            if let Some(path) = &unit.drifted {
                text.push_str(&format!("{path}: would be reformatted\n"));
            }
            results.push(text);
        },
    );
    for (task, text) in tasks.iter().zip(results) {
        if !text.is_empty() {
            with_findings += 1;
        }
        let before = seen.get(task.indexed_path).map(String::as_str);
        if before.unwrap_or("") != text {
            changed_files += 1;
            if text.is_empty() {
                println!("{}: no findings", task.file.display());
            } else {
                print!("{text}");
            }
        }
        seen.insert(task.indexed_path.to_path_buf(), text);
    }

    let plural = if tasks.len() == 1 { "file" } else { "files" };
    if first {
        println!(
            "checked {} {plural}: {with_findings} with findings",
            tasks.len()
        );
    } else {
        println!(
            "rechecked {} {plural}: {changed_files} changed",
            tasks.len()
        );
    }
    Ok(config)
}
