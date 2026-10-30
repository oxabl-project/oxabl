//! Lazy, run-wide database summaries. Resolution uses the run's batch index.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_effects::{
    FileEffects,
    summary::{self, Node, Summary},
};
use oxabl_flow::UnitKind;
use oxabl_index::{
    BatchIndex,
    calls::{CallTarget, Callee, UnknownCall},
};
use oxabl_lint::CalleeSummaries;
use oxabl_semantic::AnalysisContext;

use crate::{Expansion, LintPipeline, PipelineConfig, ROOT_FILE_ID};

fn key(path: &Path) -> PathBuf {
    if path.as_os_str().is_empty() {
        return PathBuf::new();
    }
    oxabl_index::search::normalize_lexically(
        &std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf()),
    )
}

/// Direct effects and call-site answers for one expanded file.
#[derive(Debug)]
pub struct EffectFile {
    pub effects: FileEffects,
    summary_range: std::ops::Range<usize>,
    /// Expanded line starts. All effect positions use expanded coordinates.
    pub lines: Vec<u32>,
    /// Each unit's span mapped to root coordinates, if possible.
    pub root_units: Vec<Option<oxabl_ast::Span>>,
    root_headers: Vec<Option<u32>>,
    unknown_calls: Vec<usize>,
    callees: HashMap<u32, u32>,
    solved: Arc<Vec<Summary>>,
    names: Arc<Vec<String>>,
    fingerprint: u64,
}

pub(crate) fn fingerprint(expanded: &oxabl_analyze::ExpandedFile) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hash = std::collections::hash_map::DefaultHasher::new();
    expanded.text.hash(&mut hash);
    expanded.elided_offsets().hash(&mut hash);
    expanded.elided_include_offsets().hash(&mut hash);
    (!expanded.text_losses().is_empty()).hash(&mut hash);
    hash.finish()
}

impl EffectFile {
    /// Short database summaries at root header offsets. Include-only units are omitted.
    pub fn effect_lenses(&self) -> Vec<(u32, String)> {
        self.effects
            .units()
            .iter()
            .enumerate()
            .filter_map(|(i, u)| {
                let at = self.root_headers[i]?;
                let summary = &self.summaries()[i];
                let mut reads = std::collections::BTreeSet::new();
                let mut writes = std::collections::BTreeSet::new();
                for e in &summary.effects {
                    if e.op.writes() {
                        writes.insert(e.table.clone());
                    } else {
                        reads.insert(e.table.clone());
                    }
                }
                // Keep direct facts even when a bounded summary drops an effect.
                for e in self.effects.unit_effects(u.unit) {
                    let name = match &e.table {
                        oxabl_scope::Table::Database(n) => n.clone(),
                        oxabl_scope::Table::Unknown(n) => format!("?{n}"),
                        oxabl_scope::Table::TempTable { .. } => continue,
                    };
                    if e.op.writes() {
                        writes.insert(name);
                    } else {
                        reads.insert(name);
                    }
                }
                if reads.is_empty() && writes.is_empty() && u.calls.is_empty() {
                    return None;
                }
                let mut parts = Vec::new();
                for (verb, names) in [("reads", reads), ("writes", writes)] {
                    if !names.is_empty() {
                        parts.push(format!("{verb} {}", {
                            let remaining = names.len().saturating_sub(3);
                            let mut names =
                                names.into_iter().take(3).collect::<Vec<_>>().join(", ");
                            if remaining > 0 {
                                names.push_str(&format!(", +{remaining} tables"));
                            }
                            names
                        }));
                    }
                }
                if !u.calls.is_empty() {
                    let unknown = self.unknown_calls[i];
                    parts.push(format!(
                        "{} call{}{}",
                        u.calls.len(),
                        if u.calls.len() == 1 { "" } else { "s" },
                        if unknown > 0 {
                            format!(" ({unknown} unknown)")
                        } else {
                            String::new()
                        }
                    ));
                }
                if !summary.partial.is_empty() {
                    parts.push("partial".into());
                }
                Some((at, parts.join(" · ")))
            })
            .collect()
    }

    pub(crate) fn matches(&self, expansion: &Expansion) -> bool {
        expansion
            .expanded()
            .is_some_and(|e| self.fingerprint == fingerprint(e))
    }

    /// Unit summaries, in the same order as `effects.units()`.
    pub fn summaries(&self) -> &[Summary] {
        &self.solved[self.summary_range.clone()]
    }

    /// The stable file and unit name of a summary node.
    pub fn callee_name(&self, node: u32) -> Option<&str> {
        self.names.get(node as usize).map(String::as_str)
    }
}

impl CalleeSummaries for EffectFile {
    fn callee_summary(&self, at: u32) -> Option<&Summary> {
        self.solved.get(*self.callees.get(&at)? as usize)
    }
}

/// Summaries computed once for a lint run, including reachable programs.
#[derive(Debug, Default)]
pub struct WorkspaceEffects {
    files: BTreeMap<PathBuf, EffectFile>,
}

impl WorkspaceEffects {
    pub fn file(&self, path: &Path) -> Option<&EffectFile> {
        self.files.get(&key(path))
    }
}

struct Direct {
    path: PathBuf,
    effects: FileEffects,
    lines: Vec<u32>,
    root_units: Vec<Option<oxabl_ast::Span>>,
    root_headers: Vec<Option<u32>>,
    incomplete: Vec<oxabl_effects::EffectUnknown>,
    fingerprint: u64,
}

fn direct(config: &PipelineConfig, path: &Path, expansion: &Expansion) -> Option<Direct> {
    let expanded = expansion.expanded()?;
    let text = expanded.text.as_str();
    let tokens = oxabl_lexer::tokenize(text);
    let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
    let mut ctx = AnalysisContext::new(ROOT_FILE_ID, text, &config.schema)
        .with_tokens(&tokens)
        .with_parse_recovered(!program.errors.is_empty())
        .with_elided(expanded.elided_offsets())
        .with_elided_includes(expanded.elided_include_offsets());
    ctx.schema_loaded = config.schema_loaded;
    let shared = oxabl_lint::RunShared::default();
    let effects = shared.effects(&program.statements, &ctx).clone();
    let root_units: Vec<Option<oxabl_ast::Span>> = effects
        .units()
        .iter()
        .map(|u| {
            let first = oxabl_ast::Span {
                start: u.span.start,
                end: u.span.start.saturating_add(1),
            };
            let last = oxabl_ast::Span {
                start: u.span.end.saturating_sub(1),
                end: u.span.end,
            };
            let start = expanded
                .resolve_root_span(first)
                .or_else(|| expanded.resolve_include_site(first))?;
            let end = expanded
                .resolve_root_span(last)
                .or_else(|| expanded.resolve_include_site(last))?;
            Some(oxabl_ast::Span {
                start: start.start,
                end: end.end,
            })
        })
        .collect();
    let root_headers = effects
        .units()
        .iter()
        .enumerate()
        .map(|(i, u)| {
            if u.kind == UnitKind::Main {
                return root_units[i].map(|s| s.start);
            }
            expanded
                .resolve_root_span(oxabl_ast::Span {
                    start: u.span.start,
                    end: u.span.start.saturating_add(1),
                })
                .map(|s| s.start)
        })
        .collect();
    let mut incomplete = Vec::new();
    if !program.errors.is_empty() {
        incomplete.push(oxabl_effects::EffectUnknown::RecoveredParse);
    }
    if !expanded.unresolved_includes().is_empty()
        || !expanded.nested_unresolved_includes().is_empty()
        || !expanded.unreadable_includes().is_empty()
        || !expanded.text_losses().is_empty()
        || !expanded.elided_include_offsets().is_empty()
    {
        incomplete.push(oxabl_effects::EffectUnknown::UnexpandedInclude);
    }
    Some(Direct {
        incomplete,
        fingerprint: fingerprint(expanded),
        path: key(path),
        effects,
        root_units,
        root_headers,
        lines: std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| (i + 1) as u32))
            .collect(),
    })
}

impl LintPipeline<'_> {
    /// Build direct and transitive summaries without running lint rules.
    /// The supplied expansion takes precedence over the root's disk text.
    /// Missing, unreadable, and fatally unexpanded programs remain Unknown.
    pub fn build_effect_summaries(
        &self,
        expansion: &Expansion,
        known: &[PathBuf],
    ) -> WorkspaceEffects {
        self.summaries_from(self.file().map(|root| (root, expansion)), known, 1)
    }

    /// Build the summaries of `known` and the programs they reach, every file
    /// read from disk. A batch run builds this once and hands it to each of its
    /// handles with [`with_effect_summaries`](Self::with_effect_summaries).
    pub fn build_workspace_effect_summaries(&self, known: &[PathBuf]) -> WorkspaceEffects {
        self.summaries_from(None, known, 1)
    }

    /// [`build_workspace_effect_summaries`](Self::build_workspace_effect_summaries)
    /// on up to `threads` threads. The result is the same for any thread count.
    pub fn build_workspace_effect_summaries_parallel(
        &self,
        known: &[PathBuf],
        threads: usize,
    ) -> WorkspaceEffects {
        self.summaries_from(None, known, threads)
    }

    fn summaries_from(
        &self,
        root: Option<(&Path, &Expansion)>,
        known: &[PathBuf],
        threads: usize,
    ) -> WorkspaceEffects {
        let config = self.config();
        let index = BatchIndex::new(self.file_system(), &config.include_paths)
            .with_run_extensions(&config.run_extensions)
            .with_preprocessor(&config.preprocessor);
        let mut paths: Vec<PathBuf> = root.map(|(p, _)| p.to_path_buf()).into_iter().collect();
        paths.extend(
            known
                .iter()
                .filter(|p| !p.extension().is_some_and(|e| e.eq_ignore_ascii_case("i")))
                .cloned(),
        );
        let fs = self.file_system();
        let preprocess = self.preprocess();
        let load = |path: &PathBuf| {
            oxabl_common::catch_panic(|| {
                if let Some((_, expansion)) = root.filter(|(r, _)| *path == key(r)) {
                    direct(config, path, expansion)
                } else {
                    fs.read(path).ok().and_then(|text| {
                        let run = LintPipeline::new(config, fs).with_preprocess(preprocess);
                        let handle = run.with_file(path);
                        direct(config, path, &handle.expand(&text))
                    })
                }
            })
            .ok()
            .flatten()
        };
        let index = self.effect_index(&index);
        // Breadth-first, one frontier at a time: the files of a frontier are
        // analysed in parallel, then taken in discovery order, so node ids do
        // not depend on the thread count.
        let mut by_path = HashMap::new();
        let mut files = Vec::new();
        let mut programs = HashMap::new();
        let mut pending = paths;
        while !pending.is_empty() {
            let mut frontier = Vec::new();
            for path in pending.drain(..) {
                let path = key(&path);
                if !by_path.contains_key(&path) {
                    by_path.insert(path.clone(), None);
                    frontier.push(path);
                }
            }
            let loaded = map_in_order(threads, &frontier, &load);
            let mut names = Vec::new();
            for item in loaded.iter().flatten() {
                for name in item
                    .effects
                    .units()
                    .iter()
                    .flat_map(|u| &u.calls)
                    .filter_map(program_of)
                {
                    if !programs.contains_key(name) {
                        programs.insert(name.clone(), None);
                        names.push(name.clone());
                    }
                }
            }
            let found = map_in_order(threads, &names, &|name: &String| {
                LintPipeline::effect_program_path(index, name).map(|p| key(&p))
            });
            for (name, path) in names.into_iter().zip(found) {
                programs.insert(name, Some(path));
            }
            for (path, item) in frontier.into_iter().zip(loaded) {
                let Some(item) = item else {
                    continue;
                };
                for name in item
                    .effects
                    .units()
                    .iter()
                    .flat_map(|u| &u.calls)
                    .filter_map(program_of)
                {
                    if let Some(Ok(p)) = &programs[name] {
                        pending.push(p.clone());
                    }
                }
                by_path.insert(path, Some(files.len()));
                files.push(item);
            }
        }
        let mut bases = Vec::new();
        let mut size = 0u32;
        for f in &files {
            bases.push(size);
            size += f.effects.units().len() as u32;
        }
        let find = |fi: usize, kind: UnitKind, name: &str| {
            files[fi]
                .effects
                .units()
                .iter()
                .find(|u| {
                    u.kind == kind
                        && u.name
                            .as_deref()
                            .is_some_and(|n| n.eq_ignore_ascii_case(name))
                })
                .map(|u| Callee::Node(bases[fi] + u.unit.index() as u32))
                .unwrap_or(Callee::Unknown(UnknownCall::NoRoutine))
        };
        let program = |name: &str| -> Result<usize, UnknownCall> {
            let p = programs
                .get(name)
                .ok_or(UnknownCall::NotFound)?
                .as_ref()
                .ok_or(UnknownCall::NotFound)?
                .as_ref()
                .map_err(|u| *u)?;
            by_path
                .get(p)
                .copied()
                .flatten()
                .ok_or(UnknownCall::Unusable)
        };
        let resolve = |fi, target: &CallTarget| -> Option<Callee> {
            Some(match target {
                CallTarget::Procedure(n) => find(fi, UnitKind::Procedure, n),
                CallTarget::Function(n) => find(fi, UnitKind::Function, n),
                CallTarget::Method(n) => find(fi, UnitKind::Method, n),
                CallTarget::Program(n) => program(n)
                    .map(|i| Callee::Node(bases[i]))
                    .unwrap_or_else(Callee::Unknown),
                CallTarget::InProgram {
                    program: p,
                    procedure,
                } => program(p)
                    .map(|i| find(i, UnitKind::Procedure, procedure))
                    .unwrap_or_else(Callee::Unknown),
                CallTarget::Unknown { reason, .. } => Callee::Unknown(*reason),
                CallTarget::Library(_) => return None,
            })
        };
        let indices: Vec<usize> = (0..files.len()).collect();
        let per_file = map_in_order(threads, &indices, &|&fi: &usize| {
            let f = &files[fi];
            let mut nodes = Vec::with_capacity(f.effects.units().len());
            let mut names = Vec::with_capacity(f.effects.units().len());
            let mut callees = HashMap::new();
            for u in f.effects.units() {
                let mut node = Node::of_unit(&f.effects, u, |c| {
                    if c.kind == oxabl_index::calls::CallKind::Remote {
                        None
                    } else {
                        resolve(fi, &c.target)
                    }
                });
                node.unknown.extend_from_slice(&f.incomplete);
                nodes.push(node);
                names.push(format!(
                    "{}::{}",
                    f.path.display(),
                    u.name.as_deref().unwrap_or(u.kind.as_str())
                ));
                for c in &u.calls {
                    if let Some(Callee::Node(n)) = resolve(fi, &c.target) {
                        callees.insert(c.at.start, n);
                    }
                }
            }
            (nodes, names, callees)
        });
        let mut nodes = Vec::with_capacity(size as usize);
        let mut names = Vec::with_capacity(size as usize);
        let mut callees_by_file = Vec::with_capacity(files.len());
        for (n, s, c) in per_file {
            nodes.extend(n);
            names.extend(s);
            callees_by_file.push(c);
        }
        let solved = Arc::new(summary::summarize(&nodes, &summary::graph_of(&nodes)));
        let names = Arc::new(names);
        let mut out = WorkspaceEffects::default();
        for (fi, (f, callees)) in files.into_iter().zip(callees_by_file).enumerate() {
            let base = bases[fi] as usize;
            let end = base + f.effects.units().len();
            out.files.insert(
                f.path,
                EffectFile {
                    effects: f.effects,
                    lines: f.lines,
                    root_units: f.root_units,
                    root_headers: f.root_headers,
                    unknown_calls: nodes[base..end]
                        .iter()
                        .map(|n| {
                            n.calls
                                .iter()
                                .filter(|(c, _)| matches!(c, Callee::Unknown(_)))
                                .count()
                        })
                        .collect(),
                    summary_range: base..end,
                    callees,
                    solved: Arc::clone(&solved),
                    names: Arc::clone(&names),
                    fingerprint: f.fingerprint,
                },
            );
        }
        out
    }
}

fn program_of(c: &oxabl_effects::UnitCall) -> Option<&String> {
    match &c.target {
        CallTarget::Program(p) => Some(p),
        CallTarget::InProgram { program, .. } => Some(program),
        _ => None,
    }
}

/// `f` over `items` on up to `threads` threads, the results in item order.
/// One thread or one item runs on the calling thread.
fn map_in_order<T: Sync, R: Send>(
    threads: usize,
    items: &[T],
    f: &(impl Fn(&T) -> R + Sync),
) -> Vec<R> {
    let threads = threads.min(items.len());
    if threads <= 1 {
        return items.iter().map(f).collect();
    }
    let next = std::sync::atomic::AtomicUsize::new(0);
    let mut done: Vec<(usize, R)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                let next = &next;
                scope.spawn(move || {
                    let mut out = Vec::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(item) = items.get(i) else { break };
                        out.push((i, f(item)));
                    }
                    out
                })
            })
            .collect();
        workers
            .into_iter()
            .flat_map(|w| w.join().unwrap_or_else(|e| std::panic::resume_unwind(e)))
            .collect()
    });
    done.sort_unstable_by_key(|(i, _)| *i);
    done.into_iter().map(|(_, r)| r).collect()
}
