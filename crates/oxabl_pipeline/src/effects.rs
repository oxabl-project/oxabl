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
    /// Per summary node: whether it runs a program on an AppServer, itself or
    /// through what it calls. The summaries leave such a call out.
    remote: Arc<Vec<bool>>,
    /// The includes the file's expansion read.
    includes: Vec<PathBuf>,
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
    /// [`Self::sources`] in the caller's spelling, built on the first
    /// [`Self::reads`].
    read_index: std::sync::OnceLock<std::collections::HashSet<PathBuf>>,
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
    includes: Vec<PathBuf>,
    fingerprint: u64,
}

fn direct(
    config: &PipelineConfig,
    path: &Path,
    expansion: &Expansion,
    index: &BatchIndex<'_>,
) -> Option<Direct> {
    let expanded = expansion.expanded()?;
    let text = expanded.text.as_str();
    let tokens = oxabl_lexer::tokenize(text);
    let program = oxabl_parser::Parser::new(&tokens, text)
        .without_comments()
        .parse_program();
    let mut ctx = AnalysisContext::new(ROOT_FILE_ID, text, &config.schema, &program.pools)
        .with_tokens(&tokens)
        .with_parse_recovered(!program.errors.is_empty())
        .with_elided(expanded.elided_offsets())
        .with_elided_includes(expanded.elided_include_offsets())
        .with_index(index);
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
    if expanded.lost_include_text() {
        incomplete.push(oxabl_effects::EffectUnknown::UnexpandedInclude);
    }
    Some(Direct {
        incomplete,
        includes: expansion.dependency_paths().to_vec(),
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
        let index = self.effect_index(&index);
        let load = |path: &PathBuf| {
            oxabl_common::catch_panic(|| {
                if let Some((_, expansion)) = root.filter(|(r, _)| *path == key(r)) {
                    direct(config, path, expansion, index)
                } else {
                    fs.read(path).ok().and_then(|text| {
                        let run = LintPipeline::new(config, fs).with_preprocess(preprocess);
                        let handle = run.with_file(path);
                        direct(config, path, &handle.expand(&text), index)
                    })
                }
            })
            .ok()
            .flatten()
        };
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
            let mut remote = Vec::with_capacity(f.effects.units().len());
            for u in f.effects.units() {
                remote.push(
                    u.calls
                        .iter()
                        .any(|c| c.kind == oxabl_index::calls::CallKind::Remote),
                );
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
            (nodes, names, callees, remote)
        });
        let mut nodes = Vec::with_capacity(size as usize);
        let mut names = Vec::with_capacity(size as usize);
        let mut callees_by_file = Vec::with_capacity(files.len());
        let mut remote = Vec::with_capacity(size as usize);
        for (n, s, c, r) in per_file {
            nodes.extend(n);
            names.extend(s);
            callees_by_file.push(c);
            remote.extend(r);
        }
        let solved = Arc::new(summary::summarize(&nodes, &summary::graph_of(&nodes)));
        let names = Arc::new(names);
        let remote = Arc::new(reaches(&nodes, remote));
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
                    remote: Arc::clone(&remote),
                    includes: f.includes,
                    fingerprint: f.fingerprint,
                },
            );
        }
        out
    }
}

/// Per node: whether it, or a node it calls, is marked in `marked`.
fn reaches(nodes: &[Node], mut marked: Vec<bool>) -> Vec<bool> {
    let mut callers: Vec<Vec<u32>> = vec![Vec::new(); nodes.len()];
    for (caller, node) in nodes.iter().enumerate() {
        for (callee, _) in &node.calls {
            if let Callee::Node(callee) = callee
                && let Some(list) = callers.get_mut(*callee as usize)
            {
                list.push(caller as u32);
            }
        }
    }
    let mut pending: Vec<u32> = (0..nodes.len() as u32)
        .filter(|n| marked[*n as usize])
        .collect();
    while let Some(node) = pending.pop() {
        for &caller in &callers[node as usize] {
            if !marked[caller as usize] {
                marked[caller as usize] = true;
                pending.push(caller);
            }
        }
    }
    marked
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
                oxabl_common::stack::builder()
                    .spawn_scoped(scope, move || {
                        let mut out = Vec::new();
                        loop {
                            let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some(item) = items.get(i) else { break };
                            out.push((i, f(item)));
                        }
                        out
                    })
                    .expect("failed to spawn an analysis thread")
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

/// A routine whose database effects a hover shows.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Routine {
    /// An internal procedure, function or method of `file`.
    Unit {
        file: PathBuf,
        kind: UnitKind,
        name: String,
        /// Another body than the one the unit declares may run.
        elsewhere: Option<Elsewhere>,
    },
    /// The main block of the program `file`.
    Program(PathBuf),
}

/// Why a call of a routine may run another body than the one oxabl sees.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Elsewhere {
    /// A method that a subclass can override.
    Override,
    /// A method of an interface: the implementing class supplies the body.
    Implementation,
    /// `PROCEDURE p IN SUPER`: a super procedure supplies the body.
    SuperProcedure,
    /// `PROCEDURE p IN h`: the procedure that the handle names supplies it.
    Handle,
}

impl Elsewhere {
    fn reason(self) -> &'static str {
        match self {
            Elsewhere::Override => "an override in a subclass may run instead of this method",
            Elsewhere::Implementation => {
                "the class that implements the interface supplies the body"
            }
            Elsewhere::SuperProcedure => "the body runs in a super procedure",
            Elsewhere::Handle => "the body runs in the procedure that the handle names",
        }
    }
}

impl Routine {
    /// The file that declares the routine.
    pub fn file(&self) -> &Path {
        match self {
            Routine::Unit { file, .. } | Routine::Program(file) => file,
        }
    }
}

/// The most table lines a hover lists; the rest are counted.
const MAX_EFFECT_LINES: usize = 12;

impl WorkspaceEffects {
    /// The database effects of `routine` as Markdown: each table with its
    /// operations and locks, direct effects apart from those through a call,
    /// and why the summary may miss effects. `None` when the summaries do not
    /// hold the routine, or hold more than one routine of its name. A routine
    /// without a body whose body runs [`Elsewhere`] has no known effects.
    pub fn routine_markdown(&self, routine: &Routine) -> Option<String> {
        let (file, kind, name, elsewhere) = match routine {
            Routine::Unit {
                file,
                kind,
                name,
                elsewhere,
            } => (file, *kind, Some(name.as_str()), *elsewhere),
            Routine::Program(file) => (file, UnitKind::Main, None, None),
        };
        let effects = self.file(file)?;
        // A declaration without a body (an interface method) has no unit.
        let bodiless = elsewhere.map(|elsewhere| {
            format!(
                "**Database effects**\n\nNo known database effects.\n\n\
                 **Incomplete:** effects may be missing: {}.",
                elsewhere.reason()
            )
        });
        let mut matching = effects.effects.units().iter().enumerate().filter(|(_, u)| {
            u.kind == kind
                && match name {
                    Some(name) => u
                        .name
                        .as_deref()
                        .is_some_and(|n| n.eq_ignore_ascii_case(name)),
                    None => true,
                }
        });
        let Some((at, _)) = matching.next() else {
            return bodiless;
        };
        if matching.next().is_some() {
            return None;
        }
        Some(effects.unit_markdown(at, file, elsewhere))
    }

    /// Every file the summaries read: each analysed file and its includes.
    pub fn sources(&self) -> impl Iterator<Item = &Path> {
        self.files.iter().flat_map(|(path, file)| {
            std::iter::once(path.as_path()).chain(file.includes.iter().map(PathBuf::as_path))
        })
    }

    /// Whether the summaries read `path`, as `spell` writes paths (for one
    /// client, its canonical form). The index of [`Self::sources`] is built on
    /// the first call and kept, so `spell` must be the same on every call.
    pub fn reads(&self, path: &Path, spell: &dyn Fn(&Path) -> PathBuf) -> bool {
        self.read_index
            .get_or_init(|| self.sources().map(spell).collect())
            .contains(path)
    }
}

impl EffectFile {
    /// The effects of unit `at`. Its direct effects come from its summary; each
    /// routine it calls has its lines from the callee's own summary, so a route
    /// lists all that the callee does, also what the unit does itself.
    fn unit_markdown(&self, at: usize, file: &Path, elsewhere: Option<Elsewhere>) -> String {
        let summary = &self.summaries()[at];
        let own = (self.summary_range.start + at) as u32;
        let mut lines: BTreeMap<(String, Option<String>), Vec<String>> = BTreeMap::new();
        let mut add = |table: &str, via: Option<String>, op: String| {
            let ops = lines.entry((table.to_string(), via)).or_default();
            if !ops.contains(&op) {
                ops.push(op);
            }
        };
        for effect in summary.effects.iter().filter(|e| e.via.is_none()) {
            add(&effect.table, None, op_text(effect));
        }
        let mut callees: Vec<u32> = self.effects.units()[at]
            .calls
            .iter()
            .filter(|call| call.kind != oxabl_index::calls::CallKind::Remote)
            .filter_map(|call| self.callees.get(&call.at.start).copied())
            .filter(|node| *node != own)
            .collect();
        callees.sort_unstable();
        callees.dedup();
        for node in callees {
            let Some(callee) = self.solved.get(node as usize) else {
                continue;
            };
            let via = self.via_label(node, file);
            for effect in &callee.effects {
                add(&effect.table, Some(via.clone()), op_text(effect));
            }
        }
        let mut reasons: Vec<&str> = summary.partial.iter().map(|p| partial_text(*p)).collect();
        if self.remote.get(own as usize).copied().unwrap_or(true) {
            reasons.push("a program run on an AppServer (`RUN ... ON SERVER`)");
        }
        if let Some(elsewhere) = elsewhere {
            reasons.push(elsewhere.reason());
        }
        let mut out = String::from("**Database effects**");
        if lines.is_empty() {
            out.push_str(if reasons.is_empty() {
                "\n\nNo database effects."
            } else {
                "\n\nNo known database effects."
            });
        }
        let total = lines.len();
        // Direct lines sort before routed ones of the same table (`None` first).
        for ((table, via), ops) in lines.into_iter().take(MAX_EFFECT_LINES) {
            let table = match table.strip_prefix('?') {
                Some(name) => format!("`{name}` (not in the schema)"),
                None => format!("`{table}`"),
            };
            out.push_str(&format!("\n- {table}"));
            if let Some(via) = via {
                out.push_str(&format!(" through `{via}`"));
            }
            out.push_str(&format!(": {}", ops.join(", ")));
        }
        if total > MAX_EFFECT_LINES {
            out.push_str(&format!("\n- and {} more", total - MAX_EFFECT_LINES));
        }
        if !reasons.is_empty() {
            out.push_str(&format!(
                "\n\n**Incomplete:** effects may be missing: {}.",
                reasons.join("; ")
            ));
        }
        out
    }

    /// The routine a summary node names: its name in `file`, the program's
    /// file name for a main block, else `name in file`.
    fn via_label(&self, node: u32, file: &Path) -> String {
        let Some((path, unit)) = self.callee_name(node).and_then(|n| n.rsplit_once("::")) else {
            return "a call".to_string();
        };
        let path = Path::new(path);
        let file_name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        if unit == UnitKind::Main.as_str() {
            return file_name;
        }
        if path == key(file) {
            unit.to_string()
        } else {
            format!("{unit} in {file_name}")
        }
    }
}

fn op_text(effect: &summary::SummaryEffect) -> String {
    match effect.op {
        oxabl_effects::Op::Read => format!("read {}", lock_name(effect.lock)),
        op => op.as_str().to_string(),
    }
}

fn lock_name(lock: summary::SummaryLock) -> &'static str {
    use oxabl_scope::LockMode;
    match lock {
        summary::SummaryLock::Mode(LockMode::NoLock) => "NO-LOCK",
        summary::SummaryLock::Mode(LockMode::ShareLock) => "SHARE-LOCK",
        summary::SummaryLock::Mode(LockMode::ExclusiveLock) => "EXCLUSIVE-LOCK",
        summary::SummaryLock::Default => "SHARE-LOCK (default)",
        summary::SummaryLock::None | summary::SummaryLock::Unknown => "(lock unknown)",
    }
}

fn partial_text(partial: summary::Partial) -> &'static str {
    use oxabl_effects::EffectUnknown;
    match partial {
        summary::Partial::Unit(reason) => match reason {
            EffectUnknown::Dynamic => "a dynamic query or buffer",
            EffectUnknown::SkippedStatement => "a statement oxabl does not read names a table",
            EffectUnknown::UnknownTable => "a read or write of a table the schema does not have",
            EffectUnknown::NoSchema => "no schema is loaded",
            EffectUnknown::UnexpandedInclude => "an include was not expanded",
            EffectUnknown::RecoveredParse => "the parse recovered from an error",
            EffectUnknown::TooComplex => "the analysis reached its work limit",
        },
        summary::Partial::Call(reason) => match reason {
            UnknownCall::Dynamic => "a dynamic call (`RUN VALUE`, `DYNAMIC-FUNCTION`, ...)",
            UnknownCall::Handle => "a call through a handle oxabl cannot follow",
            UnknownCall::Super => "a call to a super procedure",
            UnknownCall::Event => "a `PUBLISH`",
            UnknownCall::Method => "a method call on an object or handle",
            UnknownCall::Unexpanded => "a call target the preprocessor did not expand",
            UnknownCall::NotFound => "a program no file on the include path supplies",
            UnknownCall::Unusable => "a program oxabl cannot read",
            UnknownCall::Ambiguous => "a program that several files supply",
            UnknownCall::NoRoutine => "a procedure its program does not declare",
        },
        summary::Partial::Bound => "the call graph or the summary reached a size limit",
    }
}
