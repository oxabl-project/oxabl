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

use crate::{Expansion, LintPipeline, ROOT_FILE_ID};

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
    incomplete: Vec<oxabl_effects::EffectUnknown>,
    fingerprint: u64,
}

fn direct(run: &LintPipeline<'_>, path: &Path, expansion: &Expansion) -> Option<Direct> {
    let expanded = expansion.expanded()?;
    let text = expanded.text.as_str();
    let tokens = oxabl_lexer::tokenize(text);
    let program = oxabl_parser::Parser::new(&tokens, text).parse_program();
    let mut ctx = AnalysisContext::new(ROOT_FILE_ID, text, &run.config().schema)
        .with_tokens(&tokens)
        .with_parse_recovered(!program.errors.is_empty())
        .with_elided(expanded.elided_offsets())
        .with_elided_includes(expanded.elided_include_offsets());
    ctx.schema_loaded = run.config().schema_loaded;
    let shared = oxabl_lint::RunShared::default();
    let effects = shared.effects(&program.statements, &ctx).clone();
    let root_units = effects
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
        self.summaries_from(self.file().map(|root| (root, expansion)), known)
    }

    /// Build the summaries of `known` and the programs they reach, every file
    /// read from disk. A batch run builds this once and hands it to each of its
    /// handles with [`with_effect_summaries`](Self::with_effect_summaries).
    pub fn build_workspace_effect_summaries(&self, known: &[PathBuf]) -> WorkspaceEffects {
        self.summaries_from(None, known)
    }

    fn summaries_from(
        &self,
        root: Option<(&Path, &Expansion)>,
        known: &[PathBuf],
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
        let mut by_path = HashMap::new();
        let mut files = Vec::new();
        let mut programs = HashMap::new();
        let mut cursor = 0;
        while cursor < paths.len() {
            let path = key(&paths[cursor]);
            cursor += 1;
            if by_path.contains_key(&path) {
                continue;
            }
            by_path.insert(path.clone(), None);
            let item = oxabl_common::catch_panic(|| {
                if let Some((_, expansion)) = root.filter(|(r, _)| path == key(r)) {
                    direct(self, &path, expansion)
                } else {
                    self.file_system().read(&path).ok().and_then(|text| {
                        let handle = self.with_file(&path);
                        direct(&handle, &path, &handle.expand(&text))
                    })
                }
            })
            .ok()
            .flatten();
            let Some(item) = item else {
                continue;
            };
            for u in item.effects.units() {
                for c in &u.calls {
                    let name = match &c.target {
                        CallTarget::Program(p) => p,
                        CallTarget::InProgram { program, .. } => program,
                        _ => continue,
                    };
                    programs
                        .entry(name.clone())
                        .or_insert_with(|| self.effect_program_path(name, &index).map(|p| key(&p)));
                    if let Ok(p) = &programs[name] {
                        paths.push(p.clone());
                    }
                }
            }
            by_path.insert(path, Some(files.len()));
            files.push(item);
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
        let nodes: Vec<_> = files
            .iter()
            .enumerate()
            .flat_map(|(fi, f)| {
                let resolve = &resolve;
                f.effects.units().iter().map(move |u| {
                    let mut node = Node::of_unit(&f.effects, u, |c| {
                        if c.kind == oxabl_index::calls::CallKind::Remote {
                            None
                        } else {
                            resolve(fi, &c.target)
                        }
                    });
                    node.unknown.extend_from_slice(&f.incomplete);
                    node
                })
            })
            .collect();
        let solved = Arc::new(summary::summarize(&nodes, &summary::graph_of(&nodes)));
        let names = Arc::new(
            files
                .iter()
                .flat_map(|f| {
                    f.effects.units().iter().map(|u| {
                        format!(
                            "{}::{}",
                            f.path.display(),
                            u.name.as_deref().unwrap_or(u.kind.as_str())
                        )
                    })
                })
                .collect(),
        );
        let mut callees_by_file = Vec::with_capacity(files.len());
        for (fi, f) in files.iter().enumerate() {
            let mut callees = HashMap::new();
            for u in f.effects.units() {
                for c in &u.calls {
                    if let Some(Callee::Node(n)) = resolve(fi, &c.target) {
                        callees.insert(c.at.start, n);
                    }
                }
            }
            callees_by_file.push(callees);
        }
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
