//! Run-wide entry-point reachability for the `security` rules.
//!
//! [`LintPipeline::build_security_reach`] reads the entry points of the
//! configuration, expands and parses every file they reach once (through the
//! run's index, as `RUN` resolves), takes each file's
//! [`FileFacts`](oxabl_lint::security::reach::FileFacts), and searches the
//! graph of their routines ([`oxabl_index::reach`]). The answer for one file
//! is a [`FileReach`], which the lint run of that file reads through
//! [`SecurityReach`] when the client supplies the graph
//! ([`LintPipeline::with_security_reach`]). A run that is not given one does
//! not build it for a single file.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

use oxabl_common::{EntryTarget, ReachEntry, ReachPath, ReachStatus, ReachStep, Reachability};
use oxabl_flow::UnitKind;
use oxabl_index::BatchIndex;
use oxabl_index::calls::{CallTarget, Callee, UnknownCall};
use oxabl_index::reach::{Gated, ReachGraph, Seed};
use oxabl_lint::security::reach::{FileFacts, SecurityReach, SiteTarget, UnitFacts};
use oxabl_semantic::{AnalysisContext, ImportContext};

use crate::{Expansion, LintPipeline, ROOT_FILE_ID};

/// The most entries a finding names.
const ENTRY_CAP: usize = 4;
/// The most superclasses followed to find an inherited method.
const MAX_INHERITS: usize = 8;

/// The reachability answers of one file.
#[derive(Debug)]
pub struct FileReach {
    units: HashMap<u32, Reachability>,
    unauthorised: HashMap<u32, ReachPath>,
    unreset: HashMap<(u32, String), ReachPath>,
    /// The answer for a unit no entry reaches; `None` when it cannot be
    /// known.
    outside: Option<Reachability>,
    fingerprint: u64,
}

impl FileReach {
    fn outside(status: Option<Reachability>) -> Self {
        FileReach {
            units: HashMap::new(),
            unauthorised: HashMap::new(),
            unreset: HashMap::new(),
            outside: status,
            fingerprint: 0,
        }
    }

    pub(crate) fn matches(&self, expansion: &Expansion) -> bool {
        expansion
            .expanded()
            .is_some_and(|e| self.fingerprint == crate::effects::fingerprint(e))
    }
}

impl SecurityReach for FileReach {
    fn unit(&self, start: u32) -> Option<&Reachability> {
        self.units.get(&start).or(self.outside.as_ref())
    }

    fn unauthorised(&self, start: u32) -> Option<&ReachPath> {
        self.unauthorised.get(&start)
    }

    fn unreset(&self, start: u32, key: &str) -> Option<&ReachPath> {
        self.unreset.get(&(start, key.to_string()))
    }
}

/// Reachability over the files the entry points reach, computed once for a
/// lint run.
#[derive(Debug)]
pub struct WorkspaceReach {
    files: BTreeMap<PathBuf, FileReach>,
    /// The answer for a file the entry points do not reach.
    elsewhere: FileReach,
    /// Entry points whose target was not found, as labels.
    pub unresolved: Vec<String>,
    /// The files read.
    pub files_read: usize,
}

impl Default for WorkspaceReach {
    fn default() -> Self {
        WorkspaceReach {
            files: BTreeMap::new(),
            elsewhere: FileReach::outside(None),
            unresolved: Vec::new(),
            files_read: 0,
        }
    }
}

impl WorkspaceReach {
    /// The answers for the file at `path`, reached or not.
    pub fn file(&self, path: &Path) -> &FileReach {
        self.files.get(&key(path)).unwrap_or(&self.elsewhere)
    }

    /// Whether the entry points reach the file at `path`.
    pub fn reaches(&self, path: &Path) -> bool {
        self.files.contains_key(&key(path))
    }
}

fn key(path: &Path) -> PathBuf {
    oxabl_index::search::normalize_lexically(
        &std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf()),
    )
}

/// One file of the graph.
struct Node {
    path: PathBuf,
    facts: FileFacts,
    imports: ImportContext,
    /// The 1-based line, in the file's own text, of each call site by its
    /// expanded start.
    lines: HashMap<u32, u32>,
    fingerprint: u64,
}

impl LintPipeline<'_> {
    /// Whether a `security` rule is enabled and an entry-point file is
    /// configured.
    pub fn security_reach_enabled(&self) -> bool {
        let severities = self
            .file()
            .map(|p| self.config().lint_severities.for_path(p))
            .unwrap_or_else(|| self.config().lint_severities.clone());
        if !severities.security().reach.configured {
            return false;
        }
        let registry = oxabl_lint::Registry::builtin_shared();
        registry.rules().any(|r| {
            r.meta().group == oxabl_lint::groups::SECURITY
                && registry.severity_for(r, &severities).is_some()
        })
    }

    /// Build the reachability of the configured entry points, every file read
    /// from disk (the root's `expansion` first, when given).
    pub fn build_security_reach(&self, root: Option<(&Path, &Expansion)>) -> WorkspaceReach {
        let config = self.config();
        let security = config.lint_severities.security().clone();
        let fallback = BatchIndex::new(self.file_system(), &config.include_paths)
            .with_run_extensions(&config.run_extensions)
            .with_preprocessor(&config.preprocessor);
        let index = self.batch_index_or(&fallback);
        let mut out = WorkspaceReach::default();
        if !security.reach.configured {
            return out;
        }

        let mut graph = Graph {
            pipeline: self,
            index,
            root,
            nodes: Vec::new(),
            by_path: HashMap::new(),
            frozen: false,
        };
        // Resolve each entry, reading its file.
        let mut targets: Vec<(u32, Vec<(usize, usize)>)> = Vec::new();
        for (e, entry) in security.reach.entries.iter().enumerate() {
            let units = match &entry.target {
                EntryTarget::Program { program, procedure } => graph
                    .program(program)
                    .map(|f| match procedure {
                        None => vec![(f, 0)],
                        Some(p) => graph.units_named(f, UnitKind::Procedure, p),
                    })
                    .unwrap_or_default(),
                EntryTarget::Class { class, method } => graph
                    .class(&ImportContext::new(), class)
                    .map(|f| match method {
                        Some(m) => graph.method(f, m),
                        None => graph.class_entries(f),
                    })
                    .unwrap_or_default(),
            };
            if units.is_empty() {
                out.unresolved.push(entry.label());
            }
            targets.push((e as u32, units));
        }
        // Read every file the reached ones call.
        let mut cursor = 0;
        while cursor < graph.nodes.len() {
            let calls: Vec<SiteTarget> = graph.nodes[cursor]
                .facts
                .units
                .iter()
                .flat_map(|u| u.calls.iter().map(|c| c.target.clone()))
                .collect();
            let imports = graph.nodes[cursor].imports.clone();
            for class in graph.nodes[cursor].facts.classes.clone() {
                graph.class(&imports, &class);
            }
            for t in calls {
                match t {
                    SiteTarget::Call(CallTarget::Program(p))
                    | SiteTarget::Call(CallTarget::InProgram { program: p, .. }) => {
                        graph.program(&p);
                    }
                    SiteTarget::Object { class, .. } => {
                        if let Some(f) = graph.class(&imports, &class) {
                            graph.parents(f);
                        }
                    }
                    _ => {}
                }
            }
            cursor += 1;
        }
        out.files_read = graph.nodes.len();
        graph.frozen = true;

        // Number the units and add the calls.
        let mut base = Vec::with_capacity(graph.nodes.len());
        let mut count = 0usize;
        for n in &graph.nodes {
            base.push(count as u32);
            count += n.facts.units.len();
        }
        let node_of = |f: usize, u: usize| base[f] + u as u32;
        let mut g = ReachGraph::new(count);
        // (file, unit, site index) per site id.
        let mut sites: Vec<(usize, usize, usize)> = Vec::new();
        for f in 0..graph.nodes.len() {
            for u in 0..graph.nodes[f].facts.units.len() {
                for k in 0..graph.nodes[f].facts.units[u].calls.len() {
                    let target = graph.nodes[f].facts.units[u].calls[k].target.clone();
                    let id = sites.len() as u32;
                    sites.push((f, u, k));
                    for callee in graph.callees(f, &target) {
                        let to = match callee {
                            Ok((tf, tu)) => Callee::Node(node_of(tf, tu)),
                            Err(r) => Callee::Unknown(r),
                        };
                        g.add(node_of(f, u), id, to);
                    }
                }
            }
        }
        let seeds = |gated: bool| -> Vec<Seed> {
            targets
                .iter()
                .flat_map(|(e, units)| {
                    let auth = gated && security.reach.entries[*e as usize].auth;
                    units.iter().map(move |&(f, u)| Seed {
                        node: node_of(f, u),
                        entry: *e,
                        gated: auth,
                    })
                })
                .collect()
        };
        let site = |id: u32| {
            let (f, u, k) = sites[id as usize];
            &graph.nodes[f].facts.units[u].calls[k]
        };
        let reach = g.reach(&seeds(false), ENTRY_CAP);
        let auth = g.gated(&seeds(true), |_, s| site(s).authorised);
        let unknown: Vec<String> = reach
            .unknown()
            .iter()
            .map(|r| r.as_str().to_string())
            .collect();
        // An entry that did not resolve may reach any routine: what no other
        // entry reaches is not known to be unreachable, so it is not labelled.
        let status_elsewhere = if !out.unresolved.is_empty() {
            None
        } else if unknown.is_empty() {
            Some(ReachStatus::Unreachable)
        } else {
            Some(ReachStatus::Unknown)
        };
        let elsewhere = status_elsewhere.map(|s| Reachability::of(s, unknown.clone()));
        out.elsewhere = FileReach::outside(elsewhere.clone());

        // Locate each unit back in its file.
        let mut where_of: Vec<(usize, usize)> = Vec::with_capacity(count);
        for (f, n) in graph.nodes.iter().enumerate() {
            for u in 0..n.facts.units.len() {
                where_of.push((f, u));
            }
        }
        let entry_of = |e: u32| {
            let entry = &security.reach.entries[e as usize];
            ReachEntry {
                label: entry.label(),
                target: entry.target.describe(),
            }
        };
        let to_path = |gated: &Gated, node: u32, state: bool| -> Option<ReachPath> {
            let p = gated.path(node, state)?;
            Some(ReachPath {
                entry: entry_of(p.entry),
                steps: p
                    .calls
                    .iter()
                    .map(|&(from, s)| {
                        let (f, u) = where_of[from as usize];
                        let at = site(s).at.start;
                        ReachStep {
                            routine: graph.routine(f, u),
                            file: Some(graph.nodes[f].path.clone()),
                            line: graph.nodes[f].lines.get(&at).copied(),
                        }
                    })
                    .collect(),
            })
        };

        // Session state that reached code writes and reads.
        let mut declared: Vec<String> = graph
            .nodes
            .iter()
            .flat_map(|n| n.facts.states.iter().map(|s| s.key.clone()))
            .collect();
        declared.sort();
        declared.dedup();
        let reached_units = || {
            (0..count as u32)
                .filter(|&n| reach.is_reached(n))
                .map(|n| (n, where_of[n as usize]))
        };
        let unit_facts = |f: usize, u: usize| -> &UnitFacts { &graph.nodes[f].facts.units[u] };
        let crossing: Vec<String> = declared
            .into_iter()
            .filter(|k| {
                let mut written = false;
                let mut read = false;
                for (_, (f, u)) in reached_units() {
                    let uf = unit_facts(f, u);
                    written |= uf.writes.iter().any(|a| a.key == *k);
                    read |= uf.reads.iter().any(|a| a.key == *k);
                }
                written && read
            })
            .collect();

        let mut files: Vec<FileReach> = graph
            .nodes
            .iter()
            .map(|n| FileReach {
                fingerprint: n.fingerprint,
                ..FileReach::outside(elsewhere.clone())
            })
            .collect();
        for node in 0..count as u32 {
            let (f, u) = where_of[node as usize];
            let start = unit_facts(f, u).span.start;
            let r = if reach.is_reached(node) {
                let set = reach.entries(node);
                Some(Reachability {
                    status: ReachStatus::Reachable,
                    entries: set.ids().iter().map(|&e| entry_of(e)).collect(),
                    more_entries: set.more(),
                    unknown: Vec::new(),
                    path: to_path(&auth, node, false).or_else(|| to_path(&auth, node, true)),
                })
            } else {
                elsewhere.clone()
            };
            if let Some(r) = r {
                files[f].units.insert(start, r);
            }
            if let Some(p) = to_path(&auth, node, false) {
                files[f].unauthorised.insert(start, p);
            }
        }
        for k in &crossing {
            let gated = g.gated(&seeds(false), |_, s| site(s).resets.contains(k));
            for (node, (f, u)) in reached_units() {
                if !unit_facts(f, u).reads.iter().any(|a| a.key == *k) {
                    continue;
                }
                if let Some(p) = to_path(&gated, node, false) {
                    let start = unit_facts(f, u).span.start;
                    files[f].unreset.insert((start, k.clone()), p);
                }
            }
        }
        for (n, fr) in graph.nodes.iter().zip(files) {
            out.files.insert(n.path.clone(), fr);
        }
        out
    }
}

/// The files of the graph while it is read.
struct Graph<'p, 'a> {
    pipeline: &'p LintPipeline<'a>,
    index: &'p BatchIndex<'a>,
    root: Option<(&'p Path, &'p Expansion)>,
    nodes: Vec<Node>,
    by_path: HashMap<PathBuf, Option<usize>>,
    /// The units are numbered: no file is read any more.
    frozen: bool,
}

impl Graph<'_, '_> {
    /// The file a program name resolves to, read.
    fn program(&mut self, name: &str) -> Option<usize> {
        let path = self.pipeline.effect_program_path(name, self.index).ok()?;
        self.read(&path)
    }

    /// The file of the class `raw` names under `imports`, read.
    fn class(&mut self, imports: &ImportContext, raw: &str) -> Option<usize> {
        let d = imports.find_class(self.index, raw)?;
        let path = self.index.indexed_path(d.file)?;
        self.read(&path)
    }

    /// The files of the superclasses of the class file `f`, read.
    fn parents(&mut self, f: usize) -> Vec<usize> {
        let mut out = Vec::new();
        let mut at = f;
        for _ in 0..MAX_INHERITS {
            let Some(parent) = self.nodes[at].facts.inherits.clone() else {
                break;
            };
            let imports = self.nodes[at].imports.clone();
            match self.class(&imports, &parent) {
                Some(p) if !out.contains(&p) && p != f => {
                    out.push(p);
                    at = p;
                }
                _ => break,
            }
        }
        out
    }

    fn read(&mut self, path: &Path) -> Option<usize> {
        let path = key(path);
        if let Some(known) = self.by_path.get(&path) {
            return *known;
        }
        if self.frozen {
            return None;
        }
        self.by_path.insert(path.clone(), None);
        let node = oxabl_common::catch_panic(|| self.facts_of(&path))
            .ok()
            .flatten();
        let at = node.map(|n| {
            self.nodes.push(n);
            self.nodes.len() - 1
        });
        self.by_path.insert(path, at);
        at
    }

    fn facts_of(&self, path: &Path) -> Option<Node> {
        let pipeline = self.pipeline;
        let own;
        let disk = pipeline.file_system().read(path).ok();
        let expansion: &Expansion = match self.root {
            Some((r, e)) if key(r) == path => e,
            _ => {
                own = pipeline.with_file(path).expand(disk.as_deref()?);
                &own
            }
        };
        let expanded = expansion.expanded()?;
        let text: &str = disk.as_deref().unwrap_or(&expanded.text);
        let source = expanded.text.as_str();
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        let config = pipeline.config();
        let severities = config.lint_severities.for_path(path);
        let mut ctx = AnalysisContext::new(ROOT_FILE_ID, source, &config.schema)
            .with_tokens(&tokens)
            .with_lint_severities(severities.clone())
            .with_parse_recovered(!program.errors.is_empty())
            .with_elided(expanded.elided_offsets())
            .with_elided_includes(expanded.elided_include_offsets());
        ctx.schema_loaded = config.schema_loaded;
        let includes: Vec<(u32, PathBuf)> = expanded
            .include_starts()
            .into_iter()
            .map(|(at, p)| (at, p.to_path_buf()))
            .collect();
        let shared = oxabl_lint::RunShared::default().with_includes(&includes);
        let facts = oxabl_lint::security::reach::file_facts(
            &program.statements,
            &ctx,
            &shared,
            severities.security(),
        );
        let line_starts: Vec<usize> = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        let mut lines = HashMap::new();
        for u in &facts.units {
            for c in &u.calls {
                let span = oxabl_ast::Span {
                    start: c.at.start,
                    end: c.at.start.saturating_add(1),
                };
                let root = expanded
                    .resolve_root_span(span)
                    .or_else(|| expanded.resolve_include_site(span));
                if let Some(r) = root {
                    let line = line_starts.partition_point(|&s| s <= r.start as usize);
                    lines.insert(c.at.start, line as u32);
                }
            }
        }
        Some(Node {
            path: path.to_path_buf(),
            imports: ImportContext::from_statements(&program.statements),
            facts,
            lines,
            fingerprint: crate::effects::fingerprint(expanded),
        })
    }

    /// The units of file `f` of this kind and name.
    fn units_named(&self, f: usize, kind: UnitKind, name: &str) -> Vec<(usize, usize)> {
        self.nodes[f]
            .facts
            .units
            .iter()
            .enumerate()
            .filter(|(_, u)| {
                u.kind == kind
                    && u.name
                        .as_deref()
                        .is_some_and(|n| n.eq_ignore_ascii_case(name))
            })
            .map(|(i, _)| (f, i))
            .collect()
    }

    /// The method `name` of the class file `f`, or of the nearest superclass
    /// that has it.
    fn method(&mut self, f: usize, name: &str) -> Vec<(usize, usize)> {
        let own = self.units_named(f, UnitKind::Method, name);
        if !own.is_empty() {
            return own;
        }
        for p in self.parents(f) {
            let found = self.units_named(p, UnitKind::Method, name);
            if !found.is_empty() {
                return found;
            }
        }
        Vec::new()
    }

    /// What an entry on a class runs: its constructors and its methods that
    /// are not `PRIVATE`.
    fn class_entries(&self, f: usize) -> Vec<(usize, usize)> {
        self.nodes[f]
            .facts
            .units
            .iter()
            .enumerate()
            .filter(|(_, u)| {
                matches!(u.kind, UnitKind::Constructor | UnitKind::Method) && !u.private
            })
            .map(|(i, _)| (f, i))
            .collect()
    }

    /// What a call of file `f` calls: units, or Unknown.
    fn callees(
        &mut self,
        f: usize,
        target: &SiteTarget,
    ) -> Vec<Result<(usize, usize), UnknownCall>> {
        let one = |v: Vec<(usize, usize)>| -> Vec<Result<(usize, usize), UnknownCall>> {
            if v.is_empty() {
                vec![Err(UnknownCall::NoRoutine)]
            } else {
                v.into_iter().map(Ok).collect()
            }
        };
        match target {
            SiteTarget::Call(CallTarget::Procedure(n)) => {
                one(self.units_named(f, UnitKind::Procedure, n))
            }
            SiteTarget::Call(CallTarget::Function(n)) => {
                one(self.units_named(f, UnitKind::Function, n))
            }
            SiteTarget::Call(CallTarget::Method(n)) => one(self.method(f, n)),
            SiteTarget::Call(CallTarget::Program(p)) => match self.program(p) {
                Some(t) => vec![Ok((t, 0))],
                None => vec![Err(self.program_reason(p))],
            },
            SiteTarget::Call(CallTarget::InProgram { program, procedure }) => {
                match self.program(program) {
                    Some(t) => one(self.units_named(t, UnitKind::Procedure, procedure)),
                    None => vec![Err(self.program_reason(program))],
                }
            }
            SiteTarget::Call(CallTarget::Library(_)) => Vec::new(),
            SiteTarget::Call(CallTarget::Unknown { reason, .. }) => vec![Err(*reason)],
            SiteTarget::Object { class, method } => {
                let imports = self.nodes[f].imports.clone();
                match self.class(&imports, class) {
                    Some(t) => match method {
                        Some(m) => one(self.method(t, m)),
                        None => self
                            .units_of_kind(t, UnitKind::Constructor)
                            .into_iter()
                            .map(Ok)
                            .collect(),
                    },
                    None if imports.may_be_builtin(class) => Vec::new(),
                    None => vec![Err(UnknownCall::NotFound)],
                }
            }
        }
    }

    fn units_of_kind(&self, f: usize, kind: UnitKind) -> Vec<(usize, usize)> {
        self.nodes[f]
            .facts
            .units
            .iter()
            .enumerate()
            .filter(|(_, u)| u.kind == kind)
            .map(|(i, _)| (f, i))
            .collect()
    }

    fn program_reason(&self, name: &str) -> UnknownCall {
        match oxabl_index::calls::resolve_program(self.index, name) {
            oxabl_index::calls::ProgramAnswer::Unknown(r) => r,
            oxabl_index::calls::ProgramAnswer::Found(_) => UnknownCall::Unusable,
        }
    }

    /// `web/orders.p` or `web/orders.p:get-order`, the path relative to the
    /// first search path that holds it.
    fn routine(&self, f: usize, u: usize) -> String {
        let path = &self.nodes[f].path;
        let shown = self
            .pipeline
            .config()
            .include_paths
            .iter()
            .find_map(|p| path.strip_prefix(key(p)).ok())
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");
        let unit = &self.nodes[f].facts.units[u];
        match (&unit.name, unit.kind) {
            (_, UnitKind::Main) | (None, _) => shown,
            (Some(n), _) => format!("{shown}:{n}"),
        }
    }
}
