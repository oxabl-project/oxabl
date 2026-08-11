//! The workspace dependency graph, inverted: who depends on this, and what has
//! to rebuild when it changes.
//!
//! # Why here
//!
//! [`build_edge_set`] answers for one file, and it needs three things at once —
//! the semantic model, the real schema, and the expansion's include sets. This
//! layer is the only one where all three exist: `oxabl_index` sits beneath the
//! preprocessor and the analysis crate, and the language server's cache sits above
//! and must not own a workspace walk. Memoization is deliberately *not* here
//! either. This crate must never depend on salsa (KTD6/KTD11), so the graph is
//! built once per pass and a client that wants it kept warm holds it.
//!
//! # The pass is sequential
//!
//! One file at a time, in the order the caller supplied. There is no parallelism
//! anywhere in oxabl, and a salsa database is `Send` but not `Sync`, so
//! `WorkspaceIndex` carries no `Sync` bound to parallelise behind. Measure the
//! cost; do not add threads here.
//!
//! # Failure is per file
//!
//! A file that panics the analysis, fails to read, or fails preprocessing
//! contributes no edges and is recorded in [`ReverseGraph::unanalysed`]. It does
//! not abort the pass and it is never silently counted as "depends on nothing" —
//! that distinction is the difference between an honest impact answer and one that
//! under-reports.
//!
//! The same rule covers a narrower third failure. An edge can resolve and still be
//! unnameable here, when its target is an index file id this pass cannot map back
//! to a path — the id belongs to an index it does not own. Those are recorded in
//! [`ReverseGraph::unnameable`] and counted in the ratios, rather than dropped.
//! They are a different fact from an unresolved reference and are never merged
//! with one.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use oxabl_analyze::{
    DependencyEdgeRow, DependencySection, LookupSpanRow, UnresolvedEdgeRow, unresolved_reason_str,
};
use oxabl_ast::Span;
use oxabl_index::{
    DependencyEdges, EdgeKind, EdgeTarget, UnresolvedReference, search::normalize_lexically,
};

use crate::LintPipeline;

/// Convert one file's edge set into the envelope's `dependencies` section.
///
/// The conversion lives here because this is the only crate that depends on both
/// sides: `oxabl_analyze` owns the document and must not depend on `oxabl_index`,
/// and `oxabl_index` sits beneath the analysis crate. So the derivation stays in
/// one place and the document's producer only serialises what it is handed.
pub fn dependency_section(edges: &DependencyEdges, index_revision: u32) -> DependencySection {
    DependencySection {
        index_revision,
        edges: edges
            .edges()
            .iter()
            .map(|edge| DependencyEdgeRow {
                via: edge.kind.as_str().to_string(),
                target: edge.target.key().to_string(),
                file: match &edge.target {
                    EdgeTarget::IndexedFile { file, .. } => Some(file.raw()),
                    // An include mints no index id, and a table has no file.
                    EdgeTarget::IncludePath(_) | EdgeTarget::SchemaTable(_) => None,
                },
                span: edge.span.map(|span| LookupSpanRow {
                    start: span.start,
                    end: span.end,
                }),
            })
            .collect(),
        unresolved: edges
            .unresolved()
            .iter()
            .map(|row| UnresolvedEdgeRow {
                via: row.kind.as_str().to_string(),
                name: row.name.clone(),
                reason: unresolved_reason_str(row.reason).to_string(),
                span: row.span.map(|span| LookupSpanRow {
                    start: span.start,
                    end: span.end,
                }),
            })
            .collect(),
    }
}

/// What an impact query is asked about.
///
/// A schema table is a first-class subject rather than a file, because a table is
/// exactly the kind of change whose blast radius nobody can see today, and it has
/// no file in the workspace to stand in for it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subject {
    /// A workspace file, by path. Normalised on construction, so two spellings of
    /// one file are one subject.
    File(PathBuf),
    /// A schema table, by folded name (KTD14).
    Table(String),
}

impl Subject {
    /// A file subject, path normalised.
    pub fn file(path: impl AsRef<Path>) -> Self {
        Subject::File(normalize_lexically(path.as_ref()))
    }

    /// A table subject, name folded.
    pub fn table(name: &str) -> Self {
        Subject::Table(name.to_ascii_lowercase())
    }
}

/// One file whose compilation can change when the subject changes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependent {
    /// The dependent file.
    pub file: PathBuf,
    /// Why it depends on the subject.
    pub kind: EdgeKind,
    /// Where in `file` the dependency is written, when the file writes it in its
    /// own bytes. `None` for a transitive include and for a reference spliced in
    /// from another file — see [`oxabl_index::DependencyEdge::span`].
    pub span: Option<Span>,
}

/// A reference a file could not resolve, and the file that wrote it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedRow {
    /// The file that wrote the reference.
    pub file: PathBuf,
    pub reference: UnresolvedReference,
}

/// An edge whose target this pass could not name.
///
/// Distinct from an unresolved reference, and never folded into one. An
/// unresolved reference is a name the *workspace* failed to supply. An unnameable
/// edge resolved perfectly well — the workspace supplied it — and this pass simply
/// cannot map the index's own file id back to a path, because the id was minted by
/// an index it cannot ask. Reporting one as the other would claim a workspace gap
/// that does not exist.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnnameableEdge {
    /// The file that writes the edge.
    pub file: PathBuf,
    pub kind: EdgeKind,
    /// The target's key, which is all the pass could recover of its identity.
    pub target: String,
}

/// A file the pass could not analyse, and why.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unanalysed {
    pub file: PathBuf,
    /// A short reason, for reporting rather than matching.
    pub reason: String,
}

/// The answer to "who depends on this".
///
/// The two collections are separate for the reason the edge set keeps them
/// separate: an unresolved reference must never be counted as a dependent.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Dependents {
    dependents: Vec<Dependent>,
    unresolved: Vec<UnresolvedRow>,
}

impl Dependents {
    /// Every dependent, ordered by file then by kind. Each file appears once per
    /// kind that links it, so a file reachable two ways is visible as both.
    pub fn all(&self) -> &[Dependent] {
        &self.dependents
    }

    /// The dependents linked by one kind — the grouping an impact answer renders.
    pub fn of_kind(&self, kind: EdgeKind) -> impl Iterator<Item = &Dependent> {
        self.dependents.iter().filter(move |d| d.kind == kind)
    }

    /// Every edge kind present, in enum order, so a consumer can render one group
    /// per cause without knowing the vocabulary in advance.
    pub fn kinds(&self) -> Vec<EdgeKind> {
        let mut kinds: Vec<EdgeKind> = self.dependents.iter().map(|d| d.kind).collect();
        kinds.sort_unstable();
        kinds.dedup();
        kinds
    }

    /// Distinct dependent files, each once however many kinds link it.
    pub fn files(&self) -> Vec<&Path> {
        let mut files: Vec<&Path> = self.dependents.iter().map(|d| d.file.as_path()).collect();
        files.sort_unstable();
        files.dedup();
        files
    }

    /// References that named this subject and did not resolve, so a consumer can
    /// say the dependent set may be short rather than presenting it as complete.
    pub fn unresolved(&self) -> &[UnresolvedRow] {
        &self.unresolved
    }

    /// Whether nothing depends on the subject. A real answer, not an error.
    pub fn is_empty(&self) -> bool {
        self.dependents.is_empty() && self.unresolved.is_empty()
    }
}

/// Which unresolved references one candidate spelling of a subject can match.
///
/// An unresolved reference has no target, so the only tie back to a subject is the
/// name it wrote. Name equality alone is too weak: a file name and a bare stem
/// differ only by an extension, and the two are written by different kinds of
/// reference. Pairing each candidate with the shape that produced it keeps a class
/// reference from claiming an include, and the reverse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum NameShape {
    /// A file name carrying its extension — how the preprocessor spells an
    /// include, and how nothing else spells anything.
    Include,
    /// A bare stem or a dotted path — how a class or a `RUN` target is written.
    Reference,
    /// A folded table name, which no other vocabulary collides with.
    Table,
}

impl NameShape {
    /// Whether a reference of `kind` could have written this spelling.
    fn admits(self, kind: EdgeKind) -> bool {
        match self {
            NameShape::Include => kind.is_include(),
            NameShape::Reference => !kind.is_include(),
            NameShape::Table => true,
        }
    }
}

/// The whole workspace's edges, inverted.
pub struct ReverseGraph {
    /// Subject → the files that depend on it.
    dependents: HashMap<Subject, Vec<Dependent>>,
    /// Every unresolved reference in the workspace, keyed by folded name, so a
    /// query can find the references that *should* have reached its subject.
    unresolved_by_name: HashMap<String, Vec<UnresolvedRow>>,
    /// Files the pass could not analyse at all.
    unanalysed: Vec<Unanalysed>,
    /// Edges whose target this pass could not name. Empty for a pass over its own
    /// batch index, which is every pass today.
    unnameable: Vec<UnnameableEdge>,
    /// Normalised paths of every file the pass covered.
    files: Vec<PathBuf>,
    /// Folded stems that two or more covered files share, computed once per pass.
    ///
    /// A bare stem is how ABL spells an unqualified `RUN` target or class
    /// reference, so it has to be a candidate. It stops being usable evidence the
    /// moment two files answer to it: the reference genuinely may name either, and
    /// attributing it to both invents a dependent for one of them.
    ambiguous_stems: HashSet<String>,
    /// Total resolved edges, for the ratio below.
    edge_count: usize,
    /// Total unresolved references.
    unresolved_count: usize,
    /// The run's configured include roots, for deriving the dotted name a class or
    /// `RUN` target would have spelled a file under.
    ///
    /// Normalised here even though the configuration deliberately does not
    /// normalise them. Include *resolution* joins a root to a name and asks the
    /// filesystem, so it must keep the literal spelling — collapsing `..` across a
    /// symlink would name a different directory. [`Self::candidate_names`] does the
    /// opposite: pure prefix arithmetic against subjects that are already
    /// normalised, where an unnormalised root simply never matches.
    include_roots: Vec<PathBuf>,
}

impl ReverseGraph {
    /// Build the graph by analysing every file in `files` through `pipeline`.
    ///
    /// `pipeline` must be the **run** handle: a per-file handle is derived inside,
    /// so the whole pass shares one index and one memo. That sharing is what makes
    /// a base include read once instead of once per dependent.
    ///
    /// The caller supplies the file list. Discovery already exists in
    /// `oxabl_workspace`, and a graph that walked the filesystem itself could not
    /// be asked about a subset.
    pub fn build(pipeline: &LintPipeline<'_>, files: &[PathBuf]) -> Self {
        let mut graph = ReverseGraph {
            dependents: HashMap::new(),
            unresolved_by_name: HashMap::new(),
            unanalysed: Vec::new(),
            unnameable: Vec::new(),
            files: files.iter().map(|p| normalize_lexically(p)).collect(),
            ambiguous_stems: HashSet::new(),
            edge_count: 0,
            unresolved_count: 0,
            include_roots: pipeline
                .config()
                .include_paths
                .iter()
                .map(|root| normalize_lexically(root))
                .collect(),
        };

        for path in files {
            let normalised = normalize_lexically(path);
            let source = match pipeline.file_system().read(path) {
                Ok(source) => source,
                Err(error) => {
                    graph.unanalysed.push(Unanalysed {
                        file: normalised,
                        reason: format!("unreadable: {error}"),
                    });
                    continue;
                }
            };

            let file_pipeline = pipeline.with_file(path.clone());
            let edges = match file_pipeline.edge_set(&source) {
                Ok(edges) => edges,
                Err(reason) => {
                    graph.unanalysed.push(Unanalysed {
                        file: normalised,
                        reason,
                    });
                    continue;
                }
            };
            graph.absorb(&normalised, &edges, pipeline);
        }

        graph.sort();
        graph
    }

    /// Invert one file's edge set into the graph.
    fn absorb(&mut self, dependent: &Path, edges: &DependencyEdges, pipeline: &LintPipeline<'_>) {
        for edge in edges.edges() {
            let Some(subject) = self.subject_of(&edge.target, pipeline) else {
                // Recorded, not dropped. The pass promises never to present a
                // failure as "depends on nothing", and an edge that vanished here
                // would do exactly that while moving no number a reader could see.
                self.unnameable.push(UnnameableEdge {
                    file: dependent.to_path_buf(),
                    kind: edge.kind,
                    target: edge.target.key().to_string(),
                });
                continue;
            };
            self.edge_count += 1;
            self.dependents.entry(subject).or_default().push(Dependent {
                file: dependent.to_path_buf(),
                kind: edge.kind,
                span: edge.span,
            });
        }
        for reference in edges.unresolved() {
            self.unresolved_count += 1;
            self.unresolved_by_name
                .entry(reference.name.to_ascii_lowercase())
                .or_default()
                .push(UnresolvedRow {
                    file: dependent.to_path_buf(),
                    reference: reference.clone(),
                });
        }
    }

    /// The subject an edge target names, or `None` when the target cannot be tied
    /// to one.
    ///
    /// An `IndexedFileId` is meaningful only inside the index that minted it, so a
    /// run answering from a foreign index (the language server's cache) yields no
    /// path here. `None` drops the edge rather than guessing at a file, which is
    /// why a whole-workspace pass owns its own batch index.
    fn subject_of(&self, target: &EdgeTarget, pipeline: &LintPipeline<'_>) -> Option<Subject> {
        match target {
            EdgeTarget::IncludePath(path) => Some(Subject::file(path)),
            EdgeTarget::IndexedFile { file, .. } => pipeline.indexed_path(*file).map(Subject::file),
            EdgeTarget::SchemaTable(name) => Some(Subject::table(name.as_str())),
        }
    }

    /// Order every bucket, so two builds over one unchanged workspace answer
    /// identically and a rendered answer does not reshuffle between queries.
    fn sort(&mut self) {
        for rows in self.dependents.values_mut() {
            rows.sort_by(|a, b| {
                (a.file.as_path(), a.kind, a.span.map(|s| s.start)).cmp(&(
                    b.file.as_path(),
                    b.kind,
                    b.span.map(|s| s.start),
                ))
            });
            rows.dedup_by(|a, b| a.file == b.file && a.kind == b.kind);
        }
        for rows in self.unresolved_by_name.values_mut() {
            rows.sort_by(|a, b| {
                (a.file.as_path(), a.reference.name.as_str())
                    .cmp(&(b.file.as_path(), b.reference.name.as_str()))
            });
            rows.dedup();
        }
        self.unanalysed.sort();
        self.unanalysed.dedup();
        self.unnameable.sort();
        self.unnameable.dedup();
        self.files.sort();
        self.files.dedup();

        // Once per pass, not once per query: a query only asks whether one stem is
        // in the set.
        let mut seen: HashSet<String> = HashSet::new();
        for path in &self.files {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                let folded = stem.to_ascii_lowercase();
                if !seen.insert(folded.clone()) {
                    self.ambiguous_stems.insert(folded);
                }
            }
        }
    }

    /// Every file whose compilation can change when `subject` changes (R1).
    ///
    /// An empty answer means nothing depends on it. That is a fact, not a failure —
    /// so this returns a [`Dependents`] rather than an `Option` or a `Result`.
    pub fn dependents(&self, subject: &Subject) -> Dependents {
        Dependents {
            dependents: self.dependents.get(subject).cloned().unwrap_or_default(),
            unresolved: self.unresolved_naming(subject),
        }
    }

    /// The unresolved references that named this subject.
    ///
    /// A reference that failed to resolve has no target, so the only way to tie one
    /// to a subject is by the name it wrote. For a file that means the name it is
    /// reachable under: its file name, its stem, and the dotted form a class or
    /// `RUN` target would spell.
    fn unresolved_naming(&self, subject: &Subject) -> Vec<UnresolvedRow> {
        let mut rows = Vec::new();
        for (candidate, shape) in self.candidate_names(subject) {
            let Some(found) = self.unresolved_by_name.get(&candidate) else {
                continue;
            };
            // A spelling only counts against the kind of reference that could have
            // written it. An include is spelled with its extension by the
            // preprocessor; a class or `RUN` target never is. Matching on the
            // string alone lets `thing.cls` claim an unresolved include and
            // `thing` claim one that never named this file at all.
            rows.extend(
                found
                    .iter()
                    .filter(|row| shape.admits(row.reference.kind))
                    .cloned(),
            );
        }
        rows.sort_by(|a, b| {
            (a.file.as_path(), a.reference.name.as_str())
                .cmp(&(b.file.as_path(), b.reference.name.as_str()))
        });
        rows.dedup();
        rows
    }

    /// Every folded name a subject could have been referenced by, each paired with
    /// the kind of reference that spelling could have come from.
    fn candidate_names(&self, subject: &Subject) -> Vec<(String, NameShape)> {
        match subject {
            Subject::Table(name) => vec![(name.clone(), NameShape::Table)],
            Subject::File(path) => {
                let mut names = Vec::new();
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    names.push((name.to_ascii_lowercase(), NameShape::Include));
                }
                if let Some(stem) = path.file_stem().and_then(|n| n.to_str()) {
                    let folded = stem.to_ascii_lowercase();
                    // Dropped when two covered files share it — see
                    // `ambiguous_stems`.
                    if !self.ambiguous_stems.contains(&folded) {
                        names.push((folded, NameShape::Reference));
                    }
                }
                // The dotted spelling a qualified class or `RUN` target uses, for
                // each configured root this file sits under.
                for root in &self.include_roots {
                    if let Ok(relative) = path.strip_prefix(root) {
                        let dotted: Vec<String> = relative
                            .components()
                            .map(|c| c.as_os_str().to_string_lossy().to_ascii_lowercase())
                            .collect();
                        if let Some((last, head)) = dotted.split_last() {
                            let stem = last.rsplit_once('.').map_or(last.as_str(), |(s, _)| s);
                            let mut joined = head.to_vec();
                            joined.push(stem.to_string());
                            names.push((joined.join("."), NameShape::Reference));
                        }
                    }
                }
                names.sort();
                names.dedup();
                names
            }
        }
    }

    /// Everything that must rebuild when `subject` changes: the transitive closure
    /// of dependents (R4).
    ///
    /// Reported separately from [`dependents`](Self::dependents), because "files
    /// that mention this" and "files a compiler has to touch again" are different
    /// numbers and conflating them is how an impact answer becomes untrustworthy.
    ///
    /// A file subject is in its own rebuild set: changing a file rebuilds it. A
    /// table subject is not, having no file. Cycles terminate and each file appears
    /// once.
    pub fn rebuild_set(&self, subject: &Subject) -> Vec<PathBuf> {
        let mut seen: HashSet<PathBuf> = HashSet::new();
        let mut queue: VecDeque<Subject> = VecDeque::new();
        let mut visited: HashSet<Subject> = HashSet::new();

        if let Subject::File(path) = subject {
            seen.insert(path.clone());
        }
        queue.push_back(subject.clone());
        visited.insert(subject.clone());

        while let Some(current) = queue.pop_front() {
            let Some(rows) = self.dependents.get(&current) else {
                continue;
            };
            for row in rows {
                if !seen.insert(row.file.clone()) {
                    continue;
                }
                let next = Subject::File(row.file.clone());
                if visited.insert(next.clone()) {
                    queue.push_back(next);
                }
            }
        }

        let mut out: Vec<PathBuf> = seen.into_iter().collect();
        out.sort();
        out
    }

    /// Files the pass could not analyse. Never folded into "depends on nothing".
    pub fn unanalysed(&self) -> &[Unanalysed] {
        &self.unanalysed
    }

    /// Edges whose target the pass could not name, so a consumer can say the
    /// dependent set is short rather than presenting it as complete.
    ///
    /// Never folded into [`all_unresolved`](Self::all_unresolved): see
    /// [`UnnameableEdge`] for why the two are different facts.
    pub fn unnameable(&self) -> &[UnnameableEdge] {
        &self.unnameable
    }

    /// Every unresolved reference in the workspace, whatever it named.
    pub fn all_unresolved(&self) -> Vec<&UnresolvedRow> {
        let mut rows: Vec<&UnresolvedRow> = self.unresolved_by_name.values().flatten().collect();
        rows.sort_by(|a, b| {
            (a.file.as_path(), a.reference.name.as_str())
                .cmp(&(b.file.as_path(), b.reference.name.as_str()))
        });
        rows
    }

    /// How many files the pass covered.
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Resolved edges across the whole workspace.
    pub fn edge_count(&self) -> usize {
        self.edge_count
    }

    /// Unresolved references across the whole workspace.
    pub fn unresolved_count(&self) -> usize {
        self.unresolved_count
    }

    /// Unresolved references as a share of every reference the pass attempted.
    ///
    /// The number that makes an impact answer's trustworthiness legible instead of
    /// assumed. `0.0` when the pass attempted nothing at all.
    ///
    /// The denominator is everything attempted, which includes the unnameable
    /// edges below. Excluding them would shrink the denominator and flatter the
    /// ratio for a pass that in fact knew less.
    pub fn unresolved_ratio(&self) -> f64 {
        self.share(self.unresolved_count)
    }

    /// Unnameable edges as a share of every reference the pass attempted.
    ///
    /// Separate from [`unresolved_ratio`](Self::unresolved_ratio) over the same
    /// denominator, because the two describe different gaps and one number cannot
    /// say which is which.
    pub fn unnameable_ratio(&self) -> f64 {
        self.share(self.unnameable.len())
    }

    /// `part` as a share of everything the pass tried to turn into an edge.
    /// `0.0` when it attempted nothing at all.
    fn share(&self, part: usize) -> f64 {
        let total = self.edge_count + self.unresolved_count + self.unnameable.len();
        if total == 0 {
            return 0.0;
        }
        part as f64 / total as f64
    }
}
