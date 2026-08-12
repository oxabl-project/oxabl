//! The typed dependency edge set for one file: every file it depends on, and
//! why.
//!
//! # Why a new type rather than methods on `WorkspaceIndex`
//!
//! [`WorkspaceIndex`](oxabl_semantic::WorkspaceIndex) is consulted *during*
//! resolve, one name at a time, on behalf of one file. "Who depends on me" is a
//! workspace-level question asked *after* the facts exist, and it reads a whole
//! analysis rather than a name. Bolting it onto the resolve-time seam would put a
//! question with a different lifetime and a different shape behind the same trait.
//!
//! # Why the builder takes its inputs rather than fetching them
//!
//! This crate depends on neither the preprocessor nor the analysis crate, and its
//! own declare pass performs no include expansion and runs against an empty
//! schema. So it cannot reach an expansion's include set or a loaded schema on its
//! own — and it must not learn how to, because that dependency direction is what
//! keeps the reverse query out of salsa's way. Everything the builder needs
//! arrives in [`EdgeInputs`]. `oxabl_pipeline` is the first layer where all of it
//! exists at once, which is why the whole-workspace driver lives there.
//!
//! # Coordinates
//!
//! Every span the semantic model carries is in **expanded** (post-preprocessor)
//! coordinates. A client opening an editor needs the dependent file's own bytes,
//! and only the expansion knows the mapping, so the caller supplies it as
//! [`EdgeInputs::resolve_span`]. A span that does not map back — it originates
//! inside an include, not in the dependent file — is reported as absent rather
//! than as a plausible-looking wrong offset.

use std::path::{Path, PathBuf};

use rustc_hash::FxHashSet;

use crate::search::normalize_lexically;
use oxabl_ast::Span;
use oxabl_common::VirtualSpan;
use oxabl_schema::Schema;
use oxabl_semantic::{ClassLookup, IndexName, IndexedFileId, Semantic, UnresolvedReason};

/// Why one file depends on another.
///
/// The six kinds are the vocabulary a consumer groups an impact answer by, so it
/// never has to re-derive the reason from the shape of the answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EdgeKind {
    /// The dependent file writes `{that.i}` itself.
    DirectInclude,
    /// The dependent file pulls the target in through another include.
    TransitiveInclude,
    /// The dependent file reads a schema table.
    SchemaTable,
    /// A class or interface name the dependent file references.
    ClassReference,
    /// A `RUN` target the dependent file names.
    ProgramReference,
    /// The file whose `DEFINE NEW [GLOBAL] SHARED` a local `DEFINE SHARED`
    /// consumes.
    SharedProducer,
}

impl EdgeKind {
    /// Stable snake-case tag, the same vocabulary the envelope's `via` uses.
    pub fn as_str(self) -> &'static str {
        match self {
            EdgeKind::DirectInclude => "direct_include",
            EdgeKind::TransitiveInclude => "transitive_include",
            EdgeKind::SchemaTable => "schema_table",
            EdgeKind::ClassReference => "class",
            EdgeKind::ProgramReference => "program",
            EdgeKind::SharedProducer => "shared_producer",
        }
    }

    /// Whether this kind names an include rather than a resolved name.
    pub fn is_include(self) -> bool {
        matches!(self, EdgeKind::DirectInclude | EdgeKind::TransitiveInclude)
    }
}

/// What an edge points at.
///
/// Three identities rather than one, because the three sources genuinely know
/// different things and flattening them would mean inventing the parts they do not
/// have.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdgeTarget {
    /// An include, by the path the preprocessor resolved it to. The preprocessor
    /// works in paths and mints no index id, so there is none to report.
    ///
    /// Lexically normalised, so one file has one identity here however the
    /// resolver spelled it. The reverse query normalises again on its own side;
    /// emitting the normalised form makes the two agree by construction rather
    /// than by coincidence.
    IncludePath(PathBuf),
    /// A workspace file the index reached, by the id the index minted, plus the
    /// name that reached it. Only the index that minted the id can map it to a
    /// path.
    IndexedFile {
        file: IndexedFileId,
        name: IndexName,
    },
    /// A schema table, by folded name.
    ///
    /// No file and no CRC. A CRC needs a compiler, and `TableId` is a dense index
    /// into one `Schema` arena rather than a workspace-stable identity (KTD14), so
    /// the folded name is the only identity that travels.
    SchemaTable(IndexName),
}

impl EdgeTarget {
    /// A stable string identity, for ordering and for a consumer that groups by
    /// target without matching on the variant.
    pub fn key(&self) -> &str {
        match self {
            EdgeTarget::IncludePath(path) => path.to_str().unwrap_or_default(),
            EdgeTarget::IndexedFile { name, .. } | EdgeTarget::SchemaTable(name) => name.as_str(),
        }
    }
}

/// One reason the dependent file depends on one other thing.
///
/// `PartialEq`/`Eq` for the reason [`FileFacts`](crate::FileFacts) has them: they
/// are the equality an incremental cache backdates on, so an edit that changes no
/// edge costs a re-extraction and nothing past it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyEdge {
    pub kind: EdgeKind,
    pub target: EdgeTarget,
    /// Where the dependent file creates this edge, in its **own** bytes.
    ///
    /// `None` is a real answer, not a gap to paper over: a transitive include is
    /// named in an intermediate file, so the dependent file has no site for it,
    /// and a reference spliced in from an include resolves to no root-file offset.
    /// Inventing one would point a jump-out at unrelated bytes.
    pub span: Option<Span>,
}

/// One reference that named something oxabl could not resolve.
///
/// A first-class member of the edge set rather than a dropped row or a bump to a
/// resolved count. An impact answer that folds these in under-reports the blast
/// radius while looking more confident, which is the worst failure this data has.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedReference {
    /// Which question was asked. The kind is meaningful even though no target
    /// exists: "an include I could not find" and "a class the workspace lacks"
    /// are different gaps.
    pub kind: EdgeKind,
    /// The name as looked up.
    pub name: String,
    pub reason: UnresolvedReason,
    /// Where the dependent file names it, when it names it in its own bytes.
    pub span: Option<Span>,
}

/// Everything one file depends on, and everything it referenced but could not
/// resolve.
///
/// The two are separate collections on purpose. There is no accessor that returns
/// them merged, so a caller cannot accidentally count an unresolved reference as a
/// resolved dependency.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DependencyEdges {
    edges: Vec<DependencyEdge>,
    unresolved: Vec<UnresolvedReference>,
}

impl DependencyEdges {
    /// Every resolved edge, in a deterministic order.
    pub fn edges(&self) -> &[DependencyEdge] {
        &self.edges
    }

    /// Every reference that resolved to nothing, in a deterministic order.
    pub fn unresolved(&self) -> &[UnresolvedReference] {
        &self.unresolved
    }

    /// The resolved edges of one kind.
    pub fn of_kind(&self, kind: EdgeKind) -> impl Iterator<Item = &DependencyEdge> {
        self.edges.iter().filter(move |e| e.kind == kind)
    }

    /// Whether this file depends on nothing and referenced nothing unresolvable.
    pub fn is_empty(&self) -> bool {
        self.edges.is_empty() && self.unresolved.is_empty()
    }
}

/// One include the dependent file names itself, as the caller observed it.
///
/// Mirrors the expansion's own direct-include row without this crate depending on
/// the crate that produces it.
#[derive(Debug, Clone, Copy)]
pub struct DirectIncludeInput<'a> {
    pub path: &'a Path,
    /// The `{...}` site, already in the dependent file's own bytes.
    pub site: Span,
}

/// An include reference the preprocessor resolved to no file.
#[derive(Debug, Clone, Copy)]
pub struct UnresolvedIncludeInput<'a> {
    pub name: &'a str,
    /// The `{...}` site, already in the dependent file's own bytes.
    pub site: Span,
}

/// Everything [`build_edge_set`] needs, all of it from the caller.
///
/// See the module docs for why the builder does not fetch any of it.
pub struct EdgeInputs<'a> {
    /// The analysed file's semantic model.
    pub semantic: &'a Semantic,
    /// The schema the model was resolved under.
    ///
    /// Its revision must match the model's `schema_revision`: a `TableId` is only
    /// meaningful under the `Schema` that minted it, so a mismatch would name the
    /// wrong table. A mismatch yields no schema edges rather than wrong ones.
    pub schema: &'a Schema,
    /// The depth-1 includes, with their sites.
    pub direct_includes: &'a [DirectIncludeInput<'a>],
    /// Every include the expansion read, transitively. Paths that also appear in
    /// `direct_includes` are reported as direct, not twice.
    pub transitive_includes: &'a [PathBuf],
    /// Include references that resolved to no file.
    pub unresolved_includes: &'a [UnresolvedIncludeInput<'a>],
    /// Maps a span in the expanded buffer back to the dependent file's own bytes,
    /// or `None` when it originates in an include.
    pub resolve_span: &'a dyn Fn(VirtualSpan) -> Option<Span>,
}

/// Build the typed edge set for one file.
///
/// Not guarded against panics, for the reason the rest of this crate is not: in
/// this workspace `salsa::Cancelled` travels as a panic payload, so a guard here
/// would turn a cancelled recompute into a silently empty edge set — an impact
/// answer that says "nothing depends on this" because the query was interrupted.
pub fn build_edge_set(inputs: &EdgeInputs<'_>) -> DependencyEdges {
    let mut edges = Vec::new();
    let mut unresolved = Vec::new();

    include_edges(inputs, &mut edges);
    unresolved_include_rows(inputs, &mut unresolved);
    class_edges(inputs, &mut edges, &mut unresolved);
    symbol_linked_edges(inputs, &mut edges);
    schema_edges(inputs, &mut edges);

    // Both collections are built partly from hash maps, so an unstable order would
    // make two edge sets over one unchanged file compare unequal and defeat the
    // backdating these types derive `PartialEq` for.
    //
    // One edge per reason per target, not per mention. A file that names one table
    // in fifty places depends on it once; fifty rows would make the same dependency
    // look fifty times heavier in an impact answer. The sort puts a span-carrying
    // edge ahead of a span-less one and orders by offset, so the survivor is the
    // earliest place the file creates the edge — and is the same one every time.
    edges.sort_by(|a, b| edge_order(a).cmp(&edge_order(b)));
    edges.dedup_by(|a, b| a.kind == b.kind && a.target == b.target);
    unresolved.sort_by(|a, b| unresolved_order(a).cmp(&unresolved_order(b)));
    unresolved.dedup_by(|a, b| a.kind == b.kind && a.name == b.name);

    DependencyEdges { edges, unresolved }
}

fn edge_order(e: &DependencyEdge) -> (EdgeKind, &str, bool, u32, u32) {
    let (start, end) = e.span.map_or((0, 0), |s| (s.start, s.end));
    (e.kind, e.target.key(), e.span.is_none(), start, end)
}

fn unresolved_order(u: &UnresolvedReference) -> (EdgeKind, &str, bool, u32, u32) {
    let (start, end) = u.span.map_or((0, 0), |s| (s.start, s.end));
    (u.kind, u.name.as_str(), u.span.is_none(), start, end)
}

/// Direct and transitive include edges. A path in both lists is direct only —
/// the transitive list is everything the expansion read, direct includes among
/// them, and reporting one path twice would double every include's weight in an
/// impact answer.
fn include_edges(inputs: &EdgeInputs<'_>, out: &mut Vec<DependencyEdge>) {
    // Both sides are normalized, and the direct set is built once rather than
    // rescanned per transitive path. `/proj/./shared.i` and `/proj/shared.i` name
    // one file, and a textual comparison would call them two — emitting the same
    // include as direct *and* transitive, which is the double count the invariant
    // above exists to prevent.
    let direct_paths: FxHashSet<PathBuf> = inputs
        .direct_includes
        .iter()
        .map(|direct| normalize_lexically(direct.path))
        .collect();

    for direct in inputs.direct_includes {
        out.push(DependencyEdge {
            kind: EdgeKind::DirectInclude,
            target: EdgeTarget::IncludePath(normalize_lexically(direct.path)),
            span: Some(direct.site),
        });
    }
    for path in inputs.transitive_includes {
        let normalized = normalize_lexically(path);
        if direct_paths.contains(&normalized) {
            continue;
        }
        out.push(DependencyEdge {
            kind: EdgeKind::TransitiveInclude,
            target: EdgeTarget::IncludePath(normalized),
            // Named in an intermediate file, so this file has no site for it.
            span: None,
        });
    }
}

fn unresolved_include_rows(inputs: &EdgeInputs<'_>, out: &mut Vec<UnresolvedReference>) {
    for include in inputs.unresolved_includes {
        out.push(UnresolvedReference {
            kind: EdgeKind::DirectInclude,
            name: include.name.to_string(),
            // The preprocessor searched the configured paths and no file supplied
            // the name — the same fact about the workspace this reason names for a
            // class, reached by a different question.
            reason: UnresolvedReason::AbsentFromWorkspace,
            span: Some(include.site),
        });
    }
}

fn class_edges(
    inputs: &EdgeInputs<'_>,
    out: &mut Vec<DependencyEdge>,
    unresolved: &mut Vec<UnresolvedReference>,
) {
    for (name, lookup) in inputs.semantic.symbols.class_lookups() {
        let span = class_name_span(inputs, name);
        match lookup {
            ClassLookup::Linked(file) => out.push(DependencyEdge {
                kind: EdgeKind::ClassReference,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(name.as_ref()),
                },
                span,
            }),
            ClassLookup::Absent => unresolved.push(UnresolvedReference {
                kind: EdgeKind::ClassReference,
                name: name.as_ref().to_string(),
                reason: UnresolvedReason::AbsentFromWorkspace,
                span,
            }),
            ClassLookup::Unusable => unresolved.push(UnresolvedReference {
                kind: EdgeKind::ClassReference,
                name: name.as_ref().to_string(),
                reason: UnresolvedReason::PresentButUnusable,
                span,
            }),
            ClassLookup::Unknowable => unresolved.push(UnresolvedReference {
                kind: EdgeKind::ClassReference,
                name: name.as_ref().to_string(),
                reason: UnresolvedReason::Unknowable,
                span,
            }),
        }
    }
}

/// Where the dependent file writes a class name, if it writes it at all.
///
/// A linear scan of the recorded supertype references, which is how the model
/// itself recovers a supertype's identity from a name. `None` for an *ancestor*
/// reached through a chain walk: that name is written in another file's header,
/// and pointing at an offset in this one would name unrelated bytes.
fn class_name_span(
    inputs: &EdgeInputs<'_>,
    folded: &oxabl_lexer::oxabl_atom::OxablAtom,
) -> Option<Span> {
    let virtual_span = inputs.semantic.symbols.iter().find_map(|(id, _)| {
        let supers = inputs.semantic.symbols.supertypes(id)?;
        supers
            .inherits
            .iter()
            .chain(&supers.implements)
            .find(|r| r.name.as_atom() == folded)
            .map(|r| r.name_span)
    })?;
    (inputs.resolve_span)(virtual_span)
}

/// The two edges the model records per symbol: a `RUN` target and the producer of
/// a consumed `SHARED` name.
///
/// Walked over the symbol table because that is the only access the model offers,
/// and it is a handful of symbols per file.
fn symbol_linked_edges(inputs: &EdgeInputs<'_>, out: &mut Vec<DependencyEdge>) {
    let symbols = &inputs.semantic.symbols;
    for (id, symbol) in symbols.iter() {
        let span = (inputs.resolve_span)(symbol.name_span);
        if let Some(file) = symbols.program_file(id) {
            out.push(DependencyEdge {
                kind: EdgeKind::ProgramReference,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(symbol.name.as_ref()),
                },
                span,
            });
        }
        if let Some(file) = symbols.shared_producer(id) {
            out.push(DependencyEdge {
                kind: EdgeKind::SharedProducer,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(symbol.name.as_ref()),
                },
                span,
            });
        }
    }
}

/// Schema edges, keyed by folded table name (KTD14).
///
/// A `TableId` is only meaningful under the `Schema` that minted it, so a revision
/// mismatch yields no schema edges at all. Naming the table a stale id happens to
/// land on would be a wrong edge, and a wrong edge is worse than a missing one.
fn schema_edges(inputs: &EdgeInputs<'_>, out: &mut Vec<DependencyEdge>) {
    if inputs.schema.revision() != inputs.semantic.schema_revision {
        return;
    }
    for (_id, symbol) in inputs.semantic.symbols.iter() {
        let Some(table_id) = symbol.table_id else {
            continue;
        };
        let Some(table) = inputs.schema.get_by_id(table_id) else {
            continue;
        };
        out.push(DependencyEdge {
            kind: EdgeKind::SchemaTable,
            // Already folded by the schema, so two spellings of one table name
            // arrive here as one key and dedup to one edge.
            target: EdgeTarget::SchemaTable(IndexName::new(table.name.as_ref())),
            span: (inputs.resolve_span)(symbol.name_span),
        });
    }
}
