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
//! This crate does not depend on the analysis crate, expands a file only to read
//! a referenced file's declarations, and runs its declare pass against an empty
//! schema. So it cannot reach the analysed file's include set or a loaded schema
//! on its own — and it must not learn how to, because that dependency direction is what
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
//!
//! # Origin
//!
//! A reference spliced in from an include has no offset of its own here, but it
//! still has an origin a consumer needs: "named in this file's body" and "named
//! in an include it pulls in" are different facts to a build tool that keys a
//! file by its signature. So every edge and every unresolved row carries two
//! independent sites, each in the dependent file's own bytes:
//!
//! - `span` — the first place the file's **own text** names the target. For a
//!   declared type that is the declaring identifier (the variable, parameter,
//!   property or field), because the model keeps no span for the type name;
//! - `include_site` — the first root-level `{...}` through which an
//!   **include** names it, at any include depth.
//!
//! Both are set when the target is named both ways, and neither when the file
//! names it nowhere — an ancestor class the chain walk reached through another
//! class's header. The caller maps include sites with [`EdgeInputs::include_site`].
//!
//! Class edges see every mention. The kinds read off a symbol — a schema table,
//! a `RUN` target, a `SHARED` producer — see only the one site the symbol keeps,
//! so at most one of the two is set on them.
//!
//! ## Which include rows carry which site
//!
//! An include is the file's own when the root writes the `{...}` or a `{&var}`
//! whose value names it. Whether it resolved, could not be read, or resolved to no
//! file, such an include is a [`EdgeKind::DirectInclude`] edge or row whose `span`
//! is that reference, or the substitution for a `{&var}`, and whose `include_site`
//! is `None`.
//!
//! An include reached through a file include is a [`EdgeKind::TransitiveInclude`]
//! edge or row whose `span` is `None` and whose `include_site` is the root-level
//! `{...}` or `{&var}` of the chain that reached it. A transitive edge has no site
//! only for a file the expansion read through no include node.

use std::path::{Path, PathBuf};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::search::normalize_lexically;
use crate::written::ReferenceSyntax;
use oxabl_ast::Span;
use oxabl_common::VirtualSpan;
use oxabl_lexer::oxabl_atom::OxablAtom;
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
    /// The root-level `{...}` site, in this file's own bytes, through which an
    /// include names the target — the same span as that include's
    /// [`EdgeKind::DirectInclude`] edge. `None` when no include names it. See the
    /// module docs on origin.
    pub include_site: Option<Span>,
}

/// One reference that named something oxabl could not resolve.
///
/// A first-class member of the edge set rather than a dropped row or a bump to a
/// resolved count. An impact answer that folds these in under-reports the blast
/// radius while looking more confident, which is the worst failure this data has.
///
/// # What a consumer does with a `RUN` row
///
/// A row of kind [`EdgeKind::ProgramReference`] is a literal or computed `RUN`
/// target that did not link to a usable file. What each [`reason`](Self::reason)
/// tells a build tool:
///
/// - [`AbsentFromWorkspace`](UnresolvedReason::AbsentFromWorkspace): a named file
///   (`RUN x.p`, `RUN a/b.p`) that no configured path supplies. There is no edge.
///   The file lives outside the configured paths or does not exist; a consumer
///   that tracks only workspace files cannot key on it.
/// - [`PresentButUnusable`](UnresolvedReason::PresentButUnusable): the file was
///   found and an edge to it is present. oxabl could not read or parse it, so what
///   *it* depends on is unknown. Key on the edge and treat the file's own
///   dependencies as incomplete.
/// - [`Unknowable`](UnresolvedReason::Unknowable): the target is not a single
///   file. Either it is computed (`RUN VALUE(...)`, named `VALUE(...)` here), or
///   two path entries supply the same name and oxabl declines to pick. There is no
///   edge. A consumer that needs a sound answer widens for this file.
/// - [`External`](UnresolvedReason::External): a named file oxabl could not
///   search for, because no search path is configured or because an extensionless
///   path (`RUN a/b`) may also be satisfied by compiled code. There is no edge.
///   Read it as `AbsentFromWorkspace` with less evidence.
///
/// A bare name with no extension and no path separator (`RUN getdata`) makes no
/// row at all when no file supplies it. It is an internal procedure of a running
/// program, resolved at run time, and it is never a compile input. When a file
/// *does* supply it (`getdata.p`, then `getdata.w`), that file is an ordinary
/// edge. `RUN x IN h` is likewise silent.
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
    /// The root-level `{...}` site through which an include names it, as
    /// [`DependencyEdge::include_site`].
    pub include_site: Option<Span>,
}

/// Why an edge set may be missing dependencies that no unresolved row names.
///
/// Each reason is a place the analysis lost sight of source text. A reference in
/// that text would have produced an edge or a row, and there is nothing left to
/// say it existed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum DegradedReason {
    /// The parser recovered from an error and skipped statements, in the file
    /// itself or in an include.
    ParseRecovered,
    /// An include, at any depth, resolved to no file. Whatever it would have
    /// declared or included is unknown.
    UnresolvedInclude,
    /// An include, at any depth, resolved to a file whose content could not be
    /// read. The file is an edge, but what it includes is unknown.
    UnreadableInclude,
    /// The preprocessor dropped text or decided a branch without the facts to
    /// decide it: an include past the depth limit, an `&IF` that never closes, a
    /// condition on a predefined name that has no value. Whatever the lost text
    /// referenced is unknown.
    PreprocessorLoss,
    /// A statement the parser recognizes but does not model mentions a class or a
    /// member access (`PUT`, `EXPORT`, ...), and no class could be attributed to
    /// it. A reference in it may have produced no edge.
    UnmodelledStatement,
}

impl DegradedReason {
    /// Stable snake-case tag for a wire format.
    pub fn as_str(self) -> &'static str {
        match self {
            DegradedReason::ParseRecovered => "parse_recovered",
            DegradedReason::UnresolvedInclude => "unresolved_include",
            DegradedReason::UnreadableInclude => "unreadable_include",
            DegradedReason::PreprocessorLoss => "preprocessor_loss",
            DegradedReason::UnmodelledStatement => "unmodelled_statement",
        }
    }
}

/// The directive that opens a conditional branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BranchKind {
    If,
    ElseIf,
    Else,
}

/// One conditional branch that encloses a [`ConditionalInclude`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConditionalBranch {
    pub kind: BranchKind,
    /// The condition as written, before any substitution. An `&ELSE` branch
    /// carries the last condition of its chain.
    pub condition: String,
}

/// An include reference written in a branch the preprocessor did not take.
///
/// Not an edge: for the defines this analysis ran under the file does not depend
/// on the target. It is listed because oxabl's `&IF` evaluation can disagree with
/// the AVM's, and a different set of defines takes a different branch. A build
/// tool that keys a cache on the include set widens it with these, and knows from
/// the list that none was taken.
///
/// Never expanded, so there is no resolved path, and what the target includes is
/// unknown.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalInclude {
    /// The include name as written; a `{...}` reference inside it is left as text.
    pub name: String,
    /// The `{...}` site, in the dependent file's own bytes, when the file writes
    /// the reference itself.
    pub span: Option<Span>,
    /// The root-level `{...}` site, in the dependent file's own bytes, of the
    /// include chain that reached a reference written in an include.
    pub include_site: Option<Span>,
    /// The enclosing branches, outermost first. The innermost was not taken.
    pub branches: Vec<ConditionalBranch>,
    /// The name holds a `{&name}` or `{N}` reference, so the file it names depends
    /// on values that were not substituted.
    pub computed_name: bool,
}

/// A class or program name written in a branch the preprocessor did not take.
///
/// Not an edge, for the reason a [`ConditionalInclude`] is not. Found by a token
/// scan of the skipped text ([`written_references`](crate::written_references)),
/// never a parse, and resolved through the index the file was analysed with.
///
/// A name whose [`syntax`](Self::syntax) may name something other than a class or
/// a program file — a bare `AS x`, a static access `x:y`, a `RUN` name with no
/// extension and no path — is listed only when it resolves. Any other name is
/// listed whether it resolves or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalReference {
    /// [`EdgeKind::ClassReference`] or [`EdgeKind::ProgramReference`].
    pub kind: EdgeKind,
    /// How the skipped text names the target.
    pub syntax: ReferenceSyntax,
    /// The name as written.
    pub name: String,
    /// The file the name resolves to, or `None` when it resolves to none.
    pub target: Option<EdgeTarget>,
    /// Where the dependent file writes the name, in its own bytes.
    pub span: Option<Span>,
    /// The root-level `{...}` site, in the dependent file's own bytes, of the
    /// include chain that reached a name written in an include.
    pub include_site: Option<Span>,
    /// The enclosing branches, outermost first. The innermost was not taken.
    pub branches: Vec<ConditionalBranch>,
}

/// What a [`ConditionalInclude`]'s target would read if its branch were taken.
///
/// The target is expanded on its own, with the dependent file's include paths and
/// none of its defines or include arguments, so this is a reading of the target,
/// not of the dependent file. Includes in the target's own not-taken branches are
/// expanded too, as further entries.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct ConditionalIncludeClosure {
    /// The include name as written, as [`ConditionalInclude::name`].
    pub name: String,
    /// The conditional include, as written, whose target names this one in a
    /// not-taken branch. `None` for a name the dependent file's own expansion
    /// skipped.
    pub via: Option<String>,
    /// The file the name resolves to, lexically normalised. `None` when the name is
    /// computed or resolves to no file.
    pub path: Option<PathBuf>,
    /// Every include the target reads at any depth, lexically normalised, sorted,
    /// without `path`.
    pub reaches: Vec<PathBuf>,
    /// Include names in the target's expansion that resolved to no file or could
    /// not be read.
    pub unresolved: Vec<String>,
    /// Whether `reaches` is everything: the name resolved, the target was read and
    /// its expansion lost no text and left no include unresolved.
    pub complete: bool,
    /// The class and program names the target's expansion writes, sorted, each
    /// once. Empty unless the caller supplied an analysis
    /// ([`conditional_include_closure_with`](crate::conditional_include_closure_with)).
    pub references: Vec<ClosureReference>,
}

/// A class or program name a [`ConditionalIncludeClosure`]'s target writes.
///
/// Not an edge, for the reason a [`ConditionalInclude`] is not. The target is
/// analysed on its own, as a fragment, so a name its includer would give another
/// meaning (a buffer, a handle) can be read differently here.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct ClosureReference {
    /// [`EdgeKind::ClassReference`] or [`EdgeKind::ProgramReference`].
    pub kind: EdgeKind,
    /// Whether the name is written in a branch the target's own expansion did not
    /// take. Such a name is found by a token scan, as a [`ConditionalReference`]
    /// is, and listed under the same rule.
    pub conditional: bool,
    /// The name: as written for a conditional one, otherwise as the analysis
    /// looked it up, folded to lower case.
    pub name: String,
    /// The file the name resolves to, or `None` when it resolves to none.
    pub target: Option<EdgeTarget>,
}

impl ClosureReference {
    /// A reference, for the caller that supplies the analysis.
    pub fn new(
        kind: EdgeKind,
        conditional: bool,
        name: String,
        target: Option<EdgeTarget>,
    ) -> Self {
        ClosureReference {
            kind,
            conditional,
            name,
            target,
        }
    }
}

/// Whether an edge set can be trusted to be everything the file depends on.
///
/// `Degraded` does not mean the edges are wrong. It means the absence of an edge
/// proves nothing, so a consumer should widen its invalidation for this file
/// rather than rely on the set.
///
/// Non-exhaustive: a consumer matching it treats an arm it does not know as
/// degraded.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[non_exhaustive]
pub enum Completeness {
    /// Nothing was lost: every reference the file makes is an edge or a row.
    #[default]
    Complete,
    /// Something was lost. `reasons` is sorted, with no repeats, and never empty.
    Degraded { reasons: Vec<DegradedReason> },
}

impl Completeness {
    /// Whether nothing was lost.
    pub fn is_complete(&self) -> bool {
        matches!(self, Completeness::Complete)
    }

    /// The reasons the set is degraded, empty when it is complete.
    pub fn reasons(&self) -> &[DegradedReason] {
        match self {
            Completeness::Complete => &[],
            Completeness::Degraded { reasons } => reasons,
        }
    }

    /// `Complete` for no reasons, otherwise `Degraded` with them sorted and
    /// deduplicated.
    pub fn from_reasons(mut reasons: Vec<DegradedReason>) -> Self {
        reasons.sort();
        reasons.dedup();
        if reasons.is_empty() {
            Completeness::Complete
        } else {
            Completeness::Degraded { reasons }
        }
    }
}

/// Everything one file depends on, and everything it referenced but could not
/// resolve.
///
/// The two are separate collections on purpose. There is no accessor that returns
/// them merged, so a caller cannot accidentally count an unresolved reference as a
/// resolved dependency.
///
/// Include references in branches that were not taken are a third collection,
/// [`Self::conditional_includes`], for the same reason: they are never edges. So
/// are the class and program names those branches write,
/// [`Self::conditional_references`], and what the conditional includes would
/// read, [`Self::conditional_include_closure`].
///
/// [`Self::completeness`] says whether the pair can be trusted. The default is
/// `Complete` along with the empty collections: an edge set built from nothing
/// lost nothing.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DependencyEdges {
    edges: Vec<DependencyEdge>,
    unresolved: Vec<UnresolvedReference>,
    conditional_includes: Vec<ConditionalInclude>,
    conditional_references: Vec<ConditionalReference>,
    conditional_include_closure: Vec<ConditionalIncludeClosure>,
    completeness: Completeness,
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

    /// Every include reference written in a branch that was not taken, in a
    /// deterministic order, each once.
    ///
    /// Never merged into [`Self::edges`]. Empty unless the caller attached the
    /// list with [`Self::with_conditional_includes`].
    pub fn conditional_includes(&self) -> &[ConditionalInclude] {
        &self.conditional_includes
    }

    /// Attach the include references of branches that were not taken. They are
    /// ordered by site and deduplicated, so two sets over one unchanged file
    /// compare equal.
    #[must_use]
    pub fn with_conditional_includes(mut self, mut includes: Vec<ConditionalInclude>) -> Self {
        includes.sort_by(|a, b| conditional_order(a).cmp(&conditional_order(b)));
        includes.dedup();
        self.conditional_includes = includes;
        self
    }

    /// Every class and program name written in a branch that was not taken, in a
    /// deterministic order, each once.
    ///
    /// Never merged into [`Self::edges`]. Empty unless the caller attached the
    /// list with [`Self::with_conditional_references`].
    pub fn conditional_references(&self) -> &[ConditionalReference] {
        &self.conditional_references
    }

    /// Attach the class and program names of branches that were not taken. They
    /// are ordered by site and deduplicated.
    #[must_use]
    pub fn with_conditional_references(
        mut self,
        mut references: Vec<ConditionalReference>,
    ) -> Self {
        references.sort_by(|a, b| reference_order(a).cmp(&reference_order(b)));
        references.dedup();
        self.conditional_references = references;
        self
    }

    /// What each conditional include's target would read, one entry per name.
    ///
    /// Never merged into [`Self::edges`]. Empty unless the caller attached the
    /// list with [`Self::with_conditional_include_closure`]; the pipeline does so
    /// only when asked, since it reads and expands every target.
    pub fn conditional_include_closure(&self) -> &[ConditionalIncludeClosure] {
        &self.conditional_include_closure
    }

    /// Attach the closure of the conditional includes, ordered by name.
    #[must_use]
    pub fn with_conditional_include_closure(
        mut self,
        mut closure: Vec<ConditionalIncludeClosure>,
    ) -> Self {
        closure.sort_by(|a, b| (&a.via, &a.name).cmp(&(&b.via, &b.name)));
        closure.dedup();
        self.conditional_include_closure = closure;
        self
    }

    /// Whether the edges and rows are everything the file depends on.
    pub fn completeness(&self) -> &Completeness {
        &self.completeness
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

/// One include file the dependent file pulls in at any depth, and the site of the
/// chain that reached it.
#[derive(Debug, Clone, Copy)]
pub struct IncludeOriginInput<'a> {
    pub path: &'a Path,
    /// The outermost `{...}` or `{&var}` the dependent file itself writes on the
    /// chain, already in its own bytes.
    pub site: Span,
}

/// An include reference the preprocessor resolved to no file.
#[derive(Debug, Clone, Copy)]
pub struct UnresolvedIncludeInput<'a> {
    pub name: &'a str,
    /// The `{...}` site, already in the dependent file's own bytes. For a
    /// reference a `{&var}` value names, the substitution.
    pub site: Span,
    /// The reference's `{` never closes, so `name` is only a best reading of the
    /// text after it. The row says the reference is unknowable rather than that
    /// the workspace lacks the file.
    pub unterminated: bool,
}

/// An include reference written in an *included* file that resolved to no file.
#[derive(Debug, Clone, Copy)]
pub struct NestedUnresolvedIncludeInput<'a> {
    pub name: &'a str,
    /// The root-level `{...}` site, in the dependent file's own bytes, of the
    /// include chain that reached it, when the chain starts in the dependent
    /// file.
    pub include_site: Option<Span>,
}

/// An include that resolved to a path whose content could not be read.
#[derive(Debug, Clone, Copy)]
pub struct UnreadableIncludeInput<'a> {
    /// The include as written.
    pub name: &'a str,
    /// The path it resolved to.
    pub path: &'a Path,
    /// The `{...}` site in the dependent file's own bytes, or `None` when a
    /// nested include names it.
    pub site: Option<Span>,
    /// For a nested include, the root-level `{...}` site of the chain that
    /// reached it, in the dependent file's own bytes.
    pub include_site: Option<Span>,
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
    /// The root-level site of the chain that reached each include, at any depth.
    /// Gives a transitive include edge its `include_site`.
    pub include_origins: &'a [IncludeOriginInput<'a>],
    /// Include references that resolved to no file.
    pub unresolved_includes: &'a [UnresolvedIncludeInput<'a>],
    /// Include references that resolved to no file, written in an *included* file.
    /// The dependent file has no site for them, only the chain that reached them.
    pub nested_unresolved_includes: &'a [NestedUnresolvedIncludeInput<'a>],
    /// Includes, at any depth, that resolved to a path that could not be read.
    pub unreadable_includes: &'a [UnreadableIncludeInput<'a>],
    /// Whether the parser recovered from an error anywhere in the expansion.
    pub parse_recovered: bool,
    /// Whether the preprocessor dropped text or decided a branch on a guess.
    pub preprocessor_loss: bool,
    /// Maps a span in the expanded buffer back to the dependent file's own bytes,
    /// or `None` when it originates in an include.
    pub resolve_span: &'a dyn Fn(VirtualSpan) -> Option<Span>,
    /// Maps a span in the expanded buffer to the root-level `{...}` site, in the
    /// dependent file's own bytes, of the include it originates in — or `None`
    /// when it is written in the dependent file itself.
    pub include_site: &'a dyn Fn(VirtualSpan) -> Option<Span>,
}

/// Build the typed edge set for one file.
///
/// Not guarded against panics, for the reason the rest of this crate is not: in
/// this workspace `salsa::Cancelled` travels as a panic payload, so a guard here
/// would turn a cancelled recompute into a silently empty edge set — an impact
/// answer that says "nothing depends on this" because the query was interrupted.
///
/// `LintPipeline::edge_set_with` does span this call with
/// [`oxabl_common::catch_panic`], and that is consistent rather than an exception:
/// the guard re-raises any payload it cannot describe, so a cancellation still
/// travels out to the layer that understands it. The rule above is about what a
/// guard may *swallow*, not about which calls one may enclose.
pub fn build_edge_set(inputs: &EdgeInputs<'_>) -> DependencyEdges {
    let mut edges = Vec::new();
    let mut unresolved = Vec::new();

    include_edges(inputs, &mut edges);
    unresolved_include_rows(inputs, &mut edges, &mut unresolved);
    class_edges(inputs, &mut edges, &mut unresolved);
    symbol_linked_edges(inputs, &mut edges);
    unlinked_run_rows(inputs, &mut edges, &mut unresolved);
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
    // The two sites are independent facts, so a dropped duplicate's include site
    // is kept when the survivor has none.
    edges.sort_by(|a, b| edge_order(a).cmp(&edge_order(b)));
    edges.dedup_by(|dup, kept| {
        let same = dup.kind == kept.kind && dup.target == kept.target;
        if same {
            kept.include_site = earliest(kept.include_site, dup.include_site);
        }
        same
    });
    unresolved.sort_by(|a, b| unresolved_order(a).cmp(&unresolved_order(b)));
    unresolved.dedup_by(|dup, kept| {
        let same = dup.kind == kept.kind && dup.name == kept.name;
        if same {
            kept.include_site = earliest(kept.include_site, dup.include_site);
        }
        same
    });

    DependencyEdges {
        edges,
        unresolved,
        conditional_includes: Vec::new(),
        conditional_references: Vec::new(),
        conditional_include_closure: Vec::new(),
        completeness: completeness(inputs),
    }
}

fn completeness(inputs: &EdgeInputs<'_>) -> Completeness {
    let mut reasons = Vec::new();
    if inputs.parse_recovered {
        reasons.push(DegradedReason::ParseRecovered);
    }
    if !inputs.unresolved_includes.is_empty() || !inputs.nested_unresolved_includes.is_empty() {
        reasons.push(DegradedReason::UnresolvedInclude);
    }
    if inputs.preprocessor_loss {
        reasons.push(DegradedReason::PreprocessorLoss);
    }
    if inputs.semantic.symbols.has_unattributed_unmodelled_use() {
        reasons.push(DegradedReason::UnmodelledStatement);
    }
    if !inputs.unreadable_includes.is_empty() {
        reasons.push(DegradedReason::UnreadableInclude);
    }
    Completeness::from_reasons(reasons)
}

type SiteOrder = (bool, u32, u32);

fn site_order(span: Option<Span>) -> SiteOrder {
    span.map_or((true, 0, 0), |s| (false, s.start, s.end))
}

fn edge_order(e: &DependencyEdge) -> (EdgeKind, &str, SiteOrder, SiteOrder) {
    (
        e.kind,
        e.target.key(),
        site_order(e.span),
        site_order(e.include_site),
    )
}

fn unresolved_order(u: &UnresolvedReference) -> (EdgeKind, &str, SiteOrder, SiteOrder) {
    (
        u.kind,
        u.name.as_str(),
        site_order(u.span),
        site_order(u.include_site),
    )
}

fn conditional_order(c: &ConditionalInclude) -> (SiteOrder, SiteOrder, &str, &[ConditionalBranch]) {
    (
        site_order(c.span),
        site_order(c.include_site),
        c.name.as_str(),
        c.branches.as_slice(),
    )
}

fn reference_order(
    c: &ConditionalReference,
) -> (SiteOrder, SiteOrder, EdgeKind, &str, &[ConditionalBranch]) {
    (
        site_order(c.span),
        site_order(c.include_site),
        c.kind,
        c.name.as_str(),
        c.branches.as_slice(),
    )
}

/// The earlier of two optional sites, by offset.
fn earliest(a: Option<Span>, b: Option<Span>) -> Option<Span> {
    match (a, b) {
        (Some(a), Some(b)) => Some(if (b.start, b.end) < (a.start, a.end) {
            b
        } else {
            a
        }),
        (a, b) => a.or(b),
    }
}

/// Both sites of one mention in the expanded buffer: its own offset when the
/// dependent file writes it, and the include site it arrived through otherwise.
fn sites_of(inputs: &EdgeInputs<'_>, span: VirtualSpan) -> (Option<Span>, Option<Span>) {
    match (inputs.resolve_span)(span) {
        Some(own) => (Some(own), None),
        None => (None, (inputs.include_site)(span)),
    }
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
    //
    // An unreadable include the root names is a direct edge of its own, so its
    // path is direct here as well.
    let direct_paths: FxHashSet<PathBuf> = inputs
        .direct_includes
        .iter()
        .map(|direct| direct.path)
        .chain(
            inputs
                .unreadable_includes
                .iter()
                .filter(|unreadable| unreadable.site.is_some())
                .map(|unreadable| unreadable.path),
        )
        .map(normalize_lexically)
        .collect();

    for direct in inputs.direct_includes {
        out.push(DependencyEdge {
            kind: EdgeKind::DirectInclude,
            target: EdgeTarget::IncludePath(normalize_lexically(direct.path)),
            span: Some(direct.site),
            include_site: None,
        });
    }
    let mut origins: FxHashMap<PathBuf, Span> = FxHashMap::default();
    for origin in inputs.include_origins {
        let site = origins
            .entry(normalize_lexically(origin.path))
            .or_insert(origin.site);
        *site = earliest(Some(*site), Some(origin.site)).unwrap_or(*site);
    }
    for path in inputs.transitive_includes {
        let normalized = normalize_lexically(path);
        if direct_paths.contains(&normalized) {
            continue;
        }
        out.push(DependencyEdge {
            kind: EdgeKind::TransitiveInclude,
            // Named in an intermediate file, so this file has no site for it, only
            // the one through which the chain reached it.
            include_site: origins.get(&normalized).copied(),
            target: EdgeTarget::IncludePath(normalized),
            span: None,
        });
    }
}

fn unresolved_include_rows(
    inputs: &EdgeInputs<'_>,
    edges: &mut Vec<DependencyEdge>,
    out: &mut Vec<UnresolvedReference>,
) {
    for include in inputs.unresolved_includes {
        out.push(UnresolvedReference {
            kind: EdgeKind::DirectInclude,
            name: include.name.to_string(),
            // The preprocessor searched the configured paths and no file supplied
            // the name — the same fact about the workspace this reason names for a
            // class, reached by a different question. An unterminated reference
            // names nothing for certain, so nothing is claimed of the workspace.
            reason: if include.unterminated {
                UnresolvedReason::Unknowable
            } else {
                UnresolvedReason::AbsentFromWorkspace
            },
            span: Some(include.site),
            include_site: None,
        });
    }
    for include in inputs.nested_unresolved_includes {
        out.push(UnresolvedReference {
            kind: EdgeKind::TransitiveInclude,
            name: include.name.to_string(),
            reason: UnresolvedReason::AbsentFromWorkspace,
            span: None,
            include_site: include.include_site,
        });
    }
    // The file exists, so it is an edge whatever could be read from it. The row
    // says its own includes are unknown.
    for include in inputs.unreadable_includes {
        let kind = if include.site.is_some() {
            EdgeKind::DirectInclude
        } else {
            EdgeKind::TransitiveInclude
        };
        edges.push(DependencyEdge {
            kind,
            target: EdgeTarget::IncludePath(normalize_lexically(include.path)),
            span: include.site,
            include_site: include.include_site,
        });
        out.push(UnresolvedReference {
            kind,
            name: include.name.to_string(),
            reason: UnresolvedReason::PresentButUnusable,
            span: include.site,
            include_site: include.include_site,
        });
    }
}

fn class_edges(
    inputs: &EdgeInputs<'_>,
    out: &mut Vec<DependencyEdge>,
    unresolved: &mut Vec<UnresolvedReference>,
) {
    let symbols = &inputs.semantic.symbols;
    // One pass over every mention, keeping the earliest site of each kind per
    // name, rather than a scan of the mentions per lookup.
    let mut sites: FxHashMap<&OxablAtom, (Option<Span>, Option<Span>)> = FxHashMap::default();
    for (name, site) in symbols.class_sites() {
        let (own, via) = sites_of(inputs, *site);
        let entry = sites.entry(name).or_default();
        entry.0 = earliest(entry.0, own);
        entry.1 = earliest(entry.1, via);
    }

    for (name, lookup) in symbols.class_lookups() {
        // No mention is an ancestor the chain walk reached through another
        // class's header: this file writes it nowhere.
        let (span, include_site) = sites.get(name).copied().unwrap_or_default();
        let reason = match lookup {
            ClassLookup::Linked(file) => {
                out.push(DependencyEdge {
                    kind: EdgeKind::ClassReference,
                    target: EdgeTarget::IndexedFile {
                        file,
                        name: IndexName::new(name.as_ref()),
                    },
                    span,
                    include_site,
                });
                continue;
            }
            ClassLookup::Absent => UnresolvedReason::AbsentFromWorkspace,
            ClassLookup::Unusable => {
                // The file was located, so it is a dependency whatever oxabl could
                // extract from it. The row records what is missing.
                if let Some(file) = symbols.unusable_class_file(name) {
                    out.push(DependencyEdge {
                        kind: EdgeKind::ClassReference,
                        target: EdgeTarget::IndexedFile {
                            file,
                            name: IndexName::new(name.as_ref()),
                        },
                        span,
                        include_site,
                    });
                }
                UnresolvedReason::PresentButUnusable
            }
            ClassLookup::Unknowable => UnresolvedReason::Unknowable,
        };
        unresolved.push(UnresolvedReference {
            kind: EdgeKind::ClassReference,
            name: name.as_ref().to_string(),
            reason,
            span,
            include_site,
        });
    }
}

/// The two edges the model records per symbol: a `RUN` target and the producer of
/// a consumed `SHARED` name.
///
/// Walked over the symbol table because that is the only access the model offers,
/// and it is a handful of symbols per file.
fn symbol_linked_edges(inputs: &EdgeInputs<'_>, out: &mut Vec<DependencyEdge>) {
    let symbols = &inputs.semantic.symbols;
    for (id, symbol) in symbols.iter() {
        let program = symbols.program_file(id);
        let producer = symbols.shared_producer(id);
        if program.is_none() && producer.is_none() {
            continue;
        }
        let (span, include_site) = sites_of(inputs, symbol.name_span);
        if let Some(file) = program {
            out.push(DependencyEdge {
                kind: EdgeKind::ProgramReference,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(symbol.name.as_ref()),
                },
                span,
                include_site,
            });
        }
        if let Some(file) = producer {
            out.push(DependencyEdge {
                kind: EdgeKind::SharedProducer,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(symbol.name.as_ref()),
                },
                span,
                include_site,
            });
        }
    }
}

/// What `RUN VALUE(...)` is called in a row: it has no name of its own.
const COMPUTED_RUN_TARGET: &str = "VALUE(...)";

/// `RUN` targets the index did not link: the edge to a located file that could
/// not be used, and a row for every one.
fn unlinked_run_rows(
    inputs: &EdgeInputs<'_>,
    edges: &mut Vec<DependencyEdge>,
    unresolved: &mut Vec<UnresolvedReference>,
) {
    for run in inputs.semantic.symbols.unlinked_runs() {
        let (span, include_site) = sites_of(inputs, run.span);
        let name = if run.name.as_ref().is_empty() {
            COMPUTED_RUN_TARGET.to_string()
        } else {
            run.name.as_ref().to_string()
        };
        if let Some(file) = run.located {
            edges.push(DependencyEdge {
                kind: EdgeKind::ProgramReference,
                target: EdgeTarget::IndexedFile {
                    file,
                    name: IndexName::new(&name),
                },
                span,
                include_site,
            });
        }
        unresolved.push(UnresolvedReference {
            kind: EdgeKind::ProgramReference,
            name,
            reason: run.reason,
            span,
            include_site,
        });
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
        let (span, include_site) = sites_of(inputs, symbol.name_span);
        out.push(DependencyEdge {
            kind: EdgeKind::SchemaTable,
            // Already folded by the schema, so two spellings of one table name
            // arrive here as one key and dedup to one edge.
            target: EdgeTarget::SchemaTable(IndexName::new(table.name.as_ref())),
            span,
            include_site,
        });
    }
}
