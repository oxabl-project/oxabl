//! Shared full-file diagnostics collector (KTD3).
//!
//! [`collect_diagnostics`] is the single source of truth for the set of
//! diagnostics a file produces: it runs the existing pure pipeline
//! (preprocess → tokenize → parse-with-recovery → semantic → lint), merges
//! parse errors, loud preprocessor diagnostics, semantic diagnostics, and lint
//! diagnostics, resolves every span back to the **root buffer's** coordinate
//! space, and drops any diagnostic whose origin is not the root buffer (R8).
//!
//! Both the LSP's salsa `diagnostics` query and the CLI `analyze` path call
//! this pipeline, so the two cannot drift (R7).
//!
//! ## Split for salsa (KTD2/KTD3)
//!
//! The pipeline is split at the preprocessor boundary into two reusable halves:
//!
//! - [`expand_source`] preprocesses the root buffer and returns an
//!   [`ExpandedFile`] — the expanded text plus a **flat** virtual→real offset
//!   table and the loud, root-origin preprocessor diagnostics. This is owned,
//!   `Update`-trivial data (no rich `PreprocessedFile`), so the LSP can memoize
//!   it as a salsa `expanded_text` query and get early-cutoff on downstream
//!   diagnostics when an edit produces byte-identical expansion.
//! - [`collect_from_expanded`] runs tokenize → `parse_program` → semantic →
//!   lint over the expanded text and resolves every span through the flat table,
//!   dropping include-origin diagnostics (R8).
//!
//! Parsing uses [`Parser::parse_program`] (error recovery) rather than
//! `parse_statements` (abort-on-first-error): an editing buffer almost always
//! has a parse error, and recovery is required to still surface semantic/lint
//! diagnostics (R6).

use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, FileSpan, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::{Preprocessor, SpanNode, UnresolvedInclude};
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, NullIndex, Semantic, SourceContext, WorkspaceIndex, analyze_file,
};
use oxabl_workspace::FileSystem;

/// Which pipeline stage produced a diagnostic. Lets the CLI route preprocessor
/// diagnostics to its own JSON channel while parse/semantic/lint feed the
/// versioned `diagnostics` envelope section.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSource {
    Parse,
    Preproc,
    Semantic,
    Lint,
}

impl DiagnosticSource {
    /// Stable lowercase tag used in the `analyze` JSON envelope.
    pub fn as_str(self) -> &'static str {
        match self {
            DiagnosticSource::Parse => "parse",
            DiagnosticSource::Preproc => "preproc",
            DiagnosticSource::Semantic => "semantic",
            DiagnosticSource::Lint => "lint",
        }
    }
}

/// A diagnostic whose span has already been resolved to root-buffer
/// coordinates, tagged with the pipeline stage that produced it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CollectedDiagnostic {
    pub diagnostic: Diagnostic,
    pub source: DiagnosticSource,
}

/// The complete, root-resolved diagnostic set for a file.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CollectedDiagnostics {
    pub diagnostics: Vec<CollectedDiagnostic>,
}

impl CollectedDiagnostics {
    /// Every diagnostic (all stages), in pipeline order.
    pub fn all(&self) -> impl Iterator<Item = &CollectedDiagnostic> {
        self.diagnostics.iter()
    }

    /// Only the diagnostics from a given stage.
    pub fn by_source(
        &self,
        source: DiagnosticSource,
    ) -> impl Iterator<Item = &CollectedDiagnostic> {
        self.diagnostics.iter().filter(move |d| d.source == source)
    }

    /// Every diagnostic *except* those from `source`, as an owned set.
    ///
    /// The same filter [`LintResult::excluding_source`] applies, but on the set
    /// rather than the result, so a client that has to drop **two** stages —
    /// `check` under `--no-lint`, which reports neither the preprocessor's own
    /// coverage warnings nor the lint findings — chains two calls instead of
    /// hand-rolling a second predicate that could disagree with this one.
    ///
    /// [`LintResult::excluding_source`]: https://docs.rs/oxabl_pipeline
    #[must_use]
    pub fn excluding_source(&self, source: DiagnosticSource) -> CollectedDiagnostics {
        CollectedDiagnostics {
            diagnostics: self
                .diagnostics
                .iter()
                .filter(|d| d.source != source)
                .cloned()
                .collect(),
        }
    }
}

/// The *loud* preprocessor surfacing rule (moved here from the CLI so both
/// consumers share it): all errors, plus the always-loud warnings PREPROC007
/// (unresolvable include — symbol loss) and PREPROC002 (unclosed `&IF`).
pub fn is_loud(d: &Diagnostic) -> bool {
    matches!(d.severity, Severity::Error) || d.code.0 == "PREPROC007" || d.code.0 == "PREPROC002"
}

/// One include a file names itself, and where it names it.
///
/// The site is in the including file's own bytes, which is what lets a client open
/// an editor at the `{...}` that creates the dependency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectInclude {
    /// The path the preprocessor resolved the include to.
    pub path: PathBuf,
    /// The `{...}` reference in the including file.
    pub site: Span,
}

/// One flattened leaf of the preprocessor span tree: a contiguous run of
/// expanded text `[virt_start, virt_start+len)` that maps to `real_start..` in
/// origin file `file`.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ExpandedChunk {
    virt_start: u32,
    len: u32,
    file: u32,
    real_start: u32,
}

/// The preprocessed root buffer in owned, `Update`-trivial form.
///
/// Holds the expanded text, a flattened virtual→real offset table (the only
/// span-provenance state kept — the rich `PreprocessedFile` is deliberately not
/// retained, KTD3), the loud root-origin preprocessor diagnostics, and the
/// transitive include dependency set (for the U8 watcher). `PartialEq`/`Eq`
/// enable salsa backdating so an edit with unchanged expansion cuts off the
/// downstream diagnostics query early.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandedFile {
    /// Expanded source text fed to the lexer.
    pub text: String,
    /// Flattened span-tree leaves in expansion order. Empty means the identity
    /// mapping (preprocessing was off): virtual == real, origin == root.
    chunks: Vec<ExpandedChunk>,
    /// Loud, root-origin preprocessor diagnostics (already in real coords).
    preproc: Vec<Diagnostic>,
    /// Transitively included files (for change-tracking / invalidation).
    dependencies: Vec<FileId>,
    /// Absolute paths of every include file read during expansion — the
    /// path-level dependency set the LSP watcher matches changed `*.i` files
    /// against (R17).
    dependency_paths: Vec<PathBuf>,
    /// The includes this file names *itself*, at include depth 1.
    ///
    /// Their paths are a subset of `dependency_paths`, each once, in the order the
    /// include sites appear. Captured here because this is the last place the
    /// nesting is visible: the span tree is not retained, and `dependency_paths`
    /// is flat and transitive, so "direct or transitive?" is unanswerable
    /// downstream.
    direct_includes: Vec<DirectInclude>,
    /// Root-origin include references that resolved to no file. An edge set that
    /// dropped these would under-report impact silently.
    unresolved_includes: Vec<UnresolvedInclude>,
    /// The root file id, used for identity resolution and origin checks.
    root: FileId,
}

impl ExpandedFile {
    /// Transitively included files this expansion read (R17 watcher input).
    pub fn dependencies(&self) -> &[FileId] {
        &self.dependencies
    }

    /// Absolute paths of the include files this expansion read (R17 watcher).
    pub fn dependency_paths(&self) -> &[PathBuf] {
        &self.dependency_paths
    }

    /// The includes this file names itself — include depth 1, each once.
    ///
    /// Pairs with [`ExpandedFile::dependency_paths`], which is the transitive set.
    /// The difference between the two is what lets a dependency edge say *direct
    /// include* rather than merely *include*.
    pub fn direct_includes(&self) -> &[DirectInclude] {
        &self.direct_includes
    }

    /// Root-origin include references that named no locatable file.
    pub fn unresolved_includes(&self) -> &[UnresolvedInclude] {
        &self.unresolved_includes
    }

    /// Map a span in the **expanded** buffer back to the root file's own bytes.
    ///
    /// `None` when the span originates inside an include rather than in the root
    /// file, which is the same origin rule diagnostics follow (R8). A consumer
    /// that wants to open an editor at a reference needs this: every span the
    /// semantic model carries is in post-expansion coordinates, and only the
    /// expansion knows the mapping back.
    pub fn resolve_root_span(&self, span: Span) -> Option<Span> {
        self.resolve_span(span).map(|fs| fs.span)
    }

    /// The chunk containing virtual offset `virt`, or `None` past the end of the
    /// expansion.
    ///
    /// A binary search, because the chunks are emitted in strictly increasing virtual
    /// order by `flatten_tree` — it walks the span tree in expansion order and hands
    /// each leaf the running cursor. This used to be a linear scan of an already
    /// ordered table, which is a binary search written the long way: a diagnostic in a
    /// file with many include sites paid a walk of every chunk before it, per span,
    /// per label.
    ///
    /// The ordering is asserted rather than assumed. If the flattening ever emits out
    /// of order the search would return a wrong chunk silently, and a wrong chunk maps
    /// a span into the bytes of a different file.
    fn chunk_at(&self, virt: u32) -> Option<&ExpandedChunk> {
        debug_assert!(
            self.chunks
                .windows(2)
                .all(|pair| pair[0].virt_start + pair[0].len <= pair[1].virt_start),
            "the chunk table must be sorted and non-overlapping for the search below"
        );
        let found = self
            .chunks
            .binary_search_by(|chunk| {
                if virt < chunk.virt_start {
                    std::cmp::Ordering::Greater
                } else if virt >= chunk.virt_start + chunk.len {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .ok()?;
        self.chunks.get(found)
    }

    /// Resolve a virtual (expanded) offset to `(origin file, real offset)`.
    /// Returns `None` when the offset is past the end of the expansion.
    fn resolve(&self, virt: u32) -> Option<(FileId, u32)> {
        if self.chunks.is_empty() {
            // Identity mapping (no preprocessing): every offset is root-relative.
            return Some((self.root, virt));
        }
        let chunk = self.chunk_at(virt)?;
        Some((
            FileId::new(chunk.file),
            chunk.real_start + (virt - chunk.virt_start),
        ))
    }

    /// Resolve a virtual span to a root-buffer [`FileSpan`], or `None` if its
    /// origin is not the root buffer (R8).
    ///
    /// # Boundary convention
    ///
    /// Spans are **half-open**: `start` is the first byte and `end` is one past the
    /// last. The offset table is a map over *bytes*, so only `start` and `end - 1` are
    /// offsets it can answer for — `end` itself names the byte after the span, which
    /// routinely belongs to something else.
    ///
    /// That distinction was the defect (R17). `end` was resolved as though it were a
    /// byte of the span, so a span ending exactly where the next chunk begins resolved
    /// *into that chunk*. When the next chunk came from an include — which is the
    /// common case, since a chunk boundary is usually an include site — the origin
    /// check failed and the span collapsed to zero width. The token immediately before
    /// an include therefore reported a caret with no extent, and a client asked to jump
    /// to it landed on an empty range. `end` past the very end of the expansion failed
    /// the same way, for the same reason.
    ///
    /// The span is resolved **within the chunk its start falls in**. A span that
    /// reaches beyond that chunk is truncated at the chunk's end rather than collapsed:
    /// the bytes past the boundary are not the root file's contiguous bytes at all, so
    /// there is no honest longer answer, and a truncated extent is strictly better than
    /// no extent. A zero-length span stays zero-length.
    fn resolve_span(&self, span: Span) -> Option<FileSpan> {
        let (start_file, real_start) = self.resolve(span.start)?;
        if start_file != self.root {
            return None;
        }

        // An empty span has no last byte to map, so it keeps its point.
        if span.end <= span.start {
            return Some(FileSpan {
                file: self.root,
                span: Span {
                    start: real_start,
                    end: real_start,
                },
            });
        }

        let real_end = match self.chunk_at(span.start) {
            // The last byte of the span, clamped into the chunk the start belongs to,
            // then turned back into an exclusive end by adding one.
            Some(chunk) => {
                let chunk_last = chunk.virt_start + chunk.len - 1;
                let last = (span.end - 1).min(chunk_last);
                chunk.real_start + (last - chunk.virt_start) + 1
            }
            // No chunk table: the identity mapping, where virtual is real.
            None => span.end,
        };

        Some(FileSpan {
            file: self.root,
            span: Span {
                start: real_start,
                end: real_end.max(real_start),
            },
        })
    }
}

/// Preprocess `source` (the content of `root`) into an [`ExpandedFile`].
///
/// When `preprocess` is `false` the source is returned verbatim with an identity
/// offset table (the CLI `--preprocess`-off path). On a *fatal* preprocessing
/// failure, returns the loud root-origin error diagnostics via `Err`.
pub fn expand_source(
    root: FileId,
    source: &str,
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    preprocess: bool,
) -> Result<ExpandedFile, Vec<Diagnostic>> {
    if !preprocess {
        return Ok(ExpandedFile {
            text: source.to_string(),
            chunks: Vec::new(),
            preproc: Vec::new(),
            dependencies: Vec::new(),
            dependency_paths: Vec::new(),
            direct_includes: Vec::new(),
            unresolved_includes: Vec::new(),
            root,
        });
    }

    // Wrap the filesystem to record every include path read during expansion,
    // giving the LSP watcher a precise `*.i` → buffer dependency set (R17).
    let recorder = RecordingFileSystem::new(fs);
    let pp = Preprocessor::new(&recorder, include_paths);
    match pp.process(root, source) {
        Ok(pf) => {
            let mut chunks = Vec::new();
            let mut cursor = 0u32;
            flatten_tree(&pf.tree, &mut cursor, &mut chunks);
            // Loud, root-origin preprocessor diagnostics carry real spans
            // already, so they are filtered by origin directly.
            let mut preproc: Vec<Diagnostic> = pf
                .diagnostics
                .iter()
                .filter(|d| is_loud(d) && d.span.file == root)
                .cloned()
                .collect();
            // An unresolvable include *below* the root elides symbols from this
            // analysis exactly as a root-origin one does, so its explanation has
            // to reach this file's reader. Its own span belongs to an include's
            // buffer, which no consumer of this expansion can render, so it is
            // re-anchored on the root-origin `{...}` site that led to it.
            preproc.extend(nested_unresolved_include_diagnostics(
                &pf.unresolved_includes,
                root,
            ));
            let mut direct_includes = Vec::new();
            collect_direct_includes(&pf.tree, root, &mut direct_includes);
            // An include that failed to resolve deeper down is that file's gap to
            // report, not this one's — the same origin rule diagnostics follow.
            let unresolved_includes = pf
                .unresolved_includes
                .iter()
                .filter(|u| u.site.file == root)
                .cloned()
                .collect();
            Ok(ExpandedFile {
                text: pf.to_text().to_string(),
                chunks,
                preproc,
                dependencies: pf.dependencies.clone(),
                dependency_paths: recorder.into_reads(),
                direct_includes,
                unresolved_includes,
                root,
            })
        }
        Err(diags) => Err(diags
            .into_iter()
            .filter(|d| is_loud(d) && d.span.file == root)
            .collect()),
    }
}

/// Re-anchored `PREPROC007`s for unresolvable includes found below the root.
///
/// A root-origin unresolvable include already reports itself, so only nested
/// ones are synthesized here. Each is attributed to the outermost enclosing
/// `{...}` site, which is the one coordinate that lies in the root file's own
/// bytes — the same origin rule every other diagnostic in this expansion obeys.
/// A chain whose outermost site is not the root cannot be rendered against it
/// and is left to the file that owns it.
///
/// One unresolvable include reached through several paths is reported once per
/// distinct root-origin site: the dedup key is that site plus the missing name,
/// mirroring how `collect_direct_includes` folds a shared include into a single
/// row rather than one per expansion of it.
fn nested_unresolved_include_diagnostics(
    unresolved: &[UnresolvedInclude],
    root: FileId,
) -> Vec<Diagnostic> {
    let mut out: Vec<Diagnostic> = Vec::new();
    let mut seen: Vec<(Span, &str)> = Vec::new();
    for include in unresolved {
        // Root-origin references are already carried by the preprocessor's own
        // diagnostics; this is only about the ones that filter drops.
        if include.site.file == root {
            continue;
        }
        let Some(anchor) = include.via.first().filter(|site| site.file == root) else {
            continue;
        };
        if seen.contains(&(anchor.span, include.name.as_str())) {
            continue;
        }
        seen.push((anchor.span, include.name.as_str()));
        out.push(
            Diagnostic::warning(
                "PREPROC007",
                format!(
                    "unresolvable include '{}' — symbols it declares cannot be checked \
                     (reached through this include)",
                    include.name
                ),
                *anchor,
            )
            .with_help(
                "add its directory to include_paths (oxabl.toml [workspace.sources]) or pass -I"
                    .to_string(),
            ),
        );
    }
    out
}

/// A [`FileSystem`] decorator that records the path of every successful `read`,
/// used to capture an expansion's include dependency set.
struct RecordingFileSystem<'a> {
    inner: &'a dyn FileSystem,
    reads: Mutex<Vec<PathBuf>>,
}

impl<'a> RecordingFileSystem<'a> {
    fn new(inner: &'a dyn FileSystem) -> Self {
        RecordingFileSystem {
            inner,
            reads: Mutex::new(Vec::new()),
        }
    }

    fn into_reads(self) -> Vec<PathBuf> {
        self.reads.into_inner().unwrap_or_default()
    }
}

impl FileSystem for RecordingFileSystem<'_> {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        let result = self.inner.read(path);
        if result.is_ok()
            && let Ok(mut reads) = self.reads.lock()
        {
            reads.push(path.to_path_buf());
        }
        result
    }

    fn exists(&self, path: &Path) -> bool {
        self.inner.exists(path)
    }
}

/// Walk the span tree in expansion order, emitting a flat chunk per leaf.
fn flatten_tree(nodes: &[SpanNode], cursor: &mut u32, out: &mut Vec<ExpandedChunk>) {
    for node in nodes {
        match node {
            SpanNode::Chunk { file, start, end } => {
                let len = end - start;
                out.push(ExpandedChunk {
                    virt_start: *cursor,
                    len,
                    file: file.raw(),
                    real_start: *start,
                });
                *cursor += len;
            }
            SpanNode::Include { children, .. } => flatten_tree(children, cursor, out),
        }
    }
}

/// Collect the depth-1 includes from a span tree, each path once, in site order.
///
/// Only a node carrying a path is a file include; a `{&var}` or `{N}` substitution
/// reuses the same variant with `path: None`. Those are walked *through* rather
/// than counted, because their spliced text sits at the parent's own depth — and a
/// real include reached that way is still one this file names itself. A resolved
/// include's own children are not walked: everything below it is transitive.
///
/// A node whose site is not in `root` is skipped. Nothing produces one today, and
/// the alternative to skipping is reporting a site that points at bytes of another
/// file as though they were the root's.
fn collect_direct_includes(nodes: &[SpanNode], root: FileId, out: &mut Vec<DirectInclude>) {
    for node in nodes {
        if let SpanNode::Include {
            site,
            path,
            children,
        } = node
        {
            match path {
                Some(path) => {
                    if site.file == root && !out.iter().any(|d| d.path == *path) {
                        out.push(DirectInclude {
                            path: path.clone(),
                            site: site.span,
                        });
                    }
                }
                None => collect_direct_includes(children, root, out),
            }
        }
    }
}

/// Run tokenize → `parse_program` → semantic → lint over an already-expanded
/// buffer and return every diagnostic resolved to root-buffer coordinates,
/// plus the [`Semantic`] model (for the CLI's non-diagnostic envelope sections).
///
/// This is the half the LSP's salsa `diagnostics` query calls — its input is
/// the memoized [`ExpandedFile`], so unchanged expansion → memo hit.
///
/// `index` answers the cross-file questions the resolve pass may ask. It is a
/// borrowed handle rather than an `Option` because absence already has a
/// representation — [`NullIndex`](oxabl_semantic::NullIndex), whose revision is
/// `ABSENT` — and one that resolution code can call unconditionally. Pass that
/// to get exactly the single-file answers this function gave before there was an
/// index at all; pass a real one and the caller's own file must already be
/// excluded from it, which is the *caller's* knowledge and not something
/// derivable from an expansion.
pub fn collect_from_expanded(
    expanded: &ExpandedFile,
    schema: &Schema,
    schema_loaded: bool,
    lint_severities: &LintSeverityMap,
    index: &dyn WorkspaceIndex,
) -> (Option<Semantic>, CollectedDiagnostics) {
    collect_from_expanded_with_source_context(
        expanded,
        schema,
        schema_loaded,
        lint_severities,
        index,
        SourceContext::CompilationUnit,
    )
}

/// [`collect_from_expanded`] with an explicit root-source classification.
///
/// The compatibility entry point above keeps compilation-unit behavior for
/// callers without a root path. The shared pipeline uses this entry point when
/// it knows that an explicitly opened root is an include fragment.
pub fn collect_from_expanded_with_source_context(
    expanded: &ExpandedFile,
    schema: &Schema,
    schema_loaded: bool,
    lint_severities: &LintSeverityMap,
    index: &dyn WorkspaceIndex,
    source_context: SourceContext,
) -> (Option<Semantic>, CollectedDiagnostics) {
    let root = expanded.root;
    let mut out = CollectedDiagnostics::default();

    for d in &expanded.preproc {
        out.diagnostics.push(CollectedDiagnostic {
            diagnostic: d.clone(),
            source: DiagnosticSource::Preproc,
        });
    }

    let tokens = tokenize(&expanded.text);
    let mut parser = Parser::new(&tokens, &expanded.text);
    let program = parser.parse_program();

    for err in program.errors {
        let virt = err.into_diagnostic(root);
        if let Some(resolved) = resolve_diagnostic(virt, expanded) {
            out.diagnostics.push(CollectedDiagnostic {
                diagnostic: resolved,
                source: DiagnosticSource::Parse,
            });
        }
    }

    // Built through the builders rather than as a struct literal, so
    // `index_loaded` comes from `with_index`'s single derivation — restating
    // "loaded means the revision is not ABSENT" here would be a second copy of
    // the rule that decides whether a cross-file miss is a fact about the
    // workspace or merely "we did not look". Only `schema_loaded` is assigned
    // directly: `new` infers it from an empty schema, and this path is handed the
    // caller's explicit answer, which is the whole point of the flag.
    let mut ctx = AnalysisContext::new(root, &expanded.text, schema)
        .with_lint_severities(lint_severities.clone())
        .with_index(index)
        .with_source_context(source_context);
    ctx.schema_loaded = schema_loaded;
    let sem = analyze_file(&program.statements, &ctx);

    for d in &sem.diagnostics {
        if let Some(resolved) = resolve_diagnostic(d.clone(), expanded) {
            out.diagnostics.push(CollectedDiagnostic {
                diagnostic: resolved,
                source: DiagnosticSource::Semantic,
            });
        }
    }

    for d in oxabl_lint::lint_file(&program.statements, &sem, &ctx) {
        if let Some(resolved) = resolve_diagnostic(d, expanded) {
            out.diagnostics.push(CollectedDiagnostic {
                diagnostic: resolved,
                source: DiagnosticSource::Lint,
            });
        }
    }

    (Some(sem), out)
}

/// Resolve a diagnostic's expanded span (and label spans) to root coordinates,
/// or drop it if its origin is not the root buffer (R8).
fn resolve_diagnostic(mut d: Diagnostic, expanded: &ExpandedFile) -> Option<Diagnostic> {
    d.span = expanded.resolve_span(d.span.span)?;
    d.labels
        .retain_mut(|label| match expanded.resolve_span(label.span.span) {
            Some(fs) => {
                label.span = fs;
                true
            }
            None => false,
        });
    Some(d)
}

/// Run the full pipeline for `source` and return every diagnostic, resolved to
/// root-buffer coordinates. See the module docs for the meaning of each
/// argument; `preprocess` mirrors the CLI `--preprocess` toggle (the LSP always
/// passes `true`, R6).
#[allow(clippy::too_many_arguments)]
pub fn collect_diagnostics(
    root: FileId,
    source: &str,
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    schema: &Schema,
    schema_loaded: bool,
    lint_severities: &LintSeverityMap,
    preprocess: bool,
) -> CollectedDiagnostics {
    collect_with_model(
        root,
        source,
        fs,
        include_paths,
        schema,
        schema_loaded,
        lint_severities,
        preprocess,
    )
    .1
}

/// Like [`collect_diagnostics`], but also returns the [`Semantic`] model so the
/// CLI `analyze` dump can render the non-diagnostic envelope sections without a
/// second analysis pass. The model is `None` only when preprocessing failed
/// fatally (no parse possible).
///
/// # No cross-file index
///
/// This composition and [`collect_diagnostics`] predate the workspace index and
/// answer **single-file**, passing [`NullIndex`]. That is deliberate rather than
/// pending work: an index needs the asking file's own identity so it can be
/// excluded from its own lookups, and these two take a source string and an
/// include-path list — nothing that says *which file* the string is. The client
/// that does know is `oxabl_pipeline::LintPipeline` (named in prose because it
/// sits *above* this crate), which is why it drives [`expand_source`] and
/// [`collect_from_expanded`] directly and passes a real index.
#[allow(clippy::too_many_arguments)]
pub fn collect_with_model(
    root: FileId,
    source: &str,
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    schema: &Schema,
    schema_loaded: bool,
    lint_severities: &LintSeverityMap,
    preprocess: bool,
) -> (Option<Semantic>, CollectedDiagnostics) {
    match expand_source(root, source, fs, include_paths, preprocess) {
        Ok(expanded) => collect_from_expanded(
            &expanded,
            schema,
            schema_loaded,
            lint_severities,
            &NullIndex,
        ),
        Err(preproc_errors) => {
            let mut out = CollectedDiagnostics::default();
            for d in preproc_errors {
                out.diagnostics.push(CollectedDiagnostic {
                    diagnostic: d,
                    source: DiagnosticSource::Preproc,
                });
            }
            (None, out)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_schema::test_support::customer_schema;
    use oxabl_workspace::InMemoryFileSystem;

    const ROOT: FileId = FileId::new(1);

    /// Collect diagnostics for a single root buffer with an empty schema and no
    /// include search path, preprocessing on.
    fn collect_simple(source: &str) -> CollectedDiagnostics {
        let fs = InMemoryFileSystem::new();
        let schema = Schema::empty();
        collect_diagnostics(
            ROOT,
            source,
            &fs,
            &[],
            &schema,
            false,
            &LintSeverityMap::new(),
            true,
        )
    }

    fn codes(c: &CollectedDiagnostics) -> Vec<&str> {
        c.all().map(|d| d.diagnostic.code.0).collect()
    }

    /// An expansion whose chunk table is supplied directly.
    ///
    /// Built rather than preprocessed, because the boundary cases below are about the
    /// offset table's shape and a source fixture reaches them only by coincidence.
    /// `chunks` is `(virt_start, len, file, real_start)` in expansion order.
    fn expansion_with(chunks: &[(u32, u32, u32, u32)]) -> ExpandedFile {
        ExpandedFile {
            text: String::new(),
            chunks: chunks
                .iter()
                .map(|(virt_start, len, file, real_start)| ExpandedChunk {
                    virt_start: *virt_start,
                    len: *len,
                    file: *file,
                    real_start: *real_start,
                })
                .collect(),
            preproc: Vec::new(),
            dependencies: Vec::new(),
            dependency_paths: Vec::new(),
            direct_includes: Vec::new(),
            unresolved_includes: Vec::new(),
            root: ROOT,
        }
    }

    /// The root's own text, then an include's, then the root's again: the shape every
    /// file with an include site has.
    fn root_include_root() -> ExpandedFile {
        expansion_with(&[
            (0, 10, ROOT.raw(), 0),
            (10, 20, 2, 0),
            (30, 10, ROOT.raw(), 10),
        ])
    }

    /// A span ending exactly where the next chunk begins keeps its extent (R17).
    ///
    /// The defect: `end` is exclusive, so it names the byte *after* the span, but it
    /// was resolved as though it were a byte of the span. At a chunk boundary that byte
    /// belongs to the next chunk — an include — so the origin check failed and the span
    /// collapsed to zero width. The token immediately before an include site is exactly
    /// this case, and a client asked to jump to it landed on an empty range.
    #[test]
    fn a_span_ending_on_a_chunk_boundary_keeps_its_extent() {
        let expanded = root_include_root();

        let resolved = expanded
            .resolve_root_span(Span { start: 4, end: 10 })
            .expect("a span that starts in the root resolves");

        assert_eq!(
            resolved,
            Span { start: 4, end: 10 },
            "a span ending where the include begins must keep its six bytes"
        );
    }

    /// The same failure at the other boundary: a span ending at the very end of the
    /// expansion had no offset past it to resolve at all.
    #[test]
    fn a_span_ending_at_the_end_of_the_expansion_keeps_its_extent() {
        let expanded = root_include_root();

        let resolved = expanded
            .resolve_root_span(Span { start: 34, end: 40 })
            .expect("a span in the trailing root chunk resolves");

        assert_eq!(resolved, Span { start: 14, end: 20 });
    }

    /// A span wholly inside one chunk was never broken, and must stay that way.
    #[test]
    fn a_span_inside_one_chunk_is_unaffected() {
        let expanded = root_include_root();

        assert_eq!(
            expanded.resolve_root_span(Span { start: 32, end: 36 }),
            Some(Span { start: 12, end: 16 })
        );
    }

    /// A zero-length span is a caret, and stays one — it must not become negative,
    /// panic on the `end - 1`, or acquire width it never had.
    #[test]
    fn a_zero_length_span_at_a_boundary_stays_zero_length() {
        let expanded = root_include_root();

        // The first and last byte of each root chunk, so both edges of the boundary
        // arithmetic are covered.
        for (virt, real) in [(0, 0), (9, 9), (30, 10), (39, 19)] {
            assert_eq!(
                expanded.resolve_root_span(Span {
                    start: virt,
                    end: virt
                }),
                Some(Span {
                    start: real,
                    end: real
                }),
                "a caret at {virt} must stay a caret at {real}"
            );
        }
    }

    /// A span reaching past its start chunk is truncated at that chunk, not collapsed.
    ///
    /// The bytes beyond the boundary are not the root file's contiguous bytes, so there
    /// is no honest longer answer — and the previous behaviour, collapsing to zero
    /// width, threw away the extent that *was* known.
    #[test]
    fn a_span_crossing_a_chunk_boundary_is_truncated_rather_than_collapsed() {
        let expanded = root_include_root();

        let resolved = expanded
            .resolve_root_span(Span { start: 6, end: 35 })
            .expect("the start is in the root");

        assert_eq!(
            resolved,
            Span { start: 6, end: 10 },
            "the extent must stop at the chunk boundary rather than vanish"
        );
    }

    /// A span whose start is inside an include is not the root's to report (R8).
    #[test]
    fn a_span_starting_inside_an_include_has_no_root_span() {
        let expanded = root_include_root();
        assert_eq!(
            expanded.resolve_root_span(Span { start: 12, end: 18 }),
            None
        );
    }

    /// The search and the scan it replaced agree on every offset.
    ///
    /// Pinned over a deep table rather than a two-chunk one, because a binary search
    /// that is wrong at a boundary is right everywhere else — the fixture has to have
    /// enough boundaries for a wrong one to show.
    #[test]
    fn the_binary_search_agrees_with_a_linear_scan_at_every_offset() {
        let chunks: Vec<(u32, u32, u32, u32)> = (0..16)
            .map(|index| {
                let file = if index % 2 == 0 {
                    ROOT.raw()
                } else {
                    index + 10
                };
                (index * 7, 7, file, index * 3)
            })
            .collect();
        let expanded = expansion_with(&chunks);

        for virt in 0..(16 * 7 + 4) {
            let scanned = expanded
                .chunks
                .iter()
                .find(|chunk| virt >= chunk.virt_start && virt < chunk.virt_start + chunk.len)
                .map(|chunk| {
                    (
                        FileId::new(chunk.file),
                        chunk.real_start + (virt - chunk.virt_start),
                    )
                });
            assert_eq!(
                expanded.resolve(virt),
                scanned,
                "the search and the scan disagree at {virt}"
            );
        }
    }

    /// The identity mapping — preprocessing off, so no chunk table — is untouched.
    #[test]
    fn an_expansion_with_no_chunks_maps_a_span_to_itself() {
        let expanded = expansion_with(&[]);
        assert_eq!(
            expanded.resolve_root_span(Span { start: 3, end: 11 }),
            Some(Span { start: 3, end: 11 })
        );
    }

    /// The invariant the search rests on, over a table a real expansion produced.
    #[test]
    fn the_chunk_table_is_emitted_in_increasing_order() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            std::path::PathBuf::from("./outer.i"),
            "{inner.i}\nDEFINE VARIABLE a AS INTEGER.\n",
        );
        fs.insert(
            std::path::PathBuf::from("./inner.i"),
            "DEFINE VARIABLE b AS INTEGER.\n",
        );
        let expanded = expand_source(
            ROOT,
            "{outer.i}\nMESSAGE a.\n{outer.i}\nMESSAGE b.\n",
            &fs,
            &[std::path::PathBuf::from(".")],
            true,
        )
        .expect("the fixture expands");

        assert!(
            expanded.chunks.len() > 2,
            "a nested include must produce several chunks, got {}",
            expanded.chunks.len()
        );
        for pair in expanded.chunks.windows(2) {
            assert!(
                pair[0].virt_start + pair[0].len <= pair[1].virt_start,
                "chunks must not overlap or go backwards: {:?} then {:?}",
                pair[0],
                pair[1]
            );
        }
    }

    #[test]
    fn unused_variable_fires() {
        let c = collect_simple("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
        assert!(codes(&c).contains(&"LINT0002"), "got {:?}", codes(&c));
    }

    // Covers R6: parse-error recovery still yields semantic/lint diagnostics.
    #[test]
    fn parse_error_recovery_still_lints() {
        let src =
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n@ @ @\nDEFINE VARIABLE y AS INTEGER NO-UNDO.\n";
        let c = collect_simple(src);
        let cs = codes(&c);
        assert!(
            cs.contains(&"PARSE001"),
            "expected a parse error, got {cs:?}"
        );
        assert!(
            cs.contains(&"LINT0002"),
            "expected lint despite parse error, got {cs:?}"
        );
        assert!(c.diagnostics.len() >= 2, "got {cs:?}");
    }

    // Covers R9: include-resident symbol used in root is not flagged undefined.
    #[test]
    fn include_resident_symbol_not_undefined() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            "/proj/defs.i".into(),
            "DEFINE VARIABLE fromInclude AS INTEGER NO-UNDO.\n",
        );
        let schema = Schema::empty();
        let src = "{defs.i}\nfromInclude = 5.\n";
        let c = collect_diagnostics(
            ROOT,
            src,
            &fs,
            &["/proj".into()],
            &schema,
            false,
            &LintSeverityMap::new(),
            true,
        );
        assert!(
            !codes(&c).contains(&"LINT0001"),
            "include-resident symbol must not be undefined, got {:?}",
            codes(&c)
        );
    }

    // Covers R8: a diagnostic whose origin is inside an expanded include is
    // dropped from the root buffer's set.
    #[test]
    fn include_origin_diagnostic_dropped() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            "/proj/defs.i".into(),
            "DEFINE VARIABLE onlyInInclude AS INTEGER NO-UNDO.\n",
        );
        let schema = Schema::empty();
        let src = "{defs.i}\nMESSAGE \"hi\".\n";
        let c = collect_diagnostics(
            ROOT,
            src,
            &fs,
            &["/proj".into()],
            &schema,
            false,
            &LintSeverityMap::new(),
            true,
        );
        assert!(
            !codes(&c).contains(&"LINT0002"),
            "include-origin diagnostic must be dropped, got {:?}",
            codes(&c)
        );
    }

    // Covers R10: schema-gated LINT0003 is dark without a schema, live with one.
    #[test]
    fn schema_gated_unknown_field() {
        let fs = InMemoryFileSystem::new();
        let src = "FIND FIRST Customer.\nMESSAGE Customer.NoSuchField.\n";

        let empty = Schema::empty();
        let dark = collect_diagnostics(
            ROOT,
            src,
            &fs,
            &[],
            &empty,
            false,
            &LintSeverityMap::new(),
            true,
        );
        assert!(
            !codes(&dark).contains(&"LINT0003"),
            "LINT0003 must be dark without a schema, got {:?}",
            codes(&dark)
        );

        let schema = customer_schema();
        let live = collect_diagnostics(
            ROOT,
            src,
            &fs,
            &[],
            &schema,
            true,
            &LintSeverityMap::new(),
            true,
        );
        assert!(
            codes(&live).contains(&"LINT0003"),
            "LINT0003 must fire on a bad field under a loaded schema, got {:?}",
            codes(&live)
        );
    }

    // Loud filter: an unresolvable include surfaces PREPROC007 as a Preproc
    // diagnostic.
    #[test]
    fn unresolvable_include_surfaces_preproc007() {
        let fs = InMemoryFileSystem::new();
        let schema = Schema::empty();
        let c = collect_diagnostics(
            ROOT,
            "{missing.i}\nMESSAGE \"hi\".\n",
            &fs,
            &[],
            &schema,
            false,
            &LintSeverityMap::new(),
            true,
        );
        assert!(
            c.by_source(DiagnosticSource::Preproc)
                .any(|d| d.diagnostic.code.0 == "PREPROC007"),
            "expected PREPROC007, got {:?}",
            codes(&c)
        );
    }

    // Off-by-config lint rule is skipped; remap changes severity.
    #[test]
    fn lint_severities_applied() {
        let fs = InMemoryFileSystem::new();
        let schema = Schema::empty();
        let src = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";

        let mut off = LintSeverityMap::new();
        off.set("LINT0002", None);
        let c = collect_diagnostics(ROOT, src, &fs, &[], &schema, false, &off, true);
        assert!(
            !codes(&c).contains(&"LINT0002"),
            "off rule must be skipped, got {:?}",
            codes(&c)
        );

        let mut info = LintSeverityMap::new();
        info.set("LINT0002", Some(Severity::Info));
        let c = collect_diagnostics(ROOT, src, &fs, &[], &schema, false, &info, true);
        let d = c
            .all()
            .find(|d| d.diagnostic.code.0 == "LINT0002")
            .expect("LINT0002 present");
        assert_eq!(d.diagnostic.severity, Severity::Info);
    }

    // The expansion table resolves include-origin vs root-origin correctly and
    // exposes include dependencies for the watcher.
    #[test]
    fn expand_source_tracks_dependencies() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert("/proj/defs.i".into(), "MESSAGE \"from include\".\n");
        let expanded = expand_source(
            ROOT,
            "{defs.i}\nMESSAGE \"root\".\n",
            &fs,
            &["/proj".into()],
            true,
        )
        .expect("expansion succeeds");
        assert!(
            !expanded.dependencies().is_empty(),
            "include dependency must be tracked"
        );
    }

    /// Expand `source` as the root buffer against an in-memory `/proj` tree.
    fn expand_in_proj(files: &[(&str, &str)], source: &str) -> ExpandedFile {
        let mut fs = InMemoryFileSystem::new();
        for (name, content) in files {
            fs.insert(format!("/proj/{name}").into(), *content);
        }
        expand_source(ROOT, source, &fs, &["/proj".into()], true).expect("expansion succeeds")
    }

    fn direct_names(expanded: &ExpandedFile) -> Vec<String> {
        expanded
            .direct_includes()
            .iter()
            .map(|d| d.path.file_name().unwrap().to_string_lossy().into_owned())
            .collect()
    }

    // A file with one include reports exactly that include as direct.
    #[test]
    fn one_include_is_the_whole_direct_set() {
        let expanded = expand_in_proj(&[("a.i", "MESSAGE \"a\".\n")], "{a.i}\nMESSAGE \"root\".\n");
        assert_eq!(direct_names(&expanded), vec!["a.i"]);
    }

    // `a.i` includes `b.i`: only `a.i` is direct, but both are transitive.
    #[test]
    fn nested_include_is_transitive_not_direct() {
        let expanded = expand_in_proj(
            &[
                ("a.i", "{b.i}\nMESSAGE \"a\".\n"),
                ("b.i", "MESSAGE \"b\".\n"),
            ],
            "{a.i}\n",
        );
        assert_eq!(direct_names(&expanded), vec!["a.i"]);
        let transitive: Vec<_> = expanded
            .dependency_paths()
            .iter()
            .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        assert!(
            transitive.contains(&"a.i".to_string()) && transitive.contains(&"b.i".to_string()),
            "transitive set must hold both, got {transitive:?}"
        );
    }

    // Including the same file twice reports it once.
    #[test]
    fn repeated_include_appears_once_in_the_direct_set() {
        let expanded = expand_in_proj(&[("a.i", "MESSAGE \"a\".\n")], "{a.i}\n{a.i}\n");
        assert_eq!(direct_names(&expanded), vec!["a.i"]);
    }

    // An include cycle terminates and each participant appears at most once.
    #[test]
    fn include_cycle_terminates_with_each_participant_once() {
        let expanded = expand_in_proj(
            &[
                ("a.i", "{b.i}\nMESSAGE \"a\".\n"),
                ("b.i", "{a.i}\nMESSAGE \"b\".\n"),
            ],
            "{a.i}\n{b.i}\n",
        );
        let mut names = direct_names(&expanded);
        names.sort();
        assert_eq!(names, vec!["a.i", "b.i"]);
    }

    // Past the include depth cap, the direct includes found are still reported and
    // nothing panics. The cap is 64, so a 70-deep chain crosses it.
    #[test]
    fn include_depth_cap_reports_what_it_found() {
        let mut files: Vec<(String, String)> = Vec::new();
        for level in 0..70 {
            files.push((format!("d{level}.i"), format!("{{d{}.i}}\n", level + 1)));
        }
        let borrowed: Vec<(&str, &str)> = files
            .iter()
            .map(|(n, c)| (n.as_str(), c.as_str()))
            .collect();
        let expanded = expand_in_proj(&borrowed, "{d0.i}\n");
        assert_eq!(direct_names(&expanded), vec!["d0.i"]);
    }

    // Preprocessing off yields an empty direct set rather than an error.
    #[test]
    fn preprocessing_disabled_yields_an_empty_direct_set() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert("/proj/a.i".into(), "MESSAGE \"a\".\n");
        let expanded = expand_source(ROOT, "{a.i}\n", &fs, &["/proj".into()], false)
            .expect("no preprocessing still expands");
        assert!(expanded.direct_includes().is_empty());
    }

    // Each direct include carries the site of its own `{...}` in the root file, so
    // a client can open an editor at the reference that creates the dependency.
    #[test]
    fn a_direct_include_carries_its_site_in_the_root_file() {
        let src = "MESSAGE \"before\".\n{a.i}\n";
        let expanded = expand_in_proj(&[("a.i", "MESSAGE \"a\".\n")], src);
        let site = expanded.direct_includes()[0].site;
        assert_eq!(&src[site.start as usize..site.end as usize], "{a.i}");
    }

    // An include that resolves to no file is reported as its own row, not dropped.
    // A missing edge nobody mentions is the failure this data exists to prevent.
    #[test]
    fn an_unresolvable_include_is_reported_rather_than_dropped() {
        let expanded = expand_in_proj(&[], "{nowhere.i}\nMESSAGE \"root\".\n");
        assert!(expanded.direct_includes().is_empty());
        let unresolved = expanded.unresolved_includes();
        assert_eq!(unresolved.len(), 1, "got {unresolved:?}");
        assert_eq!(unresolved[0].name, "nowhere.i");
        assert_eq!(unresolved[0].site.file, ROOT);
    }

    // An include that fails to resolve *inside* another include is that file's gap
    // to report, matching the origin rule diagnostics follow.
    #[test]
    fn an_unresolvable_include_below_the_root_is_not_the_roots_row() {
        let expanded = expand_in_proj(&[("a.i", "{nowhere.i}\n")], "{a.i}\n");
        assert!(expanded.unresolved_includes().is_empty());
    }

    // Two expansions of identical input compare equal, so salsa can still backdate
    // and cut off the downstream diagnostics query.
    #[test]
    fn identical_expansions_still_compare_equal() {
        let files = [
            ("a.i", "{b.i}\nMESSAGE \"a\".\n"),
            ("b.i", "MESSAGE \"b\".\n"),
        ];
        let src = "{a.i}\nMESSAGE \"root\".\n";
        assert_eq!(expand_in_proj(&files, src), expand_in_proj(&files, src));
    }

    /// Collect diagnostics for a single root buffer against the canonical
    /// `Customer(CustNum, Name)` schema, preprocessing on.
    fn collect_with_customer(source: &str) -> CollectedDiagnostics {
        let fs = InMemoryFileSystem::new();
        let schema = customer_schema();
        collect_diagnostics(
            ROOT,
            source,
            &fs,
            &[],
            &schema,
            true,
            &LintSeverityMap::new(),
            true,
        )
    }

    // #107: a break field named bare inside FIRST-OF must resolve against the
    // FOR EACH block's implicit buffer, not be flagged undefined.
    #[test]
    fn unqualified_first_of_break_field_not_undefined() {
        let src = "FOR EACH Customer NO-LOCK BREAK BY Customer.Name:\n\
                   IF FIRST-OF(Name) THEN DISPLAY Customer.CustNum.\nEND.\n";
        let c = collect_with_customer(src);
        assert!(
            !codes(&c).contains(&"LINT0001"),
            "bare break field in FIRST-OF must resolve, got {:?}",
            codes(&c)
        );
    }

    // Control for #107: the qualified form already resolved; keep it green.
    #[test]
    fn qualified_first_of_break_field_not_undefined() {
        let src = "FOR EACH Customer NO-LOCK BREAK BY Customer.Name:\n\
                   IF FIRST-OF(Customer.Name) THEN DISPLAY Customer.CustNum.\nEND.\n";
        let c = collect_with_customer(src);
        assert!(
            !codes(&c).contains(&"LINT0001"),
            "qualified break field in FIRST-OF must resolve, got {:?}",
            codes(&c)
        );
    }

    // True-positive guard for #107: a bare name inside FIRST-OF that is not a
    // field of any block buffer must still be flagged undefined.
    #[test]
    fn unknown_bare_name_in_first_of_still_undefined() {
        let src = "FOR EACH Customer NO-LOCK BREAK BY Customer.Name:\n\
                   IF FIRST-OF(NoSuchField) THEN DISPLAY Customer.CustNum.\nEND.\n";
        let c = collect_with_customer(src);
        assert!(
            codes(&c).contains(&"LINT0001"),
            "unknown bare name in FIRST-OF must still be undefined, got {:?}",
            codes(&c)
        );
    }
}
