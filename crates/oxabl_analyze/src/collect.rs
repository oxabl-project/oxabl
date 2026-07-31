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
use oxabl_preprocessor::{Preprocessor, SpanNode};
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, NullIndex, Semantic, WorkspaceIndex, analyze_file};
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

    /// Resolve a virtual (expanded) offset to `(origin file, real offset)`.
    /// Returns `None` when the offset is past the end of the expansion.
    fn resolve(&self, virt: u32) -> Option<(FileId, u32)> {
        if self.chunks.is_empty() {
            // Identity mapping (no preprocessing): every offset is root-relative.
            return Some((self.root, virt));
        }
        for c in &self.chunks {
            if virt >= c.virt_start && virt < c.virt_start + c.len {
                return Some((FileId::new(c.file), c.real_start + (virt - c.virt_start)));
            }
        }
        None
    }

    /// Resolve a virtual span to a root-buffer [`FileSpan`], or `None` if its
    /// origin is not the root buffer (R8). Start and end resolve independently;
    /// the real end is clamped to be no earlier than the real start.
    fn resolve_span(&self, span: Span) -> Option<FileSpan> {
        let (start_file, real_start) = self.resolve(span.start)?;
        if start_file != self.root {
            return None;
        }
        let real_end = if span.end <= span.start {
            real_start
        } else {
            match self.resolve(span.end) {
                Some((f, r)) if f == self.root => r,
                _ => real_start,
            }
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
            let preproc = pf
                .diagnostics
                .iter()
                .filter(|d| is_loud(d) && d.span.file == root)
                .cloned()
                .collect();
            Ok(ExpandedFile {
                text: pf.to_text().to_string(),
                chunks,
                preproc,
                dependencies: pf.dependencies.clone(),
                dependency_paths: recorder.into_reads(),
                root,
            })
        }
        Err(diags) => Err(diags
            .into_iter()
            .filter(|d| is_loud(d) && d.span.file == root)
            .collect()),
    }
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
        .with_index(index);
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
