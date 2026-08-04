//! The shared lint run: one handle, two phases, one result (R1, R2, R3).
//!
//! [`LintPipeline`] is the object every client drives. It owns the orchestration
//! that the CLI, the language server, and the browser each used to hand-roll:
//! expand the root buffer through the preprocessor, then tokenize → parse →
//! semantic → lint over the expansion and resolve every span back to the root
//! buffer's coordinate space.
//!
//! # Why two phases stay visible (KTD2)
//!
//! The obvious API is one `run`, and two of the three clients want exactly that.
//! The language server does not: it needs the intermediate expansion for two
//! things a one-shot call cannot give it.
//!
//! * **Watcher matching.** [`Expansion::dependency_paths`] is how a changed
//!   include file is matched to the open buffers that read it.
//! * **Early cutoff.** The expansion is memoized, so an edit whose expanded text
//!   is byte-identical skips the expensive parse, semantic, and lint work
//!   entirely.
//!
//! A pipeline that exposed only the convenience would force the language server
//! to keep its own orchestration — which is the exact duplication this crate
//! exists to remove. So [`LintPipeline::expand`] and [`LintPipeline::collect`]
//! are first-class operations and [`LintPipeline::run`] is their composition.
//!
//! # Why only `run` is guarded (KTD6, R20)
//!
//! Panic containment is not invented here: [`oxabl_common::catch_panic`] is the
//! one guard every client shares, and [`LintPipeline::run`] calls it *directly*.
//! It must not reach for `oxabl::try_analyze` instead — this crate sits beneath
//! the `oxabl` umbrella, whose `try_*` bodies are re-pointed at this pipeline, so
//! calling them from here is a package cycle Cargo rejects and would be
//! self-recursive even if it linked.
//!
//! [`expand`](LintPipeline::expand) and [`collect`](LintPipeline::collect) are
//! deliberately **unguarded**, and that is load-bearing rather than an
//! oversight. The language server runs them inside `salsa::Cancelled::catch`,
//! and a salsa cancellation travels as a panic payload: a guard inside these two
//! would swallow it, turning "this snapshot is stale, abandon it" into "this file
//! failed to analyze" and publishing stale diagnostics. Telling the two apart
//! would require this crate to recognize salsa's payload type — i.e. to depend on
//! the exact-pinned `salsa`, which the umbrella re-exports into the browser
//! bundle. Leaving these two raw is what lets the language server layer its own
//! guard *outside* its cancellation catch, which is where it belongs.
//!
//! # Cross-file resolution, and why the file's identity is a builder (KTD7)
//!
//! The pipeline owns the run's cross-file index: it is built from the *same*
//! [`FileSystem`] handle and the *same* resolved include paths this handle
//! already has, never a second filesystem. That matters because the CLI reads
//! sources with real `std::fs` calls while holding a trait object here — an index
//! built over the wrong one would have the command line and the language server
//! searching different trees for the same name (R7).
//!
//! The three phases still take a source string, so the asking file's identity —
//! needed at minimum so a file cannot resolve a name to its own copy on disk —
//! arrives through [`LintPipeline::with_file`] rather than a signature change.
//! Every phase signature and every client compiles unchanged, and the identity
//! stays *optional*, which is correct rather than merely convenient: the browser
//! has no path for its buffer, and a file with no identity cannot collide with
//! itself.
//!
//! `with_file` borrows rather than consuming, and that is the load-bearing part.
//! One index must span a whole **run** — that is what makes the shared-dependency
//! dedup pay, since a hundred files inheriting one base must read that base
//! once — while the exclusion varies per **file**. So a multi-file walk stands up
//! one pipeline and derives a per-file handle from it for each file, all of them
//! sharing the one index behind [`ExcludingFile`](oxabl_index::ExcludingFile).
//!
//! # Byte spans only (KTD5)
//!
//! Nothing here bakes line/column. The language server's only correct position
//! oracle is the same `Rope` it uses for incremental sync, so a pipeline that
//! built its own `SourceMap` would hand it a second source of truth. Byte-offset
//! clients (CLI text output, the browser wire shape) share one position helper
//! instead.

use std::path::{Path, PathBuf};

use oxabl_analyze::{
    CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, ExpandedFile,
    collect_from_expanded, expand_source,
};
use oxabl_common::{
    Diagnostic, FileId, InternalPanic, catch_panic, panic_if_injected, panic_sites,
};
use oxabl_index::BatchIndex;
use oxabl_semantic::{Semantic, WorkspaceIndex};
use oxabl_workspace::FileSystem;

use crate::{PipelineConfig, ROOT_FILE_ID};

/// The preprocessed root buffer — the output of the first phase.
///
/// Wraps `oxabl_analyze`'s expansion *including its failure arm*: a fatal
/// preprocessing failure is an expansion outcome, not an error the caller has to
/// handle before it can ask a question. Every accessor answers on both arms
/// (empty dependencies, no text), and [`LintPipeline::collect`] turns the fatal
/// arm into a [`LintResult`] carrying the preprocessor's own diagnostics — the
/// same shape `collect_with_model` produces today.
///
/// `Clone`/`PartialEq`/`Eq` are derived on purpose: they are what let a client
/// memoize an expansion and get early cutoff when an edit leaves the expanded
/// text unchanged (KTD2).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expansion {
    inner: Result<ExpandedFile, Vec<Diagnostic>>,
}

impl Expansion {
    /// Absolute paths of every include file this expansion read — the watcher's
    /// changed-file → buffer matching input (R17). Empty when preprocessing was
    /// off or failed fatally.
    pub fn dependency_paths(&self) -> &[PathBuf] {
        match &self.inner {
            Ok(expanded) => expanded.dependency_paths(),
            Err(_) => &[],
        }
    }

    /// The transitively included [`FileId`]s, for invalidation keyed on file id
    /// rather than path. Empty on a fatal preprocessing failure.
    pub fn dependencies(&self) -> &[FileId] {
        match &self.inner {
            Ok(expanded) => expanded.dependencies(),
            Err(_) => &[],
        }
    }

    /// The expanded source text the lexer will see, or `None` when preprocessing
    /// failed fatally and there is no text to parse.
    pub fn text(&self) -> Option<&str> {
        self.expanded().map(|e| e.text.as_str())
    }

    /// Whether preprocessing failed fatally, meaning no parse was possible. The
    /// resulting [`LintResult`] still carries the preprocessor's diagnostics but
    /// no [`Semantic`] model — the distinction the CLI reports as its own exit
    /// code.
    pub fn preprocessing_failed(&self) -> bool {
        self.inner.is_err()
    }

    /// The underlying `oxabl_analyze` expansion, for a client that memoizes it
    /// directly. `None` on a fatal preprocessing failure.
    pub fn expanded(&self) -> Option<&ExpandedFile> {
        self.inner.as_ref().ok()
    }

    /// A fatally-failed expansion, for the test that pins how
    /// [`LintPipeline::collect`] maps that arm.
    ///
    /// White-box on purpose: the preprocessor only fails fatally when it emits an
    /// error *and* produces an empty span tree, and no input in this repo
    /// reaches that combination — the loud cases (unresolvable include, unclosed
    /// `&IF`) all still yield text. The arm is nonetheless live in the LSP and the
    /// CLI, which report it distinctly, so it is worth pinning even though only a
    /// fabricated expansion can get there.
    #[cfg(test)]
    fn fatally_failed(diagnostics: Vec<Diagnostic>) -> Self {
        Expansion {
            inner: Err(diagnostics),
        }
    }
}

/// One lint run's outcome, kept private so a [`LintResult`] cannot be built into
/// a contradictory state (diagnostics *and* a panic).
enum Outcome {
    /// The run completed. `semantic` is `None` only when preprocessing failed
    /// fatally, in which case `diagnostics` holds the loud preprocessor errors.
    ///
    /// The model is boxed so the failed arm does not pay for it: a `Semantic`
    /// carries several side tables and is by far the largest thing here, and this
    /// enum is returned by value from every run.
    Computed {
        diagnostics: CollectedDiagnostics,
        semantic: Option<Box<Semantic>>,
        dependency_paths: Vec<PathBuf>,
    },
    /// The run never produced a result: a contained panic (R3, R20).
    Failed(InternalPanic),
}

/// The result of a lint run: diagnostics in **byte spans**, the include
/// dependency paths, the optional semantic model, and whether the run happened
/// at all (R3).
///
/// Three states are deliberately distinct, because collapsing any pair of them
/// loses something a client reports differently:
///
/// | state | `failure()` | `semantic()` | meaning |
/// |---|---|---|---|
/// | clean | `None` | `Some` | ran, found nothing — the file is fine |
/// | fatal preprocessing | `None` | `None` | ran, no parse was possible |
/// | failed | `Some` | `None` | an internal panic; we never got to look |
///
/// A clean file and a file we never managed to analyze must not be the same
/// value: silence has to mean silence.
///
/// Cancellation is **not** a state here. It is the language server's concern and
/// is represented there as an absent result — the shared type must not learn
/// about salsa (KTD6).
///
/// Diagnostics keep their `labels` and `help` intact. The language server drops
/// both when it maps to `lsp_types::Diagnostic`, and that mapping is unchanged
/// for now, but the shared result carries them so surfacing them later is a
/// rendering change rather than a re-plumbing.
pub struct LintResult {
    outcome: Outcome,
}

impl LintResult {
    /// A completed run.
    fn computed(
        semantic: Option<Box<Semantic>>,
        diagnostics: CollectedDiagnostics,
        dependency_paths: Vec<PathBuf>,
    ) -> Self {
        LintResult {
            outcome: Outcome::Computed {
                diagnostics,
                semantic,
                dependency_paths,
            },
        }
    }

    /// A run contained by the panic guard (R20).
    fn failed(panic: InternalPanic) -> Self {
        LintResult {
            outcome: Outcome::Failed(panic),
        }
    }

    /// The contained panic, if the run failed.
    ///
    /// Carrying it — rather than reducing a failure to a bare flag — is what
    /// lets a client report *which* internal error happened; the CLI prints
    /// `analysis failed on <path>: <message>` from it.
    pub fn failure(&self) -> Option<&InternalPanic> {
        match &self.outcome {
            Outcome::Failed(panic) => Some(panic),
            Outcome::Computed { .. } => None,
        }
    }

    /// Whether the run failed rather than completing.
    pub fn failed_run(&self) -> bool {
        self.failure().is_some()
    }

    /// Whether the run completed but preprocessing failed fatally, so there is
    /// no model and the diagnostics are the preprocessor's alone.
    pub fn preprocessing_failed(&self) -> bool {
        matches!(&self.outcome, Outcome::Computed { semantic: None, .. })
    }

    /// Every diagnostic the run produced, root-resolved and byte-spanned. Empty
    /// on a failed run — check [`failure`](Self::failure) before reading silence
    /// as cleanliness.
    pub fn diagnostics(&self) -> &CollectedDiagnostics {
        static NONE: &CollectedDiagnostics = &CollectedDiagnostics {
            diagnostics: Vec::new(),
        };
        match &self.outcome {
            Outcome::Computed { diagnostics, .. } => diagnostics,
            Outcome::Failed(_) => NONE,
        }
    }

    /// Every diagnostic, in pipeline order.
    pub fn all(&self) -> impl Iterator<Item = &CollectedDiagnostic> {
        self.diagnostics().all()
    }

    /// Only the diagnostics a given stage produced — how a client routes
    /// preprocessor diagnostics to its own channel instead of dropping them.
    pub fn by_source(
        &self,
        source: DiagnosticSource,
    ) -> impl Iterator<Item = &CollectedDiagnostic> {
        self.diagnostics().by_source(source)
    }

    /// Every diagnostic *except* those from `source`, as an owned set.
    ///
    /// This is the shared form of a filter each client had written for itself:
    /// the CLI rebuilt a `CollectedDiagnostics` by hand to honor `--no-lint`,
    /// and the analyze envelope split preprocessor diagnostics out separately.
    /// One method, so the two cannot disagree about what "excluding" means.
    ///
    /// Delegates to [`CollectedDiagnostics::excluding_source`], which is the
    /// chainable form for a client that drops more than one stage.
    pub fn excluding_source(&self, source: DiagnosticSource) -> CollectedDiagnostics {
        self.diagnostics().excluding_source(source)
    }

    /// Take the diagnostics, for a client that needs to own them without
    /// cloning the set.
    ///
    /// The language server memoizes exactly this value per buffer and recomputes
    /// it per keystroke, so the difference between this and
    /// `diagnostics().clone()` is a whole diagnostic vector's worth of
    /// allocation on the hottest path in the product. Empty on a failed run,
    /// with the same caveat as [`diagnostics`](Self::diagnostics).
    pub fn into_diagnostics(self) -> CollectedDiagnostics {
        match self.outcome {
            Outcome::Computed { diagnostics, .. } => diagnostics,
            Outcome::Failed(_) => CollectedDiagnostics::default(),
        }
    }

    /// Whether the run produced any diagnostic. `false` on a failed run too, so
    /// pair it with [`failure`](Self::failure) when the question is "is this file
    /// clean?".
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics().diagnostics.is_empty()
    }

    /// The semantic model, for the clients that render more than diagnostics.
    /// `None` on a failed run and on a fatal preprocessing failure.
    pub fn semantic(&self) -> Option<&Semantic> {
        match &self.outcome {
            Outcome::Computed { semantic, .. } => semantic.as_deref(),
            Outcome::Failed(_) => None,
        }
    }

    /// Take the semantic model, for a client that needs to own it (the analyze
    /// dump borrows the model across its whole render).
    pub fn into_semantic(self) -> Option<Semantic> {
        match self.outcome {
            Outcome::Computed { semantic, .. } => semantic.map(|model| *model),
            Outcome::Failed(_) => None,
        }
    }

    /// Take both the model and the diagnostics in one move.
    ///
    /// [`into_semantic`](Self::into_semantic) and
    /// [`into_diagnostics`](Self::into_diagnostics) each consume the result, so a
    /// caller wanting both had to clone one of them. The `oxabl` umbrella's
    /// `analyze` surface returns exactly this tuple, and it is the browser's
    /// per-keystroke path, so that clone would have been a whole diagnostic vector
    /// per edit for no reason.
    ///
    /// A failed run yields `(None, empty)` — the same silence-is-not-cleanliness
    /// caveat as the individual accessors applies.
    pub fn into_parts(self) -> (Option<Semantic>, CollectedDiagnostics) {
        match self.outcome {
            Outcome::Computed {
                semantic,
                diagnostics,
                ..
            } => (semantic.map(|model| *model), diagnostics),
            Outcome::Failed(_) => (None, CollectedDiagnostics::default()),
        }
    }

    /// Absolute paths of the include files this run read (R17). Empty on a
    /// failed run.
    pub fn dependency_paths(&self) -> &[PathBuf] {
        match &self.outcome {
            Outcome::Computed {
                dependency_paths, ..
            } => dependency_paths,
            Outcome::Failed(_) => &[],
        }
    }
}

/// `Semantic` is not `Debug`, so the model is summarized rather than dumped —
/// enough for a test failure message to say which state a result is in.
impl std::fmt::Debug for LintResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.outcome {
            Outcome::Failed(panic) => f.debug_tuple("LintResult::Failed").field(panic).finish(),
            Outcome::Computed {
                diagnostics,
                semantic,
                dependency_paths,
            } => f
                .debug_struct("LintResult::Computed")
                .field("diagnostics", diagnostics)
                .field("semantic", &semantic.as_ref().map(|_| "<Semantic>"))
                .field("dependency_paths", dependency_paths)
                .finish(),
        }
    }
}

/// The shared lint run, constructed once from resolved configuration and reused
/// across many files or many edits of one buffer (R1).
///
/// Configuration and the filesystem are borrowed rather than owned: a
/// [`PipelineConfig`] holds a whole [`Schema`](oxabl_schema::Schema), and a
/// client that recomputes per keystroke must be able to stand this handle up
/// without cloning it.
pub struct LintPipeline<'a> {
    config: &'a PipelineConfig,
    fs: &'a dyn FileSystem,
    preprocess: bool,
    /// The run's cross-file index.
    index: RunIndex<'a>,
    /// The file being analysed, when the client knows which one that is.
    file: Option<PathBuf>,
}

/// Where a handle's index lives: owned by the handle that built it, borrowed
/// from the run handle by a per-file sibling, or supplied whole by a client with
/// a cache of its own.
///
/// The first two arms are what let "one index per run" and "one exclusion per
/// file" both be true. [`LintPipeline::new`] builds the
/// [`Owned`](RunIndex::Owned) arm; [`LintPipeline::with_file`] hands out siblings
/// on the [`Shared`](RunIndex::Shared) one, which is a pointer copy and reads
/// nothing.
///
/// [`External`](RunIndex::External) is the incremental client's arm. A language
/// server cannot use a [`BatchIndex`]: that cache grows for the life of the
/// value that owns it and never sees an edit, which is exactly wrong for a
/// process that outlives many edits to the files it indexed. Its own cache is
/// keyed and invalidated per file, and it arrives here as the trait object the
/// semantic layer already takes — so the *questions* stay shared (R7) while the
/// memoization differs, which is the whole shape of KTD2.
enum RunIndex<'a> {
    Owned(BatchIndex<'a>),
    Shared(&'a BatchIndex<'a>),
    External(&'a dyn WorkspaceIndex),
}

impl<'a> LintPipeline<'a> {
    /// A pipeline over `config`, reading include files through `fs`, with
    /// preprocessing **on** — the setting every client but the CLI's
    /// `--preprocess=false` escape hatch wants.
    ///
    /// This is the **run** handle, and it owns the run's cross-file index: build
    /// it once and reuse it, either across many edits of one buffer or — via
    /// [`with_file`](Self::with_file) — across every file of a walk.
    pub fn new(config: &'a PipelineConfig, fs: &'a dyn FileSystem) -> Self {
        LintPipeline {
            config,
            fs,
            preprocess: true,
            // The index reads nothing until a name is looked up (R6), so building
            // it here costs a pair of borrows and no I/O — which is what lets it
            // be unconditional rather than another thing a client can forget.
            index: RunIndex::Owned(BatchIndex::new(fs, &config.include_paths)),
            file: None,
        }
    }

    /// Turn preprocessing off, so the source is analyzed verbatim with an
    /// identity span mapping. Only the CLI's debugging toggle wants this; a
    /// formatter must never see expanded text, which is why the format pipeline
    /// has no such switch at all (KTD4).
    #[must_use]
    pub fn with_preprocess(mut self, preprocess: bool) -> Self {
        self.preprocess = preprocess;
        self
    }

    /// Tell the run's index which files this run already knows it will read.
    ///
    /// Only one cross-file question needs it: a `SHARED` name maps onto no path,
    /// so `shared_producer` can answer only from files the run has already
    /// indexed, and nothing pulls a producer in unless some file happens to `RUN`
    /// it. A walk already enumerated every file it is about to read, so handing
    /// that list over makes the producer link work on the command line without a
    /// directory scan or a whole-workspace walk — the list is the walk's own, and
    /// the index reads from it lazily, on the first `SHARED` lookup.
    ///
    /// A client with no such list (the language server, the browser) does not
    /// call this and behaves exactly as before. Apply it to the **run** handle,
    /// before any lookup: a per-file sibling shares the run's already-seeded
    /// index, so there is nothing left for it to seed.
    #[must_use]
    pub fn with_known_files(mut self, files: &'a [PathBuf]) -> Self {
        self.index = match self.index {
            RunIndex::Owned(index) => RunIndex::Owned(index.seeded_with(files)),
            // A sibling does not own the index it answers from, so it cannot
            // change what that index knows — and must not, since the run handle
            // has already told it. Explicit arm rather than a catch-all so a
            // future third arm is a compile error here.
            RunIndex::Shared(index) => RunIndex::Shared(index),
            // A supplied index owns its own file set — the language server
            // learns about a file when a lookup reaches it, and has no walk to
            // enumerate. Explicit arm rather than a catch-all so a future
            // fourth arm is a compile error here.
            RunIndex::External(index) => RunIndex::External(index),
        };
        self
    }

    /// Answer this run's cross-file questions from `index` instead of building a
    /// batch cache.
    ///
    /// For a client that memoizes across edits rather than across a walk: the
    /// language server's salsa-backed index, whose entries are keyed and
    /// invalidated per file. The questions are the same four — that is the point
    /// of handing over a [`WorkspaceIndex`] rather than a narrower handle.
    ///
    /// **The supplied index owns its own self-exclusion.** A batch index is
    /// excluded through [`with_file`](Self::with_file), which needs the memo to
    /// have already minted an id for the analysed path; a client with its own
    /// cache already knows which file it is analysing and can decline earlier and
    /// more directly. So this arm applies no exclusion of its own, and
    /// [`file`](Self::file) is not consulted for it.
    #[must_use]
    pub fn with_index(mut self, index: &'a dyn WorkspaceIndex) -> Self {
        self.index = RunIndex::External(index);
        self
    }

    /// A handle for analysing the file at `path`, sharing this run's index.
    ///
    /// The identity does exactly one thing today: it excludes `path` from its own
    /// cross-file lookups, so a buffer cannot inherit from — or be linked to as
    /// the `SHARED` producer of — its own copy on disk, which for an editor is a
    /// stale revision of the very text being analysed.
    ///
    /// # Why this borrows instead of consuming
    ///
    /// Unlike [`with_preprocess`](Self::with_preprocess) this is not a
    /// configuration tweak on one handle; it is how a walk asks the *same* run,
    /// with its one index and its one memo, about the next file. Consuming
    /// `self` would force a walk to rebuild the index per file, and re-reading
    /// every shared dependency once per file is the cost the index exists to
    /// avoid. Single-file clients simply bind the run handle first:
    ///
    /// ```ignore
    /// let run = LintPipeline::new(&config, &fs);
    /// let result = run.with_file(path).run(&source);
    /// ```
    #[must_use]
    pub fn with_file(&'a self, path: impl Into<PathBuf>) -> LintPipeline<'a> {
        LintPipeline {
            config: self.config,
            fs: self.fs,
            preprocess: self.preprocess,
            index: match &self.index {
                // A sibling shares the run's memo rather than building a second
                // one — that is what makes the shared-dependency dedup pay.
                RunIndex::Owned(index) => RunIndex::Shared(index),
                RunIndex::Shared(index) => RunIndex::Shared(index),
                // A supplied index is already shared by construction, and it
                // applies its own exclusion, so the identity below is recorded
                // but not consulted for this arm.
                RunIndex::External(index) => RunIndex::External(*index),
            },
            file: Some(path.into()),
        }
    }

    /// The configuration this pipeline runs under.
    pub fn config(&self) -> &PipelineConfig {
        self.config
    }

    /// Whether this pipeline preprocesses.
    pub fn preprocess(&self) -> bool {
        self.preprocess
    }

    /// The file being analysed, or `None` when the client has no path for its
    /// buffer.
    pub fn file(&self) -> Option<&Path> {
        self.file.as_deref()
    }

    /// Run `with` against this handle's index, viewed as the file being analysed.
    ///
    /// The three arms differ only in where the index lives and who applies the
    /// self-exclusion, and every one of them ends up as the same `&dyn
    /// WorkspaceIndex` the semantic layer takes. Written as one closure applied
    /// three ways rather than three copies of the collect call, so a change to
    /// what `collect` passes cannot be made in two arms and forgotten in the
    /// third.
    fn with_run_index<T>(&self, with: impl FnOnce(&dyn WorkspaceIndex) -> T) -> T {
        match &self.index {
            // The exclusion is applied here, per handle, while the memo behind it
            // stays the run's. With no identity nothing is excluded, which is the
            // browser's case.
            RunIndex::Owned(index) => with(&index.excluding(self.file())),
            RunIndex::Shared(index) => with(&index.excluding(self.file())),
            // A supplied index excludes the analysed file itself — see
            // [`with_index`](Self::with_index).
            RunIndex::External(index) => with(*index),
        }
    }

    /// Phase one: preprocess `source` into an [`Expansion`].
    ///
    /// **Unguarded on purpose** (KTD6): a caller that runs this inside a
    /// cancellation catch needs the unwind to reach it. See the module docs.
    pub fn expand(&self, source: &str) -> Expansion {
        // The test-only injection seam (inert unless a dev-dependency enables
        // `oxabl_common/test-panics`). It sits here, in the unguarded phase that
        // `run` also calls, so one marker exercises both halves of KTD6: a panic
        // through `run` must be contained, and the same panic through `expand`
        // must escape.
        panic_if_injected(panic_sites::ANALYZE, source);
        Expansion {
            inner: expand_source(
                ROOT_FILE_ID,
                source,
                self.fs,
                &self.config.include_paths,
                self.preprocess,
            ),
        }
    }

    /// Phase two: parse, analyze, and lint an [`Expansion`] into a
    /// [`LintResult`].
    ///
    /// **Unguarded on purpose** (KTD6), for the same reason as
    /// [`expand`](Self::expand).
    pub fn collect(&self, expansion: &Expansion) -> LintResult {
        let dependency_paths = expansion.dependency_paths().to_vec();
        match &expansion.inner {
            Ok(expanded) => {
                let (semantic, diagnostics) = self.with_run_index(|index| {
                    collect_from_expanded(
                        expanded,
                        &self.config.schema,
                        self.config.schema_loaded,
                        &self.config.lint_severities,
                        index,
                    )
                });
                LintResult::computed(semantic.map(Box::new), diagnostics, dependency_paths)
            }
            // Fatal preprocessing failure: no model, and the preprocessor's own
            // loud errors are the whole diagnostic set.
            Err(preproc_errors) => {
                let diagnostics = CollectedDiagnostics {
                    diagnostics: preproc_errors
                        .iter()
                        .map(|d| CollectedDiagnostic {
                            diagnostic: d.clone(),
                            source: DiagnosticSource::Preproc,
                        })
                        .collect(),
                };
                LintResult::computed(None, diagnostics, dependency_paths)
            }
        }
    }

    /// Both phases, with panic containment — for the clients that have no
    /// incremental needs (the CLI and the browser).
    ///
    /// A contained panic becomes [`LintResult::failed_run`] carrying the message,
    /// never an unwind past the caller. The guard is
    /// [`oxabl_common::catch_panic`], called directly (KTD6, R20).
    ///
    /// # Platform caveat
    ///
    /// `catch_panic` is a documented pass-through on `wasm32-unknown-unknown`,
    /// where stable Rust builds `-Cpanic=abort`. In the browser this method is
    /// therefore *unprotected*; that client's recovery is a panic hook plus
    /// instance reset, which lives outside this crate.
    pub fn run(&self, source: &str) -> LintResult {
        match catch_panic(|| {
            let expansion = self.expand(source);
            self.collect(&expansion)
        }) {
            Ok(result) => result,
            Err(panic) => LintResult::failed(panic),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_common::{FileSpan, LintSeverityMap, Severity};
    use oxabl_schema::Schema;
    use oxabl_schema::test_support::customer_schema;
    use oxabl_workspace::InMemoryFileSystem;
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex};

    /// Spelled out rather than imported: this crate does not depend on
    /// `oxabl_lint` directly — it reaches the rules through `oxabl_analyze` — and
    /// a dev-dependency edge added for one string in one assertion would be a
    /// worse trade than the literal.
    const LINT0004: &str = "LINT0004";

    /// Silence the default panic hook for the duration of a deliberately
    /// panicking test, so a green run does not print a backtrace.
    fn quietly<T>(f: impl FnOnce() -> T) -> T {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let out = f();
        std::panic::set_hook(previous);
        out
    }

    fn codes(result: &LintResult) -> Vec<&str> {
        result.all().map(|d| d.diagnostic.code.0).collect()
    }

    #[test]
    fn unused_variable_fires_with_the_collectors_code_and_span() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let result = LintPipeline::new(&config, &fs).run("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");

        let d = result
            .all()
            .find(|d| d.diagnostic.code.0 == "LINT0002")
            .unwrap_or_else(|| panic!("expected LINT0002, got {:?}", codes(&result)));
        assert_eq!(d.source, DiagnosticSource::Lint);
        assert_eq!(d.diagnostic.severity, Severity::Warning);
        // Byte span over `x` in `DEFINE VARIABLE x ...` (KTD5: no line/column).
        assert_eq!(d.diagnostic.span.file, ROOT_FILE_ID);
        assert_eq!(d.diagnostic.span.span.start, 16);
        assert_eq!(d.diagnostic.span.span.end, 17);
    }

    // Error recovery survives the wrapping: a parse error must not abort the run.
    #[test]
    fn parse_error_still_yields_semantic_and_lint_diagnostics() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let src =
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n@ @ @\nDEFINE VARIABLE y AS INTEGER NO-UNDO.\n";
        let result = LintPipeline::new(&config, &fs).run(src);

        let cs = codes(&result);
        assert!(
            cs.contains(&"PARSE001"),
            "expected a parse error, got {cs:?}"
        );
        assert!(cs.contains(&"LINT0002"), "expected lint anyway, got {cs:?}");
        assert!(!result.failed_run(), "recovery is not a failed run");
        assert!(result.semantic().is_some(), "model must still be built");
    }

    // The loud unresolvable-include warning must be reachable by source, not
    // silently folded into the general set.
    #[test]
    fn unresolvable_include_surfaces_preproc007_through_source_filtering() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let result = LintPipeline::new(&config, &fs).run("{missing.i}\nMESSAGE \"hi\".\n");

        assert!(
            result
                .by_source(DiagnosticSource::Preproc)
                .any(|d| d.diagnostic.code.0 == "PREPROC007"),
            "expected PREPROC007 under the preproc source, got {:?}",
            codes(&result)
        );
        // And the same filter can take it back out again — the shared form of
        // the CLI's hand-rolled `--no-lint` rebuild.
        let without = result.excluding_source(DiagnosticSource::Preproc);
        assert!(
            !without.all().any(|d| d.diagnostic.code.0 == "PREPROC007"),
            "excluding_source must drop the preproc arm"
        );
    }

    // A diagnostic whose origin is inside an expanded include is dropped;
    // root-origin spans are untouched.
    #[test]
    fn include_origin_diagnostics_dropped_root_spans_unchanged() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            "/proj/defs.i".into(),
            "DEFINE VARIABLE onlyInInclude AS INTEGER NO-UNDO.\n",
        );
        let config = PipelineConfig {
            include_paths: vec!["/proj".into()],
            ..PipelineConfig::default()
        };
        let src = "{defs.i}\nDEFINE VARIABLE rootOnly AS INTEGER NO-UNDO.\n";
        let result = LintPipeline::new(&config, &fs).run(src);

        let unused: Vec<_> = result
            .all()
            .filter(|d| d.diagnostic.code.0 == "LINT0002")
            .collect();
        assert_eq!(
            unused.len(),
            1,
            "only the root-origin unused variable survives, got {:?}",
            codes(&result)
        );
        // `rootOnly` starts at byte 25 of the *root* text, proving the span came
        // back through the virtual→real mapping rather than staying virtual.
        assert_eq!(unused[0].diagnostic.span.span.start, 25);
        assert_eq!(&src[25..33], "rootOnly");
    }

    #[test]
    fn expand_then_collect_matches_run() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            "/proj/defs.i".into(),
            "DEFINE VARIABLE fromInclude AS INTEGER NO-UNDO.\n",
        );
        let config = PipelineConfig {
            include_paths: vec!["/proj".into()],
            ..PipelineConfig::default()
        };
        let pipeline = LintPipeline::new(&config, &fs);
        let src = "{defs.i}\nfromInclude = 5.\nDEFINE VARIABLE unusedOne AS INTEGER NO-UNDO.\n";

        let two_phase = pipeline.collect(&pipeline.expand(src));
        let one_shot = pipeline.run(src);

        assert_eq!(two_phase.diagnostics(), one_shot.diagnostics());
        assert_eq!(two_phase.dependency_paths(), one_shot.dependency_paths());
        assert_eq!(two_phase.failed_run(), one_shot.failed_run());
        assert_eq!(
            two_phase.semantic().is_some(),
            one_shot.semantic().is_some()
        );
    }

    #[test]
    fn dependency_paths_list_an_include_the_source_pulls_in() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert("/proj/defs.i".into(), "MESSAGE \"from include\".\n");
        let config = PipelineConfig {
            include_paths: vec!["/proj".into()],
            ..PipelineConfig::default()
        };
        let pipeline = LintPipeline::new(&config, &fs);
        let src = "{defs.i}\nMESSAGE \"root\".\n";

        let expansion = pipeline.expand(src);
        assert!(
            expansion
                .dependency_paths()
                .iter()
                .any(|p| p.ends_with("defs.i")),
            "expected the include among {:?}",
            expansion.dependency_paths()
        );
        assert!(!expansion.dependencies().is_empty());
        // And the result carries them forward, so a client need not keep the
        // expansion alive just to answer the watcher.
        assert_eq!(
            pipeline.collect(&expansion).dependency_paths(),
            expansion.dependency_paths()
        );
    }

    #[test]
    fn per_rule_severity_override_changes_the_emitted_severity() {
        let fs = InMemoryFileSystem::new();
        let src = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";

        let mut off = LintSeverityMap::new();
        off.set("LINT0002", None);
        let silenced = PipelineConfig {
            lint_severities: off,
            ..PipelineConfig::default()
        };
        let result = LintPipeline::new(&silenced, &fs).run(src);
        assert!(
            !codes(&result).contains(&"LINT0002"),
            "an off rule must not fire, got {:?}",
            codes(&result)
        );

        let mut info = LintSeverityMap::new();
        info.set("LINT0002", Some(Severity::Info));
        let remapped = PipelineConfig {
            lint_severities: info,
            ..PipelineConfig::default()
        };
        let result = LintPipeline::new(&remapped, &fs).run(src);
        let d = result
            .all()
            .find(|d| d.diagnostic.code.0 == "LINT0002")
            .expect("LINT0002 present");
        assert_eq!(d.diagnostic.severity, Severity::Info);
    }

    #[test]
    fn schema_dependent_diagnostics_are_dark_without_a_schema_and_live_with_one() {
        let fs = InMemoryFileSystem::new();
        let src = "FIND FIRST Customer.\nMESSAGE Customer.NoSuchField.\n";

        let unloaded = PipelineConfig::default();
        assert!(!unloaded.schema_loaded);
        let dark = LintPipeline::new(&unloaded, &fs).run(src);
        assert!(
            !codes(&dark).contains(&"LINT0003"),
            "LINT0003 must stay silent with no schema, got {:?}",
            codes(&dark)
        );

        let loaded = PipelineConfig {
            schema: customer_schema(),
            schema_loaded: true,
            ..PipelineConfig::default()
        };
        let live = LintPipeline::new(&loaded, &fs).run(src);
        assert!(
            codes(&live).contains(&"LINT0003"),
            "LINT0003 must fire under a loaded schema, got {:?}",
            codes(&live)
        );
    }

    #[test]
    fn preprocess_off_analyzes_the_source_verbatim() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        // With preprocessing off the unresolvable include is not even attempted,
        // so no PREPROC007 — the identity-mapping path.
        let result = LintPipeline::new(&config, &fs)
            .with_preprocess(false)
            .run("{missing.i}\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
        assert!(
            !codes(&result).contains(&"PREPROC007"),
            "preprocessing was off, got {:?}",
            codes(&result)
        );
        assert!(result.dependency_paths().is_empty());
    }

    #[test]
    fn a_clean_source_is_computed_empty_not_failed() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let result = LintPipeline::new(&config, &fs).run("MESSAGE \"hello\".\n");

        assert!(!result.has_diagnostics(), "got {:?}", codes(&result));
        assert!(!result.failed_run(), "a clean file is not a failed run");
        assert!(result.failure().is_none());
        assert!(
            result.semantic().is_some(),
            "a clean run still carries a model"
        );
        assert!(!result.preprocessing_failed());
    }

    // The `None`-model arm stays distinguishable from both a clean run and a
    // failed one: the CLI reports each with a different exit code. Driven from a
    // fabricated expansion because no real input reaches the preprocessor's fatal
    // combination — see `Expansion::fatally_failed`.
    #[test]
    fn fatal_preprocessing_failure_is_computed_without_a_model() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let pipeline = LintPipeline::new(&config, &fs);

        let expansion = Expansion::fatally_failed(vec![Diagnostic::error(
            "PREPROC001",
            "expansion gave up".to_string(),
            FileSpan {
                file: ROOT_FILE_ID,
                span: oxabl_ast::Span { start: 0, end: 1 },
            },
        )]);
        assert!(expansion.preprocessing_failed());
        assert!(expansion.text().is_none());
        assert!(expansion.dependency_paths().is_empty());
        assert!(expansion.dependencies().is_empty());

        let result = pipeline.collect(&expansion);
        assert!(!result.failed_run(), "a preproc failure is not a panic");
        assert!(result.preprocessing_failed());
        assert!(result.semantic().is_none());
        assert!(result.has_diagnostics(), "the preproc errors are carried");
        assert!(
            result.all().all(|d| d.source == DiagnosticSource::Preproc),
            "only preprocessor diagnostics survive a fatal expansion"
        );
    }

    // R20/KTD6: the guard is on `run`, and it reports rather than unwinds.
    #[test]
    fn a_panic_inside_run_is_reported_as_a_failed_run() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let src = "/* OXABL-TEST-PANIC:analyze */\nMESSAGE \"hi\".\n";

        let result = quietly(|| LintPipeline::new(&config, &fs).run(src));

        assert!(result.failed_run(), "the guard must contain the panic");
        assert!(
            !result.has_diagnostics(),
            "a failed run reports no findings"
        );
        assert!(result.semantic().is_none());
        // The message survives, so the CLI's `analysis failed on <path>:
        // <message>` line has something to print.
        let panic = result.failure().expect("failure carried");
        assert!(
            panic.message().contains("injected test panic at analyze"),
            "got {panic}"
        );
        assert!(panic.to_string().contains("internal error"));
    }

    // KTD6's load-bearing half: the two phases stay raw, so the language server
    // can put its own guard *outside* its salsa cancellation catch.
    #[test]
    fn a_panic_inside_expand_propagates_to_the_caller() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let src = "/* OXABL-TEST-PANIC:analyze */\nMESSAGE \"hi\".\n";

        let escaped = quietly(|| {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                LintPipeline::new(&config, &fs).expand(src)
            }))
        });
        assert!(
            escaped.is_err(),
            "expand must not swallow an unwind — cancellation travels this way"
        );
    }

    #[test]
    fn a_failed_run_is_not_the_same_value_as_a_clean_one() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let pipeline = LintPipeline::new(&config, &fs);

        let clean = pipeline.run("MESSAGE \"hi\".\n");
        let failed = quietly(|| pipeline.run("/* OXABL-TEST-PANIC:analyze */\nMESSAGE \"hi\".\n"));

        assert_eq!(clean.has_diagnostics(), failed.has_diagnostics());
        assert_ne!(
            clean.failed_run(),
            failed.failed_run(),
            "the two must be distinguishable despite both being diagnostic-free"
        );
    }

    // The owned accessor must agree with the borrowing one on both a completed
    // and a failed run — it is the language server's per-keystroke path, so a
    // divergence there would be invisible in the CLI and wrong in the editor.
    #[test]
    fn into_diagnostics_matches_the_borrowed_set_and_is_empty_on_failure() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig::default();
        let pipeline = LintPipeline::new(&config, &fs);

        let result = pipeline.run("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
        let borrowed = result.diagnostics().clone();
        assert!(!borrowed.diagnostics.is_empty());
        assert_eq!(result.into_diagnostics(), borrowed);

        let failed = quietly(|| pipeline.run("/* OXABL-TEST-PANIC:analyze */\nMESSAGE \"hi\".\n"));
        assert!(failed.failed_run());
        assert!(failed.into_diagnostics().diagnostics.is_empty());
    }

    // -----------------------------------------------------------------------
    // Cross-file resolution through the shared run (KTD7)
    // -----------------------------------------------------------------------

    /// A parent class declaring one public method with a return type. Synthetic.
    const CALC_BASE: &str = r#"CLASS orders.calc-base:
    METHOD PUBLIC INTEGER calc-total():
        RETURN 0.
    END METHOD.
END CLASS."#;

    /// Where a `/src` include-path entry makes the batch index look for it: a
    /// qualified name maps onto a relative path by replacing dots with separators.
    const CALC_BASE_PATH: &str = "/src/orders/calc-base.cls";

    /// A subclass calling the inherited method. Without cross-file resolution the
    /// call is an `undefined-symbol` finding — the false positive this whole line
    /// of work removes — which is what makes resolution observable from out here.
    const CHILD: &str = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
        MESSAGE v-total.
    END METHOD.
END CLASS."#;

    /// A second subclass of the same parent, so "read the parent once per run" has
    /// two askers.
    const OTHER_CHILD: &str = r#"CLASS orders.other-child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-sum AS INTEGER NO-UNDO.
        v-sum = calc-total().
        MESSAGE v-sum.
    END METHOD.
END CLASS."#;

    fn workspace(files: &[(&str, &str)]) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for (path, contents) in files {
            fs.insert(PathBuf::from(path), *contents);
        }
        fs
    }

    fn searching(paths: &[&str]) -> PipelineConfig {
        PipelineConfig {
            include_paths: paths.iter().map(PathBuf::from).collect(),
            ..PipelineConfig::default()
        }
    }

    /// A filesystem that counts reads per path, so "the parent is read once per
    /// run" is asserted against real I/O rather than against index internals.
    struct CountingFs {
        inner: InMemoryFileSystem,
        reads: Mutex<HashMap<PathBuf, usize>>,
    }

    impl CountingFs {
        fn new(files: &[(&str, &str)]) -> Self {
            CountingFs {
                inner: workspace(files),
                reads: Mutex::new(HashMap::new()),
            }
        }

        fn reads_of(&self, path: &str) -> usize {
            self.reads
                .lock()
                .unwrap()
                .get(&PathBuf::from(path))
                .copied()
                .unwrap_or(0)
        }
    }

    impl FileSystem for CountingFs {
        fn read(&self, path: &Path) -> Result<Arc<str>, std::io::Error> {
            *self
                .reads
                .lock()
                .unwrap()
                .entry(path.to_path_buf())
                .or_insert(0) += 1;
            self.inner.read(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }
    }

    #[test]
    fn a_parents_member_resolves_through_the_composed_run() {
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs);

        let result = run.with_file("/src/orders/child.cls").run(CHILD);
        assert!(
            !codes(&result).contains(&"LINT0001"),
            "the inherited call must resolve through the run's index, got {:?}",
            codes(&result)
        );
    }

    #[test]
    fn the_two_phase_run_gives_the_identical_cross_file_answer() {
        // The property the parity suite depends on: attaching an index must not
        // make the composed convenience and the incremental pair disagree.
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs);
        let pipeline = run.with_file("/src/orders/child.cls");

        let two_phase = pipeline.collect(&pipeline.expand(CHILD));
        let one_shot = pipeline.run(CHILD);

        assert_eq!(two_phase.diagnostics(), one_shot.diagnostics());
        assert_eq!(two_phase.dependency_paths(), one_shot.dependency_paths());
        assert_eq!(two_phase.failed_run(), one_shot.failed_run());
        assert!(!codes(&two_phase).contains(&"LINT0001"));
    }

    #[test]
    fn with_no_file_identity_resolution_still_happens_and_the_file_can_be_itself() {
        // The browser's position: no path for the buffer, so nothing is excluded.
        // Resolution must still work (first half), and the run must not need an
        // identity to produce an answer (second half).
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = searching(&["/src"]);
        let anonymous = LintPipeline::new(&config, &fs);
        assert_eq!(anonymous.file(), None);

        let result = anonymous.run(CHILD);
        assert!(
            !codes(&result).contains(&"LINT0001"),
            "an identity-less run resolves cross-file too, got {:?}",
            codes(&result)
        );

        // And with no identity the file on disk that *is* the buffer is just
        // another workspace file: analysing the parent's own text against a
        // workspace containing it resolves the same way, because nothing has been
        // told to exclude it. This is the control for the exclusion test below.
        let sub_of_itself = r#"CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
        MESSAGE v-total.
    END METHOD.
END CLASS."#;
        let unexcluded = anonymous.run(sub_of_itself);
        assert!(!codes(&unexcluded).contains(&"LINT0001"));
    }

    #[test]
    fn a_file_is_excluded_from_its_own_class_lookup() {
        // The buffer being analysed *is* `/src/orders/calc-base.cls` — an editor
        // showing unsaved edits, or the CLI having just read those bytes — and the
        // buffer now declares a subclass of the class that file used to hold.
        // Resolving `orders.calc-base` to that same file would inherit from the
        // file's own stale copy, so the identity has to shut the lookup out.
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs);

        let itself = run.with_file(CALC_BASE_PATH);
        assert_eq!(itself.file(), Some(Path::new(CALC_BASE_PATH)));
        let excluded = itself.run(CHILD);
        assert!(
            codes(&excluded).contains(&"LINT0001"),
            "the analysed file must not resolve a class to itself, got {:?}",
            codes(&excluded)
        );

        // Any *other* file's handle, off the same run and the same index, still
        // resolves it — the exclusion is per asking file, not a state change.
        let neighbour = run.with_file("/src/orders/child.cls");
        assert!(!codes(&neighbour.run(CHILD)).contains(&"LINT0001"));
    }

    #[test]
    fn two_files_sharing_a_parent_read_it_once_per_run() {
        // What makes one index per run worth the plumbing. Both children are
        // analysed through per-file handles off one run handle; the parent is read
        // on the first lookup and answered from the memo thereafter.
        let fs = CountingFs::new(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs);

        let first = run.with_file("/src/orders/child.cls").run(CHILD);
        let second = run
            .with_file("/src/orders/other-child.cls")
            .run(OTHER_CHILD);

        assert!(!codes(&first).contains(&"LINT0001"));
        assert!(!codes(&second).contains(&"LINT0001"));
        assert_eq!(
            fs.reads_of(CALC_BASE_PATH),
            1,
            "the shared parent is indexed once for the run, not once per file"
        );
    }

    #[test]
    fn an_empty_include_path_list_resolves_nothing_cross_file() {
        // Nowhere to search, so the answer must be exactly the single-file one:
        // the inherited call is undefined, as it has always been.
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let config = PipelineConfig::default();
        assert!(config.include_paths.is_empty());
        let run = LintPipeline::new(&config, &fs);

        let result = run.with_file("/src/orders/child.cls").run(CHILD);
        assert!(
            codes(&result).contains(&"LINT0001"),
            "with no search path the parent is unreachable, got {:?}",
            codes(&result)
        );
        // And the whole set matches the no-index collect, not just that one code.
        let (_model, baseline) = oxabl_analyze::collect_with_model(
            ROOT_FILE_ID,
            CHILD,
            &fs,
            &[],
            &config.schema,
            config.schema_loaded,
            &config.lint_severities,
            true,
        );
        assert_eq!(
            result.diagnostics(),
            &baseline,
            "an index with nowhere to look must answer identically to no index"
        );
    }

    // What an index adds, checked where it matters: the index is live in a real
    // client here, driving the same pipeline every client drives. It used to
    // assert that attaching one added nothing; now that a cross-file type reaches
    // the lattice, the contract is the *enumeration* — a mismatched assignment
    // through an inherited member gains a LINT0004, and every other shape still
    // gains nothing. Removals stay unchecked on purpose: an `undefined-symbol`
    // disappearing from an inherited member is the pre-existing false positive
    // this work fixes.
    #[test]
    fn attaching_an_index_adds_exactly_the_enumerated_diagnostics() {
        let fs = workspace(&[
            (CALC_BASE_PATH, CALC_BASE),
            (
                "/src/init-globals.p",
                "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n",
            ),
            (
                "/src/orders/i-calc.cls",
                "INTERFACE orders.i-calc: END INTERFACE.",
            ),
        ]);
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs);

        // Every shape the index can influence: an inherited member, a typed
        // assignment from one, a private member, a `USING`, a `NEW`, an
        // implemented interface, a literal `RUN`, a `SHARED` consumer, and a
        // plain misspelling.
        let sources: [(&str, &[&str]); 9] = [
            (CHILD, &[]),
            // The one judged shape: an inherited `INTEGER` method assigned into a
            // `LOGICAL`. Silent without an index because the call resolves to
            // nothing at all; a type mismatch with one.
            (
                "CLASS orders.child INHERITS orders.calc-base:\n\
             METHOD PUBLIC VOID run-it():\n\
             DEFINE VARIABLE v-flag AS LOGICAL NO-UNDO.\n\
             v-flag = calc-total().\n\
             MESSAGE v-flag.\n\
             END METHOD.\n\
             END CLASS.",
                &[LINT0004],
            ),
            (
                "CLASS orders.child INHERITS orders.calc-base:\n\
             METHOD PUBLIC VOID run-it():\n\
             DEFINE VARIABLE v-n AS INTEGER NO-UNDO.\n\
             v-n = calc-totl().\n\
             MESSAGE v-n.\n\
             END METHOD.\n\
             END CLASS.",
                &[],
            ),
            ("USING orders.calc-base.\nMESSAGE \"hi\".\n", &[]),
            (
                "DEFINE VARIABLE v-obj AS CLASS orders.calc-base NO-UNDO.\n\
             v-obj = NEW orders.calc-base().\n\
             MESSAGE v-obj:calc-total().\n",
                &[],
            ),
            (
                "CLASS orders.impl IMPLEMENTS orders.i-calc: END CLASS.",
                &[],
            ),
            ("RUN init-globals.p.\n", &[]),
            (
                "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
             RUN init-globals.p.\n\
             MESSAGE v-site-code.\n",
                &[],
            ),
            ("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n", &[]),
        ];

        for (source, expected_added) in sources {
            let with = run.with_file("/src/orders/child.cls").run(source);
            let (_model, without) = oxabl_analyze::collect_with_model(
                ROOT_FILE_ID,
                source,
                &fs,
                &config.include_paths,
                &config.schema,
                config.schema_loaded,
                &config.lint_severities,
                true,
            );
            let mut available: Vec<_> = without.all().collect();
            let mut added = Vec::new();
            for found in with.all() {
                match available.iter().position(|baseline| *baseline == found) {
                    Some(i) => {
                        available.swap_remove(i);
                    }
                    None => added.push(found.diagnostic.code.0),
                }
            }
            added.sort_unstable();
            assert_eq!(
                added,
                expected_added.to_vec(),
                "the findings an index adds for:\n{source}"
            );
        }
    }

    // The seeded file set (R6, without a scan): the walk's own list is what lets a
    // `SHARED` consumer find its producer when nothing `RUN`s the producing file.
    #[test]
    fn a_seeded_walk_links_a_shared_consumer_to_a_producer_no_run_pulled_in() {
        let fs = workspace(&[
            (
                "/src/init-globals.p",
                "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n",
            ),
            (
                "/src/report.p",
                "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n",
            ),
        ]);
        let config = searching(&["/src"]);
        let known = vec![
            PathBuf::from("/src/init-globals.p"),
            PathBuf::from("/src/report.p"),
        ];
        // No `RUN` anywhere in the consumer — the seed is the only thing that can
        // make the producer visible.
        let consumer = "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
                        MESSAGE v-site-code.\n";

        let seeded = LintPipeline::new(&config, &fs).with_known_files(&known);
        let result = seeded.with_file("/src/report.p").run(consumer);
        assert!(
            shared_producer_named(&result, "v-site-code").is_some(),
            "the walk supplied both files, so the producer link resolves"
        );

        // Without the seed the same run cannot know the producer exists, which is
        // the behavior every client that supplies no file list keeps.
        let unseeded = LintPipeline::new(&config, &fs);
        let result = unseeded.with_file("/src/report.p").run(consumer);
        assert!(
            shared_producer_named(&result, "v-site-code").is_none(),
            "nothing pulled the producing file in"
        );
    }

    /// The indexed file a `SHARED` symbol named `name` was linked to, if any.
    fn shared_producer_named(
        result: &LintResult,
        name: &str,
    ) -> Option<oxabl_semantic::IndexedFileId> {
        let sem = result.semantic().expect("the run produced a model");
        sem.symbols
            .iter()
            .find(|(_, symbol)| &*symbol.name == name)
            .and_then(|(id, _)| sem.symbols.shared_producer(id))
    }

    /// The incremental client's arm: a supplied index is the one consulted, and
    /// the batch the handle would otherwise have built is not.
    ///
    /// Proven by making the two *disagree*. The pipeline's own configuration has an
    /// empty include-path list, so a batch index built from it can find nothing;
    /// the supplied index searches `/src` and does. A run that resolves the
    /// inherited call therefore cannot have used the handle's own index.
    #[test]
    fn a_supplied_index_answers_instead_of_the_handles_own() {
        let fs = workspace(&[(CALC_BASE_PATH, CALC_BASE)]);
        let searchable = vec![PathBuf::from("/src")];
        let supplied = oxabl_index::BatchIndex::new(&fs, &searchable);

        let nowhere = PipelineConfig::default();
        assert!(nowhere.include_paths.is_empty());

        // The control: without the supplied index this exact configuration cannot
        // resolve the parent — that is `an_empty_include_path_list_resolves_nothing`.
        let unaided = LintPipeline::new(&nowhere, &fs);
        assert!(codes(&unaided.run(CHILD)).contains(&"LINT0001"));

        let aided = LintPipeline::new(&nowhere, &fs).with_index(&supplied);
        let result = aided.run(CHILD);
        assert!(
            !codes(&result).contains(&"LINT0001"),
            "the supplied index must be the one consulted, got {:?}",
            codes(&result)
        );

        // And a per-file sibling keeps answering from it rather than falling back
        // to a fresh batch — which is what the language server relies on, since its
        // index is the only one that knows anything.
        let per_file = aided.with_file("/src/orders/child.cls");
        assert!(!codes(&per_file.run(CHILD)).contains(&"LINT0001"));
        // The supplied index owns its own exclusion, so recording the identity does
        // not make the handle apply one on its behalf.
        assert_eq!(per_file.file(), Some(Path::new("/src/orders/child.cls")));
    }

    #[test]
    fn a_per_file_handle_inherits_the_runs_configuration() {
        let fs = InMemoryFileSystem::new();
        let config = searching(&["/src"]);
        let run = LintPipeline::new(&config, &fs).with_preprocess(false);

        let per_file = run.with_file("/src/thing.p");
        assert!(
            !per_file.preprocess(),
            "a sibling must not silently re-enable preprocessing"
        );
        assert_eq!(per_file.config().include_paths, config.include_paths);
        // And the toggle still composes the other way round.
        assert!(
            run.with_file("/src/thing.p")
                .with_preprocess(true)
                .preprocess()
        );
    }

    #[test]
    fn the_pipeline_is_reusable_across_runs() {
        let fs = InMemoryFileSystem::new();
        let config = PipelineConfig {
            schema: Schema::empty(),
            ..PipelineConfig::default()
        };
        let pipeline = LintPipeline::new(&config, &fs);
        assert!(pipeline.preprocess());

        let first = pipeline.run("DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
        let second = pipeline.run("DEFINE VARIABLE b AS INTEGER NO-UNDO.\n");
        assert!(codes(&first).contains(&"LINT0002"));
        assert!(codes(&second).contains(&"LINT0002"));
        assert!(!pipeline.config().schema_loaded);
    }
}
