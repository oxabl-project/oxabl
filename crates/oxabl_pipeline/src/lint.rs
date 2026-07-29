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
//! # Byte spans only (KTD5)
//!
//! Nothing here bakes line/column. The language server's only correct position
//! oracle is the same `Rope` it uses for incremental sync, so a pipeline that
//! built its own `SourceMap` would hand it a second source of truth. Byte-offset
//! clients (CLI text output, the browser wire shape) share one position helper
//! instead.

use std::path::PathBuf;

use oxabl_analyze::{
    CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, ExpandedFile,
    collect_from_expanded, expand_source,
};
use oxabl_common::{
    Diagnostic, FileId, InternalPanic, catch_panic, panic_if_injected, panic_sites,
};
use oxabl_semantic::Semantic;
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
    pub fn excluding_source(&self, source: DiagnosticSource) -> CollectedDiagnostics {
        CollectedDiagnostics {
            diagnostics: self
                .diagnostics()
                .all()
                .filter(|d| d.source != source)
                .cloned()
                .collect(),
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
}

impl<'a> LintPipeline<'a> {
    /// A pipeline over `config`, reading include files through `fs`, with
    /// preprocessing **on** — the setting every client but the CLI's
    /// `--preprocess=false` escape hatch wants.
    pub fn new(config: &'a PipelineConfig, fs: &'a dyn FileSystem) -> Self {
        LintPipeline {
            config,
            fs,
            preprocess: true,
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

    /// The configuration this pipeline runs under.
    pub fn config(&self) -> &PipelineConfig {
        self.config
    }

    /// Whether this pipeline preprocesses.
    pub fn preprocess(&self) -> bool {
        self.preprocess
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
                let (semantic, diagnostics) = collect_from_expanded(
                    expanded,
                    &self.config.schema,
                    self.config.schema_loaded,
                    &self.config.lint_severities,
                );
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
