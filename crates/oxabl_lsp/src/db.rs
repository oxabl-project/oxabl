//! Coarse salsa 0.28 substrate (KTD2) with the write-on-main / read-on-snapshot
//! threading discipline (KTD7).
//!
//! The query graph is deliberately coarse — one input per open file, two
//! tracked queries:
//!
//! ```text
//! Buffer(text) ──▶ expanded_text ──▶ diagnostics ◀── SchemaHandle(revision)
//! ```
//!
//! - [`Buffer`] — a per-file salsa input holding the open buffer's text.
//! - [`SchemaHandle`] — a salsa input carrying a bumpable revision; bumping it
//!   invalidates the diagnostics query so a `.df` change hot-reloads without a
//!   restart (R16) and without re-setting every buffer.
//! - `expanded_text` — the shared pipeline's **first phase**, preprocessing the
//!   buffer into an [`Expansion`]; memoized so an edit with byte-identical
//!   expansion cuts off `diagnostics` early.
//! - `diagnostics` — the shared pipeline's **second phase** over that expansion.
//!
//! The two queries are the *only* orchestration left here: expanding, parsing,
//! analyzing, and linting all live in [`oxabl_pipeline`], which the CLI and the
//! browser drive through the same handle. This crate holds the pipeline, decides
//! *when* to run each phase, and renders the result.
//!
//! ## Why two phases and not one `run` (KTD2)
//!
//! [`LintPipeline::run`] would be shorter, and it is what the non-incremental
//! clients call — but it is wrong here twice over. The intermediate
//! [`Expansion`] is what [`buffer_dependencies`] reads for the watcher's
//! changed-include → buffer matching, and memoizing it is what gives salsa its
//! early cutoff: [`Expansion`] is `Clone + PartialEq + Eq` precisely so an edit
//! whose expanded text is byte-identical backdates and skips the expensive
//! parse/semantic/lint entirely. `run` is also *guarded*, and a `catch_panic`
//! inside the salsa call would swallow the [`salsa::Cancelled`] unwind — turning
//! "abandon this stale snapshot" into "this file failed to analyze" and
//! publishing stale diagnostics. [`LintPipeline::expand`] and
//! [`LintPipeline::collect`] are unguarded on purpose so this crate can layer
//! its own guard *outside* its cancellation catch (KTD6); see
//! [`crate::analyze_guarded`].
//!
//! The resolved [`PipelineConfig`], the filesystem, and the preprocess switch
//! are **not** salsa inputs — they are plain db configuration (R17). Changing
//! them requires re-triggering affected buffers, which the watcher does; the
//! schema *revision* handle is the one exception that auto-invalidates.
//!
//! ## Threading (KTD7)
//!
//! Writes (`set_*` on `&mut db`) run on the main loop. Each diagnostics
//! computation runs on a **cloned snapshot** (`db.clone()`) on a worker thread
//! and is wrapped in [`salsa::Cancelled::catch`]: a concurrent write on the
//! main thread cancels in-flight snapshot reads via a `Cancelled` unwind, which
//! [`compute_diagnostics`] swallows. Version tag-and-discard in the debounce
//! layer (U6) is the primary correctness mechanism; cancellation is an
//! optimization on top.

use std::path::PathBuf;
use std::sync::Arc;

use oxabl_analyze::CollectedDiagnostics;
use oxabl_pipeline::{Expansion, LintPipeline, PipelineConfig};
use oxabl_workspace::{FileSystem, RealFileSystem};
use salsa::Storage;

/// The root buffer's file id, re-exported from its single owner (KTD12).
///
/// Each open file is analyzed independently (no cross-file salsa), and the
/// preprocessor assigns include file ids starting at `root + 1`, so a fixed root
/// of 1 never collides with an include. This crate used to declare its own
/// `FileId::new(1)` that happened to agree with the umbrella's; the pipeline now
/// owns the constant and both point here.
pub use oxabl_pipeline::ROOT_FILE_ID;

/// Per-file salsa input: the open buffer's current text.
#[salsa::input]
pub struct Buffer {
    #[returns(ref)]
    pub text: String,
}

/// Salsa input carrying a monotonically-bumped schema revision. The diagnostics
/// query reads it so bumping it (on a `.df` change) invalidates and recomputes
/// live (R16). The schema *value* itself is db configuration, not a salsa field.
#[salsa::input]
pub struct SchemaHandle {
    pub revision: u32,
}

/// Non-salsa db configuration shared by every query on a snapshot.
///
/// The resolved [`PipelineConfig`] — include paths, lint severities, style, and
/// the schema, all from **one** read of `oxabl.toml` (KTD3) — sits behind an
/// `Arc` for two reasons that both matter on the interactive path: cloning a
/// snapshot per debounced recompute is a pointer bump rather than a `Schema`
/// clone, and replacing the configuration on a watcher event is a pointer swap.
///
/// The two fields beside it are exactly what `PipelineConfig` deliberately does
/// not own: the filesystem include reads go through, and whether to preprocess
/// at all.
#[derive(Clone)]
pub struct AnalysisConfig {
    /// Filesystem used to read include files (real disk for the live server).
    pub fs: Arc<dyn FileSystem>,
    /// The one resolved configuration every phase runs under.
    pub pipeline: Arc<PipelineConfig>,
    /// Whether to run the preprocessor (always `true` for the live server, R6;
    /// the threading tests turn it off to isolate the substrate).
    pub preprocess: bool,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        AnalysisConfig {
            fs: Arc::new(RealFileSystem),
            pipeline: Arc::new(PipelineConfig::default()),
            preprocess: true,
        }
    }
}

impl AnalysisConfig {
    /// The shared lint pipeline for this configuration.
    ///
    /// Built per query rather than stored, and that is the cheap direction:
    /// [`LintPipeline`] *borrows* its config and its filesystem, so this is two
    /// pointers and a bool — no `Schema` clone, no per-run allocation on the
    /// keystroke path (the 50ms interactivity budget, R14).
    pub fn lint_pipeline(&self) -> LintPipeline<'_> {
        LintPipeline::new(&self.pipeline, &*self.fs).with_preprocess(self.preprocess)
    }
}

/// Access to the non-salsa configuration from inside a query.
pub trait HasConfig {
    fn config(&self) -> &AnalysisConfig;
}

/// The database trait tracked queries are written against.
#[salsa::db]
pub trait AblDatabase: HasConfig + salsa::Database {}

#[salsa::db]
impl<T: HasConfig + salsa::Database> AblDatabase for T {}

/// The concrete analysis database. Cloning it produces a salsa snapshot that
/// shares memoized state for reads (KTD7).
#[salsa::db]
#[derive(Clone)]
pub struct AnalysisDatabase {
    storage: Storage<Self>,
    config: AnalysisConfig,
}

impl AnalysisDatabase {
    /// Create a database with the given configuration.
    pub fn new(config: AnalysisConfig) -> Self {
        AnalysisDatabase {
            storage: Storage::default(),
            config,
        }
    }

    /// Replace the non-salsa configuration (include paths, schema value, lint
    /// severities, …). Callers must re-trigger affected buffers afterwards
    /// (bump their input) so memoized results recompute (R17).
    pub fn set_config(&mut self, config: AnalysisConfig) {
        self.config = config;
    }

    /// Read the current configuration.
    pub fn config(&self) -> &AnalysisConfig {
        &self.config
    }

    /// A database that records the `Debug` rendering of every query salsa
    /// actually **executes**, so a test can assert an early cutoff rather than
    /// merely assert that two results happen to be equal (KTD2).
    ///
    /// Test-only, and deliberately not part of the public surface: the live
    /// server has no use for an event sink, and adding one would put a callback
    /// on the hottest path in the product.
    #[cfg(test)]
    pub(crate) fn recording(
        config: AnalysisConfig,
        executed: Arc<std::sync::Mutex<Vec<String>>>,
    ) -> Self {
        let sink = move |event: salsa::Event| {
            if let salsa::EventKind::WillExecute { database_key } = event.kind {
                executed.lock().unwrap().push(format!("{database_key:?}"));
            }
        };
        AnalysisDatabase {
            storage: Storage::new(Some(Box::new(sink))),
            config,
        }
    }
}

impl HasConfig for AnalysisDatabase {
    fn config(&self) -> &AnalysisConfig {
        &self.config
    }
}

#[salsa::db]
impl salsa::Database for AnalysisDatabase {}

/// Phase one, memoized: the shared pipeline's expansion of a buffer. Carries the
/// loud root-origin preprocessor errors on its fatal-failure arm rather than
/// making the caller handle an `Err` before it can ask a question.
#[salsa::tracked(returns(clone))]
fn expanded_text(db: &dyn AblDatabase, buffer: Buffer) -> Expansion {
    db.config().lint_pipeline().expand(buffer.text(db).as_str())
}

/// Phase two: the shared pipeline's collection over the memoized expansion,
/// yielding the full root-resolved diagnostic set. Depends on the expansion and
/// on the schema handle's revision (so schema hot-reload invalidates, R16).
#[salsa::tracked(returns(clone))]
fn diagnostics(db: &dyn AblDatabase, buffer: Buffer, schema: SchemaHandle) -> CollectedDiagnostics {
    // Establish the dependency edge on the schema revision (R16 hot-reload).
    let _revision = schema.revision(db);
    let expansion = expanded_text(db, buffer);
    // Cooperative cancellation checkpoint (KTD7): if a concurrent main-thread
    // write cancelled this snapshot while we were expanding, unwind here — before
    // the expensive parse/semantic/lint — rather than compute a doomed result.
    // This sits *between* the two phases deliberately: it is the one point where
    // the cheap work is done and the expensive work has not started.
    db.unwind_if_revision_cancelled();
    // The pipeline's fatal-preprocessing arm already yields the preprocessor's
    // own diagnostics under `DiagnosticSource::Preproc`, so there is no second
    // mapping to keep in step here. `into_diagnostics` takes the set rather than
    // cloning it — this runs per keystroke.
    db.config()
        .lint_pipeline()
        .collect(&expansion)
        .into_diagnostics()
}

/// Compute diagnostics on a snapshot, swallowing a [`salsa::Cancelled`] unwind
/// (returns `None` when the read was cancelled by a concurrent write, KTD7).
///
/// Intended to run on a worker thread against a cloned snapshot; never call it
/// with the `&mut` main database.
pub fn compute_diagnostics(
    snapshot: &AnalysisDatabase,
    buffer: Buffer,
    schema: SchemaHandle,
) -> Option<CollectedDiagnostics> {
    // The snapshot holds an `Arc<dyn FileSystem>`, which is not `RefUnwindSafe`;
    // salsa's own snapshots are designed to cross the `catch_unwind` boundary of
    // `Cancelled::catch`, so asserting unwind-safety here is sound.
    oxabl_common::panic_if_injected(
        oxabl_common::panic_sites::LSP_DIAGNOSTICS,
        buffer.text(snapshot),
    );
    salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
        diagnostics(snapshot, buffer, schema)
    }))
    .ok()
}

/// The include file paths a buffer transitively depends on, for the watcher's
/// `*.i` → buffer matching (R17). Computed via the memoized expansion query, so
/// this is cheap on an unchanged buffer. Empty on a fatal preprocessing error.
pub fn buffer_dependencies(snapshot: &AnalysisDatabase, buffer: Buffer) -> Vec<PathBuf> {
    oxabl_common::panic_if_injected(
        oxabl_common::panic_sites::LSP_DEPENDENCIES,
        buffer.text(snapshot),
    );
    expanded_text(snapshot, buffer).dependency_paths().to_vec()
}

/// The tracked expansion query, exposed for tests that exercise early cutoff.
#[cfg(test)]
pub(crate) fn expanded_text_for_test(db: &AnalysisDatabase, buffer: Buffer) -> Expansion {
    expanded_text(db, buffer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    fn config_with(source_fs: oxabl_workspace::InMemoryFileSystem) -> AnalysisConfig {
        AnalysisConfig {
            fs: Arc::new(source_fs),
            preprocess: true,
            ..Default::default()
        }
    }

    fn db_with(source_fs: oxabl_workspace::InMemoryFileSystem) -> AnalysisDatabase {
        AnalysisDatabase::new(config_with(source_fs))
    }

    /// The shared guard both diagnostics paths go through (R8). Client-observable
    /// behavior cannot tell a contained panic from a dead worker thread, so the
    /// guard's real contract — *return normally* with both halves degraded, so
    /// the worker's later `send` is reached — is pinned here rather than e2e.
    ///
    /// Injected panics via `oxabl_common`'s test-only `test-panics` feature; no
    /// ABL input panics.
    #[test]
    fn analyze_guarded_contains_a_panic_in_either_query() {
        for site in [
            oxabl_common::panic_sites::LSP_DIAGNOSTICS,
            oxabl_common::panic_sites::LSP_DEPENDENCIES,
        ] {
            let db = db_with(oxabl_workspace::InMemoryFileSystem::new());
            let text =
                format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
            let buffer = Buffer::new(&db, text);
            let schema = SchemaHandle::new(&db, 0);

            let previous = std::panic::take_hook();
            std::panic::set_hook(Box::new(|_| {}));
            let (diagnostics, dependencies) =
                crate::analyze_guarded(&db, buffer, schema, "file:///injected.p");
            std::panic::set_hook(previous);

            assert!(
                diagnostics.is_none(),
                "a panic at {site} must degrade to no diagnostics"
            );
            assert!(
                dependencies.is_empty(),
                "a panic at {site} must degrade dependencies together with diagnostics"
            );
        }
    }

    /// The same helper on a healthy buffer still returns real results, so the
    /// guard has not swallowed the normal path.
    #[test]
    fn analyze_guarded_passes_through_a_healthy_buffer() {
        let db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(&db, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string());
        let schema = SchemaHandle::new(&db, 0);

        let (diagnostics, _deps) = crate::analyze_guarded(&db, buffer, schema, "file:///healthy.p");
        let diagnostics = diagnostics.expect("a healthy buffer yields diagnostics");
        assert!(diagnostics.all().any(|c| c.diagnostic.code.0 == "LINT0002"));
    }

    #[test]
    fn diagnostics_recompute_on_change_memo_on_stable() {
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(&db, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string());
        let schema = SchemaHandle::new(&db, 0);

        // An unused variable → LINT0002.
        let d1 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(d1.all().any(|c| c.diagnostic.code.0 == "LINT0002"));

        // Recompute on unchanged input returns the same set (memo hit; equal).
        let d2 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert_eq!(d1, d2);

        // Editing the buffer to *read* x clears the unused-variable diagnostic.
        buffer
            .set_text(&mut db)
            .to("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n".to_string());
        let d3 = compute_diagnostics(&db, buffer, schema).unwrap();
        assert!(
            !d3.all().any(|c| c.diagnostic.code.0 == "LINT0002"),
            "expected the unused-variable diagnostic to clear, got {:?}",
            d3.all().map(|c| c.diagnostic.code.0).collect::<Vec<_>>()
        );
    }

    #[test]
    fn early_cutoff_on_byte_identical_edit() {
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string();
        let buffer = Buffer::new(&db, text.clone());

        let e1 = expanded_text_for_test(&db, buffer);

        // Setting the input to a byte-identical value bumps the input revision,
        // so `expanded_text` re-executes — but produces an equal `Expansion`,
        // which salsa backdates. This is the enabling property: `Expansion`
        // derives `PartialEq`/`Eq` for exactly this comparison (KTD2).
        buffer.set_text(&mut db).to(text);
        let e2 = expanded_text_for_test(&db, buffer);
        assert_eq!(e1, e2, "byte-identical edit must yield identical expansion");
    }

    /// The property the two-phase split exists for (KTD2): after a byte-identical
    /// edit the expansion re-executes and backdates, so the *expensive* second
    /// phase — parse, semantic, lint — is skipped entirely.
    ///
    /// Asserted on salsa's own execution events rather than on equal outputs,
    /// because equal outputs are also what a full recompute produces: only the
    /// event log can tell a cutoff from a coincidence. Rewiring the queries onto
    /// the pipeline is exactly the edit that could lose this.
    #[test]
    fn byte_identical_edit_skips_the_collect_phase() {
        let executed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let mut db = AnalysisDatabase::recording(
            config_with(oxabl_workspace::InMemoryFileSystem::new()),
            Arc::clone(&executed),
        );
        let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n".to_string();
        let buffer = Buffer::new(&db, text.clone());
        let schema = SchemaHandle::new(&db, 0);

        let first = compute_diagnostics(&db, buffer, schema).unwrap();
        {
            let log = executed.lock().unwrap();
            assert!(
                log.iter().any(|e| e.contains("diagnostics")),
                "the cold run must execute the collect phase, got {log:?}"
            );
        }

        executed.lock().unwrap().clear();
        buffer.set_text(&mut db).to(text);
        let second = compute_diagnostics(&db, buffer, schema).unwrap();

        let log = executed.lock().unwrap();
        assert!(
            log.iter().any(|e| e.contains("expanded_text")),
            "the expansion itself re-executes (its input revision moved), got {log:?}"
        );
        assert!(
            !log.iter().any(|e| e.contains("diagnostics")),
            "an equal expansion must backdate and skip parse/semantic/lint, got {log:?}"
        );
        assert_eq!(first, second);
    }

    #[test]
    fn schema_handle_revision_invalidates() {
        // Bumping the schema handle recomputes diagnostics even though the
        // buffer is unchanged (the R16 hot-reload dependency edge exists).
        let mut db = db_with(oxabl_workspace::InMemoryFileSystem::new());
        let buffer = Buffer::new(&db, "MESSAGE \"hi\".\n".to_string());
        let schema = SchemaHandle::new(&db, 0);

        let _ = compute_diagnostics(&db, buffer, schema).unwrap();
        schema.set_revision(&mut db).to(1);
        // Recompute succeeds after the bump (no panic / stale-handle issue).
        let _ = compute_diagnostics(&db, buffer, schema).unwrap();
    }
}
