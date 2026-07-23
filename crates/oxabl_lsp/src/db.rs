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
//! - `expanded_text` — preprocesses the buffer into an [`ExpandedFile`] (owned,
//!   `Update`-trivial); memoized so an edit with byte-identical expansion cuts
//!   off `diagnostics` early.
//! - `diagnostics` — runs the shared collector over the expanded file.
//!
//! Include search paths, the schema value, the schema-loaded flag, the lint
//! severity map, and the filesystem are **not** salsa inputs — they are plain
//! db configuration (R17). Changing them requires re-triggering affected
//! buffers, which the U8 watcher does; the schema *revision* handle is the one
//! exception that auto-invalidates.
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

use oxabl_analyze::{
    CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, ExpandedFile,
    collect_from_expanded, expand_source,
};
use oxabl_common::{Diagnostic, FileId, LintSeverityMap};
use oxabl_schema::Schema;
use oxabl_workspace::{FileSystem, RealFileSystem};
use salsa::Storage;

/// The root buffer's file id. Each open file is analyzed independently (no
/// cross-file salsa), and the preprocessor assigns include file ids starting at
/// `root + 1`, so a fixed root of 1 never collides with an include.
pub const ROOT_FILE_ID: FileId = FileId::new(1);

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

/// Non-salsa db configuration shared by every query on a snapshot. Held behind
/// `Arc`s so cloning a snapshot is cheap and updates are a pointer swap.
#[derive(Clone)]
pub struct AnalysisConfig {
    /// Filesystem used to read include files (real disk for the live server).
    pub fs: Arc<dyn FileSystem>,
    /// The loaded schema (empty when no `.df` is discovered).
    pub schema: Arc<Schema>,
    /// Whether a `.df` was actually loaded (gates schema-dependent rules, R10).
    pub schema_loaded: bool,
    /// Resolved PROPATH for include resolution.
    pub include_paths: Arc<Vec<PathBuf>>,
    /// Resolved per-rule severity surface (R15).
    pub lint_severities: Arc<LintSeverityMap>,
    /// Whether to run the preprocessor (always `true` for the live server, R6).
    pub preprocess: bool,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        AnalysisConfig {
            fs: Arc::new(RealFileSystem),
            schema: Arc::new(Schema::empty()),
            schema_loaded: false,
            include_paths: Arc::new(Vec::new()),
            lint_severities: Arc::new(LintSeverityMap::new()),
            preprocess: true,
        }
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
}

impl HasConfig for AnalysisDatabase {
    fn config(&self) -> &AnalysisConfig {
        &self.config
    }
}

#[salsa::db]
impl salsa::Database for AnalysisDatabase {}

/// Preprocess a buffer into an [`ExpandedFile`] (memoized). `Err` carries the
/// loud root-origin preprocessor errors on a fatal preprocessing failure.
#[salsa::tracked(returns(clone))]
fn expanded_text(db: &dyn AblDatabase, buffer: Buffer) -> Result<ExpandedFile, Vec<Diagnostic>> {
    let cfg = db.config();
    expand_source(
        ROOT_FILE_ID,
        buffer.text(db).as_str(),
        &*cfg.fs,
        cfg.include_paths.as_slice(),
        cfg.preprocess,
    )
}

/// Compute the full, root-resolved diagnostic set for a buffer. Depends on the
/// memoized expansion and on the schema handle's revision (so schema
/// hot-reload invalidates, R16).
#[salsa::tracked(returns(clone))]
fn diagnostics(db: &dyn AblDatabase, buffer: Buffer, schema: SchemaHandle) -> CollectedDiagnostics {
    // Establish the dependency edge on the schema revision (R16 hot-reload).
    let _revision = schema.revision(db);
    let cfg = db.config();
    let expansion = expanded_text(db, buffer);
    // Cooperative cancellation checkpoint (KTD7): if a concurrent main-thread
    // write cancelled this snapshot while we were expanding, unwind here — before
    // the expensive parse/semantic/lint — rather than compute a doomed result.
    db.unwind_if_revision_cancelled();
    match expansion {
        Ok(expanded) => {
            collect_from_expanded(
                &expanded,
                &cfg.schema,
                cfg.schema_loaded,
                &cfg.lint_severities,
            )
            .1
        }
        Err(preproc_errors) => {
            let mut out = CollectedDiagnostics::default();
            for d in preproc_errors {
                out.diagnostics.push(CollectedDiagnostic {
                    diagnostic: d,
                    source: DiagnosticSource::Preproc,
                });
            }
            out
        }
    }
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
    salsa::Cancelled::catch(std::panic::AssertUnwindSafe(|| {
        diagnostics(snapshot, buffer, schema)
    }))
    .ok()
}

/// The include file paths a buffer transitively depends on, for the watcher's
/// `*.i` → buffer matching (R17). Computed via the memoized expansion query, so
/// this is cheap on an unchanged buffer. Empty on a fatal preprocessing error.
pub fn buffer_dependencies(snapshot: &AnalysisDatabase, buffer: Buffer) -> Vec<PathBuf> {
    match expanded_text(snapshot, buffer) {
        Ok(expanded) => expanded.dependency_paths().to_vec(),
        Err(_) => Vec::new(),
    }
}

/// The tracked expansion query, exposed for tests that exercise early cutoff.
#[cfg(test)]
pub(crate) fn expanded_text_for_test(
    db: &AnalysisDatabase,
    buffer: Buffer,
) -> Result<ExpandedFile, Vec<Diagnostic>> {
    expanded_text(db, buffer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    fn db_with(source_fs: oxabl_workspace::InMemoryFileSystem) -> AnalysisDatabase {
        let config = AnalysisConfig {
            fs: Arc::new(source_fs),
            preprocess: true,
            ..Default::default()
        };
        AnalysisDatabase::new(config)
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

        let e1 = expanded_text_for_test(&db, buffer).unwrap();

        // Setting the input to a byte-identical value bumps the input revision,
        // so `expanded_text` re-executes — but produces an equal `ExpandedFile`,
        // which salsa backdates. The observable contract we assert is that the
        // output is unchanged (the early-cutoff enabler).
        buffer.set_text(&mut db).to(text);
        let e2 = expanded_text_for_test(&db, buffer).unwrap();
        assert_eq!(e1, e2, "byte-identical edit must yield identical expansion");
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
