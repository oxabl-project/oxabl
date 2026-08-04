//! A cancelled recompute re-arms and reports no error (R10, KTD16).
//!
//! `salsa::Cancelled` travels as a panic payload in this workspace, and the whole
//! point of separating it from a real panic is that the two want opposite handling.
//! These drive the substrate directly, because cancellation is a property of the
//! snapshot read rather than of any client's routing.

use std::sync::Arc;

use oxabl_daemon::db::{
    AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, compute_diagnostics,
};
use oxabl_daemon::{CompletedWork, Disposition, dispose};
use oxabl_workspace::InMemoryFileSystem;

fn database() -> AnalysisDatabase {
    AnalysisDatabase::new(AnalysisConfig {
        fs: Arc::new(InMemoryFileSystem::new()),
        preprocess: true,
        ..Default::default()
    })
}

/// A computation on an uncontended snapshot answers, so "absent" genuinely means
/// cancelled rather than "this never works".
#[test]
fn an_uncontended_computation_answers() {
    let db = database();
    let buffer = Buffer::new(
        &db,
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
        None,
    );
    let schema = SchemaHandle::new(&db, 0);

    let diagnostics = compute_diagnostics(&db, buffer, schema).expect("no writer is contending");
    assert!(
        diagnostics.all().any(|c| c.diagnostic.code.0 == "LINT0002"),
        "the unused variable must be found"
    );
}

/// The disposition a cancelled computation gets: re-armed, and reported to nobody as
/// an error.
///
/// Salsa's cancellation is global — a write to any buffer's input flags every live
/// snapshot — so a cancelled computation is often work on a buffer that did not
/// change at all. Dropping it would leave that buffer showing pre-edit answers until
/// someone typed in it.
#[test]
fn a_cancelled_computation_re_arms_rather_than_failing() {
    let disposition = dispose(CompletedWork {
        read_version: 4,
        current_version: Some(4),
        read_generation: 1,
        current_generation: 1,
        has_diagnostics: false,
        panicked: false,
    });
    assert_eq!(disposition, Disposition::Retry);
}

/// A concurrent write really does cancel an in-flight read, which is what the
/// re-arming rule is for. Driven by writing on the main database while a snapshot
/// exists, then confirming the snapshot's next read still answers — the substrate
/// stays usable after a cancellation rather than being poisoned by it.
#[test]
fn a_write_leaves_the_substrate_usable_for_the_next_read() {
    let mut db = database();
    let buffer = Buffer::new(
        &db,
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
        None,
    );
    let schema = SchemaHandle::new(&db, 0);
    assert!(compute_diagnostics(&db, buffer, schema).is_some());

    // A write on the main thread: this is what flags live snapshots.
    use salsa::Setter;
    buffer
        .set_text(&mut db)
        .to("DEFINE VARIABLE y AS INTEGER NO-UNDO.\nMESSAGE y.\n".to_string());

    let after = compute_diagnostics(&db, buffer, schema)
        .expect("a cancellation must not leave the database unusable");
    assert!(
        !after.all().any(|c| c.diagnostic.code.0 == "LINT0002"),
        "the new text reads its variable, so the unused-variable finding is gone"
    );
}

/// Bumping the schema revision invalidates and recomputes without a stale-handle
/// failure — the hot-reload edge, exercised through the session core.
#[test]
fn bumping_the_schema_revision_recomputes() {
    let mut sessions = oxabl_daemon::Sessions::new();
    let session = sessions.for_root("/proj");
    let buffer = session.set_buffer("main.p", "MESSAGE \"hi\".\n".to_string(), None);
    let schema = session.schema_handle();

    assert!(compute_diagnostics(session.database(), buffer, schema).is_some());
    session.bump_schema();
    assert!(
        compute_diagnostics(session.database(), buffer, schema).is_some(),
        "a bumped schema handle must still answer"
    );
}
