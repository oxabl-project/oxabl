//! A genuine panic fails one request, keeps the session alive, and is never
//! retried (R10, KTD16).
//!
//! No ABL input panics, so these drive the injection seam: a source carrying an
//! `OXABL-TEST-PANIC:<site>` marker panics at the guarded site. The seam is armed
//! only by a dev-dependency feature, so it is inert in a real build.

use std::sync::Arc;

use oxabl_daemon::db::{AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle};
use oxabl_daemon::dispatch::{Dispatch, MethodError};
use oxabl_daemon::{CompletedWork, Disposition, SessionHost, analyze_guarded, dispose};
use oxabl_workspace::InMemoryFileSystem;
use serde_json::{Value, json};

fn database() -> AnalysisDatabase {
    AnalysisDatabase::new(AnalysisConfig {
        fs: Arc::new(InMemoryFileSystem::new()),
        preprocess: true,
        ..Default::default()
    })
}

/// Silence the panic hook for the duration of a deliberately panicking call, so a
/// green run does not print a backtrace.
fn quietly<T>(f: impl FnOnce() -> T) -> T {
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let out = f();
    std::panic::set_hook(previous);
    out
}

/// The guard's real contract: it **returns normally** with both halves degraded, so
/// the caller's send is reached and no request stalls waiting on a result that never
/// arrives.
#[test]
fn a_panic_in_either_query_returns_normally_with_both_halves_degraded() {
    for site in [
        oxabl_common::panic_sites::LSP_DIAGNOSTICS,
        oxabl_common::panic_sites::LSP_DEPENDENCIES,
    ] {
        let db = database();
        let text =
            format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
        let buffer = Buffer::new(&db, text, None);
        let schema = SchemaHandle::new(&db, 0);

        let analysis = quietly(|| analyze_guarded(&db, buffer, schema, "injected"));

        assert!(
            analysis.diagnostics.is_none(),
            "a panic at {site} must degrade to no diagnostics"
        );
        assert!(
            analysis.dependencies.is_none(),
            "a panic at {site} must degrade dependencies together with diagnostics"
        );
        assert!(
            analysis.panicked,
            "a panic at {site} must be reported as a panic and not as a cancellation — \
             one is retried and the other must never be"
        );
    }
}

/// The never-retry rule. A panic is deterministic in the buffer's text, so retrying
/// it spins at the scheduling interval forever.
#[test]
fn a_panicked_computation_is_never_retried() {
    let disposition = dispose(CompletedWork {
        read_version: 2,
        current_version: Some(2),
        read_generation: 5,
        current_generation: 5,
        has_diagnostics: false,
        panicked: true,
    });
    assert_eq!(disposition, Disposition::Drop);
}

/// A healthy buffer still answers through the same guard, so the assertions above
/// are not passing because the guard swallows everything.
#[test]
fn the_guard_passes_a_healthy_buffer_through() {
    let db = database();
    let buffer = Buffer::new(
        &db,
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
        None,
    );
    let schema = SchemaHandle::new(&db, 0);

    let analysis = analyze_guarded(&db, buffer, schema, "healthy");
    assert!(!analysis.panicked);
    assert_eq!(
        analysis.dependencies,
        Some(Vec::new()),
        "a file with no includes has an empty set, not an absent one"
    );
    assert!(
        analysis
            .diagnostics
            .expect("a healthy buffer yields diagnostics")
            .all()
            .any(|c| c.diagnostic.code.0 == "LINT0002")
    );
}

/// A panic while answering one request fails that request and leaves every other
/// method and every other session serving.
#[test]
fn a_panicking_request_leaves_the_daemon_and_other_sessions_working() {
    let mut dispatch = Dispatch::new();
    dispatch.register("oxabl/boom", |_, _| panic!("deliberate"));
    dispatch.register("oxabl/count", |host: &SessionHost, _| {
        Ok(json!(host.with(|sessions| sessions.len())))
    });

    let host = SessionHost::new();
    host.with(|sessions| {
        sessions.for_root("/proj/alpha").attach(true);
        sessions.for_root("/proj/beta").attach(false);
    });

    let error: MethodError = quietly(|| {
        dispatch
            .call(&host, "oxabl/boom", Value::Null)
            .expect_err("the panicking request must fail")
    });
    assert!(error.message.contains("deliberate"), "got {error}");

    assert_eq!(
        dispatch.call(&host, "oxabl/count", Value::Null),
        Ok(json!(2)),
        "both sessions must survive one request's panic"
    );
    host.with(|sessions| {
        let alpha = sessions.get("/proj/alpha").expect("alpha survives");
        assert_eq!(
            alpha.clients(),
            1,
            "the other client's session is untouched"
        );
    });
}
