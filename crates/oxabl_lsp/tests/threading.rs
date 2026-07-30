//! U3 threading discipline: write-on-main / read-on-snapshot, and the
//! `Cancelled` unwind swallowed on a worker.
//!
//! These use a controlled two-thread harness (not the live editor loop): a
//! reader thread computes diagnostics on a cloned snapshot while the main
//! thread writes.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

use oxabl_analyze::CollectedDiagnostics;
use oxabl_lsp::db::{AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, compute_diagnostics};
use oxabl_workspace::{FileSystem, InMemoryFileSystem};
use salsa::{Database, Setter};

fn config_with(
    fs: Arc<dyn FileSystem>,
    include_paths: Vec<PathBuf>,
    preprocess: bool,
) -> AnalysisConfig {
    AnalysisConfig {
        fs,
        pipeline: Arc::new(oxabl_pipeline::PipelineConfig {
            include_paths,
            ..oxabl_pipeline::PipelineConfig::default()
        }),
        preprocess,
    }
}

fn has_code(d: &CollectedDiagnostics, code: &str) -> bool {
    d.all().any(|c| c.diagnostic.code.0 == code)
}

/// Writes on the main thread while a snapshot read runs on a worker; both
/// complete and the post-write recompute reflects the edit. No deadlock.
#[test]
fn write_on_main_read_on_snapshot() {
    let mut db = AnalysisDatabase::new(config_with(
        Arc::new(InMemoryFileSystem::new()),
        vec![],
        false,
    ));
    let buffer = Buffer::new(&db, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string());
    let schema = SchemaHandle::new(&db, 0);

    // Read on a worker snapshot; join before writing so the snapshot is dropped.
    let snap = db.clone();
    let handle = thread::spawn(move || compute_diagnostics(&snap, buffer, schema));
    let before = handle.join().unwrap().expect("snapshot read completes");
    assert!(has_code(&before, "LINT0002"), "unused var before edit");

    // Write on main, then recompute on a fresh snapshot.
    buffer
        .set_text(&mut db)
        .to("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n".to_string());
    let snap2 = db.clone();
    let after = compute_diagnostics(&snap2, buffer, schema).expect("recompute completes");
    assert!(
        !has_code(&after, "LINT0002"),
        "reading x must clear the unused-variable diagnostic"
    );
}

// ---- Deterministic cancellation harness ------------------------------------

/// A filesystem that blocks the first `read` until released, so the main thread
/// has a window to cancel the snapshot mid-computation.
struct BlockingFs {
    inner: InMemoryFileSystem,
    entered: Arc<(Mutex<bool>, Condvar)>,
    release: Arc<(Mutex<bool>, Condvar)>,
}

impl FileSystem for BlockingFs {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        {
            let (m, c) = &*self.entered;
            *m.lock().unwrap() = true;
            c.notify_all();
        }
        {
            let (m, c) = &*self.release;
            let mut released = m.lock().unwrap();
            while !*released {
                released = c.wait(released).unwrap();
            }
        }
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.inner.exists(path)
    }
}

fn wait_flag(pair: &Arc<(Mutex<bool>, Condvar)>) {
    let (m, c) = &**pair;
    let mut set = m.lock().unwrap();
    while !*set {
        set = c.wait(set).unwrap();
    }
}

fn set_flag(pair: &Arc<(Mutex<bool>, Condvar)>) {
    let (m, c) = &**pair;
    *m.lock().unwrap() = true;
    c.notify_all();
}

/// A concurrent cancellation of an in-flight snapshot read causes the worker to
/// observe `Cancelled` (swallowed → `None`); the server stays responsive and a
/// later recompute succeeds.
#[test]
fn snapshot_read_cancelled_mid_flight_yields_none() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert("/p/blk.i".into(), "MESSAGE \"from include\".\n");

    let entered = Arc::new((Mutex::new(false), Condvar::new()));
    let release = Arc::new((Mutex::new(false), Condvar::new()));
    let fs = Arc::new(BlockingFs {
        inner,
        entered: entered.clone(),
        release: release.clone(),
    });

    let mut db = AnalysisDatabase::new(config_with(fs, vec!["/p".into()], true));
    let buffer = Buffer::new(
        &db,
        "{blk.i}\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
    );
    let schema = SchemaHandle::new(&db, 0);

    let snap = db.clone();
    let (tok_tx, tok_rx) = mpsc::channel();
    let handle = thread::spawn(move || {
        // The cancellation token is bound to this (reader) thread's snapshot.
        let token = snap.cancellation_token();
        tok_tx.send(token).unwrap();
        compute_diagnostics(&snap, buffer, schema)
    });

    let token = tok_rx.recv().unwrap();
    // Wait until the reader is blocked inside the include read, then cancel and
    // release: the read returns, and the query's cancellation checkpoint unwinds.
    wait_flag(&entered);
    token.cancel();
    set_flag(&release);

    let result = handle.join().unwrap();
    assert!(
        result.is_none(),
        "a cancelled snapshot read must be swallowed to None"
    );

    // Server stays responsive: a fresh computation (its own token, fs no longer
    // blocking) succeeds. Scoped so the snapshot drops before the write below —
    // a live snapshot would block `set_text` (which waits for clones to drop).
    {
        let snap2 = db.clone();
        let recomputed = compute_diagnostics(&snap2, buffer, schema);
        assert!(
            recomputed.is_some(),
            "server must stay responsive after cancel"
        );
    }

    // And writes on main still work afterwards (no outstanding snapshots).
    buffer.set_text(&mut db).to("MESSAGE \"ok\".\n".to_string());
}
