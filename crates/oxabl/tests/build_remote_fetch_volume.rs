//! A fetch answers every key, also when the build takes the answers more
//! slowly than the cache downloads them.
//!
//! The client keeps a bounded buffer of outcomes and ends a fetch after its
//! idle timeout. A reader that stalls (a long sync of the disk) must not turn
//! into lost answers. This binary holds one test, because it sets the process
//! environment for the client.

#![cfg(feature = "remote-cache")]

mod common;

use std::path::Path;
use std::time::Duration;

use common::remote::world::{Account, World};
use oxabl::build::cache::{
    ArtifactCache, ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, FetchOutcome,
    FetchRequest, StoreOutcome,
};
use oxabl_build_remote::RemoteCache;
use tempfile::TempDir;

/// More entries than the client's outcome buffer (1024).
const ENTRIES: usize = 1300;
/// Idle timeout of the fetch.
const IDLE: Duration = Duration::from_secs(5);
/// One stall of the reader, longer than the idle timeout.
const STALL: Duration = Duration::from_secs(7);

fn key(index: usize) -> CompileKey {
    CompileKey(*blake3::hash(format!("unit {index}").as_bytes()).as_bytes())
}

fn path(index: usize) -> ArtifactPath {
    ArtifactPath(format!("r/u{index}.r"))
}

/// Sets the client's variables for `account` in this process.
fn act_as(account: &Account, mode: &str, state: &Path) {
    // SAFETY: this binary runs one test, so no other thread reads the
    // environment while it changes.
    unsafe {
        std::env::set_var("OXABL_CACHE_MODE", mode);
        std::env::set_var("OXABL_CACHE_TOKEN", &account.token);
        std::env::set_var("OXABL_CACHE_IDENTITY", &account.member.identity_string);
        std::env::set_var("XDG_STATE_HOME", state);
        std::env::remove_var("CI");
    }
}

#[test]
fn a_reader_that_stalls_still_gets_an_answer_for_every_key() {
    let world = World::start();
    let root = TempDir::new().unwrap();
    std::fs::create_dir_all(root.path().join(".oxabl")).unwrap();
    std::fs::write(root.path().join(".oxabl/cache.toml"), world.cache_toml("")).unwrap();
    let state = TempDir::new().unwrap();
    act_as(&world.ci, "ci", state.path());

    let files = TempDir::new().unwrap();
    let cache = RemoteCache::open(root.path(), IDLE).expect("the remote cache opens");
    let mut uploader = cache.uploader();
    for index in 0..ENTRIES {
        let bytes = format!("r-code of unit {index}\n");
        let disk_path = files.path().join(format!("u{index}.r"));
        std::fs::write(&disk_path, &bytes).unwrap();
        uploader.push(ArtifactSetRef {
            key: key(index),
            files: vec![ArtifactFileRef {
                path: path(index),
                disk_path,
                size: bytes.len() as u64,
                blake3: *blake3::hash(bytes.as_bytes()).as_bytes(),
            }],
        });
    }
    let stored = uploader.finish();
    assert_eq!(stored.len(), ENTRIES);
    assert!(
        stored
            .iter()
            .all(|(_, outcome)| matches!(outcome, StoreOutcome::Stored)),
        "every entry is stored: {:?}",
        stored
            .iter()
            .find(|(_, o)| !matches!(o, StoreOutcome::Stored))
    );
    drop(cache);

    let cache = RemoteCache::open(root.path(), IDLE).expect("the remote cache opens");
    let stage = TempDir::new().unwrap();
    let requests = (0..ENTRIES)
        .map(|index| FetchRequest {
            key: key(index),
            expect: vec![path(index)],
        })
        .collect();
    let (mut installed, mut missed) = (0, Vec::new());
    cache.fetch(requests, stage.path(), &mut |outcome| {
        if installed + missed.len() == 0 {
            std::thread::sleep(STALL);
        }
        match outcome {
            FetchOutcome::Installed { .. } => installed += 1,
            FetchOutcome::Miss { key, reason } => missed.push((key, reason)),
        }
    });

    let notes = cache.notes();
    assert!(
        notes.is_empty(),
        "a complete fetch leaves no note: {notes:?}"
    );
    assert_eq!(
        installed + missed.len(),
        ENTRIES,
        "every key gets one answer ({installed} installed, {} missed)",
        missed.len()
    );
    assert_eq!(
        installed, ENTRIES,
        "every stored entry is installed: {missed:?}"
    );
}
