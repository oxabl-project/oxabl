//! The local cache through its public API: what it stores, what it returns
//! and what it refuses.

mod common;

use std::path::{Path, PathBuf};

use common::cache::{entries, quarantined};
use oxabl::build::cache::{
    ArtifactCache, ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey, FetchOutcome,
    FetchRequest, LocalCache, MissReason, NotStoredReason, Origin, StoreOutcome,
};
use tempfile::TempDir;

fn key(n: u8) -> CompileKey {
    CompileKey([n; 32])
}

fn file(dir: &Path, name: &str, bytes: &[u8]) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, bytes).unwrap();
    path
}

fn reference(artifact: &str, disk: &Path, bytes: &[u8]) -> ArtifactFileRef {
    ArtifactFileRef {
        path: ArtifactPath(artifact.to_string()),
        disk_path: disk.to_path_buf(),
        size: bytes.len() as u64,
        blake3: *blake3::hash(bytes).as_bytes(),
    }
}

fn store(cache: &LocalCache, set: ArtifactSetRef) -> StoreOutcome {
    let mut uploader = cache.uploader();
    let key = set.key;
    uploader.push(set);
    let outcomes = uploader.finish();
    assert_eq!(outcomes.len(), 1, "{outcomes:?}");
    assert_eq!(outcomes[0].0, key);
    outcomes[0].1
}

fn fetch(cache: &LocalCache, request: FetchRequest, stage: &Path) -> FetchOutcome {
    let mut seen = Vec::new();
    cache.fetch(vec![request], stage, &mut |outcome| seen.push(outcome));
    assert_eq!(seen.len(), 1, "one outcome per key: {seen:?}");
    seen.remove(0)
}

fn expect(paths: &[&str]) -> Vec<ArtifactPath> {
    paths.iter().map(|p| ArtifactPath(p.to_string())).collect()
}

fn open(directory: &TempDir) -> LocalCache {
    LocalCache::open(directory.path(), 1 << 30).unwrap()
}

#[test]
fn a_stored_set_comes_back_whole_at_its_artifact_paths() {
    let (cache_dir, source, stage) = (
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
    );
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    let x = file(source.path(), "x.xml", b"<xref/>");
    let set = ArtifactSetRef {
        key: key(1),
        files: vec![
            reference("r/pkg/x.r", &r, b"rcode"),
            reference("xref/pkg/x.cls.xref.xml", &x, b"<xref/>"),
        ],
    };

    assert_eq!(store(&cache, set), StoreOutcome::Stored);
    let outcome = fetch(
        &cache,
        FetchRequest {
            key: key(1),
            expect: expect(&["xref/pkg/x.cls.xref.xml", "r/pkg/x.r"]),
        },
        stage.path(),
    );

    match outcome {
        FetchOutcome::Installed {
            key: got,
            mut files,
            origin,
        } => {
            files.sort();
            assert_eq!(got, key(1));
            assert_eq!(files, expect(&["r/pkg/x.r", "xref/pkg/x.cls.xref.xml"]));
            assert_eq!(origin, Origin::Local { recorded: None });
        }
        other => panic!("{other:?}"),
    }
    assert_eq!(
        std::fs::read(stage.path().join("r/pkg/x.r")).unwrap(),
        b"rcode"
    );
    assert_eq!(
        std::fs::read(stage.path().join("xref/pkg/x.cls.xref.xml")).unwrap(),
        b"<xref/>"
    );
}

#[test]
fn a_key_nobody_stored_is_not_found_and_other_artifact_paths_are_a_mismatch_that_keeps_the_entry() {
    let (cache_dir, source, stage) = (
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
    );
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &r, b"rcode")],
        },
    );

    let unknown = fetch(
        &cache,
        FetchRequest {
            key: key(9),
            expect: expect(&["r/x.r"]),
        },
        stage.path(),
    );
    let other = fetch(
        &cache,
        FetchRequest {
            key: key(1),
            expect: expect(&["r/x.r", "xref/x.p.xref"]),
        },
        stage.path(),
    );

    assert_eq!(
        unknown,
        FetchOutcome::Miss {
            key: key(9),
            reason: MissReason::NotFound
        }
    );
    assert_eq!(
        other,
        FetchOutcome::Miss {
            key: key(1),
            reason: MissReason::PathMismatch
        }
    );
    assert_eq!(quarantined(cache_dir.path()), 0);
    assert_eq!(entries(cache_dir.path()).len(), 1);
    assert!(
        !stage.path().join("r/x.r").exists(),
        "nothing was installed"
    );
}

#[test]
fn a_second_store_of_a_key_is_already_present() {
    let (cache_dir, source) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    let set = || ArtifactSetRef {
        key: key(1),
        files: vec![reference("r/x.r", &r, b"rcode")],
    };

    assert_eq!(store(&cache, set()), StoreOutcome::Stored);
    assert_eq!(store(&cache, set()), StoreOutcome::AlreadyPresent);
    assert_eq!(entries(cache_dir.path()).len(), 1);
}

#[test]
fn a_file_that_is_not_what_the_build_verified_is_not_stored() {
    let (cache_dir, source) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"other bytes");
    let same_size = file(source.path(), "y.r", b"rcodX");

    let wrong_size = store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &r, b"rcode")],
        },
    );
    let wrong_hash = store(
        &cache,
        ArtifactSetRef {
            key: key(2),
            files: vec![reference("r/y.r", &same_size, b"rcode")],
        },
    );

    assert_eq!(
        wrong_size,
        StoreOutcome::NotStored(NotStoredReason::ChangedOnDisk)
    );
    assert_eq!(
        wrong_hash,
        StoreOutcome::NotStored(NotStoredReason::ChangedOnDisk)
    );
    assert!(entries(cache_dir.path()).is_empty());
}

#[test]
fn a_symbolic_link_in_the_final_component_and_a_missing_file_are_unreadable() {
    let (cache_dir, source) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let cache = open(&cache_dir);
    let real = file(source.path(), "real.r", b"rcode");
    let link = source.path().join("link.r");
    std::os::unix::fs::symlink(&real, &link).unwrap();

    let linked = store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &link, b"rcode")],
        },
    );
    let missing = store(
        &cache,
        ArtifactSetRef {
            key: key(2),
            files: vec![reference(
                "r/y.r",
                &source.path().join("absent.r"),
                b"rcode",
            )],
        },
    );

    assert_eq!(linked, StoreOutcome::NotStored(NotStoredReason::Unreadable));
    assert_eq!(
        missing,
        StoreOutcome::NotStored(NotStoredReason::Unreadable)
    );
    assert!(entries(cache_dir.path()).is_empty());
}

#[test]
fn a_link_in_a_directory_above_the_file_is_allowed() {
    let (cache_dir, source) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let cache = open(&cache_dir);
    let real_dir = source.path().join("real");
    std::fs::create_dir(&real_dir).unwrap();
    file(&real_dir, "x.r", b"rcode");
    std::os::unix::fs::symlink(&real_dir, source.path().join("linked")).unwrap();
    let through = source.path().join("linked/x.r");

    let outcome = store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &through, b"rcode")],
        },
    );

    assert_eq!(outcome, StoreOutcome::Stored);
}

#[test]
fn artifact_paths_that_could_leave_the_tree_are_rejected() {
    let (cache_dir, source) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    for (n, bad) in [
        "../x.r",
        "/etc/x.r",
        "r/../../x.r",
        "other/x.r",
        "r",
        "r//x.r",
    ]
    .iter()
    .enumerate()
    {
        let outcome = store(
            &cache,
            ArtifactSetRef {
                key: key(n as u8 + 1),
                files: vec![reference(bad, &r, b"rcode")],
            },
        );
        assert_eq!(
            outcome,
            StoreOutcome::NotStored(NotStoredReason::Rejected),
            "{bad}"
        );
    }
    assert!(entries(cache_dir.path()).is_empty());
}

#[test]
fn an_entry_whose_manifest_was_edited_is_set_aside_not_trusted() {
    let (cache_dir, source, stage) = (
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
    );
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &r, b"rcode")],
        },
    );
    let manifest = entries(cache_dir.path())[0].join("manifest");
    let text = std::fs::read_to_string(&manifest).unwrap();
    std::fs::write(
        &manifest,
        text.replace("origin compiled", "origin remote shared mallory"),
    )
    .unwrap();

    let outcome = fetch(
        &cache,
        FetchRequest {
            key: key(1),
            expect: expect(&["r/x.r"]),
        },
        stage.path(),
    );

    assert_eq!(
        outcome,
        FetchOutcome::Miss {
            key: key(1),
            reason: MissReason::Corrupt
        }
    );
    assert_eq!(quarantined(cache_dir.path()), 1);
}

#[test]
fn every_request_of_one_call_is_answered_once() {
    let (cache_dir, source, stage) = (
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
    );
    let cache = open(&cache_dir);
    let mut requests = Vec::new();
    for n in 1..=20u8 {
        let bytes = format!("rcode {n}");
        let path = file(source.path(), &format!("{n}.r"), bytes.as_bytes());
        if n % 2 == 0 {
            store(
                &cache,
                ArtifactSetRef {
                    key: key(n),
                    files: vec![reference(&format!("r/{n}.r"), &path, bytes.as_bytes())],
                },
            );
        }
        requests.push(FetchRequest {
            key: key(n),
            expect: expect(&[&format!("r/{n}.r")]),
        });
    }
    let mut hits = 0;
    let mut misses = 0;
    let mut keys = Vec::new();

    cache.fetch(requests, stage.path(), &mut |outcome| match outcome {
        FetchOutcome::Installed { key, .. } => {
            hits += 1;
            keys.push(key);
        }
        FetchOutcome::Miss { key, reason } => {
            misses += 1;
            keys.push(key);
            assert_eq!(reason, MissReason::NotFound);
        }
    });

    assert_eq!((hits, misses), (10, 10));
    keys.sort_by_key(|k| k.0);
    keys.dedup();
    assert_eq!(keys.len(), 20);
}

#[test]
fn a_manifest_that_names_a_path_outside_the_tree_is_never_installed() {
    let (cache_dir, source, outer) = (
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
        TempDir::new().unwrap(),
    );
    let stage = outer.path().join("stage");
    std::fs::create_dir(&stage).unwrap();
    let cache = open(&cache_dir);
    let r = file(source.path(), "x.r", b"rcode");
    store(
        &cache,
        ArtifactSetRef {
            key: key(1),
            files: vec![reference("r/x.r", &r, b"rcode")],
        },
    );
    let entry = entries(cache_dir.path())[0].clone();
    std::fs::write(entry.join("escape.r"), b"rcode").unwrap();
    let hash = blake3::hash(b"rcode").to_hex();
    let lines = format!(
        "oxabl-artifacts 1\nkey {}\norigin compiled\nfile 5 {hash} r/../../escape.r\n",
        key(1).hex()
    );
    let end = blake3::hash(lines.as_bytes()).to_hex();
    std::fs::write(entry.join("manifest"), format!("{lines}end {end}\n")).unwrap();

    let outcome = fetch(
        &cache,
        FetchRequest {
            key: key(1),
            expect: expect(&["r/../../escape.r"]),
        },
        &stage,
    );

    assert_eq!(
        outcome,
        FetchOutcome::Miss {
            key: key(1),
            reason: MissReason::Corrupt
        }
    );
    assert!(
        !outer.path().join("escape.r").exists(),
        "a file was written outside the stage"
    );
    assert_eq!(quarantined(cache_dir.path()), 1);
}
