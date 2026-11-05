//! The local cache: two copies of a workspace share compile outputs through
//! it, and a damaged, partial or evicted entry is a miss.

mod common;

use std::collections::BTreeMap;

use common::built::{Built, of_kind, unit};
use common::cache::{entries, entry_files, entry_with, flip_a_byte, quarantined, set_origin, tree};
use serde_json::Value;
use tempfile::TempDir;

/// The same project at a new path: procedures, an include, two classes and a
/// user of them, with every optional output on.
fn project(extra: &str) -> Built {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        &format!(
            "[build.outputs]\nxref = \"build/xref\"\ndebug_list = \"build/dbg\"\nlisting = \"build/lst\"\n{extra}"
        ),
    );
    workspace.write(
        "src/inc/ledger.i",
        "DEFINE VARIABLE gl AS INTEGER NO-UNDO.\n",
    );
    workspace.write("src/ar/post.p", "{inc/ledger.i}\nMESSAGE \"post\".\n");
    workspace.write("src/pkg/Base.cls", "CLASS pkg.Base:\nEND CLASS.\n");
    workspace.write(
        "src/pkg/User.cls",
        "CLASS pkg.User INHERITS pkg.Base:\nEND CLASS.\n",
    );
    workspace.write(
        "src/main.p",
        "DEFINE VARIABLE o AS pkg.User NO-UNDO.\no = NEW pkg.User().\n",
    );
    workspace
}

fn cached(workspace: &Built, cache: &TempDir) {
    workspace.cache_in(cache.path());
}

/// Every output file of a workspace by path, the markers left out.
fn outputs(workspace: &Built) -> BTreeMap<String, Vec<u8>> {
    tree(&workspace.path("build"))
        .into_iter()
        .filter(|(path, _)| {
            !path.ends_with(".oxabl-output") && !path.ends_with(".oxabl-build.lock")
        })
        .collect()
}

/// Every `read <path> <sha256>` line of an r-code file names a file whose
/// bytes are what the line says: the wrong-hit oracle.
fn assert_reads_current(workspace: &Built, rcode: &str) {
    for (path, sha) in workspace.reads(rcode) {
        assert_eq!(
            workspace.sha256(&path),
            sha,
            "{rcode} read {path} with other bytes than the file has now"
        );
    }
}

#[test]
fn two_copies_of_a_workspace_share_a_local_cache_and_their_outputs_are_byte_identical() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    let run = first.build();
    run.succeeded();
    assert_eq!(first.compiled().len(), 4, "{:?}", first.compiled());
    assert!(run.stdout.contains("4 stored"), "{}", run.stdout);

    let second = project("");
    cached(&second, &cache);
    let run = second.build();

    run.succeeded();
    assert!(
        second.compiled().is_empty(),
        "the second copy compiled {:?}",
        second.compiled()
    );
    assert!(run.stdout.contains("fetched 4"), "{}", run.stdout);
    let (a, b) = (outputs(&first), outputs(&second));
    assert!(
        a.len() >= 16,
        "r-code, XREF, debug list and listing of four units: {}",
        a.len()
    );
    assert_eq!(
        a.keys().collect::<Vec<_>>(),
        b.keys().collect::<Vec<_>>(),
        "the same files"
    );
    for (path, bytes) in &a {
        assert!(bytes == &b[path], "{path} differs between the two copies");
    }

    let control = project("");
    let other = TempDir::new().unwrap();
    cached(&control, &other);
    control.build().succeeded();
    assert_ne!(
        outputs(&first)["r/main.r"],
        outputs(&control)["r/main.r"],
        "two compiles of one source differ, so equal bytes can only come from the cache"
    );
}

#[test]
fn a_flipped_byte_in_a_cached_rcode_is_a_miss_the_entry_is_quarantined_and_the_unit_compiles() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    let entry = entry_with(cache.path(), "r/main.r");
    flip_a_byte(&entry.join("files/r/main.r"));
    let damaged = std::fs::read(entry.join("files/r/main.r")).unwrap();

    let second = project("");
    cached(&second, &cache);
    let run = second.build();

    run.succeeded();
    assert_eq!(
        second.compiled(),
        ["main.p"],
        "only the damaged entry is a miss"
    );
    assert_eq!(quarantined(cache.path()), 1);
    assert_ne!(
        std::fs::read(entry.join("files/r/main.r")).unwrap(),
        damaged,
        "the compile wrote a sound entry in its place"
    );
    let installed = second.read("build/r/main.r");
    assert_ne!(installed.as_bytes(), damaged.as_slice());
    assert!(installed.starts_with("oxabl-fake-r 1"), "{installed}");
    assert_reads_current(&second, "build/r/main.r");
}

#[test]
fn a_file_missing_from_an_entry_is_a_miss_and_nothing_of_the_entry_is_installed() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    let entry = entry_with(cache.path(), "r/main.r");
    std::fs::remove_file(entry.join("files/xref/main.p.xref.xml")).unwrap();
    let cached_rcode = std::fs::read(entry.join("files/r/main.r")).unwrap();

    let second = project("");
    cached(&second, &cache);
    second.build().succeeded();

    assert_eq!(second.compiled(), ["main.p"]);
    assert_ne!(
        second.read("build/r/main.r").into_bytes(),
        cached_rcode,
        "the r-code of the damaged entry was not installed"
    );
    assert_eq!(quarantined(cache.path()), 1);
    assert_reads_current(&second, "build/r/main.r");
    assert!(second.exists("build/xref/main.p.xref.xml"));
}

#[test]
fn an_entry_that_holds_other_outputs_than_the_build_asks_for_is_a_miss_and_stays() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();

    let plain = Built::new();
    plain.write("src/main.p", "MESSAGE \"main\".\n");
    cached(&plain, &cache);
    plain.build().succeeded();

    assert_eq!(plain.compiled(), ["main.p"]);
    assert_eq!(
        quarantined(cache.path()),
        0,
        "a good entry is never set aside"
    );
}

#[test]
fn a_cache_that_fills_over_its_limit_drops_the_oldest_entries_and_a_later_fetch_of_one_is_a_miss() {
    let cache = TempDir::new().unwrap();
    let make = || {
        let workspace = Built::with(
            "include_paths = [\"src\"]",
            "[build.cache]\nlocal_max_size = \"1KiB\"",
        );
        for n in 0..12 {
            workspace.write(&format!("src/u{n:02}.p"), &format!("MESSAGE \"{n}\".\n"));
        }
        cached(&workspace, &cache);
        workspace
    };
    let first = make();
    first.build().succeeded();
    let left = entries(cache.path()).len();
    assert!(left > 0 && left < 12, "{left} entries remain of 12");

    let second = make();
    let run = second.build();

    run.succeeded();
    assert_eq!(
        second.compiled().len(),
        12 - left,
        "a unit whose entry was evicted compiles; the others are fetched"
    );
}

#[test]
fn a_cache_directory_that_others_can_write_gives_a_warning_a_correct_build_and_no_cache_use() {
    use std::os::unix::fs::PermissionsExt;
    let cache = TempDir::new().unwrap();
    std::fs::set_permissions(cache.path(), std::fs::Permissions::from_mode(0o777)).unwrap();
    let workspace = project("");
    cached(&workspace, &cache);

    let run = workspace.build();

    run.succeeded();
    assert_eq!(workspace.compiled().len(), 4);
    assert!(
        run.stderr.contains("the local cache is not used"),
        "{}",
        run.stderr
    );
    assert!(!cache.path().join("v1").exists(), "nothing was written");
}

#[test]
fn a_local_entry_that_claims_a_remote_origin_is_reported_as_unverified() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    set_origin(&entry_with(cache.path(), "r/main.r"), "remote shared bob");

    let second = project("");
    cached(&second, &cache);
    let report = second.path("report.json");
    second
        .build_with(&["--report", &report.to_string_lossy()])
        .succeeded();

    let report: Value = serde_json::from_str(&std::fs::read_to_string(report).unwrap()).unwrap();
    let main = unit(&report, "src/main.p");
    assert_eq!(main["source"], "local-cache");
    assert_eq!(main["origin_unverified"], "remote shared bob");
    assert_eq!(
        unit(&report, "src/ar/post.p")["origin_unverified"],
        Value::Null
    );
    assert_eq!(report["summary"]["fetched"], 4);
}

#[test]
fn a_fetched_unit_has_an_event_a_recorded_result_and_is_not_compiled_again() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();

    let second = project("");
    cached(&second, &cache);
    let run = second.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    assert_eq!(of_kind(&events, "unit-fetched").len(), 4);
    assert!(
        of_kind(&events, "unit-fetched")
            .iter()
            .all(|event| event["cache"] == "local")
    );
    assert!(of_kind(&events, "unit-started").is_empty());
    let summary = of_kind(&events, "summary")[0];
    assert_eq!(summary["fetched"], 4);
    assert_eq!(summary["compiled"], 0);
    second.clear_log();
    second.build().succeeded();
    assert!(
        second.compiled().is_empty(),
        "the fetch was recorded as a result"
    );
    let plan = second.plan();
    assert_eq!(plan["summary"]["rebuild"], 0);
}

#[test]
fn a_deleted_output_is_restored_from_the_cache_without_a_compile() {
    let cache = TempDir::new().unwrap();
    let workspace = project("");
    cached(&workspace, &cache);
    workspace.build().succeeded();
    workspace.remove("build/r/main.r");
    workspace.clear_log();

    let run = workspace.build();

    run.succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
    assert!(workspace.exists("build/r/main.r"));
    assert!(run.stdout.contains("fetched 1"), "{}", run.stdout);
}

#[test]
fn no_cache_neither_reads_nor_writes() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    let before = entries(cache.path()).len();

    let second = project("");
    cached(&second, &cache);
    second.build_with(&["--no-cache"]).succeeded();

    assert_eq!(second.compiled().len(), 4);
    assert_eq!(entries(cache.path()).len(), before);
    let third = project("[build.cache]\nenabled = false");
    cached(&third, &cache);
    third.build().succeeded();
    assert_eq!(
        third.compiled().len(),
        4,
        "enabled = false is no cache either"
    );
}

#[test]
fn a_user_of_a_class_is_cached_although_the_compiler_read_the_class_rcode() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    let run = first.build_with(&["--report", &first.path("r.json").to_string_lossy()]);
    run.succeeded();

    let report: Value = serde_json::from_str(&first.read("r.json")).unwrap();
    assert!(
        first
            .reads("build/r/pkg/User.r")
            .iter()
            .any(|(path, _)| path == "build/r/pkg/Base.r"),
        "User.r read the r-code of Base"
    );
    assert!(entry_with(cache.path(), "r/pkg/User.r").is_dir());
    assert_eq!(report["cache"]["stored"], 4);
    assert!(
        !run.stderr.contains("outside the closure"),
        "{}",
        run.stderr
    );
}

#[test]
fn the_entries_are_built_from_the_files_of_the_build() {
    let cache = TempDir::new().unwrap();
    let workspace = project("");
    cached(&workspace, &cache);
    workspace.build().succeeded();

    let found = entries(cache.path());
    assert_eq!(found.len(), 4);
    let paths: Vec<String> = entry_files(&entry_with(cache.path(), "r/main.r"))
        .into_iter()
        .map(|(path, _)| path)
        .collect();
    assert_eq!(
        paths,
        [
            "dbg/main.p.dbg",
            "lst/main.p.lst",
            "r/main.r",
            "xref/main.p.xref.xml"
        ]
    );
    let manifest = std::fs::read_to_string(found[0].join("manifest")).unwrap();
    assert!(
        manifest.starts_with("oxabl-artifacts 1\nkey "),
        "{manifest}"
    );
    assert!(manifest.contains("\norigin compiled\n"), "{manifest}");
    assert!(
        manifest
            .trim_end()
            .lines()
            .last()
            .unwrap()
            .starts_with("end ")
    );
}

#[test]
fn an_entry_that_a_build_used_outlives_the_entries_that_nobody_used() {
    let cache = TempDir::new().unwrap();
    let make = |names: &[&str], limit: &str| {
        let workspace = Built::with(
            "include_paths = [\"src\"]",
            &format!("[build.cache]\nlocal_max_size = \"{limit}\""),
        );
        for name in names {
            workspace.write(&format!("src/{name}.p"), &format!("MESSAGE \"{name}\".\n"));
        }
        cached(&workspace, &cache);
        workspace
    };
    let all: Vec<String> = (0..12).map(|n| format!("u{n:02}")).collect();
    let all_refs: Vec<&str> = all.iter().map(String::as_str).collect();
    make(&all_refs, "1GiB").build().succeeded();
    assert_eq!(entries(cache.path()).len(), 12);
    std::thread::sleep(std::time::Duration::from_millis(50));
    let used = make(&all_refs[..4], "1GiB");
    used.build().succeeded();
    assert!(used.compiled().is_empty(), "the four units were fetched");
    std::thread::sleep(std::time::Duration::from_millis(50));

    make(&["fresh"], "1KiB").build().succeeded();

    let held = |name: &str| {
        entries(cache.path())
            .iter()
            .any(|entry| entry.join(format!("files/r/{name}.r")).is_file())
    };
    assert!(entries(cache.path()).len() < 13, "something was evicted");
    for name in &all_refs[..4] {
        assert!(held(name), "{name} was used last and was evicted");
    }
    assert!(held("fresh"));
}

#[test]
fn an_entry_filed_under_another_key_is_a_miss_and_is_set_aside() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    let one = entry_with(cache.path(), "r/main.r");
    let other = entry_with(cache.path(), "r/ar/post.r");
    let parked = cache.path().join("parked");
    std::fs::rename(&one, &parked).unwrap();
    std::fs::rename(&other, &one).unwrap();
    std::fs::rename(&parked, &other).unwrap();

    let second = project("");
    cached(&second, &cache);
    second.build().succeeded();

    let mut compiled = second.compiled();
    compiled.sort();
    assert_eq!(
        compiled,
        ["ar/post.p", "main.p"],
        "the two swapped entries are misses"
    );
    assert_eq!(quarantined(cache.path()), 2);
}

#[test]
fn the_quarantine_is_emptied_when_the_cache_is_cleaned_up() {
    let cache = TempDir::new().unwrap();
    let first = project("");
    cached(&first, &cache);
    first.build().succeeded();
    flip_a_byte(&entry_with(cache.path(), "r/main.r").join("files/r/main.r"));
    let second = project("");
    cached(&second, &cache);
    second.build().succeeded();
    assert_eq!(quarantined(cache.path()), 1);

    let small = Built::with(
        "include_paths = [\"src\"]",
        "[build.cache]\nlocal_max_size = \"1KiB\"",
    );
    for n in 0..12 {
        small.write(&format!("src/q{n:02}.p"), &format!("MESSAGE \"{n}\".\n"));
    }
    cached(&small, &cache);
    small.build().succeeded();

    assert_eq!(
        quarantined(cache.path()),
        0,
        "an eviction deletes what was set aside"
    );
}
