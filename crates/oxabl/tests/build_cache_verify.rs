//! What a unit must pass before a cache stores its outputs: its inputs still
//! hold the bytes of the key (V1), the schema is still the probe's (V2), and
//! the compiler opened no file outside the closure (V3).

mod common;

use std::time::{Duration, Instant};

use common::built::Built;
use common::cache::{entries, entry_with};
use common::env_with_schema;
use serde_json::{Value, json};
use tempfile::TempDir;

/// Waits until the fake logged the start of `name`.
fn wait_for_compile(workspace: &Built, name: &str) {
    let deadline = Instant::now() + Duration::from_secs(30);
    let suffix = format!(" {name}");
    while Instant::now() < deadline {
        let text = std::fs::read_to_string(workspace.log()).unwrap_or_default();
        if text.lines().any(|line| line.ends_with(&suffix)) {
            return;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    panic!("{name} never started");
}

fn holds(cache: &TempDir, artifact: &str) -> bool {
    entries(cache.path())
        .iter()
        .any(|entry| entry.join("files").join(artifact).is_file())
}

#[test]
fn a_closure_file_edited_while_the_unit_compiles_blocks_the_cache_write() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/inc/a.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    workspace.write("src/first.p", "{inc/a.i}\n");
    workspace.write("src/slow.p", "/* FAKE-SLOW */\n{inc/a.i}\n");
    workspace.write("src/other.p", "MESSAGE \"other\".\n");

    let child = workspace.spawn(&[]);
    wait_for_compile(&workspace, "slow.p");
    workspace.write("src/inc/a.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert!(holds(&cache, "r/other.r"), "the untouched unit is cached");
    assert!(
        holds(&cache, "r/first.r"),
        "a unit that finished before the edit is cached, though it shares the include"
    );
    assert!(!holds(&cache, "r/slow.r"), "the edited unit is not");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("a file changed while they compiled"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    workspace.clear_log();
    workspace.build().succeeded();
    let mut compiled = workspace.compiled();
    compiled.sort();
    assert_eq!(
        compiled,
        ["first.p", "slow.p"],
        "the next build compiles both includers again"
    );
    assert!(holds(&cache, "r/slow.r"), "and now caches it");
}

fn dlc_workspace(cache: &TempDir) -> Built {
    let workspace = Built::with("include_paths = [\"src\", \"$DLC/src\"]\ndlc = \"dlc\"", "");
    workspace.cache_in(cache.path());
    workspace.set_environment(&json!({ "dlc_dir": "worker-dlc", "env": { "dlc": "/usr/dlc" } }));
    let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";
    workspace.write("dlc/src/adecomm/x.i", text);
    workspace.write("worker-dlc/src/adecomm/x.i", text);
    workspace
}

#[test]
fn a_dlc_file_that_changes_in_the_worker_copy_while_the_unit_compiles_blocks_the_cache_write() {
    let cache = TempDir::new().unwrap();
    let workspace = dlc_workspace(&cache);
    workspace.write("src/slow.p", "/* FAKE-SLOW */\n{adecomm/x.i}\n");
    workspace.write("src/other.p", "MESSAGE \"other\".\n");

    let child = workspace.spawn(&[]);
    wait_for_compile(&workspace, "slow.p");
    workspace.write(
        "worker-dlc/src/adecomm/x.i",
        "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n",
    );
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert!(holds(&cache, "r/other.r"));
    assert!(
        !holds(&cache, "r/slow.r"),
        "the host mirror did not change, but the copy the AVM compiles with did"
    );
}

#[test]
fn a_unit_whose_dlc_copies_agree_and_stay_the_same_is_cached() {
    let cache = TempDir::new().unwrap();
    let workspace = dlc_workspace(&cache);
    workspace.write("src/a.p", "{adecomm/x.i}\n");

    workspace.build().succeeded();

    assert!(holds(&cache, "r/a.r"));
}

#[test]
fn a_read_outside_the_closure_blocks_the_write_leaves_the_result_without_a_key_and_names_the_file()
{
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("data/secret.txt", "a file the analysis cannot see\n");
    workspace.write(
        "src/reader.p",
        "/* FAKE-READ data/secret.txt */\nMESSAGE \"r\".\n",
    );
    workspace.write("src/plain.p", "MESSAGE \"p\".\n");
    let report = workspace.path("report.json");

    let run = workspace.build_with(&["--report", &report.to_string_lossy()]);

    run.succeeded();
    assert!(holds(&cache, "r/plain.r"));
    assert!(!holds(&cache, "r/reader.r"));
    assert!(
        run.stderr.contains("data/secret.txt") && run.stderr.contains("report"),
        "the summary names the file and asks for a bug report: {}",
        run.stderr
    );
    let report: Value = serde_json::from_str(&workspace.read("report.json")).unwrap();
    assert!(report["notes"].to_string().contains("data/secret.txt"));
    let plan = workspace.plan();
    assert_eq!(Built::reasons(&plan, "src/reader.p"), ["unverified"]);
    assert!(
        plan.to_string().contains("data/secret.txt"),
        "the reason names the file: {plan}"
    );
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(
        workspace.compiled(),
        ["reader.p"],
        "it compiles at each build"
    );
    assert!(!holds(&cache, "r/reader.r"));
}

#[test]
fn an_include_that_the_compiler_resolved_to_another_file_blocks_the_write() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/defs.i", "&SCOPED-DEFINE a 1\n");
    workspace.write("other/defs.i", "&SCOPED-DEFINE a 2\n");
    workspace.write(
        "src/user.p",
        "/* FAKE-RESOLVE-OTHER defs.i other/defs.i */\n{defs.i}\nMESSAGE \"u\".\n",
    );
    workspace.write("src/plain.p", "MESSAGE \"p\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(holds(&cache, "r/plain.r"));
    assert!(
        !holds(&cache, "r/user.r"),
        "the divergent resolution is not cached"
    );
    assert!(run.stderr.contains("other/defs.i"), "{}", run.stderr);
}

#[test]
fn a_unit_whose_include_argument_holds_a_greater_than_sign_is_cached() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/set.i", "MESSAGE {1}.\n");
    workspace.write(
        "src/user.p",
        "{set.i \"(IF 'a' <> 'b' THEN 'c' ELSE 'd')\"}\n",
    );

    let run = workspace.build();

    run.succeeded();
    assert!(
        holds(&cache, "r/user.r"),
        "the XREF-XML of the compile is read whole: {}",
        run.stderr
    );
}

const RECORDS: &str = r#"[["db","ledger"],["file","customer"],["field","customer","name"]]"#;

fn schema_environment() -> Value {
    let records: Vec<Vec<String>> = serde_json::from_str(RECORDS).unwrap();
    env_with_schema(records)
}

/// Four classes, each inheriting the one before: they compile one after the
/// other on one worker, and each is checked on its own (`batch_units = 1`).
fn class_chain(cache: &TempDir) -> Built {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.verify]\nbatch_units = 1",
    );
    workspace.cache_in(cache.path());
    workspace.set_environment(&schema_environment());
    workspace.databases(&["ledger"]);
    workspace.write("src/pkg/C1.cls", "CLASS pkg.C1:\nEND CLASS.\n");
    for n in 2..=4 {
        workspace.write(
            &format!("src/pkg/C{n}.cls"),
            &format!("CLASS pkg.C{n} INHERITS pkg.C{}:\nEND CLASS.\n", n - 1),
        );
    }
    workspace
}

#[test]
fn a_schema_that_changes_during_the_build_stops_the_cache_writes_from_that_point() {
    let cache = TempDir::new().unwrap();
    let workspace = class_chain(&cache);
    workspace.env("FAKE_AVM_SCHEMA_CHANGE_AFTER", Some("2"));

    let run = workspace.build();

    run.succeeded();
    assert_eq!(workspace.compiled().len(), 4);
    assert!(holds(&cache, "r/pkg/C1.r"), "released before the change");
    for later in ["C2", "C3", "C4"] {
        assert!(
            !holds(&cache, &format!("r/pkg/{later}.r")),
            "{later} was released after the change"
        );
    }
    assert!(
        run.stderr
            .contains("schema of ledger differs from the probe"),
        "{}",
        run.stderr
    );
}

#[test]
fn a_schema_that_changes_later_leaves_the_units_before_it_cached() {
    let cache = TempDir::new().unwrap();
    let workspace = class_chain(&cache);
    workspace.env("FAKE_AVM_SCHEMA_CHANGE_AFTER", Some("3"));

    workspace.build().succeeded();

    assert!(holds(&cache, "r/pkg/C1.r") && holds(&cache, "r/pkg/C2.r"));
    assert!(!holds(&cache, "r/pkg/C3.r") && !holds(&cache, "r/pkg/C4.r"));
}

#[test]
fn a_schema_that_does_not_change_is_imaged_again_and_every_unit_is_cached() {
    let cache = TempDir::new().unwrap();
    let workspace = class_chain(&cache);

    workspace.build().succeeded();

    assert_eq!(entries(cache.path()).len(), 4);
    let images = workspace
        .log_lines()
        .into_iter()
        .filter(|(_, what)| what.starts_with("schema ledger"))
        .count();
    assert!(images >= 2, "the probe and at least one check: {images}");
}

#[test]
fn a_schema_that_changes_between_the_last_check_and_the_end_is_caught_by_the_final_check() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with("include_paths = [\"src\"]", "");
    workspace.cache_in(cache.path());
    workspace.set_environment(&schema_environment());
    workspace.databases(&["ledger"]);
    for name in ["a", "b", "c"] {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"x\".\n");
    }
    workspace.env("FAKE_AVM_SCHEMA_CHANGE_AFTER", Some("3"));

    let run = workspace.build();

    run.succeeded();
    assert_eq!(
        entries(cache.path()).len(),
        0,
        "the default batch holds every unit until the end, and the end shows the change"
    );
    assert!(run.stderr.contains("schema of ledger"), "{}", run.stderr);
}

#[test]
fn workers_that_see_other_dlc_copies_stop_the_build_before_any_compile() {
    let workspace = Built::with("include_paths = [\"src\", \"$DLC/src\"]\ndlc = \"dlc\"", "");
    workspace.workers("count = 2");
    workspace.write("dlc/src/adecomm/x.i", "one\n");
    workspace.write("worker-a/src/adecomm/x.i", "one\n");
    workspace.write("worker-b/src/adecomm/x.i", "two\n");
    workspace.per_worker_environments(&[
        json!({ "dlc_dir": "worker-a", "env": { "dlc": "/usr/dlc" } }),
        json!({ "dlc_dir": "worker-b", "env": { "dlc": "/usr/dlc" } }),
    ]);
    workspace.write("src/a.p", "{adecomm/x.i}\n");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(
        run.stderr.contains("workers see different $DLC files"),
        "{}",
        run.stderr
    );
    assert!(workspace.compiled().is_empty());
}

#[test]
fn no_compile_starts_before_the_dlc_hash_phase_is_over_and_the_outputs_are_pruned() {
    let workspace = Built::with("include_paths = [\"src\", \"$DLC/src\"]\ndlc = \"dlc\"", "");
    workspace.workers("count = 2");
    workspace.set_environment(&json!({ "dlc_dir": "worker-dlc", "env": { "dlc": "/usr/dlc" } }));
    workspace.write("dlc/src/adecomm/x.i", "one\n");
    workspace.write("worker-dlc/src/adecomm/x.i", "one\n");
    workspace.write("src/a.p", "{adecomm/x.i}\n");
    workspace.build().succeeded();
    let stray = workspace.path("build/r/stray.r");
    std::fs::write(&stray, "left by someone\n").unwrap();
    workspace.write("src/a.p", "{adecomm/x.i}\nMESSAGE \"changed\".\n");
    let requests = workspace.path("requests.log");
    workspace.env("FAKE_AVM_REQUEST_LOG", Some(&requests.to_string_lossy()));
    workspace.env("FAKE_AVM_WATCH", Some(&stray.to_string_lossy()));

    workspace.build().succeeded();

    let text = std::fs::read_to_string(&requests).unwrap();
    let lines: Vec<(u128, &str)> = text
        .lines()
        .map(|line| {
            let mut parts = line.splitn(3, ' ');
            (
                parts.next().unwrap().parse().unwrap(),
                parts.nth(1).unwrap(),
            )
        })
        .collect();
    let last_hash = lines
        .iter()
        .filter(|(_, what)| what.starts_with("HASH"))
        .map(|(at, _)| *at)
        .max()
        .expect("the DLC hash phase ran");
    let compiles: Vec<&(u128, &str)> = lines
        .iter()
        .filter(|(_, what)| what.starts_with("COMPILE"))
        .collect();
    assert!(!compiles.is_empty());
    for (at, what) in &compiles {
        assert!(
            *at > last_hash,
            "a COMPILE came before the last HASH: {text}"
        );
        assert!(
            what.ends_with("=false"),
            "the stray output still existed at the first compile: {what}"
        );
    }
    assert!(!stray.exists(), "the prune deleted it");
}

#[test]
fn with_the_read_check_off_nothing_is_written_and_the_summary_says_so() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with("include_paths = [\"src\"]", "[build.verify]\nreads = false");
    workspace.cache_in(cache.path());
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(entries(cache.path()).is_empty());
    assert!(run.stderr.contains("reads = false"), "{}", run.stderr);
}

#[test]
fn with_text_xref_nothing_is_written_and_the_build_says_so() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"\nxref_format = \"text\"",
    );
    workspace.cache_in(cache.path());
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(entries(cache.path()).is_empty());
    assert!(run.stderr.contains("xref_format"), "{}", run.stderr);
    assert!(workspace.exists("build/xref/a.p.xref"));
}

#[test]
fn a_requested_xref_xml_serves_as_the_private_one() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"",
    );
    workspace.cache_in(cache.path());
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    workspace.build().succeeded();

    let entry = entry_with(cache.path(), "r/a.r");
    assert!(entry.join("files/xref/a.p.xref.xml").is_file());
    assert!(
        !workspace.exists("build/verify"),
        "the private XREF is not an output"
    );
}

#[test]
fn the_private_xref_is_not_left_in_the_workspace() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    workspace.build().succeeded();

    assert!(holds(&cache, "r/a.r"));
    assert!(
        !workspace.exists(".oxabl/build/stage"),
        "the stage of the build is removed when it ends"
    );
    let files = common::cache::tree(&workspace.path(".oxabl"));
    assert!(
        files.keys().all(|path| !path.ends_with(".xref.xml")),
        "{:?}",
        files.keys().collect::<Vec<_>>()
    );
}

#[test]
fn rcode_in_the_output_directory_is_outside_the_closure_of_a_unit_with_no_class() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/other.p", "MESSAGE \"other\".\n");
    workspace.build().succeeded();
    workspace.write(
        "src/reader.p",
        "/* FAKE-READ build/r/other.r */\nMESSAGE \"r\".\n",
    );
    workspace.clear_log();

    let run = workspace.build();

    run.succeeded();
    assert_eq!(workspace.compiled(), ["reader.p"]);
    assert!(!holds(&cache, "r/reader.r"));
    assert!(run.stderr.contains("build/r/other.r"), "{}", run.stderr);
}

#[test]
fn a_unit_that_finished_during_a_schema_check_waits_for_the_next_one() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.verify]\nbatch_units = 1",
    );
    workspace.cache_in(cache.path());
    workspace.workers("count = 2");
    workspace.databases(&["ledger"]);
    let records: Vec<Vec<String>> =
        serde_json::from_str(r#"[["db","ledger"],["file","customer"]]"#).unwrap();
    workspace.set_environment(&env_with_schema(records));
    let marker = workspace.path("schema-changed");
    workspace.env(
        "FAKE_AVM_SCHEMA_CHANGE_FILE",
        Some(&marker.to_string_lossy()),
    );
    workspace.env("FAKE_AVM_SCHEMA_DELAY_MS", Some("3000"));
    workspace.write("src/a.p", "MESSAGE \"fast\".\n");
    workspace.write("src/s.p", "/* FAKE-SLOW */\nMESSAGE \"slow\".\n");

    let child = workspace.spawn(&[]);
    wait_for_compile(&workspace, "s.p");
    std::thread::sleep(Duration::from_millis(500));
    std::fs::write(&marker, "").unwrap();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert!(
        holds(&cache, "r/a.r"),
        "a.p finished before the first check began, and the check found the schema of the probe"
    );
    assert!(
        !holds(&cache, "r/s.p".replace("s.p", "s.r").as_str()),
        "s.p finished after the first check began: only a later check may release it, and that one sees the change"
    );
}

#[test]
fn with_databases_and_the_default_batch_the_final_check_releases_every_unit() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::with("include_paths = [\"src\"]", "");
    workspace.cache_in(cache.path());
    workspace.databases(&["ledger"]);
    workspace.set_environment(&schema_environment());
    for name in ["a", "b", "c"] {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"x\".\n");
    }

    workspace.build().succeeded();

    assert_eq!(entries(cache.path()).len(), 3);
}

#[test]
fn a_schema_that_changes_back_does_not_start_the_cache_writes_again() {
    let cache = TempDir::new().unwrap();
    let workspace = class_chain(&cache);
    let marker = workspace.path("schema-changed");
    workspace.env(
        "FAKE_AVM_SCHEMA_CHANGE_FILE",
        Some(&marker.to_string_lossy()),
    );
    for n in 2..=3 {
        let path = format!("src/pkg/C{n}.cls");
        let text = workspace.read(&path);
        workspace.write(&path, &format!("/* FAKE-SLOW */\n{text}"));
    }

    let child = workspace.spawn(&[]);
    wait_for_compile(&workspace, "pkg/C2.cls");
    std::fs::write(&marker, "").unwrap();
    wait_for_compile(&workspace, "pkg/C3.cls");
    std::fs::remove_file(&marker).unwrap();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert!(holds(&cache, "r/pkg/C1.r"));
    for later in ["C2", "C3", "C4"] {
        assert!(
            !holds(&cache, &format!("r/pkg/{later}.r")),
            "{later}: the schema was seen changed during the build, and nothing after it is written"
        );
    }
}

#[test]
fn without_a_cache_the_databases_are_not_imaged_again() {
    let workspace = Built::with("include_paths = [\"src\"]", "");
    workspace.databases(&["ledger"]);
    workspace.set_environment(&schema_environment());
    workspace.write("src/a.p", "MESSAGE \"x\".\n");

    workspace.build().succeeded();

    let images = workspace
        .log_lines()
        .into_iter()
        .filter(|(_, what)| what.starts_with("schema ledger"))
        .count();
    assert_eq!(images, 1, "only the probe images the database");
}

#[test]
fn a_compile_with_no_readable_xref_xml_is_not_cached_and_has_no_key() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.env("FAKE_AVM_NO_PRIVATE_XREF", Some("1"));
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(entries(cache.path()).is_empty());
    assert!(run.stderr.contains("XREF-XML"), "{}", run.stderr);
    let plan = workspace.plan();
    assert_eq!(Built::reasons(&plan, "src/a.p"), ["unverified"]);
}

#[test]
fn a_schema_check_that_fails_releases_nothing_and_stops_the_writes() {
    let cache = TempDir::new().unwrap();
    let workspace = class_chain(&cache);
    workspace.env("FAKE_AVM_SCHEMA_ERROR_AFTER", Some("2"));

    let run = workspace.build();

    run.succeeded();
    assert!(holds(&cache, "r/pkg/C1.r"));
    for later in ["C2", "C3", "C4"] {
        assert!(!holds(&cache, &format!("r/pkg/{later}.r")), "{later}");
    }
    assert!(run.stderr.contains("not cached"), "{}", run.stderr);
}
