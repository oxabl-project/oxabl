//! A unit whose closure oxabl cannot read from its source (a construct the
//! analysis does not model, an include whose name it cannot compute) gets a
//! key from the closure that its compiler reported at the last compile: the
//! observed closure. Encrypted source never does: the compiler reports no
//! include made inside it.

mod common;

use std::path::Path;
use std::time::Duration;

use common::built::{Built, of_kind, unit};
use common::cache::entries;
use serde_json::Value;
use tempfile::TempDir;

/// Text that the analysis cannot parse, with no control byte: the source is
/// not encrypted. The fake AVM still finds the includes written after it.
const UNREADABLE: &str = "§ ¤ 0x9f ((( %%% .\n";

/// The first bytes of an XCODE-encrypted source: a control byte first.
const ENCRYPTED: &str = "\u{11}Y/%\u{7}\t1I;% ((( %%% .\n";

/// A file that the refresh did not hash before a compile counts only when it
/// was last changed well before the build: wait until the files written now
/// are that old.
fn settle() {
    std::thread::sleep(Duration::from_millis(2_200));
}

fn holds(cache: &Path, artifact: &str) -> bool {
    entries(cache)
        .iter()
        .any(|entry| entry.join("files").join(artifact).is_file())
}

fn strings(value: &Value) -> Vec<String> {
    value
        .as_array()
        .unwrap_or_else(|| panic!("not a list: {value}"))
        .iter()
        .map(|item| item.as_str().unwrap().to_string())
        .collect()
}

#[test]
fn a_unit_oxabl_cannot_read_compiles_once_then_only_when_a_file_it_read_changes() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/inc/seen.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write(
        "src/inc/hidden.i",
        "DEFINE VARIABLE b AS INTEGER NO-UNDO.\n",
    );
    workspace.write(
        "src/enc.p",
        &format!("{UNREADABLE}{{inc/seen.i &PASS}}\n/* {{\"inc/hidden.i\" &company = x}} */\n"),
    );
    settle();

    let first = workspace.plan();
    assert_eq!(unit(&first, "src/enc.p")["key"], Value::Null, "{first}");
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
    assert!(
        !holds(cache.path(), "r/enc.r"),
        "a compile without a key before it is not cached"
    );

    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["action"], "up-to-date", "{plan}");
    assert_eq!(enc["closure"], "observed", "{plan}");
    assert!(enc["key"].is_string(), "{plan}");
    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "the second build compiles nothing: {:?}",
        workspace.compiled()
    );
    let again = workspace.plan();
    assert_eq!(
        again["store"]["generation"], plan["store"]["generation"],
        "the store keeps the files of the observed closure: a plan with no change changes nothing"
    );

    workspace.write(
        "src/inc/hidden.i",
        "DEFINE VARIABLE c AS INTEGER NO-UNDO.\n",
    );
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["action"], "rebuild", "{plan}");
    assert_eq!(
        Built::reasons(&plan, "src/enc.p"),
        ["dependency-changed"],
        "{plan}"
    );
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
    assert!(
        holds(cache.path(), "r/enc.r"),
        "a compile under an observed key is cached like any keyed compile"
    );

    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "after the change the unit is up to date again: {:?}",
        workspace.compiled()
    );
}

/// `src`, `lib` and `more` on the PROPATH; the unit's include is in `lib`.
fn three_entries() -> Built {
    let workspace = Built::with("include_paths = [\"src\", \"lib\", \"more\"]", "");
    workspace.write("lib/inc/b.i", "DEFINE VARIABLE b AS INTEGER NO-UNDO.\n");
    workspace.write("src/enc.p", &format!("{UNREADABLE}{{inc/b.i}}\n"));
    workspace.build().succeeded();
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["closure"], "observed", "{plan}");
    workspace.clear_log();
    workspace
}

#[test]
fn a_unit_whose_observed_closure_names_a_file_that_is_gone_has_no_key_and_compiles() {
    let workspace = three_entries();
    workspace.remove("lib/inc/b.i");
    workspace.write("more/inc/b.i", "DEFINE VARIABLE b AS INTEGER NO-UNDO.\n");

    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["action"], "rebuild", "{plan}");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        strings(&enc["unkeyable"]).contains(&"observed-missing: lib/inc/b.i".to_string()),
        "{plan}"
    );
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);

    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "the new closure keys the unit: {:?}",
        workspace.compiled()
    );
}

#[test]
fn a_new_file_where_an_include_of_the_observed_closure_resolves_first_takes_the_key_away() {
    let workspace = three_entries();
    workspace.write(
        "src/inc/b.i",
        "DEFINE VARIABLE shadow AS INTEGER NO-UNDO.\n",
    );

    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["action"], "rebuild", "{plan}");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        strings(&enc["unkeyable"]).contains(&"observed-shadowed: src/inc/b.i".to_string()),
        "{plan}"
    );
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
    let read: Vec<String> = workspace
        .reads("build/r/enc.r")
        .into_iter()
        .map(|(path, _)| path)
        .collect();
    assert!(read.contains(&"src/inc/b.i".to_string()), "{read:?}");
}

#[test]
fn an_observed_key_differs_from_the_parsed_key_of_the_same_files() {
    let workspace = Built::new();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write(
        "src/cond.p",
        "&IF CONNECTED(\"ledger\") &THEN\n{inc/a.i}\n&ENDIF\n",
    );
    workspace.build().succeeded();
    let observed = workspace.plan();
    let before = unit(&observed, "src/cond.p");
    assert_eq!(before["closure"], "observed", "{observed}");
    assert_eq!(before["action"], "up-to-date", "{observed}");

    // With the database list the analysis reads the unit, and finds the
    // closure the compiler reported: the same files, the same bytes.
    workspace.databases(&[]);
    let parsed = workspace.plan();
    let after = unit(&parsed, "src/cond.p");
    assert_eq!(after["closure"], "parsed", "{parsed}");
    assert!(after["key"].is_string(), "{parsed}");
    assert_ne!(after["key"], before["key"], "{observed}\n{parsed}");
    assert_eq!(after["action"], "rebuild", "{parsed}");
}

/// `src/pkg/Used.cls` (with an include), `src/pkg/Other.cls`, and a unit
/// oxabl cannot read that uses `pkg.Used`, as the compiler sees it.
fn class_user() -> Built {
    let workspace = Built::new();
    workspace.write("src/inc/used.i", "DEFINE VARIABLE u AS INTEGER NO-UNDO.\n");
    workspace.write(
        "src/pkg/Used.cls",
        "CLASS pkg.Used:\n{inc/used.i}\nEND CLASS.\n",
    );
    workspace.write("src/pkg/Other.cls", "CLASS pkg.Other:\nEND CLASS.\n");
    workspace.write("src/enc.p", &format!("{UNREADABLE}/* NEW pkg.Used() */\n"));
    workspace.build().succeeded();
    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["closure"], "observed", "{plan}");
    assert_eq!(enc["action"], "up-to-date", "{plan}");
    workspace.clear_log();
    workspace
}

#[test]
fn an_observed_key_holds_every_workspace_class_and_what_each_class_reads() {
    let workspace = class_user();

    workspace.write(
        "src/pkg/Other.cls",
        "CLASS pkg.Other:\n    DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\nEND CLASS.\n",
    );
    let plan = workspace.plan();
    assert_eq!(
        unit(&plan, "src/enc.p")["action"],
        "rebuild",
        "the XREF-XML does not say which classes the unit uses: any class counts: {plan}"
    );
    workspace.build().succeeded();
    workspace.clear_log();

    workspace.write("src/inc/used.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n");
    let plan = workspace.plan();
    assert_eq!(
        unit(&plan, "src/enc.p")["action"],
        "rebuild",
        "an include of a class is an input of its users: {plan}"
    );
    workspace.build().succeeded();
    let mut compiled = workspace.compiled();
    compiled.sort();
    assert_eq!(compiled, ["enc.p", "pkg/Used.cls"]);
}

#[test]
fn a_new_class_source_on_the_propath_takes_the_observed_key_away() {
    let workspace = class_user();
    workspace.write("src/pkg/New.cls", "CLASS pkg.New:\nEND CLASS.\n");

    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        strings(&enc["unkeyable"])
            .contains(&"observed-classes-changed: +src/pkg/New.cls".to_string()),
        "{plan}"
    );
    workspace.build().succeeded();
    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "the compile records the new list of classes: {:?}",
        workspace.compiled()
    );
}

#[test]
fn a_workspace_class_that_has_no_key_gives_no_observed_key() {
    let workspace = class_user();
    workspace.write(
        "src/pkg/Bad.cls",
        &format!("{UNREADABLE}CLASS pkg.Bad:\nEND CLASS.\n"),
    );
    workspace.build().succeeded();

    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    let reasons = strings(&enc["unkeyable"]);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.starts_with("observed-class: src/pkg/Bad.cls: degraded")),
        "{plan}"
    );
}

#[test]
fn a_unit_oxabl_cannot_read_compiles_after_every_class_unit_of_the_build() {
    let workspace = Built::new();
    workspace.workers("count = 2");
    workspace.write(
        "src/pkg/Slow.cls",
        "CLASS pkg.Slow:\n/* FAKE-SLOW */\nEND CLASS.\n",
    );
    workspace.write("src/a.p", &format!("{UNREADABLE}MESSAGE \"a\".\n"));

    let run = workspace.build_with(&["--events"]);
    run.succeeded();

    let events = run.events();
    let position = |kind: &str, path: &str| {
        events
            .iter()
            .position(|event| event["event"] == kind && event["path"] == path)
            .unwrap_or_else(|| panic!("no {kind} for {path}: {events:?}"))
    };
    assert_eq!(of_kind(&events, "unit-started").len(), 2, "{events:?}");
    assert!(
        position("unit-finished", "src/pkg/Slow.cls") < position("unit-started", "src/a.p"),
        "the unit oxabl cannot read can read the class's r-code: it must start after the class finished: {events:?}"
    );
}

#[test]
fn a_file_the_compiler_found_where_the_host_finds_another_gives_no_observed_key() {
    let workspace = Built::new();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write("other/a.i", "DEFINE VARIABLE o AS INTEGER NO-UNDO.\n");
    workspace.write(
        "src/enc.p",
        &format!("{UNREADABLE}/* FAKE-RESOLVE-OTHER inc/a.i other/a.i */\n{{inc/a.i}}\n"),
    );
    settle();

    let run = workspace.build();
    run.succeeded();
    assert!(
        run.stderr
            .contains("the compiler read other/a.i, which no include resolves to on the host"),
        "{}",
        run.stderr
    );
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["key"], Value::Null, "{plan}");
}

#[test]
fn with_a_text_xref_no_compile_is_observed_and_an_observed_key_does_not_stand() {
    let workspace = Built::new();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write("src/enc.p", &format!("{UNREADABLE}{{inc/a.i}}\n"));
    workspace.build().succeeded();
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["closure"], "observed", "{plan}");

    workspace.reconfigure(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"\nxref_format = \"text\"",
    );
    workspace.clear_log();
    let run = workspace.build();
    run.succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
    assert!(
        run.stderr.contains("xref_format = \"text\""),
        "{}",
        run.stderr
    );
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(
        workspace.compiled(),
        ["enc.p"],
        "a compile that no XREF-XML describes leaves the unit without a key"
    );
}

#[test]
fn a_compile_that_reads_another_closure_than_its_key_holds_is_not_cached_and_keys_the_unit_anew() {
    let cache = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.cache_in(cache.path());
    workspace.write("src/inc/hidden.i", "/* nothing */\n");
    workspace.write("src/inc/extra.i", "DEFINE VARIABLE e AS INTEGER NO-UNDO.\n");
    workspace.write(
        "src/enc.p",
        &format!("{UNREADABLE}/* {{inc/hidden.i}} */\n"),
    );
    settle();
    workspace.build().succeeded();
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["closure"], "observed", "{plan}");

    // The include that oxabl cannot see now includes a file that the stored
    // closure does not name: the key of this build does not describe it.
    workspace.write("src/inc/hidden.i", "/* {inc/extra.i} */\n");
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
    assert!(
        !holds(cache.path(), "r/enc.r"),
        "the outputs of a compile that read outside the key's closure are not cached"
    );

    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "the closure of that compile keys the unit: {:?}",
        workspace.compiled()
    );
    workspace.write("src/inc/extra.i", "DEFINE VARIABLE f AS INTEGER NO-UNDO.\n");
    let plan = workspace.plan();
    assert_eq!(
        Built::reasons(&plan, "src/enc.p"),
        ["dependency-changed"],
        "{plan}"
    );
}

#[test]
fn a_file_changed_just_before_the_build_that_the_refresh_did_not_hash_keys_the_unit_only_at_the_next_compile()
 {
    let workspace = Built::new();
    workspace.write(
        "src/enc.p",
        &format!("{UNREADABLE}/* {{inc/hidden.i}} */\n"),
    );
    workspace.write(
        "src/inc/hidden.i",
        "DEFINE VARIABLE h AS INTEGER NO-UNDO.\n",
    );

    workspace.build().succeeded();
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(
        workspace.compiled(),
        ["enc.p"],
        "nothing shows that the first compile read the bytes on disk now"
    );
    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "the second compile ran under a key made before it: {:?}",
        workspace.compiled()
    );
}

#[test]
fn a_class_unit_that_cannot_compile_where_it_is_leaves_the_observed_key_in_place() {
    let workspace = class_user();
    workspace.write(
        "src/tools/Helper.cls",
        "CLASS Helper:\n    METHOD PUBLIC VOID Run():\n    END METHOD.\nEND CLASS.\n",
    );
    workspace.write("src/tools/Other.cls", "CLASS Other:\nEND CLASS.\n");
    workspace.build().succeeded();

    let plan = workspace.plan();
    for path in ["src/tools/Helper.cls", "src/tools/Other.cls"] {
        let class = unit(&plan, path);
        assert_eq!(class["action"], "skip", "{plan}");
        assert_eq!(
            strings(&class["unkeyable"]),
            ["package-root-not-on-propath: src/tools"],
            "{plan}"
        );
    }
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["closure"], "observed", "{plan}");
    assert_eq!(enc["action"], "up-to-date", "{plan}");

    workspace.write(
        "src/tools/Helper.cls",
        "CLASS Helper:\n    METHOD PUBLIC VOID Run2():\n    END METHOD.\nEND CLASS.\n",
    );
    let plan = workspace.plan();
    assert_eq!(
        unit(&plan, "src/enc.p")["action"],
        "rebuild",
        "the bytes of a skipped class are still in the key: {plan}"
    );
}

#[test]
fn an_encrypted_unit_never_gets_an_observed_key_and_compiles_at_each_build() {
    let workspace = Built::new();
    workspace.write("src/inc/seen.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write("src/enc.p", &format!("{ENCRYPTED}{{inc/seen.i}}\n"));
    settle();

    let first = workspace.build();
    first.succeeded();
    assert!(
        first.stderr.contains("src/enc.p looks encrypted"),
        "{}",
        first.stderr
    );
    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert_eq!(enc["action"], "rebuild", "{plan}");
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"], "it compiles again");
}

#[test]
fn a_unit_that_includes_an_encrypted_file_gets_no_observed_key() {
    let workspace = Built::new();
    workspace.write("src/inc/inner.i", "DEFINE VARIABLE b AS INTEGER NO-UNDO.\n");
    workspace.write("src/inc/locked.i", &format!("{ENCRYPTED}{{inc/inner.i}}\n"));
    workspace.write("src/use.p", &format!("{UNREADABLE}{{inc/locked.i}}\n"));
    settle();

    let first = workspace.build();
    first.succeeded();
    assert!(
        first.stderr.contains("src/inc/locked.i looks encrypted"),
        "{}",
        first.stderr
    );
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/use.p")["key"], Value::Null, "{plan}");
}

#[test]
fn a_file_of_an_observed_closure_that_turns_encrypted_takes_the_key_away() {
    let workspace = Built::new();
    workspace.write("src/inc/seen.i", "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n");
    workspace.write("src/enc.p", &format!("{UNREADABLE}{{inc/seen.i}}\n"));
    settle();
    workspace.build().succeeded();
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "src/enc.p")["closure"], "observed", "{plan}");

    workspace.write(
        "src/inc/seen.i",
        &format!("{ENCRYPTED}DEFINE VARIABLE a AS INTEGER NO-UNDO.\n"),
    );
    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        strings(&enc["unkeyable"])
            .iter()
            .any(|reason| reason == "observed-encrypted: src/inc/seen.i"),
        "{plan}"
    );
}
