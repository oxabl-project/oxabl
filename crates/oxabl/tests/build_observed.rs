//! A unit whose closure oxabl cannot read from its source (an encrypted
//! source, an encrypted include) gets a key from the closure that its
//! compiler reported at the last compile: the observed closure.

mod common;

use std::path::Path;
use std::time::Duration;

use common::built::{Built, unit};
use common::cache::entries;
use serde_json::Value;
use tempfile::TempDir;

/// Bytes that the analysis cannot parse, as an encrypted source looks to it.
/// The fake AVM still finds the includes written after them.
const UNREADABLE: &str = "\u{1}\u{2}\u{3} 0x9f 0x00 ((( %%% .\n";

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
        &format!("{UNREADABLE}{{inc/seen.i}}\n/* {{inc/hidden.i}} */\n"),
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

#[test]
fn a_unit_whose_compile_used_a_class_gets_no_observed_key() {
    let workspace = Built::new();
    workspace.write("src/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("src/enc.p", &format!("{UNREADABLE}/* NEW pkg.Foo() */\n"));
    settle();

    let run = workspace.build();
    run.succeeded();
    assert!(
        run.stderr
            .contains("src/enc.p: no key from what the compiler read: the compile used a class"),
        "{}",
        run.stderr
    );
    let plan = workspace.plan();
    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["action"], "rebuild", "{plan}");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["enc.p"]);
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
