//! The language server keeps the loaded form of the `.df` files in the schema
//! cache, so the next editor start parses none of them, and the answer of
//! the editor does not change.
//!
//! All fixtures are synthetic.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::Value;
use session::{Launch, Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"shop\"\n\
                      [workspace.schema]\nfiles = [\"db/shop.df\"]\n";
const SCHEMA: &str = "ADD TABLE \"Customer\"\n\
                      ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\
                      ADD FIELD \"Name\" OF \"Customer\" AS character\n";
const PROGRAM: &str = "FIND FIRST Customer.\nDISPLAY Customer.Name Customer.Zzz.\n";

fn entries(directory: &Path) -> Vec<PathBuf> {
    let Ok(listing) = std::fs::read_dir(directory) else {
        return Vec::new();
    };
    let mut entries: Vec<_> = listing.flatten().map(|e| e.path()).collect();
    entries.sort();
    entries
}

/// The codes of the diagnostics the server publishes for `program`, in a new
/// session whose schema cache is `cache`.
fn diagnostics(root: &Path, cache: &Path, program: &Path) -> Vec<String> {
    diagnostics_with(root, cache, program, &[])
}

/// [`diagnostics`] with `env` set for `oxabl lsp`.
fn diagnostics_with(
    root: &Path,
    cache: &Path,
    program: &Path,
    env: &[(&str, &str)],
) -> Vec<String> {
    let launch = Launch {
        schema_cache: Some(cache),
        env,
        ..Launch::default()
    };
    let mut session = Session::launch_with(root, serde_json::json!({}), launch, |_| {})
        .await_initialize()
        .0;
    session.open(program, PROGRAM);
    let uri = file_uri(program);
    let published = session.wait_for(|message| {
        message["method"] == "textDocument/publishDiagnostics"
            && message["params"]["uri"] == uri
            && message["params"]["diagnostics"]
                .as_array()
                .is_some_and(|list| !list.is_empty())
    });
    let mut codes: Vec<String> = published["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|diagnostic| match &diagnostic["code"] {
            Value::String(code) => code.clone(),
            other => other.to_string(),
        })
        .collect();
    codes.sort();
    codes
}

/// A workspace with the schema and the program, and the program's path.
fn workspace() -> (tempfile::TempDir, PathBuf) {
    let root = tempfile::tempdir().unwrap();
    std::fs::create_dir(root.path().join("db")).unwrap();
    std::fs::write(root.path().join("oxabl.toml"), CONFIG).unwrap();
    std::fs::write(root.path().join("db/shop.df"), SCHEMA).unwrap();
    let program = root.path().join("t.p");
    std::fs::write(&program, PROGRAM).unwrap();
    (root, program)
}

#[test]
fn the_editor_writes_and_reads_the_schema_cache() {
    let (root, program) = workspace();
    let cache = tempfile::tempdir().unwrap();
    let cache = cache.path().join("schema");

    let first = diagnostics(root.path(), &cache, &program);
    assert!(
        first.iter().any(|code| code == "LINT0003"),
        "the unknown field must be a finding: {first:?}"
    );
    let written = entries(&cache);
    assert_eq!(written.len(), 1, "the first session wrote no entry");
    let bytes = std::fs::read(&written[0]).unwrap();

    let second = diagnostics(root.path(), &cache, &program);
    assert_eq!(first, second);
    assert_eq!(entries(&cache), written);
    assert_eq!(
        std::fs::read(&written[0]).unwrap(),
        bytes,
        "the second session replaced the entry"
    );
}

/// With `CI` set and no `OXABL_CACHE_MODE`, the mode is `read-only` and the
/// cache is off: the editor gives the same answer and writes no entry.
#[test]
fn on_a_ci_runner_the_editor_writes_no_schema_cache() {
    let (root, program) = workspace();
    let cache = tempfile::tempdir().unwrap();
    let cache = cache.path().join("schema");

    let on_ci = diagnostics_with(root.path(), &cache, &program, &[("CI", "true")]);
    assert!(
        on_ci.iter().any(|code| code == "LINT0003"),
        "the unknown field must be a finding: {on_ci:?}"
    );
    assert_eq!(
        entries(&cache),
        Vec::<PathBuf>::new(),
        "the editor wrote an entry on a CI runner"
    );
}
