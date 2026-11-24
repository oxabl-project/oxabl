//! `oxabl check <dir>` and HD005 in an include whose includers are in the same
//! run: an unnamed widget pool in an includer covers what the include creates.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const CONFIG: &str = "[workspace]\nname = \"pools\"\n\n[workspace.sources]\n\
                      include_paths = [\".\"]\n\n[workspace.lint]\nenable = [\"strict.handles\"]\n";

const INCLUDE: &str = "PROCEDURE search-records:\n\
                       \x20 DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.\n\
                       \x20 CREATE QUERY hQuery.\n\
                       \x20 DELETE OBJECT hQuery.\n\
                       END PROCEDURE.\n";

const POOLED: &str = "CREATE WIDGET-POOL.\n{search.i}\nRUN search-records.\n";
const UNPOOLED: &str = "{search.i}\nRUN search-records.\n";

fn tree(files: &[(&str, &str)]) -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    fs::write(dir.path().join("search.i"), INCLUDE).unwrap();
    for (name, text) in files {
        fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

/// The HD005 findings of `oxabl check --no-format --json <target>` in `dir`:
/// `(path, help)`.
fn hd005(dir: &Path, target: &str) -> Vec<(String, String)> {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--no-format", "--json", target])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let report: serde_json::Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"));
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "HD005")
        .map(|d| {
            (
                d["path"].as_str().unwrap().replace('\\', "/"),
                d["help"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

const UNSEEN: &str = "which was not seen";

#[test]
fn an_include_is_clean_when_its_only_includer_has_a_pool() {
    let dir = tree(&[("search.p", POOLED)]);
    assert_eq!(hd005(dir.path(), "."), []);
}

#[test]
fn an_include_is_clean_when_every_includer_has_a_pool() {
    let dir = tree(&[("a.p", POOLED), ("b.p", POOLED)]);
    assert_eq!(hd005(dir.path(), "."), []);
}

#[test]
fn an_include_reported_for_the_includer_without_a_pool_names_it() {
    let dir = tree(&[("a.p", POOLED), ("b.p", UNPOOLED)]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    let (path, help) = &found[0];
    assert!(path.ends_with("search.i"), "{path}");
    assert!(help.contains("`b.p`"), "{help}");
    assert!(!help.contains("`a.p`"), "{help}");
    assert!(
        !help.contains(UNSEEN),
        "the run checked the includers: {help}"
    );
}

#[test]
fn a_pool_created_after_a_main_block_create_does_not_cover_it() {
    let dir = tree(&[
        (
            "late.i",
            "DEFINE VARIABLE hQ AS HANDLE NO-UNDO.\nCREATE QUERY hQ.\n",
        ),
        (
            "late.p",
            "{late.i}\nCREATE WIDGET-POOL.\nDELETE OBJECT hQ.\n",
        ),
        ("search.p", POOLED),
    ]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("late.i"), "{found:?}");
    assert!(found[0].1.contains("`late.p`"), "{}", found[0].1);
}

#[test]
fn an_include_that_no_root_includes_keeps_the_unseen_note() {
    let dir = tree(&[("other.p", "MESSAGE \"x\".\n")]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
}

#[test]
fn an_unresolved_include_in_a_root_keeps_the_unseen_note() {
    let dir = tree(&[
        ("a.p", POOLED),
        ("b.p", "{missing-part.i}\nMESSAGE \"x\".\n"),
    ]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
}

#[test]
fn an_includer_that_does_not_parse_keeps_the_unseen_note() {
    let dir = tree(&[("a.p", POOLED), ("b.p", "{search.i}\nRUN IN IN IN.\n@ @\n")]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
}

#[test]
fn a_walk_below_the_workspace_root_does_not_see_every_includer() {
    let dir = tree(&[("b.p", UNPOOLED)]);
    fs::write(
        dir.path().join("oxabl.toml"),
        CONFIG.replace("[\".\"]", "[\"src\"]"),
    )
    .unwrap();
    fs::create_dir_all(dir.path().join("src")).unwrap();
    fs::write(dir.path().join("src/a.p"), POOLED).unwrap();
    fs::write(dir.path().join("src/search.i"), INCLUDE).unwrap();
    fs::remove_file(dir.path().join("search.i")).unwrap();
    // `b.p` is outside the walk and gives no pool.
    let found = hd005(dir.path(), "src");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
    // The whole workspace sees both roots.
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains("`b.p`"), "{}", found[0].1);
}

#[test]
fn an_include_file_that_no_root_includes_is_an_unseen_includer() {
    let dir = tree(&[("a.p", POOLED), ("lib.i", "{search.i}\n")]);
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
}

#[test]
fn a_file_that_the_walk_does_not_take_as_a_root_but_names_the_include_keeps_the_note() {
    for (name, text) in [
        ("b.w", UNPOOLED),
        ("trigger.t", UNPOOLED),
        (".old/x.p", UNPOOLED),
        ("SEARCH.I.bak", "{SEARCH.I}\n"),
    ] {
        let dir = tree(&[("a.p", POOLED)]);
        if let Some((parent, _)) = name.rsplit_once('/') {
            fs::create_dir_all(dir.path().join(parent)).unwrap();
        }
        fs::write(dir.path().join(name), text).unwrap();
        // The walk sees `a.p` only for `.p`.
        fs::write(
            dir.path().join("oxabl.toml"),
            CONFIG.replace(
                "include_paths = [\".\"]\n",
                "include_paths = [\".\"]\nroot_extensions = [\"p\"]\n",
            ),
        )
        .unwrap();
        let found = hd005(dir.path(), ".");
        assert_eq!(found.len(), 1, "{name}: {found:?}");
        assert!(found[0].1.contains(UNSEEN), "{name}: {}", found[0].1);
    }
}

#[test]
fn a_root_whose_expansion_has_none_of_the_creates_keeps_the_note() {
    let include = "&IF DEFINED(skip) = 0 &THEN\n\
                   DEFINE VARIABLE hQ AS HANDLE NO-UNDO.\n\
                   CREATE QUERY hQ.\n\
                   DELETE OBJECT hQ.\n\
                   &ENDIF\n";
    let dir = tree(&[
        ("cond.i", include),
        (
            "a.p",
            "&GLOBAL-DEFINE skip 1\nCREATE WIDGET-POOL.\n{cond.i}\n",
        ),
    ]);
    let found = hd005(dir.path(), ".");
    let cond: Vec<_> = found
        .iter()
        .filter(|(p, _)| p.ends_with("cond.i"))
        .collect();
    assert_eq!(cond.len(), 1, "{found:?}");
    assert!(cond[0].1.contains(UNSEEN), "{}", cond[0].1);
}

/// The HD005 findings of `search.i` keep the note that the includer was not seen.
fn assert_kept(dir: &Path, why: &str) {
    let found = hd005(dir, ".");
    assert_eq!(found.len(), 1, "{why}: {found:?}");
    assert!(found[0].0.ends_with("search.i"), "{why}: {found:?}");
    assert!(found[0].1.contains(UNSEEN), "{why}: {}", found[0].1);
}

#[test]
fn an_include_reached_through_another_include_keeps_the_note() {
    let dir = tree(&[
        ("outer.i", "{search.i}\n"),
        (
            "a.p",
            "CREATE WIDGET-POOL.\n{outer.i}\nRUN search-records.\n",
        ),
    ]);
    assert_kept(dir.path(), "intermediate include");
}

#[test]
fn an_unseen_includer_of_an_intermediate_include_keeps_the_note() {
    for unseen in ["x.t", ".old/x.p"] {
        let dir = tree(&[
            ("lib.i", "{search.i}\n"),
            ("a.p", "CREATE WIDGET-POOL.\n{lib.i}\nRUN search-records.\n"),
        ]);
        if let Some((parent, _)) = unseen.rsplit_once('/') {
            fs::create_dir_all(dir.path().join(parent)).unwrap();
        }
        fs::write(dir.path().join(unseen), "{lib.i}\nRUN search-records.\n").unwrap();
        assert_kept(dir.path(), unseen);
    }
}

#[test]
fn an_include_file_in_an_inactive_branch_keeps_the_note() {
    let dir = tree(&[
        ("a.p", POOLED),
        (
            "o.i",
            "&IF DEFINED(want-search) &THEN\n{search.i}\n&ENDIF\n",
        ),
        ("z.t", "&GLOBAL-DEFINE want-search yes\n{o.i}\n"),
    ]);
    assert_kept(dir.path(), "orphan include in an inactive branch");
}

#[test]
fn a_root_that_names_the_include_in_an_inactive_branch_keeps_the_note() {
    let dir = tree(&[
        ("a.p", POOLED),
        ("b.p", "&IF FALSE &THEN\n{search.i}\n&ENDIF\n"),
    ]);
    assert_kept(dir.path(), "inactive branch of a root");
}

#[test]
fn a_computed_include_name_keeps_the_note() {
    let computed = "&SCOPED-DEFINE n search\n{{&n}.i}\nRUN search-records.\n";
    // In a root: it reaches the include, but its text does not name it.
    let dir = tree(&[("a.p", POOLED), ("c.p", computed)]);
    assert_kept(dir.path(), "computed name in a root");
    // In a file that is not a root: it cannot be expanded.
    let dir = tree(&[("a.p", POOLED), ("c.t", computed)]);
    assert_kept(dir.path(), "computed name in a trigger file");
}

#[cfg(unix)]
#[test]
fn a_symbolic_link_in_the_workspace_keeps_the_note() {
    let outside = TempDir::new().unwrap();
    fs::write(outside.path().join("x.t"), UNPOOLED).unwrap();
    let dir = tree(&[("a.p", POOLED)]);
    std::os::unix::fs::symlink(outside.path(), dir.path().join("ext")).unwrap();
    assert_kept(dir.path(), "symlinked directory");
}

#[test]
fn an_include_path_outside_the_workspace_keeps_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    fs::write(
        dir.path().join("oxabl.toml"),
        CONFIG.replace("[\".\"]", "[\".\", \"../shared\"]"),
    )
    .unwrap();
    assert_kept(dir.path(), "include path above the workspace");
}

#[test]
fn files_that_cannot_include_it_do_not_keep_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    // Documentation without a reference, a baseline-like file and a binary file
    // that holds the name.
    fs::write(
        dir.path().join("NOTES.md"),
        "See search.i for the queries.\n",
    )
    .unwrap();
    fs::write(
        dir.path().join("report.json"),
        "{\"path\": \"search.i\", \"code\": \"HD005\"}\n",
    )
    .unwrap();
    let mut binary = vec![0u8; 16];
    binary.extend_from_slice(b"{search.i}");
    fs::write(dir.path().join("data.bin"), binary).unwrap();
    assert_eq!(hd005(dir.path(), "."), []);
}

#[test]
fn a_file_that_names_the_include_in_a_reference_keeps_the_note_whatever_it_is() {
    let dir = tree(&[("a.p", POOLED), ("NOTES.md", "Use {search.i} here.\n")]);
    assert_kept(dir.path(), "reference in notes");
}

#[test]
fn an_includer_is_named_with_its_full_path_when_the_working_directory_is_the_root() {
    let dir = tree(&[("a.p", POOLED), ("b.p", UNPOOLED)]);
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir("/")
        .args(["check", "--no-format", "--json"])
        .arg(dir.path())
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let report: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    let help = report["diagnostics"][0]["help"].as_str().unwrap();
    let expected = format!("`{}`", dir.path().join("b.p").display());
    assert!(help.contains(&expected), "{help}");
}

#[test]
fn an_absolute_include_path_outside_the_workspace_keeps_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    let outside = TempDir::new().unwrap();
    fs::write(
        dir.path().join("oxabl.toml"),
        CONFIG.replace(
            "[\".\"]",
            &format!("[\".\", {:?}]", outside.path().display().to_string()),
        ),
    )
    .unwrap();
    assert_kept(dir.path(), "absolute include path elsewhere");
}

#[test]
fn a_non_root_file_that_includes_a_root_keeps_the_note() {
    let lib = "&IF \"{&no-pool}\" = \"\" &THEN\nCREATE WIDGET-POOL.\n&ENDIF\n\
               {search.i}\nRUN search-records.\n";
    let dir = tree(&[
        ("lib.p", lib),
        ("x.t", "&GLOBAL-DEFINE no-pool yes\n{lib.p}\n"),
    ]);
    assert_kept(dir.path(), "a trigger file includes the root");
    // Control: without the unseen includer the root covers the include.
    let dir = tree(&[("lib.p", lib)]);
    assert_eq!(hd005(dir.path(), "."), []);
}

#[test]
fn a_quoted_include_name_with_a_space_is_read() {
    let dir = tree(&[("a.p", POOLED), ("x.t", "{\"lib dir/search.i\"}\n")]);
    assert_kept(dir.path(), "quoted name with a space");
}

#[test]
fn a_non_ascii_include_name_is_read_with_the_configured_encoding() {
    let dir = TempDir::new().unwrap();
    fs::write(
        dir.path().join("oxabl.toml"),
        CONFIG.replace(
            "include_paths = [\".\"]\n",
            "include_paths = [\".\"]\nencoding = \"windows-1252\"\n",
        ),
    )
    .unwrap();
    fs::write(dir.path().join("recherch\u{e9}.i"), INCLUDE).unwrap();
    // `\xE9` is the cp1252 byte for the letter in the name.
    fs::write(
        dir.path().join("a.p"),
        b"CREATE WIDGET-POOL.\n{recherch\xE9.i}\nRUN search-records.\n",
    )
    .unwrap();
    assert_eq!(hd005(dir.path(), "."), [], "control: the root covers it");
    fs::write(
        dir.path().join("x.t"),
        b"{RECHERCH\xC9.i}\nRUN search-records.\n",
    )
    .unwrap();
    let found = hd005(dir.path(), ".");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].1.contains(UNSEEN), "{}", found[0].1);
}

#[test]
fn a_double_brace_that_is_not_an_include_does_not_keep_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    fs::create_dir_all(dir.path().join(".github/workflows")).unwrap();
    fs::write(
        dir.path().join(".github/workflows/ci.yml"),
        "if: ${{ github.ref }}\n",
    )
    .unwrap();
    fs::write(dir.path().join("cfg.json"), "{\"a\":{\"b\":1}}\n").unwrap();
    fs::write(dir.path().join("README.md"), "Hello {{name}}\n").unwrap();
    assert_eq!(hd005(dir.path(), "."), []);
}

#[test]
fn a_root_relative_include_path_outside_the_workspace_keeps_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    fs::write(
        dir.path().join("oxabl.toml"),
        CONFIG.replace(
            "include_paths = [\".\"]\n",
            "include_paths = [\".\"]\nroot_relative_include_paths = [\"../shared\"]\n",
        ),
    )
    .unwrap();
    assert_kept(dir.path(), "root-relative path above the workspace");
}

#[test]
fn an_include_that_a_root_reaches_outside_the_workspace_keeps_the_note() {
    let outside = TempDir::new().unwrap();
    fs::write(outside.path().join("far.i"), "MESSAGE \"far\".\n").unwrap();
    let far = outside.path().join("far.i");
    let dir = tree(&[(
        "a.p",
        &format!(
            "CREATE WIDGET-POOL.\n{{search.i}}\n{{{}}}\nRUN search-records.\n",
            far.display()
        ),
    )]);
    assert_kept(dir.path(), "absolute reference outside the workspace");
}

#[test]
fn includes_with_the_same_base_name_in_two_directories_keep_the_note() {
    let dir = tree(&[("a.p", POOLED)]);
    fs::create_dir_all(dir.path().join("other")).unwrap();
    fs::write(dir.path().join("other/search.i"), "MESSAGE \"x\".\n").unwrap();
    fs::write(dir.path().join("b.p"), "{other/search.i}\n").unwrap();
    assert_kept(dir.path(), "same base name");
}

/// A root with a computed include name can include the fragment when a file
/// that is not a root includes the root with the name defined.
#[test]
fn a_root_that_computes_a_name_for_an_unseen_includer_keeps_the_note() {
    let by_define = "&IF DEFINED(inc) &THEN\n{{&inc}}\n&ENDIF\n";
    let dir = tree(&[
        ("search.p", POOLED),
        ("wrap.p", by_define),
        (
            "x.t",
            "&GLOBAL-DEFINE inc search.i\n{wrap.p}\nRUN search-records.\n",
        ),
    ]);
    assert_kept(dir.path(), "a computed name defined by an unseen includer");
    let by_argument = "&IF \"{1}\" <> \"\" &THEN\n{{1}}\n&ENDIF\n";
    let dir = tree(&[
        ("search.p", POOLED),
        ("wrap.p", by_argument),
        ("x.t", "{wrap.p search.i}\nRUN search-records.\n"),
    ]);
    assert_kept(dir.path(), "a computed name passed by an unseen includer");
}

/// The scan of a file reads each reference once: a long run of `{` costs no
/// more than a file of its size.
#[test]
fn a_long_run_of_braces_is_read_in_linear_time() {
    let braces = "{".repeat(200_000);
    let dir = tree(&[("search.p", POOLED), ("big.txt", &braces)]);
    let started = std::time::Instant::now();
    let found = hd005(dir.path(), ".");
    let elapsed = started.elapsed();
    assert!(
        elapsed < std::time::Duration::from_secs(20),
        "took {elapsed:?}"
    );
    // A token that long is no file name: the run keeps the note.
    assert_eq!(found.len(), 1, "{found:?}");
}
