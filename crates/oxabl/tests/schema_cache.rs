//! `oxabl check` keeps the loaded form of the `.df` files in a cache, so the
//! next process parses none of them, and its output is the output of a run
//! with no cache.
//!
//! A test build of the binary counts the schema files it parses and prints the
//! count when `OXABL_TEST_SCHEMA_PARSES` is set. All fixtures are synthetic.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;

const PROGRAM: &str = "FIND FIRST Customer.\nDISPLAY Customer.Name.\n";
const CUSTOMER: &str = "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n";
const CUSTOMER_NAME: &str = "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\
                             ADD FIELD \"Name\" OF \"Customer\" AS character\n";
const ORDER: &str = "ADD TABLE \"Order\"\nADD FIELD \"OrderNum\" OF \"Order\" AS integer\n";

struct Workspace {
    dir: TempDir,
}

struct Run {
    code: Option<i32>,
    stdout: String,
    /// Without the line that carries the count.
    stderr: String,
    parsed: usize,
}

impl Workspace {
    fn new(schemas: &[(&str, &str)]) -> Self {
        let dir = TempDir::new().unwrap();
        fs::create_dir(dir.path().join("schema")).unwrap();
        for (name, text) in schemas {
            fs::write(dir.path().join("schema").join(name), text).unwrap();
        }
        fs::write(dir.path().join("cust.p"), PROGRAM).unwrap();
        Workspace { dir }
    }

    fn path(&self) -> &Path {
        self.dir.path()
    }

    fn cache(&self) -> PathBuf {
        self.path().join("cache")
    }

    fn entries(&self) -> Vec<PathBuf> {
        let Ok(listing) = fs::read_dir(self.cache()) else {
            return Vec::new();
        };
        let mut entries: Vec<_> = listing.flatten().map(|e| e.path()).collect();
        entries.sort();
        entries
    }

    /// `oxabl check --schema schema cust.p`, with the cache in `cache/` and
    /// `env` on top of a clean environment.
    fn check(&self, env: &[(&str, &str)]) -> Run {
        self.check_with(Path::new(env!("CARGO_BIN_EXE_oxabl")), env)
    }

    fn check_with(&self, binary: &Path, env: &[(&str, &str)]) -> Run {
        let mut command = Command::new(binary);
        command
            .current_dir(self.path())
            .env_remove("CI")
            .env_remove("OXABL_CACHE_MODE")
            .env_remove("OXABL_SCHEMA_CACHE")
            .env("OXABL_SCHEMA_CACHE_DIR", self.cache())
            .env("OXABL_TEST_SCHEMA_PARSES", "1")
            .args(["check", "--no-format", "--schema", "schema", "cust.p"]);
        for (name, value) in env {
            command.env(name, value);
        }
        let out = command.output().unwrap();
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        let mut parsed = None;
        let mut kept = Vec::new();
        for line in stderr.lines() {
            match line.strip_prefix("oxabl-test: schema files parsed: ") {
                Some(count) => parsed = Some(count.parse().unwrap()),
                None => kept.push(line),
            }
        }
        Run {
            code: out.status.code(),
            stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
            stderr: kept.join("\n"),
            parsed: parsed.unwrap_or_else(|| panic!("the test build printed no count:\n{stderr}")),
        }
    }
}

fn same_output(left: &Run, right: &Run) {
    assert_eq!(left.code, right.code);
    assert_eq!(left.stdout, right.stdout);
    assert_eq!(left.stderr, right.stderr);
}

#[test]
fn the_second_run_parses_no_schema_file() {
    let ws = Workspace::new(&[("a.df", CUSTOMER), ("b.df", ORDER)]);
    let off = ws.check(&[("OXABL_SCHEMA_CACHE", "off")]);
    assert_eq!(off.parsed, 2);
    assert!(
        ws.entries().is_empty(),
        "a run with the cache off wrote one"
    );
    assert!(
        off.stdout.contains("LINT0003"),
        "the program must have a finding that needs the schema:\n{}",
        off.stdout
    );

    let first = ws.check(&[]);
    assert_eq!(first.parsed, 2, "the first run has nothing to read");
    assert_eq!(ws.entries().len(), 1);
    let second = ws.check(&[]);
    assert_eq!(
        second.parsed, 0,
        "the second run parsed a schema file again"
    );
    assert_eq!(ws.entries().len(), 1);
    same_output(&off, &first);
    same_output(&off, &second);
}

#[test]
fn a_changed_schema_file_is_parsed_again() {
    let ws = Workspace::new(&[("a.df", CUSTOMER), ("b.df", ORDER)]);
    let before = ws.check(&[]);
    assert!(before.stdout.contains("LINT0003"), "{}", before.stdout);
    ws.check(&[]);

    fs::write(ws.path().join("schema/a.df"), CUSTOMER_NAME).unwrap();
    let after = ws.check(&[]);
    assert_eq!(after.parsed, 2, "the changed file was not parsed again");
    assert!(
        !after.stdout.contains("LINT0003"),
        "the finding must go with the new field:\n{}",
        after.stdout
    );
    assert_eq!(ws.entries().len(), 2, "an entry for each text");
    assert_eq!(ws.check(&[]).parsed, 0);
}

#[test]
fn a_change_of_one_byte_is_parsed_again() {
    let ws = Workspace::new(&[("a.df", CUSTOMER_NAME)]);
    assert!(!ws.check(&[]).stdout.contains("LINT0003"));
    assert_eq!(ws.check(&[]).parsed, 0);

    let renamed = CUSTOMER_NAME.replace("\"Name\"", "\"Nome\"");
    assert_eq!(renamed.len(), CUSTOMER_NAME.len());
    fs::write(ws.path().join("schema/a.df"), renamed).unwrap();
    let after = ws.check(&[]);
    assert_eq!(after.parsed, 1, "the changed file was not parsed again");
    assert!(
        after.stdout.contains("LINT0003"),
        "the field is gone, so the program has a finding:\n{}",
        after.stdout
    );
}

#[test]
fn a_new_build_does_not_read_the_entry_of_an_old_one() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    ws.check(&[]);
    assert_eq!(ws.check(&[]).parsed, 0);

    // The same program in a file made now: a build the cache has not seen.
    let copy = ws.path().join("oxabl-copy");
    fs::copy(env!("CARGO_BIN_EXE_oxabl"), &copy).unwrap();
    assert_eq!(ws.check_with(&copy, &[]).parsed, 1);
    assert_eq!(ws.entries().len(), 2);
    assert_eq!(ws.check_with(&copy, &[]).parsed, 0);
}

#[test]
fn load_warnings_come_back_from_the_cache() {
    // Two files declare `Customer`: a warning of the load.
    let ws = Workspace::new(&[("a.df", CUSTOMER), ("b.df", CUSTOMER_NAME)]);
    let off = ws.check(&[("OXABL_SCHEMA_CACHE", "off")]);
    assert!(off.stderr.contains("SCHEMA0010"), "stderr:\n{}", off.stderr);
    ws.check(&[]);
    let cached = ws.check(&[]);
    assert_eq!(cached.parsed, 0);
    same_output(&off, &cached);
}

#[test]
fn a_damaged_entry_is_parsed_again_and_replaced() {
    let ws = Workspace::new(&[("a.df", CUSTOMER), ("b.df", ORDER)]);
    let off = ws.check(&[("OXABL_SCHEMA_CACHE", "off")]);
    ws.check(&[]);
    let entry = ws.entries().remove(0);
    let bytes = fs::read(&entry).unwrap();

    for damaged in [
        bytes[..bytes.len() / 2].to_vec(),
        Vec::new(),
        [&bytes[..], &[0]].concat(),
        b"not an entry".to_vec(),
    ] {
        fs::write(&entry, damaged).unwrap();
        let run = ws.check(&[]);
        assert_eq!(run.parsed, 2, "a damaged entry was used");
        same_output(&off, &run);
        assert_eq!(
            fs::read(&entry).unwrap(),
            bytes,
            "the entry was not replaced"
        );
    }
    assert_eq!(ws.check(&[]).parsed, 0);
}

#[test]
fn the_cache_is_not_used_outside_developer_mode() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    for env in [
        &[("OXABL_CACHE_MODE", "ci")][..],
        &[("OXABL_CACHE_MODE", "read-only")][..],
        &[("OXABL_CACHE_MODE", "nonsense")][..],
        &[("CI", "true")][..],
    ] {
        for _ in 0..2 {
            let run = ws.check(env);
            assert_eq!(run.parsed, 1, "{env:?} used the cache");
        }
        assert!(ws.entries().is_empty(), "{env:?} wrote an entry");
    }
    // `developer` wins over `CI`, as for the build caches.
    ws.check(&[("CI", "true"), ("OXABL_CACHE_MODE", "developer")]);
    assert_eq!(ws.entries().len(), 1);
}

#[test]
fn old_entries_are_removed_and_other_files_are_not() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    fs::create_dir(ws.cache()).unwrap();
    fs::write(ws.cache().join("notes.bin"), "mine").unwrap();
    for n in 0..20 {
        fs::write(
            ws.path().join("schema/a.df"),
            format!("{CUSTOMER}ADD FIELD \"F{n}\" OF \"Customer\" AS integer\n"),
        )
        .unwrap();
        ws.check(&[]);
    }
    let names: Vec<_> = ws
        .entries()
        .iter()
        .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
        .collect();
    assert_eq!(
        names.iter().filter(|n| n.as_str() != "notes.bin").count(),
        16,
        "{names:?}"
    );
    assert!(names.iter().any(|n| n == "notes.bin"));
}
