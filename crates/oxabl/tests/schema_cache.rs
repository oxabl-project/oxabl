//! `oxabl check` keeps the loaded form of the `.df` files in a cache, so the
//! next process parses none of them, and its output is the output of a run
//! with no cache.
//!
//! A test build of the binary counts the schema files it parses and prints the
//! count when `OXABL_TEST_SCHEMA_PARSES` is set. All fixtures are synthetic.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime};

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

    // Another build with the size and the time of this one.
    let other = ws.path().join("oxabl-other");
    let original = Path::new(env!("CARGO_BIN_EXE_oxabl"));
    let mut bytes = fs::read(original).unwrap();
    let needle = b"last definition wins";
    let at = bytes
        .windows(needle.len())
        .position(|window| window == needle)
        .expect("the binary holds the message of the loader");
    bytes[at..at + needle.len()].make_ascii_uppercase();
    fs::copy(original, &other).unwrap();
    fs::write(&other, &bytes).unwrap();
    let modified = fs::metadata(original).unwrap().modified().unwrap();
    fs::File::options()
        .write(true)
        .open(&other)
        .unwrap()
        .set_modified(modified)
        .unwrap();
    assert_eq!(fs::metadata(&other).unwrap().len(), bytes.len() as u64);
    assert_eq!(
        ws.check_with(&other, &[]).parsed,
        1,
        "a build with another content read the entry of this one"
    );
    assert_eq!(ws.entries().len(), 2);
    assert_eq!(ws.check_with(&other, &[]).parsed, 0);
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

/// The first position of `needle` in `bytes`.
fn find(bytes: &[u8], needle: &[u8]) -> usize {
    bytes
        .windows(needle.len())
        .position(|window| window == needle)
        .unwrap_or_else(|| panic!("{:?} is not in the entry", String::from_utf8_lossy(needle)))
}

#[test]
fn an_entry_that_changed_with_its_structure_intact_is_not_read() {
    let ws = Workspace::new(&[("a.df", CUSTOMER_NAME)]);
    let off = ws.check(&[("OXABL_SCHEMA_CACHE", "off")]);
    assert!(!off.stdout.contains("LINT0003"), "{}", off.stdout);
    ws.check(&[]);
    let entry = ws.entries().remove(0);
    let bytes = fs::read(&entry).unwrap();

    let name = find(&bytes, b"Name");
    let mut renamed = bytes.clone();
    renamed[name + 1] = b'z';
    let mut zeroed = bytes.clone();
    let end = zeroed.len();
    zeroed[end / 2..end / 2 + 16].fill(0);
    let mut hash_only = bytes.clone();
    hash_only[0] ^= 1;
    for (what, damaged) in [
        ("a letter of the pool", renamed),
        ("a zeroed block", zeroed),
        ("a bit of the hash", hash_only),
    ] {
        assert_eq!(damaged.len(), bytes.len());
        fs::write(&entry, damaged).unwrap();
        let run = ws.check(&[]);
        assert_eq!(run.parsed, 1, "{what}: the changed entry was used");
        same_output(&off, &run);
        assert_eq!(
            fs::read(&entry).unwrap(),
            bytes,
            "{what}: the entry was not replaced"
        );
    }
    assert_eq!(ws.check(&[]).parsed, 0);
}

#[test]
fn an_entry_of_another_key_is_not_read() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    ws.check(&[]);
    let first = ws.entries().remove(0);
    fs::write(ws.path().join("schema/a.df"), CUSTOMER_NAME).unwrap();
    let off = ws.check(&[("OXABL_SCHEMA_CACHE", "off")]);
    ws.check(&[]);
    let second = ws.entries().into_iter().find(|p| *p != first).unwrap();
    fs::copy(&first, &second).unwrap();
    let run = ws.check(&[]);
    assert_eq!(run.parsed, 1, "the entry of the other schema was used");
    same_output(&off, &run);
}

#[cfg(unix)]
#[test]
fn a_fifo_at_the_path_of_an_entry_does_not_stop_the_run() {
    use std::time::{Duration, Instant};

    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    ws.check(&[]);
    let entry = ws.entries().remove(0);
    fs::remove_file(&entry).unwrap();
    let made = Command::new("mkfifo").arg(&entry).status().unwrap();
    assert!(made.success());

    let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(ws.path())
        .env_remove("CI")
        .env_remove("OXABL_CACHE_MODE")
        .env("OXABL_SCHEMA_CACHE_DIR", ws.cache())
        .args(["check", "--no-format", "--schema", "schema", "cust.p"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() > deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("the run waits on a FIFO at the path of an entry");
        }
        std::thread::sleep(Duration::from_millis(50));
    };
    assert!(status.code().is_some());
}

#[test]
fn a_test_run_never_touches_the_cache_of_the_user() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    let home = ws.path().join("home");
    fs::create_dir(&home).unwrap();
    let env = [
        ("OXABL_SCHEMA_CACHE_DIR", ""),
        ("HOME", home.to_str().unwrap()),
        ("XDG_CACHE_HOME", home.to_str().unwrap()),
    ];
    for _ in 0..2 {
        assert_eq!(ws.check(&env).parsed, 1);
    }
    let left: Vec<_> = fs::read_dir(&home).unwrap().flatten().collect();
    assert!(
        left.is_empty(),
        "a test build wrote below the home: {left:?}"
    );
    assert!(ws.entries().is_empty());
}

#[test]
fn a_read_keeps_an_entry_from_being_the_first_to_go() {
    let ws = Workspace::new(&[("a.df", CUSTOMER)]);
    let schema = |n: usize| {
        fs::write(
            ws.path().join("schema/a.df"),
            format!("{CUSTOMER}ADD FIELD \"F{n}\" OF \"Customer\" AS integer\n"),
        )
        .unwrap();
    };
    let past = |path: &Path, seconds: u64| {
        let at = SystemTime::now() - Duration::from_secs(seconds);
        fs::File::options()
            .write(true)
            .open(path)
            .unwrap()
            .set_modified(at)
            .unwrap();
    };
    for n in 0..16 {
        schema(n);
        ws.check(&[]);
    }
    let entries = ws.entries();
    assert_eq!(entries.len(), 16);
    // Schema 0 made the oldest entry; every entry is old, schema 0 the most.
    schema(0);
    let first = entries
        .iter()
        .find(|path| {
            let hit = fs::metadata(path).unwrap().modified().unwrap();
            entries
                .iter()
                .all(|other| fs::metadata(other).unwrap().modified().unwrap() >= hit)
        })
        .unwrap()
        .clone();
    for (n, path) in entries.iter().enumerate() {
        past(path, 1000 + n as u64);
    }
    past(&first, 100_000);
    assert_eq!(ws.check(&[]).parsed, 0, "the entry of schema 0");
    schema(99);
    ws.check(&[]);
    let after = ws.entries();
    assert_eq!(after.len(), 16);
    assert!(
        after.contains(&first),
        "the entry that was read was removed"
    );
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
