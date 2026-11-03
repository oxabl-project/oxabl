//! Shared fixtures for the `oxabl build` tests: a workspace whose worker
//! command is `oxabl_fake_avm`, and the identities the spec derives.

#![allow(dead_code)]

pub mod built;
pub mod cache;
pub mod key_spec;
pub mod keyed;
pub mod openedge;
#[cfg(feature = "remote-cache")]
pub mod remote;
pub mod remote_double;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::OnceLock;

use serde_json::{Value, json};
use tempfile::TempDir;

/// The `oxabl_fake_avm` binary, built next to the `oxabl` binary on first use.
pub fn fake_avm() -> &'static Path {
    static PATH: OnceLock<PathBuf> = OnceLock::new();
    PATH.get_or_init(|| {
        let exe = Path::new(env!("CARGO_BIN_EXE_oxabl"));
        let profile_dir = exe.parent().expect("the oxabl binary has a directory");
        let target_dir = profile_dir
            .parent()
            .expect("a profile directory has a parent");
        let mut cargo = Command::new(env!("CARGO"));
        cargo
            .args(["build", "--quiet", "-p", "oxabl_fake_avm", "--target-dir"])
            .arg(target_dir);
        let profile = profile_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("debug");
        if profile != "debug" {
            cargo.args(["--profile", profile]);
        }
        let status = cargo.status().expect("cargo runs");
        assert!(status.success(), "building oxabl_fake_avm failed");
        let fake = profile_dir.join("oxabl_fake_avm");
        assert!(fake.exists(), "{} was not built", fake.display());
        fake
    })
}

/// A workspace root in a temporary directory.
pub struct Workspace {
    pub dir: TempDir,
}

impl Workspace {
    /// A workspace with `[workspace.preprocessor] proversion = "12.8"` and
    /// the fake AVM as the worker command. `workers` is the rest of
    /// `[build.workers]`; `extra_args` follow `{program}` in the command.
    pub fn new(workers: &str, extra_args: &[&str]) -> Workspace {
        let workspace = Workspace::bare();
        let mut command = vec![
            fake_avm().to_string_lossy().into_owned(),
            "{program}".to_string(),
        ];
        command.extend(extra_args.iter().map(|arg| arg.to_string()));
        let quoted: Vec<String> = command.iter().map(|arg| format!("{arg:?}")).collect();
        workspace.write(
            "oxabl.toml",
            &format!(
                "[workspace]\nname = \"probe\"\n\n[workspace.preprocessor]\nproversion = \"12.8\"\n\n[build.workers]\ncommand = [{}]\n{workers}\n",
                quoted.join(", ")
            ),
        );
        workspace
    }

    /// Adds a line to `[workspace.preprocessor]`.
    pub fn preprocessor_line(&self, line: &str) {
        let config = fs::read_to_string(self.path("oxabl.toml")).unwrap();
        self.write(
            "oxabl.toml",
            &config.replace(
                "proversion = \"12.8\"",
                &format!("proversion = \"12.8\"\n{line}"),
            ),
        );
    }

    /// Names the databases the workers connect, as `[workspace.preprocessor]`.
    pub fn declare_databases(&self, names: &[&str]) {
        let list: Vec<String> = names.iter().map(|name| format!("{name:?}")).collect();
        self.preprocessor_line(&format!("databases = [{}]", list.join(", ")));
    }

    /// An empty root with no `oxabl.toml`.
    pub fn bare() -> Workspace {
        Workspace {
            dir: TempDir::new().expect("a temporary directory"),
        }
    }

    pub fn root(&self) -> &Path {
        // A real path: oxabl resolves links, and the worker reports its working directory.
        self.dir.path()
    }

    pub fn write(&self, relative: &str, contents: &str) -> PathBuf {
        let path = self.root().join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, contents).unwrap();
        path
    }

    pub fn path(&self, relative: &str) -> PathBuf {
        self.root().join(relative)
    }

    /// Writes a fake AVM environment file and returns its name under the root.
    pub fn env_file(&self, name: &str, env: &Value) -> String {
        self.write(name, &env.to_string());
        name.to_string()
    }

    /// `oxabl build --probe` in this workspace.
    pub fn probe(&self, args: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .current_dir(self.root())
            .args(["build", "--probe"])
            .args(args);
        command
    }

    /// The fake AVM log written by workers that run with `FAKE_AVM_LOG`.
    pub fn fake_log(&self) -> PathBuf {
        self.path("fake-avm.log")
    }

    /// Every worker log of every build, concatenated.
    pub fn worker_logs(&self) -> String {
        let logs = self.path(".oxabl/build/logs");
        let mut text = String::new();
        let Ok(builds) = fs::read_dir(logs) else {
            return text;
        };
        for build in builds.flatten() {
            for log in fs::read_dir(build.path()).into_iter().flatten().flatten() {
                text.push_str(&fs::read_to_string(log.path()).unwrap_or_default());
            }
        }
        text
    }
}

/// Runs a command and returns its output.
pub fn run(mut command: Command) -> Output {
    command.output().expect("the command runs")
}

pub fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

pub fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// Runs `oxabl build --probe --json` with the environment and parses the answer.
pub fn probe_json(workspace: &Workspace, envs: &[(&str, &str)]) -> Value {
    let mut command = workspace.probe(&["--json"]);
    command.envs(envs.iter().copied());
    let output = run(command);
    assert_eq!(
        output.status.code(),
        Some(0),
        "probe failed: {}",
        stderr(&output)
    );
    serde_json::from_str(&stdout(&output)).expect("the probe prints one JSON document")
}

pub fn identity(report: &Value) -> String {
    report["environment"]["identity"]
        .as_str()
        .expect("an identity")
        .to_string()
}

/// A fake AVM environment with one database and the given schema records.
pub fn env_with_schema(records: Vec<Vec<String>>) -> Value {
    json!({ "databases": [{ "ldb": "ledger", "schema": records }] })
}

/// The schema identity as the spec derives it. Each record belongs to an
/// entry: the database record, a table (with its fields, indexes and
/// triggers) or a sequence. An entry's text is its records, each field
/// percent-encoded and joined with TAB, each record ended by LF. The identity
/// is BLAKE3 derive-key over the sorted encoded entries
/// (kind, name, record count, SHA-256 of the text).
pub fn schema_identity(records: &[Vec<String>]) -> String {
    use sha2::{Digest, Sha256};

    fn u64_le(out: &mut Vec<u8>, value: usize) {
        out.extend_from_slice(&(value as u64).to_le_bytes());
    }
    fn str_enc(out: &mut Vec<u8>, text: &str) {
        u64_le(out, text.len());
        out.extend_from_slice(text.as_bytes());
    }
    fn percent(field: &str) -> String {
        field
            .bytes()
            .map(|b| {
                if (0x20..=0x7E).contains(&b) && b != b'%' {
                    (b as char).to_string()
                } else {
                    format!("%{b:02X}")
                }
            })
            .collect()
    }
    let mut entries: Vec<(String, String, usize, String)> = Vec::new();
    for record in records {
        let name = record.get(1).cloned().unwrap_or_default();
        let (kind, name) = match record[0].as_str() {
            "db" => ("db", String::new()),
            "sequence" => ("sequence", name),
            _ => ("table", name),
        };
        let at = entries
            .iter()
            .position(|e| e.0 == kind && e.1 == name)
            .unwrap_or_else(|| {
                entries.push((kind.to_string(), name.clone(), 0, String::new()));
                entries.len() - 1
            });
        entries[at].2 += 1;
        let fields: Vec<String> = record.iter().map(|f| percent(f)).collect();
        entries[at].3.push_str(&format!("{}\n", fields.join("\t")));
    }
    let mut encoded: Vec<Vec<u8>> = entries
        .iter()
        .map(|(kind, name, count, text)| {
            let sha: String = Sha256::digest(text.as_bytes())
                .iter()
                .map(|b| format!("{b:02x}"))
                .collect();
            let mut bytes = Vec::new();
            str_enc(&mut bytes, kind);
            str_enc(&mut bytes, name);
            u64_le(&mut bytes, *count);
            str_enc(&mut bytes, &sha);
            bytes
        })
        .collect();
    encoded.sort();
    let mut material = Vec::new();
    u64_le(&mut material, encoded.len());
    for entry in encoded {
        material.extend_from_slice(&entry);
    }
    let mut hasher = blake3::Hasher::new_derive_key("oxabl 2026-11 schema image v2");
    hasher.update(&material);
    hasher.finalize().to_hex().to_string()
}

/// Process ids that fake workers wrote to the log at start.
pub fn started_pids(log: &Path) -> Vec<u32> {
    fs::read_to_string(log)
        .unwrap_or_default()
        .lines()
        .filter_map(|line| {
            let (pid, what) = line.split_once(' ')?;
            (what == "start").then(|| pid.parse().ok()).flatten()
        })
        .collect()
}

pub fn process_exists(pid: u32) -> bool {
    Path::new(&format!("/proc/{pid}")).exists()
}
