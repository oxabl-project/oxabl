//! A workspace whose worker is the fake AVM and whose build output lands in
//! `build/r`: `oxabl build` run over it.

#![allow(dead_code)]

use std::cell::RefCell;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

use serde_json::Value;
use tempfile::TempDir;

use super::{fake_avm, run, stderr, stdout};

/// One workspace: sources under `src/`, the fake worker and an environment
/// file the tests edit.
pub struct Built {
    dir: TempDir,
    sources: RefCell<String>,
    build_keys: RefCell<String>,
    extra: RefCell<String>,
    workers: RefCell<String>,
}

/// The answer of one `oxabl build` run.
pub struct Run {
    pub code: i32,
    pub stdout: String,
    pub stderr: String,
}

impl Run {
    pub fn succeeded(&self) -> &Self {
        assert_eq!(
            self.code, 0,
            "stdout:\n{}\nstderr:\n{}",
            self.stdout, self.stderr
        );
        self
    }

    /// The JSON events of `--events`, in order.
    pub fn events(&self) -> Vec<Value> {
        self.stdout
            .lines()
            .map(|line| serde_json::from_str(line).unwrap_or_else(|e| panic!("{line}: {e}")))
            .collect()
    }
}

/// The events of `kind`.
pub fn of_kind<'a>(events: &'a [Value], kind: &str) -> Vec<&'a Value> {
    events
        .iter()
        .filter(|event| event["event"] == kind)
        .collect()
}

impl Built {
    /// Procedures and classes under `src/`, found through
    /// `include_paths = ["src"]`, one worker.
    pub fn new() -> Built {
        Built::with("include_paths = [\"src\"]", "")
    }

    /// `sources` are `[workspace.sources]` keys, `extra` further tables.
    pub fn with(sources: &str, extra: &str) -> Built {
        let workspace = Built {
            dir: TempDir::new().expect("a temporary directory"),
            sources: RefCell::new(sources.to_string()),
            build_keys: RefCell::new(String::new()),
            extra: RefCell::new(extra.to_string()),
            workers: RefCell::new("count = 1".to_string()),
        };
        workspace.write_config();
        workspace.write("env.json", "{}");
        fs::create_dir_all(workspace.path("fake-state")).unwrap();
        workspace
    }

    fn write_config(&self) {
        let command = [
            fake_avm().to_string_lossy().into_owned(),
            "{program}".to_string(),
            "--env".to_string(),
            "{root}/env.json".to_string(),
        ];
        let quoted: Vec<String> = command.iter().map(|arg| format!("{arg:?}")).collect();
        let sources = self.sources.borrow();
        let directories = match sources.contains("directories") {
            true => "",
            false => "directories = [\"src\"]",
        };
        self.write(
            "oxabl.toml",
            &format!(
                "[workspace]\nname = \"built\"\n\n[workspace.preprocessor]\nproversion = \"12.8\"\n\n[workspace.sources]\n{directories}\n{sources}\n\n[build]\noutput = \"build/r\"\n{}\n\n{}\n\n[build.workers]\ncommand = [{}]\n{}\nenv = {{ FAKE_AVM_LOG = {:?}, FAKE_AVM_STATE = {:?} }}\n",
                self.build_keys.borrow(),
                self.extra.borrow(),
                quoted.join(", "),
                self.workers.borrow(),
                self.log().to_string_lossy(),
                self.path("fake-state").to_string_lossy()
            ),
        );
    }

    /// Replaces the `[workspace.sources]` keys and the further tables.
    pub fn reconfigure(&self, sources: &str, extra: &str) {
        *self.sources.borrow_mut() = sources.to_string();
        *self.extra.borrow_mut() = extra.to_string();
        self.write_config();
    }

    /// Replaces the lines of `[build.workers]` after `command`.
    pub fn workers(&self, lines: &str) {
        *self.workers.borrow_mut() = lines.to_string();
        self.write_config();
    }

    /// Replaces the keys of `[build]` after `output`.
    pub fn build_keys(&self, lines: &str) {
        *self.build_keys.borrow_mut() = lines.to_string();
        self.write_config();
    }

    pub fn root(&self) -> &Path {
        self.dir.path()
    }

    pub fn path(&self, relative: &str) -> PathBuf {
        self.root().join(relative)
    }

    pub fn write(&self, relative: &str, text: &str) {
        let path = self.path(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    pub fn remove(&self, relative: &str) {
        fs::remove_file(self.path(relative)).unwrap();
    }

    pub fn read(&self, relative: &str) -> String {
        fs::read_to_string(self.path(relative))
            .unwrap_or_else(|e| panic!("cannot read {relative}: {e}"))
    }

    pub fn exists(&self, relative: &str) -> bool {
        self.path(relative).exists()
    }

    /// Replaces the fake AVM's environment file.
    pub fn set_environment(&self, environment: &Value) {
        self.write("env.json", &environment.to_string());
    }

    pub fn log(&self) -> PathBuf {
        self.path("fake.log")
    }

    /// `oxabl build` with `args`, the fake AVM logging to `fake.log`.
    pub fn command(&self, args: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .current_dir(self.root())
            .env_remove("OXABL_DLC_SOURCES")
            .env("FAKE_AVM_LOG", self.log())
            .env("FAKE_AVM_STATE", self.path("fake-state"))
            .arg("build")
            .args(args);
        command
    }

    pub fn build_with(&self, args: &[&str]) -> Run {
        let output = run(self.command(args));
        Run {
            code: output.status.code().unwrap_or(-1),
            stdout: stdout(&output),
            stderr: stderr(&output),
        }
    }

    pub fn build(&self) -> Run {
        self.build_with(&[])
    }

    /// Starts `oxabl build` without waiting.
    pub fn spawn(&self, args: &[&str]) -> Child {
        self.command(args)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("oxabl starts")
    }

    /// `oxabl build --plan --probe --json --all` with `extra` arguments.
    pub fn plan(&self) -> Value {
        let output = run(self.command(&["--plan", "--probe", "--json", "--all"]));
        assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
        serde_json::from_str(&stdout(&output)).expect("one JSON document")
    }

    /// Every line of the fake log: the worker process and what it logged.
    pub fn log_lines(&self) -> Vec<(u32, String)> {
        fs::read_to_string(self.log())
            .unwrap_or_default()
            .lines()
            .filter_map(|line| {
                let (pid, what) = line.split_once(' ')?;
                Some((pid.parse().ok()?, what.to_string()))
            })
            .collect()
    }

    /// The compile names the fake compiled, in log order.
    pub fn compiled(&self) -> Vec<String> {
        self.log_lines()
            .into_iter()
            .map(|(_, what)| what)
            .filter(|what| what != "start" && what != "exit" && !what.starts_with("schema "))
            .collect()
    }

    pub fn clear_log(&self) {
        let _ = fs::remove_file(self.log());
    }

    /// The `read <path> <sha256>` lines of an r-code file of the fake.
    pub fn reads(&self, relative: &str) -> Vec<(String, String)> {
        self.read(relative)
            .lines()
            .filter_map(|line| {
                let mut parts = line.strip_prefix("read ")?.split(' ');
                Some((parts.next()?.to_string(), parts.next()?.to_string()))
            })
            .collect()
    }

    /// The SHA-256 the fake records for the file at `relative`.
    pub fn sha256(&self, relative: &str) -> String {
        use sha2::{Digest, Sha256};
        Sha256::digest(fs::read(self.path(relative)).unwrap())
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect()
    }

    /// The reasons of a unit in a plan, by kind.
    pub fn reasons(plan: &Value, path: &str) -> Vec<String> {
        plan["units"]
            .as_array()
            .unwrap()
            .iter()
            .find(|unit| unit["path"] == path)
            .unwrap_or_else(|| panic!("no unit {path} in {plan}"))["reasons"]
            .as_array()
            .unwrap()
            .iter()
            .map(|reason| reason["kind"].as_str().unwrap().to_string())
            .collect()
    }
}

/// The unit of a plan or a report at `path`.
pub fn unit<'a>(document: &'a Value, path: &str) -> &'a Value {
    document["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|unit| unit["path"] == path)
        .unwrap_or_else(|| panic!("no unit {path} in {document}"))
}
