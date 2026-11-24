//! A workspace whose worker is the fake AVM, and `oxabl build --plan --probe`
//! run over it.

#![allow(dead_code)]

use std::cell::RefCell;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde_json::{Value, json};
use tempfile::TempDir;

use super::{fake_avm, run, stderr, stdout};

/// One workspace: sources under `src/`, the fake worker, and an environment
/// file the tests edit.
pub struct Keyed {
    dir: TempDir,
    sources: RefCell<String>,
    extra: RefCell<String>,
    preprocessor: RefCell<String>,
}

/// The plan of one run.
pub struct Plan {
    pub document: Value,
    pub stderr: String,
}

impl Plan {
    pub fn unit(&self, path: &str) -> &Value {
        self.document["units"]
            .as_array()
            .unwrap()
            .iter()
            .find(|unit| unit["path"] == path)
            .unwrap_or_else(|| panic!("no unit {path} in {}", self.document))
    }

    pub fn key(&self, path: &str) -> Option<String> {
        self.unit(path)["key"].as_str().map(str::to_string)
    }

    /// The key, which the unit must have.
    pub fn key_of(&self, path: &str) -> String {
        self.key(path)
            .unwrap_or_else(|| panic!("{path} has no key: {}", self.unit(path)))
    }

    /// The reasons a unit has no key, as listed.
    pub fn unkeyable(&self, path: &str) -> Vec<String> {
        self.unit(path)["unkeyable"]
            .as_array()
            .unwrap()
            .iter()
            .map(|reason| reason.as_str().unwrap().to_string())
            .collect()
    }

    pub fn action(&self, path: &str) -> &str {
        self.unit(path)["action"].as_str().unwrap()
    }

    pub fn identity(&self) -> String {
        self.document["environment"]["identity"]
            .as_str()
            .unwrap()
            .to_string()
    }

    pub fn propath(&self, path: &str) -> Vec<(String, String)> {
        let at = self.unit(path)["propath"].as_u64().unwrap() as usize;
        self.document["propaths"][at]
            .as_array()
            .unwrap()
            .iter()
            .map(|entry| {
                (
                    entry["entry"].as_str().unwrap().to_string(),
                    entry["digest"].as_str().unwrap().to_string(),
                )
            })
            .collect()
    }
}

fn default_environment() -> Value {
    json!({ "dlc_dir": "dlc", "env": { "dlc": "/usr/dlc" } })
}

impl Keyed {
    /// A workspace with `[workspace.sources]` holding `directories = ["src"]`
    /// and `sources`, and `extra` as further tables of `oxabl.toml`.
    pub fn new(sources: &str, extra: &str) -> Keyed {
        let workspace = Keyed {
            dir: TempDir::new().expect("a temporary directory"),
            sources: RefCell::new(sources.to_string()),
            extra: RefCell::new(extra.to_string()),
            preprocessor: RefCell::new(String::new()),
        };
        workspace.write_config();
        workspace.set_environment(&default_environment());
        fs::create_dir_all(workspace.path("dlc")).unwrap();
        workspace
    }

    /// A workspace whose procedures are found through `include_paths = ["src"]`.
    pub fn simple() -> Keyed {
        Keyed::new("include_paths = [\"src\"]", "")
    }

    fn write_config(&self) {
        let command = [
            fake_avm().to_string_lossy().into_owned(),
            "{program}".to_string(),
            "--env".to_string(),
            "{root}/env.json".to_string(),
        ];
        let quoted: Vec<String> = command.iter().map(|arg| format!("{arg:?}")).collect();
        self.write(
            "oxabl.toml",
            &format!(
                "[workspace]\nname = \"keyed\"\n\n[workspace.preprocessor]\nproversion = \"12.8\"\n{}\n\n[workspace.sources]\n{}\ndlc = \"dlc\"\n{}\n\n{}\n\n[build.workers]\ncommand = [{}]\ncount = 1\n",
                self.preprocessor.borrow(),
                match self.sources.borrow().contains("directories") {
                    true => "",
                    false => "directories = [\"src\"]",
                },
                self.sources.borrow(),
                self.extra.borrow(),
                quoted.join(", ")
            ),
        );
    }

    /// Replaces the `[workspace.sources]` keys and the further tables.
    pub fn reconfigure(&self, sources: &str, extra: &str) {
        *self.sources.borrow_mut() = sources.to_string();
        *self.extra.borrow_mut() = extra.to_string();
        self.write_config();
    }

    /// Replaces the lines after `proversion` in `[workspace.preprocessor]`.
    pub fn set_preprocessor(&self, lines: &str) {
        *self.preprocessor.borrow_mut() = format!("{lines}\n");
        self.write_config();
    }

    /// Adds a line to `[workspace.preprocessor]`.
    pub fn preprocessor_line(&self, line: &str) {
        let mut lines = self.preprocessor.borrow_mut();
        lines.push_str(line);
        lines.push('\n');
        drop(lines);
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

    pub fn set_environment(&self, environment: &Value) {
        self.write("env.json", &environment.to_string());
    }

    /// Changes the environment file in place.
    pub fn edit_environment(&self, change: impl FnOnce(&mut Value)) {
        let mut environment: Value =
            serde_json::from_str(&fs::read_to_string(self.path("env.json")).unwrap()).unwrap();
        change(&mut environment);
        self.set_environment(&environment);
    }

    /// `oxabl build --plan --probe --json --all` with `extra` arguments.
    pub fn command(&self, extra: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .current_dir(self.root())
            .env_remove("OXABL_DLC_SOURCES")
            .args(["build", "--plan", "--probe", "--json", "--all"])
            .args(extra);
        command
    }

    /// The keyed plan, which must succeed.
    pub fn plan(&self) -> Plan {
        let output = run(self.command(&[]));
        assert_eq!(
            output.status.code(),
            Some(0),
            "stdout:\n{}\nstderr:\n{}",
            stdout(&output),
            stderr(&output)
        );
        Plan {
            document: serde_json::from_str(&stdout(&output)).expect("one JSON document"),
            stderr: stderr(&output),
        }
    }

    /// The plan with no probe: `oxabl build --plan --json --all`.
    pub fn plan_without_probe(&self) -> Plan {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .current_dir(self.root())
            .env_remove("OXABL_DLC_SOURCES")
            .args(["build", "--plan", "--json", "--all"]);
        let output = run(command);
        assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
        Plan {
            document: serde_json::from_str(&stdout(&output)).unwrap(),
            stderr: stderr(&output),
        }
    }
}

/// Copies the files of `from` to a new temporary directory, leaving out the
/// state directory.
pub fn copy_workspace(from: &Path) -> TempDir {
    fn copy(from: &Path, to: &Path) {
        fs::create_dir_all(to).unwrap();
        for entry in fs::read_dir(from).unwrap().flatten() {
            let name = entry.file_name();
            if name == ".oxabl" {
                continue;
            }
            let target = to.join(&name);
            if entry.file_type().unwrap().is_dir() {
                copy(&entry.path(), &target);
            } else {
                fs::copy(entry.path(), target).unwrap();
            }
        }
    }
    let to = TempDir::new().unwrap();
    copy(from, to.path());
    to
}

impl Keyed {
    /// The worked example of the key specification: the unit
    /// `src/ar/post.p`, its two includes (one under `$DLC`), a library
    /// on the PROPATH and one database.
    pub fn example() -> Keyed {
        let workspace = Keyed::new("include_paths = [\"src\", \"$DLC/src\"]", "");
        workspace.write(
            "src/ar/post.p",
            "{inc/ledger.i}\n{adecomm/x.i}\nMESSAGE \"post\".\n",
        );
        workspace.write(
            "src/inc/ledger.i",
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n",
        );
        workspace.write(
            "dlc/src/adecomm/x.i",
            "DEFINE VARIABLE y AS CHARACTER NO-UNDO.\n",
        );
        workspace.write("dlc/tty/netlib/OpenEdge.Net.pl", "library bytes");
        workspace.set_environment(&example_environment());
        workspace.preprocessor_line("databases = [\"ledger\"]");
        workspace
    }
}

/// The environment of the worked example.
pub fn example_environment() -> Value {
    json!({
        "dlc_dir": "dlc",
        "env": {
            "dlc": "/usr/dlc",
            "dlc_version": "OpenEdge Release 12.8.9",
            "cpcoll": "ICU-UCA",
            "propath": "/usr/dlc/tty/netlib/OpenEdge.Net.pl"
        },
        "parameters": ["-cpinternal UTF-8", "-cpstream UTF-8", "-d mdy", "-inp 32000"],
        "databases": [{
            "ldb": "ledger",
            "aliases": ["dictdb"],
            "schema": [
                ["db", "_db-coll-name=ICU-UCA", "_db-type=PROGRESS", "_db-xl-name=UTF-8"],
                ["table", "customer", "_can-read=*", "_crc=12345", "_dump-name=customer",
                 "_prime-index=index/customer/custnum"],
                ["field", "customer", "custnum", "_data-type=integer", "_decimals=?",
                 "_format=>>>>9", "_label=Cust Num", "_view-as=?"],
                ["field", "customer", "name", "_data-type=character", "_decimals=?",
                 "_format=x(30)", "_label=Name", "_view-as=?"],
                ["index", "customer", "custnum", "_active=yes", "_unique=yes"],
                ["index-field", "customer", "custnum", "1", "custnum", "_ascending=yes"],
                ["sequence", "next-cust", "_cycle-ok=no", "_seq-incr=1", "_seq-init=1"]
            ]
        }]
    })
}
