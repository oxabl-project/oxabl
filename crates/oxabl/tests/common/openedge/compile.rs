//! A workspace that a real AVM compiles.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use serde_json::Value;

use super::{Probe, Target, unique_name};

/// The tables of the configuration that a test may change.
pub struct Options {
    /// Lines of `[workspace.sources]`.
    pub sources: String,
    /// Lines of `[build.outputs]`; empty leaves the table out.
    pub outputs: String,
    /// Further tables, written after `[build.outputs]`.
    pub extra: String,
    pub workers: usize,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            sources: "directories = [\"src\"]\ninclude_paths = [\"src\"]".into(),
            outputs: "xref = \"build/xref\"".into(),
            extra: String::new(),
            workers: 2,
        }
    }
}

/// A workspace the worker sees, with a fixture to compile, removed on drop.
pub struct Compile {
    pub host: PathBuf,
    pub worker: String,
    options: Options,
    target_command: Vec<String>,
    proversion: String,
    databases: Vec<String>,
    _guard: Probe,
}

impl Compile {
    /// The host path of a file below the workspace.
    pub fn path(&self, relative: &str) -> PathBuf {
        self.host.join(relative)
    }

    /// Writes `oxabl.toml` again, after a change of `options`.
    fn write_config(&self) {
        let quote = |items: &[String]| {
            items
                .iter()
                .map(|item| format!("{item:?}"))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let outputs = match self.options.outputs.is_empty() {
            true => String::new(),
            false => format!("[build.outputs]\n{}\n\n", self.options.outputs),
        };
        fs::write(
            self.path("oxabl.toml"),
            format!(
                "[workspace]\nname = \"openedge\"\n\n[workspace.preprocessor]\nproversion = {:?}\ndatabases = [{}]\n\n[workspace.sources]\n{}\n\n[build]\noutput = \"build/r\"\n\n{outputs}{}\n\n[build.workers]\ncommand = [{}]\nroot = {:?}\ncount = {}\nstart_timeout = \"10m\"\nunit_timeout = \"30m\"\n",
                self.proversion,
                quote(&self.databases),
                self.options.sources,
                self.options.extra,
                quote(&self.target_command),
                self.worker,
                self.options.workers,
            ),
        )
        .unwrap();
    }

    /// Replaces the lines of `[build.outputs]`; empty removes the table.
    pub fn set_outputs(&mut self, outputs: &str) {
        self.options.outputs = outputs.into();
        self.write_config();
    }

    /// Takes the XREF output out of the configuration, so that the build
    /// writes a private text `XREF` for its own checks.
    pub fn without_xref_output(&mut self) {
        assert!(
            !self.options.outputs.is_empty(),
            "the configuration has an XREF output"
        );
        self.set_outputs("");
    }

    /// Writes a file below the workspace.
    pub fn write(&self, relative: &str, text: &str) {
        let path = self.path(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    pub fn read(&self, relative: &str) -> String {
        fs::read_to_string(self.path(relative))
            .unwrap_or_else(|e| panic!("{relative} cannot be read: {e}"))
    }

    pub fn bytes(&self, relative: &str) -> Vec<u8> {
        fs::read(self.path(relative)).unwrap_or_else(|e| panic!("{relative} cannot be read: {e}"))
    }

    pub fn exists(&self, relative: &str) -> bool {
        self.path(relative).exists()
    }

    pub fn remove(&self, relative: &str) {
        fs::remove_file(self.path(relative)).unwrap();
    }

    /// Copies the fixture project into `src/`.
    pub fn copy_fixture(&self) {
        let from = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/openedge");
        copy_tree(&from, &self.path("src"));
    }

    /// Every `.r` file below `build/r`, by its path there.
    pub fn rcode(&self) -> BTreeMap<String, Vec<u8>> {
        let mut found = BTreeMap::new();
        collect(&self.path("build/r"), "", &mut found);
        found
    }

    /// Deletes every `.r` file below `build/r`, so that the next build compiles
    /// every unit again.
    pub fn drop_rcode(&self) {
        for name in self.rcode().keys() {
            fs::remove_file(self.path(&format!("build/r/{name}"))).unwrap();
        }
    }

    /// `oxabl build` with `args`, no cache; the build must exit with `code`.
    pub fn build(&self, args: &[&str], code: i32) -> Output {
        self.build_with_cache(args, code, "ci", None)
    }

    /// The JSON document of `oxabl build --json` that exits 0.
    pub fn build_report(&self) -> Value {
        let output = self.build(&["--json"], 0);
        serde_json::from_slice(&output.stdout).expect("one JSON document")
    }

    /// `oxabl build --plan --probe --json --all`.
    pub fn plan(&self) -> Value {
        let output = self.build(&["--plan", "--probe", "--json", "--all"], 0);
        serde_json::from_slice(&output.stdout).expect("one JSON document")
    }

    /// `oxabl build` with `args` in cache mode `mode`, and the shared local
    /// cache in `cache` when given.
    pub fn build_with_cache(
        &self,
        args: &[&str],
        code: i32,
        mode: &str,
        cache: Option<&Path>,
    ) -> Output {
        let mut command = self.oxabl(args);
        command.env("OXABL_CACHE_MODE", mode);
        if let Some(cache) = cache {
            command.env("OXABL_BUILD_CACHE_DIR", cache);
        }
        let output = command.output().expect("oxabl runs");
        assert_eq!(
            output.status.code(),
            Some(code),
            "stdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }

    /// An `oxabl build` command that is not started.
    pub fn oxabl(&self, args: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .current_dir(&self.host)
            .env_remove("CI")
            .env_remove("OXABL_BUILD_CACHE_DIR")
            .env("OXABL_CACHE_MODE", "ci")
            .arg("build")
            .args(args);
        command
    }

    /// Runs the ABL program `source` through the worker command, in place of
    /// the worker program, and returns what it printed.
    pub fn run_program(&self, target: &Target, source: &str) -> String {
        super::worker::run_program(target, &self.host, &self.worker, source)
    }
}

fn collect(dir: &Path, prefix: &str, found: &mut BTreeMap<String, Vec<u8>>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries {
        let entry = entry.unwrap();
        let name = format!("{prefix}{}", entry.file_name().to_string_lossy());
        if entry.file_type().unwrap().is_dir() {
            collect(&entry.path(), &format!("{name}/"), found);
        } else if name.ends_with(".r") {
            found.insert(name, fs::read(entry.path()).unwrap());
        }
    }
}

fn copy_tree(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        match entry.file_type().unwrap().is_dir() {
            true => copy_tree(&entry.path(), &target),
            false => {
                fs::copy(entry.path(), target).unwrap();
            }
        }
    }
}

/// A workspace under `target.workspace` with the configuration of a build.
pub fn compile_workspace(target: &Target) -> Compile {
    compile_workspace_with(target, Options::default())
}

/// Like [`compile_workspace`], with `options`.
pub fn compile_workspace_with(target: &Target, options: Options) -> Compile {
    let name = unique_name("oxabl-compile");
    let host = target.workspace.join(&name);
    fs::create_dir_all(&host).unwrap();
    let workspace = Compile {
        worker: format!("{}/{name}", target.worker_root.trim_end_matches('/')),
        host: host.clone(),
        options,
        target_command: target.command.clone(),
        proversion: target.proversion.clone(),
        databases: target.databases.clone(),
        _guard: Probe { root: host },
    };
    workspace.write_config();
    workspace
}
