//! Probing a real AVM from the opt-in OpenEdge tests.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

use serde_json::Value;

/// A worker command and the workspace it runs in.
pub struct Target {
    /// The worker command, with the placeholders `{program}` and `{root}`.
    pub command: Vec<String>,
    /// A host directory that the worker sees; each probe makes a directory in it.
    pub workspace: PathBuf,
    /// Where the worker sees `workspace`.
    pub worker_root: String,
    pub proversion: String,
    /// The logical names the command connects, in connection order.
    pub databases: Vec<String>,
}

/// A probe workspace that the worker sees, removed on drop.
pub struct Probe {
    root: PathBuf,
}

impl Drop for Probe {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

/// Runs `oxabl build --probe --json` with `workers` workers and prints the report.
pub fn probe(target: &Target, workers: usize) -> (Value, Probe) {
    let name = format!(
        "oxabl-probe-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let root = target.workspace.join(&name);
    fs::create_dir_all(&root).unwrap();
    let guard = Probe { root: root.clone() };
    let command: Vec<String> = target
        .command
        .iter()
        .map(|arg| format!("{arg:?}"))
        .collect();
    let databases: Vec<String> = target
        .databases
        .iter()
        .map(|name| format!("{name:?}"))
        .collect();
    fs::write(
        root.join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"openedge\"\n\n[workspace.preprocessor]\nproversion = {:?}\ndatabases = [{}]\n\n[build.workers]\ncommand = [{}]\nroot = {:?}\ncount = {workers}\nstart_timeout = \"10m\"\nunit_timeout = \"30m\"\n",
            target.proversion,
            databases.join(", "),
            command.join(", "),
            format!("{}/{name}", target.worker_root.trim_end_matches('/')),
        ),
    )
    .unwrap();
    let started = std::time::Instant::now();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(&root)
        .args(["build", "--probe", "--json"])
        .output()
        .expect("oxabl runs");
    assert_eq!(
        output.status.code(),
        Some(0),
        "probe failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let report: Value = serde_json::from_slice(&output.stdout).expect("one JSON document");
    eprintln!("probe took {:.1}s", started.elapsed().as_secs_f64());
    eprintln!("{}", serde_json::to_string_pretty(&report).unwrap());
    (report, guard)
}

pub fn is_hex64(value: &Value) -> bool {
    value
        .as_str()
        .is_some_and(|text| text.len() == 64 && text.bytes().all(|b| b.is_ascii_hexdigit()))
}

/// A workspace the worker sees, with a fixture to compile, removed on drop.
pub struct Compile {
    pub host: PathBuf,
    pub worker: String,
    _guard: Probe,
}

impl Compile {
    /// The host path of a file below the workspace.
    pub fn path(&self, relative: &str) -> PathBuf {
        self.host.join(relative)
    }

    /// Writes a file below the workspace.
    pub fn write(&self, relative: &str, text: &str) {
        let path = self.path(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    /// `oxabl build` with `args`; the build must exit with `code`.
    pub fn build(&self, args: &[&str], code: i32) -> std::process::Output {
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .current_dir(&self.host)
            .arg("build")
            .args(args)
            .output()
            .expect("oxabl runs");
        assert_eq!(
            output.status.code(),
            Some(code),
            "stdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }

    /// Runs the ABL program `source` through the worker command, in place of
    /// the worker program, and returns what it printed.
    pub fn run_program(&self, target: &Target, source: &str) -> String {
        self.write("check/check.p", source);
        let program = format!("{}/check/check.p", self.worker);
        let expanded: Vec<String> = target
            .command
            .iter()
            .map(|arg| {
                arg.replace("{program}", &program)
                    .replace("{root}", &self.worker)
            })
            .collect();
        let output = Command::new(&expanded[0])
            .args(&expanded[1..])
            .current_dir(&self.host)
            .stdin(std::process::Stdio::null())
            .output()
            .expect("the worker command runs");
        String::from_utf8_lossy(&output.stdout).into_owned()
    }
}

/// A workspace under `target.workspace` with the configuration of a build.
pub fn compile_workspace(target: &Target) -> Compile {
    let name = format!(
        "oxabl-compile-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let host = target.workspace.join(&name);
    fs::create_dir_all(&host).unwrap();
    let guard = Probe { root: host.clone() };
    let worker = format!("{}/{name}", target.worker_root.trim_end_matches('/'));
    let command: Vec<String> = target
        .command
        .iter()
        .map(|arg| format!("{arg:?}"))
        .collect();
    let databases: Vec<String> = target
        .databases
        .iter()
        .map(|name| format!("{name:?}"))
        .collect();
    fs::write(
        host.join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"openedge\"\n\n[workspace.preprocessor]\nproversion = {:?}\ndatabases = [{}]\n\n[workspace.sources]\ndirectories = [\"src\"]\ninclude_paths = [\"src\"]\n\n[build]\noutput = \"build/r\"\n\n[build.outputs]\nxref = \"build/xref\"\n\n[build.workers]\ncommand = [{}]\nroot = {worker:?}\ncount = 2\nstart_timeout = \"10m\"\nunit_timeout = \"30m\"\n",
            target.proversion,
            databases.join(", "),
            command.join(", "),
        ),
    )
    .unwrap();
    Compile {
        host,
        worker,
        _guard: guard,
    }
}
