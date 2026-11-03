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
