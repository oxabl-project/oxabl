//! A workspace on disk and `oxabl build --plan` run over it.

#![allow(dead_code)]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::TempDir;

pub struct Workspace {
    dir: TempDir,
}

/// The answer of one `oxabl build` run.
pub struct Run {
    pub code: i32,
    pub stdout: String,
    pub stderr: String,
}

impl Run {
    /// The lines that name a unit to rebuild, with runs of spaces made one.
    pub fn rebuild(&self) -> Vec<String> {
        self.lines_starting("rebuild")
    }

    /// The lines that start with `word`, with runs of spaces made one.
    pub fn lines_starting(&self, word: &str) -> Vec<String> {
        self.stdout
            .lines()
            .map(|line| line.split_whitespace().collect::<Vec<_>>().join(" "))
            .filter(|line| line.starts_with(word))
            .collect()
    }

    /// The last line: the counts and the store generation.
    pub fn summary(&self) -> &str {
        self.stdout.lines().last().unwrap_or_default()
    }

    /// The generation the summary line names.
    pub fn generation(&self) -> u64 {
        let tail = self
            .summary()
            .split("store generation ")
            .nth(1)
            .unwrap_or_else(|| panic!("no generation in {:?}", self.summary()));
        tail.split_whitespace().next().unwrap().parse().unwrap()
    }

    pub fn succeeded(&self) -> &Self {
        assert_eq!(
            self.code, 0,
            "stdout:\n{}\nstderr:\n{}",
            self.stdout, self.stderr
        );
        self
    }
}

impl Workspace {
    /// A workspace whose `oxabl.toml` names `src` as the unit directory and has
    /// `sources` as further `[workspace.sources]` keys.
    pub fn with_sources(sources: &str) -> Self {
        Self::with_config(&format!(
            "[workspace]\nname = \"t\"\n\n[workspace.sources]\ndirectories = [\"src\"]\n{sources}\n"
        ))
    }

    /// A workspace with `config` as its whole `oxabl.toml`.
    pub fn with_config(config: &str) -> Self {
        let workspace = Workspace {
            dir: TempDir::new().unwrap(),
        };
        workspace.write("oxabl.toml", config);
        workspace
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

    pub fn state_dir(&self) -> PathBuf {
        self.path(".oxabl/build")
    }

    pub fn command(&self, arguments: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .arg("build")
            .arg("--root")
            .arg(self.root())
            .args(arguments)
            .current_dir(self.root())
            .env_remove("OXABL_DLC_SOURCES");
        command
    }

    pub fn run(&self, arguments: &[&str]) -> Run {
        run_of(self.command(arguments).output().unwrap())
    }

    /// `oxabl build --plan`.
    pub fn plan(&self) -> Run {
        self.run(&["--plan"])
    }
}

pub fn run_of(output: Output) -> Run {
    Run {
        code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

impl Workspace {
    /// Run `git` in the workspace with a fixed identity and no global config.
    pub fn git(&self, arguments: &[&str]) {
        let output = Command::new("git")
            .args(["-c", "user.name=t", "-c", "user.email=t@example.com"])
            .args([
                "-c",
                "commit.gpgsign=false",
                "-c",
                "init.defaultBranch=main",
            ])
            .args(arguments)
            .current_dir(self.root())
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {arguments:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    /// Make the workspace a git repository with everything committed.
    pub fn commit_all(&self, message: &str) {
        if !self.path(".git").exists() {
            self.git(&["init", "--quiet"]);
        }
        self.write(".gitignore", ".oxabl/\n");
        self.git(&["add", "-A"]);
        self.git(&["commit", "--quiet", "-m", message]);
    }
}
