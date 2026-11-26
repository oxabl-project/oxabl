//! The jobs of `.github/workflows/ci.yml`, in the order of that file. A job is a list of steps
//! that stops at its first failure. The runner setup (checkout, toolchain, caches, node) stays
//! in the workflow; everything else is here.

use crate::Context;
use crate::checks::{self, Outcome};
use crate::tools::Need;
use std::path::PathBuf;

pub struct Job {
    pub name: &'static str,
    pub steps: Vec<Step>,
    /// Files and directories that the job rewrites. The runner puts them back afterwards.
    pub restore: Vec<PathBuf>,
}

pub struct Step {
    pub name: &'static str,
    pub needs: Vec<Need>,
    pub action: Action,
}

pub enum Action {
    Run(Command),
    Check(fn(&Context) -> Outcome),
}

/// One program run. Paths are relative to the checkout unless they are absolute.
pub struct Command {
    pub program: &'static str,
    pub args: Vec<String>,
    pub directory: PathBuf,
    pub env: Vec<(&'static str, String)>,
    /// Write standard output to this file instead of the log.
    pub stdout_to: Option<PathBuf>,
    /// Directories to create before the run.
    pub make_directories: Vec<PathBuf>,
}

impl Command {
    fn new(program: &'static str, args: &[&str]) -> Command {
        assert!(!program.is_empty(), "a command names a program");
        Command {
            program,
            args: args.iter().map(|&a| a.to_owned()).collect(),
            directory: PathBuf::new(),
            env: Vec::new(),
            stdout_to: None,
            make_directories: Vec::new(),
        }
    }

    fn cargo(args: &[&str]) -> Command {
        Command::new("cargo", args)
    }

    fn in_directory(mut self, directory: &str) -> Command {
        self.directory = PathBuf::from(directory);
        self
    }

    fn with_env(mut self, name: &'static str, value: impl Into<String>) -> Command {
        self.env.push((name, value.into()));
        self
    }

    fn writing_to(mut self, file: &str) -> Command {
        self.stdout_to = Some(PathBuf::from(file));
        self
    }
}

fn step(name: &'static str, command: Command) -> Step {
    Step {
        name,
        needs: Vec::new(),
        action: Action::Run(command),
    }
}

fn check(name: &'static str, run: fn(&Context) -> Outcome) -> Step {
    Step {
        name,
        needs: Vec::new(),
        action: Action::Check(run),
    }
}

impl Step {
    fn needing(mut self, need: Need) -> Step {
        self.needs.push(need);
        self
    }
}

fn job(name: &'static str, steps: Vec<Step>) -> Job {
    assert!(!steps.is_empty(), "a job has a step");
    Job {
        name,
        steps,
        restore: Vec::new(),
    }
}

impl Job {
    fn restoring(mut self, paths: &[&str]) -> Job {
        self.restore = paths.iter().map(PathBuf::from).collect();
        self
    }
}

const SHELLCHECK: Need = Need::Tool {
    name: "shellcheck",
    hint: "Install it (`apt install shellcheck`) or run inside `mise x shellcheck@0.11.0 --`.",
};
const DASH: Need = Need::File {
    path: "/bin/dash",
    hint: "Install dash (`apt install dash`).",
};
const DOCKER: Need = Need::Tool {
    name: "docker",
    hint: "Install Docker and start its daemon.",
};
const XVFB: Need = Need::Tool {
    name: "xvfb-run",
    hint: "Install it (`apt install xvfb`).",
};
const PNPM: Need = Need::Tool {
    name: "pnpm",
    hint: "Install pnpm 11 on Node 22 (`mise use node@22 pnpm@11`).",
};
const NODE: Need = Need::Tool {
    name: "node",
    hint: "Install Node 22 (`mise use node@22`).",
};
const BASH: Need = Need::Tool {
    name: "bash",
    hint: "Install bash.",
};
const CLIPPY: Need = Need::Tool {
    name: "cargo-clippy",
    hint: "Run `rustup component add clippy`.",
};
const RUSTFMT: Need = Need::Tool {
    name: "cargo-fmt",
    hint: "Run `rustup component add rustfmt`.",
};
const GIT: Need = Need::Tool {
    name: "git",
    hint: "Install git.",
};

pub fn all(context: &Context) -> Vec<Job> {
    let jobs = vec![
        check_job(),
        job(
            "test",
            vec![step(
                "cargo test --workspace",
                Command::cargo(&["test", "--workspace"]),
            )],
        ),
        remote_cache(context),
        self_update(),
        install_script(),
        musl_binary(context),
        job(
            "fmt",
            vec![
                step(
                    "cargo fmt --check",
                    Command::cargo(&["fmt", "--all", "--", "--check"]),
                )
                .needing(RUSTFMT),
            ],
        ),
        job(
            "clippy",
            vec![
                step(
                    "cargo clippy",
                    Command::cargo(&["clippy", "--workspace", "--", "-D", "warnings"]),
                )
                .needing(CLIPPY),
            ],
        ),
        fuzz_targets(),
        wasm(),
        schema_drift(),
        grammar_drift(),
        vscode_client(),
        vscode_e2e(context),
    ];
    assert!(
        jobs.iter().all(|j| !j.steps.is_empty()),
        "every job has a step"
    );
    jobs
}

fn check_job() -> Job {
    job(
        "check",
        vec![
            step(
                "cargo check --workspace",
                Command::cargo(&["check", "--workspace"]),
            ),
            check(
                "workflows use only allowed actions",
                checks::workflows_use_allowed_actions,
            ),
            check(
                "fake AVM depends on no oxabl crate",
                checks::fake_avm_has_no_oxabl_dependency,
            ),
            check(
                "only remote-cache reaches the cache client",
                checks::only_the_feature_reaches_the_cache_client,
            ),
            check(
                "no build turns on unicode_lines",
                checks::no_build_turns_on_unicode_lines,
            ),
            check(
                "only the features reach the network stack",
                checks::only_the_features_reach_the_network_stack,
            ),
        ],
    )
}

fn remote_cache(context: &Context) -> Job {
    let bin =
        std::env::var("OXABL_CACHE_BIN_DIR").unwrap_or_else(|_| match std::env::var("HOME") {
            Ok(home) => format!("{home}/oxabl-cache-bin"),
            Err(_) => context
                .root
                .join("target/xtask-ci/oxabl-cache-bin")
                .display()
                .to_string(),
        });
    let test = Command::cargo(&[
        "test",
        "-p",
        "oxabl",
        "--features",
        "remote-cache",
        "--locked",
        "--test",
        "build_remote_cache",
        "--test",
        "build_remote_faults",
        "--test",
        "build_remote_trust",
        "--test",
        "build_remote_fetch_volume",
        "--test",
        "build_remote_fetch_tree",
    ])
    .with_env(
        "OXABL_CACHE_SERVER_BIN",
        format!("{bin}/bin/oxabl-cache-server"),
    )
    .with_env(
        "OXABL_CACHE_ADMIN_BIN",
        format!("{bin}/bin/oxabl-cache-admin"),
    )
    .with_env("OXABL_CACHE_BIN", format!("{bin}/bin/oxabl-cache"));
    job(
        "remote-cache",
        vec![
            // The script builds them only when the directory lacks the pinned revision.
            step(
                "build the cache server, admin tool and member command line",
                Command::new("./scripts/remote-cache-binaries.sh", &[&bin]),
            ),
            step("test the remote cache", test),
        ],
    )
}

fn self_update() -> Job {
    job(
        "self-update",
        vec![
            step(
                "test self-update",
                Command::cargo(&[
                    "test",
                    "-p",
                    "oxabl",
                    "--features",
                    "self-update",
                    "--locked",
                    "--test",
                    "self_update",
                ]),
            ),
            step(
                "clippy oxabl with both features",
                Command::cargo(&[
                    "clippy",
                    "-p",
                    "oxabl",
                    "--all-targets",
                    "--features",
                    "remote-cache,self-update",
                    "--locked",
                    "--",
                    "-D",
                    "warnings",
                ]),
            )
            .needing(CLIPPY),
        ],
    )
}

fn install_script() -> Job {
    let test = |shell: &str| {
        Command::cargo(&[
            "test",
            "-p",
            "oxabl",
            "--locked",
            "--test",
            "install_script",
        ])
        .with_env("OXABL_TEST_SH", shell)
    };
    job(
        "install-script",
        vec![
            step(
                "shellcheck",
                Command::new("shellcheck", &["scripts/install.sh"]),
            )
            .needing(SHELLCHECK),
            // dash is /bin/sh on Ubuntu, so this runs the script as a plain POSIX shell.
            step("run the script with dash", test("/bin/dash")).needing(DASH),
            step("run the script with bash", test("/bin/bash")).needing(BASH),
        ],
    )
}

fn musl_binary(context: &Context) -> Job {
    let cache = std::env::var("OXABL_MUSL_CACHE").unwrap_or_else(|_| {
        context
            .root
            .join("target/xtask-ci/musl-cache")
            .display()
            .to_string()
    });
    let dist = context
        .root
        .join("target/xtask-ci/dist")
        .display()
        .to_string();
    let mut build =
        Command::new("./scripts/build-musl.sh", &[&dist]).with_env("OXABL_MUSL_CACHE", cache);
    build.make_directories.push(PathBuf::from(&dist));
    let binary = format!("{dist}/oxabl-x86_64-unknown-linux-musl");
    job(
        "musl-binary",
        vec![
            // The release workflow publishes this binary. It must start in the usual OpenEdge
            // image (glibc 2.35) and must not need a shared library.
            step("build the static binary", build).needing(DOCKER),
            step(
                "start the binary in a plain image",
                Command::new("./scripts/check-musl.sh", &[&binary]),
            )
            .needing(DOCKER),
        ],
    )
}

fn fuzz_targets() -> Job {
    // The cargo-fuzz targets are outside the workspace; this keeps them compiling.
    job(
        "fuzz-targets",
        vec![
            step(
                "fmt",
                Command::cargo(&["fmt", "--manifest-path", "fuzz/Cargo.toml", "--", "--check"]),
            )
            .needing(RUSTFMT),
            step(
                "clippy",
                Command::cargo(&[
                    "clippy",
                    "--manifest-path",
                    "fuzz/Cargo.toml",
                    "--bins",
                    "--",
                    "-D",
                    "warnings",
                ]),
            )
            .needing(CLIPPY),
        ],
    )
    .restoring(&["fuzz/Cargo.lock"])
}

fn wasm() -> Job {
    job(
        "wasm",
        vec![
            step(
                "build oxabl_wasm",
                Command::cargo(&[
                    "build",
                    "-p",
                    "oxabl_wasm",
                    "--target",
                    "wasm32-unknown-unknown",
                    "--release",
                ]),
            )
            .needing(Need::RustTarget {
                name: "wasm32-unknown-unknown",
            }),
        ],
    )
}

fn diff(name: &'static str, path: &str) -> Step {
    step(
        name,
        Command::new("git", &["diff", "--exit-code", "--", path]),
    )
    .needing(GIT)
}

fn schema_drift() -> Job {
    let file = "clients/vscode/schemas/oxabl.schema.json";
    job(
        "schema-drift",
        vec![
            // The JSON Schema of oxabl.toml comes from the Rust config structs.
            step(
                "regenerate the schema",
                Command::cargo(&["run", "-p", "oxabl", "--", "schema"]).writing_to(file),
            ),
            diff("assert no drift", file),
        ],
    )
    .restoring(&[file])
}

fn grammar_drift() -> Job {
    let grammar = "clients/vscode/syntaxes/";
    let docs = "crates/oxabl_pipeline/src/keyword_docs.rs";
    let snippets = "crates/oxabl_pipeline/src/completion/snippets.rs";
    let generate = |what: &str| Command::cargo(&["run", "-p", "oxabl_codegen", "--", what]);
    job(
        "grammar-drift",
        vec![
            step("regenerate the grammar", generate("grammar")),
            diff("assert no grammar drift", grammar),
            step("regenerate the keyword docs", generate("keyword-docs")),
            diff("assert no keyword docs drift", docs),
            step("regenerate the completion snippets", generate("snippets")),
            diff("assert no completion snippets drift", snippets),
        ],
    )
    .restoring(&[grammar, docs, snippets])
}

fn pnpm(name: &'static str, args: &[&str]) -> Step {
    step(
        name,
        Command::new("pnpm", args).in_directory("clients/vscode"),
    )
    .needing(NODE)
    .needing(PNPM)
}

fn vscode_client() -> Job {
    job(
        "vscode-client",
        vec![
            pnpm("install", &["install", "--frozen-lockfile"]),
            pnpm("typecheck", &["run", "check"]),
            pnpm("test", &["test"]),
            pnpm("grammar tests", &["run", "test:grammar"]),
            // The release workflow publishes the VSIX this script builds.
            step(
                "package",
                Command::new("bash", &["scripts/package-vsix.sh", "dist"])
                    .in_directory("clients/vscode"),
            )
            .needing(BASH)
            .needing(NODE)
            .needing(PNPM),
        ],
    )
}

fn vscode_e2e(context: &Context) -> Job {
    let runtime = std::env::var("OXABL_E2E_RUNTIME")
        .unwrap_or_else(|_| std::env::temp_dir().join("oxrt").display().to_string());
    let binary = context
        .root
        .join("target/debug/oxabl")
        .display()
        .to_string();
    let mut run = Command::new("xvfb-run", &["-a", "pnpm", "run", "test:e2e"])
        .in_directory("clients/vscode")
        .with_env("OXABL_E2E_OXABL", binary)
        .with_env("OXABL_E2E_RUNTIME", runtime.clone());
    run.make_directories.push(PathBuf::from(runtime));
    job(
        "vscode-e2e",
        vec![
            // The outline scenario runs the real language server.
            step(
                "build oxabl",
                Command::cargo(&["build", "-p", "oxabl", "--locked"]),
            ),
            pnpm("install", &["install", "--frozen-lockfile"]),
            // The packaged VSIX in a real VS Code. The suite downloads the pinned VS Code.
            step("end-to-end suite", run)
                .needing(XVFB)
                .needing(NODE)
                .needing(PNPM),
        ],
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const ORDER: [&str; 14] = [
        "check",
        "test",
        "remote-cache",
        "self-update",
        "install-script",
        "musl-binary",
        "fmt",
        "clippy",
        "fuzz-targets",
        "wasm",
        "schema-drift",
        "grammar-drift",
        "vscode-client",
        "vscode-e2e",
    ];

    fn context() -> Context {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        Context {
            log_dir: root.join("target/xtask-ci/logs"),
            root,
            verbose: false,
        }
    }

    #[test]
    fn jobs_come_in_the_order_of_the_workflow() {
        let names: Vec<&str> = all(&context()).iter().map(|j| j.name).collect();
        assert_eq!(names, ORDER);
    }

    /// Each workflow job must run its xtask job, and no other, so that CI cannot drift from here.
    #[test]
    fn every_workflow_job_calls_its_xtask_job() {
        let workflow = std::fs::read_to_string(context().root.join(".github/workflows/ci.yml"))
            .expect("ci.yml exists");
        for name in ORDER {
            let header = format!("  {name}:");
            let calls: Vec<&str> = workflow
                .lines()
                .skip_while(|line| *line != header)
                .skip(1)
                .take_while(|line| !line.starts_with("  ") || line.starts_with("   "))
                .filter(|line| line.trim_start().starts_with("- run: cargo xtask ci"))
                .collect();
            assert_eq!(
                calls.len(),
                1,
                "job {name} must call cargo xtask ci once: {calls:?}"
            );
            assert!(
                calls[0]
                    .trim_end()
                    .ends_with(&format!("cargo xtask ci {name}")),
                "{name}"
            );
        }
        assert_eq!(workflow.matches("cargo xtask ci").count(), ORDER.len());
    }
}
