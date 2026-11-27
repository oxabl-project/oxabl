//! The jobs of `.github/workflows/ci.yml`, in the order of that file. A job is a list of steps
//! that stops at its first failure. The runner setup (checkout, toolchain, caches, node) stays
//! in the workflow; everything else is here.

use crate::Context;
use crate::changed::{Tests, Umbrella};
use crate::checks::{self, Outcome};
use crate::tools::{Need, find_on_path};
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
const DASH: Need = Need::Tool {
    name: "dash",
    hint: "Install dash (`apt install dash`) or put it on PATH.",
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

/// The names of the jobs, in the order of `ci.yml`.
pub const JOB_NAMES: [&str; 14] = [
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

/// Every job. The `test` job runs what `tests` selects.
pub fn all(context: &Context, tests: &Tests) -> Vec<Job> {
    let jobs = vec![
        check_job(),
        test_job(tests),
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
                    Command::cargo(&[
                        "clippy",
                        "--workspace",
                        "--all-targets",
                        "--",
                        "-D",
                        "warnings",
                    ]),
                )
                .needing(CLIPPY),
                check("tigerstyle ratchet", crate::tigerstyle::check_ratchet).needing(CLIPPY),
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

/// The steps of the `test` job. With nothing selected the job has one step that says so: a
/// job never has no step.
fn test_job(tests: &Tests) -> Job {
    let steps = match tests {
        Tests::Full => vec![step(
            "cargo test --workspace",
            Command::cargo(&["test", "--workspace"]),
        )],
        Tests::Nothing => vec![step("no test is selected", Command::new("true", &[]))],
        Tests::Scoped { packages, umbrella } => {
            let mut steps = Vec::new();
            if !packages.is_empty() {
                let mut args = vec!["test".to_owned()];
                for package in packages {
                    args.push("-p".into());
                    args.push(package.clone());
                }
                steps.push(step("cargo test (changed crates)", cargo_owned(args)));
            }
            match umbrella {
                Umbrella::None => {}
                Umbrella::All => steps.push(step(
                    "cargo test -p oxabl",
                    Command::cargo(&["test", "-p", "oxabl"]),
                )),
                Umbrella::Targets(targets) => {
                    let mut args: Vec<String> = ["test", "-p", "oxabl"].map(str::to_owned).into();
                    for target in targets {
                        args.push("--test".into());
                        args.push(target.clone());
                    }
                    steps.push(step(
                        "cargo test -p oxabl (selected tests)",
                        cargo_owned(args),
                    ));
                }
            }
            assert!(!steps.is_empty(), "a scoped selection has a test");
            steps
        }
    };
    job("test", steps)
}

fn cargo_owned(args: Vec<String>) -> Command {
    assert!(!args.is_empty(), "cargo needs a subcommand");
    let mut command = Command::cargo(&[]);
    command.args = args;
    command
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

/// The path of a shell on `PATH`; a missing shell fails its step through its `Need`.
fn shell_path(name: &str) -> String {
    find_on_path(name).map_or_else(|| format!("/bin/{name}"), |path| path.display().to_string())
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
            step("run the script with dash", test(&shell_path("dash"))).needing(DASH),
            step("run the script with bash", test(&shell_path("bash"))).needing(BASH),
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
        let names: Vec<&str> = all(&context(), &Tests::Full)
            .iter()
            .map(|j| j.name)
            .collect();
        assert_eq!(names, JOB_NAMES);
    }

    fn workflow(name: &str) -> String {
        std::fs::read_to_string(context().root.join(format!(".github/workflows/{name}")))
            .expect("the workflow exists")
    }

    /// The lines of one job of a workflow: from its header to the next job.
    fn job_block<'a>(workflow: &'a str, name: &str) -> Vec<&'a str> {
        let header = format!("  {name}:");
        let block: Vec<&str> = workflow
            .lines()
            .skip_while(|line| *line != header)
            .skip(1)
            .take_while(|line| !line.starts_with("  ") || line.starts_with("   "))
            .collect();
        assert!(!block.is_empty(), "job {name} is in the workflow");
        block
    }

    /// A line that runs xtask: a comment does not.
    fn is_call(line: &str) -> bool {
        line.contains("cargo xtask ci") && !line.trim_start().starts_with('#')
    }

    fn xtask_calls<'a>(block: &[&'a str]) -> Vec<&'a str> {
        block.iter().copied().filter(|line| is_call(line)).collect()
    }

    /// Each workflow job must run its xtask job, and no other, so that CI cannot drift from here.
    /// The `test` job has two calls: the scoped one of a pull request and the full one.
    #[test]
    fn every_workflow_job_calls_its_xtask_job() {
        let workflow = workflow("ci.yml");
        let mut counted = 0;
        for name in JOB_NAMES {
            let calls = xtask_calls(&job_block(&workflow, name));
            let expected = if name == "test" { 2 } else { 1 };
            assert_eq!(calls.len(), expected, "job {name}: {calls:?}");
            for call in &calls {
                assert!(
                    call.trim_end().ends_with(&format!("cargo xtask ci {name}"))
                        || call
                            .trim_end()
                            .ends_with(&format!("\"origin/$BASE\" {name}")),
                    "{name}: {call}"
                );
            }
            counted += calls.len();
        }
        // The `changes` job only lists jobs.
        let selection = xtask_calls(&job_block(&workflow, "changes"));
        assert!(!selection.is_empty());
        assert!(
            selection.iter().all(|call| call.contains("--list")),
            "{selection:?}"
        );
        counted += selection.len();
        assert_eq!(workflow.lines().filter(|l| is_call(l)).count(), counted);
    }

    /// A pull request runs a job only when `changes` selected it, and no job runs on `closed`.
    #[test]
    fn every_job_runs_only_when_selected_and_never_on_close() {
        let workflow = workflow("ci.yml");
        for name in JOB_NAMES {
            let block = job_block(&workflow, name).join("\n");
            assert!(block.contains("needs: changes"), "{name} needs changes");
            assert!(
                block.contains("github.event.action != 'closed'"),
                "{name} skips a closed event"
            );
            assert!(
                block.contains(&format!(
                    "github.event_name != 'pull_request' || contains(fromJSON(needs.changes.outputs.jobs), '{name}')"
                )),
                "{name} runs on a pull request only when selected, and always elsewhere"
            );
        }
        assert!(
            workflow.contains("\n  merge_group:\n"),
            "the merge queue runs CI"
        );
    }

    /// The required check `CI` waits for every job, and runs even when one fails or is skipped.
    #[test]
    fn the_ci_job_waits_for_every_job() {
        let workflow = workflow("ci.yml");
        let block = job_block(&workflow, "ci").join("\n");
        assert!(block.contains("name: CI\n"), "the check is named CI");
        assert!(block.contains("if: always()"), "CI runs after a failure");
        let needs = block
            .lines()
            .find(|line| line.trim_start().starts_with("needs:"))
            .expect("CI has needs");
        for name in JOB_NAMES.iter().copied().chain(["changes"]) {
            assert!(
                needs.contains(&format!("{name},")) || needs.contains(&format!("{name}]")),
                "CI does not wait for {name}"
            );
        }
        let jobs_section = workflow.split("\njobs:\n").nth(1).expect("a jobs section");
        let ids = jobs_section
            .lines()
            .filter(|line| line.starts_with("  ") && !line.starts_with("   "))
            .filter_map(|line| line.trim().strip_suffix(':'))
            .filter(|id| !id.starts_with('#'))
            .count();
        // The jobs, `changes` and `ci`.
        assert_eq!(ids, JOB_NAMES.len() + 2);
    }

    /// Only a newer push to a pull request may cancel a CI run; master and merge groups keep theirs.
    #[test]
    fn only_a_pull_request_run_is_cancelled() {
        let workflow = workflow("ci.yml");
        assert!(
            workflow.contains("cancel-in-progress: ${{ github.event_name == 'pull_request' }}"),
            "a master or merge group run is never cancelled"
        );
        assert!(
            workflow.contains(
                "group: ci-${{ github.workflow }}-${{ github.event.pull_request.number && format('pr-{0}', github.event.pull_request.number) || github.ref }}"
            ),
            "a pull request's group is its number: a merged pull request's `closed` run has the base branch as its ref"
        );
    }

    #[test]
    fn codspeed_runs_on_master_and_on_request_only() {
        let workflow = workflow("codspeed.yml");
        assert!(
            !workflow.contains("pull_request"),
            "no benchmark on a pull request"
        );
        assert!(workflow.contains("workflow_dispatch"));
        assert!(
            workflow.contains("cancel-in-progress: false"),
            "a master run is never cancelled"
        );
    }
}
