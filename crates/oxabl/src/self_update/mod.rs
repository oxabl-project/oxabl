//! `oxabl self-update` and the automatic check of a managed binary.
//!
//! Without the `self-update` feature the command only says how to update, and
//! no network or crypto code is linked.

#[cfg(feature = "self-update")]
mod github;
#[cfg(feature = "self-update")]
mod install;
#[cfg(feature = "self-update")]
mod paths;
#[cfg(feature = "self-update")]
mod record;
#[cfg(feature = "self-update")]
mod schedule;

/// What `oxabl self-update` was asked. A binary without the feature ignores it.
#[cfg_attr(not(feature = "self-update"), allow(dead_code))]
pub struct Options {
    pub check: bool,
    pub json: bool,
    pub background: bool,
}

#[cfg(not(feature = "self-update"))]
pub fn run(_: &Options) -> std::process::ExitCode {
    eprintln!(
        "error: this oxabl was built without self-update; update it with `cargo install oxabl`"
    );
    std::process::ExitCode::from(2)
}

#[cfg(not(feature = "self-update"))]
pub fn schedule_if_due() {}

#[cfg(feature = "self-update")]
pub use real::{run, schedule_if_due};

#[cfg(feature = "self-update")]
mod real {
    use std::fs::OpenOptions;
    use std::path::PathBuf;
    use std::process::{Command, ExitCode, Stdio};

    use oxabl_build::lock::{LockOutcome, try_acquire_file};
    use serde::Serialize;

    use super::github::{Api, Version};
    use super::{Options, install, paths, record, schedule};

    const LOCK: &str = "update.lock";
    const LOG: &str = "update.log";
    const LOG_LIMIT: u64 = 256 * 1024;

    fn installed() -> Version {
        Version::parse(env!("CARGO_PKG_VERSION")).expect("the crate version is X.Y.Z")
    }

    fn binary() -> Result<PathBuf, String> {
        std::env::current_exe()
            .and_then(|path| path.canonicalize())
            .map_err(|e| format!("cannot find the running binary: {e}"))
    }

    /// Off in CI, on request, and for a binary the installer does not manage.
    fn disabled() -> bool {
        std::env::var_os("CI").is_some() || std::env::var_os("OXABL_NO_SELF_UPDATE").is_some()
    }

    /// Starts a detached `oxabl self-update --background` when a check is due.
    /// It never waits on the network.
    pub fn schedule_if_due() {
        if disabled() {
            return;
        }
        let (Some(state_dir), Ok(binary)) = (paths::state_dir(), binary()) else {
            return;
        };
        if !record::managed(record::read().as_ref(), &binary)
            || !schedule::read(&state_dir).due(schedule::now())
        {
            return;
        }
        // A check that runs already makes another one needless.
        match try_acquire_file(&state_dir, LOCK) {
            LockOutcome::Held(_) => {}
            _ => return,
        }
        let log = state_dir.join(LOG);
        if std::fs::metadata(&log).is_ok_and(|meta| meta.len() > LOG_LIMIT) {
            let _ = std::fs::remove_file(&log);
        }
        let Ok(out) = OpenOptions::new().create(true).append(true).open(&log) else {
            return;
        };
        let Ok(err) = out.try_clone() else { return };
        let spawned = Command::new(&binary)
            .args(["self-update", "--background"])
            .stdin(Stdio::null())
            .stdout(out)
            .stderr(err)
            .spawn();
        if let Ok(mut child) = spawned {
            // Reaps the worker of a long-lived process such as the server.
            std::thread::spawn(move || {
                let _ = child.wait();
            });
        }
    }

    #[derive(Serialize)]
    struct Verdict {
        installed: String,
        latest: String,
        update: bool,
        managed: bool,
    }

    pub fn run(options: &Options) -> ExitCode {
        if options.background {
            // Own session, so a closed terminal does not stop the check.
            let _ = rustix::process::setsid();
        }
        match go(options) {
            Ok(code) => code,
            Err(Failure::Unmanaged(message)) => {
                eprintln!("error: {message}");
                ExitCode::from(2)
            }
            Err(Failure::Unestablished(message)) => {
                eprintln!("error: {message}");
                ExitCode::from(3)
            }
        }
    }

    enum Failure {
        Unmanaged(String),
        Unestablished(String),
    }

    use Failure::Unestablished;

    fn go(options: &Options) -> Result<ExitCode, Failure> {
        let binary = binary().map_err(Unestablished)?;
        let record = record::read();
        let managed = record::managed(record.as_ref(), &binary);
        if disabled_background(options) {
            return Ok(ExitCode::SUCCESS);
        }
        if !managed && !options.check {
            let file = record::file()
                .map_or_else(|| "the install record".into(), |f| f.display().to_string());
            return Err(Failure::Unmanaged(format!(
                "this oxabl ({}) is not managed by an installer: {file} does not name it; \
                 update it the way you installed it",
                binary.display()
            )));
        }
        let state_dir = paths::state_dir();
        // One check at a time. A busy lock is not an answer.
        let _lock = match state_dir.as_deref().map(|dir| try_acquire_file(dir, LOCK)) {
            Some(LockOutcome::Held(lock)) => Some(lock),
            Some(LockOutcome::Busy(_)) => {
                if options.background {
                    return Ok(ExitCode::SUCCESS);
                }
                return Err(Unestablished("another update check is running".into()));
            }
            _ => None,
        };
        if options.background
            && let Some(dir) = &state_dir
            && !schedule::read(dir).due(schedule::now())
        {
            return Ok(ExitCode::SUCCESS);
        }
        let outcome = establish(options, &binary, managed);
        if !options.check
            && let Some(dir) = &state_dir
        {
            let previous = schedule::read(dir);
            let now = schedule::now();
            schedule::write(
                dir,
                if outcome.is_ok() {
                    schedule::State::succeeded(now)
                } else {
                    previous.failed(now)
                },
            );
        }
        outcome.map_err(Unestablished)
    }

    /// A background run for a binary that is off or not managed does nothing.
    fn disabled_background(options: &Options) -> bool {
        options.background && (disabled() || !paths_known())
    }

    fn paths_known() -> bool {
        paths::state_dir().is_some() && paths::data_dir().is_some()
    }

    fn establish(
        options: &Options,
        binary: &std::path::Path,
        managed: bool,
    ) -> Result<ExitCode, String> {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| format!("cannot start the runtime: {e}"))?;
        runtime.block_on(async {
            let api = Api::new()?;
            let release = api.latest().await?;
            let current = installed();
            let update = release.version > current;
            if options.check {
                let verdict = Verdict {
                    installed: current.to_string(),
                    latest: release.version.to_string(),
                    update,
                    managed,
                };
                if options.json {
                    println!(
                        "{}",
                        serde_json::to_string(&verdict).expect("a verdict is JSON")
                    );
                } else if update {
                    println!(
                        "oxabl {} is available (installed {current})",
                        release.version
                    );
                } else {
                    println!("oxabl is up to date ({current})");
                }
                return Ok(ExitCode::SUCCESS);
            }
            if !update {
                println!("oxabl is up to date ({current})");
                return Ok(ExitCode::SUCCESS);
            }
            install::install(&api, &release, binary).await?;
            let written = record::write(&record::Record {
                format: 1,
                installer: "self-update".into(),
                path: binary.display().to_string(),
                version: release.version.to_string(),
            });
            if let Err(error) = written {
                eprintln!("warning: {error}");
            }
            println!("oxabl was updated from {current} to {}", release.version);
            Ok(ExitCode::SUCCESS)
        })
    }
}
