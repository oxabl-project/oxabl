//! `oxabl build --probe`: start the worker pool and print what the workers report.

use std::process::ExitCode;

use oxabl_build::config::BuildWorkspace;
use oxabl_build::error::BuildError;
use oxabl_build::key::{Environment, FileDigest};
use oxabl_build::worker::random;
use oxabl_build::worker::{Cancel, Pool, PoolError, PoolSpec};
use serde_json::json;

/// Exit code of a usage or configuration error.
const EXIT_CONFIG: u8 = 2;
/// Exit code when no worker could start.
const EXIT_INFRA: u8 = 3;
/// Exit code of a cancelled build.
const EXIT_CANCELLED: u8 = 130;

fn fail(code: u8, message: impl std::fmt::Display) -> ExitCode {
    eprintln!("error: {message}");
    ExitCode::from(code)
}

/// The first SIGINT or SIGTERM cancels the build; the second kills every worker at once.
fn install_signal_handler(cancel: &Cancel) {
    let cancel = cancel.clone();
    let result = ctrlc::set_handler(move || {
        if cancel.is_cancelled() {
            cancel.kill_all_now();
            std::process::exit(i32::from(EXIT_CANCELLED));
        }
        cancel.cancel();
    });
    if let Err(error) = result {
        eprintln!("warning: cannot install the signal handler: {error}");
    }
}

/// Starts the pool of `workspace`, prints the environment and stops the workers.
pub fn run(workspace: &BuildWorkspace, json: bool, workers: Option<usize>) -> ExitCode {
    let spec = match PoolSpec::from_config(workspace, workers, random::build_id()) {
        Ok(spec) => spec,
        Err(BuildError::Usage(message)) => return fail(EXIT_CONFIG, message),
        Err(error) => return fail(EXIT_INFRA, error),
    };
    let cancel = Cancel::new();
    install_signal_handler(&cancel);
    let started = match Pool::start(&spec, &cancel) {
        Ok(started) => started,
        Err(PoolError::Config(message)) => return fail(EXIT_CONFIG, message),
        Err(PoolError::Infra(message)) => return fail(EXIT_INFRA, message),
        Err(PoolError::Cancelled) => return ExitCode::from(EXIT_CANCELLED),
    };
    for warning in &started.warnings {
        eprintln!("warning: {warning}");
    }
    let workers = (started.pool.len(), spec.count);
    if json {
        println!("{}", json_report(&started.environment, workers));
    } else {
        print!("{}", text_report(&started.environment, workers));
    }
    started.pool.shutdown();
    ExitCode::SUCCESS
}

fn digest_word(digest: &FileDigest) -> String {
    digest.word().to_string()
}

fn json_report(environment: &Environment, workers: (usize, usize)) -> String {
    let fields: serde_json::Map<String, serde_json::Value> = environment
        .facts
        .fields()
        .filter(|(name, _)| *name != "startup_parameters")
        .map(|(name, value)| (name.to_string(), json!(value)))
        .collect();
    let databases: Vec<_> = environment
        .databases
        .iter()
        .map(|database| {
            json!({
                "number": database.info.number,
                "name": database.info.ldb,
                "type": database.info.dbtype,
                "aliases": database.info.aliases,
                "schema_identity": database.schema.identity().to_hex().to_string(),
                "entries": database.schema.entries.len(),
                "records": database.schema.records(),
                "fingerprint": {
                    "guid": database.fingerprint.guid,
                    "stamp": database.fingerprint.stamp,
                },
            })
        })
        .collect();
    let parameters: Vec<_> = environment
        .parameters
        .iter()
        .map(|p| json!({ "name": p.name, "value": p.value }))
        .collect();
    let libraries: Vec<_> = environment
        .libraries
        .iter()
        .map(|library| json!({ "entry": library.entry, "digest": digest_word(&library.digest) }))
        .collect();
    json!({
        "format": 1,
        "environment": {
            "source": "probe",
            "identity": environment.identity().to_hex().to_string(),
            "fields": fields,
            "parameters": parameters,
            "databases": databases,
            "libraries": libraries,
            "unknown": environment.unknown,
        },
        "workers": { "started": workers.0, "requested": workers.1 },
    })
    .to_string()
}

fn text_report(environment: &Environment, workers: (usize, usize)) -> String {
    let mut out = format!("environment {}\n", environment.identity().to_hex());
    for (name, value) in environment.facts.fields() {
        if name != "startup_parameters" {
            out.push_str(&format!("  {name}: {value}\n"));
        }
    }
    out.push_str("parameters\n");
    for parameter in &environment.parameters {
        let line = format!("  {} {}", parameter.name, parameter.value);
        out.push_str(line.trim_end());
        out.push('\n');
    }
    out.push_str("databases\n");
    for database in &environment.databases {
        out.push_str(&format!(
            "  {} ({}) schema {} ({} records)\n",
            database.info.ldb,
            database.info.dbtype,
            database.schema.identity().to_hex(),
            database.schema.records()
        ));
    }
    for library in &environment.libraries {
        out.push_str(&format!(
            "library {} {}\n",
            library.entry,
            library.digest.word()
        ));
    }
    for reason in &environment.unknown {
        out.push_str(&format!("unknown {reason}\n"));
    }
    out.push_str(&format!("workers: {} of {}\n", workers.0, workers.1));
    out
}
