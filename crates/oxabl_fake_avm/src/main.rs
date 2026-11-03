//! A fake AVM for black-box tests of `oxabl build`.
//!
//! Usage: `oxabl_fake_avm <program> [--env <file>]`. `<program>` is the
//! worker program path that oxabl passes as `{program}`; the fake checks it.
//! The environment comes from the JSON file named by `--env`, else by
//! `FAKE_AVM_ENV`.
//!
//! A compile reads the unit, its includes and the classes it uses on the
//! request's PROPATH, and writes a text r-code file that lists what it read
//! with a hash of each, and a time. Text in a source asks for a fault:
//! `FAKE-FAIL` (an error at that line), `FAKE-FAIL-ONCE` (the first compile
//! only), `FAKE-WARN`, `FAKE-CRASH` (exit 1), `FAKE-CRASH-ONCE`, `FAKE-HANG`,
//! `FAKE-SLOW` (two seconds before the outputs), `FAKE-NO-OUTPUT` (success
//! with no output files) and `FAKE-READ <path>` (a file the analysis cannot
//! see). `FAKE_AVM_STATE` is a directory for the markers of the `-ONCE`
//! faults; `FAKE_AVM_LOG` receives one line for each compiled unit.

mod compile;
mod faults;
mod frame;
mod hashing;
mod log;
mod model;
mod schema;
mod session;

use std::io::{self, BufReader};
use std::path::{Path, PathBuf};

use model::Model;
use session::Session;

const HEADER: &str = "/* oxabl-worker protocol 1 */";

/// The program must exist, start with the header line and sit in a directory
/// named after the BLAKE3 hash of its bytes.
fn check_program(path: &Path) -> Result<(), String> {
    let bytes = std::fs::read(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let text = String::from_utf8_lossy(&bytes);
    if text.lines().next() != Some(HEADER) {
        return Err(format!("{} does not start with {HEADER}", path.display()));
    }
    let expected = blake3::hash(&bytes).to_hex().to_string();
    let directory = path
        .parent()
        .and_then(Path::file_name)
        .and_then(|name| name.to_str());
    if directory != Some(expected.as_str()) {
        return Err(format!(
            "{} is not in a directory named after its hash {expected}",
            path.display()
        ));
    }
    Ok(())
}

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(program) = args.next() else {
        eprintln!("usage: oxabl_fake_avm <program> [--env <file>]");
        std::process::exit(2);
    };
    let mut env_file = std::env::var_os("FAKE_AVM_ENV").map(PathBuf::from);
    while let Some(arg) = args.next() {
        match (arg.as_str(), args.next()) {
            ("--env", Some(file)) => env_file = Some(PathBuf::from(file)),
            _ => {
                eprintln!("unknown argument {arg}");
                std::process::exit(2);
            }
        }
    }
    if let Err(message) = check_program(Path::new(&program)) {
        eprintln!("{message}");
        std::process::exit(2);
    }
    let model = match Model::load(env_file.as_deref()) {
        Ok(model) => model,
        Err(message) => {
            eprintln!("{message}");
            std::process::exit(2);
        }
    };
    let faults = faults::Faults::from_env();
    log::append(faults.log.as_deref(), "start");
    if let Some(delay) = faults.start_delay {
        std::thread::sleep(delay);
    }
    let mut session = Session {
        model,
        faults,
        compiler: compile::Compiler::default(),
    };
    let stdin = io::stdin();
    let mut input = BufReader::new(stdin.lock());
    let mut out = io::stdout().lock();
    let result = session.run(&mut input, &mut out);
    log::append(session.faults.log.as_deref(), "exit");
    if let Err(error) = result {
        eprintln!("fake avm: {error}");
        std::process::exit(1);
    }
}
