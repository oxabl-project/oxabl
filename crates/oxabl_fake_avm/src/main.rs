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
//! only), `FAKE-FAIL-SILENT` (a failure with no message), `FAKE-WARN`,
//! `FAKE-CRASH` (exit 1), `FAKE-CRASH-ONCE`, `FAKE-HANG`, `FAKE-SLOW` (two
//! seconds before the outputs), `FAKE-NO-OUTPUT` (success with no output
//! files), `FAKE-READ <path>` (a file the analysis cannot see) and
//! `FAKE-RESOLVE-OTHER <name> <path>` (the include resolves to that path).
//! Like the real compiler, it opens an XREF or XREF-XML file whose relative
//! name starts with `.` with that dot removed. A request with the XREF mode
//! `private` writes an XREF-XML under `verify/`. An XREF-XML names each file
//! read as a `Source`, each include as an `INCLUDE` reference with the text
//! between its braces (its name and arguments), and each class as a `NEW`
//! reference.
//!
//! Environment variables: `FAKE_AVM_STATE` is a directory for the markers of
//! the `-ONCE` faults; `FAKE_AVM_LOG` receives one line for each compiled
//! unit; `FAKE_AVM_REQUEST_LOG` receives the time and type of each request,
//! and for `COMPILE` the presence of each path of `FAKE_AVM_WATCH`;
//! `FAKE_AVM_SCHEMA_CHANGE_AFTER=<n>` and `FAKE_AVM_SCHEMA_CHANGE_FILE=<path>`
//! change the schema image after `n` compiles, or while the file exists;
//! `FAKE_AVM_SCHEMA_DELAY_MS` delays each schema answer;
//! `FAKE_AVM_SCHEMA_ERROR_AFTER=<n>` gives a wrong count after `n` compiles;
//! `FAKE_AVM_NO_PRIVATE_XREF=1` writes no private `XREF-XML`.

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
