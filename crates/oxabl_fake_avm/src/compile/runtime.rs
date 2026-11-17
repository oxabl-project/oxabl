//! The runtime and a compile outside the worker protocol, with the PROPATH
//! from the `PROPATH` variable, as a project starts the AVM.
//!
//! The runtime finds a program or a class as measured on OpenEdge 12.8: it
//! searches the PROPATH entry by entry, and in each entry takes `<stem>.r`
//! before the source; it does not compare times. A full path runs the `.r`
//! beside it when there is one, whatever the PROPATH holds.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::Compiler;
use super::outcome::Outcome;
use super::output;
use super::resolve::{self, Found};
use super::visit::Context;
use crate::faults::Faults;

const RCODE_HEADER: &str = "oxabl-fake-r 1";

/// What the runtime found for a name.
enum Located {
    Rcode(Found),
    Source(Found),
}

/// The entries of the `PROPATH` variable, split at `,` and `:`. An unset or
/// empty variable is an error: a test must give the runtime its PROPATH.
fn propath() -> Result<Vec<String>, String> {
    let value = std::env::var("PROPATH").map_err(|_| "PROPATH is not set".to_string())?;
    let entries: Vec<String> = value
        .split([',', ':'])
        .filter(|entry| !entry.is_empty())
        .map(String::from)
        .collect();
    match entries.is_empty() {
        true => Err("PROPATH is empty".to_string()),
        false => Ok(entries),
    }
}

fn dlc(faults: &Faults) -> PathBuf {
    faults
        .dlc
        .clone()
        .unwrap_or_else(|| PathBuf::from("/usr/dlc"))
}

fn at(path: &str) -> Option<Found> {
    Path::new(path).is_file().then(|| Found {
        shown: path.to_string(),
        real: PathBuf::from(path),
    })
}

/// The file that runs for `source` (a name with or without an extension, or
/// a full path), by the runtime's rule.
fn locate(propath: &[String], source: &str, dlc: &Path) -> Option<Located> {
    let rcode = format!("{}.r", output::stem(source));
    let has_source = !source.to_ascii_lowercase().ends_with(".r");
    if Path::new(source).is_absolute() {
        if let Some(found) = at(&rcode) {
            return Some(Located::Rcode(found));
        }
        return at(source).filter(|_| has_source).map(Located::Source);
    }
    propath.iter().find_map(|entry| {
        let entry = std::slice::from_ref(entry);
        if let Some(found) = resolve::find(entry, &rcode, dlc) {
            return Some(Located::Rcode(found));
        }
        resolve::find(entry, source, dlc)
            .filter(|_| has_source)
            .map(Located::Source)
    })
}

/// What one loaded program or class prints and which classes it uses.
struct Loaded {
    lines: Vec<String>,
    uses: Vec<String>,
}

fn read_rcode(found: &Found) -> Result<Loaded, String> {
    let text = std::fs::read_to_string(&found.real)
        .map_err(|error| format!("cannot read {}: {error}", found.shown))?;
    let mut lines = text.lines();
    if lines.next() != Some(RCODE_HEADER) {
        return Err(format!("** {} is not r-code. (2888)", found.shown));
    }
    let mut loaded = Loaded {
        lines: Vec::new(),
        uses: Vec::new(),
    };
    for line in lines {
        if let Some(text) = line.strip_prefix("say ") {
            loaded.lines.push(format!("SAYS {text}"));
        } else if let Some(class) = line.strip_prefix("uses ") {
            loaded.uses.push(class.to_string());
        } else if line.starts_with("read ") {
            loaded.lines.push(line.to_string());
        }
    }
    Ok(loaded)
}

/// A compile in memory, as the runtime does for a source it runs.
fn compile(
    propath: &[String],
    name: &str,
    found: &Found,
    faults: &Faults,
    dlc: &Path,
) -> Result<Outcome, String> {
    let context = Context {
        propath,
        stage: Path::new(""),
        xref: "",
        debug_list: false,
        listing: false,
        dlc,
        faults,
        save_strays: false,
    };
    let out = Compiler::default().compile_found(&context, name, found);
    match out.ok {
        true => Ok(out),
        false => Err(out
            .messages
            .iter()
            .map(|message| {
                format!(
                    "** {} line {}: {} ({})",
                    message.file, message.row, message.text, message.number
                )
            })
            .collect::<Vec<_>>()
            .join("\n")),
    }
}

/// Loads what `located` names and prints `<what> <name> <file> <how>`, then
/// what it prints and the files its r-code read.
fn load(
    what: &str,
    name: &str,
    compile_name: &str,
    located: Located,
    propath: &[String],
    faults: &Faults,
    dlc: &Path,
) -> Result<Vec<String>, String> {
    let (found, how, loaded) = match located {
        Located::Rcode(found) => {
            let loaded = read_rcode(&found)?;
            (found, "rcode", loaded)
        }
        Located::Source(found) => {
            let out = compile(propath, compile_name, &found, faults, dlc)?;
            let mut lines: Vec<String> = out.says.iter().map(|say| format!("SAYS {say}")).collect();
            lines.extend(
                out.reads
                    .iter()
                    .map(|read| format!("read {} {}", read.shown, read.sha256)),
            );
            let loaded = Loaded {
                lines,
                uses: out.uses,
            };
            (found, "compiled-on-the-fly", loaded)
        }
    };
    println!("{what} {name} {} {how}", found.shown);
    for line in &loaded.lines {
        println!("{line}");
    }
    Ok(loaded.uses)
}

/// Loads each class that `uses` names, and the classes those use, once each.
fn load_classes(
    mut uses: Vec<String>,
    propath: &[String],
    faults: &Faults,
    dlc: &Path,
) -> Result<(), String> {
    let mut loaded = HashSet::new();
    while let Some(class) = uses.pop() {
        if !loaded.insert(class.to_ascii_lowercase()) {
            continue;
        }
        let relative = class.replace('.', "/");
        let source = format!("{relative}.cls");
        let Some(located) = locate(propath, &source, dlc) else {
            return Err(format!("** Could not find class {class}. (14284)"));
        };
        let mut more = load("NEW", &class, &source, located, propath, faults, dlc)?;
        more.reverse();
        uses.extend(more);
    }
    Ok(())
}

/// `-p <program>` or `-new <class>`: runs it and the classes it uses.
pub fn run(start: &str, target: &str) -> Result<(), String> {
    let propath = propath()?;
    let faults = Faults::from_env();
    let dlc = dlc(&faults);
    let uses = match start {
        "-p" => {
            let Some(located) = locate(&propath, target, &dlc) else {
                return Err(format!("** \"{target}\" was not found. (293)"));
            };
            load("RUN", target, target, located, &propath, &faults, &dlc)?
        }
        _ => vec![target.to_string()],
    };
    let mut uses = uses;
    uses.reverse();
    load_classes(uses, &propath, &faults, &dlc)
}

/// `-c <name> [-save-into <dir>]`, as `COMPILE <name> SAVE [INTO <dir>]`: the
/// r-code goes to `<dir>/<stem of name>.r`, else beside the source.
pub fn compile_and_save(name: &str, into: Option<&str>) -> Result<(), String> {
    let propath = propath()?;
    let faults = Faults::from_env();
    let dlc = dlc(&faults);
    let found = match Path::new(name).is_absolute() {
        true => at(name),
        false => resolve::find(&propath, name, &dlc),
    };
    let Some(found) = found else {
        return Err(format!("** Cannot find or open file {name}. (293)"));
    };
    let out = compile(&propath, name, &found, &faults, &dlc)?;
    let target = match into {
        Some(dir) => {
            let relative = match Path::new(name).is_absolute() {
                true => Path::new(name)
                    .file_name()
                    .map(|file| file.to_string_lossy().into_owned())
                    .unwrap_or_default(),
                false => name.to_string(),
            };
            PathBuf::from(dir).join(format!("{}.r", output::stem(&relative)))
        }
        None => PathBuf::from(format!("{}.r", output::stem(&found.shown))),
    };
    output::put(&target, output::rcode(name, &out))
        .map_err(|error| format!("cannot write {}: {error}", target.display()))?;
    println!("COMPILE {name} OK");
    Ok(())
}
