//! Reading a unit, its includes and the classes it uses.

use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Duration;

use sha2::{Digest, Sha256};

use super::outcome::{Outcome, Read};
use super::resolve::{self, Found};
use super::scan::{Fault, scan};
use super::{Compiler, output};
use crate::faults::Faults;

/// What one `COMPILE` request fixes for all its units.
pub struct Context<'a> {
    pub propath: &'a [String],
    pub stage: &'a Path,
    pub xref: &'a str,
    pub debug_list: bool,
    pub listing: bool,
    pub dlc: &'a Path,
    pub faults: &'a Faults,
}

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

fn class_of_unit(name: &str) -> Option<String> {
    name.strip_suffix(".cls")
        .map(|stem| stem.replace('/', ".").to_ascii_lowercase())
}

impl Compiler {
    pub(super) fn compile_unit(&mut self, ctx: &Context<'_>, name: &str) -> io::Result<Outcome> {
        let mut out = Outcome::new();
        let Some(found) = resolve::find(ctx.propath, name, ctx.dlc) else {
            out.fail(name, 0, 293, &format!("Cannot find or open file {name}"));
            return Ok(out);
        };
        let mut visited: HashSet<String> = class_of_unit(name).into_iter().collect();
        self.visit_file(ctx, name, &found, &mut out, &mut visited, true);
        if out.slow {
            std::thread::sleep(Duration::from_secs(2));
        }
        if out.ok && !out.no_output {
            output::write(ctx, name, &out)?;
        }
        Ok(out)
    }

    fn visit_file(
        &mut self,
        ctx: &Context<'_>,
        unit: &str,
        found: &Found,
        out: &mut Outcome,
        visited: &mut HashSet<String>,
        is_unit: bool,
    ) {
        let bytes = match std::fs::read(&found.real) {
            Ok(bytes) => bytes,
            Err(error) => {
                out.fail(&found.shown, 0, 293, &format!("Cannot read: {error}"));
                return;
            }
        };
        out.record(Read {
            shown: found.shown.clone(),
            sha256: sha256_hex(&bytes),
        });
        let text = String::from_utf8_lossy(&bytes).into_owned();
        if is_unit {
            out.source = text.clone();
        }
        let found_in_text = scan(&text);
        for (fault, row) in &found_in_text.faults {
            apply(*fault, *row, &found.shown, unit, ctx.faults, out);
        }
        for path in &found_in_text.reads {
            if let Ok(bytes) = std::fs::read(path) {
                out.record(Read {
                    shown: path.clone(),
                    sha256: sha256_hex(&bytes),
                });
            }
        }
        for include in &found_in_text.includes {
            let other = found_in_text
                .resolve_other
                .iter()
                .find(|(name, _)| name == include)
                .map(|(_, path)| resolve::Found {
                    shown: path.clone(),
                    real: PathBuf::from(path),
                })
                .filter(|file| file.real.is_file());
            match other.or_else(|| resolve::find(ctx.propath, include, ctx.dlc)) {
                Some(file) if out.has_read(&file.shown) => {}
                Some(file) => self.visit_file(ctx, unit, &file, out, visited, false),
                None => out.fail(
                    &found.shown,
                    0,
                    293,
                    &format!("Cannot find or open include file {include}"),
                ),
            }
        }
        for class in &found_in_text.classes {
            self.visit_class(ctx, unit, class, out, visited);
        }
    }

    fn visit_class(
        &mut self,
        ctx: &Context<'_>,
        unit: &str,
        class: &str,
        out: &mut Outcome,
        visited: &mut HashSet<String>,
    ) {
        let lower = class.to_ascii_lowercase();
        if !visited.insert(lower.clone()) {
            return;
        }
        let relative = class.replace('.', "/");
        if let Some(found) = resolve::in_output(ctx.propath, &format!("{relative}.r"), ctx.dlc)
            && let Ok(bytes) = std::fs::read(&found.real)
        {
            out.record(Read {
                shown: found.shown,
                sha256: sha256_hex(&bytes),
            });
            return;
        }
        if let Some(reads) = self.classes.get(&lower) {
            for read in reads.clone() {
                out.record(read);
            }
            return;
        }
        let Some(found) = resolve::find(ctx.propath, &format!("{relative}.cls"), ctx.dlc) else {
            return;
        };
        let mut side = Outcome::new();
        self.visit_file(ctx, unit, &found, &mut side, visited, false);
        let _ = output::write_stray(ctx, &relative);
        self.classes.insert(lower, side.reads.clone());
        out.merge(side);
    }
}

fn marker(faults: &Faults, kind: &str, unit: &str) -> Option<PathBuf> {
    let state = faults.state.as_ref()?;
    Some(state.join(format!("{kind}-{}", unit.replace('/', "_"))))
}

fn apply(fault: Fault, row: usize, file: &str, unit: &str, faults: &Faults, out: &mut Outcome) {
    match fault {
        Fault::Fail => out.fail(file, row, 1234, "FAKE-FAIL"),
        Fault::Warn => out.warn(file, row, "FAKE-WARN"),
        Fault::Crash => std::process::exit(1),
        Fault::FailOnce => {
            if let Some(marker) = marker(faults, "fail-once", unit)
                && !marker.exists()
            {
                let _ = std::fs::write(&marker, b"");
                out.fail(file, row, 1234, "FAKE-FAIL-ONCE");
            }
        }
        Fault::CrashOnce => match marker(faults, "crash-once", unit) {
            Some(marker) if !marker.exists() => {
                let _ = std::fs::write(&marker, b"");
                std::process::exit(1);
            }
            Some(_) => {}
            None => std::process::exit(1),
        },
        Fault::Hang => loop {
            std::thread::sleep(Duration::from_secs(3600));
        },
        Fault::Slow => out.slow = true,
        Fault::NoOutput => out.no_output = true,
        Fault::FailSilent => out.ok = false,
    }
}
