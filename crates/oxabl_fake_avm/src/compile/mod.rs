//! Compile requests: `PROPATH`, `OPTIONS`, `COMPILE`.

mod outcome;
mod output;
mod resolve;
mod scan;
mod visit;

use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::faults::Faults;
use crate::frame::{self, emit};
use crate::log;
use outcome::Read;
use visit::Context;

/// The state a worker keeps between requests.
#[derive(Default)]
pub struct Compiler {
    propaths: HashMap<String, Vec<String>>,
    /// Classes read from source, by lower-case class name, with the files each
    /// one read. A session keeps them for good, as `MULTI-COMPILE` does.
    classes: HashMap<String, Vec<Read>>,
    /// The units this worker has compiled.
    pub compiled: usize,
}

fn text(line: &frame::Line, index: usize) -> String {
    line.fields
        .get(index)
        .map(|field| String::from_utf8_lossy(field).into_owned())
        .unwrap_or_default()
}

impl Compiler {
    /// `PROPATH <id> <entry>...`
    pub fn define_propath(&mut self, out: &mut impl Write, line: &frame::Line) -> io::Result<()> {
        let id = text(line, 0);
        let entries = (1..line.fields.len()).map(|i| text(line, i)).collect();
        self.propaths.insert(id.clone(), entries);
        emit(out, "OK", &[id.as_bytes()])
    }

    /// `OPTIONS <id> <name=value>...`
    pub fn define_options(out: &mut impl Write, line: &frame::Line) -> io::Result<()> {
        emit(out, "OK", &[text(line, 0).as_bytes()])
    }

    /// `COMPILE ...` and its `UNIT` lines, answered with `MSG`, `DONE` and `END`.
    pub fn request(
        &mut self,
        input: &mut impl BufRead,
        out: &mut impl Write,
        line: &frame::Line,
        faults: &Faults,
        dlc: &Path,
    ) -> io::Result<()> {
        let req = text(line, 0);
        let propath = self
            .propaths
            .get(&text(line, 1))
            .cloned()
            .unwrap_or_default();
        let stage = PathBuf::from(text(line, 3));
        let xref = text(line, 4);
        let debug_list = text(line, 5) == "1";
        let listing = text(line, 6) == "1";
        let count: usize = text(line, 7).parse().unwrap_or(0);
        let mut units = Vec::with_capacity(count);
        let mut buffer = String::new();
        for _ in 0..count {
            buffer.clear();
            input.read_line(&mut buffer)?;
            if let Some(unit) = frame::parse(buffer.trim_end_matches('\n')) {
                units.push(text(&unit, 1));
            }
        }
        let context = Context {
            propath: &propath,
            stage: &stage,
            xref: &xref,
            debug_list,
            listing,
            dlc,
            faults,
        };
        for name in units {
            let started = Instant::now();
            log::append(faults.log.as_deref(), &name);
            let result = self.compile_unit(&context, &name)?;
            self.compiled += 1;
            for message in &result.messages {
                emit(
                    out,
                    "MSG",
                    &[
                        req.as_bytes(),
                        name.as_bytes(),
                        message.kind.as_bytes(),
                        message.number.to_string().as_bytes(),
                        message.file.as_bytes(),
                        message.row.to_string().as_bytes(),
                        message.column.to_string().as_bytes(),
                        message.text.as_bytes(),
                    ],
                )?;
            }
            let status: &[u8] = if result.ok { b"ok" } else { b"fail" };
            let ms = started.elapsed().as_millis().to_string();
            emit(
                out,
                "DONE",
                &[req.as_bytes(), name.as_bytes(), status, ms.as_bytes()],
            )?;
        }
        emit(out, "END", &[req.as_bytes()])
    }
}
