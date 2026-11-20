//! The request loop of one fake worker.

use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use crate::compile::Compiler;
use crate::faults::Faults;
use crate::frame::{self, Line, emit};
use crate::hashing::{self, Hashed};
use crate::log;
use crate::model::Model;
use crate::schema;

pub struct Session {
    pub model: Model,
    pub faults: Faults,
    pub compiler: Compiler,
    /// The `RAWDIGEST` requests this process answered.
    pub raw_requests: usize,
}

const ENV_FIELDS: &[&str] = &[
    "program",
    "dlc",
    "dlc_version",
    "proversion",
    "opsys",
    "window_system",
    "batch_mode",
    "process_architecture",
    "cpinternal",
    "cpstream",
    "cpcoll",
    "cpcase",
    "date_format",
    "year_offset",
    "numeric_decimal_point",
    "numeric_separator",
    "startup_parameters",
    "propath",
    "cwd",
];

impl Session {
    fn database(&self, ldb: &str) -> Option<&crate::model::Database> {
        self.model
            .databases
            .iter()
            .find(|database| database.ldb.eq_ignore_ascii_case(ldb))
    }

    fn dlc(&self) -> PathBuf {
        self.model
            .dlc_dir
            .as_ref()
            .map(PathBuf::from)
            .or_else(|| self.faults.dlc.clone())
            .unwrap_or_else(|| PathBuf::from(self.model.field("dlc")))
    }

    fn field(&self, name: &str) -> String {
        match name {
            "startup_parameters" => self.model.startup_parameters(),
            "cwd" if !self.model.env.contains_key("cwd") => std::env::current_dir()
                .map(|dir| dir.to_string_lossy().into_owned())
                .unwrap_or_default(),
            _ => self.model.field(name),
        }
    }

    /// One line of the request log: the request type and its first field,
    /// and for `COMPILE` the presence of each watched path.
    fn log_request(&self, line: &Line) {
        let mut what = line.kind.clone();
        if line.kind == "COMPILE" {
            for path in &self.faults.watch {
                what.push_str(&format!(" watch:{}={}", path.display(), path.exists()));
            }
        }
        log::request(self.faults.request_log.as_deref(), &what);
    }

    fn noise(&self, out: &mut impl Write) -> io::Result<()> {
        if self.faults.noise {
            writeln!(out, "** noise: session starting")?;
        }
        Ok(())
    }

    fn handshake(&self, out: &mut impl Write, request: &Line) -> io::Result<()> {
        let token = request.fields.get(1).cloned().unwrap_or_default();
        let echoed = if self.faults.bad_token {
            b"0".repeat(32)
        } else {
            token
        };
        self.noise(out)?;
        emit(out, "HELLO", &[b"1", &echoed])?;
        let mut count = 0;
        for name in ENV_FIELDS {
            if self.faults.noise {
                writeln!(out, "** noise before {name}")?;
            }
            emit(out, "ENV", &[name.as_bytes(), self.field(name).as_bytes()])?;
            count += 1;
        }
        for (index, database) in self.model.databases.iter().enumerate() {
            emit(
                out,
                "DB",
                &[
                    (index + 1).to_string().as_bytes(),
                    database.ldb.as_bytes(),
                    database.dbtype.as_bytes(),
                    database.aliases.join(",").as_bytes(),
                ],
            )?;
            count += 1;
        }
        emit(out, "ENV-END", &[count.to_string().as_bytes()])
    }

    fn text(line: &Line, index: usize) -> String {
        line.fields
            .get(index)
            .map(|field| String::from_utf8_lossy(field).into_owned())
            .unwrap_or_default()
    }

    fn hashed(out: &mut impl Write, id: &str, found: Hashed) -> io::Result<()> {
        let id = id.as_bytes();
        match found {
            Hashed::File { size, sha256 } => emit(
                out,
                "HASHED",
                &[id, b"file", size.to_string().as_bytes(), sha256.as_bytes()],
            ),
            Hashed::Dir => emit(out, "HASHED", &[id, b"dir", b"-", b"-"]),
            Hashed::None => emit(out, "HASHED", &[id, b"none", b"-", b"-"]),
        }
    }

    /// Runs until `QUIT`, end of input, or an injected exit.
    pub fn run(&mut self, input: &mut impl BufRead, out: &mut impl Write) -> io::Result<()> {
        let mut text = String::new();
        let mut handshook = false;
        loop {
            text.clear();
            if input.read_line(&mut text)? == 0 {
                return Ok(());
            }
            let Some(line) = frame::parse(text.trim_end_matches('\n')) else {
                continue;
            };
            self.log_request(&line);
            if let Some(delay) = self.faults.request_delay {
                std::thread::sleep(delay);
            }
            match line.kind.as_str() {
                "HELLO" => {
                    if self.faults.exit_in_handshake {
                        std::process::exit(1);
                    }
                    if let Some(marker) = &self.faults.exit_in_handshake_once
                        && !marker.exists()
                    {
                        let _ = std::fs::write(marker, b"");
                        std::process::exit(1);
                    }
                    self.handshake(out, &line)?;
                    handshook = true;
                }
                "FINGERPRINT" if handshook => {
                    let ldb = Self::text(&line, 0);
                    match self.database(&ldb) {
                        Some(database) => {
                            let guid = database
                                .guid
                                .clone()
                                .unwrap_or(format!("guid-{}", database.ldb));
                            let stamp = database.stamp.clone().unwrap_or("stamp-1".to_string());
                            emit(
                                out,
                                "FPRINT",
                                &[ldb.as_bytes(), guid.as_bytes(), stamp.as_bytes()],
                            )?;
                        }
                        None => emit(out, "ERROR", &[format!("no database {ldb}").as_bytes()])?,
                    }
                }
                "SCHEMA" if handshook => {
                    let ldb = Self::text(&line, 0);
                    match self.database(&ldb) {
                        Some(database) => {
                            let part: usize = Self::text(&line, 1).parse().unwrap_or(0);
                            let parts: usize = Self::text(&line, 2).parse().unwrap_or(1).max(1);
                            log::append(
                                self.faults.log.as_deref(),
                                &format!("schema {ldb} {part}/{parts}"),
                            );
                            let changed = self
                                .faults
                                .schema_change_after
                                .is_some_and(|after| self.compiler.compiled >= after)
                                || self
                                    .faults
                                    .schema_change_file
                                    .as_ref()
                                    .is_some_and(|file| file.exists());
                            let broken = self
                                .faults
                                .schema_error_after
                                .is_some_and(|after| self.compiler.compiled >= after);
                            schema::emit_image(
                                out,
                                database,
                                part,
                                parts,
                                &self.faults,
                                changed,
                                broken,
                            )?
                        }
                        None => emit(out, "ERROR", &[format!("no database {ldb}").as_bytes()])?,
                    }
                }
                "RAWDIGEST" if handshook => {
                    let ldb = Self::text(&line, 0);
                    self.raw_requests += 1;
                    let number = |at: usize, default: usize| -> usize {
                        Self::text(&line, at).parse().unwrap_or(default)
                    };
                    let (part, parts, buckets) = (number(1, 0), number(2, 1).max(1), number(3, 1));
                    match self.database(&ldb) {
                        Some(database) => {
                            log::append(
                                self.faults.log.as_deref(),
                                &format!("raw {ldb} {part}/{parts}"),
                            );
                            let changed = self
                                .faults
                                .schema_change_after
                                .is_some_and(|after| self.compiler.compiled >= after)
                                || self
                                    .faults
                                    .schema_change_file
                                    .as_ref()
                                    .is_some_and(|file| file.exists());
                            let late = self
                                .faults
                                .raw_change_from
                                .is_some_and(|from| self.raw_requests >= from)
                                || self
                                    .faults
                                    .raw_change_file
                                    .as_ref()
                                    .is_some_and(|file| file.exists());
                            let salt = format!(
                                "{}{}",
                                self.faults.raw_salt.as_deref().unwrap_or_default(),
                                if late { "late" } else { "" }
                            );
                            let broken = self
                                .faults
                                .schema_error_after
                                .is_some_and(|after| self.compiler.compiled >= after);
                            let quit = schema::emit_raw(
                                out,
                                database,
                                (part, parts, buckets),
                                &self.faults,
                                schema::RawState {
                                    changed,
                                    salt: &salt,
                                    broken,
                                },
                            )?;
                            if quit {
                                return Ok(());
                            }
                        }
                        None => emit(out, "ERROR", &[format!("no database {ldb}").as_bytes()])?,
                    }
                }
                "HASH" if handshook => {
                    let id = Self::text(&line, 0);
                    let path = Self::text(&line, 1);
                    Self::hashed(out, &id, hashing::hash(&path, &self.dlc()))?;
                }
                "HASHSET" if handshook => {
                    let id = Self::text(&line, 0);
                    let count: usize = Self::text(&line, 1).parse().unwrap_or(0);
                    let mut paths = Vec::with_capacity(count);
                    for _ in 0..count {
                        text.clear();
                        input.read_line(&mut text)?;
                        if let Some(path) = frame::parse(text.trim_end_matches('\n')) {
                            paths.push(Self::text(&path, 1));
                        }
                    }
                    let digest = hashing::hash_set(&paths, &self.dlc());
                    emit(out, "HASHSET-DONE", &[id.as_bytes(), digest.as_bytes()])?;
                }
                "PROPATH" if handshook => self.compiler.define_propath(out, &line)?,
                "OPTIONS" if handshook => Compiler::define_options(out, &line)?,
                "COMPILE" if handshook => {
                    let dlc = self.dlc();
                    self.compiler
                        .request(input, out, &line, &self.faults, &dlc)?
                }
                "PING" if handshook => {
                    emit(out, "PONG", &[Self::text(&line, 0).as_bytes()])?;
                }
                "QUIT" => {
                    emit(out, "BYE", &[])?;
                    return Ok(());
                }
                other => {
                    emit(
                        out,
                        "ERROR",
                        &[format!("unsupported request {other}").as_bytes()],
                    )?;
                    return Ok(());
                }
            }
        }
    }
}
