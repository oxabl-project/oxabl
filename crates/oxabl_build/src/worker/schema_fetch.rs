//! Reading the schema image of one database: one hashed entry per table.

use std::time::Instant;

use super::error::WorkerError;
use super::frame::{Reply, Request};
use super::session::Session;
use crate::key::{SchemaEntry, SchemaPart};

fn protocol(message: String) -> WorkerError {
    WorkerError::Protocol(message)
}

fn is_sha256(text: &str) -> bool {
    text.len() == 64
        && text
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

/// Sends `SCHEMA <ldb> <part> <parts>` and collects the entries of that share. An entry twice, a bad
/// hash, an entry of another database or a total that differs from
/// `SCHEMA-END` is a protocol error.
pub fn fetch(
    session: &mut Session,
    ldb: &str,
    part: usize,
    parts: usize,
    deadline: Instant,
) -> Result<SchemaPart, WorkerError> {
    session.send(&Request::Schema { ldb, part, parts })?;
    let codec = session.codec;
    let mut entries: Vec<SchemaEntry> = Vec::new();
    loop {
        match session.recv(deadline)? {
            Reply::Sent {
                ldb: got,
                kind,
                name,
                records,
                sha256,
            } => {
                if codec.decode(&got).map_err(protocol)? != ldb {
                    return Err(protocol(format!("SENT for another database than {ldb}")));
                }
                let name = codec.decode(&name).map_err(protocol)?;
                let named = match kind.as_str() {
                    "db" | "system" => name.is_empty(),
                    "table" | "sequence" => !name.is_empty(),
                    _ => false,
                };
                if !named {
                    return Err(protocol(format!("bad entry `{kind} {name}`")));
                }
                if !is_sha256(&sha256) {
                    return Err(protocol(format!("bad hash `{sha256}` for `{kind} {name}`")));
                }
                let records: u64 = records
                    .parse()
                    .map_err(|_| protocol(format!("bad record count `{records}`")))?;
                if entries.iter().any(|e| e.kind == kind && e.name == name) {
                    return Err(protocol(format!("entry `{kind} {name}` appears twice")));
                }
                entries.push(SchemaEntry {
                    kind,
                    name,
                    records,
                    sha256,
                });
            }
            Reply::SchemaEnd {
                ldb: got,
                entries: total,
                tables,
            } => {
                if codec.decode(&got).map_err(protocol)? != ldb {
                    return Err(protocol(format!(
                        "SCHEMA-END for another database than {ldb}"
                    )));
                }
                if total != entries.len().to_string() {
                    return Err(protocol(format!(
                        "SCHEMA-END says {total} entries; oxabl read {}",
                        entries.len()
                    )));
                }
                let tables: usize = tables
                    .parse()
                    .map_err(|_| protocol(format!("bad table count `{tables}`")))?;
                return Ok(SchemaPart {
                    part,
                    entries,
                    tables,
                });
            }
            other => return Err(protocol(format!("unexpected {other:?} in a schema image"))),
        }
    }
}
