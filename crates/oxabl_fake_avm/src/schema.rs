//! The hashed schema image of one database: one entry per table.

use std::io::{self, Write};

use sha2::{Digest, Sha256};

use crate::faults::Faults;
use crate::frame::{emit, encode};
use crate::model::Database;

/// One entry: the canonical text of its records.
struct Entry {
    kind: &'static str,
    name: String,
    records: u64,
    text: String,
}

/// The entry that a record belongs to: `db`, `system` for a system table,
/// the table of a table, field, index, index field or trigger, or the
/// sequence.
fn entry_of(record: &[String]) -> (&'static str, String) {
    let second = record.get(1).cloned().unwrap_or_default();
    match record.first().map(String::as_str) {
        Some("db") => ("db", String::new()),
        Some("system-table") => ("system", String::new()),
        Some("sequence") => ("sequence", second),
        _ => ("table", second),
    }
}

/// Fields are percent-encoded and joined with TAB; each record ends with LF.
fn canonical_line(record: &[String]) -> String {
    let fields: Vec<String> = record
        .iter()
        .map(|field| encode(field.as_bytes()))
        .collect();
    format!("{}\n", fields.join("\t"))
}

/// Every database has system tables, so the `system` entry is always there,
/// with no records when the test gives none.
fn entries(database: &Database) -> Vec<Entry> {
    let mut entries: Vec<Entry> = vec![Entry {
        kind: "system",
        name: String::new(),
        records: 0,
        text: String::new(),
    }];
    for record in &database.schema {
        let (kind, name) = entry_of(record);
        let at = match entries
            .iter()
            .position(|e| e.kind == kind && e.name == name)
        {
            Some(at) => at,
            None => {
                entries.push(Entry {
                    kind,
                    name,
                    records: 0,
                    text: String::new(),
                });
                entries.len() - 1
            }
        };
        entries[at].records += 1;
        entries[at].text.push_str(&canonical_line(record));
    }
    entries
}

fn sha256_hex(text: &str) -> String {
    Sha256::digest(text.as_bytes())
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

/// Emits the `SENT` lines of share `part` of `parts`, then `SCHEMA-END`. With
/// `changed` every entry hashes as if the schema had been edited. The
/// tables sorted by name are dealt out round robin; share 0 also holds the
/// database record, the system tables and the sequences.
pub fn emit_image(
    out: &mut impl Write,
    database: &Database,
    part: usize,
    parts: usize,
    faults: &Faults,
    changed: bool,
    broken: bool,
) -> io::Result<()> {
    let ldb = database.ldb.as_bytes();
    let mut all = entries(database);
    if changed {
        for entry in &mut all {
            entry.text.push_str("changed\n");
        }
    }
    let mut tables: Vec<String> = all
        .iter()
        .filter(|e| e.kind == "table")
        .map(|e| e.name.clone())
        .collect();
    tables.sort();
    let table_count = tables.len() + usize::from(faults.count_skew == Some(part));
    let skipped = faults.skip_share == Some(part);
    all.retain(|entry| match entry.kind {
        "table" => {
            let place = tables.binary_search(&entry.name).unwrap_or_default();
            place % parts == part && !skipped
        }
        _ => part == 0,
    });
    let mut lines: Vec<[Vec<u8>; 4]> = all
        .into_iter()
        .map(|entry| {
            [
                entry.kind.as_bytes().to_vec(),
                entry.name.into_bytes(),
                entry.records.to_string().into_bytes(),
                sha256_hex(&entry.text).into_bytes(),
            ]
        })
        .collect();
    let mut count = lines.len();
    let fault = match broken {
        true => Some("bad-count"),
        false => faults.schema_fault.as_deref(),
    };
    match fault {
        Some("bad-count") => count += 1,
        Some("duplicate-entry") if !lines.is_empty() => {
            let first = lines[0].clone();
            lines.push(first);
            count += 1;
        }
        Some("bad-hash") if !lines.is_empty() => lines[0][3] = b"not-a-hash".to_vec(),
        Some("no-system-entry") => {
            lines.retain(|line| line[0] != b"system");
            count = lines.len();
        }
        _ => {}
    }
    if let Some(delay) = faults.schema_delay {
        std::thread::sleep(delay);
    }
    for (n, [kind, name, records, sha]) in lines.iter().enumerate() {
        if faults.noise && n % 3 == 0 {
            writeln!(out, "noise between schema entries")?;
        }
        emit(out, "SENT", &[ldb, kind, name, records, sha])?;
    }
    emit(
        out,
        "SCHEMA-END",
        &[
            ldb,
            count.to_string().as_bytes(),
            table_count.to_string().as_bytes(),
        ],
    )
}

/// What a raw digest share sees of the database besides its records.
pub struct RawState<'a> {
    /// The schema image differs, as with `changed` of [`emit_image`].
    pub changed: bool,
    /// Text that changes the raw digest and not the image: a field that the
    /// image leaves out, or a record id.
    pub salt: &'a str,
    /// The answer has a wrong count.
    pub broken: bool,
}

/// Emits the `RAW` lines of share `part` of `parts` of the raw digest in
/// `buckets` pieces, then `RAW-END`. Share 0 also makes the head. The pieces
/// hash the canonical text of the records, so the digest follows every change
/// of the image; a change only shows in the piece that holds the record. Returns `true` when the worker must quit, after an
/// `ERROR`.
pub fn emit_raw(
    out: &mut impl Write,
    database: &Database,
    (part, parts, buckets): (usize, usize, usize),
    faults: &Faults,
    state: RawState<'_>,
) -> io::Result<bool> {
    let ldb = database.ldb.as_bytes();
    let mut tables: Vec<String> = database
        .schema
        .iter()
        .map(|record| entry_of(record))
        .filter(|(kind, _)| *kind == "table")
        .map(|(_, name)| name)
        .collect();
    tables.sort();
    tables.dedup();
    // Like the worker: a table's records go into the piece of its place, the
    // other records into the head.
    let mut texts: Vec<String> = vec![String::new(); buckets + 1];
    let mut head_rows = 0;
    for record in &database.schema {
        let (kind, name) = entry_of(record);
        let at = match kind {
            "table" => tables.binary_search(&name).unwrap_or_default() % buckets.max(1),
            _ => {
                head_rows += 1;
                buckets
            }
        };
        texts[at].push_str(&canonical_line(record));
    }
    if state.changed {
        for text in texts.iter_mut().filter(|text| !text.is_empty()) {
            text.push_str("changed\n");
        }
    }
    texts[buckets].push_str(state.salt);
    let mut pieces: Vec<[String; 5]> = Vec::new();
    if part == 0 {
        let sha = sha256_hex(&format!("head\n{}", texts[buckets]));
        pieces.push([
            "head".into(),
            "-".into(),
            "-".into(),
            head_rows.to_string(),
            sha,
        ]);
    }
    for at in (part..buckets).step_by(parts.max(1)) {
        let low = if at == 0 {
            "-".into()
        } else {
            (at * 10).to_string()
        };
        let high = if at + 1 == buckets {
            "-".into()
        } else {
            ((at + 1) * 10).to_string()
        };
        let rows = texts[at].lines().count().to_string();
        let sha = sha256_hex(&format!("{at}\n{}", texts[at]));
        pieces.push([at.to_string(), low, high, rows, sha]);
    }
    let mut count = pieces.len();
    let faulty = faults.raw_fault_share.unwrap_or(0) == part;
    let fault = match state.broken {
        true => Some("bad-count"),
        false => faults.raw_fault.as_deref().filter(|_| faulty),
    };
    match fault {
        Some("bad-count") => count += 1,
        Some("missing-piece") => {
            pieces.pop();
            count = pieces.len();
        }
        Some("extra-piece") => {
            let sha = sha256_hex(&texts[buckets]);
            pieces.push([buckets.to_string(), "-".into(), "-".into(), "0".into(), sha]);
            count = pieces.len();
        }
        Some("bounds") => {
            if let Some(piece) = pieces.iter_mut().find(|piece| piece[0] != "head") {
                piece[1] = "999999".into();
            }
        }
        Some("bad-hash") if !pieces.is_empty() => pieces[0][4] = "not-a-hash".into(),
        Some("error") => {
            emit(out, "ERROR", &[b"raw digest failed"])?;
            return Ok(true);
        }
        Some("exit") => std::process::exit(1),
        _ => {}
    }
    if let Some(delay) = faults.schema_delay {
        std::thread::sleep(delay);
    }
    for [piece, low, high, rows, sha] in &pieces {
        emit(
            out,
            "RAW",
            &[
                ldb,
                piece.as_bytes(),
                low.as_bytes(),
                high.as_bytes(),
                rows.as_bytes(),
                sha.as_bytes(),
            ],
        )?;
    }
    let physical = database
        .physical
        .clone()
        .unwrap_or(format!("/db/{}", database.ldb));
    emit(
        out,
        "RAW-END",
        &[
            ldb,
            count.to_string().as_bytes(),
            database.schema.len().to_string().as_bytes(),
            physical.as_bytes(),
        ],
    )?;
    Ok(false)
}
