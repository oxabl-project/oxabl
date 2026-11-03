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

/// The entry that a record belongs to: `db`, the table of a table, field,
/// index, index field or trigger, or the sequence.
fn entry_of(record: &[String]) -> (&'static str, String) {
    let second = record.get(1).cloned().unwrap_or_default();
    match record.first().map(String::as_str) {
        Some("db") => ("db", String::new()),
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

fn entries(database: &Database) -> Vec<Entry> {
    let mut entries: Vec<Entry> = Vec::new();
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

/// Emits the `SENT` lines of share `part` of `parts`, then `SCHEMA-END`. The
/// tables sorted by name are dealt out round robin; share 0 also holds the
/// database record and the sequences.
pub fn emit_image(
    out: &mut impl Write,
    database: &Database,
    part: usize,
    parts: usize,
    faults: &Faults,
) -> io::Result<()> {
    let ldb = database.ldb.as_bytes();
    let mut all = entries(database);
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
    match faults.schema_fault.as_deref() {
        Some("bad-count") => count += 1,
        Some("duplicate-entry") if !lines.is_empty() => {
            let first = lines[0].clone();
            lines.push(first);
            count += 1;
        }
        Some("bad-hash") if !lines.is_empty() => lines[0][3] = b"not-a-hash".to_vec(),
        _ => {}
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
