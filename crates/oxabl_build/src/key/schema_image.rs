//! The schema image of one database and its identity.

use super::encode::{Encoder, SCHEMA_CONTEXT, derive};

/// One hashed part of a schema: the database record, one table with
/// everything that belongs to it, or one sequence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemaEntry {
    /// `db`, `table` or `sequence`.
    pub kind: String,
    /// The table or sequence name; empty for `db`.
    pub name: String,
    /// The number of records in the entry's text.
    pub records: u64,
    /// SHA-256 of the entry's canonical text, in lower-case hex.
    pub sha256: String,
}

impl SchemaEntry {
    fn encoded(&self) -> Vec<u8> {
        let mut encoder = Encoder::new();
        encoder
            .str(&self.kind)
            .str(&self.name)
            .u64(self.records)
            .str(&self.sha256);
        encoder.finish()
    }
}

/// The entries one worker reported for its share of a database.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemaPart {
    pub part: usize,
    pub entries: Vec<SchemaEntry>,
    /// The number of user tables in the whole database, as this worker counted.
    pub tables: usize,
}

/// The entries a worker reported for one database.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemaImage {
    pub ldb: String,
    pub entries: Vec<SchemaEntry>,
    identity: blake3::Hash,
}

impl SchemaImage {
    /// Hashes the entries sorted by their encoding, so the order the worker
    /// reported them in does not matter.
    pub fn new(ldb: String, entries: Vec<SchemaEntry>) -> SchemaImage {
        let mut encoded: Vec<Vec<u8>> = entries.iter().map(SchemaEntry::encoded).collect();
        encoded.sort();
        let mut material = Encoder::new();
        material.u64(encoded.len() as u64);
        for entry in &encoded {
            material.raw(entry);
        }
        SchemaImage {
            ldb,
            entries,
            identity: derive(SCHEMA_CONTEXT, &material.finish()),
        }
    }

    /// Joins the shares of all workers into one image. Fails unless the
    /// shares together are complete: every worker saw the same number of
    /// tables, each table is in exactly one share, the database record is in
    /// the first share only, and sequences are too.
    pub fn combine(ldb: String, mut parts: Vec<SchemaPart>) -> Result<SchemaImage, String> {
        parts.sort_by_key(|part| part.part);
        let tables = parts.first().map(|part| part.tables).unwrap_or_default();
        let mut entries: Vec<SchemaEntry> = Vec::new();
        for part in parts {
            if part.tables != tables {
                return Err(format!(
                    "workers count {tables} and {} tables in {ldb}",
                    part.tables
                ));
            }
            for entry in part.entries {
                if entry.kind != "table" && part.part != 0 {
                    return Err(format!(
                        "share {} of {ldb} holds the {} entry `{}`",
                        part.part, entry.kind, entry.name
                    ));
                }
                if entries
                    .iter()
                    .any(|e| e.kind == entry.kind && e.name == entry.name)
                {
                    return Err(format!(
                        "entry `{} {}` of {ldb} is in two shares",
                        entry.kind, entry.name
                    ));
                }
                entries.push(entry);
            }
        }
        let found = entries.iter().filter(|e| e.kind == "table").count();
        if found != tables {
            return Err(format!(
                "{ldb} has {tables} tables but the workers imaged {found}"
            ));
        }
        if entries.iter().filter(|e| e.kind == "db").count() != 1 {
            return Err(format!("{ldb} lacks its database entry"));
        }
        Ok(SchemaImage::new(ldb, entries))
    }

    pub fn identity(&self) -> blake3::Hash {
        self.identity
    }

    /// The number of records in all entries.
    pub fn records(&self) -> u64 {
        self.entries.iter().map(|entry| entry.records).sum()
    }
}
