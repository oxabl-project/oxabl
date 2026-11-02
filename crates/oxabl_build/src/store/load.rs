use rusqlite::Transaction;

use super::meta::{self, ANALYSIS_IDENTITY, GENERATION};
use super::rows::{FileRow, ListingRow, StoredAnalysis};
use super::{Store, StoreError};

/// A read transaction: everything read through it is one snapshot of the store.
pub struct Reading<'a> {
    tx: Transaction<'a>,
}

/// The path table, the tracked files and the listings of one snapshot, and the
/// head of each analysis (its generation and the hash of the file it analysed).
pub struct Loaded {
    pub generation: u64,
    pub identity: Option<[u8; 32]>,
    /// The canonical path of each path id; index 0 is unused.
    pub paths: Vec<String>,
    pub files: Vec<(u32, FileRow)>,
    pub listings: Vec<(u32, ListingRow)>,
    pub analysis_heads: Vec<(u32, u64, [u8; 32])>,
}

fn hash_of(bytes: Vec<u8>) -> Result<[u8; 32], StoreError> {
    <[u8; 32]>::try_from(bytes.as_slice())
        .map_err(|_| StoreError::Corrupt("a hash is not 32 bytes".to_string()))
}

impl Loaded {
    /// The snapshot of a workspace with no store.
    pub fn empty() -> Self {
        Loaded {
            generation: 0,
            identity: None,
            paths: vec![String::new()],
            files: Vec::new(),
            listings: Vec::new(),
            analysis_heads: Vec::new(),
        }
    }
}

impl Store {
    /// Start a read transaction.
    pub fn read(&mut self) -> Result<Reading<'_>, StoreError> {
        Ok(Reading {
            tx: self.conn.transaction()?,
        })
    }
}

impl Reading<'_> {
    /// Load the path table, the files, the listings and the analysis heads.
    pub fn load(&self) -> Result<Loaded, StoreError> {
        let generation = match meta::get(&self.tx, GENERATION)? {
            Some(bytes) => String::from_utf8_lossy(&bytes)
                .parse()
                .map_err(|_| StoreError::Corrupt("the generation is not a number".to_string()))?,
            None => 0,
        };
        let identity = match meta::get(&self.tx, ANALYSIS_IDENTITY)? {
            Some(bytes) => Some(hash_of(bytes)?),
            None => None,
        };

        let mut paths = vec![String::new()];
        let mut statement = self.tx.prepare("SELECT id, path FROM paths ORDER BY id")?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            let id = row.get::<_, i64>(0)? as usize;
            if paths.len() <= id {
                paths.resize(id + 1, String::new());
            }
            paths[id] = row.get(1)?;
        }

        let mut files = Vec::new();
        let mut statement = self.tx.prepare(
            "SELECT path_id, present, size, mtime_ns, ctime_ns, ino, dev, hash,
                    observed_ns, changed_gen, tracked_by FROM files",
        )?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            let hash = match row.get::<_, Option<Vec<u8>>>(7)? {
                Some(bytes) => Some(hash_of(bytes)?),
                None => None,
            };
            files.push((
                row.get::<_, i64>(0)? as u32,
                FileRow {
                    present: row.get::<_, i64>(1)? != 0,
                    size: row.get::<_, Option<i64>>(2)?.unwrap_or(0) as u64,
                    mtime_ns: row.get::<_, Option<i64>>(3)?.unwrap_or(0),
                    ctime_ns: row.get::<_, Option<i64>>(4)?.unwrap_or(0),
                    ino: row.get::<_, Option<i64>>(5)?.unwrap_or(0) as u64,
                    dev: row.get::<_, Option<i64>>(6)?.unwrap_or(0) as u64,
                    hash,
                    observed_ns: row.get(8)?,
                    changed_gen: row.get::<_, i64>(9)? as u64,
                    tracked_by: row.get::<_, i64>(10)? as u8,
                },
            ));
        }

        let mut listings = Vec::new();
        let mut statement = self.tx.prepare(
            "SELECT path_id, mtime_ns, ctime_ns, ino, observed_ns, digest, changed_gen
             FROM listings",
        )?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            listings.push((
                row.get::<_, i64>(0)? as u32,
                ListingRow {
                    mtime_ns: row.get(1)?,
                    ctime_ns: row.get(2)?,
                    ino: row.get::<_, i64>(3)? as u64,
                    observed_ns: row.get(4)?,
                    digest: hash_of(row.get(5)?)?,
                    changed_gen: row.get::<_, i64>(6)? as u64,
                },
            ));
        }

        let mut analysis_heads = Vec::new();
        let mut statement = self
            .tx
            .prepare("SELECT path_id, analysed_gen, content_hash FROM analyses")?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            analysis_heads.push((
                row.get::<_, i64>(0)? as u32,
                row.get::<_, i64>(1)? as u64,
                hash_of(row.get(2)?)?,
            ));
        }

        Ok(Loaded {
            generation,
            identity,
            paths,
            files,
            listings,
            analysis_heads,
        })
    }

    /// Load every analysis, blobs included.
    pub fn analyses(&self) -> Result<Vec<(u32, StoredAnalysis)>, StoreError> {
        let mut out = Vec::new();
        let mut statement = self.tx.prepare(
            "SELECT path_id, analysed_gen, content_hash, completeness, edges, unresolved,
                    observations FROM analyses",
        )?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            out.push((
                row.get::<_, i64>(0)? as u32,
                StoredAnalysis {
                    analysed_gen: row.get::<_, i64>(1)? as u64,
                    content_hash: hash_of(row.get(2)?)?,
                    completeness: row.get(3)?,
                    edges: row.get(4)?,
                    unresolved: row.get(5)?,
                    observations: row.get(6)?,
                },
            ));
        }
        Ok(out)
    }
}
