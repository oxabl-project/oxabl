use rusqlite::{TransactionBehavior, params};

use super::meta::{self, ANALYSIS_IDENTITY, BINARY_HASH, GENERATION};
use super::rows::{BinaryRow, FileRow, ListingRow, StoredAnalysis};
use super::{Store, StoreError};

/// Everything one refresh writes. It is applied in one transaction.
#[derive(Default)]
pub struct Changes {
    /// Delete every analysis first: the analysis identity changed.
    pub clear_analyses: bool,
    pub new_paths: Vec<(u32, String)>,
    pub files: Vec<(u32, FileRow)>,
    pub files_deleted: Vec<u32>,
    pub listings: Vec<(u32, ListingRow)>,
    pub listings_deleted: Vec<u32>,
    pub analyses: Vec<(u32, StoredAnalysis)>,
    pub analyses_deleted: Vec<u32>,
    /// Units whose observed closure the store forgets: they are no longer units.
    pub observed_deleted: Vec<u32>,
    pub generation: Option<u64>,
    pub identity: Option<[u8; 32]>,
    /// The hash of the running executable, when it was taken again.
    pub binary: Option<BinaryRow>,
}

impl Changes {
    pub fn is_empty(&self) -> bool {
        !self.clear_analyses
            && self.new_paths.is_empty()
            && self.files.is_empty()
            && self.files_deleted.is_empty()
            && self.listings.is_empty()
            && self.listings_deleted.is_empty()
            && self.analyses.is_empty()
            && self.analyses_deleted.is_empty()
            && self.observed_deleted.is_empty()
            && self.generation.is_none()
            && self.identity.is_none()
            && self.binary.is_none()
    }
}

impl Store {
    /// Apply `changes` in one transaction.
    pub fn commit(&mut self, changes: &Changes) -> Result<(), StoreError> {
        if changes.is_empty() {
            return Ok(());
        }
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        if changes.clear_analyses {
            tx.execute("DELETE FROM analyses", [])?;
        }
        {
            let mut insert = tx.prepare_cached("INSERT INTO paths (id, path) VALUES (?1, ?2)")?;
            for (id, path) in &changes.new_paths {
                insert.execute(params![id, path])?;
            }
        }
        {
            let mut upsert = tx.prepare_cached(
                "INSERT INTO files (path_id, present, size, mtime_ns, ctime_ns, ino, dev, hash,
                                    observed_ns, changed_gen, tracked_by)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)
                 ON CONFLICT(path_id) DO UPDATE SET
                    present = excluded.present, size = excluded.size,
                    mtime_ns = excluded.mtime_ns, ctime_ns = excluded.ctime_ns,
                    ino = excluded.ino, dev = excluded.dev, hash = excluded.hash,
                    observed_ns = excluded.observed_ns, changed_gen = excluded.changed_gen,
                    tracked_by = excluded.tracked_by",
            )?;
            for (id, row) in &changes.files {
                upsert.execute(params![
                    id,
                    row.present,
                    row.size as i64,
                    row.mtime_ns,
                    row.ctime_ns,
                    row.ino as i64,
                    row.dev as i64,
                    row.hash.as_ref().map(|h| h.as_slice()),
                    row.observed_ns,
                    row.changed_gen as i64,
                    row.tracked_by,
                ])?;
            }
            let mut delete = tx.prepare_cached("DELETE FROM files WHERE path_id = ?1")?;
            for id in &changes.files_deleted {
                delete.execute([id])?;
            }
        }
        {
            let mut upsert = tx.prepare_cached(
                "INSERT INTO listings (path_id, mtime_ns, ctime_ns, ino, observed_ns, digest,
                                       changed_gen)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)
                 ON CONFLICT(path_id) DO UPDATE SET
                    mtime_ns = excluded.mtime_ns, ctime_ns = excluded.ctime_ns,
                    ino = excluded.ino, observed_ns = excluded.observed_ns,
                    digest = excluded.digest, changed_gen = excluded.changed_gen",
            )?;
            for (id, row) in &changes.listings {
                upsert.execute(params![
                    id,
                    row.mtime_ns,
                    row.ctime_ns,
                    row.ino as i64,
                    row.observed_ns,
                    row.digest.as_slice(),
                    row.changed_gen as i64,
                ])?;
            }
            let mut delete = tx.prepare_cached("DELETE FROM listings WHERE path_id = ?1")?;
            for id in &changes.listings_deleted {
                delete.execute([id])?;
            }
        }
        {
            let mut upsert = tx.prepare_cached(
                "INSERT OR REPLACE INTO analyses (path_id, analysed_gen, content_hash,
                    completeness, edges, unresolved, observations)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            )?;
            for (id, analysis) in &changes.analyses {
                upsert.execute(params![
                    id,
                    analysis.analysed_gen as i64,
                    analysis.content_hash.as_slice(),
                    analysis.completeness,
                    analysis.edges,
                    analysis.unresolved,
                    analysis.observations,
                ])?;
            }
            let mut delete = tx.prepare_cached("DELETE FROM analyses WHERE path_id = ?1")?;
            for id in &changes.analyses_deleted {
                delete.execute([id])?;
            }
        }
        {
            let mut delete = tx.prepare_cached("DELETE FROM observed WHERE unit_id = ?1")?;
            for id in &changes.observed_deleted {
                delete.execute([id])?;
            }
        }
        if let Some(generation) = changes.generation {
            meta::put(&tx, GENERATION, generation.to_string().as_bytes())?;
        }
        if let Some(identity) = &changes.identity {
            meta::put(&tx, ANALYSIS_IDENTITY, identity)?;
        }
        if let Some(binary) = &changes.binary {
            meta::put(&tx, BINARY_HASH, &binary.encode())?;
        }
        tx.commit()?;
        Ok(())
    }
}
