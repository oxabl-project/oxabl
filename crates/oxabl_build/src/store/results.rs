//! Compile results, the outputs they own and the environments they refer to.

use rusqlite::{OptionalExtension, Transaction, TransactionBehavior, params};
use rustc_hash::FxHashMap;

use super::load::Reading;
use super::meta;
use super::{Store, StoreError};

/// `results.status`.
pub const SUCCESS: u8 = 1;
pub const FAILED: u8 = 2;

/// `results.origin`.
pub const COMPILED: u8 = 1;
pub const LOCAL_CACHE: u8 = 2;
pub const REMOTE_CACHE: u8 = 3;

/// Meta key: a build has recorded results in this store.
const BUILT: &str = "built";
/// Meta keys of the last probe.
const ENVIRONMENT: &str = "environment";
const ENVIRONMENT_IDENTITY: &str = "environment_identity";
const ENVIRONMENT_AT: &str = "environment_at";

/// The number of sections of a compile key.
pub const SECTIONS: usize = 7;
/// Where the environment identity sits among the sections.
const ENVIRONMENT_SECTION: usize = crate::key::compile_key::ENVIRONMENT_SECTION;

/// The digest of each section of a compile key, in section order: unit,
/// closure, propath, environment, options, outputs, worker. The environment
/// section is the environment identity itself.
pub type Components = [[u8; 32]; SECTIONS];

/// One result row.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResultRow {
    pub status: u8,
    pub key: Option<[u8; 32]>,
    /// Empty for a unit that had no key.
    pub components: Option<Components>,
    pub compiled_gen: u64,
    pub origin: u8,
    pub duration_ms: u64,
    pub finished_at: i64,
    /// The file a read check found outside the closure.
    pub note: Option<String>,
}

/// One output file a result owns.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputRow {
    /// Root-relative.
    pub path: String,
    pub size: u64,
    pub mtime_ns: i64,
    pub ino: u64,
    pub hash: [u8; 32],
}

/// The results of one snapshot of the store.
#[derive(Debug, Default)]
pub struct Results {
    /// A build has recorded results; before that, a plan compares with the
    /// last refresh.
    pub built: bool,
    pub by_unit: FxHashMap<u32, ResultRow>,
    pub outputs: FxHashMap<u32, Vec<OutputRow>>,
    /// The environment record of the last probe, with its identity.
    pub environment: Option<StoredEnvironment>,
    /// Every environment record that a result refers to, by identity.
    pub environments: FxHashMap<[u8; 32], String>,
}

/// An environment record as stored.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoredEnvironment {
    pub identity: [u8; 32],
    pub record: String,
    pub at: String,
}

/// What one batch of a build writes.
#[derive(Debug, Default)]
pub struct ResultBatch {
    pub upserts: Vec<(u32, ResultRow, Vec<OutputRow>)>,
    /// Units whose result and outputs the store forgets.
    pub deletes: Vec<u32>,
    /// Outputs whose stat the planner refreshed after hashing them.
    pub refreshed: Vec<(u32, OutputRow)>,
    pub environment: Option<StoredEnvironment>,
}

impl ResultBatch {
    pub fn is_empty(&self) -> bool {
        self.upserts.is_empty()
            && self.deletes.is_empty()
            && self.refreshed.is_empty()
            && self.environment.is_none()
    }
}

fn hash_of(bytes: Vec<u8>) -> Result<[u8; 32], StoreError> {
    <[u8; 32]>::try_from(bytes.as_slice())
        .map_err(|_| StoreError::Corrupt("a hash is not 32 bytes".to_string()))
}

fn components_of(bytes: Vec<u8>) -> Result<Option<Components>, StoreError> {
    if bytes.is_empty() {
        return Ok(None);
    }
    if bytes.len() != SECTIONS * 32 {
        return Err(StoreError::Corrupt(
            "a component list has the wrong length".to_string(),
        ));
    }
    let (chunks, _) = bytes.as_chunks::<32>();
    let mut components = [[0u8; 32]; SECTIONS];
    components.copy_from_slice(chunks);
    Ok(Some(components))
}

impl Reading<'_> {
    /// Load every result, the outputs they own and the last environment.
    pub fn results(&self) -> Result<Results, StoreError> {
        let tx = self.transaction();
        let mut results = Results {
            built: meta::get(tx, BUILT)?.is_some(),
            ..Results::default()
        };
        let mut statement = tx.prepare(
            "SELECT unit_id, status, key, components, compiled_gen, origin, duration_ms,
                    finished_at, note FROM results",
        )?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            let key = match row.get::<_, Option<Vec<u8>>>(2)? {
                Some(bytes) => Some(hash_of(bytes)?),
                None => None,
            };
            results.by_unit.insert(
                row.get::<_, i64>(0)? as u32,
                ResultRow {
                    status: row.get::<_, i64>(1)? as u8,
                    key,
                    components: components_of(row.get(3)?)?,
                    compiled_gen: row.get::<_, i64>(4)? as u64,
                    origin: row.get::<_, i64>(5)? as u8,
                    duration_ms: row.get::<_, i64>(6)? as u64,
                    finished_at: row.get(7)?,
                    note: row.get(8)?,
                },
            );
        }
        let mut statement =
            tx.prepare("SELECT unit_id, path, size, mtime_ns, ino, hash FROM outputs")?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            results
                .outputs
                .entry(row.get::<_, i64>(0)? as u32)
                .or_default()
                .push(OutputRow {
                    path: row.get(1)?,
                    size: row.get::<_, i64>(2)? as u64,
                    mtime_ns: row.get(3)?,
                    ino: row.get::<_, i64>(4)? as u64,
                    hash: hash_of(row.get(5)?)?,
                });
        }
        let mut statement = tx.prepare("SELECT identity, record FROM environments")?;
        let mut rows = statement.query([])?;
        while let Some(row) = rows.next()? {
            results
                .environments
                .insert(hash_of(row.get(0)?)?, row.get(1)?);
        }
        if let (Some(identity), Some(at)) = (
            meta::get(tx, ENVIRONMENT_IDENTITY)?,
            meta::get(tx, ENVIRONMENT_AT)?,
        ) {
            let identity = hash_of(identity)?;
            let record: Option<String> = tx
                .query_row(
                    "SELECT record FROM environments WHERE identity = ?1",
                    [identity.as_slice()],
                    |row| row.get(0),
                )
                .optional()?;
            if let Some(record) = record {
                results.environment = Some(StoredEnvironment {
                    identity,
                    record,
                    at: String::from_utf8_lossy(&at).into_owned(),
                });
            }
        }
        Ok(results)
    }
}

fn put_output(tx: &Transaction<'_>, unit: u32, output: &OutputRow) -> Result<(), StoreError> {
    let written = tx.execute(
        "INSERT OR REPLACE INTO outputs (unit_id, path, size, mtime_ns, ino, hash)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
        params![
            unit,
            output.path,
            output.size as i64,
            output.mtime_ns,
            output.ino as i64,
            output.hash.as_slice()
        ],
    );
    match written {
        Ok(_) => Ok(()),
        Err(error)
            if error.sqlite_error_code() == Some(rusqlite::ErrorCode::ConstraintViolation) =>
        {
            Err(StoreError::Failed(format!(
                "the output {} is owned by two units",
                output.path
            )))
        }
        Err(error) => Err(error.into()),
    }
}

impl Store {
    /// Apply `batch` in one transaction. An output path that another unit
    /// owns stops the build: two units never reach one output.
    pub fn commit_results(&mut self, batch: &ResultBatch) -> Result<(), StoreError> {
        if batch.is_empty() {
            return Ok(());
        }
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        for unit in &batch.deletes {
            tx.execute("DELETE FROM outputs WHERE unit_id = ?1", [unit])?;
            tx.execute("DELETE FROM results WHERE unit_id = ?1", [unit])?;
        }
        for (unit, result, outputs) in &batch.upserts {
            tx.execute("DELETE FROM outputs WHERE unit_id = ?1", [unit])?;
            let components: Vec<u8> = result
                .components
                .iter()
                .flatten()
                .flat_map(|digest| digest.iter().copied())
                .collect();
            tx.execute(
                "INSERT OR REPLACE INTO results (unit_id, status, key, components, compiled_gen,
                    origin, duration_ms, finished_at, note)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                params![
                    unit,
                    result.status,
                    result.key.as_ref().map(|key| key.as_slice()),
                    components,
                    result.compiled_gen as i64,
                    result.origin,
                    result.duration_ms as i64,
                    result.finished_at,
                    result.note,
                ],
            )?;
            for output in outputs {
                put_output(&tx, *unit, output)?;
            }
        }
        for (unit, output) in &batch.refreshed {
            put_output(&tx, *unit, output)?;
        }
        if let Some(environment) = &batch.environment {
            tx.execute(
                "INSERT OR IGNORE INTO environments (identity, record, first_seen)
                 VALUES (?1, ?2, ?3)",
                params![
                    environment.identity.as_slice(),
                    environment.record,
                    environment.at
                ],
            )?;
            meta::put(&tx, ENVIRONMENT, environment.record.as_bytes())?;
            meta::put(&tx, ENVIRONMENT_IDENTITY, &environment.identity)?;
            meta::put(&tx, ENVIRONMENT_AT, environment.at.as_bytes())?;
        }
        meta::put(&tx, BUILT, b"1")?;
        tx.execute(
            "DELETE FROM environments WHERE identity <> COALESCE(
                 (SELECT value FROM meta WHERE key = 'environment_identity'), x'')
             AND identity NOT IN (
                 SELECT substr(components, ?1, 32) FROM results WHERE length(components) = ?2)",
            params![
                (ENVIRONMENT_SECTION * 32 + 1) as i64,
                (SECTIONS * 32) as i64
            ],
        )?;
        tx.commit()?;
        Ok(())
    }
}
