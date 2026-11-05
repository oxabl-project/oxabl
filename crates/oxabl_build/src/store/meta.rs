use rusqlite::{OptionalExtension, params};

use super::{Store, StoreError};

pub const GENERATION: &str = "generation";
pub const ANALYSIS_IDENTITY: &str = "analysis_identity";
pub const BINARY_HASH: &str = "binary_hash";
const CLEAN: &str = "clean";

pub fn get(conn: &rusqlite::Connection, key: &str) -> Result<Option<Vec<u8>>, StoreError> {
    Ok(conn
        .query_row("SELECT value FROM meta WHERE key = ?1", [key], |row| {
            row.get(0)
        })
        .optional()?)
}

pub fn put(conn: &rusqlite::Connection, key: &str, value: &[u8]) -> Result<(), StoreError> {
    conn.execute(
        "INSERT INTO meta (key, value) VALUES (?1, ?2)
         ON CONFLICT(key) DO UPDATE SET value = excluded.value",
        params![key, value],
    )?;
    Ok(())
}

impl Store {
    pub(super) fn set_clean(&self, clean: bool) -> Result<(), StoreError> {
        put(&self.conn, CLEAN, if clean { b"1" } else { b"0" })
    }
}
