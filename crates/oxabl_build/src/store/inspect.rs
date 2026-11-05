use std::path::Path;

use rusqlite::{Connection, OpenFlags};

use super::StoreError;
use super::ddl::{APPLICATION_ID, STORE_FORMAT};

const SQLITE_MAGIC: &[u8; 16] = b"SQLite format 3\0";

/// What the file at the store path is.
pub enum Inspection {
    /// No file, or an empty one.
    Missing,
    /// An SQLite file with no tables, as a crash during creation leaves.
    Empty,
    /// An oxabl store of the current format.
    Current,
    /// An oxabl store of another format.
    OtherFormat(i32),
    /// An oxabl store that is damaged or truncated.
    Corrupt(String),
}

/// Classify the file at `path` without changing it. A file that is not an oxabl
/// store is a [`StoreError::Foreign`].
pub fn inspect(path: &Path) -> Result<Inspection, StoreError> {
    let length = match std::fs::metadata(path) {
        Ok(metadata) => metadata.len(),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Inspection::Missing),
        Err(e) => return Err(e.into()),
    };
    if length == 0 {
        return Ok(Inspection::Missing);
    }
    let mut head = [0u8; 16];
    let read = {
        use std::io::Read;
        std::fs::File::open(path)?.read(&mut head)?
    };
    if head[..read] != SQLITE_MAGIC[..read] {
        return Err(foreign(path));
    }
    if length < 100 {
        return Ok(Inspection::Corrupt("the file is truncated".to_string()));
    }
    let probe = || -> Result<Inspection, StoreError> {
        let conn = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_WRITE)?;
        let tables: i64 = conn.query_row("SELECT count(*) FROM sqlite_master", [], |r| r.get(0))?;
        let application: i32 = conn.query_row("PRAGMA application_id", [], |r| r.get(0))?;
        if tables == 0 && application == 0 {
            return Ok(Inspection::Empty);
        }
        if application != APPLICATION_ID {
            return Err(foreign(path));
        }
        let format: i32 = conn.query_row("PRAGMA user_version", [], |r| r.get(0))?;
        Ok(if format == STORE_FORMAT {
            Inspection::Current
        } else {
            Inspection::OtherFormat(format)
        })
    };
    match probe() {
        Err(StoreError::Corrupt(message)) => Ok(Inspection::Corrupt(message)),
        other => other,
    }
}

fn foreign(path: &Path) -> StoreError {
    StoreError::Foreign(format!(
        "{} is not an oxabl build store; oxabl does not delete a file it did not create",
        path.display()
    ))
}
