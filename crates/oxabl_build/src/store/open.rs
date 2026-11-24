use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use rusqlite::{Connection, OpenFlags};

use super::StoreError;
use super::ddl::{APPLICATION_ID, SCHEMA, STORE_FORMAT};
use super::filesystem::require_local;
use super::inspect::{Inspection, inspect};

const FILE_NAME: &str = "state.db";
const SIDE_FILES: [&str; 2] = ["-wal", "-shm"];

/// The store of one workspace: one SQLite file in WAL mode.
pub struct Store {
    pub(super) conn: Connection,
    writable: bool,
}

/// A store opened for writing, and what had to happen to open it.
pub struct Opened {
    pub store: Store,
    /// One line for each repair: a replaced or a moved-aside store.
    pub notes: Vec<String>,
}

impl Store {
    /// The path of the store file in `directory`.
    pub fn path_in(directory: &Path) -> PathBuf {
        directory.join(FILE_NAME)
    }

    /// Open the store in `directory` for writing, creating it when it is
    /// missing. A store of another format is deleted and created again; a
    /// damaged one is moved aside and created again. The caller holds the
    /// workspace lock.
    pub fn open_writer(directory: &Path) -> Result<Opened, StoreError> {
        std::fs::create_dir_all(directory)?;
        require_local(directory)?;
        let path = Self::path_in(directory);
        let mut notes = Vec::new();
        let conn = match inspect(&path)? {
            Inspection::Missing => create(&path, true)?,
            Inspection::Empty => create(&path, false)?,
            Inspection::Current => match open_current(&path) {
                Ok(conn) => conn,
                Err(StoreError::Corrupt(message)) => {
                    notes.push(Store::move_aside(directory, &message)?);
                    create(&path, true)?
                }
                Err(e) => return Err(e),
            },
            Inspection::OtherFormat(format) => {
                remove_files(&path)?;
                notes.push(format!(
                    "the build store has format {format}, not {STORE_FORMAT}: replaced it"
                ));
                create(&path, true)?
            }
            Inspection::Corrupt(message) => {
                notes.push(Store::move_aside(directory, &message)?);
                create(&path, true)?
            }
        };
        let store = Store {
            conn,
            writable: true,
        };
        store.set_clean(false)?;
        Ok(Opened { store, notes })
    }

    /// Open the store in `directory` to read it. `None` when there is no store
    /// this process can use: none yet, one of another format, or a damaged one.
    /// Nothing is changed.
    pub fn open_reader(directory: &Path) -> Result<Option<Store>, StoreError> {
        let path = Self::path_in(directory);
        match inspect(&path)? {
            Inspection::Current => {}
            _ => return Ok(None),
        }
        require_local(directory)?;
        let conn = Connection::open_with_flags(&path, OpenFlags::SQLITE_OPEN_READ_WRITE)?;
        conn.busy_timeout(std::time::Duration::from_millis(5000))?;
        Ok(Some(Store {
            conn,
            writable: false,
        }))
    }

    /// Move the store files in `directory` aside as `state.db.corrupt-<unix
    /// seconds>`, keeping only the newest such set. Returns the note for the
    /// caller to print.
    pub fn move_aside(directory: &Path, reason: &str) -> Result<String, StoreError> {
        set_aside(&Self::path_in(directory), reason)
    }

    /// Mark a clean close and close the connection.
    pub fn close(self) -> Result<(), StoreError> {
        if self.writable {
            self.set_clean(true)?;
        }
        self.conn.close().map_err(|(_, e)| e.into())
    }
}

fn settings(conn: &Connection) -> Result<(), StoreError> {
    conn.busy_timeout(std::time::Duration::from_millis(5000))?;
    conn.pragma_update_and_check(None, "journal_mode", "WAL", |row| row.get::<_, String>(0))?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;
    conn.pragma_update(None, "foreign_keys", "ON")?;
    Ok(())
}

/// Create the store file with the current tables. `fresh` is false for an
/// existing empty SQLite file.
fn create(path: &Path, fresh: bool) -> Result<Connection, StoreError> {
    if fresh {
        remove_files(path)?;
    }
    let conn = Connection::open(path)?;
    conn.execute_batch(&format!(
        "BEGIN;
         PRAGMA application_id = {APPLICATION_ID};
         PRAGMA user_version = {STORE_FORMAT};
         {SCHEMA}
         COMMIT;"
    ))?;
    settings(&conn)?;
    Ok(conn)
}

/// Open the store file of the current format. A store that was not closed
/// cleanly is checked; a file shorter than its header says is damaged.
fn open_current(path: &Path) -> Result<Connection, StoreError> {
    let conn = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_WRITE)?;
    settings(&conn)?;
    conn.query_row("PRAGMA wal_checkpoint(TRUNCATE)", [], |_| Ok(()))?;
    let pages: i64 = conn.query_row("PRAGMA page_count", [], |r| r.get(0))?;
    let page_size: i64 = conn.query_row("PRAGMA page_size", [], |r| r.get(0))?;
    let length = std::fs::metadata(path)?.len() as i64;
    if length < pages * page_size {
        return Err(StoreError::Corrupt(format!(
            "the file holds {length} bytes and its header says {}",
            pages * page_size
        )));
    }
    let clean: Option<Vec<u8>> = conn
        .query_row("SELECT value FROM meta WHERE key = 'clean'", [], |r| {
            r.get(0)
        })
        .ok();
    if clean.as_deref() != Some(b"1") {
        let result: String = conn.query_row("PRAGMA quick_check", [], |r| r.get(0))?;
        if result != "ok" {
            return Err(StoreError::Corrupt(result));
        }
    }
    Ok(conn)
}

/// Move the three store files aside as `state.db.corrupt-<unix seconds>` and
/// keep only the newest such set. Returns the note for the caller to print.
fn set_aside(path: &Path, reason: &str) -> Result<String, StoreError> {
    let directory = path.parent().unwrap_or(Path::new("."));
    for entry in std::fs::read_dir(directory)? {
        let entry = entry?;
        if entry
            .file_name()
            .to_string_lossy()
            .starts_with("state.db.corrupt-")
        {
            std::fs::remove_file(entry.path())?;
        }
    }
    let seconds = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());
    let aside = |suffix: &str| directory.join(format!("{FILE_NAME}.corrupt-{seconds}{suffix}"));
    std::fs::rename(path, aside(""))?;
    for suffix in SIDE_FILES {
        let side = PathBuf::from(format!("{}{suffix}", path.display()));
        if side.exists() {
            std::fs::rename(&side, aside(suffix))?;
        }
    }
    Ok(format!(
        "the build store is damaged ({reason}): moved it to {FILE_NAME}.corrupt-{seconds}, building a new one"
    ))
}

fn remove_files(path: &Path) -> Result<(), StoreError> {
    for file in std::iter::once(path.to_path_buf()).chain(
        SIDE_FILES
            .iter()
            .map(|suffix| PathBuf::from(format!("{}{suffix}", path.display()))),
    ) {
        match std::fs::remove_file(&file) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}
