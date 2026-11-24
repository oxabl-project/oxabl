//! The install record: which tool put the running binary where it is.

use std::io::Write;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::paths;

const FILE_NAME: &str = "install.json";

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Record {
    pub format: u32,
    pub installer: String,
    pub path: String,
    pub version: String,
}

pub fn file() -> Option<PathBuf> {
    Some(paths::data_dir()?.join(FILE_NAME))
}

/// The record, or none when it is absent or unreadable.
pub fn read() -> Option<Record> {
    let record: Record = serde_json::from_slice(&std::fs::read(file()?).ok()?).ok()?;
    (record.format == 1).then_some(record)
}

/// A binary is managed when the record names its canonical path.
pub fn managed(record: Option<&Record>, binary: &Path) -> bool {
    record.is_some_and(|record| {
        let named = Path::new(&record.path);
        named.canonicalize().is_ok_and(|named| named == binary)
    })
}

/// Writes the record atomically: a temporary file and one rename.
pub fn write(record: &Record) -> Result<(), String> {
    let file = file().ok_or("neither XDG_DATA_HOME nor HOME is set")?;
    let directory = file.parent().expect("the record has a directory");
    std::fs::create_dir_all(directory)
        .map_err(|e| format!("cannot create {}: {e}", directory.display()))?;
    let temporary = directory.join(format!(".{FILE_NAME}.{}", std::process::id()));
    let mut text = serde_json::to_string(record).expect("a record is JSON");
    text.push('\n');
    let written = std::fs::File::create(&temporary)
        .and_then(|mut f| f.write_all(text.as_bytes()).and_then(|()| f.sync_all()))
        .and_then(|()| std::fs::rename(&temporary, &file));
    written.map_err(|e| {
        let _ = std::fs::remove_file(&temporary);
        format!("cannot write {}: {e}", file.display())
    })
}
