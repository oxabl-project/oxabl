use std::fmt;

use rusqlite::ErrorCode;

/// Why the store could not be used.
#[derive(Debug)]
pub enum StoreError {
    /// The file is damaged or truncated; it can be moved aside and replaced.
    Corrupt(String),
    /// The file is not an oxabl store; oxabl never touches it.
    Foreign(String),
    /// The directory is on a file system that WAL cannot use.
    Unsupported(String),
    /// Anything else: the disk, a permission, a locked file.
    Failed(String),
}

impl fmt::Display for StoreError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StoreError::Corrupt(message) => write!(f, "the build store is damaged: {message}"),
            StoreError::Foreign(message)
            | StoreError::Unsupported(message)
            | StoreError::Failed(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for StoreError {}

impl From<rusqlite::Error> for StoreError {
    fn from(error: rusqlite::Error) -> Self {
        match error.sqlite_error_code() {
            Some(ErrorCode::DatabaseCorrupt | ErrorCode::NotADatabase) => {
                StoreError::Corrupt(error.to_string())
            }
            _ => StoreError::Failed(format!("build store: {error}")),
        }
    }
}

impl From<std::io::Error> for StoreError {
    fn from(error: std::io::Error) -> Self {
        StoreError::Failed(format!("build store: {error}"))
    }
}
