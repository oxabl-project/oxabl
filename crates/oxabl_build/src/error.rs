use std::fmt;

use crate::worker::PoolError;

/// Why an `oxabl build` step stopped.
#[derive(Debug)]
pub enum BuildError {
    /// The configuration or the command line is wrong: exit code 2.
    Usage(String),
    /// The store, the lock or the workspace could not be used: exit code 3.
    Infrastructure(String),
    /// A signal stopped the command: exit code 130.
    Cancelled,
}

impl BuildError {
    /// The process exit code for this error.
    pub fn exit_code(&self) -> u8 {
        match self {
            BuildError::Usage(_) => 2,
            BuildError::Infrastructure(_) => 3,
            BuildError::Cancelled => 130,
        }
    }
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildError::Usage(message) | BuildError::Infrastructure(message) => {
                f.write_str(message)
            }
            BuildError::Cancelled => f.write_str("cancelled"),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<PoolError> for BuildError {
    fn from(error: PoolError) -> Self {
        match error {
            PoolError::Config(message) => BuildError::Usage(message),
            PoolError::Infra(message) => BuildError::Infrastructure(message),
            PoolError::Cancelled => BuildError::Cancelled,
        }
    }
}
