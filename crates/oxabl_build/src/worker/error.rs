//! Why a worker or a pool failed.

use std::fmt;

/// A failure of one worker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WorkerError {
    /// The command could not start.
    Spawn(String),
    /// Its stdout ended, or its process exited.
    Lost(String),
    /// It did not answer in time.
    Timeout(String),
    /// It broke the protocol.
    Protocol(String),
    /// A cancel arrived while waiting.
    Cancelled,
}

impl fmt::Display for WorkerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WorkerError::Spawn(m) => write!(f, "cannot start the worker command: {m}"),
            WorkerError::Lost(m) => write!(f, "worker lost: {m}"),
            WorkerError::Timeout(m) => write!(f, "worker timed out: {m}"),
            WorkerError::Protocol(m) => write!(f, "worker protocol error: {m}"),
            WorkerError::Cancelled => f.write_str("cancelled"),
        }
    }
}

impl std::error::Error for WorkerError {}

/// Why the pool could not start. Each variant maps to an exit code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PoolError {
    /// The configuration or the environment is wrong: exit code 2.
    Config(String),
    /// Workers could not start: exit code 3.
    Infra(String),
    /// A cancel arrived: exit code 130.
    Cancelled,
}

impl fmt::Display for PoolError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PoolError::Config(m) | PoolError::Infra(m) => f.write_str(m),
            PoolError::Cancelled => f.write_str("cancelled"),
        }
    }
}

impl std::error::Error for PoolError {}
