//! The AVM worker pool and the `OXW1` worker protocol.

pub mod cancel;
pub mod codec;
pub mod command;
pub mod compile;
pub mod error;
pub mod frame;
pub mod handshake;
mod imaging;
pub mod kill;
pub mod logs;
pub mod pool;
pub mod program;
pub mod random;
mod reader;
mod schema_fetch;
pub mod session;
pub mod slot;
mod start;

pub use cancel::Cancel;
pub use error::{PoolError, WorkerError};
pub use pool::{Pool, PoolSpec, Started};
pub use slot::{Definitions, Job, Restart, Worker};
