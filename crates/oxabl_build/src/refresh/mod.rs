mod analyse;
mod assemble;
mod binary;
mod convert;
mod files;
mod identity;
mod intern;
mod listing;
mod outcome;
pub(crate) mod parallel;
mod probe;
mod session;
mod settle;
mod snapshot;
mod walk;

pub use outcome::{Content, Delta, Outcome};
pub use session::{Request, run};
pub use settle::Analyses;

use crate::error::BuildError;
use crate::store::StoreError;

/// Why a refresh stopped.
#[derive(Debug)]
pub enum RefreshError {
    Store(StoreError),
    Build(BuildError),
}

impl From<StoreError> for RefreshError {
    fn from(error: StoreError) -> Self {
        RefreshError::Store(error)
    }
}

impl From<BuildError> for RefreshError {
    fn from(error: BuildError) -> Self {
        RefreshError::Build(error)
    }
}
