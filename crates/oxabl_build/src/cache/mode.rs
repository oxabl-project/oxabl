//! The cache mode, and with it whether the shared local cache may be read.

use crate::error::BuildError;

/// How this machine takes part in the caches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Ci,
    Developer,
    ReadOnly,
}

impl Mode {
    /// `OXABL_CACHE_MODE` when set, else `read-only` when `CI` is set, else
    /// `developer`. A value that is not exactly `ci`, `developer` or
    /// `read-only` is a usage error: a mode that cannot be read must not
    /// fall back to the one that trusts the most.
    pub fn resolve(
        mode: Option<&std::ffi::OsStr>,
        ci: Option<&std::ffi::OsStr>,
    ) -> Result<Mode, BuildError> {
        match mode {
            Some(value) => match value.to_str() {
                Some("ci") => Ok(Mode::Ci),
                Some("developer") => Ok(Mode::Developer),
                Some("read-only") => Ok(Mode::ReadOnly),
                _ => Err(BuildError::Usage(
                    "OXABL_CACHE_MODE must be exactly ci, developer or read-only".to_string(),
                )),
            },
            None if ci.is_some() => Ok(Mode::ReadOnly),
            None => Ok(Mode::Developer),
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Mode::Ci => "ci",
            Mode::Developer => "developer",
            Mode::ReadOnly => "read-only",
        }
    }
}
