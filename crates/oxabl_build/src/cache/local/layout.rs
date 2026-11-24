//! Where the local cache keeps its files.

use std::path::{Path, PathBuf};

use super::super::seam::CompileKey;

/// The directory of the format; a new format is a new directory.
const VERSION: &str = "v1";

/// The paths below one cache directory.
#[derive(Clone, Debug)]
pub struct Layout {
    base: PathBuf,
}

impl Layout {
    pub fn new(directory: &Path) -> Layout {
        Layout {
            base: directory.join(VERSION),
        }
    }

    pub fn base(&self) -> &Path {
        &self.base
    }

    pub fn tmp(&self) -> PathBuf {
        self.base.join("tmp")
    }

    pub fn quarantine(&self) -> PathBuf {
        self.base.join("quarantine")
    }

    pub fn entries(&self) -> PathBuf {
        self.base.join("entries")
    }

    pub fn lock(&self) -> PathBuf {
        self.base.join("lock")
    }

    pub fn size(&self) -> PathBuf {
        self.base.join("size")
    }

    /// `entries/<first 2 hex>/<64 hex>`.
    pub fn entry(&self, key: &CompileKey) -> PathBuf {
        let hex = key.hex();
        self.entries().join(&hex[..2]).join(hex)
    }
}

pub const MANIFEST: &str = "manifest";
pub const FILES: &str = "files";
pub const LAST_USED: &str = "last-used";
