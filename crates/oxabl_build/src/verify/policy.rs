//! The verification of a build: one value, from the cache mode and the
//! `[build.verify]` table, that every check consults.
//!
//! A `ci` build writes what other people read, so it runs every check at full
//! strength and no setting may lower one. A setting that would is refused
//! here with a usage error while the build still writes a cache. Turning the
//! read check off lowers nothing: no set is written then.

use oxabl_workspace::BuildVerify;
use serde_json::{Value, json};

use crate::cache::Mode;
use crate::error::BuildError;

/// What a build checks before a cache stores a set.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Policy {
    pub mode: Mode,
    /// The private `XREF-XML` is read (V3). Without it no set is written.
    pub reads: bool,
    /// The pace of the schema checks (V2): `[build.verify].imaging_budget`.
    pub imaging_budget: u32,
}

impl Policy {
    /// The policy of a build in `mode` with the `[build.verify]` settings
    /// `verify`; a usage error when `mode` cannot run with them.
    pub fn resolve(mode: Mode, verify: &BuildVerify) -> Result<Policy, BuildError> {
        Ok(Policy {
            mode,
            reads: verify.reads,
            imaging_budget: verify.imaging_budget,
        })
    }

    /// Whether this build verifies its units: a cache stores sets, the read
    /// check is on and the compiler writes the `XREF-XML` that it reads.
    /// `notes` gets the reason when sets are withheld.
    pub fn verifies(&self, writes: bool, text_xref: bool, notes: &mut Vec<String>) -> bool {
        if !writes {
            return false;
        }
        if text_xref {
            notes.push(
                "no cache entries are written: [build.outputs].xref_format = \"text\" gives no compiler-resolved file names to check"
                    .to_string(),
            );
            return false;
        }
        if !self.reads {
            notes.push(
                "no cache entries are written: [build.verify].reads = false turns the read check off"
                    .to_string(),
            );
            return false;
        }
        true
    }

    /// The policy as `(name, value)` pairs, for `--print-config` and the report.
    pub fn entries(&self) -> Vec<(&'static str, Value)> {
        vec![
            ("mode", json!(self.mode.name())),
            ("reads", json!(self.reads)),
            ("imaging_budget", json!(self.imaging_budget)),
        ]
    }
}
