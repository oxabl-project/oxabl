//! How a finding is reached from the entry points of a workspace: the
//! status, the entries that reach the routine it is in, and one call path.
//!
//! The `security` rules fill it in when an entry-point file is configured
//! (`[workspace.lint.security.reach]`); output formats render it.

use std::fmt;
use std::path::PathBuf;

/// Whether an entry point reaches a routine.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReachStatus {
    /// An entry point reaches it.
    Reachable,
    /// No entry point reaches it through a known call, but a reached routine
    /// makes a call whose target is not known.
    Unknown,
    /// No entry point reaches it, and every reached call is known.
    Unreachable,
}

impl ReachStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            ReachStatus::Reachable => "reachable",
            ReachStatus::Unknown => "unknown",
            ReachStatus::Unreachable => "unreachable",
        }
    }
}

/// One entry point, as the entry-point file names it.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachEntry {
    /// `REST GET /orders/{id}`.
    pub label: String,
    /// `web/orders.p:get-order`.
    pub target: String,
}

/// One call on a path from an entry point.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachStep {
    /// The calling routine: `web/orders.p` or `web/orders.p:get-order`.
    pub routine: String,
    /// The file of the call, when it is known.
    pub file: Option<PathBuf>,
    /// The 1-based line of the call in that file, when it is known.
    pub line: Option<u32>,
}

/// A path from an entry point to a routine.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachPath {
    pub entry: ReachEntry,
    /// The calls, from the entry.
    pub steps: Vec<ReachStep>,
}

impl ReachPath {
    /// `REST GET /a → web/a.p → lib/b.p:save`, ending at `target`.
    pub fn describe(&self, target: &str) -> String {
        let mut out = self.entry.label.clone();
        for s in &self.steps {
            out.push_str(" → ");
            out.push_str(&s.routine);
        }
        out.push_str(" → ");
        out.push_str(target);
        out
    }
}

/// How a finding is reached.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reachability {
    pub status: ReachStatus,
    /// Entries that reach the routine, at most a few.
    pub entries: Vec<ReachEntry>,
    /// More entries reach it than `entries` lists.
    pub more_entries: bool,
    /// For [`ReachStatus::Unknown`]: why calls of reached routines are not
    /// known (`dynamic`, `method`, ...).
    pub unknown: Vec<String>,
    /// One path from the nearest entry.
    pub path: Option<ReachPath>,
}

impl Reachability {
    /// A status with no entries or path.
    pub fn of(status: ReachStatus, unknown: Vec<String>) -> Self {
        Reachability {
            status,
            entries: Vec::new(),
            more_entries: false,
            unknown,
            path: None,
        }
    }
}

/// The note appended to a finding: `reachable from REST GET /a`,
/// `reachability unknown: dynamic call`, `unreachable from the entry points`.
impl fmt::Display for Reachability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.status {
            ReachStatus::Reachable => {
                let labels: Vec<&str> = self.entries.iter().map(|e| e.label.as_str()).collect();
                write!(f, "reachable from {}", labels.join(", "))?;
                if self.more_entries {
                    f.write_str(" and more")?;
                }
                Ok(())
            }
            ReachStatus::Unknown => {
                f.write_str("reachability unknown")?;
                if !self.unknown.is_empty() {
                    write!(f, ": {} call", self.unknown.join(", "))?;
                }
                Ok(())
            }
            ReachStatus::Unreachable => f.write_str("unreachable from the entry points"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(label: &str) -> ReachEntry {
        ReachEntry {
            label: label.into(),
            target: "web/a.p".into(),
        }
    }

    #[test]
    fn the_note_names_the_entries_or_the_unknown_calls() {
        let mut r = Reachability::of(ReachStatus::Reachable, Vec::new());
        r.entries = vec![entry("REST GET /a"), entry("REST GET /b")];
        assert_eq!(r.to_string(), "reachable from REST GET /a, REST GET /b");
        r.more_entries = true;
        assert_eq!(
            r.to_string(),
            "reachable from REST GET /a, REST GET /b and more"
        );
        let u = Reachability::of(
            ReachStatus::Unknown,
            vec!["dynamic".into(), "method".into()],
        );
        assert_eq!(u.to_string(), "reachability unknown: dynamic, method call");
        let n = Reachability::of(ReachStatus::Unreachable, Vec::new());
        assert_eq!(n.to_string(), "unreachable from the entry points");
    }

    #[test]
    fn a_path_reads_from_the_entry_to_the_target() {
        let p = ReachPath {
            entry: entry("REST GET /a"),
            steps: vec![ReachStep {
                routine: "web/a.p".into(),
                file: None,
                line: None,
            }],
        };
        assert_eq!(
            p.describe("lib/b.p:save"),
            "REST GET /a → web/a.p → lib/b.p:save"
        );
    }
}
