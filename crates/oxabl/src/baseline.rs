//! `oxabl check --baseline`: record existing findings so a gate fails only on
//! new ones.
//!
//! # File format
//!
//! Pretty-printed JSON, entries sorted, one key per line, so a baseline diffs
//! cleanly in review:
//!
//! ```json
//! {
//!   "version": 1,
//!   "entries": [
//!     {
//!       "path": "src/orders.p",
//!       "code": "LINT0002",
//!       "hash": "5f0c3a5e9d2b7c11",
//!       "occurrence": 0,
//!       "message": "unused variable `v-total`"
//!     }
//!   ]
//! }
//! ```
//!
//! An entry matches a finding by `path`, `code`, `hash` and `occurrence`;
//! `message` is only there for a reader. No line number is stored, so an edit
//! that moves a finding to another line does not unmatch it.
//!
//! - `path` is relative to the workspace root (the directory holding
//!   `oxabl.toml`, else the directory holding the baseline file), with `/`.
//! - `hash` is a 64-bit FNV-1a hash, in hex, of the flagged line and the first
//!   line of the flagged span, each with whitespace runs collapsed. A finding
//!   reported inside an include has no text in the root file, so its message is
//!   hashed instead.
//! - `occurrence` numbers findings that share `path`, `code` and `hash`, in
//!   source order, from 0.

use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};

use oxabl_analyze::CollectedDiagnostics;
use oxabl_common::Diagnostic;
use oxabl_pipeline::ROOT_FILE_ID;
use serde::{Deserialize, Serialize};

/// The format version this build reads and writes.
const VERSION: u32 = 1;

/// One recorded finding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Entry {
    pub path: String,
    pub code: String,
    pub hash: String,
    pub occurrence: u32,
    #[serde(default)]
    pub message: String,
}

impl Entry {
    /// What a finding is matched on.
    pub fn key(&self) -> Key {
        (
            self.path.clone(),
            self.code.clone(),
            self.hash.clone(),
            self.occurrence,
        )
    }
}

/// `(path, code, hash, occurrence)`.
pub type Key = (String, String, String, u32);

#[derive(Serialize, Deserialize)]
struct BaselineFile {
    version: u32,
    entries: Vec<Entry>,
}

/// A loaded baseline: key → message.
#[derive(Debug, Default)]
pub struct Baseline {
    entries: BTreeMap<Key, String>,
}

impl Baseline {
    /// Parse a baseline document.
    pub fn parse(text: &str) -> Result<Self, String> {
        let file: BaselineFile =
            serde_json::from_str(text).map_err(|e| format!("not a baseline file: {e}"))?;
        if file.version != VERSION {
            return Err(format!(
                "baseline version {} is not supported (expected {VERSION})",
                file.version
            ));
        }
        Ok(Baseline {
            entries: file
                .entries
                .into_iter()
                .map(|e| (e.key(), e.message))
                .collect(),
        })
    }

    /// The document for these entries, sorted, with a trailing newline.
    pub fn render(&self) -> String {
        let file = BaselineFile {
            version: VERSION,
            entries: self
                .entries
                .iter()
                .map(|((path, code, hash, occurrence), message)| Entry {
                    path: path.clone(),
                    code: code.clone(),
                    hash: hash.clone(),
                    occurrence: *occurrence,
                    message: message.clone(),
                })
                .collect(),
        };
        let mut text = serde_json::to_string_pretty(&file).expect("a baseline serializes");
        text.push('\n');
        text
    }

    /// Whether `entry` is recorded.
    pub fn contains(&self, entry: &Entry) -> bool {
        self.entries.contains_key(&entry.key())
    }

    /// Entries for the paths in `checked` that no finding in `found` matches:
    /// violations that have since been fixed.
    pub fn stale(&self, checked: &HashSet<String>, found: &HashSet<Key>) -> usize {
        self.entries
            .keys()
            .filter(|k| checked.contains(&k.0) && !found.contains(*k))
            .count()
    }

    /// This baseline with every entry for a path in `checked` replaced by
    /// `current`. Entries for files this run did not check are kept, so a run
    /// over part of a tree does not drop the rest.
    pub fn updated(&self, checked: &HashSet<String>, current: &[Entry]) -> Baseline {
        let mut entries: BTreeMap<Key, String> = self
            .entries
            .iter()
            .filter(|(k, _)| !checked.contains(&k.0))
            .map(|(k, m)| (k.clone(), m.clone()))
            .collect();
        entries.extend(current.iter().map(|e| (e.key(), e.message.clone())));
        Baseline { entries }
    }
}

/// The counts a baselined `check` reports, in the JSON report's `baseline` key.
#[derive(Debug, Serialize)]
pub struct Report {
    /// The baseline file, as given.
    pub path: String,
    /// Findings the baseline does not record. With `--update-baseline`, the
    /// entries added.
    pub new: usize,
    /// Findings the baseline records, so not reported.
    pub baselined: usize,
    /// Entries for checked files that no finding matched: fixed violations.
    /// With `--update-baseline`, the entries removed. `None` under `--no-lint`,
    /// where lint findings are not collected and so cannot be matched.
    pub stale: Option<usize>,
    /// Whether this run rewrote the file.
    pub updated: bool,
}

/// One `check` run's use of a baseline file.
pub struct Run {
    file: PathBuf,
    update: bool,
    root: PathBuf,
    baseline: Baseline,
    checked: HashSet<String>,
    current: Vec<Entry>,
    new: usize,
    baselined: usize,
}

impl Run {
    /// Load `file` for a run over `target`. With `update`, a missing file is
    /// an empty baseline.
    pub fn open(file: &Path, update: bool, target: &Path) -> Result<Run, String> {
        let baseline = match std::fs::read_to_string(file) {
            Ok(text) => Baseline::parse(&text)
                .map_err(|e| format!("cannot use baseline {}: {e}", file.display()))?,
            Err(e) if update && e.kind() == std::io::ErrorKind::NotFound => Baseline::default(),
            Err(e) => return Err(format!("cannot read baseline {}: {e}", file.display())),
        };
        let absolute = |p: &Path| std::path::absolute(p).unwrap_or_else(|_| p.to_path_buf());
        let target = absolute(target);
        let start = if target.is_dir() {
            target.clone()
        } else {
            target.parent().map_or(target.clone(), Path::to_path_buf)
        };
        let root = oxabl_workspace::find_workspace_root(&start).unwrap_or_else(|| {
            absolute(file)
                .parent()
                .map_or_else(|| start.clone(), Path::to_path_buf)
        });
        Ok(Run {
            file: file.to_path_buf(),
            update,
            root,
            baseline,
            checked: HashSet::new(),
            current: Vec::new(),
            new: 0,
            baselined: 0,
        })
    }

    /// Record the findings of one checked file and keep only those the
    /// baseline does not cover. With `--update-baseline` nothing is kept: every
    /// finding is about to be recorded.
    pub fn filter(
        &mut self,
        file: &Path,
        source: &str,
        mut reported: CollectedDiagnostics,
    ) -> CollectedDiagnostics {
        let path = relative_path(&self.root, file);
        let entries = fingerprint(&path, source, reported.all().map(|c| &c.diagnostic));
        let mut keep = entries.iter().map(|e| !self.baseline.contains(e));
        reported.diagnostics.retain(|_| keep.next().unwrap_or(true));
        let new = reported.diagnostics.len();
        self.new += new;
        self.baselined += entries.len() - new;
        self.checked.insert(path);
        self.current.extend(entries);
        if self.update {
            reported.diagnostics.clear();
        }
        reported
    }

    /// Finish the run: count stale entries and, with `--update-baseline`,
    /// write the file.
    pub fn finish(self, lint_collected: bool) -> Result<Report, String> {
        let found: HashSet<Key> = self.current.iter().map(Entry::key).collect();
        let stale = lint_collected.then(|| self.baseline.stale(&self.checked, &found));
        if self.update {
            let updated = self.baseline.updated(&self.checked, &self.current);
            std::fs::write(&self.file, updated.render())
                .map_err(|e| format!("cannot write baseline {}: {e}", self.file.display()))?;
        }
        Ok(Report {
            path: self.file.display().to_string(),
            new: self.new,
            baselined: self.baselined,
            stale,
            updated: self.update,
        })
    }
}

/// One entry per diagnostic, in the order given, for the file at `path` with
/// text `source`.
pub fn fingerprint<'a>(
    path: &str,
    source: &str,
    diagnostics: impl IntoIterator<Item = &'a Diagnostic>,
) -> Vec<Entry> {
    let diagnostics: Vec<&Diagnostic> = diagnostics.into_iter().collect();
    let mut order: Vec<usize> = (0..diagnostics.len()).collect();
    // Occurrences count in source order, whatever order the rules reported in.
    order.sort_by_key(|&i| {
        let d = diagnostics[i];
        (
            d.span.file != ROOT_FILE_ID,
            d.span.span.start,
            d.span.span.end,
        )
    });
    let mut entries: Vec<Option<Entry>> = vec![None; diagnostics.len()];
    let mut seen: Vec<(String, String, u32)> = Vec::new();
    for i in order {
        let d = diagnostics[i];
        let code = d.code.0.to_string();
        let hash = format!("{:016x}", fnv1a(&content(source, d)));
        let occurrence = match seen.iter_mut().find(|(c, h, _)| *c == code && *h == hash) {
            Some(slot) => {
                slot.2 += 1;
                slot.2
            }
            None => {
                seen.push((code.clone(), hash.clone(), 0));
                0
            }
        };
        entries[i] = Some(Entry {
            path: path.to_string(),
            code,
            hash,
            occurrence,
            message: d.message.clone(),
        });
    }
    entries
        .into_iter()
        .map(|e| e.expect("every index is filled"))
        .collect()
}

/// The workspace-relative spelling of `file` under `root`, with `/`.
pub fn relative_path(root: &Path, file: &Path) -> String {
    let relative = file.strip_prefix(root).unwrap_or(file);
    relative
        .components()
        .map(|c| c.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

/// The text a diagnostic's hash is taken over.
fn content(source: &str, d: &Diagnostic) -> String {
    if d.span.file != ROOT_FILE_ID {
        return normalize(&d.message);
    }
    let start = (d.span.span.start as usize).min(source.len());
    let end = (d.span.span.end as usize).clamp(start, source.len());
    let line_start = source[..start].rfind('\n').map_or(0, |i| i + 1);
    let line_end = source[start..]
        .find('\n')
        .map_or(source.len(), |i| start + i);
    let line = source.get(line_start..line_end).unwrap_or_default();
    let flagged = source.get(start..end).unwrap_or_default();
    let flagged = flagged.lines().next().unwrap_or_default();
    format!("{}\0{}", normalize(line), normalize(flagged))
}

fn normalize(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// 64-bit FNV-1a: small, and stable across platforms and Rust versions.
fn fnv1a(text: &str) -> u64 {
    text.bytes().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01b3)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::Span;
    use oxabl_common::FileSpan;

    fn diag(code: &'static str, source: &str, needle: &str, nth: usize) -> Diagnostic {
        let start = source.match_indices(needle).nth(nth).unwrap().0 as u32;
        Diagnostic::warning(
            code,
            format!("about {needle}"),
            FileSpan::from((
                ROOT_FILE_ID,
                Span {
                    start,
                    end: start + needle.len() as u32,
                },
            )),
        )
    }

    const BEFORE: &str =
        "DEFINE VARIABLE a AS INTEGER NO-UNDO.\nDEFINE VARIABLE b AS INTEGER NO-UNDO.\n";
    const AFTER: &str = "/* header */\n\nDEFINE VARIABLE b  AS INTEGER NO-UNDO.\nDEFINE VARIABLE a AS INTEGER NO-UNDO.\n";

    #[test]
    fn a_fingerprint_survives_moved_lines_and_whitespace() {
        let before = fingerprint(
            "src/a.p",
            BEFORE,
            &[
                diag("LINT0002", BEFORE, "a AS", 0),
                diag("LINT0002", BEFORE, "b ", 0),
            ],
        );
        let after = fingerprint(
            "src/a.p",
            AFTER,
            &[
                diag("LINT0002", AFTER, "b ", 0),
                diag("LINT0002", AFTER, "a AS", 0),
            ],
        );
        assert_eq!(before[0], after[1]);
        assert_eq!(before[1], after[0]);
        assert_ne!(before[0].hash, before[1].hash);
    }

    #[test]
    fn identical_findings_get_successive_occurrences_in_source_order() {
        let source = "x = 1.\nx = 1.\nx = 1.\n";
        let d = |n| diag("LINT0006", source, "x", n);
        let entries = fingerprint("a.p", source, &[d(2), d(0), d(1)]);
        let occurrences: Vec<u32> = entries.iter().map(|e| e.occurrence).collect();
        assert_eq!(occurrences, [2, 0, 1]);
        assert!(entries.iter().all(|e| e.hash == entries[0].hash));
    }

    #[test]
    fn an_edit_on_the_flagged_line_unmatches_it() {
        let changed = BEFORE.replace("a AS INTEGER", "a AS DECIMAL");
        let before = fingerprint("a.p", BEFORE, &[diag("LINT0002", BEFORE, "a AS", 0)]);
        let after = fingerprint("a.p", &changed, &[diag("LINT0002", &changed, "a AS", 0)]);
        assert_ne!(before[0].hash, after[0].hash);
    }

    #[test]
    fn a_baseline_round_trips_sorted() {
        let entries = fingerprint(
            "b.p",
            BEFORE,
            &[
                diag("LINT0002", BEFORE, "b ", 0),
                diag("LINT0001", BEFORE, "a AS", 0),
            ],
        );
        let baseline = Baseline::default().updated(&HashSet::new(), &entries);
        let text = baseline.render();
        let parsed = Baseline::parse(&text).unwrap();
        assert_eq!(parsed.render(), text);
        assert_eq!(parsed.entries.len(), 2);
        let codes: Vec<_> = parsed.entries.keys().map(|k| k.1.as_str()).collect();
        assert_eq!(codes, ["LINT0001", "LINT0002"]);
        assert!(text.ends_with("}\n"));
    }

    #[test]
    fn matching_ignores_the_message() {
        let mut entry = fingerprint("a.p", BEFORE, &[diag("LINT0002", BEFORE, "b ", 0)]).remove(0);
        let baseline = Baseline::default().updated(&HashSet::new(), std::slice::from_ref(&entry));
        entry.message = "reworded".to_string();
        assert!(baseline.contains(&entry));
    }

    #[test]
    fn an_update_keeps_entries_for_files_not_checked_and_stale_counts_only_checked() {
        let a = fingerprint("a.p", BEFORE, &[diag("LINT0002", BEFORE, "a AS", 0)]);
        let b = fingerprint("b.p", BEFORE, &[diag("LINT0002", BEFORE, "b ", 0)]);
        let all: Vec<Entry> = a.iter().chain(&b).cloned().collect();
        let baseline = Baseline::default().updated(&HashSet::new(), &all);

        let checked: HashSet<String> = ["a.p".to_string()].into();
        assert_eq!(
            baseline.stale(&checked, &HashSet::new()),
            1,
            "a.p was fixed"
        );
        let found: HashSet<Key> = a.iter().map(Entry::key).collect();
        assert_eq!(baseline.stale(&checked, &found), 0);

        let shrunk = baseline.updated(&checked, &[]);
        assert_eq!(shrunk.entries.len(), 1, "b.p was not checked, so it stays");
        assert!(shrunk.contains(&b[0]));
    }

    #[test]
    fn a_wrong_version_or_shape_is_an_error() {
        assert!(Baseline::parse("{\"version\": 2, \"entries\": []}").is_err());
        assert!(Baseline::parse("[]").is_err());
    }

    #[test]
    fn relative_paths_use_forward_slashes() {
        assert_eq!(
            relative_path(Path::new("/ws"), Path::new("/ws/src/a.p")),
            "src/a.p"
        );
    }
}
