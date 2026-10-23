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
//!   "version": 2,
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
//!   The file is resolved through the file system first
//!   (`std::fs::canonicalize`), so `src/sub/../a.p` and `src/a.p` are one file,
//!   and a symlink followed by `..` names the file that is read, not the file
//!   the lexical path suggests. On Windows that also gives the path the case
//!   of the file system, so `SRC\A.P` and `src\a.p` are one file. A file that
//!   does not resolve uses its lexical form, and its root is then matched
//!   without regard to case on Windows. A finding in a file outside the root
//!   has no path: it is reported but never recorded. A file below the root
//!   only through a symlink that points out of it keeps its walk path, unless
//!   the path given has a `..`.
//! - `hash` is a 64-bit FNV-1a hash, in hex, of the flagged line and the first
//!   line of the flagged span, each with whitespace runs collapsed and ASCII
//!   letters folded to lower case, so a change of keyword case (`DEFINE` to
//!   `define`) keeps the match. A finding reported inside an include has no
//!   text in the root file, so its message is hashed instead. A finding in an
//!   include file that was checked as a file of its own (strict rules that are
//!   context-free) is keyed by that include's path like any other file.
//! - `occurrence` numbers findings that share `path`, `code` and `hash`, in
//!   source order, from 0.
//!
//! # A limit of the occurrence number
//!
//! The file has no line numbers, so identical findings in one file (same rule,
//! same text) are told apart only by their order. Add a third identical line
//! *above* two baselined ones and the new line takes occurrence 0, so the
//! finding reported as new is the old last one, at occurrence 2. The count is
//! right; the line may be wrong. A nearest-line match would need line numbers
//! in the file, which would make every edit that moves code change the file.
//! The hash makes the case rare: it needs the same text on both lines.
//!
//! # Versions
//!
//! Version 2 folds the case of the hashed text. A version 1 file hashes it
//! with case kept, so its entries cannot match and are not read: `check`
//! reports an error that names the fix, which is to delete the file and record
//! it again with `--update-baseline`.

use std::collections::{BTreeMap, HashSet};
use std::path::{Component, Path, PathBuf};
use std::sync::Mutex;

use oxabl_analyze::{CollectedDiagnostic, CollectedDiagnostics};
use oxabl_common::Diagnostic;
use oxabl_pipeline::ROOT_FILE_ID;
use serde::{Deserialize, Serialize};

/// The format version this build reads and writes.
const VERSION: u32 = 2;

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
        if file.version == 1 {
            return Err(
                "baseline version 1 hashes the flagged text with its case kept, which \
                 this build no longer matches; delete the file and record it again with \
                 --update-baseline"
                    .to_string(),
            );
        }
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
    /// Findings in files outside the workspace root. They are reported, never
    /// recorded or matched.
    #[serde(skip_serializing_if = "is_zero")]
    pub outside_root: usize,
}

fn is_zero(n: &usize) -> bool {
    *n == 0
}

/// The workspace root for a run over `target`: the directory holding
/// `oxabl.toml`, else the directory of `fallback`, else the target itself.
pub fn workspace_root(target: &Path, fallback: Option<&Path>) -> PathBuf {
    let target = absolute(target);
    let start = if target.is_dir() {
        target.clone()
    } else {
        target.parent().map_or(target.clone(), Path::to_path_buf)
    };
    oxabl_workspace::find_workspace_root(&start).unwrap_or_else(|| {
        fallback
            .and_then(|f| absolute(f).parent().map(Path::to_path_buf))
            .unwrap_or(start)
    })
}

/// One `check` run's use of a baseline file.
///
/// Shared by the run's worker threads: the file and its entries are read-only,
/// and the counts every file adds to sit behind one lock.
pub struct Run {
    file: PathBuf,
    update: bool,
    root: PathBuf,
    baseline: Baseline,
    tally: Mutex<Tally>,
}

#[derive(Default)]
struct Tally {
    checked: HashSet<String>,
    current: Vec<Entry>,
    new: usize,
    baselined: usize,
    outside_root: usize,
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
        let root = workspace_root(target, Some(file));
        Ok(Run {
            file: file.to_path_buf(),
            update,
            root,
            baseline,
            tally: Mutex::new(Tally::default()),
        })
    }

    /// Record the findings of one checked file. Returns those the baseline
    /// does not cover, and those it covers. With `--update-baseline` nothing is
    /// kept: every finding is about to be recorded, and none is covered. A file
    /// outside the root has no baseline path, so all its findings are kept and
    /// none is recorded.
    pub fn split(
        &self,
        file: &Path,
        source: &str,
        mut reported: CollectedDiagnostics,
    ) -> (CollectedDiagnostics, Vec<CollectedDiagnostic>) {
        let Some(path) = relative_path(&self.root, file) else {
            self.tally().outside_root += reported.diagnostics.len();
            return (reported, Vec::new());
        };
        let entries = fingerprint(&path, source, reported.all().map(|c| &c.diagnostic));
        let (kept, covered): (Vec<_>, Vec<_>) = std::mem::take(&mut reported.diagnostics)
            .into_iter()
            .zip(&entries)
            .partition(|(_, e)| !self.baseline.contains(e));
        reported.diagnostics = kept.into_iter().map(|(d, _)| d).collect();
        let covered: Vec<CollectedDiagnostic> = covered.into_iter().map(|(d, _)| d).collect();
        let new = reported.diagnostics.len();
        {
            let mut tally = self.tally();
            tally.new += new;
            tally.baselined += entries.len() - new;
            tally.checked.insert(path);
            tally.current.extend(entries);
        }
        if self.update {
            reported.diagnostics.clear();
            return (reported, Vec::new());
        }
        (reported, covered)
    }

    fn tally(&self) -> std::sync::MutexGuard<'_, Tally> {
        self.tally.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Finish the run: count stale entries and, with `--update-baseline`,
    /// write the file.
    pub fn finish(self, lint_collected: bool) -> Result<Report, String> {
        let tally = self.tally.into_inner().unwrap_or_else(|e| e.into_inner());
        let found: HashSet<Key> = tally.current.iter().map(Entry::key).collect();
        let stale = lint_collected.then(|| self.baseline.stale(&tally.checked, &found));
        if self.update {
            let updated = self.baseline.updated(&tally.checked, &tally.current);
            std::fs::write(&self.file, updated.render())
                .map_err(|e| format!("cannot write baseline {}: {e}", self.file.display()))?;
        }
        Ok(Report {
            path: self.file.display().to_string(),
            new: tally.new,
            baselined: tally.baselined,
            stale,
            updated: self.update,
            outside_root: tally.outside_root,
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

/// A stable identity for an entry: the rule, path and flagged text, plus the
/// occurrence among identical findings. No line number goes in.
pub fn finding_fingerprint(entry: &Entry) -> String {
    let id = format!("{}\0{}\0{}", entry.code, entry.path, entry.hash);
    format!("{:016x}:{}", fnv1a(&id), entry.occurrence)
}

/// The workspace-relative spelling of `file` under `root`, with `/`, or `None`
/// when `file` is not below `root`.
///
/// Both paths go through the file system first, so a symlink and a `..` that
/// follows it name the file that is read, and (on Windows) the case comes from
/// the file system. A file that is below the root only through a symlink that
/// points out of it keeps its walk path, unless that path has a `..`: then the
/// lexical form could name another file, and the file has no key.
pub fn relative_path(root: &Path, file: &Path) -> Option<String> {
    let fold_case = cfg!(windows);
    relative_key(&canonical(root), &canonical(file), fold_case).or_else(|| {
        let walked = !file.components().any(|c| c == Component::ParentDir);
        walked
            .then(|| relative_key(&absolute(root), &absolute(file), fold_case))
            .flatten()
    })
}

fn absolute(path: &Path) -> PathBuf {
    lexical(&std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf()))
}

/// `std::fs::canonicalize`; for a path that does not exist, the deepest
/// existing ancestor canonicalized plus the lexical rest.
fn canonical(path: &Path) -> PathBuf {
    let raw = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    if let Ok(found) = std::fs::canonicalize(&raw) {
        return found;
    }
    let lexical = lexical(&raw);
    let mut rest = Vec::new();
    let mut at = lexical.as_path();
    while let (Some(name), Some(parent)) = (at.file_name(), at.parent()) {
        rest.push(name);
        at = parent;
        if let Ok(mut found) = std::fs::canonicalize(at) {
            found.extend(rest.iter().rev());
            return found;
        }
    }
    lexical
}

/// The part of `file` below `root`, both already resolved. `fold_case` makes
/// the match ignore case, and the key then takes its case from `file`.
fn relative_key(root: &Path, file: &Path, fold_case: bool) -> Option<String> {
    let same = |a: Component, b: Component| {
        a == b
            || fold_case
                && a.as_os_str().to_string_lossy().to_lowercase()
                    == b.as_os_str().to_string_lossy().to_lowercase()
    };
    let mut rest = file.components();
    for want in root.components() {
        if !rest.next().is_some_and(|got| same(want, got)) {
            return None;
        }
    }
    let parts: Vec<_> = rest.map(|c| c.as_os_str().to_string_lossy()).collect();
    (!parts.is_empty()).then(|| parts.join("/"))
}

/// `path` with `.` removed and `..` resolved against the component before it,
/// without touching the file system. A `..` that has nothing to climb out of
/// stays in a relative path and is dropped at the root.
fn lexical(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => match out.components().next_back() {
                Some(Component::Normal(_)) => {
                    out.pop();
                }
                Some(Component::RootDir | Component::Prefix(_)) => {}
                _ => out.push(".."),
            },
            other => out.push(other.as_os_str()),
        }
    }
    out
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

/// Whitespace runs collapsed and ASCII letters folded: ABL keywords are
/// case-insensitive, and the formatter can change their case.
fn normalize(text: &str) -> String {
    text.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_lowercase()
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
        assert!(Baseline::parse("{\"version\": 3, \"entries\": []}").is_err());
        assert!(Baseline::parse("[]").is_err());
    }

    #[test]
    fn relative_paths_use_forward_slashes() {
        assert_eq!(
            relative_path(Path::new("/ws"), Path::new("/ws/src/a.p")).as_deref(),
            Some("src/a.p")
        );
    }

    #[test]
    fn dot_and_dot_dot_are_resolved_before_the_root_is_removed() {
        for file in [
            "/ws/src/sub/../a.p",
            "/ws/./src/a.p",
            "/ws/src/../src/./a.p",
            "/ws/../ws/src/a.p",
        ] {
            assert_eq!(
                relative_path(Path::new("/ws"), Path::new(file)).as_deref(),
                Some("src/a.p"),
                "{file}"
            );
        }
        assert_eq!(
            relative_path(Path::new("/ws/./x/.."), Path::new("/ws/a.p")).as_deref(),
            Some("a.p")
        );
    }

    #[test]
    fn a_file_outside_the_root_has_no_path() {
        for file in [
            "/other/a.p",
            "/ws/../other/a.p",
            "/ws",
            "/wsx/a.p",
            "/ws/..",
        ] {
            assert_eq!(
                relative_path(Path::new("/ws"), Path::new(file)),
                None,
                "{file}"
            );
        }
    }

    #[cfg(unix)]
    mod symlinks {
        use super::*;
        use std::os::unix::fs::symlink;

        /// `root/src/{a.p,link -> outside/x}`, `outside/{a.p,x/b.p}`.
        fn tree() -> tempfile::TempDir {
            let dir = tempfile::tempdir().unwrap();
            let t = dir.path();
            for sub in ["root/src", "outside/x"] {
                std::fs::create_dir_all(t.join(sub)).unwrap();
            }
            for file in ["root/src/a.p", "outside/a.p", "outside/x/b.p"] {
                std::fs::write(t.join(file), "").unwrap();
            }
            symlink(t.join("outside/x"), t.join("root/src/link")).unwrap();
            dir
        }

        #[test]
        fn dot_dot_after_a_symlink_names_the_file_that_is_read() {
            let dir = tree();
            let root = dir.path().join("root");
            // `src/link/../a.p` is `outside/a.p`, not `src/a.p`.
            assert_eq!(
                relative_path(&root, &root.join("src/link/../a.p")),
                None,
                "the file is outside the root"
            );
            assert_eq!(
                relative_path(&root, &root.join("src/a.p")).as_deref(),
                Some("src/a.p")
            );
        }

        #[test]
        fn two_spellings_through_symlinks_give_one_key() {
            let dir = tree();
            let t = dir.path();
            symlink(t.join("root/src"), t.join("root/alias")).unwrap();
            symlink(t.join("root"), t.join("root-link")).unwrap();
            let root = t.join("root");
            for (root, file) in [
                (root.clone(), root.join("alias/a.p")),
                (t.join("root-link"), root.join("src/a.p")),
                (root.clone(), t.join("root-link/src/a.p")),
                (root.clone(), root.join("alias/../src/a.p")),
            ] {
                assert_eq!(
                    relative_path(&root, &file).as_deref(),
                    Some("src/a.p"),
                    "{}",
                    file.display()
                );
            }
        }

        #[test]
        fn a_walked_file_in_a_linked_directory_keeps_its_walk_path() {
            let dir = tree();
            let root = dir.path().join("root");
            assert_eq!(
                relative_path(&root, &root.join("src/link/b.p")).as_deref(),
                Some("src/link/b.p")
            );
            // The same file reached with a `..` has no safe lexical form.
            assert_eq!(
                relative_path(&root, &root.join("src/./link/../x/b.p")),
                None
            );
        }
    }

    #[test]
    fn the_root_match_folds_case_only_when_asked() {
        let (root, file) = (Path::new("/WS/Src"), Path::new("/ws/src/Sub/A.p"));
        assert_eq!(relative_key(root, file, false), None);
        assert_eq!(relative_key(root, file, true).as_deref(), Some("Sub/A.p"));
    }

    #[test]
    fn a_fingerprint_survives_a_keyword_case_change() {
        let upper = "DEFINE VARIABLE c AS CHARACTER.\nRUN VALUE(c).\n";
        let lower = "define variable c as character.\nrun value(c).\n";
        let before = fingerprint("a.p", upper, &[diag("CL001", upper, "RUN VALUE", 0)]);
        let after = fingerprint("a.p", lower, &[diag("CL001", lower, "run value", 0)]);
        assert_eq!(before[0].hash, after[0].hash);
        assert_eq!(before[0].key(), after[0].key());
    }

    #[test]
    fn a_version_1_file_is_refused_with_the_fix() {
        let err = Baseline::parse("{\"version\": 1, \"entries\": []}").unwrap_err();
        assert!(err.contains("--update-baseline"), "{err}");
    }

    // Two baselined copies, then a third identical line above them: the count
    // is right, and the reported finding is the last one, not the new one.
    #[test]
    fn identical_findings_match_by_order_so_a_line_added_above_shifts_the_report() {
        let old = "x = 1.\nx = 1.\n";
        let new = "x = 1.\nx = 1.\nx = 1.\n";
        let recorded = fingerprint(
            "a.p",
            old,
            &[diag("LINT0006", old, "x", 0), diag("LINT0006", old, "x", 1)],
        );
        let baseline = Baseline::default().updated(&HashSet::new(), &recorded);
        let now = fingerprint(
            "a.p",
            new,
            &(0..3)
                .map(|n| diag("LINT0006", new, "x", n))
                .collect::<Vec<_>>(),
        );
        let fresh: Vec<usize> = now
            .iter()
            .enumerate()
            .filter(|(_, e)| !baseline.contains(e))
            .map(|(i, _)| i)
            .collect();
        assert_eq!(fresh, [2], "one new finding, reported at the last copy");
    }
}
