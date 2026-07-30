//! File discovery: the single extension policy for what counts as an ABL
//! *root*, plus the walk primitive every client shares (R8).
//!
//! Before this module existed the CLI and [`Workspace`](crate::Workspace) each
//! carried a private walker, and they disagreed on two axes at once — which
//! extensions are roots, and whether matching is case-sensitive. Two policies
//! meant a file could be analyzed by `oxabl check` and invisible to an
//! `oxabl.toml`-driven [`FileSet`](oxabl_common::FileSet), or the reverse. R8
//! collapses that to one policy, stated once, here.
//!
//! # Why `.i` is never a root (R9)
//!
//! Include fragments are textual splices, not compilation units: they routinely
//! do not parse standalone (a fragment may open a block another file closes, or
//! consist of bare field lists). Walking them as roots manufactures parse
//! errors for files that are perfectly valid in the position they are actually
//! used. They still get analyzed — the preprocessor pulls them in through
//! include resolution, at the site that gives them their meaning.
//!
//! An **explicitly named** path is a different question, and
//! [`discover_path`] answers it differently: naming a file on the command line
//! is a direct instruction, so the path is returned unfiltered, `.i` included.
//! The policy governs what a *walk* volunteers, not what a user demands.
//!
//! # Why this is unconditional (KTD11)
//!
//! `walkdir` is already a plain, non-optional dependency of this crate and the
//! `wasm32-unknown-unknown` target builds with it compiled in, so there is no
//! wasm problem for a feature gate to solve. Gating would instead force a
//! `cfg` on the already-public [`Workspace::from_path`](crate::Workspace::from_path).
//!
//! # Two call shapes, one policy (KTD10)
//!
//! Callers need different shapes: the CLI walks one arbitrary path into a
//! `Vec<PathBuf>`, while `Workspace::from_path` walks each declared
//! `[workspace.sources].directories` entry into a `FileSet`. So the shared
//! pieces are [`is_root_file`] and [`walk_directory`] — a predicate and a
//! walk-one-directory primitive — rather than one top-level function contorted
//! to serve both.

use std::path::{Path, PathBuf};

/// Extensions that make a file an ABL *root* — a unit worth analyzing on its
/// own. Matched case-insensitively by [`is_root_file`]; `.i` is deliberately
/// absent (R9, see the module docs).
pub const ROOT_EXTENSIONS: &[&str] = &["p", "w", "cls", "v"];

/// Whether `path`'s extension makes it an ABL root (R8).
///
/// Case-insensitive: `.CLS` and `.cls` are the same root kind. ABL extensions
/// are ASCII, so folding with [`str::eq_ignore_ascii_case`] avoids the
/// allocation a `to_lowercase()` would cost per candidate on a large tree.
///
/// A path with no extension, or a non-UTF-8 extension, is not a root.
pub fn is_root_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| {
            ROOT_EXTENSIONS
                .iter()
                .any(|known| known.eq_ignore_ascii_case(ext))
        })
}

/// Recursively collect the ABL roots under `dir`, sorted.
///
/// Follows symlinks, and **skips** entries it cannot read rather than failing
/// the walk: one permission-denied subdirectory in a large tree should not cost
/// the caller every other file. Non-files (directories, and anything else the
/// OS reports) are filtered out, so the result is roots only.
///
/// The result is sorted, which makes discovery order deterministic across runs
/// on the same tree — `walkdir` gives no ordering guarantee, and unstable
/// diagnostic order would make CLI output and snapshot tests flaky.
///
/// A `dir` that does not exist (or is not a directory) yields an empty vec;
/// callers that need to tell "absent" from "empty" apart should check first, as
/// [`discover_path`] does.
pub fn walk_directory(dir: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = walkdir::WalkDir::new(dir)
        .follow_links(true)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| is_root_file(entry.path()))
        .map(walkdir::DirEntry::into_path)
        .collect();
    files.sort();
    files
}

/// Resolve one user-supplied path into the set of files to process.
///
/// A **file** path is returned as-is, without extension filtering: naming a
/// file is an explicit instruction, and refusing it because of its extension
/// would make `oxabl check some.i` — or any project using an unconventional
/// suffix — impossible. A **directory** is walked under the root policy
/// ([`walk_directory`]).
///
/// # Errors
///
/// Returns `Err` when the path does not exist, or exists but is neither a file
/// nor a directory. Both are distinct from `Ok(vec![])`, which means "walked a
/// real directory and it held no ABL roots" — callers report those differently
/// (a typo'd path versus an empty source tree).
pub fn discover_path(path: &Path) -> Result<Vec<PathBuf>, String> {
    if !path.exists() {
        return Err(format!("Path does not exist: {}", path.display()));
    }

    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }

    if !path.is_dir() {
        return Err(format!(
            "Path is not a file or directory: {}",
            path.display()
        ));
    }

    Ok(walk_directory(path))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a fixture tree containing every extension the policy has an
    /// opinion about, plus one it should ignore outright.
    fn fixture_tree() -> tempfile::TempDir {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::create_dir_all(root.join("nested")).unwrap();
        for name in [
            "alpha.p",
            "beta.w",
            "Gamma.cls",
            "delta.v",
            "fragment.i",
            "notes.txt",
            "Makefile",
        ] {
            std::fs::write(root.join(name), "").unwrap();
        }
        std::fs::write(root.join("nested/epsilon.p"), "").unwrap();
        std::fs::write(root.join("nested/nested-fragment.i"), "").unwrap();
        tmp
    }

    fn file_names(paths: &[PathBuf]) -> Vec<String> {
        paths
            .iter()
            .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
            .collect()
    }

    #[test]
    fn root_policy_accepts_four_kinds_case_insensitively() {
        for name in ["a.p", "a.P", "a.w", "a.W", "a.cls", "a.CLS", "a.v", "a.V"] {
            assert!(is_root_file(Path::new(name)), "{name} should be a root");
        }
    }

    #[test]
    fn root_policy_rejects_include_fragments_and_non_abl() {
        // R9: `.i` is never a root, in any casing.
        assert!(!is_root_file(Path::new("fragment.i")));
        assert!(!is_root_file(Path::new("fragment.I")));
        assert!(!is_root_file(Path::new("notes.txt")));
        assert!(!is_root_file(Path::new("Makefile")));
        assert!(!is_root_file(Path::new("no_extension")));
    }

    #[test]
    fn walk_returns_only_roots_sorted() {
        let tmp = fixture_tree();
        let found = walk_directory(tmp.path());

        // Only the four root kinds; `.i`, `.txt`, and the extensionless file
        // are absent. Order is path-sorted, so `nested/` sorts after the
        // top-level names.
        assert_eq!(
            file_names(&found),
            vec!["Gamma.cls", "alpha.p", "beta.w", "delta.v", "epsilon.p"]
        );

        let mut sorted = found.clone();
        sorted.sort();
        assert_eq!(found, sorted, "walk_directory must return sorted paths");
    }

    #[test]
    fn walk_discovers_uppercase_extensions() {
        let tmp = tempfile::tempdir().unwrap();
        std::fs::write(tmp.path().join("Upper.P"), "").unwrap();
        std::fs::write(tmp.path().join("Shouty.CLS"), "").unwrap();
        std::fs::write(tmp.path().join("Frag.I"), "").unwrap();

        assert_eq!(
            file_names(&walk_directory(tmp.path())),
            vec!["Shouty.CLS", "Upper.P"]
        );
    }

    #[test]
    fn walk_ordering_is_stable_across_runs() {
        let tmp = fixture_tree();
        let first = walk_directory(tmp.path());
        for _ in 0..3 {
            assert_eq!(walk_directory(tmp.path()), first);
        }
    }

    #[test]
    fn walk_of_empty_directory_is_empty_not_an_error() {
        let tmp = tempfile::tempdir().unwrap();
        assert!(walk_directory(tmp.path()).is_empty());
        assert_eq!(discover_path(tmp.path()), Ok(Vec::new()));
    }

    #[test]
    fn walk_skips_unreadable_subdirectory() {
        // A chmod 000 directory is still readable by root, which would make
        // this test pass without exercising anything. Skip rather than lie.
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            let tmp = tempfile::tempdir().unwrap();
            std::fs::write(tmp.path().join("visible.p"), "").unwrap();
            let locked = tmp.path().join("locked");
            std::fs::create_dir(&locked).unwrap();
            std::fs::write(locked.join("hidden.p"), "").unwrap();
            std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();

            if std::fs::read_dir(&locked).is_ok() {
                // Permissions are not being enforced for this process (root, or
                // an fs that ignores modes). Restore and skip: asserting here
                // would pass without testing the skip path.
                std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
                eprintln!("skipping unreadable-subdirectory case: mode 0o000 is still readable");
                return;
            }

            let found = walk_directory(tmp.path());

            // Restore before the TempDir drop tries to clean up.
            std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();

            assert_eq!(file_names(&found), vec!["visible.p"]);
        }
    }

    #[test]
    fn explicit_file_path_is_returned_unfiltered() {
        let tmp = tempfile::tempdir().unwrap();
        let fragment = tmp.path().join("fragment.i");
        std::fs::write(&fragment, "").unwrap();
        let notes = tmp.path().join("notes.txt");
        std::fs::write(&notes, "").unwrap();

        // Naming a file is an instruction: honored regardless of extension.
        assert_eq!(discover_path(&fragment), Ok(vec![fragment.clone()]));
        assert_eq!(discover_path(&notes), Ok(vec![notes]));

        // The same fragment inside a walked directory is not volunteered (R9).
        assert!(!walk_directory(tmp.path()).contains(&fragment));
    }

    #[test]
    fn missing_path_is_an_error_distinct_from_finding_nothing() {
        let tmp = tempfile::tempdir().unwrap();
        let missing = tmp.path().join("no-such-place");

        let err = discover_path(&missing).unwrap_err();
        assert!(err.contains("does not exist"), "unexpected message: {err}");

        // The empty-but-real directory is the contrasting case.
        assert_eq!(discover_path(tmp.path()), Ok(Vec::new()));
    }

    #[test]
    fn discover_path_walks_a_directory() {
        let tmp = fixture_tree();
        let found = discover_path(tmp.path()).unwrap();
        assert_eq!(found, walk_directory(tmp.path()));
        assert!(!found.is_empty());
    }
}
