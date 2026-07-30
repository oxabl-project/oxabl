//! Name-to-path search over the configured include paths.
//!
//! Reuses the workspace's existing policy for *what* a candidate may be — the
//! root extension set of [`is_root_file`], where `.i` is never a root — and the
//! configured path list, but **not**
//! [`FileSystem::resolve_include`](oxabl_workspace::FileSystem::resolve_include):
//! its first-match-wins contract is the wrong answer here.
//!
//! # Why exactly-one-match, not first-match-wins
//!
//! An include splice is textual and the AVM's PROPATH genuinely shadows: the
//! first `{shared.i}` on the path is *the* file, and picking it is correct. A
//! `RUN` link is a different question — it decides which file's declarations a
//! call site is attributed to, across the whole program graph. Two path entries
//! carrying the same program name means the workspace cannot tell us which one
//! is meant, and a wrong link is poison where a missing one is merely quiet:
//! downstream, a conservative edge is useful and a wrong edge corrupts every
//! answer derived from it. So this search checks **every** entry and declines
//! ([`IndexAnswer::Unknowable`]) rather than guessing.
//!
//! # Case folding
//!
//! An [`IndexName`] is case-folded by construction, so the candidate path is
//! spelled in lower case. On a case-insensitive filesystem (where ABL
//! development overwhelmingly happens) that is exactly right. On a
//! case-sensitive one it means a class file must be named as the folded name
//! spells it — `myapp/cache.cls`, not `MyApp/Cache.cls`. The seam hands
//! implementations a folded name and nothing else, and the
//! [`FileSystem`](oxabl_workspace::FileSystem) trait offers no directory
//! listing, so there is no unfolded spelling available here to try. Recovering
//! the original casing is a seam-level change, not a search-level one.

use std::path::{Path, PathBuf};

use oxabl_semantic::{IndexAnswer, IndexName};
use oxabl_workspace::{FileSystem, is_root_file};

/// Extension of an ABL class file. A qualified class name maps onto a path by
/// replacing dots with separators and appending this — the standard ABL
/// convention, and the reason no separate class-path setting is needed.
const CLASS_EXTENSION: &str = "cls";

/// The relative path a qualified class name maps onto.
pub fn class_path(name: &IndexName) -> PathBuf {
    let folded = name.as_str();
    let mut relative = String::with_capacity(folded.len() + CLASS_EXTENSION.len() + 1);
    for (i, part) in folded.split('.').enumerate() {
        if i > 0 {
            relative.push(std::path::MAIN_SEPARATOR);
        }
        relative.push_str(part);
    }
    relative.push('.');
    relative.push_str(CLASS_EXTENSION);
    PathBuf::from(relative)
}

/// The relative path a literal `RUN` target maps onto.
///
/// A `RUN` target is written as a path already (`post-order.p`,
/// `orders/calc-total.p`), so it is taken verbatim; [`find_unique`] is what
/// rejects a spelling that is not a root file.
pub fn program_path(target: &IndexName) -> PathBuf {
    PathBuf::from(target.as_str())
}

/// Search every entry of `include_paths` for `relative`, insisting on exactly
/// one match.
///
/// - no match, or a `relative` that is not a root file → [`IndexAnswer::NotFound`]
/// - exactly one match → [`IndexAnswer::Found`]
/// - two entries carrying distinct matching files → [`IndexAnswer::Unknowable`]
///
/// Two entries that resolve to the *same* path (a duplicated PROPATH entry) are
/// one match, not an ambiguity: there is only one file, so nothing is undecided.
///
/// `dir.join(relative)` follows the same absolute-vs-relative rule the include
/// resolver documents — an absolute `relative` replaces the directory — and
/// there is no implicit current directory, so a caller that wants `.` searched
/// puts it on the path list.
pub fn find_unique(
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    relative: &Path,
) -> IndexAnswer<PathBuf> {
    // The extension policy, applied before any I/O: `.i` is never a root, so no
    // amount of searching should turn an include fragment into a class or a
    // program.
    if !is_root_file(relative) {
        return IndexAnswer::NotFound;
    }

    let mut found: Option<PathBuf> = None;
    for dir in include_paths {
        let candidate = dir.join(relative);
        if !fs.exists(&candidate) {
            continue;
        }
        match &found {
            None => found = Some(candidate),
            Some(first) if *first == candidate => {}
            Some(_) => return IndexAnswer::Unknowable,
        }
    }
    found.map_or(IndexAnswer::NotFound, IndexAnswer::Found)
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_workspace::InMemoryFileSystem;

    fn fs_with(paths: &[&str]) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for path in paths {
            fs.insert(PathBuf::from(path), "/* fixture */");
        }
        fs
    }

    fn dirs(entries: &[&str]) -> Vec<PathBuf> {
        entries.iter().map(PathBuf::from).collect()
    }

    #[test]
    fn a_qualified_class_name_maps_onto_a_cls_path() {
        assert_eq!(
            class_path(&IndexName::new("Orders.Total-Calc")),
            PathBuf::from("orders").join("total-calc.cls")
        );
        assert_eq!(
            class_path(&IndexName::new("standalone")),
            PathBuf::from("standalone.cls")
        );
    }

    #[test]
    fn a_class_with_no_file_on_the_paths_is_not_found() {
        let fs = fs_with(&["/src/orders/total-calc.cls"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src"]),
                &class_path(&IndexName::new("orders.missing"))
            ),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn the_second_of_three_entries_is_searched() {
        // Order is *searched*, not assumed: a hit on entry two proves the loop
        // does not stop at the first entry.
        let fs = fs_with(&["/second/orders/total-calc.cls"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/first", "/second", "/third"]),
                &class_path(&IndexName::new("orders.total-calc"))
            ),
            IndexAnswer::Found(PathBuf::from("/second/orders/total-calc.cls"))
        );
    }

    #[test]
    fn a_program_on_one_path_entry_resolves() {
        let fs = fs_with(&["/src/orders/calc-total.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/other", "/src"]),
                &program_path(&IndexName::new("orders/calc-total.p"))
            ),
            IndexAnswer::Found(PathBuf::from("/src/orders/calc-total.p"))
        );
    }

    #[test]
    fn a_program_on_two_entries_is_unknowable_not_the_first_match() {
        let fs = fs_with(&["/first/calc-total.p", "/second/calc-total.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/first", "/second"]),
                &program_path(&IndexName::new("calc-total.p"))
            ),
            IndexAnswer::Unknowable
        );
    }

    #[test]
    fn a_duplicated_path_entry_is_one_match_not_an_ambiguity() {
        let fs = fs_with(&["/src/calc-total.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src", "/src"]),
                &program_path(&IndexName::new("calc-total.p"))
            ),
            IndexAnswer::Found(PathBuf::from("/src/calc-total.p"))
        );
    }

    #[test]
    fn a_program_with_no_match_is_not_found() {
        let fs = fs_with(&["/src/calc-total.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src"]),
                &program_path(&IndexName::new("post-order.p"))
            ),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn an_include_extension_is_never_a_root_file() {
        // The file is right there on the path; the extension policy is what
        // declines it (R9).
        let fs = fs_with(&["/src/shared.i"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src"]),
                &program_path(&IndexName::new("shared.i"))
            ),
            IndexAnswer::NotFound
        );
    }
}
