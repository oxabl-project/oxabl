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
//! # Case folding: two spellings, tried in order
//!
//! An [`IndexName`] is case-folded for *identity* but carries the spelling the
//! source used ([`IndexName::as_written`]), and a path must be derived from the
//! latter: on a case-sensitive filesystem `USING MyApp.Cache` lives in
//! `MyApp/Cache.cls`, and the folded `myapp/cache.cls` simply is not there.
//! Deriving from the folded name alone is why cross-file resolution used to
//! fail on Linux.
//!
//! So [`find_name`] tries the as-written path first and the folded path second.
//! The second attempt is not redundant: an all-lower-case checkout is common
//! (that is what a case-insensitive filesystem's users produce), and a source
//! file is free to spell the same class `MyApp.Cache`. When the two spellings
//! coincide — the source already wrote the name in lower case — the fallback is
//! skipped, so the extra attempt costs nothing in the ordinary case. There is
//! still no directory listing on the
//! [`FileSystem`](oxabl_workspace::FileSystem) trait, so these two candidates
//! are the whole search space; a third casing nobody wrote is not guessed at.
//!
//! # How that composes with exactly-one-match
//!
//! Ambiguity means **two path entries matching the same candidate spelling** —
//! that is the case where the workspace genuinely cannot say which file is
//! meant. If the as-written spelling matches one entry and the folded spelling
//! would match a *different* one, that is not treated as ambiguous: the
//! as-written match wins. The reading is that those are one logical ABL name
//! that two checkouts happen to spell differently on disk, and the spelling the
//! source actually used is the better evidence of which was meant — declining
//! there would make a correct, unambiguous lookup fail because some other
//! directory holds a lower-cased copy. That is also why an `Unknowable` from
//! the first attempt short-circuits rather than falling through: it already
//! found the ambiguity worth declining over.

use std::path::{Path, PathBuf};

use oxabl_semantic::{IndexAnswer, IndexName};
use oxabl_workspace::{FileSystem, is_root_file};

/// Extension of an ABL class file. A qualified class name maps onto a path by
/// replacing dots with separators and appending this — the standard ABL
/// convention, and the reason no separate class-path setting is needed.
const CLASS_EXTENSION: &str = "cls";

/// The relative path a qualified class name maps onto, in the casing the source
/// spelled the name in.
pub fn class_path(name: &IndexName) -> PathBuf {
    class_path_from(name.as_written())
}

/// The same path in the folded casing — the fallback candidate, for a checkout
/// whose files are all lower case.
pub fn folded_class_path(name: &IndexName) -> PathBuf {
    class_path_from(name.as_str())
}

fn class_path_from(qualified: &str) -> PathBuf {
    let mut relative = String::with_capacity(qualified.len() + CLASS_EXTENSION.len() + 1);
    for (i, part) in qualified.split('.').enumerate() {
        if i > 0 {
            relative.push(std::path::MAIN_SEPARATOR);
        }
        relative.push_str(part);
    }
    relative.push('.');
    relative.push_str(CLASS_EXTENSION);
    PathBuf::from(relative)
}

/// The relative path a literal `RUN` target maps onto, in the casing the source
/// spelled it in.
///
/// A `RUN` target is written as a path already (`post-order.p`,
/// `orders/calc-total.p`), so it is taken verbatim; [`find_unique`] is what
/// rejects a spelling that is not a root file.
pub fn program_path(target: &IndexName) -> PathBuf {
    PathBuf::from(target.as_written())
}

/// The same path in the folded casing — the fallback candidate.
pub fn folded_program_path(target: &IndexName) -> PathBuf {
    PathBuf::from(target.as_str())
}

/// Which of the two name-to-path conventions a lookup uses.
///
/// An enum rather than two near-identical search functions so the
/// two-spellings-in-order policy lives in exactly one place and cannot drift
/// between the class path and the `RUN` path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameKind {
    /// Dots become separators and `.cls` is appended.
    Class,
    /// The name is already a path; taken verbatim.
    Program,
}

/// Locate the file `name` names: the as-written spelling first, the folded
/// spelling second when the two differ.
///
/// This is the entry point an index should call. See the module docs for why
/// two spellings are tried, and for what happens when they match different path
/// entries (the as-written match wins; that is not an ambiguity).
pub fn find_name(
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    name: &IndexName,
    kind: NameKind,
) -> IndexAnswer<PathBuf> {
    let written = match kind {
        NameKind::Class => class_path(name),
        NameKind::Program => program_path(name),
    };
    match find_unique(fs, include_paths, &written) {
        IndexAnswer::Found(path) => return IndexAnswer::Found(path),
        // Already ambiguous under the spelling the source used; a second
        // candidate cannot un-ambiguate it.
        IndexAnswer::Unknowable => return IndexAnswer::Unknowable,
        IndexAnswer::NotFound => {}
    }

    let folded = match kind {
        NameKind::Class => folded_class_path(name),
        NameKind::Program => folded_program_path(name),
    };
    if folded == written {
        // The source already wrote the name folded, so the fallback would
        // re-run the identical search.
        return IndexAnswer::NotFound;
    }
    find_unique(fs, include_paths, &folded)
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
        // The primary candidate keeps the source casing — that is what makes the
        // lookup work on a case-sensitive filesystem — and the fallback folds it.
        assert_eq!(
            class_path(&IndexName::new("Orders.Total-Calc")),
            PathBuf::from("Orders").join("Total-Calc.cls")
        );
        assert_eq!(
            folded_class_path(&IndexName::new("Orders.Total-Calc")),
            PathBuf::from("orders").join("total-calc.cls")
        );
        assert_eq!(
            class_path(&IndexName::new("standalone")),
            PathBuf::from("standalone.cls")
        );
    }

    #[test]
    fn a_name_already_written_folded_yields_one_candidate() {
        // Both derivations coincide, which is the case `find_name` detects to
        // skip the redundant second search.
        let name = IndexName::new("orders.total-calc");
        assert_eq!(class_path(&name), folded_class_path(&name));
        assert_eq!(program_path(&name), folded_program_path(&name));
    }

    #[test]
    fn the_as_written_spelling_is_searched_before_the_folded_one() {
        let fs = fs_with(&["/src/MyApp/Cache.cls"]);
        assert_eq!(
            find_name(
                &fs,
                &dirs(&["/src"]),
                &IndexName::new("MyApp.Cache"),
                NameKind::Class
            ),
            IndexAnswer::Found(PathBuf::from("/src/MyApp/Cache.cls"))
        );
    }

    #[test]
    fn a_folded_checkout_resolves_a_mixed_case_source_spelling() {
        // The fallback: the files are all lower case (what a case-insensitive
        // filesystem's users produce) while the source writes mixed case.
        let fs = fs_with(&["/src/myapp/cache.cls", "/src/calc-total.p"]);
        let paths = dirs(&["/src"]);
        assert_eq!(
            find_name(&fs, &paths, &IndexName::new("MyApp.Cache"), NameKind::Class),
            IndexAnswer::Found(PathBuf::from("/src/myapp/cache.cls"))
        );
        assert_eq!(
            find_name(
                &fs,
                &paths,
                &IndexName::new("Calc-Total.p"),
                NameKind::Program
            ),
            IndexAnswer::Found(PathBuf::from("/src/calc-total.p"))
        );
    }

    #[test]
    fn neither_spelling_present_is_not_found() {
        let fs = fs_with(&["/src/myapp/other.cls"]);
        assert_eq!(
            find_name(
                &fs,
                &dirs(&["/src"]),
                &IndexName::new("MyApp.Cache"),
                NameKind::Class
            ),
            IndexAnswer::NotFound
        );
    }

    #[test]
    fn ambiguity_under_the_as_written_spelling_still_declines() {
        // Two entries matching the *same* candidate: the case the one-match rule
        // exists for, and the fallback must not paper over it.
        let fs = fs_with(&["/first/Calc-Total.p", "/second/Calc-Total.p"]);
        assert_eq!(
            find_name(
                &fs,
                &dirs(&["/first", "/second"]),
                &IndexName::new("Calc-Total.p"),
                NameKind::Program
            ),
            IndexAnswer::Unknowable
        );
    }

    #[test]
    fn a_split_across_spellings_resolves_to_the_as_written_match() {
        // The as-written spelling is on one entry and the folded spelling on
        // another. Documented decision: one logical name spelled two ways on
        // disk, so the spelling the source used wins and this is not ambiguous.
        // Ambiguity means two entries matching the same candidate.
        let fs = fs_with(&["/first/MyApp/Cache.cls", "/second/myapp/cache.cls"]);
        assert_eq!(
            find_name(
                &fs,
                &dirs(&["/first", "/second"]),
                &IndexName::new("MyApp.Cache"),
                NameKind::Class
            ),
            IndexAnswer::Found(PathBuf::from("/first/MyApp/Cache.cls"))
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
