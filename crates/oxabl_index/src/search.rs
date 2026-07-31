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

use std::path::{Component, Path, PathBuf};

use oxabl_semantic::{IndexAnswer, IndexName};
use oxabl_workspace::{FileSystem, is_root_file};

/// Resolve `.` and `..` segments in `path` **textually**, leaving everything else
/// byte-identical.
///
/// Deliberately *not* [`std::fs::canonicalize`]. That touches the real
/// filesystem — it stats every component and follows symlinks — which this crate
/// cannot do: every lookup goes through the
/// [`FileSystem`](oxabl_workspace::FileSystem) abstraction, whose two
/// implementations are a real-filesystem adapter and an in-memory map. The
/// in-memory one is what the tests and the browser client run on, and there is
/// nothing on disk for `canonicalize` to resolve against. A lexical normalization
/// is also the *right* answer for the two jobs below, both of which are about the
/// spelling of a key rather than about disk identity.
///
/// A leading `..` that has nothing to pop is **kept**, which is what makes
/// "this path escapes its base" observable to a caller — see [`find_unique`].
/// After a root, `..` is dropped, matching POSIX's treatment of `/..` as `/`.
pub fn normalize_lexically(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    // Tracks how many trailing components are poppable `Normal` ones. A `..`
    // cannot pop a `RootDir`, a `Prefix`, or another `..`.
    let mut poppable = 0usize;
    let mut rooted = false;
    for component in path.components() {
        match component {
            Component::Prefix(_) => out.push(component.as_os_str()),
            Component::RootDir => {
                rooted = true;
                out.push(component.as_os_str());
            }
            // `a/./b` and `a/b` name the same file, so `.` contributes nothing.
            Component::CurDir => {}
            Component::ParentDir => {
                if poppable > 0 {
                    out.pop();
                    poppable -= 1;
                } else if !rooted {
                    // Nothing to pop and no root to absorb it: the path really
                    // does reach outside its base, and the `..` has to survive so
                    // a caller can see that.
                    out.push("..");
                }
            }
            Component::Normal(part) => {
                out.push(part);
                poppable += 1;
            }
        }
    }
    out
}

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
    let policy = match kind {
        // A derived class path always ends in `.cls`, so either policy admits it;
        // naming the stricter one keeps the derivation honest.
        NameKind::Class => ExtensionPolicy::WalkRoots,
        NameKind::Program => ExtensionPolicy::AnyButInclude,
    };
    match find_unique_with_policy(fs, include_paths, &written, policy) {
        IndexAnswer::Found(path) => return IndexAnswer::Found(path),
        // Already ambiguous under the spelling the source used; a second
        // candidate cannot un-ambiguate it.
        IndexAnswer::Unknowable => return IndexAnswer::Unknowable,
        // `find_unique` locates files; it never reads one, so it has no way to
        // reach a verdict about usability. The arm exists for exhaustiveness and
        // is passed straight through rather than folded into the retry, since a
        // second spelling would not make a located file readable either.
        IndexAnswer::Unusable => return IndexAnswer::Unusable,
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
    find_unique_with_policy(fs, include_paths, &folded, policy)
}

/// Search every entry of `include_paths` for `relative`, insisting on exactly
/// one match.
///
/// - no match, a `relative` that is not a root file, or one that would search
///   outside the include paths → [`IndexAnswer::NotFound`]
/// - exactly one match → [`IndexAnswer::Found`]
/// - two entries carrying distinct matching files → [`IndexAnswer::Unknowable`]
///
/// Two entries that resolve to the *same* path (a duplicated PROPATH entry) are
/// one match, not an ambiguity: there is only one file, so nothing is undecided.
///
/// There is no implicit current directory, so a caller that wants `.` searched
/// puts it on the path list.
///
/// # A name may not escape the include paths
///
/// `relative` comes from source text — the operand of a literal `RUN`, or a class
/// name — so it can be anything a file wrote, including `/etc/passwd.p` or
/// `../../secrets/keys.p`. `dir.join(relative)` follows the platform rule that an
/// *absolute* right-hand side replaces the directory entirely, and a `..` prefix
/// walks out of it, so either shape would search somewhere the workspace never
/// configured. Both answer `NotFound` instead.
///
/// That is a contract statement, not a hardening afterthought: the search's rule
/// is "the configured paths, no implicit current directory, no
/// relative-to-current-file resolution," and a name that reaches outside the path
/// list has no answer *within* the contract. Declining is the same conservative
/// choice the exactly-one-match rule makes — a missing link is quiet, a wrong one
/// corrupts everything derived from it.
pub fn find_unique(
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    relative: &Path,
) -> IndexAnswer<PathBuf> {
    find_unique_with_policy(fs, include_paths, relative, ExtensionPolicy::WalkRoots)
}

/// Which extensions a candidate path may carry.
///
/// The walk and a name lookup want different answers, and conflating them cost a
/// real codebase a wave of false `undefined-symbol` findings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionPolicy {
    /// [`is_root_file`]'s set — `.p`, `.w`, `.cls`, `.v`. What a *walk* should
    /// collect: a directory tree offers no evidence about which files are
    /// programs, so the walk picks the conventional extensions and nothing else.
    WalkRoots,
    /// Anything except `.i`. What a **literal `RUN` target** should accept,
    /// because the author wrote the path: `RUN cv/table_rec_count.pp` names that
    /// file and no other, and oxabl declining to look because `.pp` is not one of
    /// the four conventional extensions makes it report a file that plainly
    /// exists as absent from the workspace. `.i` stays excluded — the one
    /// extension the workspace policy is actually *about*, since an include
    /// fragment is never a unit of its own.
    AnyButInclude,
}

impl ExtensionPolicy {
    fn admits(self, path: &Path) -> bool {
        match self {
            ExtensionPolicy::WalkRoots => is_root_file(path),
            ExtensionPolicy::AnyButInclude => path
                .extension()
                .and_then(|ext| ext.to_str())
                .is_some_and(|ext| !ext.eq_ignore_ascii_case("i")),
        }
    }
}

/// [`find_unique`] under an explicit [`ExtensionPolicy`].
pub fn find_unique_with_policy(
    fs: &dyn FileSystem,
    include_paths: &[PathBuf],
    relative: &Path,
    policy: ExtensionPolicy,
) -> IndexAnswer<PathBuf> {
    // The extension policy, applied before any I/O: `.i` is never a root, so no
    // amount of searching should turn an include fragment into a class or a
    // program.
    if !policy.admits(relative) {
        return IndexAnswer::NotFound;
    }
    // Checked before the join, because after it the escape is indistinguishable
    // from a legitimately configured path. `has_root` as well as `is_absolute`:
    // on Windows a rooted-but-not-absolute `\foo.p` also replaces the directory's
    // path component.
    if relative.is_absolute() || relative.has_root() {
        return IndexAnswer::NotFound;
    }
    if normalize_lexically(relative).starts_with("..") {
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
    fn an_absolute_run_target_is_not_searched_outside_the_include_paths() {
        // `dir.join("/etc/thing.p")` is `/etc/thing.p` — the configured directory
        // vanishes. The file is right there and present, and the answer is still
        // `NotFound`, because it is not on the paths the workspace configured.
        let fs = fs_with(&["/etc/thing.p", "/src/thing.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src"]),
                &program_path(&IndexName::new("/etc/thing.p"))
            ),
            IndexAnswer::NotFound
        );
        // The control: the same name *relative* resolves, so the rejection is about
        // the absolute spelling rather than about the fixture.
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src"]),
                &program_path(&IndexName::new("thing.p"))
            ),
            IndexAnswer::Found(PathBuf::from("/src/thing.p"))
        );
    }

    #[test]
    fn a_run_target_that_climbs_out_with_dot_dot_is_not_searched() {
        // `/src/sub` joined with `../../outside/thing.p` reaches `/outside`, which
        // is not on the path list. A `..` that stays *inside* the entry is fine —
        // it names a file the entry really does cover — so only the escaping shape
        // is rejected.
        let fs = fs_with(&["/outside/thing.p", "/src/other/thing.p"]);
        assert_eq!(
            find_unique(
                &fs,
                &dirs(&["/src/sub"]),
                &program_path(&IndexName::new("../../outside/thing.p"))
            ),
            IndexAnswer::NotFound
        );
        // And the escape check itself does not object to a `..` that cancels out
        // within the entry — that names a file the entry really does cover, so the
        // gate has to distinguish the two rather than reject every `..`. Asserted
        // on the predicate rather than through `find_unique`, because
        // `InMemoryFileSystem` matches path keys literally: the joined
        // `/src/sub/../other/thing.p` is not a key it holds, so a round trip would
        // answer `NotFound` for a reason that has nothing to do with the gate.
        assert!(
            !normalize_lexically(Path::new("sub/../other/thing.p")).starts_with(".."),
            "a `..` that cancels out within the entry is not an escape"
        );
    }

    #[test]
    fn normalize_lexically_resolves_dot_and_dot_dot_without_touching_disk() {
        // No file in this test exists anywhere; the point is that the answer does
        // not depend on one, which is why `canonicalize` is not what this does.
        assert_eq!(
            normalize_lexically(Path::new("a/./b/../c.p")),
            PathBuf::from("a/c.p")
        );
        assert_eq!(
            normalize_lexically(Path::new("/src/./sub/../thing.p")),
            PathBuf::from("/src/thing.p")
        );
        // An escaping `..` survives, which is what makes the escape detectable.
        assert_eq!(
            normalize_lexically(Path::new("../../thing.p")),
            PathBuf::from("../../thing.p")
        );
        assert_eq!(
            normalize_lexically(Path::new("a/../../thing.p")),
            PathBuf::from("../thing.p")
        );
        // A rooted `..` is absorbed, matching POSIX's `/..` == `/`.
        assert_eq!(
            normalize_lexically(Path::new("/../thing.p")),
            PathBuf::from("/thing.p")
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

    #[test]
    fn a_run_target_with_an_unconventional_extension_is_found() {
        // `.pp` is not one of the four extensions a *walk* collects, and the author
        // wrote the path anyway. Declining to look would report a file that plainly
        // exists as absent from the workspace, which is the one claim a search must
        // not make wrongly.
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/src/cv/table_rec_count.pp"), "MESSAGE 1.");
        let paths = vec![PathBuf::from("/src")];
        assert_eq!(
            find_name(
                &fs,
                &paths,
                &IndexName::new("cv/table_rec_count.pp"),
                NameKind::Program
            ),
            IndexAnswer::Found(PathBuf::from("/src/cv/table_rec_count.pp"))
        );
    }

    #[test]
    fn an_include_fragment_is_still_never_a_run_target() {
        // The one extension the policy is about. Widening what a `RUN` target may
        // carry must not widen this.
        let mut fs = InMemoryFileSystem::new();
        fs.insert(
            PathBuf::from("/src/shared/decls.i"),
            "DEFINE VARIABLE x AS INTEGER.",
        );
        let paths = vec![PathBuf::from("/src")];
        assert_eq!(
            find_name(
                &fs,
                &paths,
                &IndexName::new("shared/decls.i"),
                NameKind::Program
            ),
            IndexAnswer::NotFound
        );
    }
}
