//! A precomputed map from a `SHARED` name to the files that produce it.
//!
//! A `DEFINE NEW [GLOBAL] SHARED` name maps onto no path, so the only way to find
//! its producer is to look at a set of files. [`SharedProducers`] is the answer
//! for a fixed file set, computed once and then read by any number of indexes —
//! it is `Send + Sync`, and [`BatchIndex`](crate::BatchIndex) is not, so a client
//! that runs one index per thread builds this once and hands every thread a
//! borrow, instead of every thread parsing the whole set itself.
//!
//! # Seed or don't trust
//!
//! An index with no producer map and no seeded file list answers
//! [`shared_producer`](oxabl_semantic::WorkspaceIndex::shared_producer) only from
//! files some other lookup happened to pull in. That answer depends on the order
//! of the run, so a `SharedProducer` edge built that way is not reproducible.
//! Build the map over every file the run can see, and the edge is the same on
//! every thread and in every order.
//!
//! A file that produces a name only through an include is found when it is
//! nothing but includes and directives. One with code of its own is judged by
//! its text as written, as [`index_file`] judges it, so its producer edge is
//! missing, on every thread and in every order alike.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use oxabl_semantic::IndexName;
use rustc_hash::FxHashMap;

use crate::{IncludeContext, IndexedFileId, index_file_at};

/// Which files define each `DEFINE NEW [GLOBAL] SHARED` name.
///
/// Holds paths as given, so an index reads a producer the same way it reads any
/// other file. A file the map lists is a producer only as far as it reads and
/// parses: one that is unreadable or does not parse defines nothing. A file
/// with code of its own defines only what its text as written defines, not a
/// name an include defines for it.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SharedProducers {
    by_name: FxHashMap<Box<str>, Vec<PathBuf>>,
}

impl SharedProducers {
    /// Scan `files` and record which of them produce each `SHARED` name.
    ///
    /// Reads every file once. For a client that wants to scan on several threads,
    /// [`definitions_in`] does one file and [`from_definitions`](Self::from_definitions)
    /// joins the results.
    /// `includes` reads both the files and the includes a file is expanded
    /// through, as `index_file` does.
    pub fn build(files: &[PathBuf], includes: &IncludeContext<'_>) -> Self {
        Self::from_definitions(
            files
                .iter()
                .map(|path| (path.clone(), definitions_in(path, includes))),
        )
    }

    /// [`build`](Self::build) on `threads` threads (at least one). The result is
    /// the same as `build`'s for any thread count.
    pub fn build_parallel(
        files: &[PathBuf],
        includes: &IncludeContext<'_>,
        threads: usize,
    ) -> Self {
        let threads = threads.clamp(1, files.len().max(1));
        if threads == 1 {
            return Self::build(files, includes);
        }
        let next = AtomicUsize::new(0);
        let definitions = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..threads)
                .map(|_| {
                    scope.spawn(|| {
                        let mut mine = Vec::new();
                        while let Some(path) = files.get(next.fetch_add(1, Ordering::Relaxed)) {
                            mine.push((path.clone(), definitions_in(path, includes)));
                        }
                        mine
                    })
                })
                .collect();
            workers
                .into_iter()
                .flat_map(|worker| match worker.join() {
                    Ok(mine) => mine,
                    Err(panic) => std::panic::resume_unwind(panic),
                })
                .collect::<Vec<_>>()
        });
        Self::from_definitions(definitions)
    }

    /// Join per-file results of [`definitions_in`]. The result does not depend on
    /// the order of `files`.
    pub fn from_definitions(files: impl IntoIterator<Item = (PathBuf, Vec<IndexName>)>) -> Self {
        let mut by_name: FxHashMap<Box<str>, Vec<PathBuf>> = FxHashMap::default();
        for (path, names) in files {
            for name in names {
                let paths = by_name.entry(name.as_str().into()).or_default();
                if !paths.contains(&path) {
                    paths.push(path.clone());
                }
            }
        }
        for paths in by_name.values_mut() {
            paths.sort();
        }
        SharedProducers { by_name }
    }

    /// The files that define `name`, in path order.
    pub fn producers_of(&self, name: &IndexName) -> &[PathBuf] {
        self.by_name.get(name.as_str()).map_or(&[], Vec::as_slice)
    }

    /// The number of distinct `SHARED` names with a producer.
    pub fn len(&self) -> usize {
        self.by_name.len()
    }

    /// Whether no file in the set produces a `SHARED` name.
    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }
}

/// The `SHARED` names the file at `path` produces: none when it cannot be read or
/// does not parse.
///
/// The same answer as indexing the file, reached without the parse for a file
/// that cannot say `SHARED` at all: the keyword has no abbreviation, so a text
/// without it defines no shared name, and that is nearly every file of a tree.
/// A file `index_file` expands, such as one that is nothing but includes and
/// directives, is judged by its expanded text. Any other file is judged by its
/// own text, so one with code of its own whose only `SHARED` definition comes
/// from an include is not a producer.
pub fn definitions_in(path: &Path, includes: &IncludeContext<'_>) -> Vec<IndexName> {
    let Ok(source) = includes.fs.read(path) else {
        return Vec::new();
    };
    if !may_define_shared(path, &source, includes) {
        return Vec::new();
    }
    index_file_at(IndexedFileId::new(0), path, &source, includes)
        .0
        .shared_definitions
}

/// Whether [`index_file_at`] can find a shared definition in `source`, the file
/// at `path`: a superset test. A file whose text does not say `SHARED` is parsed
/// only when it names an include, and expanded only when `index_file_at` may
/// expand it.
pub(crate) fn may_define_shared(path: &Path, source: &str, includes: &IncludeContext<'_>) -> bool {
    mentions_shared(source)
        || (names_an_include(source)
            && crate::may_be_expanded(source)
            && crate::expanded_text_at(path, source, includes)
                .is_some_and(|text| mentions_shared(&text)))
}

/// Whether `source` holds a `{` that does not open a `{&name}` reference. A
/// superset of the files that name an include.
fn names_an_include(source: &str) -> bool {
    source
        .as_bytes()
        .windows(2)
        .any(|pair| pair[0] == b'{' && pair[1] != b'&')
}

/// Whether `source` holds the word `SHARED` in any case, as part of any longer
/// token or not. A superset of the files that define a shared name.
pub(crate) fn mentions_shared(source: &str) -> bool {
    const WORD: &[u8] = b"shared";
    source
        .as_bytes()
        .windows(WORD.len())
        .any(|window| window.eq_ignore_ascii_case(WORD))
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_workspace::{InMemoryFileSystem, PreprocessorConfig};

    fn build(fs: &InMemoryFileSystem, files: &[PathBuf]) -> SharedProducers {
        let predefined = PreprocessorConfig::default();
        SharedProducers::build(files, &in_fs(fs, &predefined))
    }

    fn in_fs<'a>(
        fs: &'a InMemoryFileSystem,
        predefined: &'a PreprocessorConfig,
    ) -> IncludeContext<'a> {
        IncludeContext {
            fs,
            include_paths: &[],
            root_relative_paths: &[],
            predefined,
        }
    }

    fn fs_with(files: &[(&str, &str)]) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for (path, source) in files {
            fs.insert(PathBuf::from(path), *source);
        }
        fs
    }

    fn paths(files: &[(&str, &str)]) -> Vec<PathBuf> {
        files.iter().map(|(p, _)| PathBuf::from(p)).collect()
    }

    #[test]
    fn a_new_shared_definition_is_a_producer_and_a_consumer_is_not() {
        let files = [
            (
                "/a.p",
                "DEFINE NEW SHARED VARIABLE v-one AS INTEGER NO-UNDO.",
            ),
            ("/b.p", "DEFINE SHARED VARIABLE v-one AS INTEGER NO-UNDO."),
            (
                "/c.p",
                "define new global shared variable V-Two as integer no-undo.",
            ),
            ("/d.p", "MESSAGE \"nothing\"."),
        ];
        let fs = fs_with(&files);
        let map = build(&fs, &paths(&files));

        assert_eq!(
            map.producers_of(&IndexName::new("v-one")),
            [PathBuf::from("/a.p")]
        );
        assert_eq!(
            map.producers_of(&IndexName::new("v-two")),
            [PathBuf::from("/c.p")]
        );
        assert!(map.producers_of(&IndexName::new("v-three")).is_empty());
        assert_eq!(map.len(), 2);
    }

    #[test]
    fn two_producers_of_one_name_are_both_listed_whatever_the_order() {
        let files = [
            ("/b.p", "DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO."),
            ("/a.p", "DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO."),
        ];
        let fs = fs_with(&files);
        let forward = build(&fs, &paths(&files));
        let mut reversed = paths(&files);
        reversed.reverse();
        let backward = build(&fs, &reversed);

        assert_eq!(forward, backward);
        assert_eq!(
            forward.producers_of(&IndexName::new("v-x")),
            [PathBuf::from("/a.p"), PathBuf::from("/b.p")]
        );
    }

    #[test]
    fn an_unreadable_or_broken_file_produces_nothing() {
        let files = [("/broken.p", "DEFINE NEW SHARED VARIABLE v-y AS @ @ @.")];
        let fs = fs_with(&files);
        let mut listed = paths(&files);
        listed.push(PathBuf::from("/missing.p"));
        let map = build(&fs, &listed);

        assert!(map.is_empty(), "{map:?}");
    }

    #[test]
    fn the_prefilter_agrees_with_indexing_the_whole_file() {
        for source in [
            "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "define new shared variable v-a as integer no-undo.",
            "DEFINE NEW GLOBAL SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "DEFINE NEW SHARED TEMP-TABLE tt-a FIELD f AS INTEGER.",
            "DEFINE SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
            "DEF NEW SHARED VAR v-a AS INTEGER NO-UNDO.",
            "&GLOBAL-DEFINE scope NEW SHARED\nDEFINE {&scope} VARIABLE v-a AS INTEGER NO-UNDO.",
            "PROCEDURE p:\nEND PROCEDURE.",
        ] {
            let fs = fs_with(&[("/f.p", source)]);
            let predefined = PreprocessorConfig::default();
            assert_eq!(
                definitions_in(Path::new("/f.p"), &in_fs(&fs, &predefined)),
                crate::index_file(IndexedFileId::new(0), source, &in_fs(&fs, &predefined))
                    .shared_definitions,
                "{source}"
            );
        }
    }

    #[test]
    fn the_prefilter_agrees_with_indexing_a_file_through_its_includes() {
        let include_paths = [PathBuf::from("/")];
        let predefined = PreprocessorConfig::default();
        for source in [
            "{defs.i}",
            "DEFINE VARIABLE v-own AS INTEGER NO-UNDO.\n{defs.i}",
            "{plain.i}\nDEFINE VARIABLE v-own AS INTEGER NO-UNDO.",
            "{gone.i}\nDEFINE VARIABLE v-own AS INTEGER NO-UNDO.",
            "DEFINE {head.i} VARIABLE v-b AS INTEGER NO-UNDO.",
            "CLASS c:\n{defs.i}\nEND CLASS.",
        ] {
            let fs = fs_with(&[
                ("/f.p", source),
                (
                    "/defs.i",
                    "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
                ),
                ("/plain.i", "MESSAGE 1."),
                ("/head.i", "NEW SHARED"),
            ]);
            let includes = IncludeContext {
                fs: &fs,
                include_paths: &include_paths,
                root_relative_paths: &[],
                predefined: &predefined,
            };
            assert_eq!(
                definitions_in(Path::new("/f.p"), &includes),
                index_file_at(IndexedFileId::new(0), Path::new("/f.p"), source, &includes)
                    .0
                    .shared_definitions,
                "{source}"
            );
        }
        for (source, want) in [
            ("{defs.i}", &["v-a"][..]),
            ("DEFINE {head.i} VARIABLE v-b AS INTEGER NO-UNDO.", &["v-b"]),
            ("MESSAGE 2.\n{defs.i}", &[]),
        ] {
            let fs = fs_with(&[
                ("/f.p", source),
                (
                    "/defs.i",
                    "DEFINE NEW SHARED VARIABLE v-a AS INTEGER NO-UNDO.",
                ),
                ("/head.i", "NEW SHARED"),
            ]);
            let includes = IncludeContext {
                fs: &fs,
                include_paths: &include_paths,
                root_relative_paths: &[],
                predefined: &predefined,
            };
            let want: Vec<IndexName> = want.iter().map(|name| IndexName::new(name)).collect();
            assert_eq!(
                definitions_in(Path::new("/f.p"), &includes),
                want,
                "{source}"
            );
        }
    }

    #[test]
    fn a_parallel_scan_builds_the_map_a_serial_scan_builds() {
        let sources: Vec<(String, String)> = (0..40)
            .map(|n| {
                let body = match n % 3 {
                    0 => format!(
                        "DEFINE NEW SHARED VARIABLE v-{} AS CHARACTER NO-UNDO.",
                        n % 5
                    ),
                    1 => "DEFINE SHARED VARIABLE v-0 AS CHARACTER NO-UNDO.".to_string(),
                    _ => "MESSAGE \"quiet\".".to_string(),
                };
                (format!("/src/f{n:02}.p"), body)
            })
            .collect();
        let listed: Vec<(&str, &str)> = sources
            .iter()
            .map(|(p, b)| (p.as_str(), b.as_str()))
            .collect();
        let fs = fs_with(&listed);
        let files = paths(&listed);
        let predefined = PreprocessorConfig::default();
        let includes = in_fs(&fs, &predefined);

        let serial = SharedProducers::build(&files, &includes);
        assert!(!serial.is_empty());
        for threads in [0, 1, 4, 64] {
            assert_eq!(
                SharedProducers::build_parallel(&files, &includes, threads),
                serial,
                "{threads} threads"
            );
        }
    }
}
