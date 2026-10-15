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

use std::path::{Path, PathBuf};

use oxabl_semantic::IndexName;
use rustc_hash::FxHashMap;

use crate::{IncludeContext, IndexedFileId, index_file_at};

/// Which files define each `DEFINE NEW [GLOBAL] SHARED` name.
///
/// Holds paths as given, so an index reads a producer the same way it reads any
/// other file. A file the map lists is a producer only as far as it reads and
/// parses: one that is unreadable or does not parse defines nothing.
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
    /// through, as [`index_file`] does.
    pub fn build(files: &[PathBuf], includes: &IncludeContext<'_>) -> Self {
        Self::from_definitions(
            files
                .iter()
                .map(|path| (path.clone(), definitions_in(path, includes))),
        )
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
/// A file is judged by its own text here, so one whose only `SHARED` definition
/// comes from an include it is expanded through is not a producer.
pub fn definitions_in(path: &Path, includes: &IncludeContext<'_>) -> Vec<IndexName> {
    let Ok(source) = includes.fs.read(path) else {
        return Vec::new();
    };
    if !mentions_shared(&source) {
        return Vec::new();
    }
    index_file_at(IndexedFileId::new(0), path, &source, includes)
        .0
        .shared_definitions
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
}
