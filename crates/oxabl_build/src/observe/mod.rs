//! The observed closure: the files the compiler read, as its `XREF-XML`
//! names them, for a unit whose closure oxabl cannot read from its source,
//! with every class source the compile could read.
//!
//! After such a unit compiles, the build stores what the compiler read. The
//! next plan keys the unit by those files ([`crate::key::compute`]); a file
//! that is gone, a new file where an include could resolve first, or another
//! set of classes gives no key. Right after the compile,
//! [`Observer::key_after`] keys the unit from the bytes on disk, but only when
//! each file is provably the one the compiler read.

mod classes;
mod closure;
pub mod xref;

use std::path::Path;

use rustc_hash::FxHashMap;

pub use classes::class_listing;
pub use closure::{Places, observe};

use crate::key::compile_key::{Algorithm, ClosureFile, ClosureSource};
use crate::key::compute::{ClassPart, KeyBase};
use crate::refresh::{Content, Outcome, RACY_NS};
use crate::store::{Components, ObservedClosure};

/// What the build needs to observe the units it compiles.
pub struct Observer<'a> {
    pub places: Places<'a>,
    pub outcome: &'a Outcome,
    pub base: &'a KeyBase,
    /// The class part of each unit PROPATH, as the plan found it.
    pub classes: &'a FxHashMap<usize, ClassPart>,
    /// The class units of the workspace.
    pub class_units: Vec<&'a str>,
    /// The id of each path the refresh knows.
    pub ids: FxHashMap<&'a str, u32>,
    /// The wall clock when the build started, in nanoseconds.
    pub started_ns: i64,
}

fn nanoseconds(time: std::io::Result<std::time::SystemTime>) -> Option<i64> {
    let elapsed = time.ok()?.duration_since(std::time::UNIX_EPOCH).ok()?;
    i64::try_from(elapsed.as_nanos()).ok()
}

impl Observer<'_> {
    /// The stored observed closure of `unit`, as canonical paths.
    pub fn stored(&self, unit: u32) -> Option<ObservedClosure> {
        let observed = self.outcome.observed.get(&unit)?;
        let names = |ids: &[u32]| {
            ids.iter()
                .map(|id| self.outcome.name(*id).to_string())
                .collect()
        };
        Some(ObservedClosure {
            files: names(&observed.files),
            absent: names(&observed.absent),
            classes: names(&observed.classes),
        })
    }

    /// The BLAKE3 hash of `path` now, when it is provably what a compile in
    /// this build read: the refresh hashed the same bytes before the compile,
    /// or the file did not change since a time before the build started.
    fn hash_after(&self, path: &str) -> Option<[u8; 32]> {
        if path.starts_with('$') || path.starts_with('/') {
            return None;
        }
        let host = self.places.paths.absolute(path);
        let before = self.ids.get(path).map(|id| self.outcome.content_of(*id));
        if !matches!(before, Some(Content::Hash(_))) && !self.settled(&host) {
            return None;
        }
        let hash = *blake3::hash(&std::fs::read(&host).ok()?).as_bytes();
        match before {
            Some(Content::Hash(before)) if before != hash => None,
            _ => Some(hash),
        }
    }

    /// Whether the file at `host` was last changed well before the build.
    fn settled(&self, host: &Path) -> bool {
        use std::os::unix::fs::MetadataExt;
        let Ok(metadata) = std::fs::metadata(host) else {
            return false;
        };
        let changed = metadata.ctime() * 1_000_000_000 + metadata.ctime_nsec();
        let modified = nanoseconds(metadata.modified());
        let limit = self.started_ns - RACY_NS;
        changed < limit && modified.is_some_and(|modified| modified < limit)
    }

    /// The observed key of a unit right after it compiled with `closure`, or
    /// `None` when a file cannot be shown to hold the bytes the compiler read,
    /// or the classes on disk are not the ones the plan listed: then the unit
    /// compiles again at the next build and is keyed there.
    pub fn key_after(
        &self,
        unit: u32,
        compile_name: &str,
        propath: usize,
        closure: &ObservedClosure,
    ) -> Option<([u8; 32], Components)> {
        let unit_path = self.outcome.name(unit);
        let Content::Hash(unit_content) = self.outcome.content_of(unit) else {
            return None;
        };
        if self.hash_after(unit_path) != Some(unit_content) {
            return None;
        }
        let part = self.classes.get(&propath)?;
        let entries: Vec<String> = self.base.propaths[propath]
            .iter()
            .map(|(entry, _)| entry.clone())
            .collect();
        let listed = class_listing(
            self.places.paths,
            &entries,
            self.class_units.iter().copied(),
        );
        if !part.reasons.is_empty()
            || closure.classes != part.listing
            || listed.ok().as_ref() != Some(&part.listing)
        {
            return None;
        }
        let mut paths: Vec<&str> = closure
            .files
            .iter()
            .map(String::as_str)
            .chain(part.files.iter().map(|id| self.outcome.name(*id)))
            .collect();
        paths.sort_unstable();
        paths.dedup();
        let mut files = Vec::with_capacity(paths.len());
        for path in paths {
            files.push(ClosureFile {
                path,
                algorithm: Algorithm::Blake3,
                hash: self.hash_after(path)?,
            });
        }
        let absent = |path: &String| std::fs::metadata(self.places.paths.absolute(path)).is_err();
        if !closure.absent.iter().all(absent) {
            return None;
        }
        files.sort_unstable_by(|a, b| a.path.cmp(b.path));
        let unit = (unit_path, compile_name, unit_content);
        Some(
            self.base
                .key(unit, &files, ClosureSource::Observed, propath),
        )
    }
}
