//! What the include a not-taken branch names would read if the branch were
//! taken.

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use oxabl_common::FileId;
use oxabl_preprocessor::{MAX_INCLUDE_DEPTH, Preprocessor};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::deps::{ClosureReference, ConditionalIncludeClosure};
use crate::search::normalize_lexically;
use crate::{IncludeContext, included_paths};

/// The identity a target is expanded under; nothing outside this module sees it.
const TARGET_FILE: FileId = FileId::new(1);

/// One conditional include to expand: its name as written and whether the name
/// holds a `{...}` reference.
#[derive(Debug, Clone, Copy)]
pub struct ConditionalIncludeName<'a> {
    pub name: &'a str,
    pub computed_name: bool,
}

/// What one target read.
struct Expanded {
    read: Arc<Read>,
    references: Vec<ClosureReference>,
}

/// What one target read on its own, whichever file names it.
struct Read {
    source: Arc<str>,
    reaches: Vec<PathBuf>,
    unresolved: Vec<String>,
    complete: bool,
    /// The target's own conditional includes.
    skipped: Vec<(String, bool)>,
}

/// What conditional include targets read, kept across the files of one run so
/// a target many files name conditionally is read and expanded once per
/// search path. See [`conditional_include_closure_cached`].
///
/// An entry is keyed by the target, the name it is included by and the search
/// path only. Every call that shares one value must therefore pass the same
/// filesystem and the same predefined names, and no file may change while the
/// value lives. `Sync`: the threads of a run share one.
///
/// Hidden: the pipeline's `SharedRun` is the client that keeps those fixed.
#[doc(hidden)]
#[derive(Default)]
pub struct ClosureExpansions {
    entries: Mutex<FxHashMap<ExpansionKey, ExpansionSlot>>,
}

/// A target file, the name it is included by and the search path.
type ExpansionKey = (PathBuf, String, Vec<PathBuf>);

/// What a target read, once one call has read it; `None` when it could not.
type ExpansionSlot = Arc<OnceLock<Option<Arc<Read>>>>;

/// The closure of each of `names`, expanded on its own through `includes`.
///
/// The includes a target writes in its own not-taken branches are expanded too,
/// as entries whose `via` names the target. A name is expanded once, and a file
/// once however many names reach it. A chain of such branches deeper than the
/// preprocessor's include depth limit is cut there, and the entry that names the
/// next level is marked incomplete.
pub fn conditional_include_closure(
    names: &[ConditionalIncludeName<'_>],
    includes: &IncludeContext<'_>,
) -> Vec<ConditionalIncludeClosure> {
    conditional_include_closure_with(names, includes, &mut |_| Vec::new())
}

/// One target [`conditional_include_closure_with`] asks the caller to analyse.
#[derive(Debug, Clone, Copy)]
pub struct ClosureTarget<'a> {
    /// The file the name resolved to.
    pub path: &'a Path,
    /// The include name as written.
    pub name: &'a str,
    /// The target's text.
    pub source: &'a str,
}

/// [`conditional_include_closure`], with each entry's
/// [`references`](ConditionalIncludeClosure::references) from `references`.
///
/// This crate cannot analyse a file, so the caller does: `references` is called
/// once per target file that was read and expanded, and its answer is shared by
/// every name that reaches the file. It is sorted and deduplicated here.
pub fn conditional_include_closure_with(
    names: &[ConditionalIncludeName<'_>],
    includes: &IncludeContext<'_>,
    references: &mut dyn FnMut(ClosureTarget<'_>) -> Vec<ClosureReference>,
) -> Vec<ConditionalIncludeClosure> {
    closure(names, includes, None, references)
}

/// [`conditional_include_closure_with`], reading and expanding each target
/// once for every call that shares `expansions` and the include paths.
/// `references` is still called once per target per call. Every call that
/// shares `expansions` must pass the same filesystem and predefined names (see
/// [`ClosureExpansions`]).
#[doc(hidden)]
pub fn conditional_include_closure_cached(
    names: &[ConditionalIncludeName<'_>],
    includes: &IncludeContext<'_>,
    expansions: &ClosureExpansions,
    references: &mut dyn FnMut(ClosureTarget<'_>) -> Vec<ClosureReference>,
) -> Vec<ConditionalIncludeClosure> {
    closure(names, includes, Some(expansions), references)
}

fn closure(
    names: &[ConditionalIncludeName<'_>],
    includes: &IncludeContext<'_>,
    expansions: Option<&ClosureExpansions>,
    references: &mut dyn FnMut(ClosureTarget<'_>) -> Vec<ClosureReference>,
) -> Vec<ConditionalIncludeClosure> {
    let mut out = Vec::new();
    let mut seen: FxHashSet<String> = FxHashSet::default();
    let mut by_path: FxHashMap<PathBuf, Option<Expanded>> = FxHashMap::default();
    let mut work: Vec<(String, bool, Option<String>, usize)> = names
        .iter()
        .rev()
        .map(|n| (n.name.to_string(), n.computed_name, None, 0))
        .collect();

    while let Some((name, computed, via, depth)) = work.pop() {
        if !seen.insert(name.clone()) {
            continue;
        }
        let path = (!computed)
            .then(|| includes.fs.resolve_include(includes.include_paths, &name))
            .flatten();
        let Some(path) = path else {
            out.push(ConditionalIncludeClosure {
                name,
                via,
                path: None,
                reaches: Vec::new(),
                unresolved: Vec::new(),
                complete: false,
                references: Vec::new(),
            });
            continue;
        };
        let key = normalize_lexically(&path);
        let expanded = by_path
            .entry(key.clone())
            .or_insert_with(|| expand(&path, &key, &name, includes, expansions, references));
        let Some(expanded) = expanded else {
            out.push(ConditionalIncludeClosure {
                name,
                via,
                path: Some(key),
                reaches: Vec::new(),
                unresolved: Vec::new(),
                complete: false,
                references: Vec::new(),
            });
            continue;
        };
        let read = &expanded.read;
        let too_deep = depth >= MAX_INCLUDE_DEPTH && !read.skipped.is_empty();
        if !too_deep {
            for (next, computed) in read.skipped.iter().rev() {
                work.push((next.clone(), *computed, Some(name.clone()), depth + 1));
            }
        }
        out.push(ConditionalIncludeClosure {
            via,
            path: Some(key.clone()),
            reaches: read
                .reaches
                .iter()
                .filter(|reached| **reached != key)
                .cloned()
                .collect(),
            unresolved: read.unresolved.clone(),
            complete: read.complete && !too_deep,
            references: expanded.references.clone(),
            name,
        });
    }
    out
}

/// Expand the target at `path` (normalized: `key`) on its own, through
/// `expansions` when given; `None` when it cannot be read or the preprocessor
/// fails outright.
fn expand(
    path: &Path,
    key: &Path,
    name: &str,
    includes: &IncludeContext<'_>,
    expansions: Option<&ClosureExpansions>,
    references: &mut dyn FnMut(ClosureTarget<'_>) -> Vec<ClosureReference>,
) -> Option<Expanded> {
    let read = match expansions {
        Some(expansions) => {
            let key = (
                key.to_path_buf(),
                name.to_string(),
                includes.include_paths.to_vec(),
            );
            let slot = Arc::clone(
                expansions
                    .entries
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .entry(key)
                    .or_default(),
            );
            slot.get_or_init(|| read(path, name, includes).map(Arc::new))
                .clone()?
        }
        None => Arc::new(read(path, name, includes)?),
    };
    let mut references = references(ClosureTarget {
        path,
        name,
        source: &read.source,
    });
    references
        .sort_by(|a, b| (a.kind, a.conditional, &a.name).cmp(&(b.kind, b.conditional, &b.name)));
    references.dedup();
    Some(Expanded { read, references })
}

/// Read and expand the target at `path` on its own.
fn read(path: &Path, name: &str, includes: &IncludeContext<'_>) -> Option<Read> {
    let source = includes.fs.read(path).ok()?;
    let processed = Preprocessor::new(includes.fs, includes.include_paths)
        .with_predefined(includes.predefined)
        .process_named(TARGET_FILE, name, &source)
        .ok()?;
    let mut unresolved: Vec<String> = processed
        .unresolved_includes
        .iter()
        .map(|include| include.name.clone())
        .chain(
            processed
                .unreadable_includes
                .iter()
                .map(|include| include.name.clone()),
        )
        .collect();
    unresolved.sort();
    unresolved.dedup();
    Some(Read {
        reaches: included_paths(&processed),
        complete: unresolved.is_empty() && processed.text_losses.is_empty(),
        unresolved,
        skipped: processed
            .skipped_includes
            .iter()
            .map(|skipped| (skipped.name.clone(), skipped.computed_name))
            .collect(),
        source,
    })
}
