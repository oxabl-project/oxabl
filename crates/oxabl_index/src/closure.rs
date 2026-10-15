//! What the include a not-taken branch names would read if the branch were
//! taken.

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_preprocessor::{MAX_INCLUDE_DEPTH, Preprocessor};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::deps::ConditionalIncludeClosure;
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
    reaches: Vec<PathBuf>,
    unresolved: Vec<String>,
    complete: bool,
    /// The target's own conditional includes.
    skipped: Vec<(String, bool)>,
}

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
            });
            continue;
        };
        let key = normalize_lexically(&path);
        let expanded = by_path
            .entry(key.clone())
            .or_insert_with(|| expand(&path, &name, includes));
        let Some(expanded) = expanded else {
            out.push(ConditionalIncludeClosure {
                name,
                via,
                path: Some(key),
                reaches: Vec::new(),
                unresolved: Vec::new(),
                complete: false,
            });
            continue;
        };
        let too_deep = depth >= MAX_INCLUDE_DEPTH && !expanded.skipped.is_empty();
        if !too_deep {
            for (next, computed) in expanded.skipped.iter().rev() {
                work.push((next.clone(), *computed, Some(name.clone()), depth + 1));
            }
        }
        out.push(ConditionalIncludeClosure {
            via,
            path: Some(key.clone()),
            reaches: expanded
                .reaches
                .iter()
                .filter(|reached| **reached != key)
                .cloned()
                .collect(),
            unresolved: expanded.unresolved.clone(),
            complete: expanded.complete && !too_deep,
            name,
        });
    }
    out
}

/// Expand the target at `path` on its own; `None` when it cannot be read or the
/// preprocessor fails outright.
fn expand(path: &std::path::Path, name: &str, includes: &IncludeContext<'_>) -> Option<Expanded> {
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
    Some(Expanded {
        reaches: included_paths(&processed),
        complete: unresolved.is_empty() && processed.text_losses.is_empty(),
        unresolved,
        skipped: processed
            .skipped_includes
            .iter()
            .map(|skipped| (skipped.name.clone(), skipped.computed_name))
            .collect(),
    })
}
