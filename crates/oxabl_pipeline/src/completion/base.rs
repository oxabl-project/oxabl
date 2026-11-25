//! The cursor in the base: where the current text's cursor lies in the
//! analysis that completion reads, which can be of an older text.

use std::path::Path;

use oxabl_semantic::{ScopeId, ScopeKind};

use super::CompletionBase;

/// The cursor and the window start of the current text, placed in a base.
pub(crate) struct Mapped {
    /// Each place of the cursor in the expanded text of the base, one for each
    /// expansion of the document.
    pub places: Vec<u32>,
    /// Each place of the start of the statement window.
    pub window_places: Vec<u32>,
    /// The base was made from an older text.
    pub stale: bool,
}

/// Place the cursor at byte `cursor` and the window at `window_start` of
/// `text` in `base`. `None` when the cursor has no place in the analysed text:
/// an `&IF` branch the preprocessor did not take, or text the expansion lost.
pub(crate) fn map(
    base: &CompletionBase<'_, '_, '_>,
    path: &Path,
    text: &str,
    cursor: u32,
    window_start: u32,
) -> Option<Mapped> {
    let ids = base
        .navigation
        .file_ids(&oxabl_index::search::normalize_lexically(path));
    if ids.is_empty() {
        return None;
    }
    let old = base.text.as_bytes();
    let new = text.as_bytes();
    let (stale, prefix, suffix) = if old == new {
        (false, new.len(), 0)
    } else {
        let prefix = old.iter().zip(new).take_while(|(a, b)| a == b).count();
        let room = old.len().min(new.len()) - prefix;
        let suffix = old
            .iter()
            .rev()
            .zip(new.iter().rev())
            .take(room)
            .take_while(|(a, b)| a == b)
            .count();
        (true, prefix, suffix)
    };
    let to_base = |at: u32| -> u32 {
        let at = at as usize;
        if !stale || at <= prefix {
            at as u32
        } else if at >= new.len() - suffix {
            (at + old.len() - new.len()) as u32
        } else {
            prefix as u32
        }
    };
    let places_of = |at: u32| -> Vec<u32> {
        let at = to_base(at);
        let found: Vec<u32> = ids
            .iter()
            .flat_map(|id| base.navigation.expanded.expanded_offsets(*id, at))
            .collect();
        if !found.is_empty() || at == 0 || at as usize != old.len() {
            return found;
        }
        // The end of the text is the place after its last byte.
        ids.iter()
            .flat_map(|id| base.navigation.expanded.expanded_offsets(*id, at - 1))
            .map(|place| place + 1)
            .collect()
    };
    // Text in a branch the preprocessor did not take is not analysed.
    let at = to_base(cursor);
    if base
        .navigation
        .expanded
        .skipped_text()
        .iter()
        .any(|skipped| {
            ids.contains(&skipped.site.file)
                && skipped.site.span.start <= at
                && at <= skipped.site.span.end
        })
    {
        return None;
    }
    let places = if old.is_empty() {
        // An empty document expands to nothing.
        if ids.contains(&base.navigation.expanded.root_file()) {
            vec![0]
        } else {
            Vec::new()
        }
    } else {
        places_of(cursor)
    };
    if places.is_empty() {
        return None;
    }
    let window_places = places_of(window_start);
    Some(Mapped {
        places,
        window_places,
        stale,
    })
}

/// The scope that holds expanded byte `virt`, past the implicit scopes of a
/// temp-table or a frame definition.
pub(crate) fn scope_at(base: &CompletionBase<'_, '_, '_>, virt: u32) -> ScopeId {
    let tree = &base.navigation.model.semantic.scope_tree;
    let mut scope = tree.innermost_at(virt);
    while matches!(
        tree.get(scope).kind,
        ScopeKind::TempTable | ScopeKind::Frame
    ) && let Some(parent) = tree.get(scope).parent
    {
        scope = parent;
    }
    scope
}
