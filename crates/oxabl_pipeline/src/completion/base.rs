//! The cursor in the base: where the current text's cursor lies in the
//! analysis that completion reads, which can be of an older text.

use std::path::Path;

use oxabl_common::FileId;
use oxabl_lexer::Kind;
use oxabl_semantic::{ScopeId, ScopeKind};

use super::CompletionBase;
use super::tokens::{TextDiff, TokenStream};

/// The cursor and the window start of the current text, placed in a base.
pub(crate) struct Mapped {
    /// The file ids of the document in the base's expansion: the root, or
    /// each expansion of an include.
    pub ids: Vec<FileId>,
    /// Each place of the cursor in the expanded text of the base, one for each
    /// expansion of the document.
    pub places: Vec<u32>,
    /// Each place of the start of the statement window.
    pub window_places: Vec<u32>,
    /// The base was made from an older text.
    pub stale: bool,
    /// The bytes of the base's own text that the current text changed, for a
    /// stale base: a name declared there may no longer be declared.
    pub changed: Option<(u32, u32)>,
    /// The changed text opens or closes a scope, so the scope at the cursor
    /// may not be the one the base gives.
    pub scope_changed: bool,
}

impl Mapped {
    /// Whether `span` of file `file` touches the bytes of the base's own
    /// text that the current text changed.
    pub(crate) fn touches_changed(&self, file: FileId, span: oxabl_ast::Span) -> bool {
        let Some((from, to)) = self.changed else {
            return false;
        };
        self.ids.contains(&file) && span.start < to && from < span.end
    }

    /// Whether the base declares a name at expanded byte `virt` in text that
    /// the current text changed.
    pub(crate) fn in_changed(&self, base: &CompletionBase<'_, '_, '_>, virt: u32) -> bool {
        let Some((from, to)) = self.changed else {
            return false;
        };
        match base.navigation.expanded.resolve_offset(virt) {
            Some((file, real)) => self.ids.contains(&file) && from <= real && real < to,
            None => false,
        }
    }
}

/// Place the cursor at byte `cursor` and the window at `window_start` of
/// `text` in `base`. `None` when the cursor has no place in the analysed text:
/// an `&IF` branch the preprocessor did not take, or text the expansion lost.
/// With `reference`, the cursor is the `{` of a reference that the expansion
/// replaced: its place is right after the byte before it.
pub(crate) fn map(
    base: &CompletionBase<'_, '_, '_>,
    path: &Path,
    text: &str,
    tokens: &TokenStream<'_>,
    cursor: u32,
    window_start: u32,
    reference: bool,
) -> Option<Mapped> {
    let ids = base
        .navigation
        .file_ids(&oxabl_index::search::normalize_lexically(path));
    if ids.is_empty() {
        return None;
    }
    let old = base.text.as_bytes();
    let new = text.as_bytes();
    let (stale, prefix, suffix) = match tokens.diff_from(base.text) {
        TextDiff::Same => (false, new.len(), 0),
        TextDiff::Differs { prefix, suffix } => (true, prefix, suffix),
        TextDiff::Unknown if old == new => (false, new.len(), 0),
        TextDiff::Unknown => {
            let (prefix, suffix) = super::tokens::common_ends(old, new);
            (true, prefix, suffix)
        }
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
        if !found.is_empty() || !(reference || at as usize == old.len()) {
            return found;
        }
        if at == 0 {
            // A reference at the start of the file: where its text starts.
            if !reference {
                return found;
            }
            let expanded = &base.navigation.expanded;
            return ids
                .iter()
                .filter_map(|id| {
                    if *id == expanded.root_file() {
                        return Some(0);
                    }
                    expanded
                        .include_expansions()
                        .iter()
                        .find(|expansion| expansion.file == Some(*id))
                        .map(|expansion| expansion.expanded.start)
                })
                .collect();
        }
        // The end of the text, and a reference, are the place after the byte
        // before.
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
    let changed = stale.then(|| (prefix as u32, (old.len() - suffix) as u32));
    let scope_changed = stale && {
        let new_changed = (prefix, new.len() - suffix);
        let old_changed = (prefix, old.len() - suffix);
        opens_scope(tokens, new_changed)
            || match base.tokens {
                Some(old_tokens) => opens_scope(&TokenStream::from_slice(old_tokens), old_changed),
                None => {
                    let lexed = oxabl_lexer::tokenize(base.text);
                    opens_scope(&TokenStream::from_slice(&lexed), old_changed)
                }
            }
    };
    Some(Mapped {
        ids,
        places,
        window_places,
        stale,
        changed,
        scope_changed,
    })
}

/// Whether a token that touches the bytes `from..to` opens or closes a scope.
fn opens_scope(tokens: &TokenStream<'_>, (from, to): (usize, usize)) -> bool {
    let first = tokens.partition_point(|token| token.end < from);
    tokens
        .iter_from(first)
        .take_while(|token| token.start <= to)
        .any(|token| {
            matches!(
                token.kind,
                Kind::Procedure
                    | Kind::Function
                    | Kind::Method
                    | Kind::Constructor
                    | Kind::Destructor
                    | Kind::Class
                    | Kind::Interface
                    | Kind::KwEnum
                    | Kind::End
                    | Kind::Catch
                    | Kind::Finally
                    | Kind::On
                    | Kind::Get
                    | Kind::Set
            )
        })
}

/// The scope that holds expanded byte `virt`, past the implicit scopes of a
/// temp-table or a frame definition.
pub(crate) fn scope_at(base: &CompletionBase<'_, '_, '_>, virt: u32) -> ScopeId {
    let tree = &base.navigation.model.semantic.scope_tree;
    let mut scope = tree.innermost_at(virt);
    while matches!(
        tree.get(scope).kind(),
        ScopeKind::TempTable | ScopeKind::Frame
    ) && let Some(parent) = tree.get(scope).parent()
    {
        scope = parent;
    }
    scope
}
