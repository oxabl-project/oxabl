//! `textDocument/documentSymbol`: the pipeline's outline rendered as LSP
//! symbols, and the per-document cache of the last outline.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex, PoisonError};

use lsp_types::{
    DocumentSymbol, DocumentSymbolResponse, Location, PositionEncodingKind, Range,
    SymbolInformation, SymbolKind, Uri,
};
use oxabl_common::InternalPanic;
use oxabl_pipeline::{IncludeOutline, OutlineKind, OutlineNode, OutlineSymbol, Span};
use ropey::Rope;

use oxabl_daemon::canonical_path;

use crate::path_to_uri;
use crate::position::byte_to_position;

fn symbol_kind(kind: OutlineKind) -> SymbolKind {
    match kind {
        OutlineKind::Class => SymbolKind::CLASS,
        OutlineKind::Interface => SymbolKind::INTERFACE,
        OutlineKind::Procedure | OutlineKind::Function => SymbolKind::FUNCTION,
        OutlineKind::Method => SymbolKind::METHOD,
        OutlineKind::Constructor | OutlineKind::Destructor => SymbolKind::CONSTRUCTOR,
        OutlineKind::Property => SymbolKind::PROPERTY,
        OutlineKind::Event | OutlineKind::Trigger => SymbolKind::EVENT,
        OutlineKind::Variable | OutlineKind::Parameter => SymbolKind::VARIABLE,
        OutlineKind::TempTable | OutlineKind::Dataset => SymbolKind::STRUCT,
        OutlineKind::Field => SymbolKind::FIELD,
        OutlineKind::Buffer
        | OutlineKind::DataSource
        | OutlineKind::Query
        | OutlineKind::Stream
        | OutlineKind::Frame
        | OutlineKind::Widget => SymbolKind::OBJECT,
        OutlineKind::Include => SymbolKind::FILE,
        OutlineKind::PreprocessorDefine => SymbolKind::CONSTANT,
        OutlineKind::Label => SymbolKind::NAMESPACE,
        OutlineKind::Enum => SymbolKind::ENUM,
        OutlineKind::EnumMember => SymbolKind::ENUM_MEMBER,
    }
}

/// Renders an outline for a client.
///
/// A symbol that an include holds has no place in the buffer, and a
/// hierarchical symbol has only a range of the buffer. So the range of such a
/// symbol is the name of the nearest symbol of the buffer above it, which is
/// the include reference. A flat symbol has a location of its own, in the
/// include file.
struct Render<'a> {
    rope: &'a Rope,
    encoding: &'a PositionEncodingKind,
    outline: &'a IncludeOutline,
    /// The directory that the path of an include shows relative to.
    base: Option<&'a Path>,
    /// The text of each include file as a rope, made when a flat symbol needs it.
    ropes: RefCell<HashMap<usize, Rope>>,
}

impl Render<'_> {
    fn range(&self, span: Span) -> Range {
        Range {
            start: byte_to_position(self.rope, span.start as usize, self.encoding),
            end: byte_to_position(self.rope, span.end as usize, self.encoding),
        }
    }

    /// The range of `span` in include file `file`.
    fn file_range(&self, file: usize, span: Span) -> Range {
        let mut ropes = self.ropes.borrow_mut();
        let rope = ropes
            .entry(file)
            .or_insert_with(|| Rope::from_str(&self.outline.files[file].text));
        Range {
            start: byte_to_position(rope, span.start as usize, self.encoding),
            end: byte_to_position(rope, span.end as usize, self.encoding),
        }
    }

    /// What follows the name: the path of the file that an include shows,
    /// else the symbol's own detail.
    fn detail(&self, node: &OutlineNode) -> Option<String> {
        match node.opens {
            Some(file) => {
                let path = &self.outline.files[file].path;
                let shown = self
                    .base
                    .and_then(|base| path.strip_prefix(base).ok())
                    .unwrap_or(path);
                Some(shown.display().to_string())
            }
            None => node.detail.clone(),
        }
    }

    /// `anchor` is the selection of the nearest symbol of the buffer above.
    fn nested(&self, nodes: &[OutlineNode], anchor: Option<Span>) -> Vec<DocumentSymbol> {
        nodes
            .iter()
            .filter_map(|node| {
                let (range, selection) = match node.file {
                    None => (self.range(node.span), self.range(node.selection_span)),
                    Some(_) => {
                        let anchor = self.range(anchor?);
                        (anchor, anchor)
                    }
                };
                let anchor = if node.file.is_none() {
                    Some(node.selection_span)
                } else {
                    anchor
                };
                #[allow(deprecated)] // lsp-types makes the field mandatory
                Some(DocumentSymbol {
                    name: node.name.clone(),
                    detail: self.detail(node),
                    kind: symbol_kind(node.kind),
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: selection,
                    children: (!node.children.is_empty())
                        .then(|| self.nested(&node.children, anchor)),
                })
            })
            .collect()
    }

    fn flat(
        &self,
        uri: &Uri,
        nodes: &[OutlineNode],
        anchor: Option<Span>,
        container: Option<&str>,
        out: &mut Vec<SymbolInformation>,
    ) {
        for node in nodes {
            let location = match node.file {
                None => Some(Location {
                    uri: uri.clone(),
                    range: self.range(node.span),
                }),
                Some(file) => {
                    path_to_uri(&canonical_path(&self.outline.files[file].path)).map(|uri| {
                        Location {
                            uri,
                            range: self.file_range(file, node.span),
                        }
                    })
                }
            };
            // A file that has no URI keeps its symbols at the reference.
            let location = location.or_else(|| {
                anchor.map(|anchor| Location {
                    uri: uri.clone(),
                    range: self.range(anchor),
                })
            });
            let anchor = if node.file.is_none() {
                Some(node.selection_span)
            } else {
                anchor
            };
            if let Some(location) = location {
                #[allow(deprecated)] // lsp-types makes the field mandatory
                out.push(SymbolInformation {
                    name: node.name.clone(),
                    kind: symbol_kind(node.kind),
                    tags: None,
                    deprecated: None,
                    location,
                    container_name: container.map(str::to_owned),
                });
            }
            self.flat(uri, &node.children, anchor, Some(&node.name), out);
        }
    }
}

/// Render `symbols` for a client: a tree when it supports hierarchical
/// symbols, else a pre-order list with `containerName`.
pub(crate) fn to_response(
    symbols: &[OutlineSymbol],
    uri: &Uri,
    rope: &Rope,
    encoding: &PositionEncodingKind,
    hierarchical: bool,
) -> DocumentSymbolResponse {
    let outline = IncludeOutline {
        symbols: symbols
            .iter()
            .cloned()
            .map(|symbol| OutlineNode::from_symbol(symbol, None))
            .collect(),
        files: Vec::new(),
    };
    to_response_with_includes(&outline, uri, rope, encoding, hierarchical, None)
}

/// [`to_response`] for an outline that holds the symbols of include files.
/// `base` is the directory that the path of an include shows relative to.
pub(crate) fn to_response_with_includes(
    outline: &IncludeOutline,
    uri: &Uri,
    rope: &Rope,
    encoding: &PositionEncodingKind,
    hierarchical: bool,
    base: Option<&Path>,
) -> DocumentSymbolResponse {
    let render = Render {
        rope,
        encoding,
        outline,
        base,
        ropes: RefCell::default(),
    };
    if hierarchical {
        DocumentSymbolResponse::Nested(render.nested(&outline.symbols, None))
    } else {
        let mut out = Vec::new();
        render.flat(uri, &outline.symbols, None, None, &mut out);
        DocumentSymbolResponse::Flat(out)
    }
}

/// The outline of one buffer version with its includes, made once. Requests
/// for the same version wait on the first.
#[derive(Default)]
pub(crate) struct IncludeSlot {
    result: Mutex<Option<Result<Arc<IncludeOutline>, String>>>,
}

impl IncludeSlot {
    /// The stored answer, or the result of `compute`, which is stored.
    pub(crate) fn get_or_compute(
        &self,
        compute: impl FnOnce() -> Result<IncludeOutline, String>,
    ) -> Result<Arc<IncludeOutline>, String> {
        let mut result = self.result.lock().unwrap_or_else(PoisonError::into_inner);
        result
            .get_or_insert_with(|| compute().map(Arc::new))
            .clone()
    }
}

/// The last outline of each open document, with the version it was made for.
#[derive(Default)]
pub(crate) struct OutlineCache {
    entries: HashMap<Uri, (i32, Vec<OutlineSymbol>)>,
    /// The outlines with includes. An include is read from other files, so
    /// any change that [`Self::invalidate`] sees drops all of them.
    includes: Mutex<HashMap<Uri, (i32, Arc<IncludeSlot>)>>,
}

impl OutlineCache {
    /// The cached outline for `version`, else the result of `compute`. A
    /// contained panic is not cached.
    pub(crate) fn get_or_compute(
        &mut self,
        uri: &Uri,
        version: i32,
        compute: impl FnOnce() -> Result<Vec<OutlineSymbol>, InternalPanic>,
    ) -> Result<&[OutlineSymbol], InternalPanic> {
        if self
            .entries
            .get(uri)
            .is_none_or(|(cached, _)| *cached != version)
        {
            let symbols = compute()?;
            self.entries.insert(uri.clone(), (version, symbols));
        }
        Ok(&self.entries[uri].1)
    }

    pub(crate) fn invalidate(&mut self, uri: &Uri) {
        self.entries.remove(uri);
        self.invalidate_includes();
    }

    /// Drop every outline that holds include symbols: a file that a buffer
    /// includes, the configuration or the schema changed.
    pub(crate) fn invalidate_includes(&self) {
        self.includes
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .clear();
    }

    /// The slot for `uri` at `version`.
    pub(crate) fn include_slot(&self, uri: &Uri, version: i32) -> Arc<IncludeSlot> {
        let mut slots = self.includes.lock().unwrap_or_else(PoisonError::into_inner);
        match slots.get(uri) {
            Some((cached, slot)) if *cached == version => Arc::clone(slot),
            _ => {
                let slot = Arc::new(IncludeSlot::default());
                slots.insert(uri.clone(), (version, Arc::clone(&slot)));
                slot
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use oxabl_ast::Span;

    use super::*;

    fn symbol(name: &str) -> OutlineSymbol {
        OutlineSymbol {
            name: name.to_string(),
            detail: None,
            kind: OutlineKind::Variable,
            span: Span { start: 0, end: 1 },
            selection_span: Span { start: 0, end: 1 },
            children: Vec::new(),
        }
    }

    // The server cannot be shown to skip the walk over the wire: a changed
    // buffer always drops the entry, so only a counter can see the hit.
    #[test]
    fn same_version_does_not_recompute() {
        let uri = Uri::from_str("file:///a.p").unwrap();
        let mut cache = OutlineCache::default();
        let mut runs = 0;
        for version in [1, 1, 2, 2, 2] {
            let symbols = cache
                .get_or_compute(&uri, version, || {
                    runs += 1;
                    Ok(vec![symbol("x")])
                })
                .unwrap();
            assert_eq!(symbols.len(), 1);
        }
        assert_eq!(runs, 2, "one run for each version");
        cache.invalidate(&uri);
        cache
            .get_or_compute(&uri, 2, || {
                runs += 1;
                Ok(Vec::new())
            })
            .unwrap();
        assert_eq!(runs, 3, "an invalidated entry is computed again");
    }

    // The walker may not exist yet, so the mapping is checked on a tree built
    // by hand; `document_symbol_e2e` repeats it over the wire.
    #[test]
    fn mapping_uses_the_encoding_and_the_container() {
        let uri = Uri::from_str("file:///a.p").unwrap();
        // "😀" is 4 bytes and 2 UTF-16 units; the child starts after it.
        let rope = Rope::from_str("😀 PROCEDURE p:\nEND.\n");
        let mut parent = symbol("p");
        parent.kind = OutlineKind::Procedure;
        parent.span = Span { start: 5, end: 21 };
        parent.selection_span = Span { start: 15, end: 16 };
        let mut child = symbol("v");
        child.span = Span { start: 17, end: 20 };
        child.selection_span = Span { start: 17, end: 18 };
        parent.children.push(child);
        let symbols = [parent];

        let utf16 = PositionEncodingKind::UTF16;
        let DocumentSymbolResponse::Nested(tree) = to_response(&symbols, &uri, &rope, &utf16, true)
        else {
            panic!("expected a tree");
        };
        assert_eq!(tree[0].kind, SymbolKind::FUNCTION);
        assert_eq!(tree[0].range.start.character, 3);
        assert_eq!(tree[0].selection_range.start.character, 13);
        assert_eq!(
            tree[0].children.as_ref().unwrap()[0].kind,
            SymbolKind::VARIABLE
        );

        let utf8 = PositionEncodingKind::UTF8;
        let DocumentSymbolResponse::Nested(tree) = to_response(&symbols, &uri, &rope, &utf8, true)
        else {
            panic!("expected a tree");
        };
        assert_eq!(tree[0].range.start.character, 5);

        let DocumentSymbolResponse::Flat(list) = to_response(&symbols, &uri, &rope, &utf16, false)
        else {
            panic!("expected a list");
        };
        assert_eq!(list.len(), 2);
        assert_eq!(list[0].container_name, None);
        assert_eq!(list[1].container_name.as_deref(), Some("p"));
    }
}
