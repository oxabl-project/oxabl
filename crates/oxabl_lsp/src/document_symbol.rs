//! `textDocument/documentSymbol`: the pipeline's outline rendered as LSP
//! symbols, and the per-document cache of the last outline.

use std::collections::HashMap;

use lsp_types::{
    DocumentSymbol, DocumentSymbolResponse, Location, PositionEncodingKind, Range,
    SymbolInformation, SymbolKind, Uri,
};
use oxabl_common::InternalPanic;
use oxabl_pipeline::{OutlineKind, OutlineSymbol};
use ropey::Rope;

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

struct Render<'a> {
    rope: &'a Rope,
    encoding: &'a PositionEncodingKind,
}

impl Render<'_> {
    fn range(&self, start: u32, end: u32) -> Range {
        Range {
            start: byte_to_position(self.rope, start as usize, self.encoding),
            end: byte_to_position(self.rope, end as usize, self.encoding),
        }
    }

    fn nested(&self, symbols: &[OutlineSymbol]) -> Vec<DocumentSymbol> {
        symbols
            .iter()
            .map(|symbol| {
                #[allow(deprecated)] // lsp-types makes the field mandatory
                DocumentSymbol {
                    name: symbol.name.clone(),
                    detail: symbol.detail.clone(),
                    kind: symbol_kind(symbol.kind),
                    tags: None,
                    deprecated: None,
                    range: self.range(symbol.span.start, symbol.span.end),
                    selection_range: self
                        .range(symbol.selection_span.start, symbol.selection_span.end),
                    children: (!symbol.children.is_empty()).then(|| self.nested(&symbol.children)),
                }
            })
            .collect()
    }

    fn flat(
        &self,
        uri: &Uri,
        symbols: &[OutlineSymbol],
        container: Option<&str>,
        out: &mut Vec<SymbolInformation>,
    ) {
        for symbol in symbols {
            #[allow(deprecated)] // lsp-types makes the field mandatory
            out.push(SymbolInformation {
                name: symbol.name.clone(),
                kind: symbol_kind(symbol.kind),
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: self.range(symbol.span.start, symbol.span.end),
                },
                container_name: container.map(str::to_owned),
            });
            self.flat(uri, &symbol.children, Some(&symbol.name), out);
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
    let render = Render { rope, encoding };
    if hierarchical {
        DocumentSymbolResponse::Nested(render.nested(symbols))
    } else {
        let mut out = Vec::new();
        render.flat(uri, symbols, None, &mut out);
        DocumentSymbolResponse::Flat(out)
    }
}

/// The last outline of each open document, with the version it was made for.
#[derive(Default)]
pub(crate) struct OutlineCache {
    entries: HashMap<Uri, (i32, Vec<OutlineSymbol>)>,
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
