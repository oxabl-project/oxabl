//! Server capabilities and position-encoding negotiation (R2, R3).
//!
//! v1 advertises what the diagnostics skeleton implements — incremental text
//! sync (with open/close) and push `publishDiagnostics` — plus whole-document
//! formatting (`document_formatting_provider`, R1), document symbols
//! (`document_symbol_provider`), folding (`folding_range_provider`), selection
//! (`selection_range_provider`), workspace symbols (`workspace_symbol_provider`), go to
//! definition (`definition_provider`), hover (`hover_provider`) and document links
//! (`document_link_provider`). Push diagnostics require no
//! capability field — the server simply sends the notification — so we
//! deliberately leave every other capability (completion, pull
//! diagnostics, *range* formatting, …) unset so clients don't offer features we
//! don't have. The handshake adds effect code lenses only for supporting clients
//! whose workspace enables strict database rules. Dynamic clients register per file.
//! It adds inlay hints for every client that supports them: a file whose
//! configuration enables no strict database rule gets an empty list, so a change
//! of `oxabl.toml` needs no new registration.

use lsp_types::{
    ClientCapabilities, DocumentLinkOptions, FoldingRangeProviderCapability,
    HoverProviderCapability, OneOf, PositionEncodingKind, SelectionRangeProviderCapability,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
};

/// Negotiate a position encoding (KTD4): prefer UTF-8 when the client's
/// `general.positionEncodings` advertises it, otherwise fall back to UTF-16
/// (the LSP default). UTF-8 lets `SourceMap`'s byte columns map directly to LSP
/// positions with no conversion.
pub fn negotiate_position_encoding(caps: &ClientCapabilities) -> PositionEncodingKind {
    let offered = caps
        .general
        .as_ref()
        .and_then(|g| g.position_encodings.as_ref());
    match offered {
        Some(encodings) if encodings.contains(&PositionEncodingKind::UTF8) => {
            PositionEncodingKind::UTF8
        }
        _ => PositionEncodingKind::UTF16,
    }
}

/// Build the v1 [`ServerCapabilities`]: incremental sync with open/close, the
/// negotiated position encoding, whole-document formatting (R1), document symbols,
/// folding, selection, workspace symbols, go to definition, hover and document links.
/// Nothing else is advertised — range formatting in particular stays unset (the engine
/// only formats whole files; see the plan's Scope Boundaries).
pub fn server_capabilities(encoding: PositionEncodingKind) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(encoding),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                ..Default::default()
            },
        )),
        // Advertise "Format Document" (R1). Range formatting is deliberately
        // left unset (`document_range_formatting_provider`): the formatter has
        // no region concept and bails whole-file.
        document_formatting_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // Each link carries its target, so there is nothing to resolve later.
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: Default::default(),
        }),
        // Explicitly leave every other capability unset so clients don't offer
        // features v1 doesn't implement (R2).
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::GeneralClientCapabilities;

    fn caps_with(encodings: Option<Vec<PositionEncodingKind>>) -> ClientCapabilities {
        ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings: encodings,
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    #[test]
    fn prefers_utf8_when_offered() {
        let caps = caps_with(Some(vec![
            PositionEncodingKind::UTF8,
            PositionEncodingKind::UTF16,
        ]));
        assert_eq!(
            negotiate_position_encoding(&caps),
            PositionEncodingKind::UTF8
        );
    }

    #[test]
    fn falls_back_to_utf16_when_only_utf16() {
        let caps = caps_with(Some(vec![PositionEncodingKind::UTF16]));
        assert_eq!(
            negotiate_position_encoding(&caps),
            PositionEncodingKind::UTF16
        );
    }

    #[test]
    fn falls_back_to_utf16_when_field_absent() {
        let caps = caps_with(None);
        assert_eq!(
            negotiate_position_encoding(&caps),
            PositionEncodingKind::UTF16
        );
        // Also when `general` itself is absent.
        let bare = ClientCapabilities::default();
        assert_eq!(
            negotiate_position_encoding(&bare),
            PositionEncodingKind::UTF16
        );
    }

    #[test]
    fn advertises_sync_formatting_and_symbols_only() {
        let caps = server_capabilities(PositionEncodingKind::UTF8);
        assert_eq!(caps.position_encoding, Some(PositionEncodingKind::UTF8));
        match caps.text_document_sync {
            Some(TextDocumentSyncCapability::Options(opts)) => {
                assert_eq!(opts.open_close, Some(true));
                assert_eq!(opts.change, Some(TextDocumentSyncKind::INCREMENTAL));
            }
            other => panic!("expected sync options, got {other:?}"),
        }
        // Whole-document formatting is advertised (R1)…
        assert_eq!(caps.document_formatting_provider, Some(OneOf::Left(true)));
        // …but range formatting is not (whole-file engine, no region concept).
        assert!(caps.document_range_formatting_provider.is_none());
        assert_eq!(caps.document_symbol_provider, Some(OneOf::Left(true)));
        assert_eq!(
            caps.folding_range_provider,
            Some(FoldingRangeProviderCapability::Simple(true))
        );
        assert_eq!(
            caps.selection_range_provider,
            Some(SelectionRangeProviderCapability::Simple(true))
        );
        assert_eq!(caps.workspace_symbol_provider, Some(OneOf::Left(true)));
        // No other feature is advertised.
        assert!(caps.code_lens_provider.is_none());
        assert_eq!(
            caps.hover_provider,
            Some(HoverProviderCapability::Simple(true))
        );
        // Inlay hints are added at the handshake, for a client that supports them.
        assert!(caps.inlay_hint_provider.is_none());
        assert!(caps.completion_provider.is_none());
        assert_eq!(caps.definition_provider, Some(OneOf::Left(true)));
        assert_eq!(
            caps.document_link_provider
                .and_then(|links| links.resolve_provider),
            Some(false)
        );
        assert!(caps.diagnostic_provider.is_none());
    }
}
