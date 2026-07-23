//! Server capabilities and position-encoding negotiation (R2, R3).
//!
//! v1 advertises what the diagnostics skeleton implements — incremental text
//! sync (with open/close) and push `publishDiagnostics` — plus whole-document
//! formatting (`document_formatting_provider`, R1). Push diagnostics require no
//! capability field — the server simply sends the notification — so we
//! deliberately leave every other capability (hover, completion, pull
//! diagnostics, *range* formatting, …) unset so clients don't offer features we
//! don't have.

use lsp_types::{
    ClientCapabilities, OneOf, PositionEncodingKind, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
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
/// negotiated position encoding, and whole-document formatting (R1). Nothing
/// else is advertised — range formatting in particular stays unset (the engine
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
    fn advertises_incremental_open_close_only() {
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
        // No other feature is advertised.
        assert!(caps.hover_provider.is_none());
        assert!(caps.completion_provider.is_none());
        assert!(caps.definition_provider.is_none());
        assert!(caps.diagnostic_provider.is_none());
    }
}
