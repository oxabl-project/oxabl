//! The `&IF` branches the preprocessor does not compile, so an editor can dim
//! them.
//!
//! The server pushes `textDocument/inactiveRegions`, in the shape clangd uses,
//! to a client that sets
//! `capabilities.textDocument.inactiveRegionsCapabilities.inactiveRegions`. It
//! sends the regions with each diagnostics publish, from the same analysis:
//! the same buffer version, configuration and compilation context. A branch
//! whose condition oxabl cannot decide is never in the list. See
//! `docs/design/inactive-regions.md`.

use lsp_server::{Message, Notification};
use lsp_types::{Range, Uri};
use serde_json::{Value, json};

use crate::Server;
use crate::position::byte_to_position;

pub(crate) const METHOD: &str = "textDocument/inactiveRegions";

/// Whether the client asked for the notification at `initialize`.
pub(crate) fn client_supports(initialize_params: &Value) -> bool {
    initialize_params
        .pointer("/capabilities/textDocument/inactiveRegionsCapabilities/inactiveRegions")
        .and_then(Value::as_bool)
        .unwrap_or(false)
}

impl Server<'_> {
    /// Send the inactive regions of `uri` at `version`, `(start, end)` byte
    /// offsets in the buffer's text, to a client that asked for them.
    pub(crate) fn publish_inactive_regions(
        &self,
        uri: &Uri,
        version: i32,
        regions: impl IntoIterator<Item = (u32, u32)>,
    ) {
        if !self.inactive_regions {
            return;
        }
        let Some(doc) = self.documents.get(uri) else {
            return;
        };
        let ranges: Vec<Range> = regions
            .into_iter()
            .map(|(start, end)| Range {
                start: byte_to_position(&doc.rope, start as usize, &self.encoding),
                end: byte_to_position(&doc.rope, end as usize, &self.encoding),
            })
            .collect();
        let notification = Notification {
            method: METHOD.to_string(),
            params: json!({
                "textDocument": { "uri": uri.as_str(), "version": version },
                "regions": ranges,
            }),
        };
        let _ = self
            .connection
            .sender
            .send(Message::Notification(notification));
    }
}
