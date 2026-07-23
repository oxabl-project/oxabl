//! Open-buffer store (R4, R5).
//!
//! Each open document is held as a [`ropey::Rope`] plus its LSP version,
//! keyed by URI. `didChange` applies incremental range edits to the rope in
//! order; a change with no range replaces the whole buffer (full-document
//! sync, R5). `didClose` drops the entry.

use std::collections::HashMap;

use lsp_types::{PositionEncodingKind, TextDocumentContentChangeEvent, Uri};
use ropey::Rope;

use crate::position::position_to_char;

/// A single open buffer.
#[derive(Debug, Clone)]
pub struct Document {
    pub rope: Rope,
    pub version: i32,
}

impl Document {
    /// The buffer's current text (materialized from the rope).
    pub fn text(&self) -> String {
        self.rope.to_string()
    }
}

/// URI → open [`Document`] map.
#[derive(Debug, Default)]
pub struct DocumentStore {
    docs: HashMap<Uri, Document>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Handle `didOpen`: insert (or replace) the buffer.
    pub fn open(&mut self, uri: Uri, version: i32, text: &str) {
        self.docs.insert(
            uri,
            Document {
                rope: Rope::from_str(text),
                version,
            },
        );
    }

    /// Handle `didClose`: drop the buffer. Returns `true` if it was present.
    pub fn close(&mut self, uri: &Uri) -> bool {
        self.docs.remove(uri).is_some()
    }

    /// Handle `didChange`: apply the content changes to the buffer in order and
    /// bump its version. A change with a `range` splices that range; a change
    /// without a range replaces the entire buffer (full-document sync, R5).
    ///
    /// Returns `false` if the URI is not open (a stray change for a closed or
    /// never-opened document is ignored, not fatal).
    pub fn change(
        &mut self,
        uri: &Uri,
        version: i32,
        changes: &[TextDocumentContentChangeEvent],
        encoding: &PositionEncodingKind,
    ) -> bool {
        let Some(doc) = self.docs.get_mut(uri) else {
            return false;
        };
        for change in changes {
            match change.range {
                Some(range) => {
                    let start = position_to_char(&doc.rope, range.start, encoding);
                    let end = position_to_char(&doc.rope, range.end, encoding);
                    // Guard against an inverted range from a malformed client.
                    let (start, end) = if start <= end {
                        (start, end)
                    } else {
                        (end, start)
                    };
                    doc.rope.remove(start..end);
                    doc.rope.insert(start, &change.text);
                }
                None => {
                    doc.rope = Rope::from_str(&change.text);
                }
            }
        }
        doc.version = version;
        true
    }

    /// Look up an open document.
    pub fn get(&self, uri: &Uri) -> Option<&Document> {
        self.docs.get(uri)
    }

    /// Number of open documents.
    pub fn len(&self) -> usize {
        self.docs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.docs.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, Range};

    fn uri() -> Uri {
        use std::str::FromStr;
        Uri::from_str("file:///buf.p").unwrap()
    }

    fn edit(range: Option<Range>, text: &str) -> TextDocumentContentChangeEvent {
        TextDocumentContentChangeEvent {
            range,
            range_length: None,
            text: text.to_string(),
        }
    }

    fn range(sl: u32, sc: u32, el: u32, ec: u32) -> Range {
        Range {
            start: Position {
                line: sl,
                character: sc,
            },
            end: Position {
                line: el,
                character: ec,
            },
        }
    }

    #[test]
    fn open_then_single_insert_bumps_version() {
        let mut store = DocumentStore::new();
        store.open(uri(), 1, "hello world");
        // Insert "beautiful " before "world" (char/byte col 6 on line 0).
        let ok = store.change(
            &uri(),
            2,
            &[edit(Some(range(0, 6, 0, 6)), "beautiful ")],
            &PositionEncodingKind::UTF8,
        );
        assert!(ok);
        let doc = store.get(&uri()).unwrap();
        assert_eq!(doc.text(), "hello beautiful world");
        assert_eq!(doc.version, 2);
    }

    #[test]
    fn multi_range_edits_apply_in_order() {
        let mut store = DocumentStore::new();
        store.open(uri(), 1, "AAAA\nBBBB");
        // Two edits in one didChange: replace first line's start, then insert on
        // line 2. Applied in array order against the evolving rope.
        store.change(
            &uri(),
            2,
            &[
                edit(Some(range(0, 0, 0, 2)), "xx"), // "xxAA\nBBBB"
                edit(Some(range(1, 4, 1, 4)), "!"),  // append '!' to line 2
            ],
            &PositionEncodingKind::UTF8,
        );
        assert_eq!(store.get(&uri()).unwrap().text(), "xxAA\nBBBB!");
    }

    #[test]
    fn multibyte_edit_maps_under_both_encodings() {
        // Insert right after a 4-byte / 2-cu emoji. The same logical position is
        // a different `character` per encoding, but both must splice correctly.
        for (enc, col) in [
            (PositionEncodingKind::UTF8, 5u32),  // after "x😀" in bytes: 1 + 4
            (PositionEncodingKind::UTF16, 3u32), // in code units: 1 + 2
        ] {
            let mut store = DocumentStore::new();
            store.open(uri(), 1, "x😀y");
            store.change(&uri(), 2, &[edit(Some(range(0, col, 0, col)), "Z")], &enc);
            assert_eq!(store.get(&uri()).unwrap().text(), "x😀Zy", "enc {enc:?}");
        }
    }

    #[test]
    fn full_text_change_replaces_buffer() {
        let mut store = DocumentStore::new();
        store.open(uri(), 1, "old contents");
        store.change(
            &uri(),
            5,
            &[edit(None, "brand new contents")],
            &PositionEncodingKind::UTF8,
        );
        let doc = store.get(&uri()).unwrap();
        assert_eq!(doc.text(), "brand new contents");
        assert_eq!(doc.version, 5);
    }

    #[test]
    fn close_drops_entry() {
        let mut store = DocumentStore::new();
        store.open(uri(), 1, "x");
        assert_eq!(store.len(), 1);
        assert!(store.close(&uri()));
        assert!(store.get(&uri()).is_none());
        assert!(store.is_empty());
        // Closing again is a no-op.
        assert!(!store.close(&uri()));
    }

    #[test]
    fn scripted_open_change_change_close_stays_consistent() {
        let mut store = DocumentStore::new();
        store.open(uri(), 1, "DEFINE VARIABLE x AS INTEGER.");
        store.change(
            &uri(),
            2,
            &[edit(Some(range(0, 29, 0, 29)), "\nx = 1.")],
            &PositionEncodingKind::UTF8,
        );
        assert_eq!(
            store.get(&uri()).unwrap().text(),
            "DEFINE VARIABLE x AS INTEGER.\nx = 1."
        );
        store.change(
            &uri(),
            3,
            &[edit(Some(range(1, 0, 1, 1)), "y")], // rename x→y on line 2
            &PositionEncodingKind::UTF8,
        );
        assert_eq!(
            store.get(&uri()).unwrap().text(),
            "DEFINE VARIABLE x AS INTEGER.\ny = 1."
        );
        assert!(store.close(&uri()));
        assert!(store.is_empty());
    }
}
