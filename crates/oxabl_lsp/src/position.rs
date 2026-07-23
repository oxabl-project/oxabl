//! Encoding-aware conversion between LSP [`Position`]s and offsets into a
//! [`ropey::Rope`] (KTD4).
//!
//! LSP `Position.character` is a column measured in the negotiated position
//! encoding's units — UTF-8 **bytes** or UTF-16 **code units** — counted from
//! the start of the line. The document sync path (U2) needs char indices for
//! rope splicing; the diagnostics path (U5) needs the inverse, a byte offset →
//! `Position`. Both live here so the encoding logic is defined once and shared.
//!
//! Every conversion clamps out-of-range inputs to the rope's bounds rather than
//! panicking: a client can legitimately send a position one past the end (an
//! append), and a stale/racy edit must never crash the server.

use lsp_types::{Position, PositionEncodingKind};
use ropey::Rope;

/// Convert an LSP [`Position`] to a **char index** into `rope`, interpreting
/// `position.character` in `encoding`. Used to splice incremental edits.
pub fn position_to_char(rope: &Rope, position: Position, encoding: &PositionEncodingKind) -> usize {
    let line = position.line as usize;
    if line >= rope.len_lines() {
        return rope.len_chars();
    }
    let line_start_char = rope.line_to_char(line);
    // The exclusive char index at which the *next* line begins bounds how far
    // `character` may advance within this line.
    let next_line_start_char = if line + 1 < rope.len_lines() {
        rope.line_to_char(line + 1)
    } else {
        rope.len_chars()
    };
    let character = position.character as usize;

    if *encoding == PositionEncodingKind::UTF8 {
        // `character` is a byte offset within the line.
        let line_start_byte = rope.char_to_byte(line_start_char);
        let target_byte = (line_start_byte + character).min(rope.len_bytes());
        let target_char = rope.byte_to_char(target_byte);
        target_char.min(next_line_start_char)
    } else {
        // UTF-16 code-unit offset within the line.
        let line_start_cu = rope.char_to_utf16_cu(line_start_char);
        let target_cu = (line_start_cu + character).min(rope.len_utf16_cu());
        let target_char = rope.utf16_cu_to_char(target_cu);
        target_char.min(next_line_start_char)
    }
}

/// Convert an LSP [`Position`] to a **byte offset** into `rope`.
pub fn position_to_byte(rope: &Rope, position: Position, encoding: &PositionEncodingKind) -> usize {
    rope.char_to_byte(position_to_char(rope, position, encoding))
}

/// Convert a **byte offset** into `rope` to an LSP [`Position`] in `encoding`.
/// Used to place diagnostics (U5). Offsets past the end clamp to the document
/// end.
pub fn byte_to_position(
    rope: &Rope,
    byte_offset: usize,
    encoding: &PositionEncodingKind,
) -> Position {
    let byte_offset = byte_offset.min(rope.len_bytes());
    let char_idx = rope.byte_to_char(byte_offset);
    let line = rope.char_to_line(char_idx);
    let line_start_char = rope.line_to_char(line);

    let character = if *encoding == PositionEncodingKind::UTF8 {
        let line_start_byte = rope.char_to_byte(line_start_char);
        byte_offset - line_start_byte
    } else {
        rope.char_to_utf16_cu(char_idx) - rope.char_to_utf16_cu(line_start_char)
    };

    Position {
        line: line as u32,
        character: character as u32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn utf8() -> PositionEncodingKind {
        PositionEncodingKind::UTF8
    }
    fn utf16() -> PositionEncodingKind {
        PositionEncodingKind::UTF16
    }

    #[test]
    fn ascii_round_trips_both_encodings() {
        let rope = Rope::from_str("abc\ndef\nghi");
        for enc in [utf8(), utf16()] {
            let pos = Position {
                line: 1,
                character: 2,
            };
            let byte = position_to_byte(&rope, pos, &enc);
            assert_eq!(byte, 6, "enc {enc:?}"); // 'f'
            assert_eq!(byte_to_position(&rope, byte, &enc), pos, "enc {enc:?}");
        }
    }

    #[test]
    fn multibyte_char_column_differs_by_encoding() {
        // '😀' is 4 UTF-8 bytes and 2 UTF-16 code units; text after it on the
        // same line lands at a different `character` per encoding.
        let rope = Rope::from_str("x😀y");
        // The 'y' is at char index 2.
        let y_char = 2;
        let y_byte = rope.char_to_byte(y_char);

        // UTF-8: column is a byte offset → 1 ('x') + 4 ('😀') = 5.
        let p8 = byte_to_position(&rope, y_byte, &utf8());
        assert_eq!(
            p8,
            Position {
                line: 0,
                character: 5
            }
        );
        assert_eq!(position_to_byte(&rope, p8, &utf8()), y_byte);

        // UTF-16: column is code units → 1 ('x') + 2 ('😀') = 3.
        let p16 = byte_to_position(&rope, y_byte, &utf16());
        assert_eq!(
            p16,
            Position {
                line: 0,
                character: 3
            }
        );
        assert_eq!(position_to_byte(&rope, p16, &utf16()), y_byte);
    }

    #[test]
    fn position_past_end_clamps() {
        let rope = Rope::from_str("ab");
        let pos = Position {
            line: 9,
            character: 9,
        };
        assert_eq!(position_to_char(&rope, pos, &utf8()), rope.len_chars());
    }
}
