//! Source file encodings: reading a file's bytes into text and writing text back.
//!
//! UTF-8 is the default and the zero-cost path: the bytes are validated in place
//! and become the `String` without a copy. A single-byte encoding costs one scan
//! for non-ASCII bytes and a copy only when the file has some.

use std::io;
use std::path::Path;

use schemars::JsonSchema;
use serde::Deserialize;

const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

/// The character encoding of ABL source files on disk
/// (`[workspace.sources].encoding`).
///
/// Every encoding decodes to the same Unicode text in memory; spans and offsets
/// the tools report are byte offsets into that text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Deserialize, JsonSchema)]
pub enum SourceEncoding {
    /// UTF-8. A file with invalid UTF-8 cannot be read.
    #[default]
    #[serde(rename = "utf-8", alias = "utf8")]
    Utf8,
    /// ISO 8859-1 (Latin-1): every byte is the code point of the same value.
    #[serde(rename = "iso-8859-1", alias = "latin1")]
    Iso8859_1,
    /// Windows code page 1252. Every byte decodes; the five bytes the code page
    /// leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D) become the C1 control of
    /// the same value.
    #[serde(rename = "windows-1252", alias = "cp1252")]
    Windows1252,
}

impl SourceEncoding {
    /// The name `oxabl.toml` uses for this encoding.
    pub fn name(self) -> &'static str {
        match self {
            SourceEncoding::Utf8 => "utf-8",
            SourceEncoding::Iso8859_1 => "iso-8859-1",
            SourceEncoding::Windows1252 => "windows-1252",
        }
    }

    fn decode(self, bytes: Vec<u8>) -> io::Result<String> {
        match self {
            SourceEncoding::Utf8 => String::from_utf8(bytes).map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "not valid UTF-8 (invalid byte at offset {}); set \
                         `[workspace.sources] encoding` for a single-byte source",
                        e.utf8_error().valid_up_to()
                    ),
                )
            }),
            _ if bytes.is_ascii() => Ok(ascii_to_string(bytes)),
            SourceEncoding::Iso8859_1 => Ok(bytes.iter().map(|&b| char::from(b)).collect()),
            SourceEncoding::Windows1252 => Ok(encoding_rs::WINDOWS_1252
                .decode_without_bom_handling(&bytes)
                .0
                .into_owned()),
        }
    }

    fn encode(self, text: &str) -> io::Result<Vec<u8>> {
        let unmappable = |c: char| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("`{c}` cannot be written as {}", self.name()),
            )
        };
        match self {
            SourceEncoding::Utf8 => Ok(text.as_bytes().to_vec()),
            SourceEncoding::Iso8859_1 => text
                .chars()
                .map(|c| u8::try_from(u32::from(c)).map_err(|_| unmappable(c)))
                .collect(),
            SourceEncoding::Windows1252 => {
                let (bytes, _, had_unmappable) = encoding_rs::WINDOWS_1252.encode(text);
                if had_unmappable {
                    let culprit = text
                        .chars()
                        .find(|c| {
                            let mut buf = [0; 4];
                            encoding_rs::WINDOWS_1252.encode(c.encode_utf8(&mut buf)).2
                        })
                        .unwrap_or(char::REPLACEMENT_CHARACTER);
                    return Err(unmappable(culprit));
                }
                Ok(bytes.into_owned())
            }
        }
    }
}

fn ascii_to_string(bytes: Vec<u8>) -> String {
    match String::from_utf8(bytes) {
        Ok(text) => text,
        Err(_) => unreachable!("ASCII is valid UTF-8"),
    }
}

/// How a run turns file bytes into text: the configured
/// [`encoding`](Self::encoding) and an optional [`fallback`](Self::fallback).
///
/// 1. A leading UTF-8 byte order mark (`EF BB BF`) means UTF-8, whatever
///    `encoding` says, and is removed from the text.
/// 2. Otherwise the bytes are decoded as `encoding`.
/// 3. If that fails, they are decoded as `fallback` when one is set. Only UTF-8
///    can fail, so a fallback matters when `encoding` is UTF-8.
///
/// A failure of every step is an [`io::ErrorKind::InvalidData`] error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SourceDecoding {
    pub encoding: SourceEncoding,
    pub fallback: Option<SourceEncoding>,
}

impl SourceDecoding {
    pub fn new(encoding: SourceEncoding, fallback: Option<SourceEncoding>) -> Self {
        SourceDecoding { encoding, fallback }
    }

    /// Decode `bytes` as the rules on [`SourceDecoding`] say.
    pub fn decode(&self, mut bytes: Vec<u8>) -> io::Result<DecodedSource> {
        if bytes.starts_with(&UTF8_BOM) {
            bytes.drain(..UTF8_BOM.len());
            return Ok(DecodedSource {
                text: SourceEncoding::Utf8.decode(bytes)?,
                encoding: SourceEncoding::Utf8,
                bom: true,
            });
        }
        let Some(fallback) = self.fallback else {
            return self.decoded(self.encoding, bytes);
        };
        match self.encoding {
            SourceEncoding::Utf8 => match String::from_utf8(bytes) {
                Ok(text) => Ok(DecodedSource {
                    text,
                    encoding: SourceEncoding::Utf8,
                    bom: false,
                }),
                Err(e) => self.decoded(fallback, e.into_bytes()),
            },
            encoding => self.decoded(encoding, bytes),
        }
    }

    fn decoded(&self, encoding: SourceEncoding, bytes: Vec<u8>) -> io::Result<DecodedSource> {
        Ok(DecodedSource {
            text: encoding.decode(bytes)?,
            encoding,
            bom: false,
        })
    }

    /// Read and decode the file at `path`.
    pub fn read_file(&self, path: &Path) -> io::Result<DecodedSource> {
        self.decode(std::fs::read(path)?)
    }
}

/// A decoded source file, with what is needed to write text back the way the
/// file was stored.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodedSource {
    /// The file's text. Never starts with a byte order mark.
    pub text: String,
    /// The encoding the bytes were decoded as: the configured one, or the
    /// fallback when that was needed.
    pub encoding: SourceEncoding,
    /// Whether the file started with a UTF-8 byte order mark.
    pub bom: bool,
}

impl DecodedSource {
    /// `text` as bytes in this file's own encoding, with its byte order mark if
    /// it had one. Fails when `text` holds a character the encoding cannot
    /// represent.
    pub fn encode(&self, text: &str) -> io::Result<Vec<u8>> {
        let mut bytes = self.encoding.encode(text)?;
        if self.bom {
            bytes.splice(0..0, UTF8_BOM);
        }
        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn decoding(encoding: SourceEncoding, fallback: Option<SourceEncoding>) -> SourceDecoding {
        SourceDecoding::new(encoding, fallback)
    }

    #[test]
    fn utf8_is_the_default_and_rejects_invalid_bytes() {
        let d = SourceDecoding::default();
        assert_eq!(d.decode("caf\u{e9}".into()).unwrap().text, "caf\u{e9}");
        let err = d.decode(b"caf\xe9".to_vec()).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("encoding"), "{err}");
    }

    #[test]
    fn a_utf8_bom_is_removed_under_every_encoding() {
        for encoding in [
            SourceEncoding::Utf8,
            SourceEncoding::Iso8859_1,
            SourceEncoding::Windows1252,
        ] {
            let decoded = decoding(encoding, None)
                .decode(b"\xef\xbb\xbfx = \xc3\xa9.".to_vec())
                .unwrap();
            assert_eq!(decoded.text, "x = \u{e9}.");
            assert!(decoded.bom);
            assert_eq!(decoded.encoding, SourceEncoding::Utf8);
        }
    }

    #[test]
    fn windows_1252_maps_the_c1_range_to_punctuation() {
        let decoded = decoding(SourceEncoding::Windows1252, None)
            .decode(b"\x80 \x93q\x94 \xe9".to_vec())
            .unwrap();
        assert_eq!(decoded.text, "\u{20ac} \u{201c}q\u{201d} \u{e9}");
    }

    #[test]
    fn iso_8859_1_maps_every_byte_to_its_code_point() {
        let decoded = decoding(SourceEncoding::Iso8859_1, None)
            .decode(b"\x80\xe9".to_vec())
            .unwrap();
        assert_eq!(decoded.text, "\u{80}\u{e9}");
    }

    #[test]
    fn the_fallback_takes_over_when_utf8_fails() {
        let d = decoding(SourceEncoding::Utf8, Some(SourceEncoding::Windows1252));
        let latin = d.decode(b"caf\xe9".to_vec()).unwrap();
        assert_eq!(latin.text, "caf\u{e9}");
        assert_eq!(latin.encoding, SourceEncoding::Windows1252);
        let utf8 = d.decode("caf\u{e9}".into()).unwrap();
        assert_eq!(utf8.text, "caf\u{e9}");
        assert_eq!(utf8.encoding, SourceEncoding::Utf8);
    }

    #[test]
    fn encode_round_trips_the_stored_bytes() {
        for (encoding, bytes) in [
            (SourceEncoding::Windows1252, b"\x93caf\xe9\x94".to_vec()),
            (SourceEncoding::Iso8859_1, b"caf\xe9\x80".to_vec()),
            (SourceEncoding::Utf8, "caf\u{e9}".as_bytes().to_vec()),
        ] {
            let decoded = decoding(encoding, None).decode(bytes.clone()).unwrap();
            assert_eq!(decoded.encode(&decoded.text).unwrap(), bytes);
        }
        let bom = decoding(SourceEncoding::Utf8, None)
            .decode(b"\xef\xbb\xbfa".to_vec())
            .unwrap();
        assert_eq!(bom.encode("a").unwrap(), b"\xef\xbb\xbfa");
    }

    #[test]
    fn encode_refuses_a_character_the_encoding_lacks() {
        let decoded = decoding(SourceEncoding::Windows1252, None)
            .decode(b"a".to_vec())
            .unwrap();
        let err = decoded.encode("a \u{4e2d}").unwrap_err();
        assert!(err.to_string().contains('\u{4e2d}'), "{err}");
        let latin = decoding(SourceEncoding::Iso8859_1, None)
            .decode(b"a".to_vec())
            .unwrap();
        assert!(latin.encode("\u{20ac}").is_err());
    }
}
