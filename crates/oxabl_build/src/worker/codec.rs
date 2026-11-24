//! The code page the worker writes its strings in.

use encoding_rs::Encoding;

/// Converts between the worker's `-cpinternal` and Rust strings.
#[derive(Clone, Copy, Debug)]
pub struct Codec {
    encoding: &'static Encoding,
}

impl Codec {
    /// The codec for the `cpinternal` a worker reported.
    pub fn for_name(cpinternal: &str) -> Result<Codec, String> {
        Encoding::for_label(cpinternal.trim().as_bytes())
            .map(|encoding| Codec { encoding })
            .ok_or_else(|| format!("unsupported cpinternal `{cpinternal}`"))
    }

    pub fn utf8() -> Codec {
        Codec {
            encoding: encoding_rs::UTF_8,
        }
    }

    /// Decodes worker bytes; bytes the code page cannot hold are an error.
    pub fn decode(&self, bytes: &[u8]) -> Result<String, String> {
        self.encoding
            .decode_without_bom_handling_and_without_replacement(bytes)
            .map(|text| text.into_owned())
            .ok_or_else(|| format!("bytes are not valid {}", self.encoding.name()))
    }

    /// Encodes oxabl text for the worker. Outside UTF-8 only ASCII is safe.
    pub fn encode(&self, text: &str) -> Result<Vec<u8>, String> {
        if text.is_ascii() || self.encoding == encoding_rs::UTF_8 {
            Ok(text.as_bytes().to_vec())
        } else {
            Err(format!(
                "path-encoding: `{text}` is not ASCII and the worker uses {}",
                self.encoding.name()
            ))
        }
    }
}
