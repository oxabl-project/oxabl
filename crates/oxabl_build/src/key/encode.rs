//! The canonical byte encoding that identities hash.

/// Context string of the environment identity.
pub const ENVIRONMENT_CONTEXT: &str = "oxabl 2026-11 compile environment v1";
/// Context string of the schema identity.
pub const SCHEMA_CONTEXT: &str = "oxabl 2026-11 schema image v2";

/// Builds a byte string from length-prefixed primitives.
#[derive(Default)]
pub struct Encoder {
    bytes: Vec<u8>,
}

impl Encoder {
    pub fn new() -> Encoder {
        Encoder::default()
    }

    /// Eight bytes, little-endian.
    pub fn u64(&mut self, value: u64) -> &mut Self {
        self.bytes.extend_from_slice(&value.to_le_bytes());
        self
    }

    /// A length, then the bytes.
    pub fn bytes(&mut self, value: &[u8]) -> &mut Self {
        self.u64(value.len() as u64);
        self.bytes.extend_from_slice(value);
        self
    }

    pub fn str(&mut self, value: &str) -> &mut Self {
        self.bytes(value.as_bytes())
    }

    /// The 32 raw bytes of a hash, with no length prefix.
    pub fn hash(&mut self, value: &blake3::Hash) -> &mut Self {
        self.bytes.extend_from_slice(value.as_bytes());
        self
    }

    /// The name that starts a section of material.
    pub fn section(&mut self, name: &str) -> &mut Self {
        self.str(name)
    }

    /// Appends material that is already encoded.
    pub fn raw(&mut self, value: &[u8]) -> &mut Self {
        self.bytes.extend_from_slice(value);
        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }
}

/// `BLAKE3` in derive-key mode over `material`.
pub fn derive(context: &str, material: &[u8]) -> blake3::Hash {
    let mut hasher = blake3::Hasher::new_derive_key(context);
    hasher.update(material);
    hasher.finalize()
}
