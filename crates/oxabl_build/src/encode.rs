//! The canonical byte encoding that every derived hash is built from: explicit
//! lengths and counts, so no two different inputs encode to the same bytes.

/// Builds one encoded byte string.
#[derive(Default)]
pub struct Encoder {
    bytes: Vec<u8>,
}

impl Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn u64(&mut self, value: u64) -> &mut Self {
        self.bytes.extend_from_slice(&value.to_le_bytes());
        self
    }

    pub fn bytes(&mut self, value: &[u8]) -> &mut Self {
        self.u64(value.len() as u64);
        self.bytes.extend_from_slice(value);
        self
    }

    pub fn str(&mut self, value: &str) -> &mut Self {
        self.bytes(value.as_bytes())
    }

    pub fn hash(&mut self, value: &[u8; 32]) -> &mut Self {
        self.bytes.extend_from_slice(value);
        self
    }

    pub fn bool(&mut self, value: bool) -> &mut Self {
        self.u64(u64::from(value))
    }

    pub fn list<T>(&mut self, items: &[T], mut each: impl FnMut(&mut Self, &T)) -> &mut Self {
        self.u64(items.len() as u64);
        for item in items {
            each(self, item);
        }
        self
    }

    pub fn strs<S: AsRef<str>>(&mut self, items: &[S]) -> &mut Self {
        self.list(items, |encoder, item| {
            encoder.str(item.as_ref());
        })
    }

    /// The BLAKE3 derive-key hash of everything encoded, under `context`.
    pub fn finish(&self, context: &str) -> [u8; 32] {
        let mut hasher = blake3::Hasher::new_derive_key(context);
        hasher.update(&self.bytes);
        *hasher.finalize().as_bytes()
    }
}
