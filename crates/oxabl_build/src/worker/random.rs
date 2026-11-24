//! Random bytes for session tokens and build ids.

use std::io::Read;

/// `N` random bytes from the operating system.
pub fn bytes<const N: usize>() -> [u8; N] {
    let mut out = [0u8; N];
    let filled = std::fs::File::open("/dev/urandom")
        .and_then(|mut file| file.read_exact(&mut out))
        .is_ok();
    if !filled {
        let seed = format!(
            "{:?}{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default(),
            std::process::id()
        );
        let mut reader = blake3::Hasher::new().update(seed.as_bytes()).finalize_xof();
        reader.fill(&mut out);
    }
    out
}

/// 32 random hex digits.
pub fn token() -> String {
    bytes::<16>()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

/// A ULID: 48 bits of milliseconds and 80 random bits in Crockford base32.
pub fn build_id() -> String {
    const ALPHABET: &[u8; 32] = b"0123456789ABCDEFGHJKMNPQRSTVWXYZ";
    let millis = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let random = bytes::<10>();
    let mut random_bits: u128 = 0;
    for byte in random {
        random_bits = (random_bits << 8) | u128::from(byte);
    }
    let value = ((millis & ((1 << 48) - 1)) << 80) | random_bits;
    (0..26)
        .rev()
        .map(|i| ALPHABET[((value >> (i * 5)) & 31) as usize] as char)
        .collect()
}
