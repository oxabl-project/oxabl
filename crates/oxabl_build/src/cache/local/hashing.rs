//! Copying and hashing in one pass.

use std::fs::File;
use std::io::{Read, Write};

/// What a stream held.
pub struct Digest {
    pub size: u64,
    pub blake3: [u8; 32],
}

/// Which side of a copy failed.
pub enum CopyError {
    Read,
    Write,
}

const CHUNK: usize = 256 * 1024;

/// Copies `source` to `target` (when given) and returns its size and BLAKE3
/// hash.
pub fn copy_and_hash(
    source: &mut File,
    mut target: Option<&mut File>,
) -> Result<Digest, CopyError> {
    let mut hasher = blake3::Hasher::new();
    let mut buffer = vec![0u8; CHUNK];
    let mut size = 0u64;
    loop {
        let read = source.read(&mut buffer).map_err(|_| CopyError::Read)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        if let Some(target) = target.as_mut() {
            target
                .write_all(&buffer[..read])
                .map_err(|_| CopyError::Write)?;
        }
        size += read as u64;
    }
    Ok(Digest {
        size,
        blake3: *hasher.finalize().as_bytes(),
    })
}
