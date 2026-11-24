//! XCODE-encrypted source.
//!
//! The XCODE utility encrypts ABL source, and `COMPILE ... XCODE key` compiles
//! it. A reader that has no key cannot judge such a file, so every oxabl client
//! skips it and says so once. This module is the one detector they share.
//!
//! No published format exists. A wrong "encrypted" hides a readable file from
//! every check, and a wrong "plain" only costs a parse error, so the detector
//! is strict. It names a file encrypted on one of two signals:
//!
//! * the file starts with the 10 header bytes of the encrypted files seen so
//!   far ([`HEADER`]), or
//! * the first [`HEAD_LEN`] bytes look random: at least [`MIN_CONTROLS`] control
//!   bytes, and at least one in [`MAX_GAP`] bytes. About one byte in ten of
//!   random data is a control byte. Plain source has a few at most (a printer
//!   escape in a string, a stray end-of-file byte).
//!
//! Tab, line feed, form feed, carriage return and NUL do not count as control
//! bytes: white space is plain, and NUL is what text in UTF-16 holds.
//!
//! `oxabl build` asks the opposite question. A compile of encrypted source
//! reports no `INCLUDE`, so a miss there gives a wrong key, and a false alarm
//! only costs a compile. It uses [`may_be_encrypted`]: any control byte, NUL
//! included, in the first block.

use std::fmt;
use std::io;
use std::path::Path;

/// The stable code of the diagnostic that reports an encrypted file.
pub const ENCRYPTED_CODE: &str = "SOURCE001";

/// How much of a file the detector reads.
pub const HEAD_LEN: usize = 512;

/// The first bytes of the encrypted files seen so far.
pub const HEADER: [u8; 10] = [0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];

/// The least number of control bytes a file without the header holds.
pub const MIN_CONTROLS: usize = 8;

/// A file without the header holds a control byte in at least one byte in this
/// many.
pub const MAX_GAP: usize = 50;

/// Whether `head`, the first bytes of a file, are the start of encrypted
/// source, by the rules in the module documentation. Only the first
/// [`HEAD_LEN`] bytes are read. A file the rules do not name is plain.
pub fn looks_encrypted(head: &[u8]) -> bool {
    let head = &head[..head.len().min(HEAD_LEN)];
    if head.starts_with(&HEADER) {
        return true;
    }
    let controls = head
        .iter()
        .filter(|&&byte| byte < 0x20 && !matches!(byte, 0 | b'\t' | b'\n' | b'\r' | 0x0C))
        .count();
    controls >= MIN_CONTROLS && controls * MAX_GAP >= head.len()
}

/// Whether `head` may be the start of encrypted source: a byte below `0x20`
/// other than tab, line feed, form feed and carriage return (NUL included) in
/// the first [`HEAD_LEN`] bytes. A superset of [`looks_encrypted`], for a caller
/// that must not miss one.
pub fn may_be_encrypted(head: &[u8]) -> bool {
    head[..head.len().min(HEAD_LEN)]
        .iter()
        .any(|&byte| byte < 0x20 && !matches!(byte, b'\t' | b'\n' | b'\r' | 0x0C))
}

/// Read the first [`HEAD_LEN`] bytes of the file at `path`.
pub fn read_head(path: &Path) -> io::Result<Vec<u8>> {
    use io::Read as _;
    let mut head = Vec::with_capacity(HEAD_LEN);
    std::fs::File::open(path)?
        .take(HEAD_LEN as u64)
        .read_to_end(&mut head)?;
    Ok(head)
}

/// The error a read returns for an encrypted file, as the payload of an
/// [`io::ErrorKind::InvalidData`] error. Test for it with [`is_encrypted`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EncryptedSource;

impl fmt::Display for EncryptedSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("encrypted, not checked")
    }
}

impl std::error::Error for EncryptedSource {}

impl From<EncryptedSource> for io::Error {
    fn from(error: EncryptedSource) -> Self {
        io::Error::new(io::ErrorKind::InvalidData, error)
    }
}

/// Whether `error` says that the file is encrypted.
pub fn is_encrypted(error: &io::Error) -> bool {
    error
        .get_ref()
        .is_some_and(|inner| inner.is::<EncryptedSource>())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pseudo-random bytes, as encrypted text is.
    fn random(len: usize) -> Vec<u8> {
        let mut state = 0x9e37_79b9_7f4a_7c15_u64;
        (0..len)
            .map(|_| {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                (state >> 24) as u8
            })
            .collect()
    }

    #[test]
    fn the_known_header_is_encrypted() {
        assert!(looks_encrypted(&HEADER));
        let mut bytes = HEADER.to_vec();
        bytes.extend(b"MESSAGE 1.");
        assert!(looks_encrypted(&bytes));
    }

    #[test]
    fn random_bytes_are_encrypted_from_200_bytes_on() {
        for len in [200, 256, 511, 512, 4096] {
            for seed in 0..20u8 {
                let mut bytes = random(len + usize::from(seed));
                bytes[0] = 0x41 + seed;
                assert!(looks_encrypted(&bytes), "len {len} seed {seed}");
            }
        }
    }

    #[test]
    fn plain_source_is_not_encrypted() {
        assert!(!looks_encrypted(b"DEF VAR x AS INT.\r\n\tx = 1.\x0c\n"));
        assert!(!looks_encrypted("caf\u{e9} \u{4e2d}".as_bytes()));
        assert!(!looks_encrypted(b""));
    }

    #[test]
    fn a_few_stray_control_bytes_do_not_make_source_encrypted() {
        let escape = b"DEFINE VARIABLE x AS CHARACTER NO-UNDO.\nx = \"\x1bE\".\nPUT CONTROL x.\n";
        assert!(!looks_encrypted(escape));
        assert!(!looks_encrypted(b"MESSAGE 1.\x1a"));
        assert!(!looks_encrypted(b"MESSAGE\x1a 1."));
        let mut many = b"MESSAGE \"".to_vec();
        many.extend([0x07; MIN_CONTROLS - 1]);
        many.extend(b"\".\n");
        assert!(!looks_encrypted(&many));
        // Enough control bytes, but too sparse in a full block.
        let mut sparse = vec![b'a'; HEAD_LEN];
        for at in 0..MIN_CONTROLS {
            sparse[at * 60] = 0x07;
        }
        assert!(!looks_encrypted(&sparse));
        for at in 0..MIN_CONTROLS {
            sparse[at * 60] = 0x07;
            sparse[at * 60 + 1] = 0x07;
        }
        assert!(looks_encrypted(&sparse), "16 in 512 is dense enough");
    }

    #[test]
    fn utf_16_text_is_not_encrypted() {
        for (bom, wide) in [([0xFF, 0xFE], false), ([0xFE, 0xFF], true), ([0, 0], false)] {
            let mut bytes = bom.to_vec();
            for unit in "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".encode_utf16() {
                bytes.extend(if wide {
                    unit.to_be_bytes()
                } else {
                    unit.to_le_bytes()
                });
            }
            assert!(!looks_encrypted(&bytes));
        }
        assert!(!looks_encrypted(&[0u8; 600]));
    }

    #[test]
    fn the_lenient_question_names_any_control_byte() {
        assert!(may_be_encrypted(b"MESSAGE \"\x1b\"."));
        assert!(may_be_encrypted(b"a\0b"));
        assert!(!may_be_encrypted(b"a\tb\r\n\x0c"));
        assert!(may_be_encrypted(&HEADER));
        let mut late = vec![b'a'; HEAD_LEN];
        late.push(0x07);
        assert!(!may_be_encrypted(&late));
    }

    #[test]
    fn a_control_byte_after_the_first_block_is_not_read() {
        let mut bytes = vec![b'a'; HEAD_LEN];
        bytes.extend([0x07; 100]);
        assert!(!looks_encrypted(&bytes));
    }

    #[test]
    fn the_error_is_told_from_other_errors() {
        let error = io::Error::from(EncryptedSource);
        assert!(is_encrypted(&error));
        assert_eq!(error.to_string(), "encrypted, not checked");
        assert!(!is_encrypted(&io::Error::other("x")));
        assert!(!is_encrypted(&io::Error::from(io::ErrorKind::InvalidData)));
    }
}
