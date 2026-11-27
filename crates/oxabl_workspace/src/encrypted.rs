//! XCODE-encrypted source.
//!
//! The XCODE utility encrypts ABL source, and `COMPILE ... XCODE key` compiles
//! it. A reader that has no key cannot judge such a file, so every oxabl client
//! skips it and says so once. This module is the one detector they share.
//!
//! No published format exists. A wrong "encrypted" hides a readable file from
//! every check, and a wrong "plain" only costs a parse error, so the detector
//! is strict. A file is encrypted only when both hold:
//!
//! * it starts with the 10 header bytes of the encrypted files seen so far
//!   ([`HEADER`]), and
//! * it is shorter than [`PROFILE_MIN_LEN`] bytes, or its first [`HEAD_LEN`]
//!   bytes have the profile of random data: at least [`MIN_DISTINCT`] different
//!   control byte values, and at least one control byte in [`MAX_GAP`] bytes.
//!   Random data holds about one control byte in ten, spread over most of the
//!   27 values. Plain source repeats one or two (a printer escape, a stray
//!   end-of-file byte).
//!
//! A short file has too few bytes to show the profile: a random file of 50
//! bytes fails it half of the time, and a miss would splice the bytes into the
//! includer with no message. So a short file is judged by the header alone. The
//! header holds DC3 and BEL, which plain ABL does not start with.
//!
//! A file with another header and a file that is plain text with control bytes
//! are read as source.
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

/// A file with the header and fewer bytes than this is encrypted without the
/// profile test. At this length a random file fails the profile less than once
/// in 100 000 times.
pub const PROFILE_MIN_LEN: usize = 256;

/// The least number of different control byte values in a file that is
/// encrypted.
pub const MIN_DISTINCT: usize = 6;

/// An encrypted file holds a control byte in at least one byte in this many.
pub const MAX_GAP: usize = 50;

/// Whether `head`, the first bytes of a file, are the start of encrypted
/// source, by the rules in the module documentation. Only the first
/// [`HEAD_LEN`] bytes are read. A file the rules do not name is plain.
pub fn looks_encrypted(head: &[u8]) -> bool {
    let head = &head[..head.len().min(HEAD_LEN)];
    if !head.starts_with(&HEADER) {
        return false;
    }
    if head.len() < PROFILE_MIN_LEN {
        return true;
    }
    let mut seen = 0u32;
    let mut controls = 0usize;
    for &byte in head {
        if byte < 0x20 && !matches!(byte, 0 | b'\t' | b'\n' | b'\r' | 0x0C) {
            seen |= 1 << byte;
            controls += 1;
        }
    }
    seen.count_ones() as usize >= MIN_DISTINCT && controls * MAX_GAP >= head.len()
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

/// The text of the finding for an include that is encrypted. The unit that
/// reaches it is not checked in full, and says so.
pub fn include_note(name: &str) -> String {
    format!(
        "include '{name}' is {EncryptedSource}: the unit is not fully checked and its findings are withheld"
    )
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
                (state >> 24).to_le_bytes()[0]
            })
            .collect()
    }

    /// The header, then random bytes: the shape of an encrypted file.
    fn encrypted(len: usize) -> Vec<u8> {
        let mut bytes = HEADER.to_vec();
        bytes.extend(random(len - HEADER.len()));
        bytes
    }

    #[test]
    fn the_header_and_random_bytes_are_encrypted() {
        for len in [100, 200, 511, 512, 4096] {
            assert!(looks_encrypted(&encrypted(len)), "len {len}");
        }
    }

    #[test]
    fn a_short_file_with_the_header_is_encrypted_without_a_profile() {
        assert!(looks_encrypted(&HEADER));
        // A one-line include of 30 bytes holds too few control bytes to show a
        // profile; most random files of this length fail it.
        for len in [11, 30, 50, 90, PROFILE_MIN_LEN - 1] {
            for start in 0..20 {
                let mut bytes = HEADER.to_vec();
                bytes.extend(random(start * 97 + len).into_iter().skip(start * 97));
                assert!(looks_encrypted(&bytes), "len {len} start {start}");
            }
        }
    }

    #[test]
    fn a_file_with_the_header_and_plain_text_is_read_as_source() {
        let mut bytes = HEADER.to_vec();
        bytes.extend(b"MESSAGE 1.\n".repeat(30));
        assert!(bytes.len() >= PROFILE_MIN_LEN);
        assert!(!looks_encrypted(&bytes));
    }

    #[test]
    fn random_bytes_without_the_header_are_read_as_source() {
        for seed in 0..20u8 {
            let mut bytes = random(600);
            bytes[0] = 0x41 + seed;
            assert!(!looks_encrypted(&bytes), "seed {seed}");
        }
    }

    #[test]
    fn plain_source_is_not_encrypted() {
        assert!(!looks_encrypted(b"DEF VAR x AS INT.\r\n\tx = 1.\x0c\n"));
        assert!(!looks_encrypted("caf\u{e9} \u{4e2d}".as_bytes()));
        assert!(!looks_encrypted(b""));
    }

    #[test]
    fn plain_text_with_control_bytes_is_not_encrypted() {
        let escape = b"DEFINE VARIABLE x AS CHARACTER NO-UNDO.\nx = \"\x1bE\".\nPUT CONTROL x.\n";
        assert!(!looks_encrypted(escape));
        assert!(!looks_encrypted(b"MESSAGE 1.\x1a"));
        // A short include of printer codes: dense, but one value.
        let printer: Vec<u8> = (0..8)
            .flat_map(|i| {
                format!("&GLOBAL-DEFINE P{i} \"\x1b{}\"\n", char::from(65 + i)).into_bytes()
            })
            .collect();
        assert!(!looks_encrypted(&printer));
        // The header, and dense, but plain text after it with a few values.
        let mut marked = HEADER.to_vec();
        marked.extend(printer.iter().cycle().take(300));
        assert!(!looks_encrypted(&marked));
    }

    #[test]
    fn the_profile_needs_both_variety_and_density() {
        // Variety, but sparse in a full block.
        let mut sparse = HEADER.to_vec();
        sparse.resize(HEAD_LEN, b'a');
        for (at, value) in [1u8, 2, 3, 4, 5, 6, 8, 11].into_iter().enumerate() {
            sparse[20 + at * 50] = value;
        }
        assert!(!looks_encrypted(&sparse));
        for (at, value) in [1u8, 2, 3, 4, 5, 6, 8, 11].into_iter().enumerate() {
            sparse[21 + at * 50] = value;
            sparse[22 + at * 50] = value;
        }
        assert!(looks_encrypted(&sparse));
        // Density, but too few values.
        let mut few = HEADER.to_vec();
        few.extend([0x1b, 0x07, 0x1b, 0x07, 0x1a].repeat(60));
        assert!(!looks_encrypted(&few));
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
    fn control_bytes_after_the_first_block_are_not_read() {
        let mut bytes = HEADER.to_vec();
        bytes.resize(HEAD_LEN, b'a');
        bytes.extend(random(2000));
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
