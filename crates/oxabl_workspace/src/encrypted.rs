//! XCODE-encrypted source.
//!
//! The XCODE utility encrypts ABL source, and `COMPILE ... XCODE key` compiles
//! it. A reader that has no key cannot judge such a file, so every oxabl client
//! skips it and says so once. This module is the one detector they share.
//!
//! No published format exists. The detector relies on the one thing every
//! encrypted file seen so far has: control bytes in its first block, which
//! plain source never has. The header of such files (`13 59 2f 25 07 09 31 49
//! 3b 25`) holds two of them.

use std::fmt;
use std::io;
use std::path::Path;

/// The stable code of the diagnostic that reports an encrypted file.
pub const ENCRYPTED_CODE: &str = "SOURCE001";

/// How much of a file the detector reads.
pub const HEAD_LEN: usize = 512;

/// Whether `head`, the first bytes of a file, are the start of encrypted
/// source: a control byte other than white space (tab, line feed, form feed,
/// carriage return) in the first [`HEAD_LEN`] bytes.
///
/// Two kinds of plain text stay plain. A text that starts with a UTF-16 or
/// UTF-32 byte order mark is not encrypted. A DOS end-of-file byte (`1a`) at
/// the end of a file that fits in the block is not a control byte here.
/// A file the rule does not name is read as source: a wrong "plain" costs a
/// parse error, a wrong "encrypted" hides a file.
pub fn looks_encrypted(head: &[u8]) -> bool {
    let head = &head[..head.len().min(HEAD_LEN)];
    if head.starts_with(&[0xFF, 0xFE]) || head.starts_with(&[0xFE, 0xFF]) {
        return false;
    }
    let head = match head.split_last() {
        Some((0x1A, rest)) if head.len() < HEAD_LEN => rest,
        _ => head,
    };
    head.iter()
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

    const ISSUE_HEADER: [u8; 10] = [0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];

    #[test]
    fn the_known_header_is_encrypted() {
        assert!(looks_encrypted(&ISSUE_HEADER));
    }

    #[test]
    fn plain_source_is_not_encrypted() {
        assert!(!looks_encrypted(b"DEF VAR x AS INT.\r\n\tx = 1.\x0c\n"));
        assert!(!looks_encrypted("caf\u{e9} \u{4e2d}".as_bytes()));
        assert!(!looks_encrypted(b""));
    }

    #[test]
    fn a_control_byte_after_the_first_block_is_not_read() {
        let mut bytes = vec![b'a'; HEAD_LEN];
        bytes.push(0x07);
        assert!(!looks_encrypted(&bytes));
        bytes[HEAD_LEN - 1] = 0x07;
        assert!(looks_encrypted(&bytes));
    }

    #[test]
    fn utf_16_text_and_a_dos_end_of_file_byte_are_not_encrypted() {
        assert!(!looks_encrypted(&[0xFF, 0xFE, b'a', 0, b'b', 0]));
        assert!(!looks_encrypted(&[0xFE, 0xFF, 0, b'a', 0, b'b']));
        assert!(!looks_encrypted(b"MESSAGE 1.\x1a"));
        assert!(looks_encrypted(b"MESSAGE\x1a 1."));
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
