//! The `OXW1` line codec of the fake, written from the protocol document.

use std::io::{self, Write};

const PREFIX: &str = "OXW1\t";

/// One protocol line: its message type and its decoded fields.
pub struct Line {
    pub kind: String,
    pub fields: Vec<Vec<u8>>,
}

/// Percent-encodes `%`, `TAB`, `LF`, `CR` and every byte outside `0x20..=0x7E`.
pub fn encode(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    for &byte in bytes {
        if (0x20..=0x7E).contains(&byte) && byte != b'%' {
            out.push(byte as char);
        } else {
            out.push_str(&format!("%{byte:02X}"));
        }
    }
    out
}

fn decode(field: &str) -> Vec<u8> {
    let bytes = field.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hex = std::str::from_utf8(&bytes[i + 1..i + 3]).unwrap_or("00");
            out.push(u8::from_str_radix(hex, 16).unwrap_or(b'?'));
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    out
}

/// Parses one request line; `None` when it does not start with the protocol prefix.
pub fn parse(line: &str) -> Option<Line> {
    let line = line.strip_suffix('\r').unwrap_or(line);
    let rest = line.strip_prefix(PREFIX)?;
    let mut parts = rest.split('\t');
    let kind = parts.next()?.to_string();
    Some(Line {
        kind,
        fields: parts.map(decode).collect(),
    })
}

/// Writes one protocol line and flushes it.
pub fn emit(out: &mut impl Write, kind: &str, fields: &[&[u8]]) -> io::Result<()> {
    let mut line = format!("{PREFIX}{kind}");
    for field in fields {
        line.push('\t');
        line.push_str(&encode(field));
    }
    line.push('\n');
    out.write_all(line.as_bytes())?;
    out.flush()
}
