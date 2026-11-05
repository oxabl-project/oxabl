//! Framing: percent-encoded, tab-separated lines that start with `OXW1`.

use super::codec::Codec;

pub const PREFIX: &[u8] = b"OXW1\t";
/// The longest protocol line, in bytes, without the line end.
pub const MAX_LINE: usize = 30_000;

/// Percent-encodes `%`, `TAB`, `LF`, `CR` and every byte outside `0x20..=0x7E`.
pub fn encode_field(out: &mut Vec<u8>, bytes: &[u8]) {
    for &byte in bytes {
        if (0x20..=0x7E).contains(&byte) && byte != b'%' {
            out.push(byte);
        } else {
            out.extend_from_slice(format!("%{byte:02X}").as_bytes());
        }
    }
}

fn hex(digit: u8) -> Option<u8> {
    (digit as char).to_digit(16).map(|d| d as u8)
}

/// Reverses [`encode_field`]. A raw byte outside the printable range is an error.
pub fn decode_field(field: &[u8]) -> Result<Vec<u8>, String> {
    let mut out = Vec::with_capacity(field.len());
    let mut i = 0;
    while i < field.len() {
        match field[i] {
            b'%' => {
                let value = field
                    .get(i + 1..i + 3)
                    .and_then(|pair| Some(hex(pair[0])? << 4 | hex(pair[1])?))
                    .ok_or("bad percent escape")?;
                out.push(value);
                i += 3;
            }
            byte if (0x20..=0x7E).contains(&byte) => {
                out.push(byte);
                i += 1;
            }
            byte => return Err(format!("raw byte 0x{byte:02X} in a field")),
        }
    }
    Ok(out)
}

/// A parsed protocol line: the message type and the decoded fields.
#[derive(Debug, PartialEq, Eq)]
pub struct Frame {
    pub kind: String,
    pub fields: Vec<Vec<u8>>,
}

impl Frame {
    /// Parses a line without its `LF`; a `CR` before the end is removed.
    pub fn parse(line: &[u8]) -> Result<Frame, String> {
        let line = line.strip_suffix(b"\r").unwrap_or(line);
        let rest = line.strip_prefix(PREFIX).ok_or("line has no OXW1 prefix")?;
        let mut parts = rest.split(|byte| *byte == b'\t');
        let kind = parts.next().unwrap_or_default();
        if kind.is_empty() || !kind.iter().all(|b| b.is_ascii_uppercase() || *b == b'-') {
            return Err("bad message type".into());
        }
        let fields = parts.map(decode_field).collect::<Result<Vec<_>, _>>()?;
        Ok(Frame {
            kind: String::from_utf8_lossy(kind).into_owned(),
            fields,
        })
    }
}

/// A message from oxabl to the worker.
#[derive(Debug)]
pub enum Request<'a> {
    Hello {
        token: &'a str,
    },
    Schema {
        ldb: &'a str,
        part: usize,
        parts: usize,
    },
    Fingerprint {
        ldb: &'a str,
    },
    Hash {
        id: u64,
        path: &'a str,
    },
    HashSet {
        id: u64,
        paths: &'a [String],
    },
    Ping {
        n: u64,
    },
    Quit,
}

fn line(kind: &str, fields: &[Vec<u8>]) -> Result<Vec<u8>, String> {
    let mut out = PREFIX.to_vec();
    out.extend_from_slice(kind.as_bytes());
    for field in fields {
        out.push(b'\t');
        encode_field(&mut out, field);
    }
    if out.len() > MAX_LINE {
        return Err(format!("a {kind} line would be {} bytes long", out.len()));
    }
    out.push(b'\n');
    Ok(out)
}

impl Request<'_> {
    /// The lines to write, each ending in `LF`.
    pub fn lines(&self, codec: &Codec) -> Result<Vec<Vec<u8>>, String> {
        let text = |value: &str| codec.encode(value);
        let number = |value: u64| value.to_string().into_bytes();
        match self {
            Request::Hello { token } => Ok(vec![line("HELLO", &[b"1".to_vec(), text(token)?])?]),
            Request::Schema { ldb, part, parts } => Ok(vec![line(
                "SCHEMA",
                &[text(ldb)?, number(*part as u64), number(*parts as u64)],
            )?]),
            Request::Fingerprint { ldb } => Ok(vec![line("FINGERPRINT", &[text(ldb)?])?]),
            Request::Hash { id, path } => Ok(vec![line("HASH", &[number(*id), text(path)?])?]),
            Request::HashSet { id, paths } => {
                let mut lines = vec![line("HASHSET", &[number(*id), number(paths.len() as u64)])?];
                for path in *paths {
                    lines.push(line("PATH", &[number(*id), text(path)?])?);
                }
                Ok(lines)
            }
            Request::Ping { n } => Ok(vec![line("PING", &[number(*n)])?]),
            Request::Quit => Ok(vec![line("QUIT", &[])?]),
        }
    }
}

/// A message from the worker to oxabl, with text fields still as bytes.
#[derive(Debug, PartialEq, Eq)]
pub enum Reply {
    Hello {
        version: String,
        token: String,
    },
    Env {
        name: String,
        value: Vec<u8>,
    },
    Db {
        number: String,
        ldb: Vec<u8>,
        dbtype: Vec<u8>,
        aliases: Vec<u8>,
    },
    EnvEnd {
        count: String,
    },
    Fprint {
        ldb: Vec<u8>,
        guid: Vec<u8>,
        stamp: Vec<u8>,
    },
    Sent {
        ldb: Vec<u8>,
        kind: String,
        name: Vec<u8>,
        records: String,
        sha256: String,
    },
    SchemaEnd {
        ldb: Vec<u8>,
        entries: String,
        tables: String,
    },
    Hashed {
        id: String,
        kind: String,
        size: String,
        sha256: String,
    },
    HashsetDone {
        id: String,
        sha256: String,
    },
    Pong {
        n: String,
    },
    Bye,
    Error {
        text: Vec<u8>,
    },
}

fn ascii(field: &[u8]) -> Result<String, String> {
    if field.is_ascii() {
        Ok(String::from_utf8_lossy(field).into_owned())
    } else {
        Err("a non-ASCII byte in a field that must be ASCII".into())
    }
}

impl Reply {
    /// Maps a frame onto a reply, checking the field count of its type.
    pub fn from_frame(frame: Frame) -> Result<Reply, String> {
        let Frame { kind, mut fields } = frame;
        let expect = |count: usize, fields: &Vec<Vec<u8>>| {
            if fields.len() == count {
                Ok(())
            } else {
                Err(format!(
                    "{kind} has {} fields, expected {count}",
                    fields.len()
                ))
            }
        };
        let take = |fields: &mut Vec<Vec<u8>>| fields.remove(0);
        match kind.as_str() {
            "HELLO" => {
                expect(2, &fields)?;
                Ok(Reply::Hello {
                    version: ascii(&take(&mut fields))?,
                    token: ascii(&take(&mut fields))?,
                })
            }
            "ENV" => {
                expect(2, &fields)?;
                Ok(Reply::Env {
                    name: ascii(&take(&mut fields))?,
                    value: take(&mut fields),
                })
            }
            "DB" => {
                expect(4, &fields)?;
                Ok(Reply::Db {
                    number: ascii(&take(&mut fields))?,
                    ldb: take(&mut fields),
                    dbtype: take(&mut fields),
                    aliases: take(&mut fields),
                })
            }
            "ENV-END" => {
                expect(1, &fields)?;
                Ok(Reply::EnvEnd {
                    count: ascii(&take(&mut fields))?,
                })
            }
            "FPRINT" => {
                expect(3, &fields)?;
                Ok(Reply::Fprint {
                    ldb: take(&mut fields),
                    guid: take(&mut fields),
                    stamp: take(&mut fields),
                })
            }
            "SENT" => {
                expect(5, &fields)?;
                Ok(Reply::Sent {
                    ldb: take(&mut fields),
                    kind: ascii(&take(&mut fields))?,
                    name: take(&mut fields),
                    records: ascii(&take(&mut fields))?,
                    sha256: ascii(&take(&mut fields))?,
                })
            }
            "SCHEMA-END" => {
                expect(3, &fields)?;
                Ok(Reply::SchemaEnd {
                    ldb: take(&mut fields),
                    entries: ascii(&take(&mut fields))?,
                    tables: ascii(&take(&mut fields))?,
                })
            }
            "HASHED" => {
                expect(4, &fields)?;
                Ok(Reply::Hashed {
                    id: ascii(&take(&mut fields))?,
                    kind: ascii(&take(&mut fields))?,
                    size: ascii(&take(&mut fields))?,
                    sha256: ascii(&take(&mut fields))?,
                })
            }
            "HASHSET-DONE" => {
                expect(2, &fields)?;
                Ok(Reply::HashsetDone {
                    id: ascii(&take(&mut fields))?,
                    sha256: ascii(&take(&mut fields))?,
                })
            }
            "PONG" => {
                expect(1, &fields)?;
                Ok(Reply::Pong {
                    n: ascii(&take(&mut fields))?,
                })
            }
            "BYE" => {
                expect(0, &fields)?;
                Ok(Reply::Bye)
            }
            "ERROR" => {
                expect(1, &fields)?;
                Ok(Reply::Error {
                    text: take(&mut fields),
                })
            }
            other => Err(format!("unexpected message type {other}")),
        }
    }
}
