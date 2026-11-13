//! Reading the raw digest of one database: one hashed piece per `RAW` line.

use std::time::Instant;

use super::error::WorkerError;
use super::frame::{Reply, Request};
use super::session::Session;
use crate::key::{RawPart, RawPiece};

fn protocol(message: String) -> WorkerError {
    WorkerError::Protocol(message)
}

fn is_sha256(text: &str) -> bool {
    text.len() == 64
        && text
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

fn is_bound(text: &str) -> bool {
    text == "-" || (!text.is_empty() && text.bytes().all(|b| b.is_ascii_digit()))
}

/// Sends `RAWDIGEST <ldb> <part> <parts> <buckets>` and collects the pieces
/// of that share. A piece of another database, a bad hash or bound, a piece
/// twice or a total that differs from `RAW-END` is a protocol error.
pub fn fetch(
    session: &mut Session,
    ldb: &str,
    part: usize,
    parts: usize,
    buckets: usize,
    deadline: Instant,
) -> Result<RawPart, WorkerError> {
    session.send(&Request::RawDigest {
        ldb,
        part,
        parts,
        buckets,
    })?;
    let codec = session.codec;
    let mut pieces: Vec<RawPiece> = Vec::new();
    loop {
        match session.recv(deadline)? {
            Reply::Raw {
                ldb: got,
                piece,
                low,
                high,
                rows,
                sha256,
            } => {
                if codec.decode(&got).map_err(protocol)? != ldb {
                    return Err(protocol(format!("RAW for another database than {ldb}")));
                }
                if !is_sha256(&sha256) {
                    return Err(protocol(format!("bad hash `{sha256}` for piece {piece}")));
                }
                if !is_bound(&low) || !is_bound(&high) {
                    return Err(protocol(format!(
                        "bad bounds `{low}` `{high}` of piece {piece}"
                    )));
                }
                let rows: u64 = rows
                    .parse()
                    .map_err(|_| protocol(format!("bad row count `{rows}`")))?;
                if pieces.iter().any(|p| p.piece == piece) {
                    return Err(protocol(format!("piece {piece} appears twice")));
                }
                pieces.push(RawPiece {
                    piece,
                    low,
                    high,
                    rows,
                    sha256,
                });
            }
            Reply::RawEnd {
                ldb: got,
                pieces: total,
                files,
                physical,
            } => {
                if codec.decode(&got).map_err(protocol)? != ldb {
                    return Err(protocol(format!("RAW-END for another database than {ldb}")));
                }
                if total != pieces.len().to_string() {
                    return Err(protocol(format!(
                        "RAW-END says {total} pieces; oxabl read {}",
                        pieces.len()
                    )));
                }
                let files: u64 = files
                    .parse()
                    .map_err(|_| protocol(format!("bad file count `{files}`")))?;
                return Ok(RawPart {
                    part,
                    pieces,
                    files,
                    physical: codec.decode(&physical).map_err(protocol)?,
                });
            }
            other => return Err(protocol(format!("unexpected {other:?} in a raw digest"))),
        }
    }
}
