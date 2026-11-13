//! The raw digest of a database: every record of the eight metaschema tables
//! of the schema image, as the database stores it, hashed on the worker.
//!
//! The digest holds record ids, so it names "this database as it was at the
//! last imaging", never a schema: it is never part of a key. Equal digests
//! mean equal records, and the schema image is a function of those records,
//! so an equal digest lets a build reuse the image it stored with it.

use sha2::{Digest, Sha256};

/// The pieces that the rows other than `head` are split into.
pub const BUCKETS: usize = 16;
/// The most workers that one raw pass uses; more give nothing.
pub const MAX_WORKERS: usize = 8;
/// The version of how the worker frames the records it hashes.
const FRAMING: u32 = 1;

/// The form of the digest: a stored digest of another form is void.
pub fn form() -> String {
    format!("buckets={BUCKETS} framing={FRAMING}")
}

/// One `RAW` line: a piece, `head` or its number, with its bounds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawPiece {
    pub piece: String,
    /// The record ids that bound the piece, `-` for an open end.
    pub low: String,
    pub high: String,
    pub rows: u64,
    pub sha256: String,
}

/// The pieces one worker made of one database, with what it saw of it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawPart {
    pub part: usize,
    pub pieces: Vec<RawPiece>,
    /// The number of `_File` rows.
    pub files: u64,
    /// The physical name of the database (`PDBNAME`).
    pub physical: String,
}

/// The raw digest of one database.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawDigest {
    pub ldb: String,
    pub physical: String,
    /// SHA-256 over the piece digests in order, `head` first, lower-case hex.
    pub digest: String,
    pub rows: u64,
}

impl RawDigest {
    /// Joins the parts of all workers. Fails unless `head` and every piece
    /// from 0 to `buckets - 1` are there exactly once, `head` comes from part
    /// 0, the bounds of the pieces join with open ends, and every worker saw
    /// the same number of `_File` rows and the same physical name.
    pub fn combine(
        ldb: String,
        mut parts: Vec<RawPart>,
        buckets: usize,
    ) -> Result<RawDigest, String> {
        parts.sort_by_key(|part| part.part);
        let Some(first) = parts.first() else {
            return Err(format!("no raw digest of {ldb}"));
        };
        let (files, physical) = (first.files, first.physical.clone());
        let mut head: Option<RawPiece> = None;
        let mut numbered: Vec<Option<RawPiece>> = vec![None; buckets];
        for part in parts {
            if part.files != files || part.physical != physical {
                return Err(format!(
                    "the workers see two forms of {ldb}: {files} files in {physical} and {} in {}",
                    part.files, part.physical
                ));
            }
            for piece in part.pieces {
                let slot = match piece.piece.as_str() {
                    "head" if part.part == 0 => &mut head,
                    "head" => {
                        return Err(format!("share {} of {ldb} holds the head", part.part));
                    }
                    number => match number.parse::<usize>() {
                        Ok(at) if at < buckets && number == at.to_string() => &mut numbered[at],
                        _ => return Err(format!("{ldb} has no piece `{number}`")),
                    },
                };
                if slot.is_some() {
                    return Err(format!("piece {} of {ldb} is there twice", piece.piece));
                }
                *slot = Some(piece);
            }
        }
        let head = head.ok_or_else(|| format!("the raw digest of {ldb} lacks its head"))?;
        let numbered: Vec<RawPiece> = numbered
            .into_iter()
            .enumerate()
            .map(|(at, piece)| {
                piece.ok_or_else(|| format!("the raw digest of {ldb} lacks piece {at}"))
            })
            .collect::<Result<_, _>>()?;
        if head.low != "-" || head.high != "-" {
            return Err(format!("the head of {ldb} has bounds"));
        }
        let open = |bound: &str| bound == "-";
        if !numbered.first().is_some_and(|p| open(&p.low))
            || !numbered.last().is_some_and(|p| open(&p.high))
        {
            return Err(format!("the pieces of {ldb} do not cover every record id"));
        }
        for pair in numbered.windows(2) {
            if pair[0].high != pair[1].low || open(&pair[0].high) {
                return Err(format!(
                    "the bounds of pieces {} and {} of {ldb} do not join",
                    pair[0].piece, pair[1].piece
                ));
            }
        }
        let mut hasher = Sha256::new();
        let mut rows = 0;
        for piece in std::iter::once(&head).chain(&numbered) {
            hasher.update(
                unhex(&piece.sha256)
                    .ok_or_else(|| format!("piece {} of {ldb} has a bad hash", piece.piece))?,
            );
            rows += piece.rows;
        }
        let digest = hasher
            .finalize()
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect();
        Ok(RawDigest {
            ldb,
            physical,
            digest,
            rows,
        })
    }
}

fn unhex(text: &str) -> Option<Vec<u8>> {
    if text.len() != 64 {
        return None;
    }
    (0..32)
        .map(|at| u8::from_str_radix(text.get(at * 2..at * 2 + 2)?, 16).ok())
        .collect()
}
