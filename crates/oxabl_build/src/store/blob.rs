//! The encoding of the blobs of the `analyses` table.
//!
//! A blob starts with one byte, the blob version. A `varint` is unsigned
//! LEB128. Edges and observations are sorted by `(kind, path id)`, and each
//! path id is stored as the difference to the previous one of its kind.

use super::StoreError;
use super::rows::{Analysis, Edge, Observation, ObservedClosure, StoredAnalysis, Unresolved};
use crate::kinds::{EdgeKind, ObservationKind};

const VERSION: u8 = 1;

fn put_varint(out: &mut Vec<u8>, mut value: u64) {
    while value >= 0x80 {
        out.push((value as u8 & 0x7f) | 0x80);
        value >>= 7;
    }
    out.push(value as u8);
}

fn put_text(out: &mut Vec<u8>, text: &str) {
    put_varint(out, text.len() as u64);
    out.extend_from_slice(text.as_bytes());
}

struct Reader<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl<'a> Reader<'a> {
    fn new(bytes: &'a [u8]) -> Result<Self, StoreError> {
        match bytes.first() {
            Some(&VERSION) => Ok(Reader { bytes, at: 1 }),
            _ => Err(corrupt("unknown blob version")),
        }
    }

    fn byte(&mut self) -> Result<u8, StoreError> {
        let byte = *self
            .bytes
            .get(self.at)
            .ok_or_else(|| corrupt("short blob"))?;
        self.at += 1;
        Ok(byte)
    }

    fn varint(&mut self) -> Result<u64, StoreError> {
        let mut value = 0u64;
        for shift in (0..64).step_by(7) {
            let byte = self.byte()?;
            value |= u64::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return Ok(value);
            }
        }
        Err(corrupt("overlong varint"))
    }

    fn text(&mut self) -> Result<String, StoreError> {
        let length = self.varint()? as usize;
        let end = self
            .at
            .checked_add(length)
            .filter(|end| *end <= self.bytes.len())
            .ok_or_else(|| corrupt("short blob"))?;
        let text = std::str::from_utf8(&self.bytes[self.at..end])
            .map_err(|_| corrupt("name is not UTF-8"))?;
        self.at = end;
        Ok(text.to_string())
    }

    fn finish(self) -> Result<(), StoreError> {
        if self.at == self.bytes.len() {
            Ok(())
        } else {
            Err(corrupt("trailing bytes in blob"))
        }
    }
}

fn corrupt(message: &str) -> StoreError {
    StoreError::Corrupt(message.to_string())
}

/// Encode `edges`, in any order.
pub fn encode_edges(edges: &[Edge]) -> Vec<u8> {
    let mut sorted: Vec<&Edge> = edges.iter().collect();
    sorted.sort();
    sorted.dedup();
    let mut out = vec![VERSION];
    put_varint(&mut out, sorted.len() as u64);
    let mut previous: Option<(EdgeKind, u32)> = None;
    for edge in sorted {
        let base = match previous {
            Some((kind, target)) if kind == edge.kind => target,
            _ => 0,
        };
        out.push(edge.kind.code());
        put_varint(&mut out, u64::from(edge.target - base));
        if edge.kind.carries_name() {
            put_text(&mut out, edge.name.as_deref().unwrap_or(""));
        }
        previous = Some((edge.kind, edge.target));
    }
    out
}

pub fn decode_edges(bytes: &[u8]) -> Result<Vec<Edge>, StoreError> {
    let mut reader = Reader::new(bytes)?;
    let count = reader.varint()? as usize;
    let mut edges = Vec::with_capacity(count.min(bytes.len()));
    let mut previous: Option<(EdgeKind, u32)> = None;
    for _ in 0..count {
        let kind = EdgeKind::from_code(reader.byte()?).ok_or_else(|| corrupt("edge kind"))?;
        let base = match previous {
            Some((last, target)) if last == kind => target,
            _ => 0,
        };
        let target = u32::try_from(u64::from(base) + reader.varint()?)
            .map_err(|_| corrupt("path id out of range"))?;
        let name = kind.carries_name().then(|| reader.text()).transpose()?;
        edges.push(Edge { kind, target, name });
        previous = Some((kind, target));
    }
    reader.finish()?;
    Ok(edges)
}

/// Encode `observations`, in any order.
pub fn encode_observations(observations: &[Observation]) -> Vec<u8> {
    let mut sorted = observations.to_vec();
    sorted.sort();
    sorted.dedup();
    let mut out = vec![VERSION];
    put_varint(&mut out, sorted.len() as u64);
    let mut previous: Option<Observation> = None;
    for observation in sorted {
        let base = match previous {
            Some(last) if last.kind == observation.kind => last.path,
            _ => 0,
        };
        out.push(observation.kind.code());
        put_varint(&mut out, u64::from(observation.path - base));
        previous = Some(observation);
    }
    out
}

pub fn decode_observations(bytes: &[u8]) -> Result<Vec<Observation>, StoreError> {
    let mut reader = Reader::new(bytes)?;
    let count = reader.varint()? as usize;
    let mut observations = Vec::with_capacity(count.min(bytes.len()));
    let mut previous: Option<Observation> = None;
    for _ in 0..count {
        let kind = ObservationKind::from_code(reader.byte()?)
            .ok_or_else(|| corrupt("observation kind"))?;
        let base = match previous {
            Some(last) if last.kind == kind => last.path,
            _ => 0,
        };
        let path = u32::try_from(u64::from(base) + reader.varint()?)
            .map_err(|_| corrupt("path id out of range"))?;
        let observation = Observation { kind, path };
        observations.push(observation);
        previous = Some(observation);
    }
    reader.finish()?;
    Ok(observations)
}

/// Encode `unresolved`, in any order.
pub fn encode_unresolved(unresolved: &[Unresolved]) -> Vec<u8> {
    let mut sorted: Vec<&Unresolved> = unresolved.iter().collect();
    sorted.sort_by(|a, b| (a.kind, &a.name, a.reason).cmp(&(b.kind, &b.name, b.reason)));
    sorted.dedup();
    let mut out = vec![VERSION];
    put_varint(&mut out, sorted.len() as u64);
    for entry in sorted {
        out.push(entry.kind.code());
        out.push(entry.reason);
        put_text(&mut out, &entry.name);
    }
    out
}

pub fn decode_unresolved(bytes: &[u8]) -> Result<Vec<Unresolved>, StoreError> {
    let mut reader = Reader::new(bytes)?;
    let count = reader.varint()? as usize;
    let mut unresolved = Vec::with_capacity(count.min(bytes.len()));
    for _ in 0..count {
        let kind = EdgeKind::from_code(reader.byte()?).ok_or_else(|| corrupt("unresolved kind"))?;
        let reason = reader.byte()?;
        let name = reader.text()?;
        unresolved.push(Unresolved { kind, name, reason });
    }
    reader.finish()?;
    Ok(unresolved)
}

/// Encode the degraded reasons; an empty list is a complete analysis.
pub fn encode_completeness(reasons: &[u8]) -> Vec<u8> {
    let mut sorted = reasons.to_vec();
    sorted.sort_unstable();
    sorted.dedup();
    let mut out = vec![VERSION];
    put_varint(&mut out, sorted.len() as u64);
    out.extend_from_slice(&sorted);
    out
}

pub fn decode_completeness(bytes: &[u8]) -> Result<Vec<u8>, StoreError> {
    let mut reader = Reader::new(bytes)?;
    let count = reader.varint()? as usize;
    let reasons = (0..count)
        .map(|_| reader.byte())
        .collect::<Result<_, _>>()?;
    reader.finish()?;
    Ok(reasons)
}

/// Encode an observed closure: `varint count`, then each entry as `u8 kind`
/// (`1` file, `2` absent path) and its canonical path, sorted by kind and path.
pub fn encode_observed(closure: &ObservedClosure) -> Vec<u8> {
    let mut entries: Vec<(ObservationKind, &str)> = closure
        .files
        .iter()
        .map(|path| (ObservationKind::Read, path.as_str()))
        .chain(
            closure
                .absent
                .iter()
                .map(|path| (ObservationKind::Absent, path.as_str())),
        )
        .collect();
    entries.sort();
    entries.dedup();
    let mut out = vec![VERSION];
    put_varint(&mut out, entries.len() as u64);
    for (kind, path) in entries {
        out.push(kind.code());
        put_text(&mut out, path);
    }
    out
}

pub fn decode_observed(bytes: &[u8]) -> Result<ObservedClosure, StoreError> {
    let mut reader = Reader::new(bytes)?;
    let count = reader.varint()? as usize;
    let mut closure = ObservedClosure::default();
    for _ in 0..count {
        let kind = reader.byte()?;
        let path = reader.text()?;
        match ObservationKind::from_code(kind) {
            Some(ObservationKind::Read) => closure.files.push(path),
            Some(ObservationKind::Absent) => closure.absent.push(path),
            _ => return Err(corrupt("observed closure kind")),
        }
    }
    reader.finish()?;
    Ok(closure)
}

impl Analysis {
    pub fn encode(&self) -> StoredAnalysis {
        StoredAnalysis {
            analysed_gen: self.analysed_gen,
            content_hash: self.content_hash,
            completeness: encode_completeness(&self.completeness),
            edges: encode_edges(&self.edges),
            unresolved: encode_unresolved(&self.unresolved),
            observations: encode_observations(&self.observations),
        }
    }
}

impl StoredAnalysis {
    pub fn decode(&self) -> Result<Analysis, StoreError> {
        Ok(Analysis {
            analysed_gen: self.analysed_gen,
            content_hash: self.content_hash,
            completeness: decode_completeness(&self.completeness)?,
            edges: decode_edges(&self.edges)?,
            unresolved: decode_unresolved(&self.unresolved)?,
            observations: decode_observations(&self.observations)?,
        })
    }
}
