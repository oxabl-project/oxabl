//! The compile key of one unit.

use super::encode::{Encoder, KEY_CONTEXT, derive};
use super::options::CompileSettings;
use crate::store::{Components, SECTIONS};

/// Where the environment identity sits among the sections.
pub const ENVIRONMENT_SECTION: usize = 3;

/// What a PROPATH entry contributes to the key besides its name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryDigest {
    /// A directory: its source files enter the key through the closure.
    Dir,
    /// A procedure library, by the lower-case hex SHA-256 of its bytes.
    Library(String),
    /// Nothing is there.
    Absent,
}

impl EntryDigest {
    /// The digest as the key encodes it.
    pub fn text(&self) -> String {
        match self {
            EntryDigest::Dir => "dir".to_string(),
            EntryDigest::Library(sha256) => format!("sha256:{sha256}"),
            EntryDigest::Absent => "absent".to_string(),
        }
    }
}

/// The hash a closure file is known by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Algorithm {
    /// BLAKE3 of the bytes oxabl read.
    Blake3,
    /// SHA-256 of the bytes a worker read, for a file under `$DLC`.
    Sha256,
}

impl Algorithm {
    fn name(self) -> &'static str {
        match self {
            Algorithm::Blake3 => "blake3",
            Algorithm::Sha256 => "sha256",
        }
    }
}

/// One file of a closure.
#[derive(Debug, Clone, Copy)]
pub struct ClosureFile<'a> {
    pub path: &'a str,
    pub algorithm: Algorithm,
    pub hash: [u8; 32],
}

/// Everything one compile key is made of. `closure` is sorted by path bytes.
pub struct Material<'a> {
    pub unit_path: &'a str,
    pub compile_name: &'a str,
    pub unit_content: [u8; 32],
    pub closure: &'a [ClosureFile<'a>],
    pub propath: &'a [(String, EntryDigest)],
    pub environment: blake3::Hash,
    pub settings: &'a CompileSettings,
    pub worker: blake3::Hash,
}

/// The encoded bytes of each section of the key, in order.
fn sections(material: &Material<'_>) -> [Vec<u8>; SECTIONS] {
    let mut unit = Encoder::new();
    unit.section("unit")
        .str(material.unit_path)
        .str(material.compile_name)
        .hash(&blake3::Hash::from_bytes(material.unit_content));
    let mut closure = Encoder::new();
    closure
        .section("closure")
        .u64(material.closure.len() as u64);
    for file in material.closure {
        closure
            .str(file.path)
            .str(file.algorithm.name())
            .hash(&blake3::Hash::from_bytes(file.hash));
    }
    let mut propath = Encoder::new();
    propath
        .section("propath")
        .u64(material.propath.len() as u64);
    for (entry, digest) in material.propath {
        propath.str(entry).str(&digest.text());
    }
    let mut environment = Encoder::new();
    environment
        .section("environment")
        .hash(&material.environment);
    let pairs = material.settings.pairs();
    let mut options = Encoder::new();
    options.section("options").u64(pairs.len() as u64);
    for (name, value) in pairs {
        options.str(name).str(value);
    }
    let outputs = material.settings.outputs();
    let mut kinds = Encoder::new();
    kinds.section("outputs").u64(outputs.len() as u64);
    for kind in outputs {
        kinds.str(kind);
    }
    let mut worker = Encoder::new();
    worker.section("worker").hash(&material.worker);
    [
        unit.finish(),
        closure.finish(),
        propath.finish(),
        environment.finish(),
        options.finish(),
        kinds.finish(),
        worker.finish(),
    ]
}

/// The compile key of `material`.
pub fn compile_key(material: &Material<'_>) -> blake3::Hash {
    derive(KEY_CONTEXT, &sections(material).concat())
}

/// The digest of each section of the key of `material`, which the planner
/// compares to name what changed. The environment section is the environment
/// identity itself.
pub fn components(material: &Material<'_>) -> Components {
    let sections = sections(material);
    let mut digests = [[0u8; 32]; SECTIONS];
    for (at, section) in sections.iter().enumerate() {
        digests[at] = *blake3::hash(section).as_bytes();
    }
    digests[ENVIRONMENT_SECTION] = *material.environment.as_bytes();
    digests
}
