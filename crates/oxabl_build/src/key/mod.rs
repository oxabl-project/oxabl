//! The compile environment and the encodings behind its identity.

pub mod digest;
pub mod encode;
pub mod environment;
pub mod parameters;
pub mod predefined;
pub mod schema_image;

pub use digest::FileDigest;
pub use environment::{Database, DatabaseInfo, Environment, Facts, Fingerprint, Library, Profile};
pub use parameters::Parameter;
pub use schema_image::{SchemaEntry, SchemaImage, SchemaPart};
