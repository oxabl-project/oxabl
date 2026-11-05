//! What a unit must pass before a cache may store its outputs.
//!
//! V1 hashes the unit and its closure again ([`rehash`]), V2 images the
//! databases again ([`release`]) and V3 compares the files the compiler
//! opened with the closure ([`reads`]).

pub mod reads;
pub mod rehash;
pub mod release;
