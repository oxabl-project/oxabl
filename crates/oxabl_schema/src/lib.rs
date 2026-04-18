//! Parser and in-memory model for Progress OpenEdge `.df` dump files.
//!
//! The `.df` format is the standard textual schema interchange for Progress
//! databases (see Riverside Software's `DumpFileGrammar.g4`). `oxabl_schema`
//! parses a subset sufficient for semantic analysis — `ADD TABLE`,
//! `ADD FIELD`, `ADD INDEX`, and their attributes. Unrecognised directives
//! and unknown attributes round-trip silently so that format drift across
//! OpenEdge versions never hard-errors the loader.
//!
//! ```no_run
//! use std::path::PathBuf;
//! use oxabl_schema::SchemaLoader;
//! use oxabl_workspace::RealFileSystem;
//!
//! let fs = RealFileSystem;
//! let (schema, diags) = SchemaLoader::load_files(&[PathBuf::from("db.df")], &fs);
//! for table in schema.tables() {
//!     let _ = table;
//! }
//! # let _ = diags;
//! ```

mod atom;
mod diagnostics;
mod loader;
mod parser;
mod schema;

pub use atom::fold_atom;
pub use diagnostics::{SCHEMA0001, SCHEMA0010, SCHEMA0011, SCHEMA0012, SCHEMA0030, SCHEMA0031};
pub use loader::{LOAD_FIELD_CAP, LOAD_TABLE_CAP, SchemaLoader};
pub use parser::{ParseOutcome, parse_df};
pub use schema::{Field, Index, IndexField, Schema, SchemaRevision, SchemaType, Table, TableId};
