//! Unknown values and missing records in Progress ABL.
//!
//! The plan (`docs/design/strict-analysis-plan.md`, M5) gives this crate the
//! unknown value `?` from its sources to its sinks. This first slice covers
//! the record a failed `FIND` leaves missing: [`analyze_file`] runs a forward
//! dataflow over the flow graph of each unit (`oxabl_flow`) that tracks, per
//! program point, the `FIND ... NO-ERROR` statements (`FIRST`, `LAST`, `NEXT`,
//! `PREV` and unique) that may have failed and left their buffer empty.
//! `FIND CURRENT ... NO-ERROR` re-reads the record the buffer holds and keeps
//! the buffer as it was. A field reference that runs where its buffer may be empty is a
//! [`MissingRecord`]: at run time it raises ERROR, it does not read `?`.
//!
//! * **Guards** narrow on both edges of an `IF` and of a `WHILE` header:
//!   `AVAILABLE b`, `NOT`, `AND`, `OR`, and `ERROR-STATUS:ERROR` right after
//!   the `FIND`. An arm that leaves (`LEAVE`, `NEXT`, `RETURN`, `UNDO, ...`)
//!   is the flow graph's business. A guard of `c` also proves every buffer
//!   whose fields the criteria of a `FIND c ... NO-ERROR` read.
//! * **Refills**: `FIND` without `NO-ERROR`, `CREATE`, a field reference that
//!   completed, a `FOR` or query that reads the buffer.
//! * **Error paths** are not followed: on a raise edge every failed find is
//!   forgotten.
//! * Buffers resolve through `oxabl_scope`: its names, default buffers and its
//!   reading of unqualified fields.
//!
//! The ABL rules this follows, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 9.
//!
//! # Unknown
//!
//! When the analysis cannot tell, it forgets the failed finds it was carrying
//! and reports nothing, never a guess: a dynamic buffer method, a buffer
//! passed to code it cannot see, a call that may change a shared buffer or
//! one another routine of the file names, an unexpanded `{...}` or an elided
//! `{&name}` in the statement, a statement form it does not know that names
//! the buffer. A file with a file-level Unknown of the scope analysis (a
//! recovered parse, an unexpanded include, past the work budget) gets no
//! answer at all ([`FileNulls::unknown`]), and a buffer whose table the
//! schema does not have is never judged.
//!
//! Spans are in the text the statements were parsed from (the expanded text
//! when includes were expanded).

mod analysis;
mod cond;
mod dump;
mod model;

pub use model::{FailedFind, MissingRecord};

use oxabl_flow::FileFlow;
use oxabl_scope::FileScopes;

/// Options for [`analyze_file`].
#[derive(Debug, Clone, Copy, Default)]
pub struct NullOptions<'a> {
    /// Offsets in the source where the preprocessor replaced a reference with
    /// no text, sorted. A statement that holds one may hide `NO-ERROR`, a
    /// guard or a write: every failed find is forgotten there.
    pub elided: &'a [u32],
}

/// The failed finds of one file and the field references they reach.
#[derive(Debug, Clone, Default)]
pub struct FileNulls {
    finds: Vec<FailedFind>,
    missing: Vec<MissingRecord>,
    unknown: Vec<oxabl_scope::Unknown>,
}

impl FileNulls {
    /// Every `FIND ... NO-ERROR` of a judged buffer, in source order.
    pub fn finds(&self) -> &[FailedFind] {
        &self.finds
    }

    /// One find.
    pub fn find(&self, index: u32) -> &FailedFind {
        &self.finds[index as usize]
    }

    /// Every field reference that may run with its buffer empty, in source
    /// order.
    pub fn missing(&self) -> &[MissingRecord] {
        &self.missing
    }

    /// Why the file got no answer: the file-level Unknown reasons of the scope
    /// analysis. Empty when it was judged.
    pub fn unknown(&self) -> &[oxabl_scope::Unknown] {
        &self.unknown
    }

    /// A stable text rendering, for snapshot tests and debugging. `source` is
    /// the text that was analysed; sites show as `@line`.
    pub fn dump(&self, source: &str, scopes: &FileScopes) -> String {
        dump::dump(self, source, scopes)
    }
}

/// Analyse a file over its flow graphs and its scope analysis, both built
/// from the same statements. `tokens` are the lexer's tokens of `source`.
pub fn analyze_file(
    source: &str,
    tokens: &[oxabl_lexer::Token],
    flow: &FileFlow,
    scopes: &FileScopes,
    options: &NullOptions<'_>,
) -> FileNulls {
    analysis::analyze(source, tokens, flow, scopes, options)
}
