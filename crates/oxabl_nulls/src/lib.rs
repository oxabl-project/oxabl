//! Unknown values and missing records in Progress ABL.
//!
//! The plan (`docs/design/strict-analysis-plan.md`, M5) gives this crate the
//! unknown value `?` from its sources to its sinks.
//! [`FileNulls::with_values`] adds local variable facts before each flow node.
//! [`FileNulls::value_before`] separates known values, possible `?` values,
//! Unknown and unreachable points.
//!
//! The record analysis covers the record a failed `FIND` leaves missing: [`analyze_file`] runs a forward
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
//!   is the flow graph's business. A guard of `c` also proves the buffers
//!   whose fields the criteria of the `FIND c ... NO-ERROR` that filled it
//!   read, when `c` was empty before that `FIND`.
//! * **Refills**: `FIND` without `NO-ERROR`, `CREATE`, a field reference that
//!   completed, a `FOR` or query that reads the buffer. A frame-field
//!   reference (`@ b.f`, `b.f:SCREEN-VALUE`, `ENABLE b.f`) neither reads nor
//!   proves a record.
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
//! the buffer, a test of `ERROR-STATUS` it cannot read or that follows a
//! call. A file with a file-level Unknown of the scope analysis (a
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
mod values;
pub use values::{ValueState, ValueUse};

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

/// Failed finds, local unknown values, and the uses they reach.
#[derive(Debug, Clone, Default)]
pub struct FileNulls {
    values: Vec<ValueUse>,
    value_facts: Vec<values::UnitValues>,
    finds: Vec<FailedFind>,
    missing: Vec<MissingRecord>,
    unknown: Vec<oxabl_scope::Unknown>,
}

impl FileNulls {
    /// Local variable uses with a known possible source of `?`, in source order.
    pub fn values(&self) -> &[ValueUse] {
        &self.values
    }

    /// The first use of [`values`](Self::values) per unit, variable and kind:
    /// the uses after it repeat the same source.
    pub fn first_values(&self) -> impl Iterator<Item = &ValueUse> {
        let mut seen = rustc_hash::FxHashSet::default();
        self.values
            .iter()
            .filter(move |u| seen.insert((u.unit, u.name.to_ascii_lowercase(), u.propagation)))
    }

    /// Read a local variable before a node. Names are case insensitive.
    /// Returns `None` for variables and units outside this analysis.
    pub fn value_before(
        &self,
        unit: oxabl_flow::UnitId,
        node: oxabl_flow::NodeIx,
        name: &str,
    ) -> Option<ValueState> {
        self.value_facts
            .iter()
            .find(|f| f.unit == unit)?
            .before(node, name)
    }

    /// Add local variable dataflow over the same program and flow graphs.
    pub fn with_values(
        mut self,
        program: &[oxabl_ast::Statement],
        flow: &FileFlow,
        options: &NullOptions<'_>,
    ) -> Self {
        if self.unknown.is_empty() {
            (self.values, self.value_facts) = values::analyze(program, flow, options.elided);
        }
        self
    }
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
