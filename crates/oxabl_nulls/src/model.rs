//! The data model: `FIND ... NO-ERROR` statements that may fail, and the field
//! references that may then find no record.

use oxabl_ast::{NodeId, Span};
use oxabl_flow::UnitId;
use oxabl_scope::BufferId;
use smallvec::SmallVec;

/// A `FIND ... NO-ERROR` that may find no record and leave its buffer empty
/// (section 9.2, N1, N5). `FIND CURRENT ... NO-ERROR` is none (N6).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FailedFind {
    pub unit: UnitId,
    pub stmt: NodeId,
    pub buffer: BufferId,
    /// The whole statement.
    pub at: Span,
}

/// A field reference that may run while its buffer has no record, because a
/// [`FailedFind`] of the buffer may have failed and nothing on some path in
/// between guards or refills it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingRecord {
    pub unit: UnitId,
    pub buffer: BufferId,
    /// The field reference.
    pub at: Span,
    /// The reference assigns the field.
    pub write: bool,
    /// The finds that may have left the buffer empty, as indices into
    /// [`FileNulls::finds`](crate::FileNulls::finds), in source order.
    pub finds: SmallVec<[u32; 2]>,
}
