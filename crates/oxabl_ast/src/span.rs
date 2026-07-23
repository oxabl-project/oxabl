/// Byte offset range tracking a node's location in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// A zero-width placeholder span (`start == end == 0`).
    ///
    /// Used as the default on hand-constructed wrappers (mirroring
    /// [`NodeId::DUMMY`](crate::NodeId::DUMMY)) and on genuinely token-less
    /// synthetic/recovery nodes. Zero-width spans are legal per
    /// `docs/design/ast-invariants.md` §1 and are tolerated by the sibling
    /// source-order assert.
    pub const DUMMY: Span = Span { start: 0, end: 0 };
}
