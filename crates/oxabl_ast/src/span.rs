/// Byte offset range tracking a node's location in the source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}
