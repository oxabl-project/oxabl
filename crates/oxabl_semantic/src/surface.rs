//! One interface for the members and supertypes of a class of any origin: a
//! class of the analysed file, a workspace class or a built-in type. See
//! `docs/design/builtin-classes.md`, "One interface for classes of every
//! origin".

use crate::builtin::BuiltinClass;
use crate::{IndexName, SymbolId};

/// Where a class or interface comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassOrigin {
    /// A class of the analysed file, by its symbol.
    Local(SymbolId),
    /// A class of the workspace index.
    Workspace(IndexName),
    /// A documented built-in type.
    Builtin(&'static BuiltinClass),
}
