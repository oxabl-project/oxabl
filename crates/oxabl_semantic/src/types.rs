//! Resolved types.
//!
//! `ResolvedType` is the semantic layer's type currency. Declaration-carried
//! types (variable `AS INTEGER`, parameter `AS CLASS`, property `AS
//! DATETIME`) are lowered into [`ResolvedType`] by the declare pass; the
//! resolve and type-check passes extend this side table with inferred
//! expression types in Phases 4a/4b.

use oxabl_ast::DataType;
use oxabl_schema::{SchemaRevision, TableId};

use crate::SymbolId;

/// A type inferred or declared for an expression / declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    Primitive(PrimitiveTy),
    /// Resolved class type. `SymbolId` targets the class symbol.
    Class(SymbolId),
    /// Buffer-typed expression (e.g. a `FOR EACH` iteration variable).
    Buffer(SymbolId),
    /// Schema-table-typed expression, revision-tagged to prove freshness.
    Table(SchemaRevision, TableId),
    /// Array / `EXTENT` type. `extent` is `None` for dynamic extent.
    Array {
        element: Box<ResolvedType>,
        extent: Option<u32>,
    },
    /// ABL `?` — lattice bottom, compatible with every type.
    Unknown,
    /// A previous error prevented inference here. Suppresses cascading
    /// diagnostics on dependent nodes.
    Error,
}

/// ABL primitive types. `WIDGET-HANDLE` is folded into `Handle` per plan
/// §Coercion catalog.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveTy {
    Integer,
    Int64,
    Decimal,
    Character,
    Longchar,
    Logical,
    Date,
    Datetime,
    DatetimeTz,
    Handle,
    Rowid,
    Recid,
    Raw,
    Memptr,
    Clob,
    Blob,
    ComHandle,
}

impl ResolvedType {
    /// Lower a parser [`DataType`] into a primitive-or-class [`ResolvedType`].
    ///
    /// Class names are returned as [`ResolvedType::Unknown`] here; resolution
    /// to a concrete [`SymbolId`] happens in the resolve pass (Phase 4a)
    /// once the type namespace is populated.
    pub fn from_data_type(dt: &DataType) -> Self {
        match dt {
            DataType::Integer => ResolvedType::Primitive(PrimitiveTy::Integer),
            DataType::Int64 => ResolvedType::Primitive(PrimitiveTy::Int64),
            DataType::Decimal => ResolvedType::Primitive(PrimitiveTy::Decimal),
            DataType::Character => ResolvedType::Primitive(PrimitiveTy::Character),
            DataType::Longchar => ResolvedType::Primitive(PrimitiveTy::Longchar),
            DataType::Logical => ResolvedType::Primitive(PrimitiveTy::Logical),
            DataType::Date => ResolvedType::Primitive(PrimitiveTy::Date),
            DataType::DateTime => ResolvedType::Primitive(PrimitiveTy::Datetime),
            DataType::DateTimeTz => ResolvedType::Primitive(PrimitiveTy::DatetimeTz),
            DataType::Handle => ResolvedType::Primitive(PrimitiveTy::Handle),
            DataType::Rowid => ResolvedType::Primitive(PrimitiveTy::Rowid),
            DataType::Recid => ResolvedType::Primitive(PrimitiveTy::Recid),
            DataType::Raw => ResolvedType::Primitive(PrimitiveTy::Raw),
            DataType::Memptr => ResolvedType::Primitive(PrimitiveTy::Memptr),
            DataType::Clob => ResolvedType::Primitive(PrimitiveTy::Clob),
            DataType::Blob => ResolvedType::Primitive(PrimitiveTy::Blob),
            DataType::Com => ResolvedType::Primitive(PrimitiveTy::ComHandle),
            // Class type resolution requires the type namespace — deferred
            // to Phase 4a. Declare pass emits `Unknown` as a placeholder.
            DataType::Class(_) => ResolvedType::Unknown,
            // Preprocessor `&IF` in a type position yields `Unknown` until
            // the resolve pass picks an active branch.
            DataType::PreprocIf(_) => ResolvedType::Unknown,
        }
    }

    /// Whether this is the universal bottom (`?`).
    #[inline]
    pub fn is_unknown(&self) -> bool {
        matches!(self, ResolvedType::Unknown)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lowers_primitive_data_types() {
        assert_eq!(
            ResolvedType::from_data_type(&DataType::Integer),
            ResolvedType::Primitive(PrimitiveTy::Integer)
        );
        assert_eq!(
            ResolvedType::from_data_type(&DataType::Logical),
            ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn lowers_class_to_unknown_in_declare_pass() {
        assert_eq!(
            ResolvedType::from_data_type(&DataType::Class("Foo.Bar".into())),
            ResolvedType::Unknown
        );
    }
}
