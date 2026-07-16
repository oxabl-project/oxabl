//! Resolved types.
//!
//! `ResolvedType` is the semantic layer's type currency. Declaration-carried
//! types (variable `AS INTEGER`, parameter `AS CLASS`, property `AS
//! DATETIME`) are lowered into [`ResolvedType`] by the declare pass; the
//! resolve and type-check passes extend this side table with inferred
//! expression types in Phases 4a/4b.

use oxabl_ast::DataType;
use oxabl_schema::{Field, SchemaRevision, SchemaType, TableId};

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

    /// Lower a schema [`Field`] into a [`ResolvedType`]. Each `SchemaType`
    /// primitive maps 1:1 to the corresponding [`PrimitiveTy`];
    /// `SchemaType::Unknown(_)` maps to `Unknown` and `SchemaType::Error`
    /// maps to `Error` (the "prior error, suppress cascade" bottom — never
    /// collapsed into `Unknown`). `EXTENT` fields wrap the scalar conversion
    /// in `Array`, mirroring the declare pass's `wrap_extent`: extent `0` is
    /// dynamic and represented as `None`.
    pub fn from_schema_field(field: &Field) -> Self {
        let scalar = match &field.data_type {
            SchemaType::Integer => ResolvedType::Primitive(PrimitiveTy::Integer),
            SchemaType::Int64 => ResolvedType::Primitive(PrimitiveTy::Int64),
            SchemaType::Decimal => ResolvedType::Primitive(PrimitiveTy::Decimal),
            SchemaType::Character => ResolvedType::Primitive(PrimitiveTy::Character),
            SchemaType::Longchar => ResolvedType::Primitive(PrimitiveTy::Longchar),
            SchemaType::Logical => ResolvedType::Primitive(PrimitiveTy::Logical),
            SchemaType::Date => ResolvedType::Primitive(PrimitiveTy::Date),
            SchemaType::Datetime => ResolvedType::Primitive(PrimitiveTy::Datetime),
            SchemaType::DatetimeTz => ResolvedType::Primitive(PrimitiveTy::DatetimeTz),
            SchemaType::Handle => ResolvedType::Primitive(PrimitiveTy::Handle),
            SchemaType::Raw => ResolvedType::Primitive(PrimitiveTy::Raw),
            SchemaType::Recid => ResolvedType::Primitive(PrimitiveTy::Recid),
            SchemaType::Rowid => ResolvedType::Primitive(PrimitiveTy::Rowid),
            SchemaType::Blob => ResolvedType::Primitive(PrimitiveTy::Blob),
            SchemaType::Clob => ResolvedType::Primitive(PrimitiveTy::Clob),
            SchemaType::Unknown(_) => ResolvedType::Unknown,
            SchemaType::Error => ResolvedType::Error,
        };
        match field.extent {
            None => scalar,
            Some(n) => ResolvedType::Array {
                element: Box::new(scalar),
                // ABL extent `0` is dynamic; represent as `None`.
                extent: if n == 0 { None } else { Some(n) },
            },
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

    // ---- from_schema_field ------------------------------------------------

    fn schema_field(ty: SchemaType, extent: Option<u32>) -> Field {
        use oxabl_common::{FileId, FileSpan};
        Field {
            name: oxabl_lexer::oxabl_atom::OxablAtom::from("f"),
            display_name: "f".into(),
            data_type: ty,
            extent,
            mandatory: false,
            case_sensitive: false,
            format: None,
            label: None,
            initial: None,
            description: None,
            decimals: None,
            position: None,
            order: None,
            max_width: None,
            help: None,
            valexp: None,
            valmsg: None,
            extras: Vec::new(),
            source: FileSpan {
                file: FileId::UNKNOWN,
                span: oxabl_ast::Span { start: 0, end: 0 },
            },
        }
    }

    #[test]
    fn schema_field_primitives_map_one_to_one() {
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::Integer, None)),
            ResolvedType::Primitive(PrimitiveTy::Integer)
        );
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::DatetimeTz, None)),
            ResolvedType::Primitive(PrimitiveTy::DatetimeTz)
        );
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::Clob, None)),
            ResolvedType::Primitive(PrimitiveTy::Clob)
        );
    }

    #[test]
    fn schema_field_unknown_and_error_preserve_lattice_roles() {
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(
                SchemaType::Unknown(oxabl_lexer::oxabl_atom::OxablAtom::from("weird")),
                None
            )),
            ResolvedType::Unknown
        );
        // `Error` is the suppress-cascade bottom — never collapsed to Unknown.
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::Error, None)),
            ResolvedType::Error
        );
    }

    #[test]
    fn schema_field_extent_wraps_array() {
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::Character, Some(3))),
            ResolvedType::Array {
                element: Box::new(ResolvedType::Primitive(PrimitiveTy::Character)),
                extent: Some(3),
            }
        );
        // Extent `0` is dynamic — represented as `None`.
        assert_eq!(
            ResolvedType::from_schema_field(&schema_field(SchemaType::Integer, Some(0))),
            ResolvedType::Array {
                element: Box::new(ResolvedType::Primitive(PrimitiveTy::Integer)),
                extent: None,
            }
        );
    }
}
