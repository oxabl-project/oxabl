use crate::{Expression, Identifier};

/// A statement in ABL - an executable unit that performs an action.
/// All statements are terminated by a period.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Variables
    VariableDeclaration {
        name: Identifier,
        data_type: DataType,
        initial_value: Option<Expression>,
        no_undo: bool,
        /// Extent/Array, none for scalar, some(0) for dynamic
        extent: Option<u32>,
    },

    /// Assignment
    Assignment {
        target: Expression,
        value: Expression,
    },

    /// Expression Statement
    ExpressionStatement(Expression),

    /// Code block
    Block(Vec<Statement>),

    /// Empty (just a period)
    Empty,
}

/// ABL data types for variable declarations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Integer,
    Int64,
    Decimal,
    Character,
    Logical,
    Date,
    DateTime,
    DateTimeTz,
    Handle,
    Rowid,
    Recid,
    Raw,
    Memptr,
    Longchar,
    Clob,
    Blob,
    Com,
    /// Class type with fully qualified name
    Class(String),
}
