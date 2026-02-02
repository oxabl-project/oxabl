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

    /// DO block with optional loop
    Do {
        /// loop variable assignment
        loop_var: Option<Identifier>,
        /// the from start value for loops
        from: Option<Expression>,
        /// End value for loops
        to: Option<Expression>,
        /// Step value for loops
        by: Option<Expression>,
        while_condition: Option<Expression>,
        body: Vec<Statement>,
    },

    /// IF statements
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },

    /// REPEAT block
    Repeat {
        while_condition: Option<Expression>,
        body: Vec<Statement>,
    },

    /// FOR EACH
    ForEach {
        buffer: Identifier,
        // Optional join to parent records
        of_relation: Option<Identifier>,
        where_clause: Option<Expression>,
        lock_type: LockType,
        body: Vec<Statement>,
    },

    /// FIND statement
    Find {
        find_type: FindType,
        buffer: Identifier,
        /// Key value for direct lookup (FIND Customer 1 syntax)
        key_value: Option<Expression>,
        where_clause: Option<Expression>,
        lock_type: LockType,
        no_error: bool,
    },

    /// Leave statement - exist innermost loop
    Leave,

    /// Next statement - skip to next iteration
    Next,

    /// Return statement to return [expression]
    Return(Option<Expression>),

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

/// ABL Record Lock Types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LockType {
    NoLock,
    ShareLock,
    ExclusiveLock,
}

/// ABL Find Type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FindType {
    First,
    Last,
    Next,
    Prev,
    Unique, // No qualifier
}
