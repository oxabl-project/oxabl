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

    /// Case statement
    Case {
        expression: Expression,
        when_branches: Vec<WhenBranch>,
        otherwise: Option<Vec<Statement>>,
    },

    /// Procedures
    Procedure {
        name: Identifier,
        body: Vec<Statement>, // parameters are parsed as DEFINE PARAMTER statements
    },

    /// Define input/output params
    DefineParameter {
        direction: ParameterDirection,
        name: Identifier,
        data_type: DataType,
        no_undo: bool,
    },

    /// Run statements
    Run {
        target: RunTarget,
        arguments: Vec<RunArgument>,
    },

    /// DISPLAY statement — outputs field/variable values to the screen or a frame.
    ///
    /// Each item is an expression with an optional per-item WHEN condition.
    /// The EXCEPT clause excludes fields, and WITH FRAME names the target frame.
    Display {
        /// Expressions to display, each with an optional WHEN condition.
        items: Vec<DisplayItem>,
        /// Fields to exclude (`DISPLAY Customer EXCEPT CustNum`).
        except: Vec<Identifier>,
        /// Optional frame name (`WITH FRAME f1`).
        frame: Option<Identifier>,
    },

    /// MESSAGE statement — displays messages to the user (console or alert box).
    ///
    /// Items are the expressions in the message body. SET/UPDATE targets are
    /// variables that receive user input (e.g., button responses).
    Message {
        /// Expressions in the message body.
        items: Vec<Expression>,
        /// Variables bound by SET or UPDATE clauses.
        set_targets: Vec<Identifier>,
    },

    /// Leave statement - exit innermost loop
    Leave,

    /// Next statement - skip to next iteration
    Next,

    /// Return statement with an optional return value expression.
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

/// A single WHEN branch in a CASE statement.
///
/// Supports multiple match values via `WHEN "a" OR WHEN "b"` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhenBranch {
    /// One or more values to match against the CASE expression.
    pub values: Vec<Expression>,
    /// Statements to execute when a value matches.
    pub body: Vec<Statement>,
}

/// Direction qualifier for a procedure parameter (INPUT, OUTPUT, or INPUT-OUTPUT).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParameterDirection {
    /// Parameter is passed into the procedure.
    Input,
    /// Parameter is returned from the procedure.
    Output,
    /// Parameter is both passed in and returned.
    InputOutput,
}

/// Target of a RUN statement -- either a static procedure name or a dynamic expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RunTarget {
    /// Static procedure name, e.g. `RUN my-proc.p` or `RUN "file.p"`.
    Literal(String),
    /// Dynamic target via `RUN VALUE(expr)`.
    Dynamic(Expression),
}

/// A single argument passed to a RUN statement, with its [`ParameterDirection`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunArgument {
    pub direction: ParameterDirection,
    pub expression: Expression,
}

/// A single item in a DISPLAY statement.
///
/// Each display item is an expression with an optional WHEN condition
/// that controls whether it is displayed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisplayItem {
    /// The expression to display.
    pub expression: Expression,
    /// Optional `WHEN condition` — controls whether this item is displayed.
    pub when_condition: Option<Expression>,
}
