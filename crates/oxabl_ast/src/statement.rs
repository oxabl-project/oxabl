use crate::{Expression, Identifier, Span};

/// Preprocessor conditional block, generic over the content type.
///
/// Used as:
/// - `Statement::PreprocIf(PreprocIf<Vec<Statement>>)` — statement level
/// - `Expression::PreprocIf(Box<PreprocIf<Expression>>)` — expression level
/// - `DataType::PreprocIf(Box<PreprocIf<DataType>>)` — data type level
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocIf<T> {
    pub condition: Expression,
    pub then_branch: T,
    pub elseif_branches: Vec<(Expression, T)>,
    pub else_branch: Option<T>,
}

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

    /// RUN statement — executes an internal procedure or external `.p` file.
    ///
    /// Supports static names (`RUN my-proc.`), dynamic dispatch (`RUN VALUE(expr).`),
    /// `IN handle`, `PERSISTENT [SET handle]`, `ASYNCHRONOUS [SET handle] [EVENT-PROCEDURE expr]`,
    /// and `NO-ERROR`.
    Run {
        target: RunTarget,
        arguments: Vec<RunArgument>,
        /// Handle for `RUN ... IN handle` (run on a persistent server).
        in_handle: Option<Expression>,
        /// Whether `PERSISTENT` was specified.
        persistent: bool,
        /// Handle variable for `PERSISTENT SET hProc`.
        persistent_handle: Option<Expression>,
        /// Whether `ASYNCHRONOUS` was specified.
        asynchronous: bool,
        /// Handle variable for `ASYNCHRONOUS SET hAsync`.
        async_handle: Option<Expression>,
        /// Event procedure for `ASYNCHRONOUS ... EVENT-PROCEDURE expr`.
        event_procedure: Option<Expression>,
        /// Whether `NO-ERROR` was specified.
        no_error: bool,
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

    /// DEFINE TEMP-TABLE statement.
    ///
    /// ```abl
    /// DEFINE TEMP-TABLE ttCustomer NO-UNDO
    ///     FIELD CustNum AS INTEGER
    ///     FIELD Name AS CHARACTER
    ///     INDEX idx1 IS PRIMARY UNIQUE CustNum.
    /// ```
    DefineTempTable {
        /// Table name.
        name: Identifier,
        /// Whether NO-UNDO was specified.
        no_undo: bool,
        /// Table to inherit structure from (`LIKE table-name`).
        like_table: Option<Identifier>,
        /// Whether VALIDATE was specified with LIKE.
        validate: bool,
        /// USE-INDEX clauses for LIKE.
        use_indexes: Vec<UseIndex>,
        /// Field definitions.
        fields: Vec<TempTableField>,
        /// Index definitions.
        indexes: Vec<TempTableIndex>,
    },

    /// DEFINE BUFFER statement.
    ///
    /// `DEFINE BUFFER bCust FOR Customer.`
    /// `DEFINE BUFFER bTT FOR TEMP-TABLE ttCustomer.`
    DefineBuffer {
        /// Buffer name.
        name: Identifier,
        /// The target table or temp-table.
        target: BufferTarget,
        /// Whether PRESELECT was specified.
        preselect: bool,
        /// Optional label for error messages.
        label: Option<String>,
    },

    /// CATCH block within a DO/REPEAT/FOR block.
    ///
    /// ```abl
    /// CATCH e AS Progress.Lang.Error:
    ///     MESSAGE e:GetMessage(1).
    /// END CATCH.
    /// ```
    Catch {
        /// The error variable name.
        error_var: Identifier,
        /// The error class type (e.g., "Progress.Lang.Error").
        error_type: String,
        /// Statements in the CATCH body.
        body: Vec<Statement>,
    },

    /// FINALLY block within a DO/REPEAT/FOR block.
    ///
    /// ```abl
    /// FINALLY:
    ///     cleanup().
    /// END FINALLY.
    /// ```
    Finally {
        /// Statements in the FINALLY body.
        body: Vec<Statement>,
    },

    /// UNDO/THROW statement.
    ///
    /// `UNDO, THROW NEW Progress.Lang.AppError("msg").`
    Throw(Expression),

    /// ASSIGN statement — multi-target assignment.
    ///
    /// `ASSIGN x = 1 y = 2 z = "hello".`
    Assign {
        /// One or more target = value pairs.
        assignments: Vec<AssignPair>,
    },

    /// FUNCTION definition.
    ///
    /// `FUNCTION name RETURNS type (params): body END FUNCTION.`
    Function {
        /// Function name.
        name: Identifier,
        /// Return data type.
        return_type: DataType,
        /// Function body statements (parameters are parsed as DEFINE PARAMETER).
        body: Vec<Statement>,
    },

    /// CLASS definition.
    ///
    /// ```text
    /// CLASS MyApp.CustomerService INHERITS BaseService IMPLEMENTS IService:
    ///   /* body */
    /// END CLASS.
    /// ```
    Class {
        /// Fully qualified class name (e.g., "MyApp.CustomerService").
        name: Identifier,
        /// Parent class name, if any.
        inherits: Option<Identifier>,
        /// Implemented interfaces.
        implements: Vec<Identifier>,
        /// Whether ABSTRACT was specified.
        is_abstract: bool,
        /// Whether FINAL was specified.
        is_final: bool,
        /// Class body statements (methods, properties, variables, constructors).
        body: Vec<Statement>,
    },

    /// METHOD definition inside a CLASS.
    ///
    /// `METHOD PUBLIC VOID DoSomething(INPUT x AS INTEGER): body END METHOD.`
    Method {
        /// Access modifier.
        access: AccessModifier,
        /// Whether STATIC was specified.
        is_static: bool,
        /// Whether ABSTRACT was specified (no body).
        is_abstract: bool,
        /// Whether OVERRIDE was specified.
        is_override: bool,
        /// Return type (None = VOID).
        return_type: Option<DataType>,
        /// Method name.
        name: Identifier,
        /// Parameters (reuses DefineParameter Statement variant).
        parameters: Vec<Statement>,
        /// Method body (empty vec if abstract).
        body: Vec<Statement>,
    },

    /// DEFINE PROPERTY inside a CLASS.
    ///
    /// ```text
    /// DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    ///     GET.
    ///     SET.
    /// ```
    Property {
        /// Access modifier.
        access: AccessModifier,
        /// Whether STATIC was specified.
        is_static: bool,
        /// Property name.
        name: Identifier,
        /// Property data type.
        data_type: DataType,
        /// Whether NO-UNDO was specified.
        no_undo: bool,
        /// GET accessor: None = no getter, Some(vec![]) = auto-getter (`GET.`),
        /// Some(body) = computed getter (`GET: body END GET.`).
        get_body: Option<Vec<Statement>>,
        /// SET accessor: None = no setter, Some(vec![]) = auto-setter (`SET.`),
        /// Some(body) = computed setter (`SET: body END SET.`).
        set_body: Option<Vec<Statement>>,
    },

    /// CONSTRUCTOR definition inside a CLASS.
    ///
    /// `CONSTRUCTOR PUBLIC MyClass(INPUT x AS INTEGER): body END CONSTRUCTOR.`
    Constructor {
        /// Access modifier.
        access: AccessModifier,
        /// Parameters (reuses DefineParameter Statement variant).
        parameters: Vec<Statement>,
        /// Constructor body.
        body: Vec<Statement>,
    },

    /// DESTRUCTOR definition inside a CLASS.
    ///
    /// `DESTRUCTOR PUBLIC MyClass(): body END DESTRUCTOR.`
    Destructor {
        /// Destructor body.
        body: Vec<Statement>,
    },

    /// INTERFACE definition.
    ///
    /// `INTERFACE IService: METHOD PUBLIC VOID Run(). END INTERFACE.`
    Interface {
        /// Interface name.
        name: Identifier,
        /// Interfaces this interface inherits from.
        inherits: Vec<Identifier>,
        /// Interface body (method signatures, property signatures).
        body: Vec<Statement>,
    },

    /// USING statement for class imports.
    ///
    /// `USING MyApp.Services.*.`
    /// `USING Progress.Lang.Object.`
    Using {
        /// Type name or wildcard path (e.g., "MyApp.Services.*").
        type_name: String,
    },

    /// CREATE buffer-name [NO-ERROR].
    Create { buffer: Identifier, no_error: bool },

    /// DELETE buffer-name [NO-ERROR].
    Delete { buffer: Identifier, no_error: bool },

    /// RELEASE buffer-name [NO-ERROR].
    Release { buffer: Identifier, no_error: bool },

    /// VALIDATE buffer-name [NO-ERROR].
    Validate { buffer: Identifier, no_error: bool },

    /// BUFFER-COPY source TO target [ASSIGN field = expr ...] [NO-ERROR].
    BufferCopy {
        source: Identifier,
        target: Identifier,
        assignments: Vec<(Identifier, Expression)>,
        no_error: bool,
    },

    /// BUFFER-COMPARE source TO target [SAVE RESULT IN lvar] [NO-ERROR].
    BufferCompare {
        source: Identifier,
        target: Identifier,
        result_var: Option<Identifier>,
        no_error: bool,
    },

    /// Preprocessor conditional: &IF cond &THEN stmts [&ELSEIF ...] [&ELSE stmts] &ENDIF
    PreprocIf(PreprocIf<Vec<Statement>>),

    /// &SCOPED-DEFINE name value / &GLOBAL-DEFINE name value
    PreprocDefine {
        name: Identifier,
        /// Byte offsets into source for the define value (not a String allocation).
        value_span: Option<Span>,
        is_global: bool,
    },

    /// &UNDEFINE name
    PreprocUndefine { name: Identifier },

    /// &MESSAGE expression
    PreprocMessage { expression: Expression },

    /// Leave statement - exit innermost loop
    Leave,

    /// Next statement - skip to next iteration
    Next,

    /// Return statement with an optional return value expression.
    Return(Option<Expression>),

    /// Empty (just a period)
    Empty,
}

/// Access modifier for OO-ABL members.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
    PackagePrivate,
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
    /// Conditional data type: &IF DEFINED(x) &THEN INTEGER &ELSE CHARACTER &ENDIF
    /// The else_branch is semantically required (parser enforces this).
    PreprocIf(Box<PreprocIf<DataType>>),
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

/// Source of a temp-table field's type — either explicit or inherited via LIKE.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldTypeSource {
    /// Explicit type: `FIELD x AS INTEGER`
    Explicit(DataType),
    /// Inherited type: `FIELD x LIKE Customer.CustNum [VALIDATE]`
    Like { source: Identifier, validate: bool },
}

/// A field definition in a DEFINE TEMP-TABLE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TempTableField {
    /// Field name.
    pub name: Identifier,
    /// Type source — either explicit (AS type) or inherited (LIKE field).
    pub type_source: FieldTypeSource,
    /// Optional initial value(s). Scalar fields have one element; array fields may have multiple.
    pub initial_value: Option<Vec<Expression>>,
    /// Extent/array size. None for scalar, Some(0) for dynamic extent.
    pub extent: Option<u32>,
}

/// A USE-INDEX clause in a DEFINE TEMP-TABLE LIKE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseIndex {
    pub name: Identifier,
    pub as_primary: bool,
}

/// An index definition in a DEFINE TEMP-TABLE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TempTableIndex {
    /// Index name.
    pub name: Identifier,
    /// Whether this is a PRIMARY index.
    pub is_primary: bool,
    /// Whether this index enforces UNIQUE values.
    pub is_unique: bool,
    /// Whether this is a WORD-INDEX.
    pub is_word_index: bool,
    /// Fields in this index with optional sort direction.
    pub fields: Vec<IndexField>,
}

/// A field in an index definition with optional sort direction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexField {
    pub name: Identifier,
    /// Explicit sort direction. None means inherit from previous field (or ASCENDING by default).
    pub direction: Option<SortDirection>,
}

/// Sort direction for index fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortDirection {
    Ascending,
    Descending,
}

/// Target of a DEFINE BUFFER statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BufferTarget {
    /// Buffer for a database table: `FOR Customer`
    Table(Identifier),
    /// Buffer for a temp-table: `FOR TEMP-TABLE ttCustomer`
    TempTable(Identifier),
}

/// A single target = value pair in an ASSIGN statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignPair {
    /// The assignment target (variable or field reference).
    pub target: Expression,
    /// The value to assign.
    pub value: Expression,
}
