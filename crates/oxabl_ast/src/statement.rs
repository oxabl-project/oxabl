use crate::{Expression, Identifier, NodeId, Span};
use smallvec::SmallVec;

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

// Guard against accidental growth of the AST's central enum. `StatementKind`'s
// size is dominated by its largest variant (`Class`/`Method`/`DefineDataset`).
// Adding the `span` field to the `Expression` wrapper (which several variants
// embed inline, e.g. `Assignment`/`Do`) grew this from 656 to 720. Gated to
// 64-bit; bump the bound deliberately if a variant legitimately grows.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<StatementKind>() <= 720);

// Lock the `Statement` wrapper size deliberately (KTD4). The wrapper adds a
// `NodeId` (4 bytes) and a `Span` (8 bytes) on top of `StatementKind`; padding
// keeps the total at `size_of::<StatementKind>() + 16`. Bump the bound
// deliberately if the wrapper legitimately grows.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Statement>() <= 720 + 16);

/// A statement in ABL — an executable unit that performs an action.
/// All statements are terminated by a period.
///
/// Wrapped by [`Statement`], which carries a parser-assigned [`NodeId`].
/// See `docs/design/ast-invariants.md` §NodeId invariants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    /// Variables
    VariableDeclaration {
        name: Identifier,
        type_source: TypeSource,
        initial_value: Option<Expression>,
        no_undo: bool,
        /// Extent/Array, none for scalar, some(0) for dynamic
        extent: Option<u32>,
        /// `DEFINE NEW SHARED …` — producer, visible to callees in this file.
        is_new_shared: bool,
        /// `DEFINE SHARED …` — consumer, binds an existing shared var.
        is_shared: bool,
        /// `DEFINE NEW GLOBAL SHARED …` — producer, visible session-wide.
        is_new_global_shared: bool,
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
        /// Whether the block runs inside a transaction (DO TRANSACTION:)
        transaction: bool,
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
        param_type: ParameterType,
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

    /// Include file reference at statement level: {file.i}, {file.i args}
    IncludeReference {
        /// Raw content between braces (trimmed), e.g. "file.i" or "file.i arg1 arg2"
        path_and_args: String,
        span: Span,
    },

    /// Include positional argument reference at statement level: {0}, {1}, {2}
    IncludeArgReference { index: i64, span: Span },

    /// Leave statement - exist innermost loop
    /// DISPLAY statement — outputs field/variable values to the screen or a frame.
    ///
    /// Each item is an expression with an optional per-item WHEN condition.
    /// The EXCEPT clause excludes fields, and WITH FRAME names the target frame.
    Display {
        /// Optional named stream (`DISPLAY STREAM s1 ...`).
        stream_name: Option<Identifier>,
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
        /// XML and serialization options (NAMESPACE-URI, SERIALIZE-NAME, etc.).
        xml_options: XmlSerializeOptions,
        /// `DEFINE NEW SHARED …` — producer, visible to callees in this file.
        is_new_shared: bool,
        /// `DEFINE SHARED …` — consumer, binds an existing shared temp-table.
        is_shared: bool,
        /// `DEFINE NEW GLOBAL SHARED …` — producer, visible session-wide.
        is_new_global_shared: bool,
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
        /// XML and serialization options (NAMESPACE-URI, SERIALIZE-NAME, etc.).
        xml_options: XmlSerializeOptions,
        /// `DEFINE NEW SHARED …` — producer, visible to callees in this file.
        is_new_shared: bool,
        /// `DEFINE SHARED …` — consumer, binds an existing shared buffer.
        is_shared: bool,
        /// `DEFINE NEW GLOBAL SHARED …` — producer, visible session-wide.
        is_new_global_shared: bool,
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
        /// One or more target = value pairs. Stored inline for the common case
        /// (≤4 pairs) to avoid a heap allocation per ASSIGN statement.
        assignments: SmallVec<[AssignPair; 4]>,
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
        /// Parameters of a computed SET accessor (`SET (INPUT pv AS CHARACTER):`).
        /// Empty when the setter has no parenthesized parameter list. Each
        /// entry is a `DefineParameter` statement (same shape as METHOD params).
        set_parameters: Vec<Statement>,
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

    /// CREATE statement — record creation or dynamic object creation.
    Create {
        target: CreateTarget,
        no_error: bool,
    },

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
        assignments: SmallVec<[AssignPair; 4]>,
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

    /// DEFINE STREAM stream-name.
    DefineStream { name: Identifier },

    /// DEFINE FRAME frame-name ... .
    /// Simplified: captures name and raw span of unparsed content for formatter round-tripping.
    DefineFrame { name: Identifier, raw_span: Span },

    /// DEFINE DATASET statement.
    ///
    /// ```abl
    /// DEFINE DATASET dsPerson FOR ttPerson, ttAddress
    ///     DATA-RELATION drPersonAddr FOR ttPerson, ttAddress
    ///     RELATION-FIELDS (personId, personId).
    /// ```
    DefineDataset {
        name: Identifier,
        access: Option<AccessModifier>,
        is_static: bool,
        /// `DEFINE NEW SHARED …` — producer, visible to callees in this file.
        is_new_shared: bool,
        /// `DEFINE SHARED …` — consumer, binds an existing shared dataset.
        is_shared: bool,
        /// `DEFINE NEW GLOBAL SHARED …` — producer, visible session-wide.
        is_new_global_shared: bool,
        serializable: bool,
        non_serializable: bool,
        xml_options: XmlSerializeOptions,
        reference_only: bool,
        buffers: Vec<Identifier>,
        data_relations: Vec<DataRelation>,
        parent_id_relations: Vec<ParentIdRelation>,
    },

    /// DEFINE DATA-SOURCE statement.
    ///
    /// `DEFINE DATA-SOURCE dsCustomer FOR Customer.`
    DefineDataSource {
        name: Identifier,
        access: Option<AccessModifier>,
        is_static: bool,
        query: Option<Identifier>,
        source_buffers: Vec<DataSourceBuffer>,
    },

    /// PUBLISH event-name [FROM publisher-handle] [(args...)].
    Publish {
        /// Event name — string literal or character expression.
        event_name: Expression,
        /// Optional FROM publisher-handle.
        from_handle: Option<Expression>,
        /// Arguments passed to subscribers (reuses RunArgument).
        arguments: Vec<RunArgument>,
    },

    /// SUBSCRIBE [PROCEDURE subscriber-handle] [TO] event-name {IN handle | ANYWHERE}
    ///   [RUN-PROCEDURE handler-name] [NO-ERROR].
    Subscribe {
        /// Optional PROCEDURE subscriber-handle.
        subscriber: Option<Expression>,
        /// Event name — string literal or character expression.
        event_name: Expression,
        /// IN publisher-handle or ANYWHERE (required).
        target: SubscribeTarget,
        /// Optional RUN-PROCEDURE handler name.
        run_procedure: Option<Identifier>,
        /// Whether NO-ERROR was specified.
        no_error: bool,
    },

    /// UNSUBSCRIBE [PROCEDURE subscriber-handle] [TO] {event-name | ALL} [IN publisher-handle].
    Unsubscribe {
        /// Optional PROCEDURE subscriber-handle.
        subscriber: Option<Expression>,
        /// Event name, or None if ALL was specified.
        event_name: Option<Expression>,
        /// Optional IN publisher-handle.
        in_handle: Option<Expression>,
    },

    /// DEFINE [access] [STATIC] [ABSTRACT] EVENT event-name SIGNATURE VOID (params...).
    DefineEvent {
        /// Access modifier (defaults to PUBLIC).
        access: AccessModifier,
        /// Whether STATIC was specified.
        is_static: bool,
        /// Whether ABSTRACT was specified.
        is_abstract: bool,
        /// Event name.
        name: Identifier,
        /// Signature parameters (reuses DefineParameter via Vec<Statement>).
        parameters: Vec<Statement>,
    },

    /// INPUT/OUTPUT/INPUT-OUTPUT stream I/O statement.
    StreamIo {
        direction: StreamDirection,
        stream_name: Option<Identifier>,
        operation: StreamOperation,
    },

    /// ON trigger statement -- event handlers for UI, database, and key events.
    ///
    /// ```abl
    /// ON CHOOSE OF btnOk IN FRAME f1 DO: /* ... */ END.
    /// ON WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld DO: /* ... */ END.
    /// ON F1 HELP.
    /// ```
    On { kind: OnKind },

    /// TRIGGER PROCEDURE FOR event OF table [NEW/OLD clauses].
    ///
    /// Declares a schema trigger -- always the first statement in a trigger procedure file.
    ///
    /// ```abl
    /// TRIGGER PROCEDURE FOR WRITE OF Customer
    ///     NEW BUFFER bNew OLD BUFFER bOld.
    /// ```
    TriggerProcedure {
        /// The trigger event (CREATE, DELETE, FIND, WRITE, ASSIGN, or REPLICATION-*).
        event: DbTriggerEvent,
        /// The target table (or table.field for ASSIGN OF form).
        target: Identifier,
        /// NEW/OLD BUFFER referencing (WRITE triggers).
        referencing: TriggerReferencing,
        /// NEW VALUE variable definition (ASSIGN triggers, mutually exclusive with OF form).
        new_value: Option<TriggerAssignParam>,
        /// OLD VALUE variable definition (ASSIGN NEW VALUE form).
        old_value_param: Option<TriggerAssignParam>,
    },

    /// Leave statement - exit innermost loop, optionally naming the target label
    Leave(Option<String>),

    /// Next statement - skip to next iteration, optionally naming the target label
    Next(Option<String>),

    /// Return statement with an optional return value expression.
    Return(Option<Expression>),

    /// Empty (just a period)
    Empty,

    /// A statement form the parser *recognizes* but does not model: the
    /// dispatch site matched a leading keyword and then skipped the statement's
    /// tokens wholesale (`PUT`, `UPDATE`, `ENABLE`, `EXPORT`, embedded SQL, …).
    ///
    /// Distinct from [`Self::Empty`], which means error recovery or a genuinely
    /// empty statement. The distinction matters because these forms carry real
    /// variable traffic in both directions, and consumers that reason about
    /// whether a variable was touched must not read a `Skipped` node as "nothing
    /// happened here".
    ///
    /// `names` holds the identifier-shaped tokens the skip passed over, filtered
    /// lexically: the dispatch keyword itself is dropped, as is any token
    /// byte-adjacent to a preceding `.`, `:`, or `/` (so `table.field` keeps only
    /// `table`, and a path like `/usr/tmp/log.txt` keeps nothing). The filter is
    /// deliberately broad — ABL lexes a variable named `value` as a keyword kind,
    /// so option keywords are kept as candidate names and the semantic pass's
    /// non-diagnostic lookup is the real filter. Over-inclusion can only silence
    /// a diagnostic, never invent one.
    ///
    /// `may_reference_tables` is a narrow marker, `false` for every ordinary
    /// unmodelled form. `true` means the form is one whose grammar names a table
    /// or temp-table (`DEFINE QUERY`, `OPEN QUERY`, `EMPTY TEMP-TABLE`), so the
    /// semantic pass should additionally offer `names` to the buffer/table
    /// namespaces as read candidates. It is a request for a conservative extra
    /// lookup, not an assertion that any particular name *is* a table — the same
    /// silent-on-miss resolution applies, so an over-inclusive candidate can only
    /// silence a diagnostic, never invent one.
    ///
    /// The statement's full extent is [`Statement::span`]; no companion
    /// `raw_span` is needed.
    Skipped {
        names: Vec<Identifier>,
        may_reference_tables: bool,
    },

    /// A labeled block: `LABEL: DO: ... END.` or `LABEL: REPEAT: ... END.`
    /// The label can be referenced by LEAVE and NEXT statements.
    Label { name: String, body: Box<Statement> },
}

/// A statement in ABL paired with its parser-assigned [`NodeId`].
///
/// `PartialEq` is implemented manually to ignore the `id` field, so
/// structural value-equality in tests continues to work unchanged — tests can
/// hand-construct `Statement` values via [`Statement::new`] (which defaults
/// `id` to [`NodeId::DUMMY`]) or compare directly against a [`StatementKind`]
/// value (the impl short-circuits through the wrapper).
///
/// See `docs/design/ast-invariants.md` §NodeId invariants.
#[derive(Debug, Clone, Eq)]
pub struct Statement {
    pub id: NodeId,
    pub kind: StatementKind,
    /// Full byte extent of this statement in source, including its trailing
    /// `.`/`:`. Defaults to [`Span::DUMMY`] on hand-constructed nodes; the
    /// parser stamps a real span. Excluded from `PartialEq`
    /// (`docs/design/ast-invariants.md` §1).
    pub span: Span,
}

impl Statement {
    /// Construct a `Statement` with `id` set to [`NodeId::DUMMY`] and `span`
    /// set to [`Span::DUMMY`].
    ///
    /// Intended for hand-constructed AST in tests. The parser always assigns
    /// a real NodeId and span via [`Statement::with_id`].
    #[inline]
    pub fn new(kind: StatementKind) -> Self {
        Statement {
            id: NodeId::DUMMY,
            kind,
            span: Span::DUMMY,
        }
    }

    /// Construct a `Statement` with an explicit `NodeId` and `span`.
    ///
    /// Used by the parser; external callers should prefer [`Statement::new`].
    #[inline]
    pub fn with_id(id: NodeId, span: Span, kind: StatementKind) -> Self {
        Statement { id, kind, span }
    }
}

impl PartialEq for Statement {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl PartialEq<StatementKind> for Statement {
    #[inline]
    fn eq(&self, other: &StatementKind) -> bool {
        &self.kind == other
    }
}

impl PartialEq<Statement> for StatementKind {
    #[inline]
    fn eq(&self, other: &Statement) -> bool {
        self == &other.kind
    }
}

impl From<StatementKind> for Statement {
    #[inline]
    fn from(kind: StatementKind) -> Self {
        Statement::new(kind)
    }
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
    Current,
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
    /// Return value parameter (DEFINE RETURN PARAMETER).
    Return,
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

/// Source of a type specification — either explicit (`AS type`) or inherited (`LIKE field`).
///
/// Used in variable declarations, parameter declarations, and temp-table field definitions.
/// For temp-table fields, the enclosing [`TempTableField`] carries the `validate` flag
/// separately (VALIDATE is not valid on DEFINE VARIABLE or DEFINE PARAMETER LIKE).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSource {
    /// Explicit type: `... AS INTEGER` or `... AS CLASS Foo`
    Explicit(DataType),
    /// Inherited type: `... LIKE Customer.CustNum`
    Like { source: Identifier },
}

/// A field definition in a DEFINE TEMP-TABLE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TempTableField {
    /// Field name.
    pub name: Identifier,
    /// Type source — either explicit (AS type) or inherited (LIKE field).
    pub type_source: TypeSource,
    /// Whether to inherit validation rules from the source field (`LIKE field VALIDATE`).
    /// Only meaningful when `type_source` is [`TypeSource::Like`]; always `false` otherwise.
    pub validate: bool,
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

/// Direction for stream I/O statements.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum StreamDirection {
    Input,
    Output,
    InputOutput,
}

/// Operation for stream I/O statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StreamOperation {
    From(Expression),
    To { target: Expression, append: bool },
    Through(Expression),
    Close,
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

// =============================================================================
// XML / Serialize options (shared across TEMP-TABLE, BUFFER, DATASET)
// =============================================================================

/// XML and serialization options shared by TEMP-TABLE, BUFFER, and DATASET definitions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct XmlSerializeOptions {
    pub namespace_uri: Option<Identifier>,
    pub namespace_prefix: Option<Identifier>,
    pub xml_node_name: Option<Identifier>,
    pub xml_node_type: Option<Identifier>,
    pub serialize_name: Option<Identifier>,
    pub serialize_hidden: bool,
}

// =============================================================================
// Dataset types
// =============================================================================

/// A DATA-RELATION clause in a DEFINE DATASET statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRelation {
    pub name: Option<Identifier>,
    pub parent_buffer: Identifier,
    pub child_buffer: Identifier,
    /// Pairs of (parent_field, child_field) from RELATION-FIELDS.
    pub relation_fields: Vec<(Identifier, Identifier)>,
    pub reposition: bool,
    pub nested: bool,
    pub foreign_key_hidden: bool,
    pub not_active: bool,
    pub recursive: bool,
}

/// A PARENT-ID-RELATION clause in a DEFINE DATASET statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParentIdRelation {
    pub name: Option<Identifier>,
    pub parent_buffer: Identifier,
    pub child_buffer: Identifier,
    pub id_field: Identifier,
    pub parent_fields_before: Vec<Identifier>,
    pub parent_fields_after: Vec<Identifier>,
}

// =============================================================================
// Data-source types
// =============================================================================

/// A source buffer phrase in a DEFINE DATA-SOURCE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataSourceBuffer {
    pub name: Identifier,
    pub keys: Option<DataSourceKeys>,
}

/// KEYS clause in a DATA-SOURCE source buffer phrase.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataSourceKeys {
    /// KEYS (field1, field2, ...)
    Fields(Vec<Identifier>),
    /// KEYS (ROWID)
    Rowid,
}

// =============================================================================
// CREATE target types
// =============================================================================

/// Target of a CREATE statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CreateTarget {
    /// CREATE buffer-name — record creation or any unrecognized CREATE.
    Name(Identifier),
    /// CREATE DATASET/DATA-SOURCE/TEMP-TABLE handle [IN WIDGET-POOL pool].
    Handle {
        kind: CreateTargetKind,
        handle: Identifier,
        widget_pool: Option<Expression>,
    },
}

/// The type keyword in a CREATE ... handle statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CreateTargetKind {
    Dataset,
    DataSource,
    TempTable,
    Buffer,
    Query,
    Widget,
}

// =============================================================================
// Parameter types
// =============================================================================

/// The type/shape of a DEFINE PARAMETER statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParameterType {
    /// Standard variable parameter: DEFINE INPUT PARAMETER name AS type [NO-UNDO].
    Variable {
        name: Identifier,
        type_source: TypeSource,
        no_undo: bool,
    },
    /// Handle-based parameter: TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE
    Handle {
        kind: HandleParamKind,
        name: Identifier,
        passing: HandlePassingOptions,
    },
    /// Buffer parameter: DEFINE PARAMETER BUFFER buf FOR table.
    Buffer {
        name: Identifier,
        target: Identifier,
    },
}

/// Discriminant for handle-based parameter types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandleParamKind {
    Table,
    TableHandle,
    Dataset,
    DatasetHandle,
}

/// Passing options for handle-based parameters (APPEND, BIND, BY-VALUE).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HandlePassingOptions {
    pub append: bool,
    pub bind: bool,
    pub by_value: bool,
}

// =============================================================================
// Event system types
// =============================================================================

/// Target for SUBSCRIBE — where to listen for events.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubscribeTarget {
    /// Subscribe to events from a specific publisher handle.
    InHandle(Expression),
    /// Subscribe to events from any publisher.
    Anywhere,
}

// =============================================================================
// ON trigger types
// =============================================================================

/// Discriminant for the different forms of the ON statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OnKind {
    /// UI/developer event trigger (includes "WEB-NOTIFY" ANYWHERE form):
    /// ON event-list OF widget-list [OR event-list OF widget-list]... [ANYWHERE]
    ///   { trigger-block | REVERT | PERSISTENT RUN proc [(args)] }
    UiEvent {
        /// Event/widget clauses -- at least one, chained via OR.
        /// Empty when ANYWHERE is used standalone (e.g., ON "WEB-NOTIFY" ANYWHERE).
        clauses: Vec<OnEventClause>,
        /// Whether ANYWHERE was specified.
        anywhere: bool,
        /// The trigger action.
        action: OnAction,
    },
    /// Database event trigger:
    /// ON CREATE|DELETE|FIND|WRITE|ASSIGN OF table [referencing] [OVERRIDE]
    ///   { trigger-block | REVERT }
    DbEvent {
        /// The database event.
        event: DbTriggerEvent,
        /// The table (or table.field for ASSIGN) the trigger is on.
        target: Identifier,
        /// NEW/OLD BUFFER/VALUE referencing phrases.
        referencing: TriggerReferencing,
        /// Whether OVERRIDE was specified.
        is_override: bool,
        /// The trigger action (block or REVERT).
        action: OnAction,
    },
    /// Key remapping: ON key-label key-function.
    KeyRemap {
        /// The key label (e.g., F1, CTRL-X) -- any identifier.
        key_label: Identifier,
        /// The key function (e.g., HELP, ENDKEY, GO) -- any identifier.
        key_function: Identifier,
    },
}

/// A single event/widget-list clause in a UI ON trigger.
///
/// `ON CHOOSE, ENTRY OF btnOk IN FRAME f1, btnCancel`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OnEventClause {
    /// Comma-separated event names (identifiers, including keywords like LEAVE/ENTRY).
    pub events: Vec<Identifier>,
    /// Comma-separated widget references with optional frame/browse qualifiers.
    pub widgets: Vec<WidgetRef>,
}

/// The action taken by an ON trigger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OnAction {
    /// A trigger block -- either a single statement or DO...END block.
    Block(Box<Statement>),
    /// REVERT -- removes the trigger.
    Revert,
    /// PERSISTENT RUN procedure [(args)].
    PersistentRun {
        procedure: Identifier,
        arguments: Vec<Expression>,
    },
}

/// Database trigger event types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DbTriggerEvent {
    Create,
    Delete,
    Find,
    Write,
    Assign,
    ReplicationCreate,
    ReplicationDelete,
    ReplicationWrite,
}

/// Referencing phrase for database triggers (NEW/OLD BUFFER for WRITE, OLD VALUE for ASSIGN).
/// Shared between ON db-event triggers and TRIGGER PROCEDURE.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TriggerReferencing {
    /// NEW [BUFFER] alias (WRITE triggers).
    pub new_buffer: Option<Identifier>,
    /// OLD [BUFFER] alias (WRITE triggers).
    pub old_buffer: Option<Identifier>,
    /// OLD [VALUE] alias (ASSIGN triggers in ON statement).
    pub old_value: Option<Identifier>,
}

/// Widget reference in an ON trigger, with optional frame/browse qualifier.
///
/// `btnOk IN FRAME main-frame` or `col1 IN BROWSE brw1`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WidgetRef {
    pub name: Identifier,
    pub qualifier: Option<WidgetQualifier>,
}

/// Optional qualification for a widget reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WidgetQualifier {
    /// IN FRAME frame-name
    InFrame(Identifier),
    /// IN BROWSE browse-name
    InBrowse(Identifier),
}

/// A variable-like parameter for TRIGGER PROCEDURE FOR ASSIGN NEW VALUE form.
///
/// ```abl
/// TRIGGER PROCEDURE FOR ASSIGN
///     NEW VALUE newVal AS CHARACTER
///     OLD VALUE oldVal AS CHARACTER.
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TriggerAssignParam {
    pub name: Identifier,
    pub data_type: DataType,
}

#[cfg(test)]
mod skipped_tests {
    use super::{Statement, StatementKind};
    use crate::node_id::NodeId;
    use crate::span::Span;

    fn skipped(names: Vec<&str>, span_start: u32) -> Statement {
        let mut s = Statement::new(StatementKind::Skipped {
            names: names
                .into_iter()
                .map(|n| crate::Identifier {
                    name: n.to_string(),
                    span: Span {
                        start: span_start,
                        end: span_start + n.len() as u32,
                    },
                })
                .collect(),
            may_reference_tables: false,
        });
        s.span = Span {
            start: span_start,
            end: span_start + 10,
        };
        s
    }

    /// `PartialEq` ignores `id` and `span`, as it does for every other variant
    /// (§2 of `docs/design/ast-invariants.md`).
    #[test]
    fn skipped_equality_ignores_id_and_span() {
        let mut a = Statement::new(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: false,
        });
        let mut b = Statement::new(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: false,
        });
        a.id = NodeId::from_u32(7);
        a.span = Span { start: 0, end: 4 };
        b.id = NodeId::from_u32(99);
        b.span = Span { start: 40, end: 90 };
        assert_eq!(a, b);
    }

    /// The whole point of the variant: a recognized-but-unmodelled statement is
    /// not an error-recovery `Empty`. A consumer that folded them together would
    /// read "the parser skipped a `PUT` that reads v-total" as "nothing here".
    #[test]
    fn skipped_is_not_empty() {
        let a = Statement::new(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: false,
        });
        let b = Statement::new(StatementKind::Empty);
        assert_ne!(a, b);
        assert_ne!(a, StatementKind::Empty);
    }

    /// Harvested names participate in equality — two skips over different
    /// content are different statements. `Identifier` equality includes `span`,
    /// so tests that care about names should assert on the vec's contents rather
    /// than hand-building whole statements.
    #[test]
    fn skipped_equality_discriminates_on_names() {
        assert_ne!(skipped(vec!["v-total"], 0), skipped(vec!["v-count"], 0));
        assert_eq!(skipped(vec!["v-total"], 0), skipped(vec!["v-total"], 0));
    }

    /// The table-candidate marker participates in structural equality. It is a
    /// semantic difference, not an annotation: the marked node asks the resolve
    /// pass for a buffer/table lookup the unmarked one does not get, so two nodes
    /// over identical tokens are not interchangeable.
    #[test]
    fn skipped_equality_discriminates_on_table_marker() {
        let unmarked = Statement::new(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: false,
        });
        let marked = Statement::new(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: true,
        });
        assert_ne!(unmarked, marked);
    }
}
