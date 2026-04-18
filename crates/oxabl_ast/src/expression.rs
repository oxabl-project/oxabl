//! Expression AST nodes for Progress ABL.
//!
//! Expressions are parsed with the following precedence (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative > unary > postfix > primary.

use crate::{FindType, Literal, LockType, NodeId, PreprocIf, Span};

/// A named identifier with its source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

/// An ABL expression node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionKind {
    /// A literal value (integer, decimal, string, boolean, or unknown `?`).
    Literal(Literal),
    /// A variable or buffer reference.
    Identifier(Identifier),
    /// Arithmetic
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Modulo(Box<Expression>, Box<Expression>),
    /// Comparison
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanOrEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    Begins(Box<Expression>, Box<Expression>),
    Matches(Box<Expression>, Box<Expression>),
    Contains(Box<Expression>, Box<Expression>),
    /// Logical
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    /// Unary
    Negate(Box<Expression>), // Unary minus: -expr
    Not(Box<Expression>), // Logical NOT: NOT expr
    /// Ternary
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>), // condition, then_expr, else_expr
    /// Function call
    FunctionCall {
        name: Identifier,
        arguments: Vec<Expression>,
    },
    /// Object member access via colon syntax (`object:member`).
    MemberAccess {
        object: Box<Expression>,
        member: Identifier,
    },
    /// Object method call via colon syntax (`object:method(args)`).
    MethodCall {
        object: Box<Expression>,
        method: Identifier,
        arguments: Vec<Expression>,
    },
    /// Array/extent subscript access (`arr[index]`).
    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    /// Database field access via dot syntax (`table.field`).
    FieldAccess {
        qualifier: Box<Expression>,
        field: Identifier,
    },
    /// Include file reference in expression position: {file.i}, {file.i args}
    IncludeReference {
        /// Raw content between braces (trimmed)
        path_and_args: String,
        span: Span,
    },
    /// Include positional argument reference in expression position: {0}, {1}, {2}
    IncludeArgReference {
        index: i64,
        span: Span,
    },
    /// Preprocessor variable reference: `{&variable}`.
    PreprocReference(String),
    /// Mid-expression preprocessor conditional.
    /// The else_branch is semantically required (parser enforces this).
    PreprocIf(Box<PreprocIf<Expression>>),
    /// Object instantiation: `NEW ClassName(args)` or `NEW package.Class(args)`
    New {
        /// The class name, possibly dotted (e.g., "oe.wsdeco")
        class_name: String,
        arguments: Vec<Expression>,
    },
    /// CAN-FIND record phrase: `CAN-FIND([FIRST|LAST] table [WHERE expr] [lock] [NO-ERROR])`
    CanFind {
        find_type: FindType,
        buffer: Identifier,
        where_clause: Option<Box<Expression>>,
        lock_type: LockType,
        no_error: bool,
    },
}

/// An ABL expression node with parser-assigned identity.
///
/// Wraps an [`ExpressionKind`] with a stable [`NodeId`] for semantic side tables.
/// See `docs/design/ast-invariants.md` §NodeId invariants.
#[derive(Debug, Clone, Eq)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
}

impl Expression {
    /// Construct an `Expression` with `id` set to [`NodeId::DUMMY`].
    ///
    /// Intended for hand-constructed AST in tests. The parser always assigns
    /// a real NodeId via its allocator.
    #[inline]
    pub fn new(kind: ExpressionKind) -> Self {
        Expression {
            id: NodeId::DUMMY,
            kind,
        }
    }

    /// Construct an `Expression` with an explicit `NodeId`.
    ///
    /// Used by the parser; external callers should prefer [`Expression::new`].
    #[inline]
    pub fn with_id(id: NodeId, kind: ExpressionKind) -> Self {
        Expression { id, kind }
    }
}

impl PartialEq for Expression {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl PartialEq<ExpressionKind> for Expression {
    #[inline]
    fn eq(&self, other: &ExpressionKind) -> bool {
        &self.kind == other
    }
}

impl PartialEq<Expression> for ExpressionKind {
    #[inline]
    fn eq(&self, other: &Expression) -> bool {
        self == &other.kind
    }
}

impl From<ExpressionKind> for Expression {
    #[inline]
    fn from(kind: ExpressionKind) -> Self {
        Expression::new(kind)
    }
}
