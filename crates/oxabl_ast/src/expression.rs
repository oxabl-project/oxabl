//! Expression AST nodes for Progress ABL.
//!
//! Expressions are parsed with the following precedence (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative > unary > postfix > primary.
//!
//! Every expression node is stored contiguously in an [`ExprPool`](crate::ExprPool)
//! and referenced by its 4-byte [`ExprId`]; argument lists are referenced by
//! 8-byte [`ExprList`] slices.

use crate::{
    ExprId, ExprList, FindType, Literal, LockType, NameId, NodeId, PreprocIf, Span, TypeArguments,
};

/// A named identifier with its source location and interned [`NameId`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub span: Span,
    pub name: NameId,
}

// Lock the size of a name deliberately: the tree holds one for each name in the
// source. Gated to 64-bit; bump the bound deliberately.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Identifier>() <= 12);

/// An ABL expression node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionKind {
    /// A literal value (integer, decimal, string, boolean, or unknown `?`).
    Literal(Literal),
    /// A variable or buffer reference.
    Identifier(Identifier),
    /// Arithmetic
    Add(ExprId, ExprId),
    Minus(ExprId, ExprId),
    Multiply(ExprId, ExprId),
    Divide(ExprId, ExprId),
    Modulo(ExprId, ExprId),
    /// Comparison
    Equal(ExprId, ExprId),
    NotEqual(ExprId, ExprId),
    LessThan(ExprId, ExprId),
    LessThanOrEqual(ExprId, ExprId),
    GreaterThan(ExprId, ExprId),
    GreaterThanOrEqual(ExprId, ExprId),
    Begins(ExprId, ExprId),
    Matches(ExprId, ExprId),
    Contains(ExprId, ExprId),
    /// Logical
    And(ExprId, ExprId),
    Or(ExprId, ExprId),
    /// Unary
    Negate(ExprId), // Unary minus: -expr
    Not(ExprId), // Logical NOT: NOT expr
    /// Ternary
    IfThenElse(ExprId, ExprId, ExprId), // condition, then_expr, else_expr
    /// Function call
    FunctionCall {
        name: Identifier,
        arguments: ExprList,
    },
    /// Object member access via colon syntax (`object:member`).
    MemberAccess {
        object: ExprId,
        member: Identifier,
    },
    /// Object method call via colon syntax (`object:method(args)`).
    MethodCall {
        object: ExprId,
        method: Identifier,
        arguments: ExprList,
    },
    /// Array/extent subscript access (`arr[index]`).
    ArrayAccess {
        array: ExprId,
        index: ExprId,
    },
    /// Database field access via dot syntax (`table.field`).
    FieldAccess {
        qualifier: ExprId,
        field: Identifier,
    },
    /// Include file reference in expression position: {file.i}, {file.i args}
    IncludeReference {
        /// Raw content between braces (trimmed), interned in [`NamePool`](crate::NamePool).
        path_and_args: NameId,
        span: Span,
    },
    /// Include positional argument reference in expression position: {0}, {1}, {2}
    IncludeArgReference {
        index: i64,
        span: Span,
    },
    /// Preprocessor variable reference: `{&variable}`.
    PreprocReference(NameId),
    /// Mid-expression preprocessor conditional.
    /// The else_branch is semantically required (parser enforces this).
    PreprocIf(Box<PreprocIf<ExprId>>),
    /// Object instantiation: `NEW ClassName(args)` or `NEW package.Class(args)`
    New {
        /// The class name, possibly dotted (e.g., "app.Widget"), interned.
        class_name: NameId,
        /// The type arguments of a generic class, as in `NEW List<pkg.Thing>()`.
        type_arguments: Option<Box<TypeArguments>>,
        arguments: ExprList,
    },
    /// CAN-FIND record phrase: `CAN-FIND([FIRST|LAST] table [WHERE expr] [lock] [NO-ERROR])`
    CanFind {
        find_type: FindType,
        buffer: Identifier,
        where_clause: Option<ExprId>,
        lock_type: LockType,
        no_error: bool,
    },
}

// Lock the wrapper sizes deliberately (KTD4). With `ExprId`/`ExprList`/`NameId`,
// `ExpressionKind` shrinks from 64 B to <= 32 B and `Expression` from 80 B to
// <= 48 B. Gated to 64-bit; bump the bounds deliberately if a variant grows.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<ExpressionKind>() <= 32);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Expression>() <= 48);

/// An ABL expression node with parser-assigned identity.
///
/// Wraps an [`ExpressionKind`] with a stable [`NodeId`] for semantic side tables.
/// See `docs/design/ast-invariants.md` §NodeId invariants.
#[derive(Debug, Clone, Eq)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
    /// Full byte extent of this expression in source (parenthesized groups
    /// include the enclosing parens). Defaults to [`Span::DUMMY`] on
    /// hand-constructed nodes; the parser stamps a real span. Excluded from
    /// `PartialEq` (`docs/design/ast-invariants.md` §1).
    pub span: Span,
}

impl Expression {
    /// Construct an `Expression` with `id` set to [`NodeId::DUMMY`] and `span`
    /// set to [`Span::DUMMY`].
    ///
    /// Intended for hand-constructed AST in tests. The parser always assigns
    /// a real NodeId and span via [`Expression::with_id`].
    #[inline]
    pub fn new(kind: ExpressionKind) -> Self {
        Expression {
            id: NodeId::DUMMY,
            kind,
            span: Span::DUMMY,
        }
    }

    /// Construct an `Expression` with an explicit `NodeId` and `span`.
    ///
    /// Used by the parser; external callers should prefer [`Expression::new`].
    #[inline]
    pub fn with_id(id: NodeId, span: Span, kind: ExpressionKind) -> Self {
        Expression { id, kind, span }
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
