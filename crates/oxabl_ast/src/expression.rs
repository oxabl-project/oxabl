//! Expression AST nodes for Progress ABL.
//!
//! Expressions are parsed with the following precedence (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative > unary > postfix > primary.

use crate::{Literal, Span};

/// A named identifier with its source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

/// An ABL expression node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
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
}
