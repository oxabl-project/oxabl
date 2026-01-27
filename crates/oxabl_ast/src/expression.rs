use crate::{Literal, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    // Arithmetic
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Modulo(Box<Expression>, Box<Expression>),
    // Comparison
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanOrEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    Begins(Box<Expression>, Box<Expression>),
    Matches(Box<Expression>, Box<Expression>),
    Contains(Box<Expression>, Box<Expression>),
    // Logical
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    // Unary
    Negate(Box<Expression>),  // Unary minus: -expr
    Not(Box<Expression>),     // Logical NOT: NOT expr
    // Ternary
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>), // condition, then_expr, else_expr
}
