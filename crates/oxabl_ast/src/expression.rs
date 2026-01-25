use crate::Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Add(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}
