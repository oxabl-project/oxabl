use crate::Literal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Add(Box<Expression>, Box<Expression>),
}