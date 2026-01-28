use crate::Expression;

/// A statement in ABL - an executable unit that performs an action.
/// All statements are terminated by a period.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Assignment
    Assignment {
        target: Expression,
        value: Expression,
    },

    /// Expression Statement
    ExpressionStatement(Expression),

    /// Code block
    Block(Vec<Statement>),

    /// Empty (just a period)
    Empty,
}
