//! Sequence operands of `NEXT-VALUE`, `CURRENT-VALUE` and their dynamic forms.

use oxabl_ast::{Expression, ExpressionKind, Identifier, Literal, Span};

/// The operand of a sequence function that names a database sequence.
#[derive(Debug, Clone, Copy)]
pub struct SequenceOperand<'e> {
    /// The expression carrying the operand's [`Resolution`](crate::Resolution).
    pub expr: &'e Expression,
    /// The sequence name as written, without any database qualifier.
    pub name: &'e str,
    /// Where `name` sits in the source.
    pub span: Span,
    /// `NEXT-VALUE` / `CURRENT-VALUE`: the operand is a bare name (or
    /// `db.seq`) and every argument is part of the sequence reference, the
    /// second being a logical database name. The dynamic forms take ordinary
    /// string expressions instead.
    pub is_static: bool,
}

/// The sequence operand of a call, if `name` is one of the four sequence
/// functions and the operand has the shape that names a sequence: a name or
/// `db.name` for the static pair, a string literal for the dynamic pair.
pub fn sequence_operand<'e>(
    name: &Identifier,
    arguments: &'e [Expression],
) -> Option<SequenceOperand<'e>> {
    let callee = name.name.as_str();
    let first = arguments.first()?;
    if callee.eq_ignore_ascii_case("next-value") || callee.eq_ignore_ascii_case("current-value") {
        let (name, span) = match &first.kind {
            ExpressionKind::Identifier(id) => (id.name.as_str(), id.span),
            ExpressionKind::FieldAccess { field, .. } => (field.name.as_str(), field.span),
            _ => return None,
        };
        return Some(SequenceOperand {
            expr: first,
            name,
            span,
            is_static: true,
        });
    }
    if callee.eq_ignore_ascii_case("dynamic-next-value")
        || callee.eq_ignore_ascii_case("dynamic-current-value")
    {
        let ExpressionKind::Literal(Literal::String(s)) = &first.kind else {
            return None;
        };
        return Some(SequenceOperand {
            expr: first,
            name: s.value.as_str(),
            span: s.span,
            is_static: false,
        });
    }
    None
}
