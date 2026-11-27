//! ABL annotations: `@Name.` and `@Name(attr="value", ...).`
//!
//! The compiler records annotations and otherwise ignores them, so the parser
//! models each one as a standalone [`StatementKind::Annotation`] statement.
//! [`associate_annotations`] pairs each run of annotations with the statement
//! that follows it.

use crate::{Identifier, NamePool, Statement, StatementKind};

/// One annotation statement.
///
/// ```abl
/// @Test (expected="Progress.Lang.AppError").
/// @strict.effects(reads="Customer", writes="Order").
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    /// Dotted name without the leading `@`, as written (`strict.effects`).
    pub name: Identifier,
    /// A single bare word between the name and the attribute list, as in
    /// `@openapi.openedge.export FILE(...)`.
    pub qualifier: Option<Identifier>,
    /// Attributes in source order. Empty for `@Name.` and `@Name().`.
    pub attributes: Vec<AnnotationAttribute>,
}

impl Annotation {
    /// First attribute named `name`, compared ASCII case-insensitively.
    pub fn attribute(&self, names: &NamePool, name: &str) -> Option<&AnnotationAttribute> {
        self.attributes
            .iter()
            .find(|a| names.get(a.name.name).eq_ignore_ascii_case(name))
    }
}

/// A `name = value` pair inside an annotation's parentheses.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationAttribute {
    pub name: Identifier,
    pub value: AnnotationValue,
}

/// The value of an [`AnnotationAttribute`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationValue {
    /// A quoted string, without its quotes. Tilde escapes are left as written.
    String(String),
    /// An unquoted word or number, verbatim. Kept rather than rejected so an
    /// unusual annotation is never a parse error; consumers should treat it as
    /// unverified.
    Bare(String),
}

impl AnnotationValue {
    /// The text of the value, whichever way it was written.
    pub fn as_str(&self) -> &str {
        match self {
            AnnotationValue::String(s) | AnnotationValue::Bare(s) => s,
        }
    }
}

/// A run of consecutive annotations and the statement they precede.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotated<'a> {
    /// The annotation statements in source order. Never empty.
    pub annotations: Vec<&'a Annotation>,
    /// The first non-annotation statement after the run, or `None` when the
    /// run ends its statement list (a dangling annotation).
    pub target: Option<&'a Statement>,
}

/// Pair each run of consecutive annotations in `statements` with the statement
/// that follows it.
///
/// Works on one statement list; it does not descend into bodies, so callers
/// apply it to each body they visit. Empty statements between an annotation
/// and its target are skipped. Statements without annotations are not
/// reported.
pub fn associate_annotations(statements: &[Statement]) -> Vec<Annotated<'_>> {
    let mut out = Vec::new();
    let mut pending: Vec<&Annotation> = Vec::new();
    for stmt in statements {
        match &stmt.kind {
            StatementKind::Annotation(a) => pending.push(a),
            StatementKind::Empty if !pending.is_empty() => {}
            _ if pending.is_empty() => {}
            _ => out.push(Annotated {
                annotations: std::mem::take(&mut pending),
                target: Some(stmt),
            }),
        }
    }
    if !pending.is_empty() {
        out.push(Annotated {
            annotations: pending,
            target: None,
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;

    fn ident(names: &mut NamePool, name: &str) -> Identifier {
        Identifier {
            span: Span::DUMMY,
            name: names.push(name),
        }
    }

    fn annotation(names: &mut NamePool, name: &str) -> Statement {
        Statement::new(StatementKind::Annotation(Annotation {
            name: ident(names, name),
            qualifier: None,
            attributes: Vec::new(),
        }))
    }

    fn proc(names: &mut NamePool, name: &str) -> Statement {
        Statement::new(StatementKind::Procedure {
            name: ident(names, name),
            body: Vec::new(),
            is_prototype: false,
            is_external: false,
        })
    }

    #[test]
    fn pairs_a_run_with_the_next_statement() {
        let mut names = NamePool::new();
        let stmts = [
            annotation(&mut names, "A"),
            annotation(&mut names, "B"),
            proc(&mut names, "p"),
            proc(&mut names, "q"),
            annotation(&mut names, "C"),
            proc(&mut names, "r"),
        ];
        let pairs = associate_annotations(&stmts);
        assert_eq!(pairs.len(), 2);
        assert_eq!(pairs[0].annotations.len(), 2);
        assert_eq!(names.get(pairs[0].annotations[1].name.name), "B");
        assert_eq!(pairs[0].target, Some(&stmts[2]));
        assert_eq!(pairs[1].annotations.len(), 1);
        assert_eq!(pairs[1].target, Some(&stmts[5]));
    }

    #[test]
    fn skips_empty_statements_between_annotation_and_target() {
        let mut names = NamePool::new();
        let stmts = [
            annotation(&mut names, "A"),
            Statement::new(StatementKind::Empty),
            proc(&mut names, "p"),
        ];
        let pairs = associate_annotations(&stmts);
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].target, Some(&stmts[2]));
    }

    #[test]
    fn a_trailing_run_has_no_target() {
        let mut names = NamePool::new();
        let stmts = [proc(&mut names, "p"), annotation(&mut names, "A")];
        let pairs = associate_annotations(&stmts);
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].target, None);
    }

    #[test]
    fn no_annotations_no_pairs() {
        let mut names = NamePool::new();
        assert!(associate_annotations(&[proc(&mut names, "p")]).is_empty());
        assert!(associate_annotations(&[]).is_empty());
    }

    #[test]
    fn attribute_lookup_ignores_case() {
        let mut names = NamePool::new();
        let a = Annotation {
            name: ident(&mut names, "Test"),
            qualifier: None,
            attributes: vec![AnnotationAttribute {
                name: ident(&mut names, "Expected"),
                value: AnnotationValue::String("X".into()),
            }],
        };
        assert_eq!(a.attribute(&names, "expected").unwrap().value.as_str(), "X");
        assert!(a.attribute(&names, "other").is_none());
    }
}
