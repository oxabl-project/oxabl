//! `block-var-used-outside` lint (LINT0005).
//!
//! ABL scopes a `DEFINE VARIABLE` to its enclosing routine, not to the
//! `DO`/`FOR`/`REPEAT`/`CATCH`/`FINALLY` block it textually sits in (see the
//! semantic layer's variable hoisting). That makes a definition-inside-a-block
//! legal, but it hides a hazard: the variable exists for the whole routine yet
//! is only *assigned* when the block runs. Read it from outside the block and,
//! if the block never executed, it still holds its default value (`0`, `""`,
//! `?`, `FALSE`, …) rather than anything the author computed.
//!
//! ```abl
//! IF cond THEN DO:
//!    DEFINE VARIABLE i AS INTEGER NO-UNDO.
//!    i = compute().
//! END.
//! DISPLAY i.   /* i is 0 here whenever `cond` was false */
//! ```
//!
//! The rule is deliberately advisory (default `info`) and conservative: it
//! fires only when the variable is **read** from outside its defining block
//! **and** never **written** outside that block. An assignment outside the
//! block means the author is managing the value across the scope on purpose —
//! exactly the "I know about ABL's scoping" case — so the rule stays silent.
//! This is a set-membership approximation, not flow analysis: it does not
//! reason about statement ordering or which paths actually reach the read.

use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{AnalysisContext, Semantic, Symbol, SymbolFlags, SymbolKind};

use super::LINT0005;

/// Entry point.
pub fn run(
    _program: &[oxabl_ast::Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for (_sid, sym) in sem.symbols.iter() {
        if !is_hazard(sym) {
            continue;
        }
        let name = display_name(sym, ctx.source);
        let span = FileSpan {
            file: ctx.file_id,
            span: oxabl_ast::Span {
                start: sym.name_span.start,
                end: sym.name_span.end,
            },
        };
        diags.push(Diagnostic::info(
            LINT0005,
            format!(
                "`{name}` is defined inside a block but read outside it and never assigned there; \
                 it holds its default value unless that block ran. Assign it before use, or move \
                 the DEFINE to the routine level."
            ),
            span,
        ));
    }
    diags
}

/// A block-hoisted variable read outside its defining block, with no
/// compensating assignment outside that block.
fn is_hazard(sym: &Symbol) -> bool {
    sym.kind == SymbolKind::Variable
        // READ_OUTSIDE_BLOCK is only ever set by the resolve pass for a
        // variable that was hoisted out of a block, so it doubles as the
        // "is block-hoisted" gate here.
        // Assigned somewhere, but only inside the block (no outside write):
        // that is precisely the conditional case where the read may see the
        // default value. A never-assigned variable holds its default
        // unconditionally (the `DEFINE` is not executable), so the "unless the
        // block ran" framing would be wrong — leave that to other rules.
        && sym.write_count > 0
        && sym.flags.contains(SymbolFlags::READ_OUTSIDE_BLOCK)
        && !sym.flags.contains(SymbolFlags::WRITE_OUTSIDE_BLOCK)
        // A SHARED variable takes its value from the sharing procedure, not
        // from the block's assignments, so the hazard reasoning does not apply.
        && !sym.flags.intersects(
            SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED,
        )
}

/// Display name = original casing sliced from source; falls back to the
/// case-folded atom when the span maps outside the buffer (synthetic tests)
/// or lands on a non-char boundary.
fn display_name(sym: &Symbol, source: &str) -> String {
    let start = sym.name_span.start as usize;
    let end = sym.name_span.end as usize;
    source
        .get(start..end)
        .map(str::to_string)
        .unwrap_or_else(|| sym.name.as_ref().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        DataType, Expression, ExpressionKind, Identifier, IntegerLiteral, Literal, Span, Statement,
        StatementKind, TypeSource,
    };
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use oxabl_semantic::analyze_file;

    fn id(n: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: n.len() as u32,
            },
            name: n.into(),
        }
    }
    fn stmt(k: StatementKind) -> Statement {
        Statement::new(k)
    }
    fn ident_expr(n: &str) -> Expression {
        Expression::new(ExpressionKind::Identifier(id(n)))
    }
    fn var_decl(n: &str) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: true,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }
    fn assign(n: &str) -> Statement {
        stmt(StatementKind::Assignment {
            target: ident_expr(n),
            value: Expression::new(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 1,
            }))),
        })
    }
    fn read(n: &str) -> Statement {
        stmt(StatementKind::ExpressionStatement(ident_expr(n)))
    }
    fn do_block(body: Vec<Statement>) -> Statement {
        stmt(StatementKind::Do {
            loop_var: None,
            from: None,
            to: None,
            by: None,
            while_condition: None,
            transaction: false,
            body,
        })
    }
    fn lint(stmts: Vec<Statement>) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    #[test]
    fn fires_on_block_defined_var_read_outside() {
        // DO: DEFINE VARIABLE i; i = 1. END.  then  read i (outside).
        let diags = lint(vec![do_block(vec![var_decl("i"), assign("i")]), read("i")]);
        assert_eq!(diags.len(), 1, "expected one LINT0005: {diags:?}");
        assert_eq!(diags[0].code.0, LINT0005);
        assert_eq!(diags[0].severity, oxabl_common::Severity::Info);
        assert!(diags[0].message.contains("`i`"));
    }

    #[test]
    fn silent_when_also_written_outside_the_block() {
        // An assignment outside the defining block means the author is
        // deliberately managing the value across the scope.
        let diags = lint(vec![
            do_block(vec![var_decl("i"), assign("i")]),
            assign("i"),
            read("i"),
        ]);
        assert!(
            diags.is_empty(),
            "outside write must silence the rule: {diags:?}"
        );
    }

    #[test]
    fn silent_when_only_used_inside_the_block() {
        let diags = lint(vec![do_block(vec![var_decl("i"), assign("i"), read("i")])]);
        assert!(diags.is_empty(), "in-block use must not fire: {diags:?}");
    }

    #[test]
    fn silent_for_routine_level_variable() {
        // A variable defined at routine level is never a block-hoist hazard,
        // regardless of how it is read.
        let diags = lint(vec![var_decl("i"), read("i")]);
        assert!(
            diags.is_empty(),
            "routine-level var must not fire: {diags:?}"
        );
    }

    #[test]
    fn silent_when_never_assigned() {
        // Defined in a block and read outside, but never assigned anywhere:
        // the value is its default unconditionally (the DEFINE is not
        // executable), so the "unless the block ran" hazard does not apply.
        let diags = lint(vec![do_block(vec![var_decl("i")]), read("i")]);
        assert!(
            diags.is_empty(),
            "never-assigned var must not fire: {diags:?}"
        );
    }
}
