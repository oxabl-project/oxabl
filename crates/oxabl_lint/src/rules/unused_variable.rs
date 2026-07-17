//! `unused-variable` lint (LINT0002).
//!
//! Fires on variables and parameters whose `read_count` is zero.
//! Skip-list (captured as tests below):
//! - `OUTPUT` and `INPUT-OUTPUT` parameters (writing is the contract).
//! - Parameters in `INTERFACE` method declarations (interfaces have no bodies).
//! - Parameters in `ABSTRACT` methods (body never runs).
//! - `SHARED` / `NEW SHARED` / `NEW GLOBAL SHARED` variables (cross-file
//!   readers not visible in v1).

use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{
    AnalysisContext, ScopeId, ScopeKind, ScopeTree, Semantic, Symbol, SymbolFlags, SymbolId,
    SymbolKind, SymbolTable,
};

use super::LINT0002;

/// Entry point.
pub fn run(
    _program: &[oxabl_ast::Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for (sid, sym) in sem.symbols.iter() {
        if !is_candidate(sym) {
            continue;
        }
        if sym.read_count > 0 {
            continue;
        }
        if is_skipped(sid, sym, &sem.scope_tree, &sem.symbols) {
            continue;
        }
        // Display name = slice the original casing from the source; if the
        // span maps outside the buffer (e.g. synthetic tests) fall back to
        // the case-folded atom.
        let name = display_name(sym, ctx.source);
        let span = FileSpan {
            file: ctx.file_id,
            span: oxabl_ast::Span {
                start: sym.name_span.start,
                end: sym.name_span.end,
            },
        };
        let label = match sym.kind {
            SymbolKind::Variable => "variable",
            SymbolKind::Parameter => "parameter",
            _ => "symbol",
        };
        diags.push(Diagnostic::warning(
            LINT0002,
            format!("unused {label} `{name}`"),
            span,
        ));
    }
    diags
}

fn is_candidate(sym: &Symbol) -> bool {
    matches!(sym.kind, SymbolKind::Variable | SymbolKind::Parameter)
}

fn is_skipped(sid: SymbolId, sym: &Symbol, tree: &ScopeTree, symbols: &SymbolTable) -> bool {
    // OUTPUT / INPUT-OUTPUT parameters: writing is the contract.
    if sym.kind == SymbolKind::Parameter
        && sym
            .flags
            .intersects(SymbolFlags::PARAM_OUTPUT | SymbolFlags::PARAM_INPUT_OUT)
    {
        return true;
    }
    // SHARED variables — readers may live in other files.
    if sym
        .flags
        .intersects(SymbolFlags::SHARED | SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
    {
        return true;
    }
    // Parameters of an INTERFACE method or an ABSTRACT method never
    // execute a body; their read-count is meaningless.
    if sym.kind == SymbolKind::Parameter && in_skipped_method(sym.declared_in, tree, symbols) {
        return true;
    }
    // Don't self-warn on the rule's own books.
    let _ = sid;
    false
}

/// Whether the `Parameter` declared in `scope` lives inside a method scope
/// whose declaring method is ABSTRACT, or inside an INTERFACE body.
fn in_skipped_method(scope: ScopeId, tree: &ScopeTree, symbols: &SymbolTable) -> bool {
    let mut cur = Some(scope);
    while let Some(id) = cur {
        let s = tree.get(id);
        // Parameter declared inside an INTERFACE body — any method there
        // has no body; skip its parameters.
        if s.kind == ScopeKind::Interface {
            return true;
        }
        if s.kind == ScopeKind::Method {
            // Look up the Method symbol whose declaration NodeId matches
            // this scope's owner, and check its ABSTRACT flag.
            if let Some((_, msym)) = symbols.iter().find(|(_, sym)| {
                sym.kind == SymbolKind::Function && sym.declaration == s.owner_node
            }) && msym.flags.contains(SymbolFlags::ABSTRACT)
            {
                return true;
            }
        }
        cur = s.parent;
    }
    false
}

fn display_name(sym: &Symbol, source: &str) -> String {
    let start = sym.name_span.start as usize;
    let end = sym.name_span.end as usize;
    if end > start && end <= source.len() {
        source[start..end].to_string()
    } else {
        sym.name.as_ref().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        AccessModifier, DataType, Identifier, ParameterDirection, ParameterType, Span, Statement,
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
    fn var_decl(n: &str, ty: DataType) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }
    fn param(n: &str, dir: ParameterDirection) -> Statement {
        stmt(StatementKind::DefineParameter {
            direction: dir,
            param_type: ParameterType::Variable {
                name: id(n),
                type_source: TypeSource::Explicit(DataType::Integer),
                no_undo: false,
            },
        })
    }
    fn analyze_and_lint(stmts: Vec<Statement>) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    #[test]
    fn fires_on_unused_variable() {
        let diags = analyze_and_lint(vec![var_decl("x", DataType::Integer)]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0002);
        assert!(diags[0].message.contains("unused variable"));
    }

    #[test]
    fn does_not_fire_on_unused_shared_variable() {
        // A SHARED variable's readers may live in another file, so an unused
        // one within this file must not be flagged. Guards the within-file
        // SHARED flag → LINT0002 exemption.
        let shared = stmt(StatementKind::VariableDeclaration {
            name: id("gShared"),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: true,
            is_new_global_shared: false,
        });
        let diags = analyze_and_lint(vec![shared]);
        assert!(
            diags.is_empty(),
            "unused SHARED variable must be skipped: {diags:?}"
        );
    }

    #[test]
    fn does_not_fire_on_used_variable() {
        let u = oxabl_ast::Expression::new(oxabl_ast::ExpressionKind::Identifier(id("x")));
        let diags = analyze_and_lint(vec![
            var_decl("x", DataType::Integer),
            stmt(StatementKind::ExpressionStatement(u)),
        ]);
        assert!(diags.is_empty());
    }

    #[test]
    fn skip_list_output_parameter() {
        let stmts = vec![stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![param("out", ParameterDirection::Output)],
        })];
        let diags = analyze_and_lint(stmts);
        assert!(
            diags.is_empty(),
            "OUTPUT parameter must be skipped: {diags:?}"
        );
    }

    #[test]
    fn skip_list_input_output_parameter() {
        let stmts = vec![stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![param("io", ParameterDirection::InputOutput)],
        })];
        let diags = analyze_and_lint(stmts);
        assert!(diags.is_empty());
    }

    #[test]
    fn fires_on_unused_input_parameter() {
        // True positive: a read-free INPUT parameter is a genuine smell.
        let stmts = vec![stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![param("in_", ParameterDirection::Input)],
        })];
        let diags = analyze_and_lint(stmts);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("parameter"));
    }

    #[test]
    fn skip_list_interface_method_parameters() {
        let stmts = vec![stmt(StatementKind::Interface {
            name: id("I"),
            inherits: vec![],
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![param("p", ParameterDirection::Input)],
                body: vec![],
            })],
        })];
        let diags = analyze_and_lint(stmts);
        assert!(
            diags.is_empty(),
            "interface params must be skipped: {diags:?}"
        );
    }

    #[test]
    fn skip_list_abstract_method_parameters() {
        let stmts = vec![stmt(StatementKind::Class {
            name: id("A"),
            inherits: None,
            implements: vec![],
            is_abstract: true,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: true,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![param("p", ParameterDirection::Input)],
                body: vec![],
            })],
        })];
        let diags = analyze_and_lint(stmts);
        assert!(diags.is_empty());
    }

    #[test]
    fn skip_list_interface_method_parameter_is_not_duplicate_of_abstract() {
        // Interface and Abstract are independent skip-list entries; both
        // fixtures must stay green under the rule.
        let stmts_iface = vec![stmt(StatementKind::Interface {
            name: id("I"),
            inherits: vec![],
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![param("x", ParameterDirection::Input)],
                body: vec![],
            })],
        })];
        assert!(analyze_and_lint(stmts_iface).is_empty());
    }

    #[test]
    fn no_diagnostic_for_used_parameter_in_concrete_method() {
        use oxabl_ast::{Expression, ExpressionKind};
        let use_p = Expression::new(ExpressionKind::Identifier(id("p")));
        let stmts = vec![stmt(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![param("p", ParameterDirection::Input)],
                body: vec![stmt(StatementKind::ExpressionStatement(use_p))],
            })],
        })];
        assert!(analyze_and_lint(stmts).is_empty());
    }

    #[test]
    fn assign_counts_as_write_but_not_read_warns() {
        // Assigning without reading leaves read_count=0; that's still unused.
        use oxabl_ast::{Expression, ExpressionKind, IntegerLiteral, Literal};
        let stmts = vec![
            var_decl("x", DataType::Integer),
            stmt(StatementKind::Assignment {
                target: Expression::new(ExpressionKind::Identifier(id("x"))),
                value: Expression::new(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1,
                }))),
            }),
        ];
        let diags = analyze_and_lint(stmts);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn no_diagnostic_on_procedure_declaration() {
        // Procedure symbols aren't candidates (SymbolKind::Procedure, not
        // Variable/Parameter).
        let diags = analyze_and_lint(vec![stmt(StatementKind::Procedure {
            name: id("p"),
            body: vec![],
        })]);
        assert!(diags.is_empty());
    }

    #[test]
    fn synthetic_schema_symbols_not_reported() {
        // Schema-backed resolution synthesizes symbols the resolve pass
        // never counts as read: `Customer.Name` under a loaded schema mints
        // a default-buffer symbol and a `Field` symbol whose read_count
        // stays 0. Pin that the rule's Variable/Parameter kind filter keeps
        // both out — if the filter ever widens to Buffer/Field, this is the
        // regression that catches it.
        use oxabl_ast::{Expression, ExpressionKind};
        use oxabl_schema::test_support::customer_schema;

        let fa = Expression::new(ExpressionKind::FieldAccess {
            qualifier: Box::new(Expression::new(ExpressionKind::Identifier(id("Customer")))),
            field: id("Name"),
        });
        let stmts = vec![stmt(StatementKind::ExpressionStatement(fa))];
        let schema = customer_schema();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        let diags = run(&stmts, &sem, &ctx);
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }
}
