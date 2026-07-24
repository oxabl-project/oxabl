//! `unused-variable` lint (LINT0002).
//!
//! Fires on variables and parameters that are never *referenced at all* —
//! `read_count == 0` and `write_count == 0`. A symbol that is written and never
//! read is a dead store rather than a stray declaration, and belongs to
//! `assigned-but-never-read` (LINT0006), which reports it at the assignment
//! instead of at the `DEFINE`. The two rules divide one population, so a given
//! symbol yields exactly one diagnostic.
//!
//! Skip-list (shared with LINT0006 via
//! [`super::unused_symbol_shared`], captured as tests below):
//! - `OUTPUT` and `INPUT-OUTPUT` parameters (writing is the contract).
//! - Parameters in `INTERFACE` method declarations (interfaces have no bodies).
//! - Parameters in `ABSTRACT` methods (body never runs).
//! - `SHARED` / `NEW SHARED` / `NEW GLOBAL SHARED` variables (cross-file
//!   readers not visible in v1).
//! - Variables passed as a write-back (`OUTPUT` / `INPUT-OUTPUT` / `RETURN`)
//!   argument to a `RUN` — the callee writes into them, so they are used even
//!   when the call site never reads the result back.
//!
//! Redirect (not a skip):
//! - A `TABLE FOR` / `DATASET FOR` parameter (`SymbolFlags::PARAM_TABLE_LIKE`)
//!   names a temp-table or dataset, so references to the name land on that
//!   declaration and this symbol's own `read_count` is permanently zero. The
//!   rule asks the backing declaration whether it was read instead of
//!   exempting the parameter, which keeps the true positive: a table parameter
//!   whose table is genuinely never touched still warns.

use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{
    AnalysisContext, NamespaceId, ScopeTree, Semantic, Symbol, SymbolFlags, SymbolId, SymbolKind,
    SymbolTable,
};

use super::LINT0002;
use super::unused_symbol_shared::{display_name, is_candidate, is_skipped};

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
        // A table-shaped parameter's own `read_count` is meaningless — every
        // reference to the name resolves to the backing `DEFINE TEMP-TABLE`,
        // never here — so redirect the question rather than skip the symbol.
        // Skipping would discard the genuine finding this keeps (R3).
        let was_read = if sym.flags.contains(SymbolFlags::PARAM_TABLE_LIKE) {
            match backing_read_count(sid, sym, &sem.scope_tree, &sem.symbols) {
                // Backing declaration not visible — typically declared in an
                // include we could not resolve. An unprovable claim is not a
                // diagnostic, so stay silent instead of guessing.
                None => continue,
                Some(reads) => reads > 0,
            }
        } else {
            sym.read_count > 0
        };
        if was_read {
            continue;
        }
        // Written but never read is a *dead store*, not a stray declaration:
        // a different finding, wanting a different span. It belongs to
        // `assigned-but-never-read` (LINT0006), which reports at the
        // assignment. Narrowing here is what keeps one symbol to exactly one
        // diagnostic instead of two.
        if sym.write_count > 0 {
            continue;
        }
        // The remaining exemptions still apply to a table-shaped parameter, so
        // e.g. an `OUTPUT ... TABLE FOR` parameter stays silent exactly as it
        // does today; the redirect above replaces only the read-count test.
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

/// Total reads credited to the table a table-shaped parameter names.
///
/// `TABLE FOR tt` puts a `Parameter` in `NamespaceId::Values` while every
/// reference to `tt` resolves through `NamespaceId::Buffers` instead. But those
/// references do not all land on one symbol, so a single scope-walking lookup is
/// not enough:
///
/// - `tt.field` and `BUFFER-COPY tt TO x` credit the `DEFINE TEMP-TABLE` symbol,
///   which sits in an *ancestor* scope of the parameter (file scope, typically).
/// - `FOR EACH tt:` and `FIND FIRST tt` declare a fresh implicit buffer symbol
///   in the block scope they open and credit *that*, leaving the temp-table's own
///   `read_count` at zero. Those scopes are *descendants* of the parameter's, so
///   an upward-only walk cannot see them — and `FOR EACH` is the single most
///   idiomatic thing to do with a table parameter.
///
/// So gather every `Buffers` binding of the name that the routine could be
/// talking about — one in an ancestor-or-self scope (the declaration) or in a
/// descendant scope (an implicit buffer opened inside the routine) — and sum
/// their reads.
///
/// Returns `None` when no such binding exists at all, which the caller treats as
/// "stay silent": the backing table is not visible, typically because it is
/// declared in an include we could not resolve, and an unprovable claim is not a
/// diagnostic. `DATASET FOR` lands there too, since `DEFINE DATASET` declares
/// into `Values` rather than `Buffers`.
///
/// Collapses into a def-use query once CFG def-use records land (#126).
fn backing_read_count(
    sid: SymbolId,
    sym: &Symbol,
    tree: &ScopeTree,
    symbols: &SymbolTable,
) -> Option<u32> {
    let param_scope = sym.declared_in;
    let mut found = false;
    let mut reads: u32 = 0;
    for (other_id, other) in symbols.iter() {
        // Never answer with the parameter's own meaningless count.
        if other_id == sid || other.namespace != NamespaceId::Buffers || other.name != sym.name {
            continue;
        }
        let visible_to_routine = tree.ancestors(param_scope).any(|s| s == other.declared_in);
        let opened_inside_routine = tree.ancestors(other.declared_in).any(|s| s == param_scope);
        if !visible_to_routine && !opened_inside_routine {
            continue;
        }
        found = true;
        reads = reads.saturating_add(other.read_count);
    }
    found.then_some(reads)
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
    fn assigned_never_read_is_narrowed_away_to_lint0006() {
        // Deliberate contract change, not a regression: an assigned-never-read
        // variable used to warn here. It is a *dead store* — something computed
        // a value nothing consumes — and that finding belongs at the assignment,
        // so `assigned-but-never-read` (LINT0006) owns it now and this rule
        // narrows to "declared and never referenced at all". Paired with
        // `fires_at_the_write_site_for_a_dead_store` in
        // `assigned_but_never_read.rs`; one symbol must yield exactly one
        // diagnostic, never two.
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
        assert!(
            diags.is_empty(),
            "written-then-unread is LINT0006's finding, not LINT0002's: {diags:?}"
        );
    }

    #[test]
    fn no_diagnostic_for_variable_used_as_do_loop_counter() {
        // `DEF VAR i` then `DO i = 1 TO 10:` — the counter is a use of `i`,
        // so LINT0002 must not fire (regression for #83, where the counter was
        // shadowed and both the def and the `DO i =` site were flagged).
        use oxabl_ast::{Expression, ExpressionKind, IntegerLiteral, Literal};
        let int_lit = |v: i64, end: u32| {
            Expression::new(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end },
                value: v,
            })))
        };
        let stmts = vec![
            var_decl("i", DataType::Integer),
            stmt(StatementKind::Do {
                loop_var: Some(id("i")),
                from: Some(int_lit(1, 1)),
                to: Some(int_lit(10, 2)),
                by: None,
                while_condition: None,
                transaction: false,
                body: vec![],
            }),
        ];
        let diags = analyze_and_lint(stmts);
        assert!(
            diags.is_empty(),
            "DO-loop counter must not be flagged unused: {diags:?}"
        );
    }

    /// `DEFINE VARIABLE x` then `RUN proc (<dir> x).`
    ///
    /// The argument expression needs a real `NodeId`: the resolve pass keys
    /// its `references` side table by node id and silently drops
    /// `NodeId::DUMMY`, so a `Expression::new` argument would never carry the
    /// resolution `PASSED_AS_OUTPUT_ARG` is derived from.
    fn var_plus_run_arg(dir: ParameterDirection) -> Vec<Statement> {
        use oxabl_ast::{Expression, ExpressionKind, NodeId, RunArgument, RunTarget};
        use std::sync::atomic::{AtomicU32, Ordering};
        static COUNTER: AtomicU32 = AtomicU32::new(1);
        let nid = NodeId::from_u32(COUNTER.fetch_add(1, Ordering::Relaxed));
        vec![
            var_decl("x", DataType::Integer),
            stmt(StatementKind::Run {
                target: RunTarget::Literal("proc".into()),
                arguments: vec![RunArgument {
                    direction: dir,
                    expression: Expression::with_id(
                        nid,
                        Span { start: 0, end: 1 },
                        ExpressionKind::Identifier(id("x")),
                    ),
                }],
                in_handle: None,
                persistent: false,
                persistent_handle: None,
                asynchronous: false,
                async_handle: None,
                event_procedure: None,
                no_error: false,
            }),
        ]
    }

    #[test]
    fn no_diagnostic_for_variable_passed_as_output_argument() {
        // The fix: a callee's required out-param that this call site never
        // reads back is still a use of the variable.
        let diags = analyze_and_lint(var_plus_run_arg(ParameterDirection::Output));
        assert!(
            diags.is_empty(),
            "OUTPUT-argument-only variable must be skipped: {diags:?}"
        );
    }

    #[test]
    fn no_diagnostic_for_variable_passed_as_input_output_argument() {
        let diags = analyze_and_lint(var_plus_run_arg(ParameterDirection::InputOutput));
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }

    #[test]
    fn no_diagnostic_for_variable_passed_as_input_argument() {
        // Already spared via `read_count`; pinned so the INPUT path can't
        // regress into a false positive if the flag ever moves.
        let diags = analyze_and_lint(var_plus_run_arg(ParameterDirection::Input));
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }

    #[test]
    fn no_double_handling_when_output_argument_is_also_read() {
        // Belt-and-braces: two independent skip paths (`read_count > 0` and
        // the flag) apply to the same symbol. This can't isolate either one —
        // it only pins that their overlap still yields a single, empty result
        // rather than a double-emit.
        use oxabl_ast::{Expression, ExpressionKind};
        let mut stmts = var_plus_run_arg(ParameterDirection::Output);
        stmts.push(stmt(StatementKind::ExpressionStatement(Expression::new(
            ExpressionKind::Identifier(id("x")),
        ))));
        let diags = analyze_and_lint(stmts);
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }

    #[test]
    fn unused_variable_alongside_output_argument_still_warns() {
        // Discrimination: the skip is per-symbol, not per-file.
        let mut stmts = var_plus_run_arg(ParameterDirection::Output);
        stmts.insert(1, var_decl("spare", DataType::Integer));
        let diags = analyze_and_lint(stmts);
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert!(diags[0].message.contains("spare"), "{diags:?}");
    }

    // =======================================================================
    // Table- and dataset-shaped parameters (`PARAM_TABLE_LIKE` redirect)
    // =======================================================================

    fn temp_table(n: &str) -> Statement {
        stmt(StatementKind::DefineTempTable {
            name: id(n),
            no_undo: false,
            like_table: None,
            validate: false,
            use_indexes: vec![],
            fields: vec![],
            indexes: vec![],
            xml_options: Default::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    /// `DEFINE INPUT PARAMETER TABLE FOR n.` (or the other handle-ish forms).
    fn handle_param(n: &str, kind: oxabl_ast::HandleParamKind) -> Statement {
        stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Handle {
                kind,
                name: id(n),
                passing: Default::default(),
            },
        })
    }

    /// `FIND FIRST n.` — a read of the buffer/temp-table `n`.
    fn find_stmt(n: &str) -> Statement {
        stmt(StatementKind::Find {
            find_type: oxabl_ast::FindType::First,
            buffer: id(n),
            key_value: None,
            where_clause: None,
            lock_type: oxabl_ast::LockType::NoLock,
            no_error: false,
        })
    }

    #[test]
    fn no_diagnostic_for_table_parameter_whose_table_is_used() {
        // The false positive: `tt`'s reads land on the DEFINE TEMP-TABLE, so
        // the parameter's own read_count stays 0 forever.
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            handle_param("tt", oxabl_ast::HandleParamKind::Table),
            find_stmt("tt"),
        ]);
        assert!(
            diags.is_empty(),
            "used TABLE FOR parameter must not warn: {diags:?}"
        );
    }

    #[test]
    fn no_diagnostic_for_table_parameter_used_only_as_field_qualifier() {
        // Pins the mechanism the redirect leans on: `resolve_field_access`
        // credits the qualifier as a Read unconditionally. If that ever stops,
        // this rule starts false-positiving again and this test is the alarm.
        use oxabl_ast::{Expression, ExpressionKind};
        let fa = Expression::new(ExpressionKind::FieldAccess {
            qualifier: Box::new(Expression::new(ExpressionKind::Identifier(id("tt")))),
            field: id("f"),
        });
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            handle_param("tt", oxabl_ast::HandleParamKind::Table),
            stmt(StatementKind::ExpressionStatement(fa)),
        ]);
        assert!(
            diags.is_empty(),
            "field-qualifier-only reference must silence the parameter: {diags:?}"
        );
    }

    #[test]
    fn no_diagnostic_for_dataset_parameter() {
        // `DEFINE DATASET` declares into Values, not Buffers, so the lookup
        // misses and a dataset parameter takes the silent path.
        let ds = stmt(StatementKind::DefineDataset {
            name: id("ds"),
            access: None,
            is_static: false,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
            serializable: false,
            non_serializable: false,
            xml_options: Default::default(),
            reference_only: false,
            buffers: vec![id("tt")],
            data_relations: vec![],
            parent_id_relations: vec![],
        });
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            ds,
            handle_param("ds", oxabl_ast::HandleParamKind::Dataset),
        ]);
        assert!(
            diags.iter().all(|d| !d.message.contains("ds")),
            "DATASET FOR parameter must not warn: {diags:?}"
        );
    }

    #[test]
    fn fires_on_table_parameter_whose_table_is_never_referenced() {
        // The preserved true positive (R3): the redirect points the question at
        // the right symbol, it does not blanket-exempt table parameters.
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            handle_param("tt", oxabl_ast::HandleParamKind::Table),
        ]);
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert_eq!(diags[0].code.0, LINT0002);
        assert!(diags[0].message.contains("tt"), "{diags:?}");
    }

    #[test]
    fn no_diagnostic_when_backing_table_is_not_discoverable() {
        // No DEFINE TEMP-TABLE in sight — typically declared in an include we
        // could not resolve. Stay silent rather than guess, and do not panic.
        let diags = analyze_and_lint(vec![handle_param("tt", oxabl_ast::HandleParamKind::Table)]);
        assert!(
            diags.is_empty(),
            "undiscoverable backing table must stay silent: {diags:?}"
        );
    }

    #[test]
    fn fires_on_unused_table_handle_parameter() {
        // Discrimination: `TABLE-HANDLE` names a real handle value whose reads
        // land on the parameter, so it keeps its own read_count and still
        // warns. Proves the redirect did not widen into the handle forms.
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            handle_param("h", oxabl_ast::HandleParamKind::TableHandle),
            find_stmt("tt"),
        ]);
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert!(diags[0].message.contains('h'), "{diags:?}");
    }

    #[test]
    fn unused_variable_alongside_used_table_parameter_still_warns() {
        // Discrimination: the redirect is per-symbol, not per-file.
        let diags = analyze_and_lint(vec![
            temp_table("tt"),
            handle_param("tt", oxabl_ast::HandleParamKind::Table),
            find_stmt("tt"),
            var_decl("spare", DataType::Integer),
        ]);
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert!(diags[0].message.contains("spare"), "{diags:?}");
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
