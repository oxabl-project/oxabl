//! `assigned-but-never-read` lint (LINT0006).
//!
//! Fires on a variable or parameter that is written and never read — a *dead
//! store*: something produced a value and nothing consumes it. Split out of
//! `unused-variable` (LINT0002), whose `read_count == 0` predicate collapsed two
//! findings of different value into one message. A variable never mentioned
//! again is a stray declaration; delete the line. A variable written and never
//! read means a computation went nowhere, and the interesting line is the
//! assignment rather than the `DEFINE` — often far away from it. LINT0002 now
//! narrows to "declared and never referenced at all", so one symbol yields
//! exactly one diagnostic.
//!
//! Skips: everything in [`super::unused_symbol_shared::is_skipped`], plus
//! table-shaped parameters (whose counts are meaningless). Notably that skip
//! list is what leaves the *callee*-written flavor — a variable written only
//! through a write-back `RUN` argument — to the separate opt-in advisory (#125)
//! instead of annexing it here at warning severity.
//!
//! Which write forms reach this rule is a deliberately audited list, because a
//! form arriving unexamined is a false positive waiting to happen. Reported:
//! plain assignment (including an array element), an `ASSIGN` pair,
//! `MESSAGE ... UPDATE`/`SET` targets, `RUN ... PERSISTENT`/`ASYNCHRONOUS SET`
//! handles, `BUFFER-COMPARE ... SAVE RESULT IN`, and `CREATE <widget> h`. A
//! stored value nobody consumes is a real finding however it was stored, so the
//! message speaks of the value rather than of a computation. Unreachable by
//! construction and therefore not special-cased: `DO i = 1 TO n` and
//! `INPUT-OUTPUT` arguments (credited `ReadWrite`, so they always carry a read);
//! `BUFFER-COPY` / `CREATE <buffer>` targets and field writes (Buffer, Table and
//! Field symbols are not candidates); and `SET` / `UPDATE` / `PROMPT-FOR` /
//! `GET-KEY-VALUE` / bare `ASSIGN <field>` / `obj:PROP =` / `INITIAL`, none of
//! which the resolve pass credits as a write at all.

use oxabl_ast::{AssignPair, Expression, ExpressionKind, Statement, StatementKind};
use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{AnalysisContext, Resolution, Semantic, SymbolId, SymbolKind};
use rustc_hash::{FxHashMap, FxHashSet};

use super::LINT0006;
use super::unused_symbol_shared::{
    declaration_span, display_name, is_candidate, is_skipped, is_table_like_param,
};

/// Entry point.
pub fn run(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    // Stage one — candidates straight off the symbol table: written, never
    // read, and not exempt. Cheap enough to do before any walking, and it
    // usually leaves nothing to walk for.
    let mut candidates: Vec<SymbolId> = Vec::new();
    for (sid, sym) in sem.symbols.iter() {
        if !is_candidate(sym) || sym.read_count > 0 || sym.write_count == 0 {
            continue;
        }
        if is_skipped(sid, sym, &sem.scope_tree, &sem.symbols) || is_table_like_param(sym) {
            continue;
        }
        candidates.push(sid);
    }
    if candidates.is_empty() {
        return Vec::new();
    }

    // Stage two — locate each candidate's *first* write site, so the diagnostic
    // lands on the assignment. One diagnostic per symbol, not per write.
    let mut v = Visitor {
        sem,
        file_id: ctx.file_id,
        wanted: candidates.iter().copied().collect(),
        first_write: FxHashMap::default(),
    };
    v.walk_block(program);
    let first_write = v.first_write;

    // Emit in symbol-table order, so output is stable regardless of the walk.
    candidates
        .iter()
        .map(|sid| {
            let sym = sem.symbols.get(*sid);
            let name = display_name(sym, ctx.source);
            let label = match sym.kind {
                SymbolKind::Variable => "variable",
                SymbolKind::Parameter => "parameter",
                _ => "symbol",
            };
            // No located write site — the write arrived through a form the walk
            // does not descend into (`MESSAGE ... UPDATE`, `CREATE <widget>`,
            // and friends). Fall back to the declaration rather than drop the
            // finding: less useful, still true. Widening the walk form-by-form
            // is the per-shape treadmill def-use records (#126) exist to end.
            let span = first_write
                .get(sid)
                .copied()
                .unwrap_or_else(|| declaration_span(ctx, sym));
            Diagnostic::warning(
                LINT0006,
                format!("value assigned to {label} `{name}` is never read"),
                span,
            )
        })
        .collect()
}

struct Visitor<'a> {
    sem: &'a Semantic,
    /// Spans stay in virtual space, exactly like every other rule's —
    /// `PreprocessedFile::resolve` maps them back to real files at the CLI and
    /// analyze boundary. So a dead store whose write lives in an expanded
    /// include reports against the include, which is where the assignment is.
    file_id: oxabl_common::FileId,
    wanted: FxHashSet<SymbolId>,
    first_write: FxHashMap<SymbolId, FileSpan>,
}

impl Visitor<'_> {
    fn walk_block(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            self.walk_statement(stmt);
        }
    }

    fn walk_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Assignment { target, .. } => self.record_target(target),
            StatementKind::Assign { assignments } => {
                for AssignPair { target, .. } in assignments {
                    self.record_target(target);
                }
            }

            // Recurse into bodies.
            StatementKind::Procedure { body, .. }
            | StatementKind::Function { body, .. }
            | StatementKind::Class { body, .. }
            | StatementKind::Interface { body, .. }
            | StatementKind::Destructor { body }
            | StatementKind::Finally { body } => self.walk_block(body),
            StatementKind::Method {
                parameters, body, ..
            }
            | StatementKind::Constructor {
                parameters, body, ..
            } => {
                self.walk_block(parameters);
                self.walk_block(body);
            }
            StatementKind::Catch { body, .. } => self.walk_block(body),
            StatementKind::Property {
                get_body, set_body, ..
            } => {
                if let Some(b) = get_body {
                    self.walk_block(b);
                }
                if let Some(b) = set_body {
                    self.walk_block(b);
                }
            }
            StatementKind::Do { body, .. } | StatementKind::Repeat { body, .. } => {
                self.walk_block(body)
            }
            StatementKind::ForEach { body, .. } => self.walk_block(body),
            StatementKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_statement(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_statement(eb);
                }
            }
            StatementKind::Case {
                when_branches,
                otherwise,
                ..
            } => {
                for wb in when_branches {
                    self.walk_block(&wb.body);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o);
                }
            }
            StatementKind::Block(body) => self.walk_block(body),
            StatementKind::Label { body, .. } => self.walk_statement(body),
            StatementKind::PreprocIf(pif) => {
                // Every branch is walked: a write in any of them is a real
                // write site, and the lint layer does not evaluate conditions.
                self.walk_block(&pif.then_branch);
                for (_, br) in &pif.elseif_branches {
                    self.walk_block(br);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb);
                }
            }
            _ => {}
        }
    }

    /// Record `target` as a candidate's first write site, if it resolves to one.
    fn record_target(&mut self, target: &Expression) {
        // `arr[i] = 1` writes `arr`; the resolve pass credits the array symbol,
        // so follow the same descent to reach it.
        let ident = match &target.kind {
            ExpressionKind::Identifier(ident) => ident,
            ExpressionKind::ArrayAccess { array, .. } => match &array.kind {
                ExpressionKind::Identifier(ident) => ident,
                _ => return,
            },
            // A field write credits the `Field` symbol, which is never a
            // candidate; nothing to record.
            _ => return,
        };
        let node = match &target.kind {
            ExpressionKind::ArrayAccess { array, .. } => array.id,
            _ => target.id,
        };
        let Some(Resolution::Resolved(sid)) = self.sem.references.get(node) else {
            return;
        };
        if !self.wanted.contains(sid) {
            return;
        }
        self.first_write.entry(*sid).or_insert(FileSpan {
            file: self.file_id,
            span: ident.span,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        CreateTarget, CreateTargetKind, DataType, Identifier, IntegerLiteral, Literal,
        ParameterDirection, ParameterType, RunArgument, RunTarget, Span, TypeSource,
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
    fn id_at(n: &str, start: u32) -> Identifier {
        Identifier {
            span: Span {
                start,
                end: start + n.len() as u32,
            },
            name: n.into(),
        }
    }
    fn stmt(k: StatementKind) -> Statement {
        Statement::new(k)
    }
    /// An identifier expression carrying a real `NodeId`. Required: the resolve
    /// pass keys its `references` side table by node id and silently drops
    /// `NodeId::DUMMY`, so an `Expression::new` target would never carry the
    /// resolution the write-site walk looks up — and the diagnostic would
    /// quietly fall back to the declaration span.
    fn ident_expr(n: &str, start: u32) -> Expression {
        use oxabl_ast::NodeId;
        use std::sync::atomic::{AtomicU32, Ordering};
        static COUNTER: AtomicU32 = AtomicU32::new(1);
        let nid = NodeId::from_u32(COUNTER.fetch_add(1, Ordering::Relaxed));
        Expression::with_id(
            nid,
            Span {
                start,
                end: start + n.len() as u32,
            },
            ExpressionKind::Identifier(id_at(n, start)),
        )
    }
    fn var_decl(n: &str) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }
    fn shared_var_decl(n: &str) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: true,
            is_new_global_shared: false,
        })
    }
    fn one(v: i64) -> Expression {
        Expression::new(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: v,
        })))
    }
    /// `n = 1.` with a distinctive target span, so a diagnostic reported at the
    /// write site is distinguishable from one reported at the declaration.
    fn assign(n: &str, target_start: u32) -> Statement {
        stmt(StatementKind::Assignment {
            target: ident_expr(n, target_start),
            value: one(1),
        })
    }
    fn lint(stmts: &[Statement]) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(stmts, &ctx);
        run(stmts, &sem, &ctx)
    }
    fn lint0002(stmts: &[Statement]) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(stmts, &ctx);
        super::super::unused_variable::run(stmts, &sem, &ctx)
    }

    #[test]
    fn fires_at_the_write_site_for_a_dead_store() {
        // The contract this rule exists for: a value computed into a variable
        // that nothing consumes, reported at the assignment rather than at the
        // far-away DEFINE. Paired with
        // `assigned_never_read_is_narrowed_away_to_lint0006` in
        // `unused_variable.rs` — together they are the division of the old
        // LINT0002 population, and they must not drift apart.
        let stmts = vec![var_decl("x"), assign("x", 40)];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert_eq!(diags[0].code.0, LINT0006);
        assert_eq!(
            diags[0].span.span.start, 40,
            "span must be the write site, not the declaration: {diags:?}"
        );
        assert!(diags[0].message.contains("never read"), "{diags:?}");
        assert!(diags[0].message.contains('x'), "{diags:?}");
    }

    #[test]
    fn silent_for_a_variable_never_mentioned_after_its_define() {
        // Discrimination (R7): that is a stray declaration, which stays
        // LINT0002's finding. Nothing was written, so there is no dead store.
        let stmts = vec![var_decl("x")];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
        assert_eq!(lint0002(&stmts).len(), 1, "LINT0002 should own this one");
    }

    #[test]
    fn silent_for_a_variable_that_is_assigned_then_read() {
        use oxabl_ast::Expression as E;
        let stmts = vec![
            var_decl("x"),
            assign("x", 40),
            stmt(StatementKind::ExpressionStatement(E::new(
                ExpressionKind::Identifier(id("x")),
            ))),
        ];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
        assert!(lint0002(&stmts).is_empty(), "{:?}", lint0002(&stmts));
    }

    #[test]
    fn reports_once_at_the_first_write_when_assigned_repeatedly() {
        let stmts = vec![var_decl("x"), assign("x", 40), assign("x", 80)];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "one diagnostic per symbol: {diags:?}");
        assert_eq!(diags[0].span.span.start, 40, "first write wins: {diags:?}");
    }

    #[test]
    fn fires_on_an_assign_statement_pair() {
        let stmts = vec![
            var_decl("x"),
            stmt(StatementKind::Assign {
                assignments: vec![AssignPair {
                    target: ident_expr("x", 55),
                    value: one(7),
                }]
                .into(),
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 55, "{diags:?}");
    }

    #[test]
    fn fires_on_an_array_element_assignment() {
        // `arr[i] = 1` credits the array symbol, so the walk descends to it.
        let arr = stmt(StatementKind::VariableDeclaration {
            name: id("arr"),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: false,
            extent: Some(4),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        });
        let stmts = vec![
            arr,
            stmt(StatementKind::Assignment {
                target: Expression::new(ExpressionKind::ArrayAccess {
                    array: Box::new(ident_expr("arr", 70)),
                    index: Box::new(one(1)),
                }),
                value: one(1),
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("arr"), "{diags:?}");
    }

    #[test]
    fn silent_for_an_output_parameter() {
        // Shared skip-list: writing is an OUTPUT parameter's whole contract.
        let stmts = vec![stmt(StatementKind::Procedure {
            name: id("p"),
            body: vec![
                stmt(StatementKind::DefineParameter {
                    direction: ParameterDirection::Output,
                    param_type: ParameterType::Variable {
                        name: id("out"),
                        type_source: TypeSource::Explicit(DataType::Integer),
                        no_undo: false,
                    },
                }),
                assign("out", 40),
            ],
        })];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    #[test]
    fn silent_for_a_shared_variable() {
        // Shared skip-list: a reader may live in another file.
        let stmts = vec![shared_var_decl("gShared"), assign("gShared", 40)];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    #[test]
    fn silent_when_the_only_write_is_a_write_back_run_argument() {
        // KTD5: those symbols are technically write-only too, but the
        // callee-written flavor is #125's opt-in INFO advisory. Without this
        // skip, this rule would annex that scope at warning severity.
        use oxabl_ast::NodeId;
        use std::sync::atomic::{AtomicU32, Ordering};
        static COUNTER: AtomicU32 = AtomicU32::new(9000);
        let nid = NodeId::from_u32(COUNTER.fetch_add(1, Ordering::Relaxed));
        let stmts = vec![
            var_decl("x"),
            stmt(StatementKind::Run {
                target: RunTarget::Literal("proc".into()),
                arguments: vec![RunArgument {
                    direction: ParameterDirection::Output,
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
        ];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    #[test]
    fn silent_for_a_table_shaped_parameter() {
        // KTD4: a table parameter's counts are meaningless, so it is never a
        // dead store. LINT0002 owns the warn-or-silent call for these.
        let stmts = vec![
            stmt(StatementKind::DefineTempTable {
                name: id("tt"),
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
            }),
            stmt(StatementKind::DefineParameter {
                direction: ParameterDirection::Input,
                param_type: ParameterType::Handle {
                    kind: oxabl_ast::HandleParamKind::Table,
                    name: id("tt"),
                    passing: Default::default(),
                },
            }),
        ];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    #[test]
    fn silent_for_a_do_loop_counter() {
        // Unreachable by construction: a loop index is credited `ReadWrite`, so
        // it always carries a read and never becomes a candidate. Pinned so a
        // change to that crediting shows up here rather than as a false
        // positive on idiomatic loop code.
        let stmts = vec![
            var_decl("i"),
            stmt(StatementKind::Do {
                loop_var: Some(id("i")),
                from: Some(one(1)),
                to: Some(one(10)),
                by: None,
                while_condition: None,
                transaction: false,
                body: vec![],
            }),
        ];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    // -- Audited write forms whose site the walk cannot locate (R6, R11). -----
    // Each is a real dead store, so it fires; the span falls back to the
    // declaration because the write is a statement-position identifier rather
    // than an assignment target.

    #[test]
    fn fires_with_declaration_span_for_message_update_target() {
        // Input collected from the user and then discarded.
        let stmts = vec![
            var_decl("answer"),
            stmt(StatementKind::Message {
                items: vec![],
                set_targets: vec![id_at("answer", 90)],
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(
            diags[0].span.span.start, 0,
            "no locatable write site — expect the declaration span: {diags:?}"
        );
        assert!(diags[0].message.contains("answer"), "{diags:?}");
    }

    #[test]
    fn fires_with_declaration_span_for_persistent_run_handle() {
        // A persistent procedure started and its handle dropped on the floor.
        for (persistent, asynchronous) in [(true, false), (false, true)] {
            let handle = Some(Expression::new(ExpressionKind::Identifier(id_at("h", 90))));
            let stmts = vec![
                var_decl("h"),
                stmt(StatementKind::Run {
                    target: RunTarget::Literal("proc".into()),
                    arguments: vec![],
                    in_handle: None,
                    persistent,
                    persistent_handle: if persistent { handle.clone() } else { None },
                    asynchronous,
                    async_handle: if asynchronous { handle } else { None },
                    event_procedure: None,
                    no_error: false,
                }),
            ];
            let diags = lint(&stmts);
            assert_eq!(diags.len(), 1, "persistent={persistent}: {diags:?}");
            assert_eq!(diags[0].span.span.start, 0, "{diags:?}");
        }
    }

    #[test]
    fn fires_with_declaration_span_for_buffer_compare_result() {
        let stmts = vec![
            var_decl("res"),
            stmt(StatementKind::BufferCompare {
                source: id("a"),
                target: id("b"),
                result_var: Some(id_at("res", 90)),
                no_error: false,
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 0, "{diags:?}");
    }

    #[test]
    fn fires_with_declaration_span_for_created_widget_handle() {
        let stmts = vec![
            var_decl("hWidget"),
            stmt(StatementKind::Create {
                target: CreateTarget::Handle {
                    kind: CreateTargetKind::Widget,
                    handle: id_at("hWidget", 90),
                    widget_pool: None,
                },
                no_error: false,
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 0, "{diags:?}");
    }
}
