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
//! handles, `BUFFER-COMPARE ... SAVE RESULT IN`, and `CREATE <widget> h`. Each
//! reports at its own write site, including one inside an `ON` trigger body. A
//! stored value nobody consumes is a real finding however it was stored, so the
//! message speaks of the value rather than of a computation. Targets with no
//! `NodeId` (the `MESSAGE`, `BUFFER-COMPARE` and `CREATE` names) resolve by
//! spelling in the scope the walk is in. A candidate whose write the walk cannot
//! locate still reports, at its declaration. Unreachable by
//! construction and therefore not special-cased: `DO i = 1 TO n` and
//! `INPUT-OUTPUT` arguments (credited `ReadWrite`, so they always carry a read);
//! `BUFFER-COPY` / `CREATE <buffer>` targets and field writes (Buffer, Table and
//! Field symbols are not candidates); and `SET` / `UPDATE` / `PROMPT-FOR` /
//! `GET-KEY-VALUE` / bare `ASSIGN <field>` / `obj:PROP =` / `INITIAL`, none of
//! which the resolve pass credits as a write at all.
//!
//! # Unmodelled statements: what is credited, and what still is not
//!
//! Around thirty ABL statement forms are recognized by the parser and then
//! discarded — the dispatch site matches a leading keyword and skips the
//! statement's tokens wholesale (`PUT`, `EXPORT`, `UPDATE`, `SET`,
//! `PROMPT-FOR`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE`, `APPLY`,
//! `ENABLE` / `DISABLE`, `CLEAR`, `NEXT-PROMPT`, `WAIT-FOR`, `ACCUMULATE`,
//! embedded SQL, …). They credit no reads and no writes, which used to make this
//! rule report a variable whose only read is a `PUT`:
//!
//! ```abl
//! DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
//! v-total = 42.
//! PUT v-total.        /* a real read the model could not see */
//! ```
//!
//! The parser now emits `StatementKind::Skipped` for those forms, carrying the
//! identifier-shaped tokens it passed over. The resolve pass resolves them
//! best-effort against the enclosing scope — through a lookup that records
//! nothing, so an unresolvable name produces neither a diagnostic nor a
//! `references` entry — and marks each hit with
//! `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`. This rule declines to fire
//! for any symbol so marked, via
//! [`is_skipped`](super::unused_symbol_shared::is_skipped). The read and write
//! counts stay exact: the flag says "these counts are incomplete", it does not
//! fabricate an access.
//!
//! Two gaps remain, and one property of the fix is worth knowing:
//!
//! - **Skipped tails inside *modelled* statements.** Around a dozen sites parse
//!   a real statement and then discard a trailing region that can contain
//!   genuine reads — `DISPLAY … WITH FRAME f` options (which routinely hold
//!   `title "x" + table.field`), `DEFINE VARIABLE … VIEW-AS`, `RUN` trailing
//!   content, `ASSIGN FRAME f`, `FUNCTION … IN super`, and the stream-I/O option
//!   tails. Same root cause, but the statement node is already occupied, so the
//!   payload shape used here does not transfer. Tracked by #134.
//! - **Keyword collisions.** The harvest keeps unreserved option keywords
//!   (`VALUE`, `FORMAT`, `LABEL`, `FRAME`, `TITLE`, …) as candidate names,
//!   because ABL lexes a user's variable named `value` as `Kind::Value` and the
//!   parser cannot distinguish the two inside a statement whose grammar it does
//!   not model. A variable named after an option keyword used by a skipped
//!   statement may therefore be exempted when it should not be. Narrowing the
//!   filter would reintroduce the false-positive class above for every variable
//!   named after an unreserved keyword.
//! - **The suppression is coarse.** It is per-symbol and file-wide: one mention
//!   in a skipped statement silences this rule for that variable everywhere in
//!   the file, so a genuine dead store on line 40 can go unreported because of
//!   an `ENABLE` mention on line 900. That is the real cost of a lexical
//!   fallback, and it is why #136 — head-parsing the forms so they credit
//!   exactly — is scheduled rather than hoped for.

use oxabl_ast::{
    AssignPair, CreateTarget, Expression, ExpressionKind, Identifier, NodeId, OnAction, OnKind,
    Statement, StatementKind,
};
use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{
    AnalysisContext, NamespaceId, Resolution, ScopeId, ScopeKind, Semantic, SymbolId, SymbolKind,
};
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
        scope: ScopeId::ROOT,
        opened: opened_scopes(sem),
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
            // No located write site: the write sits in a form the walk does not
            // reach. Fall back to the declaration rather than drop the finding:
            // less useful, still true.
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
    /// Scope of the statement being walked, for names that carry no `NodeId`.
    scope: ScopeId,
    opened: FxHashMap<NodeId, Vec<(ScopeId, ScopeKind)>>,
}

/// Scopes by the statement that opened them.
fn opened_scopes(sem: &Semantic) -> FxHashMap<NodeId, Vec<(ScopeId, ScopeKind)>> {
    let mut opened: FxHashMap<NodeId, Vec<(ScopeId, ScopeKind)>> = FxHashMap::default();
    for (id, scope) in sem.scope_tree.iter() {
        if scope.parent.is_some() {
            opened
                .entry(scope.owner_node)
                .or_default()
                .push((id, scope.kind));
        }
    }
    opened
}

impl Visitor<'_> {
    fn walk_block(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            self.walk_statement(stmt);
        }
    }

    /// Walk `f` inside the scope of `kind` that `owner` opened, when it opened one.
    fn within(&mut self, owner: NodeId, kind: ScopeKind, f: impl FnOnce(&mut Self)) {
        let parent = self.scope;
        let child = self.opened.get(&owner).and_then(|scopes| {
            scopes
                .iter()
                .find(|(id, k)| *k == kind && self.sem.scope_tree.get(*id).parent == Some(parent))
        });
        if let Some(&(id, _)) = child {
            self.scope = id;
        }
        f(self);
        self.scope = parent;
    }

    fn walk_statement(&mut self, stmt: &Statement) {
        let owner = stmt.id;
        match &stmt.kind {
            StatementKind::Assignment { target, .. } => self.record_target(target),
            StatementKind::Assign { assignments } => {
                for AssignPair { target, .. } in assignments {
                    self.record_target(target);
                }
            }
            StatementKind::Message { set_targets, .. } => {
                for target in set_targets {
                    self.record_ident(target);
                }
            }
            StatementKind::Run {
                persistent_handle,
                async_handle,
                ..
            } => {
                for handle in [persistent_handle, async_handle].into_iter().flatten() {
                    self.record_target(handle);
                }
            }
            StatementKind::BufferCompare {
                result_var: Some(result),
                ..
            } => self.record_ident(result),
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        handle, triggers, ..
                    },
                ..
            } => {
                self.record_target(handle);
                self.within(owner, ScopeKind::Trigger, |v| {
                    for t in triggers {
                        if let OnAction::Block(body) = &t.action {
                            v.walk_statement(body);
                        }
                    }
                });
            }

            // Recurse into bodies.
            StatementKind::Procedure { body, .. } => {
                self.within(owner, ScopeKind::Procedure, |v| v.walk_block(body))
            }
            StatementKind::Function { body, .. } => {
                self.within(owner, ScopeKind::Function, |v| v.walk_block(body))
            }
            StatementKind::Class { body, .. } => {
                self.within(owner, ScopeKind::Class, |v| v.walk_block(body))
            }
            StatementKind::Interface { body, .. } => {
                self.within(owner, ScopeKind::Interface, |v| v.walk_block(body))
            }
            StatementKind::Destructor { body } => {
                self.within(owner, ScopeKind::Destructor, |v| v.walk_block(body))
            }
            StatementKind::Finally { body } => {
                self.within(owner, ScopeKind::Finally, |v| v.walk_block(body))
            }
            StatementKind::Method {
                parameters, body, ..
            } => self.within(owner, ScopeKind::Method, |v| {
                v.walk_block(parameters);
                v.walk_block(body);
            }),
            StatementKind::Constructor {
                parameters, body, ..
            } => self.within(owner, ScopeKind::Constructor, |v| {
                v.walk_block(parameters);
                v.walk_block(body);
            }),
            StatementKind::Catch { body, .. } => {
                self.within(owner, ScopeKind::Catch, |v| v.walk_block(body))
            }
            StatementKind::Property {
                get_body, set_body, ..
            } => {
                if let Some(b) = get_body {
                    self.within(owner, ScopeKind::PropertyGet, |v| v.walk_block(b));
                }
                if let Some(b) = set_body {
                    self.within(owner, ScopeKind::PropertySet, |v| v.walk_block(b));
                }
            }
            StatementKind::Do { body, .. }
            | StatementKind::Repeat { body, .. }
            | StatementKind::ForEach { body, .. } => {
                self.within(owner, ScopeKind::Block, |v| v.walk_block(body))
            }
            StatementKind::On {
                kind:
                    OnKind::UiEvent {
                        action: OnAction::Block(body),
                        ..
                    }
                    | OnKind::DbEvent {
                        action: OnAction::Block(body),
                        ..
                    },
            } => self.within(owner, ScopeKind::Trigger, |v| v.walk_statement(body)),
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
            StatementKind::Block(body) | StatementKind::VariableDeclarationList(body) => {
                self.walk_block(body)
            }
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

    /// Record a statement-position name as a write site. Such a name has no
    /// `NodeId`, so it resolves by spelling in the walk's current scope.
    fn record_ident(&mut self, ident: &Identifier) {
        let Some(sid) =
            self.sem
                .scope_tree
                .resolve_name(self.scope, NamespaceId::Values, &ident.name)
        else {
            return;
        };
        self.note(sid, ident);
    }

    fn note(&mut self, sid: SymbolId, ident: &Identifier) {
        if self.wanted.contains(&sid) {
            self.first_write.entry(sid).or_insert(FileSpan {
                file: self.file_id,
                span: ident.span,
            });
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
        self.note(*sid, ident);
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
        // Assert the span too: without this the test passes on the
        // declaration-span fallback, so a regression that stopped the walk
        // descending into `ArrayAccess` would go unnoticed.
        assert_eq!(diags[0].span.span.start, 70, "{diags:?}");
    }

    #[test]
    fn silent_for_an_output_parameter() {
        // Shared skip-list: writing is an OUTPUT parameter's whole contract.
        let stmts = vec![stmt(StatementKind::Procedure {
            name: id("p"),
            is_prototype: false,
            is_external: false,
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
                target: RunTarget::literal("proc"),
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
                on_actions: Vec::new(),
                body: vec![],
            }),
        ];
        assert!(lint(&stmts).is_empty(), "{:?}", lint(&stmts));
    }

    // -- Audited write forms: each reports at its own write site. -------------

    #[test]
    fn fires_at_the_message_update_target() {
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
        assert_eq!(diags[0].span.span.start, 90, "{diags:?}");
        assert!(diags[0].message.contains("answer"), "{diags:?}");
    }

    #[test]
    fn fires_at_the_persistent_or_async_run_handle() {
        // A procedure started and its handle dropped on the floor.
        for (persistent, asynchronous) in [(true, false), (false, true)] {
            let handle = Some(ident_expr("h", 90));
            let stmts = vec![
                var_decl("h"),
                stmt(StatementKind::Run {
                    target: RunTarget::literal("proc"),
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
            assert_eq!(diags[0].span.span.start, 90, "{diags:?}");
        }
    }

    #[test]
    fn fires_at_the_buffer_compare_result() {
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
        assert_eq!(diags[0].span.span.start, 90, "{diags:?}");
    }

    #[test]
    fn fires_at_the_created_widget_handle() {
        let stmts = vec![
            var_decl("hWidget"),
            stmt(StatementKind::Create {
                target: CreateTarget::Handle {
                    kind: CreateTargetKind::Widget,
                    handle: ident_expr("hWidget", 90),
                    widget_pool: None,
                    class: None,
                    assignments: Vec::new(),
                    triggers: Vec::new(),
                    connect: None,
                    for_table: None,
                },
                no_error: false,
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 90, "{diags:?}");
    }

    #[test]
    fn fires_at_a_write_inside_an_on_trigger_body() {
        use oxabl_ast::{OnAction, OnKind};
        let stmts = vec![
            var_decl("x"),
            stmt(StatementKind::On {
                kind: OnKind::UiEvent {
                    clauses: vec![],
                    anywhere: true,
                    action: OnAction::Block(Box::new(assign("x", 60))),
                },
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 60, "{diags:?}");
    }

    #[test]
    fn fires_at_a_write_inside_a_widget_create_trigger() {
        use oxabl_ast::{OnAction, WidgetTrigger};
        let stmts = vec![
            var_decl("hWidget"),
            var_decl("x"),
            stmt(StatementKind::Create {
                target: CreateTarget::Handle {
                    kind: CreateTargetKind::Widget,
                    handle: ident_expr("hWidget", 90),
                    widget_pool: None,
                    class: None,
                    assignments: Vec::new(),
                    triggers: vec![WidgetTrigger {
                        events: Vec::new(),
                        action: OnAction::Block(Box::new(assign("x", 60))),
                    }],
                    connect: None,
                    for_table: None,
                },
                no_error: false,
            }),
        ];
        let diags = lint(&stmts);
        let at: Vec<_> = diags.iter().map(|d| d.span.span.start).collect();
        assert!(at.contains(&60), "{diags:?}");
        assert!(!at.contains(&0), "{diags:?}");
    }

    #[test]
    fn falls_back_to_the_declaration_when_the_write_site_is_not_located() {
        // The write is credited (a `Skipped` mention never is one, but an
        // `Expression::new` target carries no `NodeId`), yet the walk cannot find
        // it. The finding must still be emitted, at the declaration.
        let stmts = vec![
            var_decl("x"),
            stmt(StatementKind::Assignment {
                target: Expression::new(ExpressionKind::Identifier(id_at("x", 90))),
                value: one(1),
            }),
        ];
        let diags = lint(&stmts);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].span.span.start, 0, "{diags:?}");
    }
}
