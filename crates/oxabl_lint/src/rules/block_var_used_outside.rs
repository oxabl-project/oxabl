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
//!
//! **Paths.** A variable that passes those gates is then checked on the
//! control-flow graph (`oxabl_flow`): when the variable is definitely assigned
//! on every path that reaches every outside read, the block always ran first
//! and the rule stays silent. That clears the guard-clause case, where the
//! branch that skips the block leaves the routine:
//!
//! ```abl
//! IF NOT valid THEN DO:
//!    RETURN.
//! END.
//! ELSE DO:
//!    DEFINE VARIABLE v AS DECIMAL NO-UNDO.
//!    v = compute().
//! END.
//! DISPLAY v.   /* silent: only the ELSE path gets here */
//! ```
//!
//! Only the unit that holds the defining block is built (`oxabl_flow::UnitBuilder`),
//! and a read in another unit gives up before any build. One solve answers for
//! every hoisted variable of a block, a bit for each.
//!
//! The check can only remove findings. It gives up, and the finding stays,
//! when it cannot account for every reference to the variable, when a read
//! is in another unit (an internal procedure or trigger can run at any time),
//! or when an undo (an error handler or an `UNDO` statement) of a block that
//! overlaps the defining block could restore an undoable variable. An
//! assignment that raises an error does not count on the error path.
//!
//! **What is credited, and what still is not.** Statement forms the parser
//! recognizes but does not model (`PUT`, `EXPORT`, `UPDATE`, `SET`,
//! `PROMPT-FOR`, `GET-KEY-VALUE`, `ENABLE`, embedded SQL, …) credit no reads and
//! no writes, so an assignment arriving through one of them used to leave a
//! variable looking block-written-only. The parser now records those statements
//! as `StatementKind::Skipped` carrying the identifiers it passed over, the
//! resolve pass best-effort-resolves them, and this rule declines to fire for
//! any symbol so marked — through the shared
//! [`is_skipped`](super::unused_symbol_shared::is_skipped) predicate, the same
//! one LINT0002 and LINT0006 use.
//!
//! Two gaps remain, and both are quiet rather than loud — they cost findings,
//! not correctness:
//!
//! - **Skipped tails inside *modelled* statements.** Around a dozen sites parse
//!   a real statement and then discard a trailing region that can contain
//!   genuine reads (`DISPLAY … WITH FRAME f (title "x" + v-name)`, `DEFINE
//!   VARIABLE … VIEW-AS`, `RUN` trailing content). The statement node is already
//!   occupied, so the harvest payload used here does not transfer. Tracked by
//!   #134.
//! - **Keyword collisions.** The harvest keeps unreserved option keywords as
//!   candidate names, because ABL lexes a user's variable named `value` as
//!   `Kind::Value` and the parser cannot tell them apart inside a statement
//!   whose grammar it does not model. A variable named after an option keyword
//!   used by a skipped statement may therefore be exempted when it should not
//!   be.
//!
//! The suppression is also coarse: it is per-symbol and file-wide, so one
//! mention in a skipped statement silences this rule for that variable
//! everywhere. #136 drains that by head-parsing the forms.

use oxabl_common::Diagnostic;
use oxabl_semantic::{
    AnalysisContext, ScopeTree, Semantic, Symbol, SymbolFlags, SymbolId, SymbolKind, SymbolTable,
};

use super::LINT0005;
use super::unused_symbol_shared::{declaration_span, is_skipped};

/// Entry point.
pub fn run(
    program: &[oxabl_ast::Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let hazards: Vec<(SymbolId, &Symbol)> = sem
        .symbols
        .iter()
        .filter(|(sid, sym)| is_hazard(*sid, sym, &sem.scope_tree, &sem.symbols))
        .collect();
    if hazards.is_empty() {
        return Vec::new();
    }
    let paths = paths::Paths::new(program, ctx.source);
    let cleared = paths.assigned_before_every_outside_read(&hazards, sem);
    hazards
        .iter()
        .zip(&cleared)
        .filter(|(_, cleared)| !**cleared)
        .map(|((_, sym), _)| {
            let name = display_name(sym, ctx.source);
            let span = declaration_span(ctx, sym);
            Diagnostic::info(
                LINT0005,
                format!(
                    "`{name}` is defined inside a block but read outside it and never assigned \
                     there; it holds its default value unless that block ran. Assign it before \
                     use, or move the DEFINE to the routine level."
                ),
                span,
            )
        })
        .collect()
}

/// A block-hoisted variable read outside its defining block, with no
/// compensating assignment outside that block.
///
/// Delegates its exemptions to [`is_skipped`], rather than restating them: this
/// rule gates on `write_count` and the outside-block flags, so every fact that
/// makes those counts untrustworthy for LINT0002 and LINT0006 makes them
/// untrustworthy here too. A parallel clause would be one more copy to keep in
/// step, which is the failure mode `unused_symbol_shared` exists to prevent.
fn is_hazard(sid: SymbolId, sym: &Symbol, tree: &ScopeTree, symbols: &SymbolTable) -> bool {
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
        // Shared exemptions. Covers the SHARED family (a SHARED variable takes
        // its value from the sharing procedure, not from the block's
        // assignments, so the hazard reasoning does not apply), write-back RUN
        // arguments, and symbols touched by an unmodelled statement.
        && !is_skipped(sid, sym, tree, symbols)
}

/// Display name = original casing sliced from source; falls back to the
/// case-folded atom when the span maps outside the buffer (synthetic tests)
/// or lands on a non-char boundary.
/// Deliberately not the shared `unused_symbol_shared::display_name`: that one
/// falls back to the case-folded atom for a zero-length span, where this one
/// yields the empty slice. Consolidating them would change behavior here.
fn display_name(sym: &Symbol, source: &str) -> String {
    let start = sym.name_span.start as usize;
    let end = sym.name_span.end as usize;
    source
        .get(start..end)
        .map(str::to_string)
        .unwrap_or_else(|| sym.name.as_ref().to_string())
}

mod paths {
    //! Definite assignment of block-hoisted variables over the CFG.
    //!
    //! One solve per defining block covers every hoisted variable of that
    //! block (a bit for each), and only the unit that holds the block is built.

    use oxabl_ast::{
        CreateTarget, Expression, ExpressionKind, NodeId, OnAction, OnKind, RunTarget, Statement,
        StatementKind as SK, StreamOperation, SubscribeTarget,
    };
    use oxabl_flow::dataflow::{Analysis, Direction, solve};
    use oxabl_flow::{
        BlockId, Cfg, Edge, EdgeKind, FlowOptions, NodeIx, NodeKind, UnitBuilder, UnitId,
    };
    use oxabl_semantic::{Resolution, Semantic, Symbol, SymbolId};
    use rustc_hash::{FxHashMap, FxHashSet};

    /// The variables of one block that one solve answers for.
    const CHUNK: usize = u64::BITS as usize;

    /// A reference to a symbol: the statement whose node evaluates it, and
    /// whether it is an assignment target.
    #[derive(Clone, Copy)]
    struct Use {
        stmt: NodeId,
        write: bool,
    }

    /// What the walk of the program learned.
    pub(super) struct Paths<'a> {
        units: UnitBuilder<'a>,
        uses: FxHashMap<NodeId, Use>,
        no_undo: FxHashSet<NodeId>,
        /// `name = <literal>.` statements. Assigning a literal to a variable
        /// cannot fail, though the CFG (which cannot tell a variable from a
        /// field) gives every assignment an error edge.
        literal_writes: FxHashSet<NodeId>,
    }

    /// A candidate whose references the walk placed.
    struct Candidate {
        /// Index into the caller's list.
        at: usize,
        undoable: bool,
        refs: Vec<NodeId>,
    }

    impl<'a> Paths<'a> {
        pub(super) fn new(program: &'a [Statement], source: &'a str) -> Self {
            let mut paths = Paths {
                units: UnitBuilder::new(program, source, &FlowOptions::default()),
                uses: FxHashMap::default(),
                no_undo: FxHashSet::default(),
                literal_writes: FxHashSet::default(),
            };
            paths.walk(program);
            paths
        }

        /// For each of `hazards`, whether it is definitely assigned before
        /// every read outside the block it is defined in. `false` whenever
        /// that cannot be shown.
        pub(super) fn assigned_before_every_outside_read(
            &self,
            hazards: &[(SymbolId, &Symbol)],
            sem: &Semantic,
        ) -> Vec<bool> {
            let mut cleared = vec![false; hazards.len()];

            // One pass over the references for every hazard.
            let by_symbol: FxHashMap<SymbolId, usize> = hazards
                .iter()
                .enumerate()
                .map(|(at, (sid, _))| (*sid, at))
                .collect();
            let mut refs: Vec<Vec<NodeId>> = vec![Vec::new(); hazards.len()];
            for (id, res) in sem.references.iter() {
                if let Resolution::Resolved(s) = res
                    && let Some(&at) = by_symbol.get(s)
                {
                    refs[at].push(id);
                }
            }

            // Group by defining block, in first-seen order.
            let mut blocks: Vec<(NodeId, Vec<Candidate>)> = Vec::new();
            for (at, (sid, sym)) in hazards.iter().enumerate() {
                let Some(scope) = sem.symbols.block_defined_scope(*sid) else {
                    continue;
                };
                let owner = sem.scope_tree.get(scope).owner_node;
                let candidate = Candidate {
                    at,
                    undoable: !self.no_undo.contains(&sym.declaration),
                    refs: std::mem::take(&mut refs[at]),
                };
                match blocks.iter_mut().find(|(o, _)| *o == owner) {
                    Some((_, list)) => list.push(candidate),
                    None => blocks.push((owner, vec![candidate])),
                }
            }

            // The unit that holds a block is built once, for every block in it.
            let mut cfgs: FxHashMap<UnitId, Cfg> = FxHashMap::default();
            for (owner, mut candidates) in blocks {
                let Some(unit) = self.units.unit_of(owner) else {
                    continue;
                };
                // A read in another unit (a procedure or trigger can run at any
                // time) is not proven: nothing to build for it.
                candidates.retain(|c| {
                    c.refs.iter().all(|r| {
                        self.uses
                            .get(r)
                            .is_some_and(|u| self.units.unit_of(u.stmt) == Some(unit))
                    })
                });
                if candidates.is_empty() {
                    continue;
                }
                let cfg = cfgs.entry(unit).or_insert_with(|| self.units.build(unit));
                let Some(block) = cfg.block_of(owner) else {
                    continue;
                };
                for chunk in candidates.chunks(CHUNK) {
                    self.solve_block(cfg, block, chunk, &mut cleared);
                }
            }
            cleared
        }

        /// One solve for the `candidates` (at most [`CHUNK`]) of one block.
        fn solve_block(
            &self,
            cfg: &Cfg,
            block: BlockId,
            candidates: &[Candidate],
            out: &mut [bool],
        ) {
            // Statement -> the candidates (bits) it assigns.
            let mut writes: FxHashMap<NodeId, u64> = FxHashMap::default();
            let mut reads: Vec<(usize, Vec<NodeId>)> = Vec::new();
            let mut undoable = 0u64;
            for (bit, c) in candidates.iter().enumerate() {
                let mut written = false;
                let mut read_stmts = Vec::new();
                let mut proven = true;
                for r in &c.refs {
                    // `uses` has it: the unit check above placed every one.
                    let u = self.uses[r];
                    let nodes = cfg.nodes_of(u.stmt);
                    if nodes.is_empty() {
                        // Not in this unit's CFG: another unit, or a statement
                        // the CFG does not execute.
                        proven = false;
                        break;
                    }
                    let inside = nodes.iter().all(|&n| cfg.node_in_block(n, block));
                    match (u.write, inside) {
                        (true, true) => {
                            *writes.entry(u.stmt).or_default() |= 1 << bit;
                            written = true;
                        }
                        (true, false) => {
                            proven = false;
                            break;
                        }
                        (false, true) => {}
                        (false, false) => read_stmts.push(u.stmt),
                    }
                }
                if proven && written && !read_stmts.is_empty() {
                    reads.push((bit, read_stmts));
                    if c.undoable {
                        undoable |= 1 << bit;
                    }
                }
            }
            if reads.is_empty() {
                return;
            }

            let analysis = Assigned {
                writes: &writes,
                literal_writes: &self.literal_writes,
                undoable,
                defined_in: block,
            };
            let solved = solve(cfg, &analysis);
            for (bit, read_stmts) in reads {
                let assigned = read_stmts.iter().all(|&r| {
                    cfg.nodes_of(r)
                        .iter()
                        .all(|&n| solved.before(n) & (1 << bit) != 0)
                });
                out[candidates[bit].at] = assigned;
            }
        }

        fn walk(&mut self, stmts: &[Statement]) {
            for s in stmts {
                self.stmt(s);
            }
        }

        fn stmt(&mut self, s: &Statement) {
            let id = s.id;
            let read = |this: &mut Self, e: &Expression| this.expr(e, id);
            match &s.kind {
                SK::VariableDeclaration {
                    initial_value,
                    no_undo,
                    ..
                } => {
                    if *no_undo {
                        self.no_undo.insert(id);
                    }
                    if let Some(e) = initial_value {
                        read(self, e);
                    }
                }
                SK::Assignment { target, value } => {
                    if matches!(target.kind, ExpressionKind::Identifier(_))
                        && matches!(value.kind, ExpressionKind::Literal(_))
                    {
                        self.literal_writes.insert(id);
                    }
                    self.target(target, id);
                    read(self, value);
                }
                SK::Assign { assignments } => {
                    for pair in assignments {
                        self.target(&pair.target, id);
                        read(self, &pair.value);
                    }
                }
                SK::BufferCopy { assignments, .. } => {
                    for pair in assignments {
                        self.target(&pair.target, id);
                        read(self, &pair.value);
                    }
                }
                SK::ExpressionStatement(e) | SK::Throw(e) | SK::Return(Some(e)) => read(self, e),
                SK::DeleteObject {
                    target,
                    extra_targets,
                    ..
                } => {
                    read(self, target);
                    for e in extra_targets {
                        read(self, e);
                    }
                }
                SK::ReleaseObject { target, .. } => read(self, target),
                SK::CreateWidgetPool { name, .. } | SK::DeleteWidgetPool { name, .. } => {
                    if let Some(e) = name {
                        read(self, e);
                    }
                }
                SK::Apply { event, target } => {
                    read(self, event);
                    if let Some(e) = target {
                        read(self, e);
                    }
                }
                SK::PreprocMessage { expression } => read(self, expression),
                SK::Do {
                    from,
                    to,
                    by,
                    while_condition,
                    on_actions,
                    body,
                    ..
                } => {
                    for e in [from, to, by, while_condition].into_iter().flatten() {
                        read(self, e);
                    }
                    for e in on_actions {
                        read(self, e);
                    }
                    self.walk(body);
                }
                SK::Repeat {
                    while_condition,
                    on_actions,
                    body,
                } => {
                    if let Some(e) = while_condition {
                        read(self, e);
                    }
                    for e in on_actions {
                        read(self, e);
                    }
                    self.walk(body);
                }
                SK::ForEach {
                    where_clause,
                    on_actions,
                    body,
                    ..
                } => {
                    if let Some(e) = where_clause {
                        read(self, e);
                    }
                    for e in on_actions {
                        read(self, e);
                    }
                    self.walk(body);
                }
                SK::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    read(self, condition);
                    self.stmt(then_branch);
                    if let Some(e) = else_branch {
                        self.stmt(e);
                    }
                }
                SK::Case {
                    expression,
                    when_branches,
                    otherwise,
                } => {
                    read(self, expression);
                    for w in when_branches {
                        for v in &w.values {
                            read(self, v);
                        }
                        self.walk(&w.body);
                    }
                    if let Some(o) = otherwise {
                        self.walk(o);
                    }
                }
                SK::Find {
                    key_value,
                    where_clause,
                    ..
                } => {
                    for e in [key_value, where_clause].into_iter().flatten() {
                        read(self, e);
                    }
                }
                SK::Run {
                    target,
                    arguments,
                    in_handle,
                    persistent_handle,
                    async_handle,
                    event_procedure,
                    ..
                } => {
                    if let RunTarget::Dynamic(e) = target {
                        read(self, e);
                    }
                    for a in arguments {
                        read(self, &a.expression);
                    }
                    for e in [in_handle, persistent_handle, async_handle, event_procedure]
                        .into_iter()
                        .flatten()
                    {
                        read(self, e);
                    }
                }
                SK::Display { items, .. } => {
                    for item in items {
                        read(self, &item.expression);
                        if let Some(w) = &item.when_condition {
                            read(self, w);
                        }
                    }
                }
                SK::Message { items, .. } => {
                    for e in items {
                        read(self, e);
                    }
                }
                SK::Publish {
                    event_name,
                    from_handle,
                    arguments,
                } => {
                    read(self, event_name);
                    if let Some(e) = from_handle {
                        read(self, e);
                    }
                    for a in arguments {
                        read(self, &a.expression);
                    }
                }
                SK::Subscribe {
                    subscriber,
                    event_name,
                    target,
                    ..
                } => {
                    if let Some(e) = subscriber {
                        read(self, e);
                    }
                    read(self, event_name);
                    if let SubscribeTarget::InHandle(e) = target {
                        read(self, e);
                    }
                }
                SK::Unsubscribe {
                    subscriber,
                    event_name,
                    in_handle,
                } => {
                    for e in [subscriber, event_name, in_handle].into_iter().flatten() {
                        read(self, e);
                    }
                }
                SK::StreamIo { operation, .. } => match operation {
                    StreamOperation::From(e) | StreamOperation::Through(e) => read(self, e),
                    StreamOperation::To { target, .. } => read(self, target),
                    StreamOperation::Close => {}
                },
                SK::Create {
                    target:
                        CreateTarget::Handle {
                            handle,
                            widget_pool,
                            class,
                            assignments,
                            triggers,
                            connect,
                            for_table,
                            ..
                        },
                    ..
                } => {
                    self.target(handle, id);
                    if let Some(e) = for_table {
                        read(self, e);
                    }
                    for e in [widget_pool.as_ref(), class.as_ref()].into_iter().flatten() {
                        read(self, e);
                    }
                    if let Some(file) = connect.as_ref().and_then(|c| c.file.as_ref()) {
                        read(self, file);
                    }
                    for a in assignments {
                        read(self, &a.value);
                    }
                    for t in triggers {
                        match &t.action {
                            OnAction::Block(b) => self.stmt(b),
                            OnAction::PersistentRun {
                                in_handle,
                                arguments,
                                ..
                            } => {
                                for e in in_handle.iter().chain(arguments) {
                                    read(self, e);
                                }
                            }
                            OnAction::Revert => {}
                        }
                    }
                }
                SK::On { kind } => match kind {
                    OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action
                    {
                        OnAction::Block(b) => self.stmt(b),
                        OnAction::PersistentRun {
                            in_handle,
                            arguments,
                            ..
                        } => {
                            for e in in_handle.iter().chain(arguments) {
                                read(self, e);
                            }
                        }
                        OnAction::Revert => {}
                    },
                    OnKind::KeyRemap { .. } => {}
                },
                SK::Label { body, .. } => self.stmt(body),
                SK::Block(body)
                | SK::VariableDeclarationList(body)
                | SK::Procedure { body, .. }
                | SK::Function { body, .. }
                | SK::Class { body, .. }
                | SK::Interface { body, .. }
                | SK::Destructor { body }
                | SK::Catch { body, .. }
                | SK::Finally { body } => self.walk(body),
                SK::Method {
                    parameters, body, ..
                }
                | SK::Constructor {
                    parameters, body, ..
                } => {
                    self.walk(parameters);
                    self.walk(body);
                }
                SK::Property {
                    get_body,
                    set_body,
                    set_parameters,
                    ..
                } => {
                    self.walk(set_parameters);
                    for b in [get_body, set_body].into_iter().flatten() {
                        self.walk(b);
                    }
                }
                SK::PreprocIf(p) => {
                    self.walk(&p.then_branch);
                    for (_, b) in &p.elseif_branches {
                        self.walk(b);
                    }
                    if let Some(b) = &p.else_branch {
                        self.walk(b);
                    }
                }
                // Statements without expressions the semantic pass resolves
                // here. A reference they do hold stays unplaced, which makes
                // any proof over it give up.
                _ => {}
            }
        }

        /// An assignment target: a bare name or a subscripted one is a write;
        /// anything inside it (a subscript, a qualifier) is a read.
        fn target(&mut self, e: &Expression, stmt: NodeId) {
            match &e.kind {
                ExpressionKind::Identifier(_) => {
                    self.uses.insert(e.id, Use { stmt, write: true });
                }
                ExpressionKind::ArrayAccess { array, index }
                    if matches!(array.kind, ExpressionKind::Identifier(_)) =>
                {
                    self.uses.insert(array.id, Use { stmt, write: true });
                    self.expr(index, stmt);
                }
                _ => self.expr(e, stmt),
            }
        }

        /// A read of `e` and everything in it.
        fn expr(&mut self, e: &Expression, stmt: NodeId) {
            self.uses.insert(e.id, Use { stmt, write: false });
            let mut sub = |x: &Expression| self.expr(x, stmt);
            match &e.kind {
                ExpressionKind::Add(a, b)
                | ExpressionKind::Minus(a, b)
                | ExpressionKind::Multiply(a, b)
                | ExpressionKind::Divide(a, b)
                | ExpressionKind::Modulo(a, b)
                | ExpressionKind::Equal(a, b)
                | ExpressionKind::NotEqual(a, b)
                | ExpressionKind::LessThan(a, b)
                | ExpressionKind::LessThanOrEqual(a, b)
                | ExpressionKind::GreaterThan(a, b)
                | ExpressionKind::GreaterThanOrEqual(a, b)
                | ExpressionKind::Begins(a, b)
                | ExpressionKind::Matches(a, b)
                | ExpressionKind::Contains(a, b)
                | ExpressionKind::And(a, b)
                | ExpressionKind::Or(a, b)
                | ExpressionKind::ArrayAccess { array: a, index: b } => {
                    sub(a);
                    sub(b);
                }
                ExpressionKind::Negate(a)
                | ExpressionKind::Not(a)
                | ExpressionKind::MemberAccess { object: a, .. }
                | ExpressionKind::FieldAccess { qualifier: a, .. } => sub(a),
                ExpressionKind::IfThenElse(a, b, c) => {
                    sub(a);
                    sub(b);
                    sub(c);
                }
                ExpressionKind::FunctionCall { arguments, .. }
                | ExpressionKind::New { arguments, .. } => {
                    for a in arguments {
                        sub(a);
                    }
                }
                ExpressionKind::MethodCall {
                    object, arguments, ..
                } => {
                    sub(object);
                    for a in arguments {
                        sub(a);
                    }
                }
                ExpressionKind::CanFind { where_clause, .. } => {
                    if let Some(w) = where_clause {
                        sub(w);
                    }
                }
                ExpressionKind::PreprocIf(p) => {
                    sub(&p.condition);
                    sub(&p.then_branch);
                    for (c, b) in &p.elseif_branches {
                        sub(c);
                        sub(b);
                    }
                    if let Some(b) = &p.else_branch {
                        sub(b);
                    }
                }
                ExpressionKind::Literal(_)
                | ExpressionKind::Identifier(_)
                | ExpressionKind::IncludeReference { .. }
                | ExpressionKind::IncludeArgReference { .. }
                | ExpressionKind::PreprocReference(_) => {}
            }
        }
    }

    /// Forward must-analysis: which variables (one bit each) are assigned on
    /// every path here?
    struct Assigned<'a> {
        writes: &'a FxHashMap<NodeId, u64>,
        literal_writes: &'a FxHashSet<NodeId>,
        /// Variables without `NO-UNDO`.
        undoable: u64,
        defined_in: BlockId,
    }

    impl Analysis for Assigned<'_> {
        type Fact = u64;

        fn direction(&self) -> Direction {
            Direction::Forward
        }

        fn initial(&self, _: &Cfg) -> u64 {
            u64::MAX
        }

        fn boundary(&self, _: &Cfg) -> u64 {
            0
        }

        fn join(&self, acc: &mut u64, other: &u64) {
            *acc &= *other;
        }

        fn transfer(&self, cfg: &Cfg, node: NodeIx, fact: &mut u64) {
            let n = cfg.node(node);
            // A handler or an `UNDO` statement that undoes a block overlapping
            // the defining block can restore an undoable variable's earlier
            // value.
            if cfg.undone_block(node).is_some_and(|b| {
                cfg.block_contains(b, self.defined_in) || cfg.block_contains(self.defined_in, b)
            }) {
                *fact &= !self.undoable;
            }
            if n.kind == NodeKind::Stmt
                && let Some(written) = n.stmt.and_then(|s| self.writes.get(&s))
            {
                *fact |= written;
            }
        }

        fn transfer_edge(&self, cfg: &Cfg, node: NodeIx, edge: &Edge, fact: &mut u64) {
            // A statement that raised did not finish its assignment, unless it
            // is a literal assignment, which cannot raise.
            let raised = matches!(edge.kind, EdgeKind::Raise(_))
                && !cfg
                    .node(node)
                    .stmt
                    .is_some_and(|s| self.literal_writes.contains(&s));
            if !raised {
                self.transfer(cfg, node, fact);
            }
        }
    }
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
            on_actions: Vec::new(),
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
