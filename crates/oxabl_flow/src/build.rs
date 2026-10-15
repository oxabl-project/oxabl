//! Builds the CFGs of one file.
//!
//! One pass per unit walks the statements with a stack of frames, one per
//! enclosing ABL block. Every non-sequential transfer (LEAVE, NEXT, RETURN, a
//! raised condition, a handler action) is a *jump* to a target frame; a jump
//! runs, innermost first, the `FINALLY` of every frame it leaves. Each such
//! `FINALLY` is built as a copy whose end continues to the jump's target, and
//! copies are shared by jumps with the same target and kind.

use std::mem;

use oxabl_ast::{
    FindType, LockType, NodeId, OnAction, OnKind, Span, Statement, StatementKind as SK,
};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::FileFlow;
use crate::model::*;
use crate::tokens::{self, Control, Header, Input};

/// Options for [`build_file_with`].
#[derive(Debug, Clone, Copy, Default)]
pub struct FlowOptions {
    /// The error directive the compiler applies to every file (the
    /// `-undothrow` startup option). A directive in the file also applies;
    /// the stronger of the two wins.
    pub default_directive: Option<ErrorDirective>,
}

/// Build the CFGs of every unit in a parsed file.
///
/// `source` must be the text the statements were parsed from (the expanded
/// text when includes were expanded): block-header phrases are read from it.
pub fn build_file(program: &[Statement], source: &str) -> FileFlow {
    build_file_with(program, source, &FlowOptions::default())
}

/// [`build_file`] with options.
pub fn build_file_with(program: &[Statement], source: &str, options: &FlowOptions) -> FileFlow {
    let directive = file_directive(program, source).max(options.default_directive);
    let specs = unit_specs(program, source);
    let units = specs
        .iter()
        .enumerate()
        .map(|(i, spec)| Builder::new(source, directive, UnitId(i as u32), spec).build(spec))
        .collect();
    FileFlow { units, directive }
}

/// Builds the units of a parsed file one at a time.
///
/// [`build_file`] builds every unit. A question about one block needs only the
/// unit that holds it: [`unit_of`](Self::unit_of) says which unit a statement
/// is in, and [`build`](Self::build) builds that unit alone. The result equals
/// the unit of [`build_file`] with the same [`UnitId`].
pub struct UnitBuilder<'a> {
    source: &'a str,
    directive: Option<ErrorDirective>,
    specs: Vec<UnitSpec<'a>>,
    unit_of: FxHashMap<NodeId, UnitId>,
}

impl<'a> UnitBuilder<'a> {
    /// Find the units of `program`, which was parsed from `source`. This walks
    /// the statements once and builds nothing.
    pub fn new(program: &'a [Statement], source: &'a str, options: &FlowOptions) -> Self {
        let directive = file_directive(program, source).max(options.default_directive);
        let specs = unit_specs(program, source);
        let mut unit_of = FxHashMap::default();
        for (i, spec) in specs.iter().enumerate() {
            index_stmts(spec.body, UnitId(i as u32), &mut unit_of);
        }
        // The unit a statement defines (and the `DO` of a trigger) is its own,
        // not the one that registers it.
        for (i, spec) in specs.iter().enumerate() {
            let id = UnitId(i as u32);
            unit_of.entry(spec.stmt).or_insert(id);
            if let Some(h) = spec.header {
                unit_of.entry(h.id).or_insert(id);
            }
        }
        UnitBuilder {
            source,
            directive,
            specs,
            unit_of,
        }
    }

    /// The unit whose own code holds `stmt`: a block or an ordinary statement
    /// at any depth, but not one inside a nested unit. A statement that defines
    /// a unit, and the `DO` of an `ON ... DO:` trigger, answer with that unit.
    /// A property with two accessors answers with the getter. `None` when no
    /// unit holds `stmt`.
    pub fn unit_of(&self, stmt: NodeId) -> Option<UnitId> {
        self.unit_of.get(&stmt).copied()
    }

    /// Build one unit.
    pub fn build(&self, unit: UnitId) -> Cfg {
        let spec = &self.specs[unit.index()];
        Builder::new(self.source, self.directive, unit, spec).build(spec)
    }
}

/// Map every statement of `stmts`, outside nested units, to `unit`.
fn index_stmts(stmts: &[Statement], unit: UnitId, out: &mut FxHashMap<NodeId, UnitId>) {
    for s in stmts {
        if !is_unit_stmt(s) {
            out.insert(s.id, unit);
            for list in child_lists(s) {
                index_stmts(list, unit, out);
            }
        }
    }
}

/// The main block and every nested unit, in source pre-order.
fn unit_specs<'a>(program: &'a [Statement], source: &str) -> Vec<UnitSpec<'a>> {
    let main_kind = if program
        .iter()
        .any(|s| matches!(s.kind, SK::TriggerProcedure { .. }))
    {
        UnitKind::DbTrigger
    } else {
        UnitKind::Main
    };
    let mut specs = vec![UnitSpec {
        kind: main_kind,
        name: None,
        stmt: NodeId::PROGRAM,
        span: Span {
            start: 0,
            end: source.len() as u32,
        },
        parent: None,
        body: program,
        header: None,
    }];
    collect_units(program, UnitId(0), source, &mut specs);
    specs
}

fn file_directive(program: &[Statement], source: &str) -> Option<ErrorDirective> {
    program
        .iter()
        .filter(|s| matches!(s.kind, SK::Skipped { .. }))
        .filter_map(|s| match tokens::read_control(source, s.span) {
            Control::Directive(d) => Some(d),
            _ => None,
        })
        .max()
}

struct UnitSpec<'a> {
    kind: UnitKind,
    name: Option<String>,
    stmt: NodeId,
    span: Span,
    parent: Option<UnitId>,
    body: &'a [Statement],
    /// The `DO` of an `ON ... DO:` trigger, whose header phrases apply to the
    /// trigger block.
    header: Option<&'a Statement>,
}

/// Find every nested unit, in source pre-order.
fn collect_units<'a>(
    stmts: &'a [Statement],
    parent: UnitId,
    src: &str,
    out: &mut Vec<UnitSpec<'a>>,
) {
    for s in stmts {
        match &s.kind {
            SK::Procedure { name, body, .. } => {
                let id = push_unit(out, s, parent, UnitKind::Procedure, Some(&name.name), body);
                collect_units(body, id, src, out);
            }
            SK::Function { name, body, .. } => {
                if !body.is_empty() || tokens::function_has_block(src, s.span) {
                    let id = push_unit(out, s, parent, UnitKind::Function, Some(&name.name), body);
                    collect_units(body, id, src, out);
                }
            }
            SK::Method {
                name,
                body,
                is_abstract,
                ..
            } => {
                if !is_abstract {
                    let id = push_unit(out, s, parent, UnitKind::Method, Some(&name.name), body);
                    collect_units(body, id, src, out);
                }
            }
            SK::Constructor { body, .. } => {
                let id = push_unit(out, s, parent, UnitKind::Constructor, None, body);
                collect_units(body, id, src, out);
            }
            SK::Destructor { body } => {
                let id = push_unit(out, s, parent, UnitKind::Destructor, None, body);
                collect_units(body, id, src, out);
            }
            SK::Property {
                name,
                get_body,
                set_body,
                ..
            } => {
                for (kind, body) in [
                    (UnitKind::PropertyGet, get_body),
                    (UnitKind::PropertySet, set_body),
                ] {
                    if let Some(body) = body.as_deref().filter(|b| !b.is_empty()) {
                        let id = push_unit(out, s, parent, kind, Some(&name.name), body);
                        collect_units(body, id, src, out);
                    }
                }
            }
            SK::Class { body, .. } => collect_units(body, parent, src, out),
            SK::Interface { .. } => {}
            SK::On { kind } => {
                if let Some(action) = trigger_block(kind) {
                    let (body, header) = match &action.kind {
                        SK::Do { body, .. } => (body.as_slice(), Some(action)),
                        _ => (std::slice::from_ref(action), None),
                    };
                    let id = push_unit(out, s, parent, UnitKind::Trigger, None, body);
                    out[id.index()].header = header;
                    collect_units(body, id, src, out);
                }
            }
            _ => {
                for list in child_lists(s) {
                    collect_units(list, parent, src, out);
                }
            }
        }
    }
}

fn push_unit<'a>(
    out: &mut Vec<UnitSpec<'a>>,
    s: &Statement,
    parent: UnitId,
    kind: UnitKind,
    name: Option<&str>,
    body: &'a [Statement],
) -> UnitId {
    out.push(UnitSpec {
        kind,
        name: name.map(str::to_string),
        stmt: s.id,
        span: s.span,
        parent: Some(parent),
        body,
        header: None,
    });
    UnitId(out.len() as u32 - 1)
}

fn trigger_block(kind: &OnKind) -> Option<&Statement> {
    match kind {
        OnKind::UiEvent {
            action: OnAction::Block(b),
            ..
        }
        | OnKind::DbEvent {
            action: OnAction::Block(b),
            ..
        } => Some(b),
        _ => None,
    }
}

/// The statement lists nested in a statement that is not a unit.
fn child_lists(s: &Statement) -> SmallVec<[&[Statement]; 2]> {
    let mut out: SmallVec<[&[Statement]; 2]> = SmallVec::new();
    match &s.kind {
        SK::Do { body, .. }
        | SK::Repeat { body, .. }
        | SK::ForEach { body, .. }
        | SK::Catch { body, .. }
        | SK::Finally { body }
        | SK::Block(body)
        | SK::VariableDeclarationList(body) => out.push(body),
        SK::Label { body, .. } => out.push(std::slice::from_ref(&**body)),
        SK::If {
            then_branch,
            else_branch,
            ..
        } => {
            out.push(std::slice::from_ref(&**then_branch));
            if let Some(e) = else_branch {
                out.push(std::slice::from_ref(&**e));
            }
        }
        SK::Case {
            when_branches,
            otherwise,
            ..
        } => {
            out.extend(when_branches.iter().map(|w| w.body.as_slice()));
            if let Some(o) = otherwise {
                out.push(o);
            }
        }
        SK::PreprocIf(p) => {
            out.push(&p.then_branch);
            out.extend(p.elseif_branches.iter().map(|(_, b)| b.as_slice()));
            if let Some(e) = &p.else_branch {
                out.push(e);
            }
        }
        _ => {}
    }
    out
}

fn is_unit_stmt(s: &Statement) -> bool {
    match &s.kind {
        SK::Procedure { .. }
        | SK::Function { .. }
        | SK::Method { .. }
        | SK::Constructor { .. }
        | SK::Destructor { .. }
        | SK::Property { .. }
        | SK::Class { .. }
        | SK::Interface { .. } => true,
        SK::On { kind } => trigger_block(kind).is_some(),
        _ => false,
    }
}

/// Whether any statement in `stmts` (outside nested units) blocks for input.
fn contains_input(stmts: &[Statement], src: &str) -> bool {
    stmts.iter().any(|s| match &s.kind {
        SK::Skipped { .. } => tokens::skipped_input(src, s.span) == Input::Blocking,
        SK::Update { .. } | SK::Set { .. } => true,
        SK::Message { set_targets, .. } => !set_targets.is_empty(),
        _ if is_unit_stmt(s) => false,
        _ => child_lists(s).iter().any(|l| contains_input(l, src)),
    })
}

/// A block body split into its statements, `CATCH` blocks and `FINALLY`.
struct Body<'a> {
    main: Vec<&'a Statement>,
    catches: Vec<&'a Statement>,
    finally: Option<&'a Statement>,
}

fn split_body(body: &[Statement]) -> Body<'_> {
    let mut out = Body {
        main: Vec::with_capacity(body.len()),
        catches: Vec::new(),
        finally: None,
    };
    for s in body {
        match s.kind {
            SK::Catch { .. } => out.catches.push(s),
            SK::Finally { .. } if out.finally.is_none() => out.finally = Some(s),
            _ => out.main.push(s),
        }
    }
    out
}

fn body_of(s: &Statement) -> &[Statement] {
    match &s.kind {
        SK::Do { body, .. }
        | SK::Repeat { body, .. }
        | SK::ForEach { body, .. }
        | SK::Catch { body, .. }
        | SK::Finally { body } => body,
        _ => &[],
    }
}

/// From the block statement's start to its first body statement.
fn header_span(s: &Statement) -> Span {
    let end = body_of(s)
        .first()
        .map(|b| b.span.start)
        .filter(|&e| e >= s.span.start && e <= s.span.end)
        .unwrap_or(s.span.end);
    Span {
        start: s.span.start,
        end,
    }
}

fn point(at: u32) -> Span {
    Span { start: at, end: at }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FrameKind {
    Routine,
    Block,
    /// A `CATCH` of the block at frame `owner`.
    Catch {
        owner: usize,
    },
    /// A `FINALLY` copy; the frames below it end at the owner's parent.
    Finally,
}

struct Frame<'a> {
    kind: FrameKind,
    block: BlockId,
    stmt: NodeId,
    header: Span,
    end_at: u32,
    label: Option<&'a str>,
    iterating: bool,
    /// The body runs at most once (`FOR FIRST`, `FOR LAST`), though the block
    /// is iterating for `NEXT` and `RETRY`.
    single_pass: bool,
    head: NodeIx,
    end: Option<NodeIx>,
    after: Option<NodeIx>,
    finally: Option<(&'a Statement, BlockId)>,
    handlers: SmallVec<[(Condition, NodeIx); 2]>,
}

#[derive(Debug, Clone, Copy)]
enum Target {
    /// After the block of a frame (the unit's normal exit for the routine).
    After(usize),
    /// The end of the current iteration of a frame's block.
    End(usize),
    /// The header of a frame's block (loop back, retry).
    Head(usize),
    /// A frame's handler for a condition.
    Handler(usize, Condition),
    /// A unit exit.
    Exit(ExitKind),
}

/// How a simple statement transfers control.
enum Shape<'a> {
    Plain {
        raises: bool,
        /// Reads input, so may raise ENDKEY.
        endkey: bool,
        /// Runs other code, waits for a lock or waits for an event, so may
        /// raise STOP.
        waits: bool,
        opaque: Option<OpaqueReason>,
    },
    Leave(Option<&'a str>),
    Next(Option<&'a str>),
    Return {
        raises: bool,
    },
    ReturnError,
    Throw,
    Undo {
        label: Option<String>,
        action: Option<Action>,
        /// `UNDO, RETURN v` and `UNDO, THROW v` evaluate `v` first, and that
        /// can raise.
        raises: bool,
    },
    Stop,
    Quit,
}

impl Shape<'_> {
    fn plain(raises: bool) -> Self {
        Shape::Plain {
            raises,
            endkey: false,
            waits: false,
            opaque: None,
        }
    }
}

struct BlockInit<'a, 'h> {
    kind: BlockKind,
    stmt: &'a Statement,
    parent: BlockId,
    label: Option<&'a str>,
    iterating: bool,
    transaction: bool,
    header: &'h Header,
}

struct Builder<'a> {
    src: &'a str,
    directive: Option<ErrorDirective>,
    cfg: Cfg,
    frames: Vec<Frame<'a>>,
    pending: Vec<(NodeIx, EdgeKind)>,
    finally_copies: FxHashMap<(BlockId, NodeIx, EdgeKind), NodeIx>,
}

impl<'a> Builder<'a> {
    fn new(src: &'a str, directive: Option<ErrorDirective>, id: UnitId, spec: &UnitSpec) -> Self {
        Builder {
            src,
            directive,
            cfg: Cfg {
                id,
                unit: Unit {
                    kind: spec.kind,
                    name: spec.name.clone(),
                    stmt: spec.stmt,
                    span: spec.span,
                    parent: spec.parent,
                },
                nodes: Vec::new(),
                edges: Vec::new(),
                succ: Vec::new(),
                pred: Vec::new(),
                blocks: Vec::new(),
                entry: NodeIx(0),
                exits: [None; 4],
                stmt_nodes: FxHashMap::default(),
                stmt_block: FxHashMap::default(),
            },
            frames: Vec::new(),
            pending: Vec::new(),
            finally_copies: FxHashMap::default(),
        }
    }

    fn build(mut self, spec: &UnitSpec<'a>) -> Cfg {
        let header = spec
            .header
            .map(|h| tokens::read_header(self.src, header_span(h)))
            .unwrap_or_default();
        let body = split_body(spec.body);
        let kind = BlockKind::Routine(spec.kind);
        let handling = self.handling(kind, false, &header);
        self.cfg.blocks.push(Block {
            kind,
            stmt: spec.stmt,
            span: spec.span,
            parent: None,
            label: None,
            iterating: false,
            transaction: false,
            stop_after: header.stop_after,
            buffers: Vec::new(),
            input_blocking: contains_input(spec.body, self.src),
            handling,
            catches: SmallVec::new(),
            finally: None,
            catch_type: None,
        });
        let b0 = BlockId::ROUTINE;
        self.cfg.stmt_block.insert(spec.stmt, b0);
        if let Some(h) = spec.header {
            self.cfg.stmt_block.insert(h.id, b0);
        }
        let finally = self.add_handler_blocks(b0, &body);

        let entry = self.new_node(NodeKind::Entry, b0, Some(spec.stmt), point(spec.span.start));
        let exit = self.new_node(
            NodeKind::Exit(ExitKind::Return),
            b0,
            None,
            point(spec.span.end),
        );
        self.cfg.entry = entry;
        self.cfg.exits[ExitKind::Return as usize] = Some(exit);
        self.frames.push(Frame {
            kind: FrameKind::Routine,
            block: b0,
            stmt: spec.stmt,
            header: point(spec.span.start),
            end_at: spec.span.end,
            label: None,
            iterating: false,
            single_pass: false,
            head: entry,
            end: None,
            after: Some(exit),
            finally,
            handlers: SmallVec::new(),
        });
        self.pending.push((entry, EdgeKind::Normal));
        for s in body.main {
            self.stmt(s);
        }
        self.finish_frame(&body.catches);
        self.frames.pop();
        self.finalize()
    }

    fn finalize(mut self) -> Cfg {
        let mut pred = vec![SmallVec::new(); self.cfg.nodes.len()];
        for (i, e) in self.cfg.edges.iter().enumerate() {
            pred[e.to.index()].push(i as u32);
        }
        self.cfg.pred = pred;
        self.cfg
    }

    // ----- nodes, edges, blocks -----

    fn new_node(
        &mut self,
        kind: NodeKind,
        block: BlockId,
        stmt: Option<NodeId>,
        span: Span,
    ) -> NodeIx {
        let ix = NodeIx(self.cfg.nodes.len() as u32);
        self.cfg.nodes.push(Node {
            kind,
            block,
            stmt,
            span,
            opaque: None,
            undoes: None,
        });
        self.cfg.succ.push(SmallVec::new());
        if let (Some(id), NodeKind::Stmt | NodeKind::Head) = (stmt, kind) {
            self.cfg.stmt_nodes.entry(id).or_default().push(ix);
        }
        ix
    }

    /// A new node in the current block that the pending flow reaches.
    fn place(&mut self, kind: NodeKind, stmt: &Statement, span: Span) -> NodeIx {
        let block = self.cur_block();
        self.place_in(block, kind, stmt, span)
    }

    fn place_in(&mut self, block: BlockId, kind: NodeKind, stmt: &Statement, span: Span) -> NodeIx {
        let n = self.new_node(kind, block, Some(stmt.id), span);
        for (from, kind) in mem::take(&mut self.pending) {
            self.edge(from, n, kind);
        }
        n
    }

    fn edge(&mut self, from: NodeIx, to: NodeIx, kind: EdgeKind) {
        let edges = &self.cfg.edges;
        if self.cfg.succ[from.index()]
            .iter()
            .any(|&e| edges[e as usize].to == to && edges[e as usize].kind == kind)
        {
            return;
        }
        self.cfg.succ[from.index()].push(self.cfg.edges.len() as u32);
        self.cfg.edges.push(Edge { from, to, kind });
    }

    fn set_opaque(&mut self, n: NodeIx, reason: OpaqueReason) {
        self.cfg.nodes[n.index()].opaque.get_or_insert(reason);
    }

    fn top(&self) -> usize {
        self.frames.len() - 1
    }

    fn cur_block(&self) -> BlockId {
        self.frames[self.top()].block
    }

    fn block(&self, frame: usize) -> &Block {
        &self.cfg.blocks[self.frames[frame].block.index()]
    }

    /// The handling of each condition for a block, defaults included.
    fn handling(
        &self,
        kind: BlockKind,
        transaction: bool,
        header: &Header,
    ) -> [Option<Handling>; 4] {
        let mut out: [Option<Handling>; 4] = Default::default();
        let routine = matches!(kind, BlockKind::Routine(_));
        let destructor = kind == BlockKind::Routine(UnitKind::Destructor);
        let undoable = routine
            || matches!(kind, BlockKind::Repeat | BlockKind::For(_))
            || (kind == BlockKind::Do && transaction);
        if undoable {
            let (action, origin) = match self.directive {
                Some(ErrorDirective::BlockLevel) if !destructor => {
                    (Action::Throw, Origin::BlockLevel)
                }
                Some(ErrorDirective::RoutineLevel) if routine && !destructor => {
                    (Action::Throw, Origin::RoutineLevel)
                }
                _ => (Action::Retry(None), Origin::Default),
            };
            out[Condition::Error as usize] = Some(Handling {
                undo: None,
                action,
                origin,
            });
            out[Condition::Endkey as usize] = Some(Handling {
                undo: None,
                action: Action::Leave(None),
                origin: Origin::Default,
            });
        }
        if matches!(kind, BlockKind::Catch | BlockKind::Finally) {
            out[Condition::Error as usize] = Some(Handling {
                undo: None,
                action: Action::Throw,
                origin: Origin::Default,
            });
        }
        for (condition, handling) in &header.phrases {
            out[*condition as usize] = Some(handling.clone());
        }
        out
    }

    /// The block record for a block statement, created on first sight. A
    /// `FINALLY` body is built once per copy, so its blocks are seen again.
    fn open_block(&mut self, init: BlockInit<'a, '_>) -> BlockId {
        let id = init.stmt.id;
        if id != NodeId::DUMMY
            && let Some(&b) = self.cfg.stmt_block.get(&id)
        {
            return b;
        }
        let handling = self.handling(init.kind, init.transaction, init.header);
        let input_blocking = match init.kind {
            BlockKind::Catch | BlockKind::Finally => false,
            _ => contains_input(body_of(init.stmt), self.src),
        };
        let catch_type = match &init.stmt.kind {
            SK::Catch { error_type, .. } => Some(error_type.clone()),
            _ => None,
        };
        let b = BlockId(self.cfg.blocks.len() as u32);
        self.cfg.blocks.push(Block {
            kind: init.kind,
            stmt: id,
            span: init.stmt.span,
            parent: Some(init.parent),
            label: init.label.map(str::to_string),
            iterating: init.iterating,
            transaction: init.transaction,
            stop_after: init.header.stop_after,
            buffers: init.header.buffers.clone(),
            input_blocking,
            handling,
            catches: SmallVec::new(),
            finally: None,
            catch_type,
        });
        if id != NodeId::DUMMY {
            self.cfg.stmt_block.insert(id, b);
        }
        b
    }

    /// Create the `CATCH` and `FINALLY` block records of block `owner`.
    fn add_handler_blocks(
        &mut self,
        owner: BlockId,
        body: &Body<'a>,
    ) -> Option<(&'a Statement, BlockId)> {
        let none = Header::default();
        for &c in &body.catches {
            let cb = self.open_block(BlockInit {
                kind: BlockKind::Catch,
                stmt: c,
                parent: owner,
                label: None,
                iterating: false,
                transaction: false,
                header: &none,
            });
            if !self.cfg.blocks[owner.index()].catches.contains(&cb) {
                self.cfg.blocks[owner.index()].catches.push(cb);
            }
        }
        body.finally.map(|f| {
            let fb = self.open_block(BlockInit {
                kind: BlockKind::Finally,
                stmt: f,
                parent: owner,
                label: None,
                iterating: false,
                transaction: false,
                header: &none,
            });
            self.cfg.blocks[owner.index()].finally = Some(fb);
            (f, fb)
        })
    }

    fn after_node(&mut self, frame: usize) -> NodeIx {
        if let Some(n) = self.frames[frame].after {
            return n;
        }
        let f = &self.frames[frame];
        let (block, stmt, at) = (f.block, f.stmt, f.end_at);
        let parent = self.cfg.blocks[block.index()]
            .parent
            .unwrap_or(BlockId::ROUTINE);
        let n = self.new_node(NodeKind::After(block), parent, Some(stmt), point(at));
        self.frames[frame].after = Some(n);
        n
    }

    fn end_node(&mut self, frame: usize) -> NodeIx {
        if let Some(n) = self.frames[frame].end {
            return n;
        }
        let f = &self.frames[frame];
        let (block, stmt, at) = (f.block, f.stmt, f.end_at);
        let n = self.new_node(NodeKind::End, block, Some(stmt), point(at));
        self.frames[frame].end = Some(n);
        n
    }

    fn handler_node(&mut self, frame: usize, condition: Condition) -> NodeIx {
        let f = &self.frames[frame];
        if let Some(&(_, n)) = f.handlers.iter().find(|(c, _)| *c == condition) {
            return n;
        }
        let (block, stmt, header) = (f.block, f.stmt, f.header);
        let n = self.new_node(NodeKind::Handler(condition), block, Some(stmt), header);
        self.frames[frame].handlers.push((condition, n));
        n
    }

    fn exit_node(&mut self, kind: ExitKind) -> NodeIx {
        if let Some(n) = self.cfg.exits[kind as usize] {
            return n;
        }
        let at = self.cfg.unit.span.end;
        let n = self.new_node(NodeKind::Exit(kind), BlockId::ROUTINE, None, point(at));
        self.cfg.exits[kind as usize] = Some(n);
        n
    }

    // ----- jumps -----

    /// Move control from `from` to `target`, through the `FINALLY` of every
    /// frame left on the way.
    fn jump(&mut self, from: NodeIx, target: Target, kind: EdgeKind) {
        let top = self.top();
        let (dest, first_left) = match target {
            Target::After(t) => (self.after_node(t), t),
            Target::End(t) => (self.end_node(t), t + 1),
            Target::Head(t) => (self.frames[t].head, t),
            Target::Handler(t, c) => (self.handler_node(t, c), t + 1),
            Target::Exit(e) => (self.exit_node(e), 0),
        };
        let mut cont = dest;
        for i in first_left..=top {
            if self.frames[i].finally.is_some() {
                cont = self.finally_copy(i, cont, kind);
            }
        }
        self.edge(from, cont, kind);
    }

    /// The copy of frame `i`'s `FINALLY` that continues to `cont` by `kind`.
    fn finally_copy(&mut self, i: usize, cont: NodeIx, kind: EdgeKind) -> NodeIx {
        let (fin, fb) = self.frames[i].finally.expect("frame has a FINALLY");
        if let Some(&n) = self.finally_copies.get(&(fb, cont, kind)) {
            return n;
        }
        let saved_frames = self.frames.split_off(i);
        let saved_pending = mem::take(&mut self.pending);
        let header = header_span(fin);
        let head = self.new_node(NodeKind::Head, fb, Some(fin.id), header);
        self.frames.push(Frame {
            kind: FrameKind::Finally,
            block: fb,
            stmt: fin.id,
            header,
            end_at: fin.span.end,
            label: None,
            iterating: false,
            single_pass: false,
            head,
            end: None,
            after: None,
            finally: None,
            handlers: SmallVec::new(),
        });
        self.pending.push((head, EdgeKind::Normal));
        self.seq(body_of(fin));
        if !self.pending.is_empty() {
            let end = self.end_node(self.top());
            for (from, k) in mem::take(&mut self.pending) {
                self.edge(from, end, k);
            }
            self.edge(end, cont, kind);
        }
        self.frames.pop();
        self.frames.extend(saved_frames);
        self.pending = saved_pending;
        self.finally_copies.insert((fb, cont, kind), head);
        head
    }

    /// Whether the block of frame `i` handles `condition`: an `ON` phrase or
    /// default, or a `CATCH` that may catch it.
    fn handles(&self, i: usize, condition: Condition) -> bool {
        let b = &self.cfg.blocks[self.frames[i].block.index()];
        b.handling(condition).is_some()
            || b.catches
                .iter()
                .any(|c| self.cfg.blocks[c.index()].may_catch(condition))
    }

    /// The innermost frame at or below `start` that handles `condition`.
    /// Conditions raised in a `CATCH` or `FINALLY` pass its owner by.
    fn handler_frame(&self, start: usize, condition: Condition) -> Option<usize> {
        let mut i = start as isize;
        while i >= 0 {
            let f = &self.frames[i as usize];
            match f.kind {
                FrameKind::Catch { owner } => {
                    i = owner as isize - 1;
                    continue;
                }
                FrameKind::Finally => {
                    i -= 1;
                    continue;
                }
                FrameKind::Routine | FrameKind::Block => {}
            }
            if self.handles(i as usize, condition) {
                return Some(i as usize);
            }
            i -= 1;
        }
        None
    }

    /// The frame an `UNDO` with no label undoes: the closest block with the
    /// error property. In a `CATCH` that is the `CATCH`'s owner, not a block
    /// the owner sits in: a `CATCH` is part of its owner, and `handler_frame`'s
    /// pass-through is for raised conditions only.
    fn undo_frame(&self) -> usize {
        let mut i = self.top() as isize;
        while i >= 0 {
            match self.frames[i as usize].kind {
                FrameKind::Catch { owner } => return owner,
                FrameKind::Finally => {}
                FrameKind::Routine | FrameKind::Block => {
                    if self.handles(i as usize, Condition::Error) {
                        return i as usize;
                    }
                }
            }
            i -= 1;
        }
        0
    }

    /// Whether the code being built is in a block with `STOP-AFTER`.
    fn under_stop_after(&self) -> bool {
        self.frames
            .iter()
            .any(|f| self.cfg.blocks[f.block.index()].stop_after)
    }

    /// `n` may raise STOP. Every node under `STOP-AFTER` may. A node that runs
    /// other code, waits for a lock or waits for an event (`waits`) may too,
    /// but that is modelled only where this unit handles STOP: an unhandled
    /// STOP leaves the unit, and that path is modelled only for `STOP-AFTER`
    /// and the `STOP` statement.
    fn may_stop(&mut self, n: NodeIx, waits: bool) {
        let top = self.top();
        if self.under_stop_after() || (waits && self.handler_frame(top, Condition::Stop).is_some())
        {
            self.raise(n, Condition::Stop, EdgeKind::Raise(Condition::Stop));
        }
    }

    fn unhandled_exit(condition: Condition) -> ExitKind {
        match condition {
            Condition::Error => ExitKind::Error,
            Condition::Endkey => ExitKind::Return,
            Condition::Stop => ExitKind::Stop,
            Condition::Quit => ExitKind::Quit,
        }
    }

    /// `from`, in the current frame, raises `condition`.
    fn raise(&mut self, from: NodeIx, condition: Condition, kind: EdgeKind) {
        let start = self.top();
        self.raise_from(from, start, condition, kind);
    }

    /// The condition goes to the first handler at or below frame `start`.
    fn raise_from(&mut self, from: NodeIx, start: usize, condition: Condition, kind: EdgeKind) {
        let target = match self.handler_frame(start, condition) {
            Some(t) => Target::Handler(t, condition),
            None => Target::Exit(Self::unhandled_exit(condition)),
        };
        self.jump(from, target, kind);
    }

    /// The block of frame `k` passes `condition` on to the enclosing blocks.
    fn propagate(&mut self, from: NodeIx, k: usize, condition: Condition, kind: EdgeKind) {
        if k == 0 {
            self.jump(from, Target::Exit(Self::unhandled_exit(condition)), kind);
        } else {
            self.raise_from(from, k - 1, condition, kind);
        }
    }

    /// The frame `label` names, else `default` (and `from` is opaque).
    fn label_target(&mut self, from: NodeIx, label: Option<&str>, default: usize) -> usize {
        let Some(label) = label else {
            return default;
        };
        match self
            .frames
            .iter()
            .rposition(|f| f.label.is_some_and(|l| l.eq_ignore_ascii_case(label)))
        {
            Some(t) => t,
            None => {
                self.set_opaque(from, OpaqueReason::UnresolvedLabel);
                default
            }
        }
    }

    /// The innermost iterating block, else the routine.
    fn innermost_iterating(&self) -> usize {
        self.frames
            .iter()
            .rposition(|f| f.kind == FrameKind::Block && f.iterating)
            .unwrap_or(0)
    }

    fn leave_to(&mut self, from: NodeIx, t: usize) {
        self.jump(from, Target::After(t), EdgeKind::Leave);
    }

    fn next_to(&mut self, from: NodeIx, t: usize) {
        if self.frames[t].iterating {
            self.jump(from, Target::End(t), EdgeKind::Next);
        } else {
            self.leave_to(from, t);
        }
    }

    /// Carry out an `UNDO` action whose unlabelled target is frame `k`.
    fn apply(&mut self, from: NodeIx, k: usize, action: &Action) {
        match action {
            Action::Retry(label) => {
                let t = self.label_target(from, label.as_deref(), k);
                match self.block(t).retry_becomes() {
                    RetryBehavior::Retry => self.jump(from, Target::Head(t), EdgeKind::Retry),
                    RetryBehavior::Next => self.next_to(from, t),
                    RetryBehavior::Leave => self.leave_to(from, t),
                }
            }
            Action::Leave(label) => {
                let t = self.label_target(from, label.as_deref(), k);
                self.leave_to(from, t);
            }
            Action::Next(label) => {
                let t = self.label_target(from, label.as_deref(), k);
                self.next_to(from, t);
            }
            Action::Return(ReturnKind::Error) => {
                self.jump(from, Target::Exit(ExitKind::Error), EdgeKind::ReturnError)
            }
            Action::Return(_) => self.jump(from, Target::Exit(ExitKind::Return), EdgeKind::Return),
            Action::Throw => self.propagate(from, k, Condition::Error, EdgeKind::Throw),
        }
    }

    // ----- blocks -----

    fn block_stmt(&mut self, s: &'a Statement, label: Option<&'a str>) {
        let hspan = header_span(s);
        let header = tokens::read_header(self.src, hspan);
        let (kind, iterating, tests, transaction) = match &s.kind {
            SK::Do {
                to,
                while_condition,
                transaction,
                ..
            } => {
                let iterating = to.is_some() || while_condition.is_some() || header.has_while;
                (
                    BlockKind::Do,
                    iterating,
                    iterating,
                    *transaction || header.transaction,
                )
            }
            SK::Repeat {
                while_condition, ..
            } => {
                let tests = while_condition.is_some() || header.has_while || header.has_to;
                (BlockKind::Repeat, true, tests, header.transaction)
            }
            _ => (
                BlockKind::For(header.for_qualifier.unwrap_or(ForQualifier::Unqualified)),
                true,
                true,
                header.transaction,
            ),
        };
        let body = split_body(body_of(s));
        let parent = self.cur_block();
        let b = self.open_block(BlockInit {
            kind,
            stmt: s,
            parent,
            label,
            iterating,
            transaction,
            header: &header,
        });
        let finally = self.add_handler_blocks(b, &body);
        let head = self.place_in(b, NodeKind::Head, s, hspan);
        self.frames.push(Frame {
            kind: FrameKind::Block,
            block: b,
            stmt: s.id,
            header: hspan,
            end_at: s.span.end,
            label,
            iterating,
            single_pass: header.single_record,
            head,
            end: None,
            after: None,
            finally,
            handlers: SmallVec::new(),
        });
        if tests {
            self.raise(head, Condition::Error, EdgeKind::Raise(Condition::Error));
            let waits =
                matches!(&s.kind, SK::ForEach { lock_type, .. } if *lock_type != LockType::NoLock);
            self.may_stop(head, waits);
            let after = self.after_node(self.top());
            self.edge(head, after, EdgeKind::LoopDone);
        }
        self.pending.push((
            head,
            if iterating {
                EdgeKind::LoopBody
            } else {
                EdgeKind::Normal
            },
        ));
        for s in body.main {
            self.stmt(s);
        }
        self.finish_frame(&body.catches);
        let frame = self.frames.pop().expect("block frame");
        debug_assert!(self.pending.is_empty());
        if let Some(after) = frame.after {
            self.pending.push((after, EdgeKind::Normal));
        }
    }

    /// Close the top frame: the body's normal end, the `CATCH` blocks, the
    /// handlers, and the end of the iteration.
    fn finish_frame(&mut self, catches: &[&'a Statement]) {
        let k = self.top();
        if !self.pending.is_empty() {
            let end = self.end_node(k);
            for (from, kind) in mem::take(&mut self.pending) {
                self.edge(from, end, kind);
            }
        }

        let mut catch_heads: SmallVec<[(BlockId, NodeIx); 1]> = SmallVec::new();
        let catch_blocks = self.block(k).catches.clone();
        for (&c, &cb) in catches.iter().zip(&catch_blocks) {
            let header = header_span(c);
            let head = self.new_node(NodeKind::Head, cb, Some(c.id), header);
            catch_heads.push((cb, head));
            self.frames.push(Frame {
                kind: FrameKind::Catch { owner: k },
                block: cb,
                stmt: c.id,
                header,
                end_at: c.span.end,
                label: None,
                iterating: false,
                single_pass: false,
                head,
                end: None,
                after: None,
                finally: None,
                handlers: SmallVec::new(),
            });
            self.pending.push((head, EdgeKind::Normal));
            self.seq(body_of(c));
            if !self.pending.is_empty() {
                let end = self.end_node(self.top());
                for (from, kind) in mem::take(&mut self.pending) {
                    self.edge(from, end, kind);
                }
                // After a CATCH the associated block goes on with its next
                // iteration, or is left.
                if self.frames[k].iterating {
                    self.jump(end, Target::End(k), EdgeKind::Next);
                } else {
                    self.jump(end, Target::After(k), EdgeKind::Leave);
                }
            }
            self.frames.pop();
        }

        let mut i = 0;
        while i < self.frames[k].handlers.len() {
            let (condition, h) = self.frames[k].handlers[i];
            i += 1;
            let mut caught_all = false;
            for &(cb, head) in &catch_heads {
                let cblock = &self.cfg.blocks[cb.index()];
                if cblock.may_catch(condition) {
                    caught_all |= cblock.catches_all(condition);
                    self.edge(h, head, EdgeKind::Catch);
                }
            }
            if caught_all {
                continue;
            }
            match self.block(k).handling(condition).map(|h| h.action.clone()) {
                Some(action) => self.apply(h, k, &action),
                None => self.propagate(h, k, condition, EdgeKind::Raise(condition)),
            }
        }

        if let Some(end) = self.frames[k].end {
            if self.frames[k].single_pass {
                self.jump(end, Target::After(k), EdgeKind::LoopDone);
            } else if self.frames[k].iterating {
                self.jump(end, Target::Head(k), EdgeKind::LoopBack);
            } else {
                self.jump(end, Target::After(k), EdgeKind::Normal);
            }
        }
    }

    // ----- statements -----

    /// A statement list that is not a block body.
    fn seq(&mut self, stmts: &'a [Statement]) {
        for s in stmts {
            self.stmt(s);
        }
    }

    fn stmt(&mut self, s: &'a Statement) {
        match &s.kind {
            SK::Do { .. } | SK::Repeat { .. } | SK::ForEach { .. } => self.block_stmt(s, None),
            SK::Label { name, body } => match body.kind {
                SK::Do { .. } | SK::Repeat { .. } | SK::ForEach { .. } => {
                    self.block_stmt(body, Some(name.as_str()))
                }
                _ => self.stmt(body),
            },
            SK::If {
                then_branch,
                else_branch,
                ..
            } => {
                let span = Span {
                    start: s.span.start,
                    end: then_branch.span.start.clamp(s.span.start, s.span.end),
                };
                let n = self.place(NodeKind::Stmt, s, span);
                self.raise(n, Condition::Error, EdgeKind::Raise(Condition::Error));
                self.may_stop(n, false);
                self.pending.push((n, EdgeKind::True));
                self.stmt(then_branch);
                let then_out = mem::take(&mut self.pending);
                self.pending.push((n, EdgeKind::False));
                if let Some(e) = else_branch {
                    self.stmt(e);
                }
                self.pending.splice(0..0, then_out);
            }
            SK::Case {
                when_branches,
                otherwise,
                ..
            } => {
                let n = self.place(NodeKind::Stmt, s, s.span);
                self.raise(n, Condition::Error, EdgeKind::Raise(Condition::Error));
                self.may_stop(n, false);
                let mut out = Vec::new();
                for (i, w) in when_branches.iter().enumerate() {
                    self.pending.push((n, EdgeKind::When(i as u32)));
                    self.seq(&w.body);
                    out.append(&mut self.pending);
                }
                self.pending.push((n, EdgeKind::Otherwise));
                if let Some(o) = otherwise {
                    self.seq(o);
                }
                out.append(&mut self.pending);
                self.pending = out;
            }
            SK::PreprocIf(p) => {
                let n = self.place(NodeKind::Stmt, s, s.span);
                let mut out = Vec::new();
                let branches = std::iter::once(p.then_branch.as_slice())
                    .chain(p.elseif_branches.iter().map(|(_, b)| b.as_slice()))
                    .chain(p.else_branch.as_deref());
                for list in branches {
                    self.pending.push((n, EdgeKind::Preproc));
                    self.seq(list);
                    out.append(&mut self.pending);
                }
                if p.else_branch.is_none() {
                    out.push((n, EdgeKind::Preproc));
                }
                self.pending = out;
            }
            SK::Block(list) | SK::VariableDeclarationList(list) => self.seq(list),
            SK::Catch { .. } | SK::Finally { .. } => {
                let n = self.place(NodeKind::Stmt, s, s.span);
                self.set_opaque(n, OpaqueReason::StrayHandlerBlock);
                self.pending.push((n, EdgeKind::Normal));
            }
            // An ON statement registers its trigger at run time.
            SK::On { .. } => self.simple(s),
            _ if is_unit_stmt(s) => {}
            _ => self.simple(s),
        }
    }

    fn simple(&mut self, s: &'a Statement) {
        let shape = self.shape(s);
        let n = self.place(NodeKind::Stmt, s, s.span);
        match shape {
            Shape::Plain {
                raises,
                endkey,
                waits,
                opaque,
            } => {
                if let Some(reason) = opaque {
                    self.set_opaque(n, reason);
                }
                if raises {
                    self.raise(n, Condition::Error, EdgeKind::Raise(Condition::Error));
                }
                if endkey {
                    self.raise(n, Condition::Endkey, EdgeKind::Raise(Condition::Endkey));
                }
                self.may_stop(n, waits);
                self.pending.push((n, EdgeKind::Normal));
            }
            Shape::Leave(label) => {
                let default = self.innermost_iterating();
                let t = self.label_target(n, label, default);
                self.leave_to(n, t);
            }
            Shape::Next(label) => {
                let default = self.innermost_iterating();
                let t = self.label_target(n, label, default);
                self.next_to(n, t);
            }
            Shape::Return { raises } => {
                if raises {
                    self.raise(n, Condition::Error, EdgeKind::Raise(Condition::Error));
                    self.may_stop(n, false);
                }
                self.jump(n, Target::Exit(ExitKind::Return), EdgeKind::Return);
            }
            Shape::ReturnError => {
                self.jump(n, Target::Exit(ExitKind::Error), EdgeKind::ReturnError)
            }
            Shape::Throw => self.raise(n, Condition::Error, EdgeKind::Throw),
            Shape::Undo {
                label,
                action,
                raises,
            } => {
                // UNDO without a label undoes the closest block with the
                // error property.
                let closest = self.undo_frame();
                let u = self.label_target(n, label.as_deref(), closest);
                self.cfg.nodes[n.index()].undoes = Some(self.frames[u].block);
                if raises {
                    self.raise(n, Condition::Error, EdgeKind::Raise(Condition::Error));
                    self.may_stop(n, false);
                }
                match action {
                    Some(action) => self.apply(n, u, &action),
                    // Without an action the statement's own default is RETRY.
                    // Whether a block action of THROW overrides it is not
                    // settled, so both outcomes are kept (F7).
                    None => match self.block(u).handling(Condition::Error) {
                        Some(h) if h.action == Action::Throw => {
                            self.apply(n, u, &Action::Retry(None));
                            self.apply(n, u, &Action::Throw);
                        }
                        Some(h) => {
                            let action = h.action.clone();
                            self.apply(n, u, &action);
                        }
                        None => self.apply(n, u, &Action::Retry(None)),
                    },
                }
            }
            Shape::Stop => self.raise(n, Condition::Stop, EdgeKind::Raise(Condition::Stop)),
            Shape::Quit => self.raise(n, Condition::Quit, EdgeKind::Raise(Condition::Quit)),
        }
    }

    fn no_error_text(&self, s: &Statement) -> bool {
        tokens::ends_with_no_error(self.src, s.span)
    }

    fn shape(&self, s: &'a Statement) -> Shape<'a> {
        match &s.kind {
            SK::VariableDeclaration { .. }
            | SK::DefineParameter { .. }
            | SK::DefineTempTable { .. }
            | SK::DefineBuffer { .. }
            | SK::DefineStream { .. }
            | SK::DefineFrame { .. }
            | SK::DefineWidget { .. }
            | SK::DefineDataset { .. }
            | SK::DefineDataSource { .. }
            | SK::DefineEvent { .. }
            | SK::Using { .. }
            | SK::PreprocDefine { .. }
            | SK::PreprocUndefine { .. }
            | SK::PreprocMessage { .. }
            | SK::TriggerProcedure { .. }
            | SK::Annotation(_)
            | SK::On { .. } => Shape::plain(false),
            SK::Empty => match tokens::read_control(self.src, s.span) {
                Control::Plain | Control::Directive(_) => Shape::plain(false),
                Control::Undo { label, action } => Shape::Undo {
                    label,
                    action,
                    raises: false,
                },
                Control::Quit => Shape::Quit,
                Control::Stop => Shape::Stop,
                Control::Throw => Shape::Throw,
                Control::ReturnError => Shape::ReturnError,
                Control::Unrecognized => Shape::Plain {
                    raises: false,
                    endkey: false,
                    waits: false,
                    opaque: Some(OpaqueReason::UnrecognizedStatement),
                },
            },
            SK::ExpressionStatement(_) => {
                if tokens::is_stop_statement(self.src, s.span) {
                    Shape::Stop
                } else {
                    Shape::plain(!self.no_error_text(s))
                }
            }
            SK::Assignment { .. }
            | SK::Assign { .. }
            | SK::Display { .. }
            | SK::Put { .. }
            | SK::Export { .. }
            | SK::Down { .. }
            | SK::Form { .. }
            | SK::Up { .. }
            | SK::StreamIo { .. }
            | SK::Publish { .. }
            | SK::Unsubscribe { .. } => Shape::plain(!self.no_error_text(s)),
            // Reading past the end of the input, or a key pressed during a
            // pause, raises ENDKEY.
            SK::Import { .. } | SK::Pause { .. } | SK::Update { .. } | SK::Set { .. } => {
                Shape::Plain {
                    raises: !self.no_error_text(s),
                    endkey: true,
                    waits: false,
                    opaque: None,
                }
            }
            SK::Message { set_targets, .. } => Shape::Plain {
                raises: !self.no_error_text(s),
                endkey: !set_targets.is_empty(),
                waits: false,
                opaque: None,
            },
            // A FIND NEXT/PREV past the last record ends the classic
            // `REPEAT: FIND NEXT ...` loop. Whether ABL raises ERROR or ENDKEY
            // there is not modelled, so both are possible. A FIND with a lock
            // can wait for it.
            SK::Find {
                find_type,
                no_error,
                lock_type,
                ..
            } => Shape::Plain {
                raises: !no_error,
                endkey: !no_error && matches!(find_type, FindType::Next | FindType::Prev),
                waits: *lock_type != LockType::NoLock,
                opaque: None,
            },
            SK::Run { no_error, .. } => Shape::Plain {
                raises: !no_error,
                endkey: false,
                waits: true,
                opaque: None,
            },
            SK::Create { no_error, .. }
            | SK::Delete { no_error, .. }
            | SK::DeleteObject { no_error, .. }
            | SK::Release { no_error, .. }
            | SK::Validate { no_error, .. }
            | SK::BufferCopy { no_error, .. }
            | SK::BufferCompare { no_error, .. }
            | SK::Subscribe { no_error, .. } => Shape::plain(!no_error),
            SK::Skipped { .. } => match tokens::read_control(self.src, s.span) {
                Control::Directive(_) => Shape::plain(false),
                _ => Shape::Plain {
                    raises: !self.no_error_text(s),
                    endkey: tokens::skipped_input(self.src, s.span) != Input::None,
                    waits: tokens::skipped_waits(self.src, s.span),
                    opaque: Some(OpaqueReason::SkippedStatement),
                },
            },
            SK::IncludeReference { .. } | SK::IncludeArgReference { .. } => Shape::Plain {
                raises: true,
                endkey: false,
                waits: false,
                opaque: Some(OpaqueReason::UnexpandedInclude),
            },
            SK::Leave(label) => Shape::Leave(label.as_deref()),
            SK::Next(label) => Shape::Next(label.as_deref()),
            // The parser keeps the value of `UNDO, RETURN v`, `UNDO, RETURN
            // ERROR v` and `UNDO, THROW v` as a `RETURN` or `THROW`.
            SK::Return(value) => match tokens::read_control(self.src, s.span) {
                Control::ReturnError => Shape::ReturnError,
                Control::Undo { label, action } => Shape::Undo {
                    label,
                    action,
                    raises: value.is_some(),
                },
                _ => Shape::Return {
                    raises: value.is_some(),
                },
            },
            SK::Throw(_) => match tokens::read_control(self.src, s.span) {
                Control::ReturnError => Shape::ReturnError,
                Control::Undo { label, action } => Shape::Undo {
                    label,
                    action,
                    raises: true,
                },
                _ => Shape::Throw,
            },
            // Structural statements are handled by `stmt`; anything new is
            // treated as an executable statement that may raise.
            _ => Shape::plain(true),
        }
    }
}
