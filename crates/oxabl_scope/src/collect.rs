//! The walk: definitions, buffer references, record reads and update events,
//! with the unit and block each one is in. Names are resolved later
//! ([`crate::resolve`]), once every definition of the file is known.

use oxabl_ast::{
    BufferTarget, CreateTarget, Expression, ExpressionKind as EK, NodeId, OnAction, OnKind,
    ParameterType, RunTarget, Span, Statement, StatementKind as SK, TriggerReferencing,
};
use oxabl_flow::{BlockId, BlockKind, Cfg, FileFlow, UnitId, UnitKind};
use oxabl_lexer::{Kind, Token as LexToken, is_callable_kind};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::model::{BufferKind, LockMode, ReadKind, UnknownReason, UpdateKind};

/// Lowercase ASCII copy of a name, the key of every lookup.
pub(crate) fn fold(s: &str) -> String {
    s.to_ascii_lowercase()
}

/// How a reference uses its buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Role {
    Strong,
    Weak,
    Free,
}

/// What a reference names, before resolution.
#[derive(Debug, Clone)]
pub(crate) enum What {
    /// A buffer position (`FIND b`, `FOR EACH b`, `CREATE b`): the name is a
    /// buffer, possibly database-qualified.
    Buffer(String),
    /// Dotted words in an expression: `b.f`, `db.t`, `db.t.f` (folded).
    Dotted(SmallVec<[String; 3]>),
    /// A bare word in an expression (folded): a buffer, a field, a variable
    /// or something else. A word the lexer reads as a keyword is a field only
    /// by its exact name, never as an abbreviation.
    Bare { word: String, keyword: bool },
}

/// The record phrases a bare field of a statement resolves against first.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Phrase {
    /// A reference of the statement (by index).
    Ref(u32),
    /// The private buffer of a `CAN-FIND` (index into `Collected::privates`).
    Private(u32),
}

/// Where a bare field looks for its buffer.
#[derive(Debug, Clone, Default)]
pub(crate) struct Context {
    pub phrase: SmallVec<[Phrase; 2]>,
    /// The enclosing `FOR`, `DO FOR`, `REPEAT FOR` and `PRESELECT` blocks of
    /// the unit, innermost first.
    pub enclosing: SmallVec<[BlockId; 4]>,
}

/// The form of the statement a reference is in, for transactions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Form {
    /// A statement the model reads fully.
    Known,
    /// A skipped statement of a form the model does not know: a database
    /// buffer it names makes the block's transaction Unknown.
    UnknownSkipped,
}

#[derive(Debug, Clone)]
pub(crate) struct RawRef {
    pub unit: UnitId,
    pub block: BlockId,
    /// A reference in the header of this block: it is in the block when it
    /// names one of the block's own buffers, else in `block`.
    pub header_of: Option<BlockId>,
    pub at: Span,
    pub role: Role,
    pub what: What,
    pub write: Option<UpdateKind>,
    /// `BUFFER b` passed to a call, which may change the record.
    pub passed: bool,
    pub form: Form,
    /// Where the statement starts.
    pub origin: u32,
    pub context: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct RawRead {
    pub unit: UnitId,
    pub block: BlockId,
    pub stmt: NodeId,
    pub kind: ReadKind,
    pub name: String,
    pub at: Span,
    pub lock: Option<LockMode>,
    pub target: ReadTarget,
    /// For `OPEN QUERY` and `GET`, the query (folded).
    pub query: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ReadTarget {
    /// The reference of the phrase.
    Ref(u32),
    /// The private buffer of a `CAN-FIND`.
    Private,
    /// The buffers of a query, by its position in `Collected::query_uses`.
    Query(u32),
}

#[derive(Debug, Clone)]
pub(crate) struct RawUnknown {
    pub unit: UnitId,
    pub block: BlockId,
    pub reason: UnknownReason,
    pub at: Span,
    /// Whether it also makes the scopes of the unit's buffers Unknown.
    pub scopes: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct DefBuffer {
    pub name: String,
    pub unit: UnitId,
    pub kind: BufferKind,
    /// The table as written.
    pub table: String,
    /// `FOR TEMP-TABLE tt`.
    pub temp_table: bool,
    pub at: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct DefTempTable {
    pub name: String,
    pub unit: UnitId,
    pub no_undo: bool,
    /// Folded field names.
    pub fields: Vec<String>,
    /// `LIKE t`, folded.
    pub like: Option<String>,
}

/// A query and the buffers it reads, by name.
#[derive(Debug, Clone)]
pub(crate) struct Query {
    pub unit: UnitId,
    pub name: String,
    pub buffers: SmallVec<[String; 2]>,
}

#[derive(Debug, Default)]
pub(crate) struct Collected {
    pub refs: Vec<RawRef>,
    pub contexts: Vec<Context>,
    pub reads: Vec<RawRead>,
    pub unknowns: Vec<RawUnknown>,
    pub buffers: Vec<DefBuffer>,
    pub temp_tables: Vec<DefTempTable>,
    /// `(unit, folded name)` of every variable, parameter and property.
    pub variables: Vec<(UnitId, String)>,
    /// Folded names of the `CAN-FIND` buffers, with their unit.
    pub privates: Vec<(UnitId, String)>,
    pub queries: Vec<Query>,
    /// Every `RUN`: its unit and where it starts.
    pub runs: Vec<(UnitId, u32)>,
    /// `(unit, folded name)` of the query each `GET` reads.
    pub query_uses: Vec<(UnitId, String)>,
    /// Block statements whose header names buffers: `(unit, block)` to the
    /// references of the header's own phrases (strong, weak).
    pub block_phrases: FxHashMap<(UnitId, BlockId), SmallVec<[u32; 2]>>,
}

/// A token without its value: what the scans need, cheap to copy.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Token {
    pub kind: Kind,
    pub start: usize,
    pub end: usize,
}

/// The significant tokens of the analysed text: the lexer's, without
/// comments.
pub(crate) fn significant(tokens: &[LexToken]) -> Vec<Token> {
    tokens
        .iter()
        .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof))
        .map(|t| Token {
            kind: t.kind,
            start: t.start,
            end: t.end,
        })
        .collect()
}

pub(crate) fn collect(
    program: &[Statement],
    source: &str,
    tokens: Vec<Token>,
    flow: &FileFlow,
) -> Collected {
    let mut units_of: FxHashMap<NodeId, SmallVec<[UnitId; 2]>> = FxHashMap::default();
    for cfg in flow.units().iter().skip(1) {
        units_of.entry(cfg.unit().stmt).or_default().push(cfg.id());
    }
    let mut c = Collector {
        src: source,
        tokens,
        flow,
        units_of,
        out: Collected::default(),
    };
    let main = UnitId::from_index(0);
    c.list(program, main, BlockId::ROUTINE, &mut SmallVec::new());
    c.out
}

struct Collector<'a> {
    src: &'a str,
    tokens: Vec<Token>,
    flow: &'a FileFlow,
    units_of: FxHashMap<NodeId, SmallVec<[UnitId; 2]>>,
    out: Collected,
}

/// Where a statement is, for [`Collector::scan_stmt`].
#[derive(Clone, Copy)]
struct Place {
    unit: UnitId,
    block: BlockId,
    stmt: NodeId,
    origin: u32,
    form: Form,
}

/// The parts of a statement a scan treats apart.
#[derive(Clone, Copy, Default)]
struct Spans<'c> {
    /// Spans whose first reference is a write of the given kind.
    writes: &'c [(Span, UpdateKind)],
    /// Assignment targets, attributes included.
    targets: &'c [Span],
    /// Spans already read as buffer positions.
    consumed: &'c [Span],
}

/// What a scan of a token range does with the references it finds.
struct Scan<'c> {
    unit: UnitId,
    block: BlockId,
    header_of: Option<BlockId>,
    stmt: NodeId,
    origin: u32,
    form: Form,
    write: Option<UpdateKind>,
    /// Spans whose first reference is a write of the given kind.
    writes: &'c [(Span, UpdateKind)],
    /// Spans already read as buffer positions.
    consumed: &'c [Span],
    /// Assignment targets, attributes included: where `h:BUFFER-VALUE` and
    /// `h::f` write.
    targets: &'c [Span],
    context: u32,
}

impl<'a> Collector<'a> {
    fn cfg(&self, unit: UnitId) -> &'a Cfg {
        self.flow.unit(unit)
    }

    fn text(&self, t: &Token) -> &'a str {
        self.src.get(t.start..t.end).unwrap_or("")
    }

    /// The token indices inside `span`.
    fn range(&self, span: Span) -> std::ops::Range<usize> {
        let lo = self
            .tokens
            .partition_point(|t| t.start < span.start as usize);
        let hi = self.tokens.partition_point(|t| t.start < span.end as usize);
        lo..hi.max(lo)
    }

    fn list(
        &mut self,
        stmts: &[Statement],
        unit: UnitId,
        block: BlockId,
        enclosing: &mut SmallVec<[BlockId; 4]>,
    ) {
        for s in stmts {
            self.stmt(s, unit, block, enclosing);
        }
    }

    fn stmt(
        &mut self,
        s: &Statement,
        unit: UnitId,
        block: BlockId,
        enclosing: &mut SmallVec<[BlockId; 4]>,
    ) {
        let place = Place {
            unit,
            block,
            stmt: s.id,
            origin: s.span.start,
            form: Form::Known,
        };
        match &s.kind {
            SK::Procedure {
                body, is_prototype, ..
            } => {
                if !*is_prototype {
                    self.unit_body(s, body);
                }
            }
            SK::Function { body, .. } => self.unit_body(s, body),
            SK::Method {
                body, parameters, ..
            } => {
                if let Some(&u) = self.units_of.get(&s.id).and_then(|v| v.first()) {
                    self.declare_list(parameters, u);
                }
                self.unit_body(s, body);
            }
            SK::Constructor {
                body, parameters, ..
            } => {
                if let Some(&u) = self.units_of.get(&s.id).and_then(|v| v.first()) {
                    self.declare_list(parameters, u);
                }
                self.unit_body(s, body);
            }
            SK::Destructor { body } => self.unit_body(s, body),
            SK::Property {
                name,
                get_body,
                set_body,
                set_parameters,
                ..
            } => {
                self.out.variables.push((unit, fold(&name.name)));
                let units: SmallVec<[UnitId; 2]> =
                    self.units_of.get(&s.id).cloned().unwrap_or_default();
                for u in units {
                    let body = match self.cfg(u).unit().kind {
                        UnitKind::PropertyGet => get_body.as_deref(),
                        _ => {
                            self.declare_list(set_parameters, u);
                            set_body.as_deref()
                        }
                    };
                    if let Some(body) = body {
                        self.list(body, u, BlockId::ROUTINE, &mut SmallVec::new());
                    }
                }
            }
            SK::Class { body, .. } => self.list(body, unit, block, enclosing),
            SK::Interface { .. } => {}
            SK::On { kind } => self.on(s, kind, unit),
            SK::TriggerProcedure {
                target,
                referencing,
                ..
            } => {
                let table = target.name.split('.').next().unwrap_or(&target.name);
                self.trigger_buffers(unit, table, referencing, target.span);
            }
            SK::Do { body, .. } | SK::Repeat { body, .. } | SK::ForEach { body, .. } => {
                self.block_stmt(s, body, unit, block, enclosing)
            }
            SK::Label { body, .. } => self.stmt(body, unit, block, enclosing),
            SK::Catch {
                body, error_var, ..
            } => {
                self.out.variables.push((unit, fold(&error_var.name)));
                let b = self.cfg(unit).block_of(s.id).unwrap_or(block);
                self.list(body, unit, b, enclosing);
            }
            SK::Finally { body } => {
                let b = self.cfg(unit).block_of(s.id).unwrap_or(block);
                self.list(body, unit, b, enclosing);
            }
            SK::If {
                then_branch,
                else_branch,
                ..
            } => {
                let mut children: SmallVec<[Span; 2]> = SmallVec::new();
                children.push(then_branch.span);
                if let Some(e) = else_branch {
                    children.push(e.span);
                }
                self.scan_gaps(s, &children, unit, block, enclosing);
                self.stmt(then_branch, unit, block, enclosing);
                if let Some(e) = else_branch {
                    self.stmt(e, unit, block, enclosing);
                }
            }
            SK::Case {
                when_branches,
                otherwise,
                ..
            } => {
                let mut children: SmallVec<[Span; 4]> = when_branches
                    .iter()
                    .filter_map(|w| list_span(&w.body))
                    .collect();
                children.extend(otherwise.as_deref().and_then(list_span));
                self.scan_gaps(s, &children, unit, block, enclosing);
                for w in when_branches {
                    self.list(&w.body, unit, block, enclosing);
                }
                if let Some(o) = otherwise {
                    self.list(o, unit, block, enclosing);
                }
            }
            SK::PreprocIf(p) => {
                self.list(&p.then_branch, unit, block, enclosing);
                for (_, b) in &p.elseif_branches {
                    self.list(b, unit, block, enclosing);
                }
                if let Some(e) = &p.else_branch {
                    self.list(e, unit, block, enclosing);
                }
            }
            SK::Block(list) => self.list(list, unit, block, enclosing),
            SK::VariableDeclarationList(list) => {
                for d in list {
                    self.declare(d, unit);
                }
            }
            SK::VariableDeclaration { .. }
            | SK::DefineParameter { .. }
            | SK::DefineBuffer { .. }
            | SK::DefineTempTable { .. } => self.declare(s, unit),
            SK::DefineDataset { .. }
            | SK::DefineDataSource { .. }
            | SK::DefineStream { .. }
            | SK::DefineFrame { .. }
            | SK::DefineEvent { .. }
            | SK::Using { .. }
            | SK::Annotation(_)
            | SK::PreprocDefine { .. }
            | SK::PreprocUndefine { .. }
            | SK::PreprocMessage { .. }
            | SK::Empty
            | SK::Leave(_)
            | SK::Next(_) => {}
            SK::IncludeReference { span, .. } => self.out.unknowns.push(RawUnknown {
                unit,
                block,
                reason: UnknownReason::UnexpandedInclude,
                at: *span,
                scopes: true,
            }),
            SK::IncludeArgReference { span, .. } => self.out.unknowns.push(RawUnknown {
                unit,
                block,
                reason: UnknownReason::UnexpandedInclude,
                at: *span,
                scopes: true,
            }),
            SK::Skipped { .. } => self.skipped(s, unit, block, enclosing),
            SK::Find { buffer, .. } => {
                let context = self.context(enclosing);
                let lock = self.lock_in(s.span, Some(buffer.span));
                let r = self.buffer_ref(unit, block, buffer, None, context);
                self.out.contexts[context as usize]
                    .phrase
                    .push(Phrase::Ref(r));
                self.out.reads.push(RawRead {
                    unit,
                    block,
                    stmt: s.id,
                    kind: ReadKind::Find,
                    name: buffer.name.clone(),
                    at: buffer.span,
                    lock,
                    target: ReadTarget::Ref(r),
                    query: None,
                });
                let spans = Spans {
                    consumed: &[buffer.span],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Create {
                target: CreateTarget::Name(name),
                ..
            } => {
                let context = self.context(enclosing);
                let r = self.buffer_ref(unit, block, name, Some(UpdateKind::Create), context);
                self.out.contexts[context as usize]
                    .phrase
                    .push(Phrase::Ref(r));
                let spans = Spans {
                    consumed: &[name.span],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Delete { buffer, .. } => {
                let context = self.context(enclosing);
                self.buffer_ref(unit, block, buffer, Some(UpdateKind::Delete), context);
                let spans = Spans {
                    consumed: &[buffer.span],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Release { buffer, .. } | SK::Validate { buffer, .. } => {
                let context = self.context(enclosing);
                self.buffer_ref(unit, block, buffer, None, context);
            }
            SK::BufferCopy {
                source: from,
                target,
                ..
            } => {
                let context = self.context(enclosing);
                self.buffer_ref(unit, block, from, None, context);
                let r = self.buffer_ref(unit, block, target, Some(UpdateKind::BufferCopy), context);
                self.out.contexts[context as usize]
                    .phrase
                    .push(Phrase::Ref(r));
                // The field list between the buffers names fields of the
                // source record, not references.
                let spans = Spans {
                    consumed: &[from.span.cover(target.span)],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::BufferCompare {
                source: from,
                target,
                ..
            } => {
                let context = self.context(enclosing);
                self.buffer_ref(unit, block, from, None, context);
                self.buffer_ref(unit, block, target, None, context);
                let spans = Spans {
                    consumed: &[from.span.cover(target.span)],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Run { target, .. } => {
                let context = self.context(enclosing);
                self.out.runs.push((unit, s.span.start));
                // The target names a procedure, not a record.
                let name: SmallVec<[Span; 1]> = match target {
                    RunTarget::Literal { name_span, .. } => smallvec::smallvec![*name_span],
                    RunTarget::Dynamic(_) => SmallVec::new(),
                };
                let spans = Spans {
                    consumed: &name,
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Assignment { target, .. } => {
                let context = self.context(enclosing);
                let writes: SmallVec<[(Span, UpdateKind); 1]> = write_span(target)
                    .map(|w| (w, UpdateKind::Assign))
                    .into_iter()
                    .collect();
                let spans = Spans {
                    writes: &writes,
                    targets: &[target.span],
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            SK::Assign { assignments } => {
                let context = self.context(enclosing);
                let writes: SmallVec<[(Span, UpdateKind); 4]> = assignments
                    .iter()
                    .filter_map(|p| write_span(&p.target))
                    .map(|w| (w, UpdateKind::Assign))
                    .collect();
                let targets: SmallVec<[Span; 4]> =
                    assignments.iter().map(|p| p.target.span).collect();
                let spans = Spans {
                    writes: &writes,
                    targets: &targets,
                    ..Spans::default()
                };
                self.scan_stmt(s.span, place, spans, context);
            }
            // A word the parser did not know, followed by more: a statement
            // form the model does not know either.
            SK::ExpressionStatement(e)
                if matches!(e.kind, EK::Identifier(_)) && self.has_more(e.span, s.span) =>
            {
                let context = self.context(enclosing);
                let place = Place {
                    form: Form::UnknownSkipped,
                    ..place
                };
                self.scan_stmt(s.span, place, Spans::default(), context);
            }
            _ => {
                let context = self.context(enclosing);
                self.scan_stmt(s.span, place, Spans::default(), context);
            }
        }
    }

    /// Whether tokens other than `NO-ERROR` and the period follow `inner`
    /// in `outer`.
    fn has_more(&self, inner: Span, outer: Span) -> bool {
        let rest = Span {
            start: inner.end,
            end: outer.end,
        };
        self.tokens[self.range(rest)]
            .iter()
            .any(|t| !matches!(t.kind, Kind::Period | Kind::NoError))
    }

    fn unit_body(&mut self, s: &Statement, body: &[Statement]) {
        let Some(units) = self.units_of.get(&s.id).cloned() else {
            return;
        };
        for u in units {
            self.list(body, u, BlockId::ROUTINE, &mut SmallVec::new());
        }
    }

    fn on(&mut self, s: &Statement, kind: &OnKind, unit: UnitId) {
        let body = match kind {
            OnKind::UiEvent {
                action: OnAction::Block(b),
                ..
            }
            | OnKind::DbEvent {
                action: OnAction::Block(b),
                ..
            } => b,
            _ => return,
        };
        let Some(&u) = self.units_of.get(&s.id).and_then(|v| v.first()) else {
            return;
        };
        if let OnKind::DbEvent {
            target,
            referencing,
            ..
        } = kind
        {
            let table = target.name.split('.').next().unwrap_or(&target.name);
            self.trigger_buffers(u, table, referencing, target.span);
        }
        let _ = unit;
        match &body.kind {
            SK::Do { body, .. } => self.list(body, u, BlockId::ROUTINE, &mut SmallVec::new()),
            _ => self.stmt(body, u, BlockId::ROUTINE, &mut SmallVec::new()),
        }
    }

    fn trigger_buffers(&mut self, unit: UnitId, table: &str, r: &TriggerReferencing, at: Span) {
        let mut names: SmallVec<[(&str, Span); 3]> = SmallVec::new();
        names.push((table, at));
        for b in [&r.new_buffer, &r.old_buffer].into_iter().flatten() {
            names.push((&b.name, b.span));
        }
        for (name, span) in names {
            self.out.buffers.push(DefBuffer {
                name: name.to_string(),
                unit,
                kind: BufferKind::Trigger,
                table: table.to_string(),
                temp_table: false,
                at: span,
            });
        }
    }

    fn declare_list(&mut self, stmts: &[Statement], unit: UnitId) {
        for s in stmts {
            self.declare(s, unit);
        }
    }

    fn declare(&mut self, s: &Statement, unit: UnitId) {
        match &s.kind {
            SK::VariableDeclaration { name, .. } => {
                self.out.variables.push((unit, fold(&name.name)));
            }
            SK::VariableDeclarationList(list) => self.declare_list(list, unit),
            SK::DefineParameter { param_type, .. } => match param_type {
                ParameterType::Variable { name, .. } => {
                    self.out.variables.push((unit, fold(&name.name)));
                }
                ParameterType::Buffer { name, target } => self.out.buffers.push(DefBuffer {
                    name: name.name.clone(),
                    unit,
                    kind: BufferKind::Parameter,
                    table: target.name.clone(),
                    temp_table: false,
                    at: name.span,
                }),
                ParameterType::Handle { .. } => {}
            },
            SK::DefineBuffer {
                name,
                target,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let (table, temp_table) = match target {
                    BufferTarget::Table(t) => (t.name.clone(), false),
                    BufferTarget::TempTable(t) => (t.name.clone(), true),
                };
                let kind = if *is_new_shared || *is_new_global_shared {
                    BufferKind::NewShared
                } else if *is_shared {
                    BufferKind::Shared
                } else {
                    BufferKind::Defined
                };
                self.out.buffers.push(DefBuffer {
                    name: name.name.clone(),
                    unit,
                    kind,
                    table,
                    temp_table,
                    at: name.span,
                });
            }
            SK::DefineTempTable {
                name,
                no_undo,
                like_table,
                fields,
                ..
            } => self.out.temp_tables.push(DefTempTable {
                name: name.name.clone(),
                unit,
                no_undo: *no_undo,
                fields: fields.iter().map(|f| fold(&f.name.name)).collect(),
                like: like_table.as_ref().map(|l| fold(last_segment(&l.name))),
            }),
            _ => {}
        }
    }

    fn context(&mut self, enclosing: &SmallVec<[BlockId; 4]>) -> u32 {
        self.out.contexts.push(Context {
            phrase: SmallVec::new(),
            enclosing: enclosing.iter().rev().copied().collect(),
        });
        (self.out.contexts.len() - 1) as u32
    }

    fn push_ref(&mut self, r: RawRef) -> u32 {
        self.out.refs.push(r);
        (self.out.refs.len() - 1) as u32
    }

    fn buffer_ref(
        &mut self,
        unit: UnitId,
        block: BlockId,
        name: &oxabl_ast::Identifier,
        write: Option<UpdateKind>,
        context: u32,
    ) -> u32 {
        self.push_ref(RawRef {
            unit,
            block,
            header_of: None,
            at: name.span,
            role: Role::Free,
            what: What::Buffer(name.name.clone()),
            write,
            passed: false,
            origin: name.span.start,
            form: Form::Known,
            context,
        })
    }

    /// The lock phrase of a one-buffer statement, at paren depth 0, outside
    /// `skip`.
    fn lock_in(&self, span: Span, skip: Option<Span>) -> Option<LockMode> {
        let mut depth = 0i32;
        let mut lock = None;
        for i in self.range(span) {
            let t = &self.tokens[i];
            if skip.is_some_and(|s| inside(s, t.start)) {
                continue;
            }
            match t.kind {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                _ if depth > 0 => {}
                _ => lock = lock.or(lock_kind(t.kind)),
            }
        }
        lock
    }

    // ----- blocks -----

    fn block_stmt(
        &mut self,
        s: &Statement,
        body: &[Statement],
        unit: UnitId,
        parent: BlockId,
        enclosing: &mut SmallVec<[BlockId; 4]>,
    ) {
        let cfg = self.cfg(unit);
        let b = cfg.block_of(s.id).unwrap_or(parent);
        let kind = cfg.block(b).kind;
        let header = Span {
            start: s.span.start,
            end: body
                .first()
                .map(|f| f.span.start)
                .filter(|&e| e >= s.span.start && e <= s.span.end)
                .unwrap_or(s.span.end),
        };
        let phrases = self.header(s, header, unit, parent, b, kind, enclosing);
        let pushed = !phrases.is_empty();
        if pushed {
            self.out.block_phrases.insert((unit, b), phrases);
            enclosing.push(b);
        }
        self.list(body, unit, b, enclosing);
        if pushed {
            enclosing.pop();
        }
    }

    /// Read a block header: the record phrases of a `FOR`, the buffers of
    /// `DO FOR` / `REPEAT FOR`, `PRESELECT` phrases, and the references in
    /// `WHERE`, `OF`, `WHILE`, `BY`. Returns the references of the block's
    /// own buffers.
    #[allow(clippy::too_many_arguments)]
    fn header(
        &mut self,
        s: &Statement,
        header: Span,
        unit: UnitId,
        parent: BlockId,
        b: BlockId,
        kind: BlockKind,
        enclosing: &SmallVec<[BlockId; 4]>,
    ) -> SmallVec<[u32; 2]> {
        let range = self.range(header);
        let toks: Vec<Token> = self.tokens[range.clone()].to_vec();
        let toks = toks.as_slice();
        let mut own: SmallVec<[u32; 2]> = SmallVec::new();
        let mut consumed: SmallVec<[Span; 4]> = SmallVec::new();
        let context = self.context(enclosing);
        let is_for = matches!(kind, BlockKind::For(_));
        // Record phrases: (name token index, read kind, phrase token range).
        let mut phrases: SmallVec<[(usize, ReadKind, std::ops::Range<usize>); 2]> = SmallVec::new();
        let mut strong: SmallVec<[usize; 2]> = SmallVec::new();
        let mut depth = 0i32;
        let mut i = 0;
        let mut phrase_mode: Option<ReadKind> = if is_for { Some(ReadKind::For) } else { None };
        // The first token of a FOR header is FOR; the phrase starts after it.
        if is_for {
            let at = 1 + usize::from(qualifier(toks.get(1).map(|t| t.kind)));
            if self.name_at(toks, at) {
                phrases.push((at, ReadKind::For, at..toks.len()));
            }
            i = at + 1;
        }
        while i < toks.len() {
            let k = toks[i].kind;
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                _ if depth > 0 => {}
                Kind::Comma if phrase_mode.is_some() => {
                    let next = toks.get(i + 1).map(|t| t.kind);
                    if qualifier(next) && self.name_at(toks, i + 2) {
                        if let Some(last) = phrases.last_mut() {
                            last.2.end = i;
                        }
                        let rk = phrase_mode.unwrap_or(ReadKind::For);
                        phrases.push((i + 2, rk, i + 2..toks.len()));
                        i += 3;
                        continue;
                    }
                }
                Kind::KwFor if !is_for && phrases.is_empty() => {
                    let mut j = i + 1;
                    while self.name_at(toks, j) {
                        strong.push(j);
                        j = self.skip_name(toks, j);
                        if toks.get(j).map(|t| t.kind) == Some(Kind::Comma) {
                            j += 1;
                        } else {
                            break;
                        }
                    }
                    i = j;
                    continue;
                }
                Kind::Preselect if !is_for => {
                    phrase_mode = Some(ReadKind::Preselect);
                    let at = i + 1 + usize::from(qualifier(toks.get(i + 1).map(|t| t.kind)));
                    if self.name_at(toks, at) {
                        phrases.push((at, ReadKind::Preselect, at..toks.len()));
                    }
                    i = at + 1;
                    continue;
                }
                Kind::By | Kind::KwBreak | Kind::KwWhile | Kind::On | Kind::With
                    if phrase_mode.is_some() =>
                {
                    if let Some(last) = phrases.last_mut() {
                        last.2.end = last.2.end.min(i);
                    }
                    phrase_mode = None;
                }
                _ => {}
            }
            i += 1;
        }
        for &j in &strong {
            let (name, span) = self.name_text(toks, j);
            consumed.push(span);
            let r = self.push_ref(RawRef {
                unit,
                block: b,
                header_of: None,
                at: span,
                role: Role::Strong,
                what: What::Buffer(name),
                write: None,
                passed: false,
                origin: s.span.start,
                form: Form::Known,
                context,
            });
            own.push(r);
        }
        for (j, rk, prange) in &phrases {
            let (name, span) = self.name_text(toks, *j);
            consumed.push(span);
            let lock = phrase_lock(toks, prange.clone());
            let r = self.push_ref(RawRef {
                unit,
                block: b,
                header_of: None,
                at: span,
                role: Role::Weak,
                what: What::Buffer(name.clone()),
                write: None,
                passed: false,
                origin: s.span.start,
                form: Form::Known,
                context,
            });
            own.push(r);
            self.out.reads.push(RawRead {
                unit,
                block: b,
                stmt: s.id,
                kind: *rk,
                name,
                at: span,
                lock,
                target: ReadTarget::Ref(r),
                query: None,
            });
        }
        for &r in &own {
            self.out.contexts[context as usize]
                .phrase
                .push(Phrase::Ref(r));
        }
        let scan = Scan {
            unit,
            block: parent,
            header_of: Some(b),
            stmt: s.id,
            origin: s.span.start,
            form: Form::Known,
            write: None,
            writes: &[],
            consumed: &consumed,
            targets: &[],
            context,
        };
        self.scan(range, &scan);
        own
    }

    fn name_at(&self, toks: &[Token], i: usize) -> bool {
        toks.get(i).is_some_and(|t| is_word(t.kind, self.text(t)))
    }

    /// The index after a possibly database-qualified name at `i`.
    fn skip_name(&self, toks: &[Token], i: usize) -> usize {
        let mut j = i + 1;
        while toks.get(j).map(|t| t.kind) == Some(Kind::Period)
            && adjacent(toks, j)
            && adjacent(toks, j + 1)
            && self.name_at(toks, j + 1)
        {
            j += 2;
        }
        j
    }

    fn name_text(&self, toks: &[Token], i: usize) -> (String, Span) {
        let end = self.skip_name(toks, i);
        let start = toks[i].start;
        let stop = toks[end - 1].end;
        (
            self.src[start..stop].to_string(),
            Span {
                start: start as u32,
                end: stop as u32,
            },
        )
    }

    // ----- skipped statements -----

    fn skipped(
        &mut self,
        s: &Statement,
        unit: UnitId,
        block: BlockId,
        enclosing: &mut SmallVec<[BlockId; 4]>,
    ) {
        let range = self.range(s.span);
        if range.is_empty() {
            return;
        }
        let toks: Vec<Token> = self.tokens[range.clone()].to_vec();
        let toks = toks.as_slice();
        let first = toks[0].kind;
        let second = toks.get(1).map(|t| t.kind);
        let context = self.context(enclosing);
        let (form, write) = match first {
            Kind::Define | Kind::Form => {
                self.skipped_definition(toks, unit);
                return;
            }
            Kind::Update | Kind::Set | Kind::Import => (Form::Known, Some(UpdateKind::Input)),
            Kind::Insert => (Form::Known, Some(UpdateKind::Create)),
            Kind::Open if second == Some(Kind::Query) => {
                self.open_query(s, toks, range.start, unit, block, context);
                return;
            }
            Kind::Get => {
                self.get(s, toks, unit, block, context);
                return;
            }
            Kind::Empty | Kind::Close | Kind::Reposition => return,
            k if read_only_statement(k) => (Form::Known, None),
            _ => (Form::UnknownSkipped, None),
        };
        // `COPY-LOB FROM x TO y`: what follows TO is written.
        let mut writes: SmallVec<[(Span, UpdateKind); 1]> = SmallVec::new();
        let form = if first == Kind::CopyLob {
            let to = toks.iter().position(|t| t.kind == Kind::To);
            if let Some(to) = to.and_then(|i| toks.get(i + 1)) {
                writes.push((
                    Span {
                        start: to.start as u32,
                        end: s.span.end,
                    },
                    UpdateKind::Assign,
                ));
            }
            Form::Known
        } else {
            form
        };
        let scan = Scan {
            unit,
            block,
            header_of: None,
            stmt: s.id,
            origin: s.span.start,
            form,
            write,
            writes: &writes,
            consumed: &[],
            targets: &[],
            context,
        };
        self.scan(range, &scan);
    }

    /// `DEFINE QUERY q FOR b1, b2` and `DEFINE WORK-TABLE`; other definitions
    /// the parser skips name no buffer.
    fn skipped_definition(&mut self, toks: &[Token], unit: UnitId) {
        // A definition the parser could not read still declares its name.
        if let Some(v) = toks
            .iter()
            .position(|t| matches!(t.kind, Kind::Variable | Kind::Parameter))
            && self.name_at(toks, v + 1)
            && !matches!(toks[v + 1].kind, Kind::Buffer | Kind::Table | Kind::Dataset)
        {
            self.out
                .variables
                .push((unit, fold(self.text(&toks[v + 1]))));
            return;
        }
        let Some(at) = toks
            .iter()
            .position(|t| matches!(t.kind, Kind::Query | Kind::WorkTable | Kind::Workfile))
        else {
            return;
        };
        if !self.name_at(toks, at + 1) {
            return;
        }
        let name = self.text(&toks[at + 1]).to_string();
        if toks[at].kind == Kind::Query {
            let mut buffers = SmallVec::new();
            if toks.get(at + 2).map(|t| t.kind) == Some(Kind::KwFor) {
                let mut j = at + 3;
                while self.name_at(toks, j) {
                    buffers.push(self.name_text(toks, j).0);
                    j = self.skip_name(toks, j);
                    // `FIELDS (...)` / `EXCEPT (...)` after a buffer.
                    if toks.get(j).map(|t| t.kind) == Some(Kind::LeftParen) {
                        j = close_paren(toks, j);
                    }
                    if toks.get(j).map(|t| t.kind) == Some(Kind::Comma) {
                        j += 1;
                    } else {
                        break;
                    }
                }
            }
            self.out.queries.push(Query {
                unit,
                name,
                buffers,
            });
            return;
        }
        let mut fields = Vec::new();
        let mut like = None;
        for (j, t) in toks.iter().enumerate() {
            if t.kind == Kind::Field && self.name_at(toks, j + 1) {
                fields.push(fold(self.text(&toks[j + 1])));
            }
            if t.kind == Kind::Like && j == at + 2 && self.name_at(toks, j + 1) {
                like = Some(fold(last_segment(&self.name_text(toks, j + 1).0)));
            }
        }
        self.out.temp_tables.push(DefTempTable {
            name,
            unit,
            no_undo: true,
            fields,
            like,
        });
    }

    /// `OPEN QUERY q FOR EACH b ... , EACH c ...`.
    fn open_query(
        &mut self,
        s: &Statement,
        toks: &[Token],
        base: usize,
        unit: UnitId,
        block: BlockId,
        context: u32,
    ) {
        let qname = toks.get(2).map(|t| self.text(t).to_string());
        let mut consumed: SmallVec<[Span; 4]> = SmallVec::new();
        let mut buffers: SmallVec<[String; 2]> = SmallVec::new();
        let mut phrases: SmallVec<[(usize, std::ops::Range<usize>); 2]> = SmallVec::new();
        let mut depth = 0i32;
        for i in 3..toks.len() {
            match toks[i].kind {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                _ if depth > 0 => {}
                Kind::KwFor | Kind::Preselect | Kind::Comma
                    if qualifier(toks.get(i + 1).map(|t| t.kind)) && self.name_at(toks, i + 2) =>
                {
                    if let Some(last) = phrases.last_mut() {
                        last.1.end = i;
                    }
                    phrases.push((i + 2, i + 2..toks.len()));
                }
                Kind::By | Kind::KwBreak | Kind::With => {
                    if let Some(last) = phrases.last_mut() {
                        last.1.end = last.1.end.min(i);
                    }
                }
                _ => {}
            }
        }
        for (j, prange) in &phrases {
            let (name, span) = self.name_text(toks, *j);
            consumed.push(span);
            buffers.push(name.clone());
            let r = self.push_ref(RawRef {
                unit,
                block,
                header_of: None,
                at: span,
                role: Role::Free,
                what: What::Buffer(name.clone()),
                write: None,
                passed: false,
                origin: s.span.start,
                form: Form::Known,
                context,
            });
            self.out.contexts[context as usize]
                .phrase
                .push(Phrase::Ref(r));
            self.out.reads.push(RawRead {
                unit,
                block,
                stmt: s.id,
                kind: ReadKind::OpenQuery,
                name,
                at: span,
                lock: phrase_lock(toks, prange.clone()),
                target: ReadTarget::Ref(r),
                query: qname.as_deref().map(fold),
            });
        }
        if let Some(name) = qname {
            self.out.queries.push(Query {
                unit,
                name,
                buffers,
            });
        }
        let scan = Scan {
            unit,
            block,
            header_of: None,
            stmt: s.id,
            origin: s.span.start,
            form: Form::Known,
            write: None,
            writes: &[],
            consumed: &consumed,
            targets: &[],
            context,
        };
        self.scan(base..base + toks.len(), &scan);
    }

    /// `GET [FIRST|NEXT|PREV|LAST|CURRENT] q [lock]`: a read of each buffer of
    /// the query (resolved by name later).
    fn get(&mut self, s: &Statement, toks: &[Token], unit: UnitId, block: BlockId, _c: u32) {
        let at = 1 + usize::from(matches!(
            toks.get(1).map(|t| t.kind),
            Some(Kind::First | Kind::Next | Kind::Prev | Kind::Last | Kind::Current)
        ));
        let Some(t) = toks.get(at) else {
            return;
        };
        let name = self.text(t).to_string();
        let lock = phrase_lock(toks, 0..toks.len());
        let query = Some(fold(&name));
        self.out.query_uses.push((unit, fold(&name)));
        let q = (self.out.query_uses.len() - 1) as u32;
        self.out.reads.push(RawRead {
            unit,
            block,
            stmt: s.id,
            kind: ReadKind::Get,
            name,
            at: Span {
                start: t.start as u32,
                end: t.end as u32,
            },
            lock,
            target: ReadTarget::Query(q),
            query,
        });
    }

    // ----- token scans -----

    fn scan_gaps(
        &mut self,
        s: &Statement,
        children: &[Span],
        unit: UnitId,
        block: BlockId,
        enclosing: &SmallVec<[BlockId; 4]>,
    ) {
        let context = self.context(enclosing);
        let mut sorted: SmallVec<[Span; 4]> = children.iter().copied().collect();
        sorted.sort_by_key(|c| c.start);
        let mut at = s.span.start;
        for c in sorted.iter().chain(std::iter::once(&Span {
            start: s.span.end,
            end: s.span.end,
        })) {
            if c.start > at {
                let gap = Span {
                    start: at,
                    end: c.start,
                };
                let place = Place {
                    unit,
                    block,
                    stmt: s.id,
                    origin: s.span.start,
                    form: Form::Known,
                };
                self.scan_stmt(gap, place, Spans::default(), context);
            }
            at = at.max(c.end);
        }
    }

    /// Scan a statement's own tokens in `span`.
    fn scan_stmt(&mut self, span: Span, at: Place, spans: Spans, context: u32) {
        let range = self.range(span);
        let scan = Scan {
            unit: at.unit,
            block: at.block,
            header_of: None,
            stmt: at.stmt,
            origin: at.origin,
            form: at.form,
            write: None,
            writes: spans.writes,
            consumed: spans.consumed,
            targets: spans.targets,
            context,
        };
        self.scan(range, &scan);
    }

    /// Find the references in a token range.
    fn scan(&mut self, range: std::ops::Range<usize>, sc: &Scan) {
        let mut used_writes: SmallVec<[bool; 4]> = SmallVec::from_elem(false, sc.writes.len());
        let mut depth = 0i32;
        let mut i = range.start;
        while i < range.end {
            let t = self.tokens[i];
            let k = t.kind;
            if sc.consumed.iter().any(|c| inside(*c, t.start)) {
                i += 1;
                continue;
            }
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                // The frame phrase and what follows name no record.
                Kind::With if depth <= 0 => return,
                _ => {}
            }
            if k == Kind::CanFind && self.tokens.get(i + 1).map(|t| t.kind) == Some(Kind::LeftParen)
            {
                i = self.can_find(i, range.end, sc);
                continue;
            }
            // A dynamic method that may write a record.
            if matches!(k, Kind::Colon | Kind::DoubleColon) {
                if i + 1 < range.end && self.dynamic_update(i, range.end, sc) {
                    let end = self.tokens[i + 1].end;
                    self.out.unknowns.push(RawUnknown {
                        unit: sc.unit,
                        block: sc.block,
                        reason: UnknownReason::DynamicBuffer,
                        at: Span {
                            start: t.start as u32,
                            end: end as u32,
                        },
                        scopes: false,
                    });
                }
                // Skip the attribute or method name.
                i += 2;
                continue;
            }
            if !is_word(k, self.text(&t)) {
                i += 1;
                continue;
            }
            let prev = i
                .checked_sub(1)
                .filter(|&p| p >= range.start)
                .map(|p| self.tokens[p].kind);
            if prev.is_some_and(names_no_record) {
                i = self.skip_name(&self.tokens, i);
                continue;
            }
            let end = self.skip_name(&self.tokens, i);
            // A call: a function, or a method of a class named with dots. A
            // field before an aggregate phrase (`qty (TOTAL)`) is no call.
            if self.tokens.get(end).map(|t| t.kind) == Some(Kind::LeftParen)
                && !self.aggregate_phrase(end + 1)
            {
                i = end;
                continue;
            }
            let words: SmallVec<[String; 3]> = (i..end)
                .step_by(2)
                .map(|j| fold(self.text(&self.tokens[j])))
                .collect();
            let span = Span {
                start: t.start as u32,
                end: self.tokens[end - 1].end as u32,
            };
            let mut write = sc.write;
            for (w, (ws, kind)) in sc.writes.iter().enumerate() {
                if !used_writes[w] && inside(*ws, t.start) {
                    used_writes[w] = true;
                    write = Some(*kind);
                }
            }
            let what = if words.len() == 1 {
                // A keyword is a field name only where an operand can stand,
                // and never a built-in function's.
                let target = sc.writes.iter().any(|(w, _)| w.start as usize == t.start);
                if k != Kind::Identifier
                    && (is_callable_kind(k) || !target && !prev.is_some_and(operand_before))
                {
                    i = end;
                    continue;
                }
                What::Bare {
                    word: words.into_iter().next().unwrap_or_default(),
                    keyword: k != Kind::Identifier,
                }
            } else if words.len() <= 3 {
                What::Dotted(words)
            } else {
                i = end;
                continue;
            };
            let passed = prev == Some(Kind::Buffer)
                && self.tokens.get(end).map(|t| t.kind) != Some(Kind::Colon);
            self.out.refs.push(RawRef {
                unit: sc.unit,
                block: sc.block,
                header_of: sc.header_of,
                at: span,
                role: Role::Free,
                what,
                write,
                passed,
                origin: sc.origin,
                form: sc.form,
                context: sc.context,
            });
            i = end;
        }
    }

    /// Whether the token at `i` starts an aggregate phrase: `TOTAL`, `COUNT`,
    /// `AVERAGE`, `MAXIMUM`, `MINIMUM` and their `SUB-` forms.
    fn aggregate_phrase(&self, i: usize) -> bool {
        let Some(t) = self.tokens.get(i) else {
            return false;
        };
        let word = self.text(t);
        let word = word
            .get(..4)
            .filter(|p| p.eq_ignore_ascii_case("sub-"))
            .map_or(word, |_| &word[4..]);
        ["total", "count", "average", "maximum", "minimum"]
            .iter()
            .any(|a| word.eq_ignore_ascii_case(a))
    }

    /// Whether the `:` or `::` at `i` starts a dynamic buffer access that may
    /// change a record: `h:BUFFER-CREATE()`, `h:BUFFER-DELETE()`,
    /// `h:BUFFER-COPY(...)`, `h:BUFFER-VALUE = v`, `h::f = v`, and
    /// `h:FIND-FIRST(..., EXCLUSIVE-LOCK)` and its siblings.
    fn dynamic_update(&self, i: usize, limit: usize, sc: &Scan) -> bool {
        let at = self.tokens[i].start;
        let in_write = sc.targets.iter().any(|w| inside(*w, at));
        if self.tokens[i].kind == Kind::DoubleColon {
            return in_write;
        }
        let name = self.text(&self.tokens[i + 1]);
        let is = |m: &str| name.eq_ignore_ascii_case(m);
        if is("buffer-create")
            || is("buffer-delete")
            || is("buffer-copy")
            || is("raw-transfer")
            || is("save-row-changes")
        {
            return true;
        }
        if is("buffer-value") {
            return in_write;
        }
        let finds = [
            "find-first",
            "find-last",
            "find-unique",
            "find-by-rowid",
            "find-current",
        ];
        if finds.iter().any(|m| is(m))
            && self.tokens.get(i + 2).map(|t| t.kind) == Some(Kind::LeftParen)
        {
            let close = close_paren(&self.tokens, i + 2).min(limit);
            return self.tokens[i + 2..close]
                .iter()
                .any(|t| t.kind == Kind::ExclusiveLock);
        }
        false
    }

    /// `CAN-FIND([FIRST|LAST|...] t ... [lock])` at `i`. Its own buffer is
    /// private; references to other buffers inside it are free references.
    /// Returns the index after the closing parenthesis.
    fn can_find(&mut self, i: usize, limit: usize, sc: &Scan) -> usize {
        let close = close_paren(&self.tokens, i + 1).min(limit);
        let at = i + 2 + usize::from(qualifier(self.tokens.get(i + 2).map(|t| t.kind)));
        if at >= close || !self.name_at(&self.tokens, at) {
            return close;
        }
        let (name, span) = self.name_text(&self.tokens, at);
        let after = self.skip_name(&self.tokens, at);
        let lock = phrase_lock(&self.tokens, at..close.saturating_sub(1).max(at));
        self.out.privates.push((sc.unit, fold(last_segment(&name))));
        let private = (self.out.privates.len() - 1) as u32;
        self.out.reads.push(RawRead {
            unit: sc.unit,
            block: sc.block,
            stmt: sc.stmt,
            kind: ReadKind::CanFind,
            name,
            at: span,
            lock,
            target: ReadTarget::Private,
            query: None,
        });
        let mut inner = self.out.contexts[sc.context as usize].clone();
        inner.phrase.insert(0, Phrase::Private(private));
        self.out.contexts.push(inner);
        let context = (self.out.contexts.len() - 1) as u32;
        let scan = Scan {
            unit: sc.unit,
            block: sc.block,
            header_of: sc.header_of,
            stmt: sc.stmt,
            origin: sc.origin,
            form: sc.form,
            write: None,
            writes: &[],
            consumed: sc.consumed,
            targets: sc.targets,
            context,
        };
        self.scan(after..close.saturating_sub(1).max(after), &scan);
        close
    }
}

trait Cover {
    fn cover(self, other: Span) -> Span;
}

impl Cover for Span {
    /// The span from the start of one to the end of the other.
    fn cover(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

fn inside(span: Span, at: usize) -> bool {
    (span.start as usize) <= at && at < span.end as usize
}

fn adjacent(toks: &[Token], i: usize) -> bool {
    i > 0 && matches!((toks.get(i - 1), toks.get(i)), (Some(a), Some(b)) if a.end == b.start)
}

fn list_span(list: &[Statement]) -> Option<Span> {
    Some(Span {
        start: list.first()?.span.start,
        end: list.last()?.span.end,
    })
}

/// The last dotted segment: `db.table` gives `table`.
pub(crate) fn last_segment(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// A word that may name a buffer, field or variable.
fn is_word(kind: Kind, text: &str) -> bool {
    !matches!(
        kind,
        Kind::StringLiteral
            | Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::Preprop
            | Kind::Invalid
    ) && text
        .bytes()
        .next()
        .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
}

/// `EACH`, `FIRST`, `LAST`, `NEXT`, `PREV` before a record phrase's buffer.
fn qualifier(kind: Option<Kind>) -> bool {
    matches!(
        kind,
        Some(Kind::Each | Kind::First | Kind::Last | Kind::Next | Kind::Prev)
    )
}

fn lock_kind(kind: Kind) -> Option<LockMode> {
    match kind {
        Kind::NoLock => Some(LockMode::NoLock),
        Kind::ShareLock => Some(LockMode::ShareLock),
        Kind::ExclusiveLock => Some(LockMode::ExclusiveLock),
        _ => None,
    }
}

/// The lock of a record phrase: a lock keyword at paren depth 0.
fn phrase_lock(toks: &[Token], range: std::ops::Range<usize>) -> Option<LockMode> {
    let mut depth = 0i32;
    for t in toks.get(range).unwrap_or(&[]) {
        match t.kind {
            Kind::LeftParen | Kind::LeftBracket => depth += 1,
            Kind::RightParen | Kind::RightBracket => depth -= 1,
            k if depth == 0 => {
                if let Some(m) = lock_kind(k) {
                    return Some(m);
                }
            }
            _ => {}
        }
    }
    None
}

/// The index after the parenthesis that closes the one at `open`.
fn close_paren(toks: &[Token], open: usize) -> usize {
    let mut depth = 0i32;
    let mut j = open;
    while j < toks.len() {
        match toks[j].kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => {
                depth -= 1;
                if depth == 0 {
                    return j + 1;
                }
            }
            _ => {}
        }
        j += 1;
    }
    toks.len()
}

/// The span of an assignment target that a write lands in: a field, an array
/// element, or the first argument of `ENTRY`, `SUBSTRING`, `OVERLAY` and the
/// like. An attribute (`x:SCREEN-VALUE`) is no record write.
fn write_span(target: &Expression) -> Option<Span> {
    match &target.kind {
        EK::Identifier(_) | EK::FieldAccess { .. } => Some(target.span),
        EK::ArrayAccess { array, .. } => write_span(array),
        EK::FunctionCall { arguments, .. } => arguments.first().and_then(write_span),
        _ => None,
    }
}

/// Keywords after which a word names no record: a frame, stream, query,
/// class, type, label or procedure.
fn names_no_record(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Frame
            | Kind::Stream
            | Kind::Query
            | Kind::Browse
            | Kind::Menu
            | Kind::TempTable
            | Kind::Table
            | Kind::Dataset
            | Kind::Like
            | Kind::Leave
            | Kind::Next
            | Kind::Undo
            | Kind::Retry
            | Kind::New
            | Kind::KwAs
            | Kind::Class
            | Kind::Inherits
            | Kind::Implements
            | Kind::Procedure
            | Kind::Function
            | Kind::Run
            | Kind::Format
            | Kind::Label
            | Kind::ColumnLabel
    )
}

/// Tokens after which a keyword-spelled word can be an operand (a field).
fn operand_before(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Equals
            | Kind::Eq
            | Kind::Ne
            | Kind::Lt
            | Kind::Le
            | Kind::Gt
            | Kind::Ge
            | Kind::LessThan
            | Kind::GreaterThan
            | Kind::LessThanOrEqual
            | Kind::GreaterThanOrEqual
            | Kind::NotEqual
            | Kind::Plus
            | Kind::Minus
            | Kind::Star
            | Kind::Slash
            | Kind::LeftParen
            | Kind::Comma
            | Kind::KwWhere
            | Kind::And
            | Kind::Or
            | Kind::Not
            | Kind::Then
            | Kind::KwElse
            | Kind::By
            | Kind::Begins
            | Kind::Matches
            | Kind::Contains
            | Kind::Available
            | Kind::Locked
            | Kind::Ambiguous
            | Kind::Of
            | Kind::Buffer
            | Kind::Using
    )
}

/// Statements the parser skips that read records but never change one.
fn read_only_statement(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Export
            | Kind::Put
            | Kind::Enable
            | Kind::Disable
            | Kind::PromptFor
            | Kind::Choose
            | Kind::Readkey
            | Kind::Accumulate
            | Kind::View
            | Kind::Hide
            | Kind::Clear
            | Kind::Color
            | Kind::Down
            | Kind::Up
            | Kind::Underline
            | Kind::Page
            | Kind::Pause
            | Kind::WaitFor
            | Kind::Apply
            | Kind::Status
            | Kind::Bell
            | Kind::Process
            | Kind::Quit
            | Kind::Undo
            | Kind::NextPrompt
            | Kind::OsCommand
            | Kind::Unix
            | Kind::Dos
            | Kind::Compile
            | Kind::Connect
            | Kind::Disconnect
            | Kind::SystemDialog
            | Kind::Scroll
            | Kind::Input
            | Kind::Output
            | Kind::Display
            | Kind::Message
            | Kind::KwReturn
            | Kind::Leave
            | Kind::Next
            | Kind::Release
            | Kind::Validate
            | Kind::On
            | Kind::FrameField
            | Kind::Accum
            | Kind::OsCopy
            | Kind::OsAppend
            | Kind::OsDelete
            | Kind::OsRename
            | Kind::OsCreateDir
    )
}
