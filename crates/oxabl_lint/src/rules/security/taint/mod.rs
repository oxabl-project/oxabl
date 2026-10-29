//! Stage 2 of the security rules: taint inside one procedure.
//!
//! A sink rule in taint mode reports a sink only when a value from an
//! untrusted source can reach its argument, on some path, without a sanitiser
//! that protects that kind of sink. The analysis looks at one unit (main
//! block, procedure, function, method, ...) at a time, over its control-flow
//! graph (`oxabl_flow`), with the [`Provenance`] fact.
//!
//! # Values
//!
//! A value is a set of labels `(origin, kinds)`: the source it came from and
//! the [`SinkKind`]s it is still dangerous for. A sanitiser removes kinds:
//!
//! | Code | Removes |
//! |---|---|
//! | `QUOTER(x)` | `query` |
//! | a number: `INTEGER(x)`, `DECIMAL(x)`, `LENGTH(x)`, arithmetic, ... | every kind but `size` |
//! | a logical: a comparison, `CAN-DO`, `VALID-HANDLE`, ... | every kind |
//! | `html-encode(x)` | `html` |
//! | a configured validator `v(x)` | the kinds it protects |
//!
//! A guard narrows a variable on the edge it holds on: `IF x = "lit"`, `IF
//! LOOKUP(x, "a,b") > 0`, `IF CAN-DO("a,b", x)`, `IF v(x)` for a validator,
//! with `NOT`, `AND` and `OR`, and `CASE x` on a `WHEN` whose values are all
//! literals. A literal has no labels, and an assignment of one replaces them.
//!
//! An unknown call returns the union of its receiver and arguments, and its
//! `OUTPUT` arguments take the same value.
//!
//! # Calls of the file's own routines
//!
//! A procedure, function or method of the file has a summary: what its
//! result and each `OUTPUT` parameter hold at its exit, in terms of real
//! origins and of the origins of its own parameters. A call (`RUN p (...)`,
//! `f(...)`, `m(...)`, `THIS-OBJECT:m(...)`) puts its arguments in for those,
//! so `f(pc)` is tainted when `f` returns `pc`, and `f("x")` is not. The
//! summary is solved with the parameters holding only their own origins.
//! Callers also pass their arguments to the parameters, so a `PRIVATE`
//! procedure that the file runs with untrusted input sees it; a parameter of
//! a unit that no caller outside the file reaches is a placeholder, never a
//! source of its own.
//!
//! A JSON object or array (a variable declared as a `Json...` class, or not
//! declared) takes what `o:Add`, `o:Set` and `o:Insert` give it, and
//! `o:Write(target)` puts it into `target`; a read such as `o:GetJsonText()`
//! returns it.
//!
//! `READ-JSON`, `READ-XML`, `COPY-TEMP-TABLE`, `COPY-DATASET` and the
//! `BUFFER-COPY` method fill their receiver with their arguments: the tables
//! of `TEMP-TABLE tt:`, `DATASET ds:` or `BUFFER b:`, or the handle variable
//! and the tables the file sets it to (`h = TEMP-TABLE tt:HANDLE`). The
//! `BUFFER-COPY` statement gives its target what every field of its source
//! holds.
//!
//! # Sources
//!
//! `INPUT` parameters of a unit that code outside the file can call (see
//! [`ParameterSources`]), `INPUT TABLE` and `INPUT DATASET` parameters of such
//! a unit, a `PUBLIC` or `PROTECTED` class variable or automatic property
//! setter, WebSpeed input (`get-value`, `get-field`, ...,
//! `WEB-CONTEXT:...`), a member of an object named like a request
//! (`poRequest:...`), `IMPORT`, `SET`, `UPDATE`, `COPY-LOB FROM FILE`,
//! `READ-JSON` and `READ-XML` from a file, a socket `READ`, `OS-GETENV`,
//! `SESSION:PARAMETER`, configured project sources, and database fields when
//! the options say so.
//!
//! # Variables the unit does not declare
//!
//! A variable that a unit reads and does not declare, and every field, is
//! file-level. Its value at the unit entry is the union of what every unit of
//! the file writes to it (a main block counts for its own variables), and a
//! call of a routine of the file (`RUN`, a function, a method) adds it again
//! after the call. Units, these summaries and the routine summaries are
//! solved again until nothing changes, at most [`ROUNDS`] times.
//!
//! # Unknown
//!
//! When the analysis cannot judge a sink, the rule reports it as stage 1
//! does: the operand holds an unexpanded reference, no statement of any unit
//! holds it, the unit ran out of its work budget or holds an include that did
//! not arrive, or the parse recovered.
//!
//! What it cannot judge also travels as a value, the origin `UNKNOWN` (in
//! `value`; `BUDGET`, below, for the first two), so a sink it reaches is
//! reported too: what a unit past its work budget writes to file-level slots,
//! passes to routines or returns;
//! every file-level read, routine result and parameter when the rounds end
//! before the summaries stop changing; every file-level read and parameter
//! when code outside the file may run first (in every unit of a file where
//! an include did not arrive, in the routines of an include checked on its
//! own); a variable or temp-table the file binds as `SHARED` or `NEW GLOBAL
//! SHARED`, and every shared one after a `RUN` of another program; and a
//! variable that no unit of the file declares, or a field of a table that it
//! does not define and the schema does not know, when the name may come from
//! outside the file (an include checked on its own, an include that did not
//! arrive, a class that inherits).
//!
//! What a unit past its work budget leaves, and what the rounds leave when the
//! summaries do not settle, carries its own origin, `BUDGET`: a sink rule
//! reads it as Unknown, and ABLSEC010, which drops Unknown, reports it as not
//! judged (`Gate::judge_decision`).

mod unit;
mod value;
mod witness;

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_common::{Label, ParameterSources, SecurityMode, SecurityOptions};
use oxabl_flow::FileFlow;
use oxabl_flow::dataflow::Solution;
use oxabl_flow::provenance::Provenance;
use oxabl_semantic::AnalysisContext;
use rustc_hash::FxHashSet;

use super::literal::StatementText;
use crate::rules::syntactic::file_span;
use crate::shared::RunShared;
use unit::UnitIr;
use value::{Compiler, Env, Origins, Routines, Summaries, Tables};

/// The most rounds of unit solves and file-level summaries.
pub const ROUNDS: usize = 8;

/// The kind of sink a value is dangerous for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SinkKind {
    Query,
    Sql,
    Command,
    Code,
    Path,
    Html,
    Header,
    Deserialize,
    Xml,
    Size,
    /// Not a sink: a decision that reads who the client says it is
    /// (`client-identity`). No sanitiser removes it but a validator that
    /// protects `identity`.
    Identity,
}

/// Every injection kind; [`SinkKind::Identity`] is not one.
pub const ALL: u32 = (1 << 10) - 1;

impl SinkKind {
    /// Every kind, in bit order.
    pub const EVERY: [SinkKind; 11] = [
        SinkKind::Query,
        SinkKind::Sql,
        SinkKind::Command,
        SinkKind::Code,
        SinkKind::Path,
        SinkKind::Html,
        SinkKind::Header,
        SinkKind::Deserialize,
        SinkKind::Xml,
        SinkKind::Size,
        SinkKind::Identity,
    ];

    /// The bit of the kind in a label mask.
    pub const fn bit(self) -> u32 {
        1 << self as u32
    }

    /// The name in configuration.
    pub fn name(self) -> &'static str {
        match self {
            SinkKind::Query => "query",
            SinkKind::Sql => "sql",
            SinkKind::Command => "command",
            SinkKind::Code => "code",
            SinkKind::Path => "path",
            SinkKind::Html => "html",
            SinkKind::Header => "header",
            SinkKind::Deserialize => "deserialize",
            SinkKind::Xml => "xml",
            SinkKind::Size => "size",
            SinkKind::Identity => "identity",
        }
    }

    /// The mask of a configured kind name, `all` (every injection kind)
    /// included.
    pub fn mask_of(name: &str) -> Option<u32> {
        if name.eq_ignore_ascii_case("all") {
            return Some(ALL);
        }
        Self::EVERY
            .into_iter()
            .find(|k| k.name().eq_ignore_ascii_case(name))
            .map(SinkKind::bit)
    }
}

/// [`SecurityOptions`] as the analysis reads them.
#[derive(Debug, Clone, Default)]
pub(crate) struct Options {
    parameters: ParameterSources,
    database_fields: bool,
    sources: Vec<String>,
    validators: Vec<(String, u32)>,
    identity_keys: Vec<String>,
    identity_sources: Vec<String>,
    /// The work budget of the solve of one unit.
    work: usize,
}

/// The words of a request key that make it a client-supplied identity, when
/// the options name none.
pub const IDENTITY_KEYS: [&str; 10] = [
    "user", "username", "userid", "login", "company", "tenant", "role", "roles", "owner", "admin",
];

impl Options {
    fn new(o: &SecurityOptions) -> Self {
        Options {
            parameters: o.parameters,
            database_fields: o.database_fields,
            sources: o.sources.clone(),
            validators: o
                .validators
                .iter()
                .map(|v| {
                    let mask = v
                        .protects
                        .iter()
                        .filter_map(|k| SinkKind::mask_of(k))
                        .fold(0, |a, b| a | b);
                    (v.name.clone(), mask)
                })
                .collect(),
            identity_keys: if o.identity_keys.is_empty() {
                IDENTITY_KEYS.iter().map(|k| k.to_string()).collect()
            } else {
                o.identity_keys.clone()
            },
            identity_sources: o.identity_sources.clone(),
            work: o.work_budget.unwrap_or(unit::WORK_LIMIT),
        }
    }

    /// Whether a request key names an identity: one of its words is an
    /// identity key.
    fn is_identity_key(&self, key: &str) -> bool {
        super::names::segments(key)
            .iter()
            .any(|w| self.identity_keys.iter().any(|k| k == w))
    }

    fn is_identity_source(&self, name: &str) -> bool {
        self.identity_sources
            .iter()
            .any(|s| s.eq_ignore_ascii_case(name))
    }

    fn validator(&self, name: &str) -> Option<u32> {
        self.validators
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))
            .map(|(_, m)| *m)
    }

    fn is_source(&self, name: &str) -> bool {
        self.sources.iter().any(|s| s.eq_ignore_ascii_case(name))
    }
}

/// The taint facts of one file.
pub struct FileTaint {
    units: Vec<UnitIr>,
    /// Per unit; `None` past the work budget.
    solutions: Vec<Option<Solution<Provenance>>>,
    origins: Origins,
    summaries: Summaries,
    tables: Tables,
    options: Options,
    routines: Routines,
    /// The units an include that did not arrive sits in: it may write any of
    /// their slots between any two statements.
    elided: FxHashSet<usize>,
}

/// Analyse the units of a file.
pub(crate) fn analyze(program: &[Statement], ctx: &AnalysisContext, flow: &FileFlow) -> FileTaint {
    let source = ctx.source;
    let options = Options::new(ctx.lint_severities.security());
    let stmts = unit::statements(program);
    let mut tables = unit::tables(&stmts, source);
    let fragment = ctx.source_context.is_include_fragment();
    let elided = !ctx.elided_includes.is_empty();
    tables.open = fragment
        || elided
        || program.iter().any(|s| {
            matches!(
                &s.kind,
                StatementKind::Class {
                    inherits: Some(_),
                    ..
                }
            )
        });
    if tables.open {
        tables.database = database_tables(ctx, &tables);
    }
    tables.callbacks = flow.units().len() > 1;
    let routines = routines(flow);
    let mut origins = Origins::default();
    let mut units: Vec<UnitIr> = flow
        .units()
        .iter()
        .map(|cfg| {
            unit::compile(
                cfg,
                &stmts,
                &tables,
                source,
                &options,
                &routines,
                &mut origins,
            )
        })
        .collect();
    for ir in &mut units {
        ir.outside = elided || fragment && !ir.main;
    }
    let callable: FxHashSet<u32> = routines.units().collect();
    let written: FxHashSet<String> = units
        .iter()
        .filter(|ir| !ir.main)
        .flat_map(|ir| {
            ir.ops
                .iter()
                .flatten()
                .map(|op| &ir.slots.info[op.slot as usize])
        })
        .filter(|s| s.global)
        .map(|s| s.key.clone())
        .collect();
    let elsewhere = unit::reaching_elsewhere(&units);
    for ir in &mut units {
        unit::refresh_after_calls(ir, &elsewhere, &written, &tables);
    }
    let mut summaries = Summaries::default();
    unit::member_sources(program, source, &options, &mut origins, &mut summaries);
    let mut solutions = Vec::new();
    let mut converged = false;
    for _ in 0..ROUNDS {
        let env = Env::new(&summaries);
        solutions = flow
            .units()
            .iter()
            .zip(&units)
            .map(|(cfg, ir)| unit::solve(cfg, ir, env, options.work))
            .collect();
        let mut next = summaries.clone();
        for (i, (cfg, (ir, sol))) in flow
            .units()
            .iter()
            .zip(units.iter().zip(&solutions))
            .enumerate()
        {
            let i = i as u32;
            match sol {
                Some(sol) => {
                    unit::contribute(ir, sol, &mut next);
                    unit::pass_arguments(ir, sol, env, &mut next);
                }
                None => unit::contribute_unknown(ir, &mut next),
            }
            if !callable.contains(&i) {
                continue;
            }
            // A routine's summary holds what it does with its own parameters,
            // not what one caller passes.
            let seeded = summaries.seeds.keys().any(|(u, _)| *u == i);
            let own = if seeded {
                unit::solve(
                    cfg,
                    ir,
                    Env {
                        seeds: false,
                        ..env
                    },
                    options.work,
                )
            } else {
                None
            };
            let sol = if seeded { own.as_ref() } else { sol.as_ref() };
            next.units.insert(i, unit::summarize(cfg, ir, sol));
        }
        if next == summaries {
            converged = true;
            break;
        }
        summaries = next;
    }
    if !converged {
        summaries.unconverged = true;
        let env = Env::new(&summaries);
        solutions = flow
            .units()
            .iter()
            .zip(&units)
            .map(|(cfg, ir)| unit::solve(cfg, ir, env, options.work))
            .collect();
    }
    FileTaint {
        units,
        solutions,
        origins,
        summaries,
        tables,
        options,
        routines,
        elided: units_at(flow, ctx.elided_includes),
    }
}

/// The qualifiers of `q.f` in the source that the file does not define and
/// that name a table of the schema (or the metaschema), lower-cased. A
/// qualifier that names a table counts only when every `f` read through it is
/// a field of that table: otherwise it is a buffer of another table. One that
/// abbreviates a table does not count: the includer may define it as a buffer
/// of a table with the same fields.
fn database_tables(ctx: &AnalysisContext, tables: &Tables) -> FxHashSet<String> {
    let lexed;
    let tokens = if ctx.tokens.is_empty() {
        lexed = oxabl_lexer::tokenize(ctx.source);
        &lexed[..]
    } else {
        ctx.tokens
    };
    let word = |t: &oxabl_lexer::Token| {
        t.kind != oxabl_lexer::Kind::StringLiteral
            && ctx
                .source
                .as_bytes()
                .get(t.start)
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    };
    let mut out = FxHashSet::default();
    let mut other = FxHashSet::default();
    for w in tokens.windows(3) {
        let [q, dot, f] = w else { continue };
        if !(word(q) && dot.kind == oxabl_lexer::Kind::Period && word(f))
            || q.end != dot.start
            || dot.end != f.start
        {
            continue;
        }
        let (Some(name), Some(field)) = (
            ctx.source.get(q.start..q.end),
            ctx.source.get(f.start..f.end),
        ) else {
            continue;
        };
        let name = name.to_ascii_lowercase();
        if tables.names.contains(&name) || other.contains(&name) {
            continue;
        }
        let atom = oxabl_lexer::oxabl_atom::OxablAtom::from(name.as_str());
        let known = ctx
            .schema
            .resolve_table_with_metaschema(&atom)
            .and_then(|id| ctx.schema.get_by_id(id))
            .is_some_and(|table| {
                table.display_name.eq_ignore_ascii_case(&name)
                    && matches!(
                        table.resolve_field_by_name(field),
                        oxabl_schema::FieldResolution::Unique(_)
                    )
            });
        if known {
            out.insert(name);
        } else {
            out.remove(&name);
            other.insert(name);
        }
    }
    out
}

/// The units that hold one of `offsets`: the innermost unit around each, or
/// the main block.
fn units_at(flow: &FileFlow, offsets: &[u32]) -> FxHashSet<usize> {
    offsets
        .iter()
        .map(|&at| {
            flow.units()
                .iter()
                .filter(|cfg| {
                    let span = cfg.unit().span;
                    span.start <= at && at <= span.end
                })
                .min_by_key(|cfg| {
                    let span = cfg.unit().span;
                    span.end - span.start
                })
                .map_or(0, |cfg| cfg.id().index())
        })
        .collect()
}

/// The procedures, functions and methods of a file by name.
fn routines(flow: &FileFlow) -> Routines {
    let mut r = Routines::default();
    for (i, cfg) in flow.units().iter().enumerate() {
        let unit = cfg.unit();
        let Some(name) = &unit.name else {
            continue;
        };
        let map = match unit.kind {
            oxabl_flow::UnitKind::Procedure => &mut r.procedures,
            oxabl_flow::UnitKind::Function => &mut r.functions,
            oxabl_flow::UnitKind::Method => &mut r.methods,
            _ => continue,
        };
        map.entry(name.to_ascii_lowercase())
            .or_default()
            .push(i as u32);
    }
    r
}

/// What reaches a sink operand.
pub(crate) enum Reach {
    Clean,
    /// The analysis cannot judge it.
    Unknown,
    /// The analysis did not finish it: the unit that holds it, or one whose
    /// value reaches it, ran past its work budget, or the rounds ended before
    /// the summaries converged. See `value::BUDGET`.
    Unjudged,
    /// The path from the source, source first.
    Tainted(Vec<witness::Step>),
}

impl FileTaint {
    /// What reaches any of `operands` for a sink of `kind`. A `decision`
    /// reads the operands of logical functions too.
    pub(crate) fn reach(
        &self,
        flow: &FileFlow,
        source: &str,
        operands: &[Span],
        kind: SinkKind,
        decision: bool,
    ) -> Reach {
        let mut tainted = None;
        for &op in operands {
            match self.reach_one(flow, source, op, kind, decision) {
                r @ (Reach::Unknown | Reach::Unjudged) => return r,
                Reach::Clean => {}
                t @ Reach::Tainted(_) => {
                    tainted.get_or_insert(t);
                }
            }
        }
        tainted.unwrap_or(Reach::Clean)
    }

    fn reach_one(
        &self,
        flow: &FileFlow,
        source: &str,
        operand: Span,
        kind: SinkKind,
        decision: bool,
    ) -> Reach {
        // The innermost statement over every unit: a class statement is a
        // node of the file's main block and holds every method.
        let found = flow
            .units()
            .iter()
            .filter_map(|cfg| {
                let nodes = cfg.nodes_at(operand.start);
                let span = cfg.node(*nodes.first()?).span;
                Some((span.end - span.start, cfg, nodes))
            })
            .min_by_key(|(len, ..)| *len);
        let Some((_, cfg, nodes)) = found else {
            return Reach::Unknown;
        };
        let u = cfg.id().index();
        if self.elided.contains(&u) {
            return Reach::Unknown;
        }
        let (Some(sol), ir) = (&self.solutions[u], &self.units[u]) else {
            return Reach::Unjudged;
        };
        let Some(st) = StatementText::over(source, operand) else {
            return Reach::Unknown;
        };
        let mut slots = ir.slots.clone();
        let mut origins = Origins::on_top_of(&self.origins);
        let mut value = Compiler {
            options: &self.options,
            tables: &self.tables,
            slots: &mut slots,
            origins: &mut origins,
            declared: &ir.declared,
            frozen: true,
            routines: &self.routines,
            calls: Vec::new(),
            foreign: Vec::new(),
            decision,
        }
        .tokens(&st, 0..st.len());
        value.expand_tables(&ir.slots);
        let env = Env::new(&self.summaries).outside(ir.outside);
        let mut fact = Provenance::new(ir.slots.len());
        for &n in &nodes {
            let mut f = sol.before(n).clone();
            // A weak write inside the operand is a call there: what it writes
            // may be read after it.
            for op in ir.ops[n.index()].iter().filter(|op| {
                op.span.end <= operand.start
                    || !op.strong && op.span.start >= operand.start && op.span.start < operand.end
            }) {
                unit::apply(op, &mut f, env);
            }
            fact.join(&f);
        }
        let (labels, unknown) = value.eval(&fact, env);
        if unknown {
            return Reach::Unknown;
        }
        let real = |o: u32| {
            self.origins
                .get(o)
                .or_else(|| origins.get(o))
                .is_some_and(|x| !x.placeholder)
        };
        let Some(&(origin, _)) = labels.iter().find(|(o, m)| m & kind.bit() != 0 && real(*o))
        else {
            let held = |origin: u32| {
                labels
                    .iter()
                    .any(|(o, m)| *o == origin && m & kind.bit() != 0)
            };
            return if held(value::BUDGET) {
                Reach::Unjudged
            } else if held(value::UNKNOWN) {
                Reach::Unknown
            } else {
                Reach::Clean
            };
        };
        Reach::Tainted(witness::path(
            self,
            cfg,
            &nodes,
            operand.start,
            &value,
            &fact,
            origin,
            &origins,
        ))
    }
}

/// What a rule reports for one sink, or nothing.
pub(crate) struct Judgement {
    /// Where the value comes from: "a non-literal value", or "untrusted
    /// input: ... → ...".
    pub subject: String,
    /// The path, source first.
    pub labels: Vec<Label>,
    /// The taint analysis found a source, so the subject names it.
    pub tainted: bool,
    /// The path in words, source first: "web input `get-value` → `c`".
    pub chain: String,
    /// The untrusted source: where it is, and what it is.
    pub origin: Option<(Span, String)>,
    /// The analysis did not finish the value: past a work budget.
    pub unjudged: bool,
}

impl Judgement {
    fn stage_one() -> Self {
        Judgement {
            subject: "a non-literal value".to_string(),
            labels: Vec::new(),
            tainted: false,
            chain: String::new(),
            origin: None,
            unjudged: false,
        }
    }

    /// A decision the analysis did not finish.
    fn unjudged() -> Self {
        Judgement {
            subject: "a value the analysis did not finish".to_string(),
            unjudged: true,
            ..Judgement::stage_one()
        }
    }

    /// Add the path to a finding.
    pub fn attach(self, mut d: oxabl_common::Diagnostic) -> oxabl_common::Diagnostic {
        d.labels.extend(self.labels);
        d
    }
}

/// Decides, for one rule, whether a stage 1 finding is reported.
pub(crate) struct Gate<'g> {
    program: &'g [Statement],
    ctx: &'g AnalysisContext<'g>,
    shared: &'g RunShared<'g>,
    taint: bool,
}

impl<'g> Gate<'g> {
    /// The gate of the rule `code` (`name`) under the context's options.
    pub fn new(
        code: &str,
        name: &str,
        program: &'g [Statement],
        ctx: &'g AnalysisContext<'g>,
        shared: &'g RunShared<'g>,
    ) -> Self {
        let taint = ctx.lint_severities.security().mode_for(code, name) == SecurityMode::Taint;
        Gate {
            program,
            ctx,
            shared,
            taint,
        }
    }

    /// Judge a stage 1 finding whose argument text is at `operands`. With no
    /// operand, or in stage 1 mode, the finding stands.
    pub fn judge(&self, operands: &[Span], kind: SinkKind) -> Option<Judgement> {
        if !self.taint || operands.is_empty() || self.ctx.parse_recovered {
            return Some(Judgement::stage_one());
        }
        let elided = |s: &Span| {
            let i = self.ctx.elided.partition_point(|&e| e < s.start);
            self.ctx.elided.get(i).is_some_and(|&e| e <= s.end)
        };
        if operands.iter().any(elided) {
            return Some(Judgement::stage_one());
        }
        match self.reach(operands, kind, false) {
            Reach::Clean => None,
            Reach::Unknown | Reach::Unjudged => Some(Judgement::stage_one()),
            Reach::Tainted(steps) => Some(self.tainted(&steps)),
        }
    }

    /// Judge a decision whose operands are at `operands`: the path when a
    /// value of `kind` reaches them, or [`Judgement::unjudged`] when the
    /// analysis did not finish a value that reaches them (a work budget), so
    /// the budget hides no decision. A decision the analysis cannot judge for
    /// another reason is not reported; neither is any in a file whose parse
    /// recovered.
    pub fn judge_decision(&self, operands: &[Span], kind: SinkKind) -> Option<Judgement> {
        if operands.is_empty() || self.ctx.parse_recovered {
            return None;
        }
        match self.reach(operands, kind, true) {
            Reach::Tainted(steps) => Some(self.tainted(&steps)),
            Reach::Unjudged => Some(Judgement::unjudged()),
            Reach::Clean | Reach::Unknown => None,
        }
    }

    fn reach(&self, operands: &[Span], kind: SinkKind, decision: bool) -> Reach {
        let flow = self.shared.flow(self.program, self.ctx);
        let taint = self.shared.taint(self.program, self.ctx);
        taint.reach(flow, self.ctx.source, operands, kind, decision)
    }

    fn tainted(&self, steps: &[witness::Step]) -> Judgement {
        let mut chain: Vec<&str> = Vec::new();
        for s in steps {
            if chain.last() != Some(&s.chain.as_str()) {
                chain.push(&s.chain);
            }
        }
        let chain = chain.join(" → ");
        let origin = steps
            .first()
            .filter(|s| s.label.starts_with("untrusted source:"))
            .and_then(|s| s.span.map(|span| (span, s.chain.clone())));
        Judgement {
            origin,
            unjudged: false,
            tainted: true,
            subject: format!("untrusted input: {chain}"),
            chain,
            labels: steps
                .iter()
                .filter_map(|s| {
                    s.span.map(|span| Label {
                        span: file_span(self.ctx, span),
                        message: s.label.clone(),
                    })
                })
                .collect(),
        }
    }
}
