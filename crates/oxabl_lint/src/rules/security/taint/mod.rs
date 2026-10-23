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
//! | `html-encode(x)`, `url-encode(x)` | `html`, `header` |
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
//! # Sources
//!
//! `INPUT` parameters of a unit that code outside the file can call (see
//! [`ParameterSources`]), WebSpeed input (`get-value`, `get-field`, ...,
//! `WEB-CONTEXT:...`), a member of an object named like a request
//! (`poRequest:...`), `IMPORT`, `SET`, `UPDATE`, `COPY-LOB FROM FILE`, a socket
//! `READ`, `OS-GETENV`, `SESSION:PARAMETER`, configured project sources, and
//! database fields when the options say so.
//!
//! # Variables the unit does not declare
//!
//! A variable that a unit reads and does not declare, and every field, is
//! file-level. Its value at the unit entry is the union of what every unit of
//! the file writes to it (a main block counts for its own variables). Units
//! and these summaries are solved again until nothing changes, at most
//! [`ROUNDS`] times.
//!
//! # Unknown
//!
//! When the analysis cannot judge a sink, the rule reports it as stage 1
//! does: the operand holds an unexpanded reference, no statement of any unit
//! holds it, the unit ran out of its work budget, or the parse recovered.

mod unit;
mod value;
mod witness;

use oxabl_ast::{Span, Statement};
use oxabl_common::{Label, ParameterSources, SecurityMode, SecurityOptions};
use oxabl_flow::FileFlow;
use oxabl_flow::dataflow::Solution;
use oxabl_flow::provenance::Provenance;
use oxabl_semantic::AnalysisContext;

use super::literal::StatementText;
use crate::rules::syntactic::file_span;
use crate::shared::RunShared;
use unit::UnitIr;
use value::{Compiler, Origins, Summaries, Tables};

/// The most rounds of unit solves and file-level summaries.
pub const ROUNDS: usize = 4;

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
}

/// Every kind.
pub const ALL: u32 = (1 << 10) - 1;

impl SinkKind {
    /// Every kind, in bit order.
    pub const EVERY: [SinkKind; 10] = [
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
        }
    }

    /// The mask of a configured kind name, `all` included.
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
}

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
        }
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
}

/// Analyse the units of a file.
pub(crate) fn analyze(
    program: &[Statement],
    source: &str,
    flow: &FileFlow,
    options: &SecurityOptions,
) -> FileTaint {
    let options = Options::new(options);
    let stmts = unit::statements(program);
    let tables = unit::tables(&stmts);
    let mut origins = Origins::default();
    let units: Vec<UnitIr> = flow
        .units()
        .iter()
        .map(|cfg| unit::compile(cfg, &stmts, &tables, source, &options, &mut origins))
        .collect();
    let mut summaries = Summaries::default();
    let mut solutions = Vec::new();
    for _ in 0..ROUNDS {
        solutions = flow
            .units()
            .iter()
            .zip(&units)
            .map(|(cfg, ir)| unit::solve(cfg, ir, &summaries))
            .collect();
        let mut next = summaries.clone();
        for (ir, sol) in units.iter().zip(&solutions) {
            if let Some(sol) = sol {
                unit::contribute(ir, sol, &mut next);
            }
        }
        if next == summaries {
            break;
        }
        summaries = next;
    }
    FileTaint {
        units,
        solutions,
        origins,
        summaries,
        tables,
        options,
    }
}

/// What reaches a sink operand.
pub(crate) enum Reach {
    Clean,
    /// The analysis cannot judge it.
    Unknown,
    /// The path from the source, source first.
    Tainted(Vec<witness::Step>),
}

impl FileTaint {
    /// What reaches any of `operands` for a sink of `kind`.
    pub(crate) fn reach(
        &self,
        flow: &FileFlow,
        source: &str,
        operands: &[Span],
        kind: SinkKind,
    ) -> Reach {
        let mut tainted = None;
        for &op in operands {
            match self.reach_one(flow, source, op, kind) {
                Reach::Unknown => return Reach::Unknown,
                Reach::Clean => {}
                t @ Reach::Tainted(_) => {
                    tainted.get_or_insert(t);
                }
            }
        }
        tainted.unwrap_or(Reach::Clean)
    }

    fn reach_one(&self, flow: &FileFlow, source: &str, operand: Span, kind: SinkKind) -> Reach {
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
        let (Some(sol), ir) = (&self.solutions[u], &self.units[u]) else {
            return Reach::Unknown;
        };
        let Some(st) = StatementText::over(source, operand) else {
            return Reach::Unknown;
        };
        let mut slots = ir.slots.clone();
        let mut origins = Origins::on_top_of(&self.origins);
        let value = Compiler {
            options: &self.options,
            tables: &self.tables,
            slots: &mut slots,
            origins: &mut origins,
            declared: &ir.declared,
            frozen: Some(&self.summaries),
        }
        .tokens(&st, 0..st.len());
        let mut fact = Provenance::new(ir.slots.len());
        for &n in &nodes {
            let mut f = sol.before(n).clone();
            for op in ir.ops[n.index()]
                .iter()
                .filter(|op| op.span.end <= operand.start)
            {
                unit::apply(op, &mut f);
            }
            fact.join(&f);
        }
        let (labels, unknown) = value.eval(&fact);
        if unknown {
            return Reach::Unknown;
        }
        let Some(&(origin, _)) = labels.iter().find(|(_, m)| m & kind.bit() != 0) else {
            return Reach::Clean;
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
}

impl Judgement {
    fn stage_one() -> Self {
        Judgement {
            subject: "a non-literal value".to_string(),
            labels: Vec::new(),
            tainted: false,
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
        let flow = self.shared.flow(self.program, self.ctx);
        let taint = self.shared.taint(self.program, self.ctx);
        match taint.reach(flow, self.ctx.source, operands, kind) {
            Reach::Clean => None,
            Reach::Unknown => Some(Judgement::stage_one()),
            Reach::Tainted(steps) => {
                let mut chain: Vec<&str> = Vec::new();
                for s in &steps {
                    if chain.last() != Some(&s.chain.as_str()) {
                        chain.push(&s.chain);
                    }
                }
                Some(Judgement {
                    tainted: true,
                    subject: format!("untrusted input: {}", chain.join(" → ")),
                    labels: steps
                        .iter()
                        .filter_map(|s| {
                            s.span.map(|span| Label {
                                span: file_span(self.ctx, span),
                                message: s.label.clone(),
                            })
                        })
                        .collect(),
                })
            }
        }
    }
}
