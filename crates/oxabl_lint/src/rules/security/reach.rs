//! Stage 3 of the security rules: how an entry point reaches a finding.
//!
//! The call graph spans the files an entry point reaches, so a client that
//! reads the workspace builds it (`oxabl_pipeline`), from the [`FileFacts`] of
//! each file, and answers for the file under analysis through
//! [`SecurityReach`]. This module owns what one file says about itself, so
//! the client and the rules read the same facts:
//!
//! - its routines (units) and the calls each makes, with the class of a
//!   call into an object where the file declares it;
//! - the **gates** in each routine: a call of an `auth-routines` entry or an
//!   include of one, and a write of a piece of session state. A site is
//!   behind a gate when a gate site dominates it in the routine's CFG;
//! - the **sensitive operations**: database writes (temp-tables excluded) and
//!   calls of `sensitive-calls` entries;
//! - the **session state** it declares and touches: `NEW GLOBAL SHARED`
//!   variables (and their `SHARED` consumers), and `STATIC` variables and
//!   properties of a class (by name in the class, as `Cls:name` elsewhere).
//!
//! Every finding of a `security` rule is labelled in `annotate`, which the
//! registry calls: the status, the entries, one path, and a note on the
//! message. With `raise-reachable` or `lower-unreachable` the severity moves
//! one level.
//!
//! # Matching a routine name
//!
//! An `auth-routines` or `sensitive-calls` entry matches, without regard to
//! case, a procedure, function or method name, a program as `RUN` writes it
//! (with or without its extension, or by its last path segments), an include
//! file by its path or file name, and a method of a class as `Cls:method` or
//! `pkg.Cls:method`.

use oxabl_ast::{
    AccessModifier, Expression, ExpressionKind, ParameterDirection, Span, Statement, StatementKind,
};
use oxabl_common::{Diagnostic, ReachPath, ReachStatus, Reachability, SecurityOptions, Severity};
use oxabl_flow::{Cfg, FileFlow, NodeIx, UnitKind};
use oxabl_index::calls::{CallKind, CallTarget, object_calls};
use oxabl_semantic::AnalysisContext;

use super::literal::StatementText;
use crate::rules::syntactic::{Visitor, walk_block};
use crate::shared::RunShared;

/// Reachability answers for the file under analysis, in expanded byte
/// coordinates.
pub trait SecurityReach {
    /// How the unit whose span starts at `start` is reached. A unit the graph
    /// does not know has the file's status.
    fn unit(&self, start: u32) -> &Reachability;

    /// A path from an entry point to the unit that passes no authorisation.
    fn unauthorised(&self, start: u32) -> Option<&ReachPath>;

    /// A path from an entry point to the unit that writes the session state
    /// `key` nowhere before, when code reached from the entry points writes
    /// `key`.
    fn unreset(&self, start: u32, key: &str) -> Option<&ReachPath>;
}

/// What one file says about its routines.
#[derive(Debug, Clone, Default)]
pub struct FileFacts {
    /// By unit index of the file's flow.
    pub units: Vec<UnitFacts>,
    /// The class or interface the file defines, as written.
    pub class: Option<String>,
    /// Its `INHERITS`, as written.
    pub inherits: Option<String>,
    /// The session state the file declares (`NEW GLOBAL SHARED`, `STATIC`).
    pub states: Vec<State>,
    /// The classes whose static members it names (`Cls:member`), as
    /// written: the file of each declares the state.
    pub classes: Vec<String>,
}

/// One piece of session state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct State {
    /// `global:name` or `static:class:name`, lower-cased.
    pub key: String,
    /// As written.
    pub name: String,
}

/// One routine.
#[derive(Debug, Clone)]
pub struct UnitFacts {
    pub kind: UnitKind,
    pub name: Option<String>,
    pub span: Span,
    /// `PRIVATE`: an entry point on its class does not run it.
    pub private: bool,
    pub calls: Vec<SiteFacts>,
    pub sensitive: Vec<Sensitive>,
    pub reads: Vec<Access>,
    pub writes: Vec<Access>,
}

/// One call of a routine.
#[derive(Debug, Clone)]
pub struct SiteFacts {
    /// As [`oxabl_index::calls::CallSite::at`].
    pub at: Span,
    pub kind: CallKind,
    pub target: SiteTarget,
    /// A gate of `auth-routines` comes before the call on every path.
    pub authorised: bool,
    /// The session state keys written before the call on every path.
    pub resets: Vec<String>,
}

/// What a call calls, as the file names it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SiteTarget {
    Call(CallTarget),
    /// A method of a class (`None`: its constructor).
    Object {
        class: String,
        method: Option<String>,
    },
}

/// One sensitive operation.
#[derive(Debug, Clone)]
pub struct Sensitive {
    pub at: Span,
    /// `create Customer`, `call of post-ledger`.
    pub what: String,
    pub authorised: bool,
}

/// One read or write of session state.
#[derive(Debug, Clone)]
pub struct Access {
    pub key: String,
    pub name: String,
    pub at: Span,
    /// For a read: a write of the same key comes before it on every path.
    pub reset: bool,
}

/// Whether `pattern` (an `auth-routines` or `sensitive-calls` entry) names
/// one of `names`.
pub fn names_routine(pattern: &str, names: &[&str]) -> bool {
    let norm = |s: &str| s.trim().replace('\\', "/").to_ascii_lowercase();
    let stem = |s: &str| -> String {
        match s.rsplit_once('.') {
            Some((a, b)) if !b.contains('/') && !b.contains(':') && b.len() <= 3 => a.to_string(),
            _ => s.to_string(),
        }
    };
    let p = norm(pattern);
    if p.is_empty() {
        return false;
    }
    names.iter().any(|n| {
        let n = norm(n);
        n == p
            || stem(&n) == p
            || stem(&n) == stem(&p)
            || n.ends_with(&format!("/{p}"))
            || stem(&n).ends_with(&format!("/{}", stem(&p)))
    })
}

/// The names a call answers to for [`names_routine`].
fn site_names(t: &SiteTarget) -> Vec<String> {
    match t {
        SiteTarget::Call(CallTarget::Procedure(n))
        | SiteTarget::Call(CallTarget::Function(n))
        | SiteTarget::Call(CallTarget::Method(n))
        | SiteTarget::Call(CallTarget::Program(n))
        | SiteTarget::Call(CallTarget::Library(n)) => vec![n.clone()],
        SiteTarget::Call(CallTarget::InProgram { program, procedure }) => {
            vec![procedure.clone(), format!("{program}:{procedure}")]
        }
        SiteTarget::Call(CallTarget::Unknown { name, .. }) => name.iter().cloned().collect(),
        SiteTarget::Object { class, method } => {
            let short = class.rsplit('.').next().unwrap_or(class);
            match method {
                Some(m) => vec![m.clone(), format!("{class}:{m}"), format!("{short}:{m}")],
                None => vec![class.clone(), short.to_string()],
            }
        }
    }
}

fn matches_any(patterns: &[String], names: &[String]) -> bool {
    let names: Vec<&str> = names.iter().map(String::as_str).collect();
    patterns.iter().any(|p| names_routine(p, &names))
}

/// The innermost unit that holds offset `at`, with the nodes there.
pub fn unit_at(flow: &FileFlow, at: u32) -> Option<(usize, Vec<NodeIx>)> {
    flow.units()
        .iter()
        .enumerate()
        .filter_map(|(i, cfg)| {
            let nodes = cfg.nodes_at(at);
            let span = cfg.node(*nodes.first()?).span;
            Some((span.end - span.start, i, nodes))
        })
        .min_by_key(|(len, ..)| *len)
        .map(|(_, i, nodes)| (i, nodes))
}

/// The innermost unit whose span holds offset `at`, by its span alone: for a
/// finding that no statement node holds.
fn unit_spanning(flow: &FileFlow, at: u32) -> Option<usize> {
    flow.units()
        .iter()
        .enumerate()
        .filter(|(_, c)| c.unit().span.start <= at && at < c.unit().span.end)
        .min_by_key(|(_, c)| c.unit().span.end - c.unit().span.start)
        .map(|(i, _)| i)
}

/// Gate offsets of one unit, and whether one of them comes before a site on
/// every path.
struct Gates<'c> {
    cfg: &'c Cfg,
    dom: Option<oxabl_flow::dominators::Dominators>,
}

impl<'c> Gates<'c> {
    fn new(cfg: &'c Cfg) -> Self {
        Gates { cfg, dom: None }
    }

    /// Whether the gate at offset `gate` comes before offset `site` on every
    /// path through the unit.
    fn before(&mut self, gate: u32, site: u32) -> bool {
        let g = self.cfg.nodes_at(gate);
        let s = self.cfg.nodes_at(site);
        if g.is_empty() || s.is_empty() {
            return false;
        }
        let dom = self.dom.get_or_insert_with(|| self.cfg.dominators());
        s.iter().all(|&sn| {
            g.iter().any(|&gn| {
                if gn == sn {
                    gate < site
                } else {
                    dom.dominates(gn, sn)
                }
            })
        })
    }

    fn any_before(&mut self, gates: &[u32], site: u32) -> bool {
        gates.iter().any(|&g| self.before(g, site))
    }
}

/// The facts of a file.
pub fn file_facts(
    program: &[Statement],
    ctx: &AnalysisContext,
    shared: &RunShared,
    options: &SecurityOptions,
) -> FileFacts {
    let flow = shared.flow(program, ctx);
    let tokens = shared.tokens(ctx);
    let reach = &options.reach;
    let mut out = FileFacts {
        units: flow
            .units()
            .iter()
            .map(|cfg| UnitFacts {
                kind: cfg.unit().kind,
                name: cfg.unit().name.clone(),
                span: cfg.unit().span,
                private: false,
                calls: Vec::new(),
                sensitive: Vec::new(),
                reads: Vec::new(),
                writes: Vec::new(),
            })
            .collect(),
        ..FileFacts::default()
    };
    let mut privacy = Privacy {
        source: ctx.source,
        private: Vec::new(),
    };
    walk_block(&mut privacy, program);
    for (u, cfg) in flow.units().iter().enumerate() {
        out.units[u].private = privacy.private.iter().any(|s| *s == cfg.unit().span.start);
    }
    for s in program {
        if let StatementKind::Class { name, inherits, .. } = &s.kind {
            out.class = Some(name.name.clone());
            out.inherits = inherits.as_ref().map(|i| i.name.clone());
            break;
        }
        if let StatementKind::Interface { name, .. } = &s.kind {
            out.class = Some(name.name.clone());
            break;
        }
    }

    // Calls: the call graph's sites, with the class of an object call.
    let mut sites: Vec<(Span, CallKind, SiteTarget)> = shared
        .calls(program, ctx)
        .sites
        .iter()
        .map(|s| (s.at, s.kind, SiteTarget::Call(s.target.clone())))
        .collect();
    for oc in object_calls(program, ctx.source, tokens) {
        let target = SiteTarget::Object {
            class: oc.class,
            method: oc.method,
        };
        match sites.iter_mut().find(|(at, ..)| *at == oc.at) {
            Some(site) => site.2 = target,
            None => sites.push((oc.at, CallKind::Call, target)),
        }
    }
    sites.sort_by_key(|(at, ..)| (at.start, at.end));

    // Gates and sensitive calls.
    let mut auth: Vec<Vec<u32>> = vec![Vec::new(); flow.units().len()];
    for (at, _, target) in &sites {
        if matches_any(&reach.auth_routines, &site_names(target))
            && let Some((u, _)) = unit_at(flow, at.start)
        {
            auth[u].push(at.start);
        }
    }
    for (at, path) in shared.includes() {
        let path = path.to_string_lossy();
        if !reach
            .auth_routines
            .iter()
            .any(|p| names_routine(p, &[&path]))
        {
            continue;
        }
        // The include's first statement: its text may start with a comment.
        let i = tokens.partition_point(|t| (t.start as u32) < *at);
        let Some(first) = tokens[i..]
            .iter()
            .find(|t| t.kind != oxabl_lexer::Kind::Comment)
            .map(|t| t.start as u32)
        else {
            continue;
        };
        if let Some((u, _)) = unit_at(flow, first) {
            auth[u].push(first);
        }
    }

    // Session state.
    let states = declared_states(program, ctx.source, out.class.as_deref());
    out.states = states
        .iter()
        .filter(|(_, _, declares)| *declares)
        .map(|(key, name, _)| State {
            key: key.clone(),
            name: name.clone(),
        })
        .collect();
    let mut accesses = Accesses {
        source: ctx.source,
        names: states
            .iter()
            .map(|(k, n, _)| (n.to_ascii_lowercase(), k.clone(), n.clone()))
            .collect(),
        locals: local_names(program),
        writes: Vec::new(),
        found: Vec::new(),
        classes: Vec::new(),
    };
    walk_block(&mut accesses, program);
    out.classes = std::mem::take(&mut accesses.classes);
    let mut resets: Vec<Vec<(String, u32)>> = vec![Vec::new(); flow.units().len()];
    let found = std::mem::take(&mut accesses.found);
    for (key, name, at, write) in found {
        let Some(u) = unit_at(flow, at.start)
            .map(|(u, _)| u)
            .or_else(|| unit_spanning(flow, at.start))
        else {
            continue;
        };
        let access = Access {
            key: key.clone(),
            name,
            at,
            reset: false,
        };
        if write {
            resets[u].push((key, at.start));
            out.units[u].writes.push(access);
        } else {
            out.units[u].reads.push(access);
        }
    }

    // Each site with its gates.
    for (u, cfg) in flow.units().iter().enumerate() {
        let mut gates = Gates::new(cfg);
        let unit = &mut out.units[u];
        for read in &mut unit.reads {
            let writes: Vec<u32> = resets[u]
                .iter()
                .filter(|(k, _)| *k == read.key)
                .map(|(_, at)| *at)
                .collect();
            read.reset = gates.any_before(&writes, read.at.start);
        }
        for (at, kind, target) in &sites {
            if unit_at(flow, at.start).map(|(i, _)| i) != Some(u) {
                continue;
            }
            let mut keys: Vec<String> = Vec::new();
            for (k, w) in &resets[u] {
                if !keys.contains(k) && gates.before(*w, at.start) {
                    keys.push(k.clone());
                }
            }
            let authorised = gates.any_before(&auth[u], at.start);
            if matches_any(&reach.sensitive_calls, &site_names(target)) {
                unit.sensitive.push(Sensitive {
                    at: *at,
                    what: format!("call of `{}`", site_names(target)[0]),
                    authorised,
                });
            }
            unit.calls.push(SiteFacts {
                at: *at,
                kind: *kind,
                target: target.clone(),
                authorised,
                resets: keys,
            });
        }
    }

    // Database writes.
    for e in shared.effects(program, ctx).effects() {
        if !e.op.writes() {
            continue;
        }
        let table = match &e.table {
            oxabl_scope::Table::Database(n) | oxabl_scope::Table::Unknown(n) => n,
            oxabl_scope::Table::TempTable { .. } => continue,
        };
        let u = e.unit.index();
        let Some(cfg) = flow.units().get(u) else {
            continue;
        };
        let authorised = Gates::new(cfg).any_before(&auth[u], e.statement.start);
        out.units[u].sensitive.push(Sensitive {
            at: e.statement,
            what: format!("{} `{table}`", e.op.as_str()),
            authorised,
        });
    }
    for unit in &mut out.units {
        unit.sensitive.sort_by_key(|s| s.at.start);
    }
    out
}

/// The unit spans of `PRIVATE` procedures, functions and methods.
struct Privacy<'a> {
    source: &'a str,
    private: Vec<u32>,
}

impl Visitor for Privacy<'_> {
    fn statement(&mut self, s: &Statement) -> bool {
        let private = match &s.kind {
            StatementKind::Method { access, .. } | StatementKind::Constructor { access, .. } => {
                *access == AccessModifier::Private
            }
            StatementKind::Procedure { .. } | StatementKind::Function { .. } => {
                header_has(self.source, s.span, "PRIVATE")
            }
            _ => false,
        };
        if private {
            self.private.push(s.span.start);
        }
        true
    }
}

/// Whether the header of a definition, up to its `(`, `:` or `.`, holds
/// `word`.
fn header_has(source: &str, span: Span, word: &str) -> bool {
    let Some(st) = StatementText::over(source, span) else {
        return false;
    };
    (0..st.len())
        .take_while(|&i| {
            !st.is(i, oxabl_lexer::Kind::LeftParen)
                && !st.is(i, oxabl_lexer::Kind::Colon)
                && !st.is(i, oxabl_lexer::Kind::Period)
        })
        .any(|i| st.is_word(i, word))
}

/// The session state the file names: (key, name as written, whether this
/// file declares it).
fn declared_states(
    program: &[Statement],
    source: &str,
    class: Option<&str>,
) -> Vec<(String, String, bool)> {
    struct Decls<'a> {
        source: &'a str,
        class: Option<String>,
        out: Vec<(String, String, bool)>,
    }
    impl Visitor for Decls<'_> {
        fn statement(&mut self, s: &Statement) -> bool {
            match &s.kind {
                StatementKind::VariableDeclaration {
                    name,
                    is_shared,
                    is_new_global_shared,
                    ..
                } => {
                    if *is_new_global_shared || *is_shared {
                        self.out.push((
                            format!("global:{}", name.name.to_ascii_lowercase()),
                            name.name.clone(),
                            *is_new_global_shared,
                        ));
                    } else if let Some(c) = &self.class
                        && header_has(self.source, s.span, "STATIC")
                    {
                        self.out
                            .push((static_key(c, &name.name), name.name.clone(), true));
                    }
                }
                StatementKind::Property {
                    name,
                    is_static: true,
                    ..
                } => {
                    if let Some(c) = &self.class {
                        self.out
                            .push((static_key(c, &name.name), name.name.clone(), true));
                    }
                }
                _ => {}
            }
            true
        }
    }
    let mut d = Decls {
        source,
        class: class.map(str::to_string),
        out: Vec::new(),
    };
    walk_block(&mut d, program);
    d.out
}

/// The key of a static member of a class.
pub fn static_key(class: &str, member: &str) -> String {
    let short = class.rsplit('.').next().unwrap_or(class);
    format!(
        "static:{}:{}",
        short.to_ascii_lowercase(),
        member.to_ascii_lowercase()
    )
}

/// Variables, parameters and buffers the file declares, lower-cased: a name
/// before `:` that is one of these is not a class.
fn local_names(program: &[Statement]) -> Vec<String> {
    struct Names(Vec<String>);
    impl Visitor for Names {
        fn statement(&mut self, s: &Statement) -> bool {
            match &s.kind {
                StatementKind::VariableDeclaration { name, .. }
                | StatementKind::DefineBuffer { name, .. }
                | StatementKind::DefineTempTable { name, .. } => {
                    self.0.push(name.name.to_ascii_lowercase())
                }
                StatementKind::DefineParameter {
                    param_type:
                        oxabl_ast::ParameterType::Variable { name, .. }
                        | oxabl_ast::ParameterType::Handle { name, .. }
                        | oxabl_ast::ParameterType::Buffer { name, .. },
                    ..
                } => self.0.push(name.name.to_ascii_lowercase()),
                _ => {}
            }
            true
        }
    }
    let mut n = Names(Vec::new());
    walk_block(&mut n, program);
    n.0
}

/// Reads and writes of session state.
struct Accesses<'a> {
    source: &'a str,
    /// (lower-cased name, key, name as written) of the state the file names
    /// by itself.
    names: Vec<(String, String, String)>,
    locals: Vec<String>,
    /// Spans of the expressions statements write.
    writes: Vec<Span>,
    /// (key, name, at, write).
    found: Vec<(String, String, Span, bool)>,
    /// The classes named before `:`.
    classes: Vec<String>,
}

impl Accesses<'_> {
    fn source_of(&self, span: Span) -> Option<&str> {
        self.source.get(span.start as usize..span.end as usize)
    }

    fn target(&mut self, e: &Expression) {
        match &e.kind {
            ExpressionKind::ArrayAccess { array, .. } => self.target(array),
            _ => self.writes.push(e.span),
        }
    }
}

impl Visitor for Accesses<'_> {
    fn statement(&mut self, s: &Statement) -> bool {
        match &s.kind {
            StatementKind::Assignment { target, .. } => self.target(target),
            StatementKind::Assign { assignments } => {
                for p in assignments {
                    self.target(&p.target);
                }
            }
            StatementKind::Run { arguments, .. } => {
                for a in arguments {
                    if a.direction != ParameterDirection::Input {
                        self.target(&a.expression);
                    }
                }
            }
            StatementKind::Set { targets, .. }
            | StatementKind::Update { targets, .. }
            | StatementKind::Import { targets, .. } => {
                for t in targets {
                    self.target(t);
                }
            }
            _ => {}
        }
        true
    }

    fn expression(&mut self, e: &Expression) {
        let (key, name) = match &e.kind {
            ExpressionKind::Identifier(id) => {
                let lower = id.name.to_ascii_lowercase();
                match self.names.iter().find(|(n, ..)| *n == lower) {
                    Some((_, k, n)) => (k.clone(), n.clone()),
                    None => return,
                }
            }
            ExpressionKind::MemberAccess { object, member } => {
                let (class, root) = match &object.kind {
                    ExpressionKind::Identifier(c) => (c.name.as_str(), c.name.to_ascii_lowercase()),
                    ExpressionKind::FieldAccess { field, .. } => {
                        (field.name.as_str(), String::new())
                    }
                    _ => return,
                };
                if self.locals.contains(&root) || is_system_handle(class) {
                    return;
                }
                let written = self.source_of(object.span).unwrap_or(class).to_string();
                if !self.classes.contains(&written) {
                    self.classes.push(written);
                }
                (static_key(class, &member.name), member.name.clone())
            }
            _ => return,
        };
        let write = self.writes.contains(&e.span);
        self.found.push((key, name, e.span, write));
    }
}

fn is_system_handle(name: &str) -> bool {
    [
        "THIS-OBJECT",
        "THIS-PROCEDURE",
        "SESSION",
        "ERROR-STATUS",
        "WEB-CONTEXT",
        "SUPER",
        "SOURCE-PROCEDURE",
        "TARGET-PROCEDURE",
        "LOG-MANAGER",
        "FILE-INFO",
        "COMPILER",
        "SECURITY-POLICY",
    ]
    .iter()
    .any(|h| h.eq_ignore_ascii_case(name))
}

/// One level more severe.
fn raised(s: Severity) -> Severity {
    match s {
        Severity::Hint => Severity::Info,
        Severity::Info => Severity::Warning,
        _ => Severity::Error,
    }
}

/// One level less severe.
fn lowered(s: Severity) -> Severity {
    match s {
        Severity::Error => Severity::Warning,
        Severity::Warning => Severity::Info,
        _ => Severity::Hint,
    }
}

/// Label a finding of a `security` rule with how it is reached, when the
/// run has reachability answers.
pub(crate) fn annotate(
    d: &mut Diagnostic,
    program: &[Statement],
    ctx: &AnalysisContext,
    shared: &RunShared,
) {
    let Some(reach) = shared.reach() else {
        return;
    };
    let flow = shared.flow(program, ctx);
    let at = d.span.span.start;
    let start = unit_at(flow, at)
        .map(|(u, _)| u)
        .or_else(|| unit_spanning(flow, at))
        .map(|u| flow.units()[u].unit().span.start)
        .unwrap_or(0);
    let r = reach.unit(start).clone();
    let options = &ctx.lint_severities.security().reach;
    match r.status {
        ReachStatus::Reachable if options.raise_reachable => d.severity = raised(d.severity),
        ReachStatus::Unreachable if options.lower_unreachable => d.severity = lowered(d.severity),
        _ => {}
    }
    d.message = format!("{} ({r})", d.message);
    d.reach = Some(Box::new(r));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_routine_name_matches_programs_procedures_methods_and_includes() {
        assert!(names_routine("check-access", &["CHECK-ACCESS"]));
        assert!(names_routine("auth/check.p", &["auth/check.p"]));
        assert!(names_routine("auth/check", &["auth/check.p"]));
        assert!(names_routine("auth/check.p", &["auth\\check"]));
        assert!(names_routine("check.p", &["src/auth/check.p"]));
        assert!(names_routine("auth/guard.i", &["/ws/src/auth/guard.i"]));
        assert!(names_routine(
            "Guard:Require",
            &["app.security.Guard:Require", "Guard:Require"]
        ));
        assert!(!names_routine("check", &["recheck"]));
        assert!(!names_routine("auth/check.p", &["other/check.p"]));
        assert!(!names_routine("", &["x"]));
    }
}
