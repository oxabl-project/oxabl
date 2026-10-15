//! The static call graph.
//!
//! [`FileCalls::extract`] lists the calls one parsed file makes and what each
//! one calls, as far as the file shows it: a `RUN` of an internal procedure,
//! of another program, of an internal procedure of a program the file runs
//! persistently (`RUN p IN h`), and calls of the file's own user-defined
//! functions. What the file cannot tell is [`CallTarget::Unknown`] with a
//! reason, never a guess: `RUN VALUE(...)`, a handle the file does not set
//! with `RUN ... PERSISTENT SET`, a super procedure, `DYNAMIC-FUNCTION`,
//! `PUBLISH`, and a method call on an object or handle.
//!
//! [`resolve_program`] turns a program name into a file over the search path
//! through the run index ([`WorkspaceIndex::program`]), and [`CallGraph`] holds
//! the resolved edges between routines of many files, with their strongly
//! connected components for a fixed point over the graph.
//!
//! The semantics, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 8.

use oxabl_ast::{
    DataType, Expression, ExpressionKind, OnAction, OnKind, ParameterDirection, ParameterType,
    RunTarget, Span, Statement, StatementKind, TypeSource,
};
use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{IndexAnswer, IndexName, IndexedFileId, WorkspaceIndex};

/// Call sites past this many in one file are dropped, and the file is marked
/// [`FileCalls::truncated`].
pub const MAX_SITES: usize = 20_000;

/// Edges past this many in one [`CallGraph`] are dropped, and the graph is
/// marked [`CallGraph::truncated`].
pub const MAX_EDGES: usize = 2_000_000;

/// Why a call's target is not known.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnknownCall {
    /// `RUN VALUE(...)`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`.
    Dynamic,
    /// `RUN p IN h` (or a function declared `IN h`) where the file does not
    /// set `h` only with `RUN ... PERSISTENT SET h` of one program.
    Handle,
    /// `RUN SUPER`, `SUPER(...)` in a function, or a routine declared
    /// `IN SUPER`: the super procedures are added at run time.
    Super,
    /// `PUBLISH`: the subscribers are known at run time.
    Event,
    /// A method call on an object or a handle.
    Method,
    /// The target is spelled with text the preprocessor did not expand.
    Unexpanded,
    /// No file on the search path supplies the program.
    NotFound,
    /// A file supplies the program but cannot be used (unreadable, its parse
    /// recovered, or it is the file being analysed).
    Unusable,
    /// Several files match the program name.
    Ambiguous,
    /// The program was found, but not the internal procedure the call names.
    NoRoutine,
}

impl UnknownCall {
    /// Kebab-case name used in dumps and reports.
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownCall::Dynamic => "dynamic",
            UnknownCall::Handle => "handle",
            UnknownCall::Super => "super",
            UnknownCall::Event => "event",
            UnknownCall::Method => "method",
            UnknownCall::Unexpanded => "unexpanded",
            UnknownCall::NotFound => "not-found",
            UnknownCall::Unusable => "unusable",
            UnknownCall::Ambiguous => "ambiguous",
            UnknownCall::NoRoutine => "no-routine",
        }
    }
}

/// What a call site calls.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallTarget {
    /// An internal procedure of the file, with a body (also `RUN p IN
    /// THIS-PROCEDURE`).
    Procedure(String),
    /// A user-defined function of the file, with a body.
    Function(String),
    /// A method of the class the file defines, called on `THIS-OBJECT` or
    /// without a receiver.
    Method(String),
    /// Another program, by the name written (`RUN x.p`, `RUN dir/x`).
    Program(String),
    /// An internal procedure of another program: `RUN procedure IN h`, where
    /// every `PERSISTENT SET h` of the file runs `program`.
    InProgram { program: String, procedure: String },
    /// An entry point of a shared library (`PROCEDURE p EXTERNAL "lib"`): no
    /// database effects.
    Library(String),
    /// Not known.
    Unknown {
        /// The name written, when there is one.
        name: Option<String>,
        reason: UnknownCall,
    },
}

/// How a call runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallKind {
    /// `RUN`.
    Run,
    /// `RUN ... PERSISTENT`: the program's main block runs now, and its
    /// internal procedures stay callable.
    Persistent,
    /// `RUN ... ASYNCHRONOUS` or `RUN ... ON SERVER h`: another session runs
    /// the callee, outside the caller's transaction.
    Remote,
    /// A function or method call in an expression.
    Call,
    /// `PUBLISH`.
    Publish,
}

/// One call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallSite {
    /// The `RUN` or `PUBLISH` statement, or the name of the called function
    /// or method.
    pub at: Span,
    pub kind: CallKind,
    pub target: CallTarget,
}

/// The calls of one file, and the routines they may name.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FileCalls {
    /// Every call, in source order.
    pub sites: Vec<CallSite>,
    /// Internal procedures with a body, as written.
    pub procedures: Vec<String>,
    /// User-defined functions with a body, as written.
    pub functions: Vec<String>,
    /// More than [`MAX_SITES`] calls: the rest are not listed.
    pub truncated: bool,
}

impl FileCalls {
    /// The calls of a parsed file. `tokens` are the tokens `program` was
    /// parsed from (`oxabl_lexer::tokenize(source)`).
    pub fn extract(program: &[Statement], source: &str, tokens: &[Token]) -> FileCalls {
        let mut decls = Decls::default();
        walk(program, &mut |s| decls.statement(s, source, tokens));
        let mut out = FileCalls {
            procedures: decls.procedures.clone(),
            functions: decls.functions.clone(),
            ..FileCalls::default()
        };
        walk(program, &mut |s| {
            if let Some(site) = run_site(s, tokens, &decls) {
                out.sites.push(site);
            }
        });
        expression_calls(source, tokens, &decls, &mut out.sites);
        out.sites.sort_by_key(|s| (s.at.start, s.at.end));
        if out.sites.len() > MAX_SITES {
            out.sites.truncate(MAX_SITES);
            out.truncated = true;
        }
        out
    }
}

/// The names the file declares that a call may name.
#[derive(Default)]
struct Decls {
    procedures: Vec<String>,
    functions: Vec<String>,
    methods: Vec<String>,
    /// Procedures declared `EXTERNAL`.
    libraries: Vec<String>,
    /// Procedures and functions declared `IN h` or `IN SUPER`, with whether
    /// it is `SUPER`.
    delegated: Vec<(String, bool)>,
    /// Handles set by `RUN program PERSISTENT SET h`: (folded name, program).
    persistent: Vec<(String, String)>,
    /// Handles written any other way (folded).
    other_writes: Vec<String>,
    /// Names declared `AS HANDLE`, `WIDGET-HANDLE` or `COM-HANDLE`, and
    /// `TABLE-HANDLE` / `DATASET-HANDLE` parameters (folded).
    handles: Vec<String>,
    /// Where routine definitions write their names: not calls.
    definitions: Vec<u32>,
}

/// Whether a declared type is a handle.
fn is_handle(t: &TypeSource) -> bool {
    matches!(t, TypeSource::Explicit(DataType::Handle | DataType::Com))
}

impl Decls {
    fn statement(&mut self, s: &Statement, source: &str, tokens: &[Token]) {
        match &s.kind {
            StatementKind::Procedure {
                name,
                is_prototype,
                is_external,
                ..
            } => {
                if *is_external {
                    self.libraries.push(name.name.clone());
                } else if *is_prototype {
                    let sup = delegated_to_super(source, tokens, s.span);
                    self.delegated.push((name.name.clone(), sup));
                } else {
                    self.procedures.push(name.name.clone());
                }
            }
            StatementKind::Function { name, body, .. } => {
                if !body.is_empty() {
                    self.functions.push(name.name.clone());
                } else if let Some(sup) = prototype_in(source, tokens, s.span) {
                    self.delegated.push((name.name.clone(), sup));
                } else if !self.functions.iter().any(|f| same(f, &name.name)) {
                    // A FORWARD declaration, or a definition with no statements.
                    self.functions.push(name.name.clone());
                }
            }
            StatementKind::Method { name, .. } => {
                self.methods.push(name.name.clone());
                self.definitions.push(name.span.start);
            }
            StatementKind::Run {
                target: RunTarget::Literal { name, .. },
                persistent_handle: Some(h),
                ..
            } => {
                if let Some(hn) = handle_name(h) {
                    self.persistent.push((hn, name.clone()));
                }
            }
            StatementKind::Run {
                persistent_handle: Some(h),
                ..
            } => {
                if let Some(hn) = handle_name(h) {
                    self.other_writes.push(hn);
                }
            }
            _ => {}
        }
        match &s.kind {
            StatementKind::VariableDeclaration {
                name, type_source, ..
            } if is_handle(type_source) => self.handles.push(name.name.to_ascii_lowercase()),
            StatementKind::DefineParameter {
                param_type:
                    ParameterType::Variable {
                        name, type_source, ..
                    },
                ..
            } if is_handle(type_source) => self.handles.push(name.name.to_ascii_lowercase()),
            StatementKind::DefineParameter {
                param_type: ParameterType::Handle { name, .. },
                ..
            } => self.handles.push(name.name.to_ascii_lowercase()),
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields.iter().filter(|f| is_handle(&f.type_source)) {
                    self.handles.push(f.name.name.to_ascii_lowercase());
                }
            }
            StatementKind::Property {
                name,
                data_type: DataType::Handle | DataType::Com,
                ..
            } => self.handles.push(name.name.to_ascii_lowercase()),
            _ => {}
        }
        // Any other write to a name: an assignment, an OUTPUT argument, a
        // parameter or shared definition.
        match &s.kind {
            StatementKind::Assignment { target, .. } => self.write(target),
            StatementKind::Assign { assignments } => {
                for p in assignments {
                    self.write(&p.target);
                }
            }
            StatementKind::Run { arguments, .. } => {
                for a in arguments {
                    if matches!(
                        a.direction,
                        ParameterDirection::Output | ParameterDirection::InputOutput
                    ) {
                        self.write(&a.expression);
                    }
                }
            }
            StatementKind::VariableDeclaration {
                name,
                is_shared,
                is_new_shared,
                is_new_global_shared,
                ..
            } if *is_shared || *is_new_shared || *is_new_global_shared => {
                self.other_writes.push(name.name.to_ascii_lowercase());
            }
            StatementKind::DefineParameter { .. } => {
                if let Some(name) = parameter_name(source, tokens, s.span) {
                    self.other_writes.push(name);
                }
            }
            _ => {}
        }
    }

    fn write(&mut self, target: &Expression) {
        if let Some(n) = handle_name(target) {
            self.other_writes.push(n);
        }
    }

    /// The program every `PERSISTENT SET` of the handle runs, when there is
    /// exactly one and nothing else writes the handle.
    fn handle_program(&self, handle: &str) -> Option<&str> {
        if self.other_writes.iter().any(|w| w == handle) {
            return None;
        }
        let mut programs = self
            .persistent
            .iter()
            .filter(|(h, _)| h == handle)
            .map(|(_, p)| p.as_str());
        let first = programs.next()?;
        programs.all(|p| same(p, first)).then_some(first)
    }
}

fn same(a: &str, b: &str) -> bool {
    a.eq_ignore_ascii_case(b)
}

/// The folded name of a plain variable expression.
fn handle_name(e: &Expression) -> Option<String> {
    match &e.kind {
        ExpressionKind::Identifier(id) => Some(id.name.to_ascii_lowercase()),
        _ => None,
    }
}

/// Significant tokens (no comments) within `span`.
fn tokens_in(tokens: &[Token], span: Span) -> impl Iterator<Item = &Token> {
    let from = tokens.partition_point(|t| t.start < span.start as usize);
    tokens[from..]
        .iter()
        .take_while(move |t| t.end <= span.end as usize && t.kind != Kind::Eof)
        .filter(|t| t.kind != Kind::Comment)
}

fn text<'a>(source: &'a str, t: &Token) -> &'a str {
    source.get(t.start..t.end).unwrap_or("")
}

/// For a procedure prototype: whether it is declared `IN SUPER`.
fn delegated_to_super(source: &str, tokens: &[Token], span: Span) -> bool {
    prototype_in(source, tokens, span).unwrap_or(false)
}

/// For a routine prototype: `Some(true)` for `IN SUPER`, `Some(false)` for
/// `IN h`, `None` when it has no `IN` phrase at the top level.
fn prototype_in(source: &str, tokens: &[Token], span: Span) -> Option<bool> {
    let mut depth = 0i32;
    let mut it = tokens_in(tokens, span);
    while let Some(t) = it.next() {
        match t.kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => depth -= 1,
            Kind::KwIn if depth == 0 => {
                return Some(it.next().is_some_and(|n| same(text(source, n), "super")));
            }
            _ => {}
        }
    }
    None
}

/// The folded name a `DEFINE PARAMETER` statement defines: the word after
/// `PARAMETER`.
fn parameter_name(source: &str, tokens: &[Token], span: Span) -> Option<String> {
    let mut it = tokens_in(tokens, span);
    it.find(|t| t.kind == Kind::Parameter)?;
    let t = it.next()?;
    (t.kind != Kind::Buffer && t.kind != Kind::Table && t.kind != Kind::TableHandle)
        .then(|| text(source, t).to_ascii_lowercase())
}

/// The call a `RUN` or `PUBLISH` statement makes.
fn run_site(s: &Statement, tokens: &[Token], decls: &Decls) -> Option<CallSite> {
    if let StatementKind::Run {
        target,
        in_handle,
        persistent,
        asynchronous,
        ..
    } = &s.kind
    {
        let kind = if *asynchronous || on_server(tokens, s.span) {
            CallKind::Remote
        } else if *persistent {
            CallKind::Persistent
        } else {
            CallKind::Run
        };
        let target = match (target, in_handle) {
            (RunTarget::Dynamic(_), _) => CallTarget::Unknown {
                name: None,
                reason: UnknownCall::Dynamic,
            },
            (RunTarget::Literal { name, .. }, _) if name.contains('{') => CallTarget::Unknown {
                name: Some(name.clone()),
                reason: UnknownCall::Unexpanded,
            },
            (RunTarget::Literal { name, .. }, Some(h)) => {
                let this = tokens_in(tokens, h.span)
                    .next()
                    .is_some_and(|t| t.kind == Kind::ThisProcedure);
                if this {
                    internal(name, decls)
                } else {
                    match handle_name(h).and_then(|hn| decls.handle_program(&hn)) {
                        Some(program) => CallTarget::InProgram {
                            program: program.to_string(),
                            procedure: name.clone(),
                        },
                        None => CallTarget::Unknown {
                            name: Some(name.clone()),
                            reason: UnknownCall::Handle,
                        },
                    }
                }
            }
            (RunTarget::Literal { name, .. }, None) if same(name, "super") => CallTarget::Unknown {
                name: None,
                reason: UnknownCall::Super,
            },
            (RunTarget::Literal { name, .. }, None) => internal(name, decls),
        };
        return Some(CallSite {
            at: s.span,
            kind,
            target,
        });
    }
    let first = tokens_in(tokens, s.span).next()?;
    (first.kind == Kind::Publish).then_some(CallSite {
        at: s.span,
        kind: CallKind::Publish,
        target: CallTarget::Unknown {
            name: None,
            reason: UnknownCall::Event,
        },
    })
}

/// A `RUN name` without a handle: an internal procedure of the file, a
/// prototype, or another program.
fn internal(name: &str, decls: &Decls) -> CallTarget {
    if decls.procedures.iter().any(|p| same(p, name)) {
        return CallTarget::Procedure(name.to_string());
    }
    if decls.libraries.iter().any(|p| same(p, name)) {
        return CallTarget::Library(name.to_string());
    }
    if let Some((_, sup)) = decls.delegated.iter().find(|(p, _)| same(p, name)) {
        return CallTarget::Unknown {
            name: Some(name.to_string()),
            reason: if *sup {
                UnknownCall::Super
            } else {
                UnknownCall::Handle
            },
        };
    }
    CallTarget::Program(name.to_string())
}

/// Whether a `RUN` has an `ON [SERVER] h` phrase naming another session.
/// `ON [SERVER] SESSION` is the local session.
fn on_server(tokens: &[Token], span: Span) -> bool {
    let mut depth = 0i32;
    let mut it = tokens_in(tokens, span);
    while let Some(t) = it.next() {
        match t.kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => depth -= 1,
            Kind::On if depth == 0 => {
                let mut next = it.next().map(|t| t.kind);
                if next == Some(Kind::Server) {
                    next = it.next().map(|t| t.kind);
                }
                let after = it.next().map(|t| t.kind);
                return next != Some(Kind::Session) || after == Some(Kind::Colon);
            }
            _ => {}
        }
    }
    false
}

/// Receivers whose methods never touch the database: system handles.
fn harmless_receiver(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::ErrorStatus
            | Kind::Session
            | Kind::FileInfo
            | Kind::Compiler
            | Kind::LogManager
            | Kind::SecurityPolicy
            | Kind::Clipboard
            | Kind::Debugger
            | Kind::Profiler
            | Kind::RcodeInformation
            | Kind::LastEvent
            | Kind::Focus
            | Kind::ActiveWindow
            | Kind::CurrentWindow
            | Kind::DefaultWindow
            | Kind::Frame
            | Kind::Browse
            | Kind::KwSelf
    )
}

/// System handles the lexer reads as plain names.
const SYSTEM_HANDLE_WORDS: [&str; 8] = [
    "source-procedure",
    "target-procedure",
    "web-context",
    "color-table",
    "font-table",
    "audit-control",
    "audit-policy",
    "rcode-info",
];

/// What a method is called on.
enum Receiver {
    /// A handle: a system handle, a variable, parameter, field or property
    /// declared as one, or a `BUFFER`, `QUERY`, `FRAME`, ... reference. Its
    /// methods are built in.
    Handle,
    /// `THIS-OBJECT`.
    This,
    /// Anything else: an object, or a name the file does not declare.
    Object,
}

/// The root of the receiver chain that ends at the colon at `colon`:
/// `h:BUFFER-FIELD("f"):BUFFER-VALUE(` has the root `h`.
fn receiver(source: &str, toks: &[&Token], colon: usize, decls: &Decls) -> Receiver {
    let mut j = colon;
    // Walk back over `name`, `name(...)`, `[...]` and `:` / `::` links.
    loop {
        if j == 0 {
            return Receiver::Object;
        }
        j -= 1;
        if matches!(toks[j].kind, Kind::RightParen | Kind::RightBracket) {
            let mut depth = 0i32;
            loop {
                match toks[j].kind {
                    Kind::RightParen | Kind::RightBracket => depth += 1,
                    Kind::LeftParen | Kind::LeftBracket => depth -= 1,
                    _ => {}
                }
                if depth == 0 || j == 0 {
                    break;
                }
                j -= 1;
            }
            if j == 0 {
                return Receiver::Object;
            }
            j -= 1;
        }
        let t = toks[j];
        let linked = j > 0
            && matches!(toks[j - 1].kind, Kind::Colon | Kind::DoubleColon)
            && toks[j - 1].end == t.start;
        if linked {
            j -= 1;
            continue;
        }
        if j > 0
            && matches!(
                toks[j - 1].kind,
                Kind::Buffer
                    | Kind::Query
                    | Kind::Frame
                    | Kind::Browse
                    | Kind::TempTable
                    | Kind::Dataset
                    | Kind::DataSource
                    | Kind::Menu
                    | Kind::SubMenu
                    | Kind::MenuItem
            )
        {
            return Receiver::Handle;
        }
        if t.kind == Kind::ThisObject {
            return Receiver::This;
        }
        if harmless_receiver(t.kind) || t.kind == Kind::ThisProcedure {
            return Receiver::Handle;
        }
        let name = text(source, t).to_ascii_lowercase();
        if SYSTEM_HANDLE_WORDS.contains(&name.as_str()) {
            return Receiver::Handle;
        }
        let field = name.rsplit('.').next().unwrap_or(&name);
        if decls.handles.iter().any(|h| h == &name || h == field) {
            return Receiver::Handle;
        }
        return Receiver::Object;
    }
}

/// Function and method calls in expressions, found in the tokens: a name of
/// a function of the file, `DYNAMIC-FUNCTION(`, `DYNAMIC-INVOKE(`,
/// `DYNAMIC-NEW`, `SUPER(` in a function, and `x:name(`.
fn expression_calls(source: &str, tokens: &[Token], decls: &Decls, out: &mut Vec<CallSite>) {
    let toks: Vec<&Token> = tokens
        .iter()
        .take_while(|t| t.kind != Kind::Eof)
        .filter(|t| t.kind != Kind::Comment)
        .collect();
    let class = !decls.methods.is_empty();
    for i in 0..toks.len() {
        let t = toks[i];
        let next = toks.get(i + 1).map(|n| n.kind);
        let prev = i.checked_sub(1).map(|p| toks[p].kind);
        let span = Span {
            start: t.start as u32,
            end: t.end as u32,
        };
        let unknown = |reason| CallSite {
            at: span,
            kind: CallKind::Call,
            target: CallTarget::Unknown { name: None, reason },
        };
        match t.kind {
            Kind::DynamicFunction | Kind::DynamicInvoke if next == Some(Kind::LeftParen) => {
                out.push(unknown(UnknownCall::Dynamic));
                continue;
            }
            Kind::DynamicNew => {
                out.push(unknown(UnknownCall::Dynamic));
                continue;
            }
            _ => {}
        }
        if next != Some(Kind::LeftParen) || t.kind == Kind::StringLiteral {
            continue;
        }
        let adjacent = toks[i + 1].start == t.end;
        if prev == Some(Kind::Colon) && i >= 2 {
            // `x:name(`: a method of a handle or of an object.
            if toks[i - 1].end != t.start {
                continue;
            }
            let name = text(source, t);
            match receiver(source, &toks, i - 1, decls) {
                Receiver::Handle => {}
                Receiver::This if decls.methods.iter().any(|m| same(m, name)) => {
                    out.push(CallSite {
                        at: span,
                        kind: CallKind::Call,
                        target: CallTarget::Method(name.to_string()),
                    });
                }
                Receiver::This | Receiver::Object => out.push(CallSite {
                    at: span,
                    kind: CallKind::Call,
                    target: CallTarget::Unknown {
                        name: Some(name.to_string()),
                        reason: UnknownCall::Method,
                    },
                }),
            }
            continue;
        }
        let qualified = i > 0
            && toks[i - 1].end == t.start
            && matches!(prev, Some(Kind::Period | Kind::DoubleColon));
        if prev == Some(Kind::Function)
            || qualified
            || decls.definitions.contains(&(t.start as u32))
        {
            continue;
        }
        if !adjacent && t.kind != Kind::Identifier {
            continue;
        }
        let name = text(source, t);
        if t.kind == Kind::Identifier && same(name, "super") {
            out.push(unknown(UnknownCall::Super));
        } else if decls.functions.iter().any(|f| same(f, name)) {
            out.push(CallSite {
                at: span,
                kind: CallKind::Call,
                target: CallTarget::Function(name.to_string()),
            });
        } else if let Some((_, sup)) = decls.delegated.iter().find(|(f, _)| same(f, name)) {
            out.push(CallSite {
                at: span,
                kind: CallKind::Call,
                target: CallTarget::Unknown {
                    name: Some(name.to_string()),
                    reason: if *sup {
                        UnknownCall::Super
                    } else {
                        UnknownCall::Handle
                    },
                },
            });
        } else if class && decls.methods.iter().any(|m| same(m, name)) {
            out.push(CallSite {
                at: span,
                kind: CallKind::Call,
                target: CallTarget::Method(name.to_string()),
            });
        }
    }
}

/// Visit every statement, nested bodies included, in source order.
fn walk(stmts: &[Statement], f: &mut impl FnMut(&Statement)) {
    for s in stmts {
        walk_one(s, f);
    }
}

fn walk_one(s: &Statement, f: &mut impl FnMut(&Statement)) {
    f(s);
    match &s.kind {
        StatementKind::Procedure { body, .. }
        | StatementKind::Function { body, .. }
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Destructor { body }
        | StatementKind::Finally { body }
        | StatementKind::Catch { body, .. }
        | StatementKind::Block(body)
        | StatementKind::VariableDeclarationList(body)
        | StatementKind::Method { body, .. }
        | StatementKind::Constructor { body, .. }
        | StatementKind::Do { body, .. }
        | StatementKind::Repeat { body, .. }
        | StatementKind::ForEach { body, .. } => walk(body, f),
        StatementKind::Property {
            get_body, set_body, ..
        } => {
            for body in [get_body, set_body].into_iter().flatten() {
                walk(body, f);
            }
        }
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            walk_one(then_branch, f);
            if let Some(e) = else_branch {
                walk_one(e, f);
            }
        }
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            for wb in when_branches {
                walk(&wb.body, f);
            }
            if let Some(o) = otherwise {
                walk(o, f);
            }
        }
        StatementKind::Label { body, .. } => walk_one(body, f),
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
        } => walk_one(body, f),
        StatementKind::PreprocIf(pif) => {
            walk(&pif.then_branch, f);
            for (_, b) in &pif.elseif_branches {
                walk(b, f);
            }
            if let Some(b) = &pif.else_branch {
                walk(b, f);
            }
        }
        _ => {}
    }
}

/// What the run index answers for a program name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramAnswer {
    Found(IndexedFileId),
    Unknown(UnknownCall),
}

/// Resolve a program a `RUN` names over the search path, through the run
/// index. The index memoizes the answer, so asking again is cheap.
pub fn resolve_program(index: &dyn WorkspaceIndex, name: &str) -> ProgramAnswer {
    match index.program(&IndexName::new(name)) {
        IndexAnswer::Found(file) => ProgramAnswer::Found(file),
        IndexAnswer::NotFound => ProgramAnswer::Unknown(UnknownCall::NotFound),
        IndexAnswer::Unusable => ProgramAnswer::Unknown(UnknownCall::Unusable),
        IndexAnswer::Unknowable => ProgramAnswer::Unknown(UnknownCall::Ambiguous),
    }
}

/// An edge of a [`CallGraph`]: a routine, or Unknown with the reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Callee {
    Node(u32),
    Unknown(UnknownCall),
}

/// The resolved calls between routines (nodes `0..n`, numbered by the
/// caller), bounded by [`MAX_EDGES`].
#[derive(Debug, Clone, Default)]
pub struct CallGraph {
    edges: Vec<Vec<Callee>>,
    count: usize,
    truncated: bool,
}

impl CallGraph {
    /// A graph of `nodes` routines and no edges.
    pub fn new(nodes: usize) -> Self {
        CallGraph {
            edges: vec![Vec::new(); nodes],
            count: 0,
            truncated: false,
        }
    }

    /// The number of routines.
    pub fn len(&self) -> usize {
        self.edges.len()
    }

    /// Whether the graph has no routine.
    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    /// Add a call from `from`. Returns `false`, and marks the graph
    /// truncated, past [`MAX_EDGES`].
    pub fn add(&mut self, from: u32, to: Callee) -> bool {
        if self.count >= MAX_EDGES {
            self.truncated = true;
            return false;
        }
        let list = &mut self.edges[from as usize];
        if !list.contains(&to) {
            list.push(to);
            self.count += 1;
        }
        true
    }

    /// The calls of one routine, in the order added.
    pub fn callees(&self, node: u32) -> &[Callee] {
        &self.edges[node as usize]
    }

    /// Whether edges were dropped.
    pub fn truncated(&self) -> bool {
        self.truncated
    }

    /// The strongly connected components, callees before callers: every
    /// component comes after the components it calls into.
    pub fn components(&self) -> Vec<Vec<u32>> {
        // Tarjan's algorithm, iterative.
        let n = self.edges.len();
        const NONE: u32 = u32::MAX;
        let mut index = vec![NONE; n];
        let mut low = vec![0u32; n];
        let mut on_stack = vec![false; n];
        let mut stack: Vec<u32> = Vec::new();
        let mut out: Vec<Vec<u32>> = Vec::new();
        let mut next = 0u32;
        let mut frames: Vec<(u32, usize)> = Vec::new();
        for root in 0..n as u32 {
            if index[root as usize] != NONE {
                continue;
            }
            frames.push((root, 0));
            index[root as usize] = next;
            low[root as usize] = next;
            next += 1;
            stack.push(root);
            on_stack[root as usize] = true;
            while let Some(&mut (v, ref mut i)) = frames.last_mut() {
                let edges = &self.edges[v as usize];
                let mut descended = false;
                while *i < edges.len() {
                    let e = edges[*i];
                    *i += 1;
                    let Callee::Node(w) = e else {
                        continue;
                    };
                    if index[w as usize] == NONE {
                        index[w as usize] = next;
                        low[w as usize] = next;
                        next += 1;
                        stack.push(w);
                        on_stack[w as usize] = true;
                        frames.push((w, 0));
                        descended = true;
                        break;
                    } else if on_stack[w as usize] {
                        low[v as usize] = low[v as usize].min(index[w as usize]);
                    }
                }
                if descended {
                    continue;
                }
                frames.pop();
                if let Some(&(parent, _)) = frames.last() {
                    low[parent as usize] = low[parent as usize].min(low[v as usize]);
                }
                if low[v as usize] == index[v as usize] {
                    let mut comp = Vec::new();
                    while let Some(w) = stack.pop() {
                        on_stack[w as usize] = false;
                        comp.push(w);
                        if w == v {
                            break;
                        }
                    }
                    comp.sort_unstable();
                    out.push(comp);
                }
            }
        }
        out
    }
}
