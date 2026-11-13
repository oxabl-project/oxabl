//! The document outline: the declarations of one file as a tree, for an
//! editor's outline view, breadcrumbs and "Go to Symbol in File".
//!
//! Like [`FormatPipeline`](crate::FormatPipeline), it reads the **raw** source:
//! no preprocessing and no includes, so every span is a byte range of the
//! buffer as written and a client can map it without a source map.

use std::collections::HashSet;

use oxabl_ast::{
    AccessModifier, BufferTarget, CreateTarget, DataType, HandleParamKind, Identifier, OnAction,
    OnKind, ParameterDirection, ParameterType, Span, Statement, StatementKind, TempTableField,
    TypeSource, WidgetTrigger,
};
use oxabl_common::{InternalPanic, catch_panic, panic_if_injected, panic_sites};
use oxabl_lexer::{Kind, Token};

/// What a symbol declares. A client maps it to its own symbol kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OutlineKind {
    Class,
    Interface,
    Procedure,
    Function,
    Method,
    Constructor,
    Destructor,
    Property,
    Event,
    Variable,
    Parameter,
    TempTable,
    Field,
    Buffer,
    Dataset,
    DataSource,
    Query,
    Stream,
    Frame,
    Widget,
    /// An `ON` trigger block or a `TRIGGER PROCEDURE` header.
    Trigger,
    /// A statement-level `{file.i}` reference.
    Include,
    /// `&SCOPED-DEFINE` / `&GLOBAL-DEFINE`.
    PreprocessorDefine,
    /// A labelled block (`main-loop: DO: … END.`).
    Label,
}

/// One declaration and the declarations nested in it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutlineSymbol {
    pub name: String,
    /// Short extra text shown next to the name, e.g. `INTEGER` or
    /// `PUBLIC STATIC`.
    pub detail: Option<String>,
    pub kind: OutlineKind,
    /// The whole declaration, e.g. `PROCEDURE p: … END PROCEDURE.`
    pub span: Span,
    /// The part an editor selects and reveals, normally the name. Always
    /// inside `span`.
    pub selection_span: Span,
    /// In source order; every child's `span` is inside this symbol's `span`.
    pub children: Vec<OutlineSymbol>,
}

/// The outline of `source`, top-level symbols in source order.
///
/// A source that does not parse cleanly still yields the declarations the
/// parser recovered. `Err` means an internal panic was contained: the outline
/// is unknown, not empty.
///
/// The parse recurses once for each level of nesting, so the caller's thread
/// must hold that stack, as for every other parse.
pub fn outline(source: &str) -> Result<Vec<OutlineSymbol>, InternalPanic> {
    catch_panic(|| {
        panic_if_injected(panic_sites::OUTLINE, source);
        outline_inner(source)
    })
}

/// The longest `ON` event phrase kept as a name, in characters.
const TRIGGER_NAME_MAX: usize = 80;

/// The most symbols that nest in each other. An editor shows no deeper tree, and
/// the JSON of one that is deeper is more than a JSON reader accepts.
const MAX_DEPTH: usize = 32;

fn outline_inner(source: &str) -> Vec<OutlineSymbol> {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    let mut walker = Walker {
        source,
        tokens: &tokens,
        defined_functions: HashSet::new(),
        prototypes: Vec::new(),
        open: Vec::new(),
        top: Vec::new(),
    };
    walker.walk(&program.statements);
    walker.drop_shadowed_prototypes();
    walker.top
}

/// A unit of pending work. The walk keeps its own stack, so the depth of the
/// input never reaches the call stack.
enum Frame<'a> {
    Statements(std::slice::Iter<'a, Statement>),
    Triggers(std::slice::Iter<'a, WidgetTrigger>),
    /// The symbol on top of `Walker::open` has all its children.
    Close,
}

struct Walker<'a> {
    source: &'a str,
    tokens: &'a [Token],
    /// Lower-case names of the functions the file defines, not only declares.
    defined_functions: HashSet<String>,
    /// The function prototypes seen: lower-case name and statement span.
    prototypes: Vec<(String, Span)>,
    /// Symbols that wait for their children, innermost last.
    open: Vec<OutlineSymbol>,
    top: Vec<OutlineSymbol>,
}

impl<'a> Walker<'a> {
    fn walk(&mut self, statements: &'a [Statement]) {
        let mut stack = vec![Frame::Statements(statements.iter())];
        while let Some(frame) = stack.last_mut() {
            match frame {
                Frame::Statements(iter) => match iter.next() {
                    Some(statement) => self.visit(statement, &mut stack),
                    None => {
                        stack.pop();
                    }
                },
                Frame::Triggers(iter) => match iter.next() {
                    Some(trigger) => self.widget_trigger(trigger, &mut stack),
                    None => {
                        stack.pop();
                    }
                },
                Frame::Close => {
                    stack.pop();
                    self.close();
                }
            }
        }
    }

    fn visit(&mut self, statement: &'a Statement, stack: &mut Vec<Frame<'a>>) {
        let span = statement.span;
        match &statement.kind {
            StatementKind::Class {
                name,
                is_abstract,
                is_final,
                ..
            } => {
                let detail = match (is_abstract, is_final) {
                    (true, true) => Some("ABSTRACT FINAL"),
                    (true, false) => Some("ABSTRACT"),
                    (false, true) => Some("FINAL"),
                    (false, false) => None,
                };
                let symbol = self.named(OutlineKind::Class, name, detail.map(str::to_string), span);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Interface { name, .. } => {
                let symbol = self.named(OutlineKind::Interface, name, None, span);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Procedure {
                name,
                is_prototype,
                is_external,
                ..
            } => {
                let detail = if *is_external {
                    Some("EXTERNAL")
                } else if *is_prototype {
                    Some("PROTOTYPE")
                } else {
                    None
                };
                let symbol = self.named(
                    OutlineKind::Procedure,
                    name,
                    detail.map(str::to_string),
                    span,
                );
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Function {
                name,
                return_type,
                body,
                ..
            } => {
                let lower = name.name.to_ascii_lowercase();
                if self.is_function_prototype(span, body) {
                    self.prototypes.push((lower, span));
                } else {
                    self.defined_functions.insert(lower);
                }
                let detail = data_type_name(return_type);
                let symbol = self.named(OutlineKind::Function, name, detail, span);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                ..
            } => {
                let mut detail = String::with_capacity(32);
                detail.push_str(access_name(*access));
                for (written, word) in [
                    (*is_static, "STATIC"),
                    (*is_abstract, "ABSTRACT"),
                    (*is_override, "OVERRIDE"),
                ] {
                    if written {
                        detail.push(' ');
                        detail.push_str(word);
                    }
                }
                detail.push(' ');
                match return_type {
                    None => detail.push_str("VOID"),
                    Some(data_type) => {
                        detail.push_str(&data_type_name(data_type).unwrap_or_default());
                    }
                }
                detail.truncate(detail.trim_end().len());
                let symbol = self.named(OutlineKind::Method, name, Some(detail), span);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Constructor { access, .. } => {
                let name = self.class_tail().unwrap_or("CONSTRUCTOR").to_string();
                let selection = self.first_word(span);
                let detail = Some(access_name(*access).to_string());
                let symbol = self.make(OutlineKind::Constructor, &name, detail, span, selection);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Destructor { .. } => {
                let name = self.class_tail().unwrap_or("DESTRUCTOR").to_string();
                let selection = self.first_word(span);
                let detail = Some("DESTRUCTOR".to_string());
                let symbol = self.make(OutlineKind::Destructor, &name, detail, span, selection);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::Property {
                access,
                is_static,
                name,
                data_type,
                get_body,
                set_body,
                set_parameters,
                ..
            } => {
                let mut detail = String::from(access_name(*access));
                if *is_static {
                    detail.push_str(" STATIC");
                }
                if let Some(type_name) = data_type_name(data_type) {
                    detail.push(' ');
                    detail.push_str(&type_name);
                }
                let symbol = self.named(OutlineKind::Property, name, Some(detail), span);
                let computed = [get_body, set_body]
                    .into_iter()
                    .any(|body| body.as_ref().is_some_and(|body| !body.is_empty()))
                    || !set_parameters.is_empty();
                if computed {
                    self.open_container(symbol, statement, stack);
                } else {
                    self.emit(symbol);
                }
            }
            StatementKind::DefineEvent { access, name, .. } => {
                let detail = Some(access_name(*access).to_string());
                let symbol = self.named(OutlineKind::Event, name, detail, span);
                self.open_container(symbol, statement, stack);
            }
            StatementKind::VariableDeclaration { .. } => self.variable(statement),
            StatementKind::VariableDeclarationList(declarations) => {
                for declaration in declarations {
                    self.variable(declaration);
                }
            }
            StatementKind::DefineParameter {
                direction,
                param_type,
            } => {
                let direction = direction_name(direction);
                let (name, detail) = match param_type {
                    ParameterType::Variable {
                        name, type_source, ..
                    } => {
                        let mut detail = direction.to_string();
                        if let Some(type_name) = type_detail(type_source, None) {
                            detail.push(' ');
                            detail.push_str(&type_name);
                        }
                        (name, detail)
                    }
                    ParameterType::Handle { kind, name, .. } => {
                        let kind = match kind {
                            HandleParamKind::Table => "TABLE",
                            HandleParamKind::TableHandle => "TABLE-HANDLE",
                            HandleParamKind::Dataset => "DATASET",
                            HandleParamKind::DatasetHandle => "DATASET-HANDLE",
                        };
                        (name, format!("{direction} {kind}"))
                    }
                    ParameterType::Buffer { name, target } => {
                        (name, format!("BUFFER FOR {}", target.name))
                    }
                };
                // The parser names an unnamed prototype parameter `$N`; an ABL
                // name never starts with `$`.
                if name.name.starts_with('$') {
                    return;
                }
                let symbol = self.named(OutlineKind::Parameter, name, Some(detail), span);
                self.emit(symbol);
            }
            StatementKind::DefineTempTable {
                name,
                like_table,
                fields,
                ..
            } => {
                let detail = like_table
                    .as_ref()
                    .map(|table| format!("LIKE {}", table.name));
                let mut symbol = self.named(OutlineKind::TempTable, name, detail, span);
                if self.open.len() >= MAX_DEPTH {
                    symbol = None;
                    for field in fields {
                        let field = self.field(field, span);
                        self.emit(field);
                    }
                }
                if let Some(table) = symbol.as_mut() {
                    for field in fields {
                        if let Some(child) = self.field(field, span) {
                            push_child(table, child);
                        }
                    }
                }
                self.emit(symbol);
            }
            StatementKind::DefineBuffer { name, target, .. } => {
                let detail = match target {
                    BufferTarget::Table(table) => format!("FOR {}", table.name),
                    BufferTarget::TempTable(table) => format!("FOR TEMP-TABLE {}", table.name),
                };
                let symbol = self.named(OutlineKind::Buffer, name, Some(detail), span);
                self.emit(symbol);
            }
            StatementKind::DefineDataset { name, buffers, .. } => {
                let detail = (!buffers.is_empty()).then(|| {
                    let names: Vec<&str> = buffers.iter().map(|b| b.name.as_str()).collect();
                    format!("FOR {}", names.join(", "))
                });
                let symbol = self.named(OutlineKind::Dataset, name, detail, span);
                self.emit(symbol);
            }
            StatementKind::DefineDataSource { name, .. } => {
                let symbol = self.named(OutlineKind::DataSource, name, None, span);
                self.emit(symbol);
            }
            StatementKind::Skipped {
                may_reference_tables: true,
                ..
            } => {
                let symbol = self.query(span);
                self.emit(symbol);
            }
            StatementKind::DefineStream { name } => {
                let symbol = self.named(OutlineKind::Stream, name, None, span);
                self.emit(symbol);
            }
            StatementKind::DefineFrame { name, .. } => {
                let symbol = self.named(OutlineKind::Frame, name, None, span);
                self.emit(symbol);
            }
            StatementKind::DefineWidget { name } => {
                let symbol = self.named(OutlineKind::Widget, name, None, span);
                self.emit(symbol);
            }
            StatementKind::On { kind } => {
                let (OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. }) = kind else {
                    return;
                };
                if let OnAction::Block(block) = action {
                    let symbol = self.trigger(span, block.span.start);
                    self.open_container(symbol, statement, stack);
                }
            }
            StatementKind::Create {
                target: CreateTarget::Handle { triggers, .. },
                ..
            } => stack.push(Frame::Triggers(triggers.iter())),
            StatementKind::TriggerProcedure { target, .. } => {
                let symbol = self.trigger_procedure(span, target);
                self.emit(symbol);
            }
            StatementKind::IncludeReference { path_and_args, .. } => {
                let symbol = self.include(path_and_args, span);
                self.emit(symbol);
            }
            StatementKind::PreprocDefine {
                name, is_global, ..
            } => {
                let detail = if *is_global { "GLOBAL" } else { "SCOPED" };
                let kind = OutlineKind::PreprocessorDefine;
                let symbol = self.named(kind, name, Some(detail.to_string()), span);
                self.emit(symbol);
            }
            StatementKind::Label { name, .. } => {
                let selection = self.label_selection(name, span);
                let symbol = self.make(OutlineKind::Label, name, None, span, selection);
                self.open_container(symbol, statement, stack);
            }
            _ => children(statement, |slice| {
                stack.push(Frame::Statements(slice.iter()))
            }),
        }
    }

    fn variable(&mut self, statement: &Statement) {
        if let StatementKind::VariableDeclaration {
            name,
            type_source,
            extent,
            ..
        } = &statement.kind
        {
            // The parser reads `AS type EXTENT n` without keeping the extent.
            let extent = extent.or_else(|| self.written_extent(name.span.end, statement.span));
            let detail = type_detail(type_source, extent);
            let symbol = self.named(OutlineKind::Variable, name, detail, statement.span);
            self.emit(symbol);
        }
    }

    /// The extent that an `EXTENT [n]` after `from` writes in `span`: `0` for a
    /// dynamic one.
    fn written_extent(&self, from: u32, span: Span) -> Option<u32> {
        let mut tokens = self
            .tokens_in(span)
            .iter()
            .filter(|t| t.kind != Kind::Comment && t.start >= from as usize);
        tokens.find(|t| t.kind == Kind::Extent)?;
        Some(
            tokens
                .next()
                .filter(|t| t.kind == Kind::IntegerLiteral)
                .and_then(|t| self.source[t.start..t.end].parse().ok())
                .unwrap_or(0),
        )
    }

    /// The last segment of the nearest enclosing class name.
    fn class_tail(&self) -> Option<&str> {
        let class = self
            .open
            .iter()
            .rev()
            .find(|symbol| symbol.kind == OutlineKind::Class)?;
        class
            .name
            .rsplit('.')
            .next()
            .filter(|tail| !tail.is_empty())
    }

    /// Opens `symbol` so the children of `statement` land in it. An invalid
    /// symbol is dropped and its children go to the enclosing symbol. So is a
    /// symbol at `MAX_DEPTH`: its children nest no deeper.
    fn open_container(
        &mut self,
        symbol: Option<OutlineSymbol>,
        statement: &'a Statement,
        stack: &mut Vec<Frame<'a>>,
    ) {
        self.open(symbol, stack);
        children(statement, |slice| {
            stack.push(Frame::Statements(slice.iter()))
        });
    }

    fn open(&mut self, symbol: Option<OutlineSymbol>, stack: &mut Vec<Frame<'a>>) {
        if let Some(symbol) = symbol
            && self.open.len() < MAX_DEPTH
        {
            self.open.push(symbol);
            stack.push(Frame::Close);
        }
    }

    fn close(&mut self) {
        let symbol = self.open.pop();
        self.emit(symbol);
    }

    /// Adds `symbol` to the innermost open symbol, or to the top level. A symbol
    /// that is out of order or outside its parent is dropped: an editor rejects
    /// the whole answer for one such range.
    fn emit(&mut self, symbol: Option<OutlineSymbol>) {
        let Some(symbol) = symbol else { return };
        match self.open.last_mut() {
            Some(parent) => push_child(parent, symbol),
            None => {
                let ordered = self
                    .top
                    .last()
                    .is_none_or(|prev| prev.span.end <= symbol.span.start);
                debug_assert!(
                    ordered,
                    "outline: top-level {:?} {:?} at {:?} starts before the previous symbol ends",
                    symbol.kind, symbol.name, symbol.span
                );
                if ordered {
                    self.top.push(symbol);
                }
            }
        }
    }

    /// A symbol named by `name`, selected at the name.
    fn named(
        &self,
        kind: OutlineKind,
        name: &Identifier,
        detail: Option<String>,
        span: Span,
    ) -> Option<OutlineSymbol> {
        // A zero-width name is a recovery or synthetic one: select the first token.
        let selection = if name.span.start < name.span.end {
            debug_assert!(
                contains(span, name.span),
                "outline: name {:?} at {:?} is outside its statement {span:?}",
                name.name,
                name.span
            );
            contains(span, name.span).then_some(name.span)
        } else {
            self.first_word(span)
        };
        self.make(kind, &name.name, detail, span, selection)
    }

    /// Builds a symbol and checks the span rules; `None` when they do not hold.
    /// A zero-width statement is a recovery node and gives no symbol.
    fn make(
        &self,
        kind: OutlineKind,
        name: &str,
        detail: Option<String>,
        span: Span,
        selection: Option<Span>,
    ) -> Option<OutlineSymbol> {
        if span.start >= span.end {
            return None;
        }
        let selection = selection.filter(|s| s.start < s.end);
        let valid = selection.is_some_and(|selection| {
            let on_boundary = |offset: u32| self.source.is_char_boundary(offset as usize);
            span.end as usize <= self.source.len()
                && span.start <= selection.start
                && selection.end <= span.end
                && on_boundary(span.start)
                && on_boundary(span.end)
                && on_boundary(selection.start)
                && on_boundary(selection.end)
        });
        debug_assert!(
            valid,
            "outline: no valid symbol for {kind:?} {name:?}: span {span:?}, selection {selection:?}, source length {}",
            self.source.len()
        );
        if !valid {
            return None;
        }
        // A name of only whitespace (a vertical tab lexes as part of a name)
        // gives no symbol.
        Some(OutlineSymbol {
            name: clean_name(name)?,
            detail,
            kind,
            span,
            selection_span: selection?,
            children: Vec::new(),
        })
    }

    fn field(&self, field: &TempTableField, table: Span) -> Option<OutlineSymbol> {
        let name = field.name.span;
        if name.start >= name.end {
            return None;
        }
        debug_assert!(
            contains(table, name),
            "outline: field {:?} at {name:?} is outside its table {table:?}",
            field.name.name
        );
        if !contains(table, name) {
            return None;
        }
        let detail = type_detail(&field.type_source, field.extent);
        // A hand-built field has no span of its own.
        let span = if contains(field.span, name) && contains(table, field.span) {
            field.span
        } else {
            name
        };
        self.make(
            OutlineKind::Field,
            &field.name.name,
            detail,
            span,
            Some(name),
        )
    }

    /// The tokens of `span`, comments included. Tokens are in source order, so
    /// the range is two binary searches.
    fn tokens_in(&self, span: Span) -> &'a [Token] {
        let lo = self
            .tokens
            .partition_point(|t| t.start < span.start as usize);
        let n = self.tokens[lo..].partition_point(|t| t.start < span.end as usize);
        &self.tokens[lo..lo + n]
    }

    /// The tokens of `span` that are not comments.
    fn significant(&self, span: Span) -> impl DoubleEndedIterator<Item = &'a Token> {
        self.tokens_in(span)
            .iter()
            .filter(|t| t.kind != Kind::Comment)
    }

    /// The first token of `span` that is not a comment: a keyword, a name or a
    /// symbol.
    fn first_word(&self, span: Span) -> Option<Span> {
        self.significant(span).next().map(token_span)
    }

    /// `DEFINE [modifiers] QUERY name ...`. The parser skips the statement, so
    /// the name is read from its tokens.
    fn query(&self, span: Span) -> Option<OutlineSymbol> {
        let mut tokens = self.significant(span);
        if tokens.next()?.kind != Kind::Define {
            return None;
        }
        loop {
            match tokens.next()?.kind {
                Kind::Query => break,
                Kind::New
                | Kind::Global
                | Kind::Shared
                | Kind::Public
                | Kind::Private
                | Kind::Protected
                | Kind::PackagePrivate
                | Kind::PackageProtected
                | Kind::KwStatic => {}
                _ => return None,
            }
        }
        let name = tokens
            .next()
            .filter(|t| !matches!(t.kind, Kind::Period | Kind::Colon | Kind::Comma))?;
        let selection = token_span(name);
        self.make(
            OutlineKind::Query,
            &self.source[name.start..name.end],
            None,
            span,
            Some(selection),
        )
    }

    /// A function with no body is a prototype unless it is a definition: the
    /// last tokens before its terminator are `END [FUNCTION]`.
    fn is_function_prototype(&self, span: Span, body: &[Statement]) -> bool {
        if !body.is_empty() {
            return false;
        }
        let mut tokens = self.significant(span).rev().peekable();
        if tokens
            .next_if(|t| matches!(t.kind, Kind::Period | Kind::Colon))
            .is_none()
        {
            return true;
        }
        tokens.next_if(|t| t.kind == Kind::Function);
        tokens.next().is_none_or(|t| t.kind != Kind::End)
    }

    /// Removes the prototypes that the file also defines.
    fn drop_shadowed_prototypes(&mut self) {
        let shadowed: HashSet<(u32, u32)> = self
            .prototypes
            .iter()
            .filter(|(name, _)| self.defined_functions.contains(name))
            .map(|(_, span)| (span.start, span.end))
            .collect();
        if shadowed.is_empty() {
            return;
        }
        let mut stack = vec![&mut self.top];
        while let Some(symbols) = stack.pop() {
            symbols.retain(|symbol| {
                symbol.kind != OutlineKind::Function
                    || !shadowed.contains(&(symbol.span.start, symbol.span.end))
            });
            stack.extend(symbols.iter_mut().map(|symbol| &mut symbol.children));
        }
    }

    /// A label is written at the start of its statement.
    fn label_selection(&self, name: &str, span: Span) -> Option<Span> {
        let end = span.start as usize + name.len();
        match self.source.get(span.start as usize..end) {
            Some(written) if written.eq_ignore_ascii_case(name) => Some(Span {
                start: span.start,
                end: end as u32,
            }),
            _ => self.first_word(span),
        }
    }

    /// An `ON ... DO:` trigger. The name is the event phrase between `ON` and
    /// the action, which starts at `action_start`; comments are left out.
    fn trigger(&self, span: Span, action_start: u32) -> Option<OutlineSymbol> {
        let on = self.first_word(span)?;
        let phrase = Span {
            start: on.end,
            end: action_start.min(span.end),
        };
        let mut tokens = self
            .significant(phrase)
            .filter(|t| t.start >= on.end as usize);
        let Some(first) = tokens.next() else {
            return self.make(OutlineKind::Trigger, "ON", None, span, Some(on));
        };
        let mut name = self.source[first.start..first.end].to_string();
        let mut end = first.end;
        // The name is cut to `TRIGGER_NAME_MAX` characters, so a long phrase is not joined whole.
        // Joined bytes are at least joined characters.
        for token in tokens {
            if name.len() > TRIGGER_NAME_MAX {
                break;
            }
            join_token(self.source, &mut name, end, token);
            end = token.end;
        }
        let last = self
            .significant(phrase)
            .rfind(|t| t.start >= on.end as usize)
            .unwrap_or(first);
        let selection = Span {
            start: first.start as u32,
            end: last.end as u32,
        };
        let name = truncate_chars(&name, TRIGGER_NAME_MAX);
        self.make(OutlineKind::Trigger, &name, None, span, Some(selection))
    }

    /// An `ON event-list` entry of a widget's `TRIGGERS` phrase. It has no span
    /// of its own: the `ON` is the token before the first event.
    fn widget_trigger(&mut self, trigger: &'a WidgetTrigger, stack: &mut Vec<Frame<'a>>) {
        let OnAction::Block(block) = &trigger.action else {
            return;
        };
        let on = trigger.events.first().and_then(|event| {
            let before = Span {
                start: 0,
                end: event.span.start,
            };
            self.significant(before).next_back().map(token_span)
        });
        let span = on.map(|on| Span {
            start: on.start,
            end: block.span.end,
        });
        let symbol = span.and_then(|span| self.trigger(span, block.span.start));
        self.open(symbol, stack);
        stack.push(Frame::Statements(std::slice::from_ref(&**block).iter()));
    }

    /// `TRIGGER PROCEDURE FOR WRITE OF Customer ...`: the name runs from the
    /// event to the end of the table.
    fn trigger_procedure(&self, span: Span, target: &Identifier) -> Option<OutlineSymbol> {
        let mut tokens = self.significant(span).skip(3);
        let detail = Some("TRIGGER PROCEDURE".to_string());
        let Some(event) = tokens.next() else {
            return self.make(OutlineKind::Trigger, "", detail, span, None);
        };
        let target_end = target.span.end as usize;
        let phrase_end =
            if !target.name.is_empty() && target_end > event.end && target_end <= span.end as usize
            {
                target_end
            } else {
                event.end
            };
        let mut name = self.source[event.start..event.end].to_string();
        let mut end = event.end;
        for token in tokens.take_while(|t| t.end <= phrase_end) {
            join_token(self.source, &mut name, end, token);
            end = token.end;
        }
        let selection = Span {
            start: event.start as u32,
            end: end as u32,
        };
        self.make(OutlineKind::Trigger, &name, detail, span, Some(selection))
    }

    /// `{file.i arg}`: the name is the file, as written.
    fn include(&self, path_and_args: &str, span: Span) -> Option<OutlineSymbol> {
        let written = self.source.get(span.start as usize..span.end as usize)?;
        // `~{` escapes the brace: the text is not an include.
        if written.starts_with('~') {
            return None;
        }
        let file = path_and_args.split_whitespace().next()?;
        let selection = written.find(file).map_or(span, |at| {
            let start = span.start + at as u32;
            Span {
                start,
                end: start + file.len() as u32,
            }
        });
        self.make(OutlineKind::Include, file, None, span, Some(selection))
    }
}

/// Calls `f` with the statement lists that `statement` holds, last list first,
/// so a stack that pops them sees source order.
fn children<'a>(statement: &'a Statement, mut f: impl FnMut(&'a [Statement])) {
    use std::slice::from_ref;
    match &statement.kind {
        StatementKind::Do { body, .. }
        | StatementKind::Repeat { body, .. }
        | StatementKind::ForEach { body, .. }
        | StatementKind::Catch { body, .. }
        | StatementKind::Finally { body }
        | StatementKind::Block(body)
        | StatementKind::Class { body, .. }
        | StatementKind::Interface { body, .. }
        | StatementKind::Procedure { body, .. }
        | StatementKind::Destructor { body } => f(body),
        StatementKind::Function {
            body,
            prototype_parameters,
            ..
        } => {
            f(body);
            f(prototype_parameters);
        }
        StatementKind::Method {
            parameters, body, ..
        }
        | StatementKind::Constructor {
            parameters, body, ..
        } => {
            f(body);
            f(parameters);
        }
        StatementKind::DefineEvent { parameters, .. } => f(parameters),
        StatementKind::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            // The parser reads GET before SET, and the SET parameters precede its body.
            f(set_body.as_deref().unwrap_or_default());
            f(set_parameters);
            f(get_body.as_deref().unwrap_or_default());
        }
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            if let Some(else_branch) = else_branch {
                f(from_ref(else_branch));
            }
            f(from_ref(then_branch));
        }
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            if let Some(otherwise) = otherwise {
                f(otherwise);
            }
            for branch in when_branches.iter().rev() {
                f(&branch.body);
            }
        }
        StatementKind::PreprocIf(preproc) => {
            if let Some(else_branch) = &preproc.else_branch {
                f(else_branch);
            }
            for (_, branch) in preproc.elseif_branches.iter().rev() {
                f(branch);
            }
            f(&preproc.then_branch);
        }
        StatementKind::On {
            kind:
                OnKind::UiEvent {
                    action: OnAction::Block(block),
                    ..
                }
                | OnKind::DbEvent {
                    action: OnAction::Block(block),
                    ..
                },
        } => f(from_ref(block)),
        StatementKind::Label { body, .. } => f(from_ref(body)),
        _ => {}
    }
}

fn push_child(parent: &mut OutlineSymbol, child: OutlineSymbol) {
    let ordered = parent
        .children
        .last()
        .is_none_or(|prev| prev.span.end <= child.span.start);
    let inside = contains(parent.span, child.span);
    debug_assert!(
        ordered && inside,
        "outline: {:?} {:?} at {:?} is {} {:?} {:?} at {:?}",
        child.kind,
        child.name,
        child.span,
        if inside { "out of order in" } else { "outside" },
        parent.kind,
        parent.name,
        parent.span
    );
    if ordered && inside {
        parent.children.push(child);
    }
}

fn token_span(token: &Token) -> Span {
    Span {
        start: token.start as u32,
        end: token.end as u32,
    }
}

/// Appends `token` to `name`, after a space when the source has a gap before it.
fn join_token(source: &str, name: &mut String, previous_end: usize, token: &Token) {
    if token.start > previous_end {
        name.push(' ');
    }
    name.push_str(&source[token.start..token.end]);
}

fn contains(outer: Span, inner: Span) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
}

/// A name with no leading or trailing space and no line break; `None` when
/// nothing is left.
fn clean_name(name: &str) -> Option<String> {
    let name = if name.contains(['\n', '\r']) {
        collapse_space(name)
    } else {
        name.trim().to_string()
    };
    (!name.is_empty()).then_some(name)
}

fn collapse_space(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for word in text.split_whitespace() {
        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(word);
    }
    out
}

fn truncate_chars(text: &str, max: usize) -> String {
    match text.char_indices().nth(max) {
        Some((end, _)) => text[..end].to_string(),
        None => text.to_string(),
    }
}

fn access_name(access: AccessModifier) -> &'static str {
    match access {
        AccessModifier::Public => "PUBLIC",
        AccessModifier::Private => "PRIVATE",
        AccessModifier::Protected => "PROTECTED",
        AccessModifier::PackagePrivate => "PACKAGE-PRIVATE",
        AccessModifier::PackageProtected => "PACKAGE-PROTECTED",
    }
}

fn direction_name(direction: &ParameterDirection) -> &'static str {
    match direction {
        ParameterDirection::Input => "INPUT",
        ParameterDirection::Output => "OUTPUT",
        ParameterDirection::InputOutput => "INPUT-OUTPUT",
        ParameterDirection::Return => "RETURN",
    }
}

/// The type as a detail; `None` for a conditional type, which has no one name.
fn data_type_name(data_type: &DataType) -> Option<String> {
    let name = match data_type {
        DataType::Integer => "INTEGER",
        DataType::Int64 => "INT64",
        DataType::Decimal => "DECIMAL",
        DataType::Character => "CHARACTER",
        DataType::Logical => "LOGICAL",
        DataType::Date => "DATE",
        DataType::DateTime => "DATETIME",
        DataType::DateTimeTz => "DATETIME-TZ",
        DataType::Handle => "HANDLE",
        DataType::Rowid => "ROWID",
        DataType::Recid => "RECID",
        DataType::Raw => "RAW",
        DataType::Memptr => "MEMPTR",
        DataType::Longchar => "LONGCHAR",
        DataType::Clob => "CLOB",
        DataType::Blob => "BLOB",
        DataType::Com => "COM-HANDLE",
        DataType::Class(name) => return Some(name.clone()),
        DataType::Generic(generic) => return Some(generic.class.clone()),
        DataType::PreprocIf(_) => return None,
    };
    Some(name.to_string())
}

/// `INTEGER`, `CHARACTER EXTENT 3` or `LIKE Customer.Name`.
fn type_detail(type_source: &TypeSource, extent: Option<u32>) -> Option<String> {
    let mut detail = match type_source {
        TypeSource::Explicit(data_type) => data_type_name(data_type)?,
        TypeSource::Like { source } => format!("LIKE {}", source.name),
    };
    match extent {
        None => {}
        Some(0) => detail.push_str(" EXTENT"),
        Some(n) => detail.push_str(&format!(" EXTENT {n}")),
    }
    Some(detail)
}
