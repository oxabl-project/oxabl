//! Signature help and parameter-name hints: the parameters of the routine a
//! call reaches, and which argument the cursor is in.
//!
//! The call is found in the tokens of the expanded unit, so a call that is
//! still being typed (no closing parenthesis, an empty argument) has an
//! answer. The routine comes from the analysis, as go to definition finds it:
//! the callee's name has a navigation site that resolves to a declaration. The
//! parameters are read from that declaration's text, so a signature shows what
//! the code writes.
//!
//! An answer never guesses. A call that the analysis does not resolve, or
//! resolves to a member whose overloads oxabl may not know all of, has no
//! signature. When the arguments do not pick one overload, the answer lists
//! all of them and names none as active. See `docs/design/signature-help.md`.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use oxabl_ast::{
    AccessModifier, NodeId, ParameterDirection, ParameterType, Span, Statement, StatementKind,
};
use oxabl_common::VirtualSpan;
use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{
    CallParam, CallParamKind, ClassLattice, ParamMode, PrimitiveTy, Resolution, ResolvedType,
    SiteTarget, SymbolId, SymbolKind, UnresolvedReason, call_candidates, call_param_of,
    reachable_overloads,
};

use super::folding::{Entry, Loaded, class_may_hide_members};
use super::hover::{
    Source, doc_markdown, find_declaration, find_doc, full_function_name, parameters_of, type_name,
};
use super::{Navigation, Parsed, search};
use crate::builtin_signatures::{parameter_ranges, syntaxes};
use crate::hints::{InlayHint, InlayKind};
use crate::keyword_docs::DocKind;

/// How many tokens a scan from the cursor reads before it gives up.
const MAX_SCAN: usize = 20_000;

/// The signatures of the call around a cursor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    /// Every declaration the call may reach. The one the arguments pick is
    /// first.
    pub signatures: Vec<SignatureInfo>,
    /// The index of the signature the arguments pick; `None` when they do not
    /// pick one.
    pub active_signature: Option<usize>,
}

/// One declaration a call may reach.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureInfo {
    pub label: String,
    /// The bytes of each parameter in `label`. An include that declares
    /// several parameters is one.
    pub parameters: Vec<Range<usize>>,
    /// The parameter the cursor's argument fills in this signature; `None`
    /// when it has no parameter there.
    pub active_parameter: Option<usize>,
    /// Markdown.
    pub documentation: Option<String>,
}

/// One declaration, as the resolver reads it.
struct Sig {
    info: SignatureInfo,
    /// The parameters a call passes arguments for, in order. An include
    /// contributes each of its parameters.
    params: Vec<Param>,
    /// The entries of `info.parameters`, with the arguments each one takes.
    slots: Vec<Slot>,
    /// The parameters as an overload pick reads them; `None` when one is not a
    /// plain definition.
    call: Option<Vec<CallParam>>,
    /// The last parameter takes any number of arguments, up to `max_args`.
    repeats: bool,
    max_args: Option<usize>,
    /// The declaration states the mode of each parameter. A declaration that
    /// does not (a built-in function, a catalog of built-in classes) is never
    /// the one an overload pick chooses and gets no hint.
    modes_known: bool,
}

/// One entry of the parameter list: a parameter, or an include.
#[derive(Clone)]
struct Slot {
    /// How many arguments the entry takes.
    width: usize,
    /// The include reference as written and its place in the declaring file;
    /// `None` for a parameter.
    include: Option<(String, Span)>,
}

impl Sig {
    /// The entry that takes argument `at`, and whether `at` is the first
    /// argument of that entry.
    fn slot_of(&self, at: usize) -> Option<(usize, bool)> {
        let mut first = 0;
        for (index, slot) in self.slots.iter().enumerate() {
            if at < first + slot.width {
                return Some((index, at == first));
            }
            first += slot.width;
        }
        None
    }
}

struct Param {
    /// `None` for an unnamed parameter.
    name: Option<String>,
    /// `None` when the declaration does not state it.
    mode: Option<ParamMode>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CalleeKind {
    /// `name(`: a user function, or a method of the class.
    Function,
    /// `object:name(`.
    Method,
    /// `NEW class(`.
    New,
    /// `RUN target (`, with or without `IN handle`.
    Run,
    /// `SUPER(`: the constructor of the parent class.
    Super,
    /// `THIS-OBJECT(`: another constructor of the class.
    ThisObject,
}

/// The call that a `(` opens.
#[derive(Debug)]
struct Call {
    kind: CalleeKind,
    /// The first and last token of the callee's name; a dotted class name or
    /// program name has several.
    name: (usize, usize),
    open: usize,
}

/// The arguments of a call, as token ranges, first and last token inclusive.
struct Args {
    /// `None` for an empty argument (`f(1, )`).
    items: Vec<Option<(usize, usize)>>,
    /// The `)` that closes the call, when there is one.
    close: Option<usize>,
}

fn is_word(text: &str) -> bool {
    text.chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_')
}

fn before(tokens: &[Token], from: usize) -> Option<usize> {
    (0..from).rev().find(|&i| tokens[i].kind != Kind::Comment)
}

fn after(tokens: &[Token], from: usize) -> Option<usize> {
    (from + 1..tokens.len()).find(|&i| tokens[i].kind != Kind::Comment)
}

fn adjacent(tokens: &[Token], first: usize, second: usize) -> bool {
    tokens[first].end == tokens[second].start
}

fn text_of<'t>(tokens: &[Token], text: &'t str, i: usize) -> &'t str {
    text.get(tokens[i].start..tokens[i].end).unwrap_or_default()
}

/// Whether the period at `i` ends a statement: nothing follows it, or white
/// space. A period inside a name (`customer.name`) is followed by the rest.
fn ends_statement(tokens: &[Token], text: &str, i: usize) -> bool {
    tokens[i].kind == Kind::Period
        && text[tokens[i].end..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace)
}

/// The `(` tokens that enclose the token position `end` (the tokens before
/// `end` are read), nearest first, each with the number of commas between it
/// and the cursor at its own level.
fn enclosing_opens(tokens: &[Token], text: &str, end: usize) -> Vec<(usize, usize)> {
    let mut found = Vec::new();
    let mut depth = 0usize;
    let mut commas = 0usize;
    let mut read = 0usize;
    for i in (0..end).rev() {
        let token = &tokens[i];
        if token.kind == Kind::Comment {
            continue;
        }
        read += 1;
        if read > MAX_SCAN || ends_statement(tokens, text, i) {
            break;
        }
        match token.kind {
            Kind::RightParen | Kind::RightBracket => depth += 1,
            Kind::LeftParen | Kind::LeftBracket if depth > 0 => depth -= 1,
            Kind::LeftParen => {
                found.push((i, commas));
                commas = 0;
            }
            Kind::Comma if depth == 0 => commas += 1,
            _ => {}
        }
    }
    found
}

/// The callee of the call that the `(` at `open` starts, when it is one
/// oxabl can name.
fn call_at(tokens: &[Token], text: &str, open: usize) -> Option<Call> {
    let q = before(tokens, open)?;
    if tokens[q].kind == Kind::ThisObject {
        return Some(Call {
            kind: CalleeKind::ThisObject,
            name: (q, q),
            open,
        });
    }
    if !is_word(text_of(tokens, text, q)) {
        return None;
    }
    if let Some(name) = run_target(tokens, text, q) {
        return Some(Call {
            kind: CalleeKind::Run,
            name,
            open,
        });
    }
    let mut first = q;
    while let Some(dot) = before(tokens, first)
        && tokens[dot].kind == Kind::Period
        && adjacent(tokens, dot, first)
        && let Some(part) = before(tokens, dot)
        && adjacent(tokens, part, dot)
        && is_word(text_of(tokens, text, part))
    {
        first = part;
    }
    let previous = before(tokens, first);
    let kind = match previous.map(|p| (p, tokens[p].kind)) {
        Some((p, Kind::Colon)) if first == q && adjacent(tokens, p, q) => CalleeKind::Method,
        Some((_, Kind::New)) => CalleeKind::New,
        _ if first != q => return None,
        _ if text_of(tokens, text, q).eq_ignore_ascii_case("SUPER") => CalleeKind::Super,
        _ => CalleeKind::Function,
    };
    Some(Call {
        kind,
        name: (first, q),
        open,
    })
}

/// The target of the `RUN` statement whose argument list the `(` after `from`
/// opens: `RUN p (`, `RUN p IN h (`, `RUN p PERSISTENT SET h (`.
fn run_target(tokens: &[Token], text: &str, from: usize) -> Option<(usize, usize)> {
    let mut i = from;
    for _ in 0..12 {
        match tokens[i].kind {
            Kind::Run => {
                let first = after(tokens, i)?;
                let mut last = first;
                while let Some(dot) = after(tokens, last)
                    && tokens[dot].kind == Kind::Period
                    && adjacent(tokens, last, dot)
                    && let Some(part) = after(tokens, dot)
                    && adjacent(tokens, dot, part)
                    && is_word(text_of(tokens, text, part))
                {
                    last = part;
                }
                return Some((first, last));
            }
            Kind::Period if !ends_statement(tokens, text, i) => {}
            _ if is_word(text_of(tokens, text, i)) => {}
            _ => return None,
        }
        i = before(tokens, i)?;
    }
    None
}

/// The arguments of the call that the `(` at `open` starts.
fn arguments(tokens: &[Token], text: &str, open: usize) -> Args {
    let mut items: Vec<Option<(usize, usize)>> = Vec::new();
    let mut current: Option<(usize, usize)> = None;
    let mut depth = 0usize;
    let mut close = None;
    let mut seen_comma = false;
    let mut read = 0usize;
    for i in open + 1..tokens.len() {
        let token = &tokens[i];
        if token.kind == Kind::Comment {
            continue;
        }
        read += 1;
        if read > MAX_SCAN || token.kind == Kind::Eof || ends_statement(tokens, text, i) {
            break;
        }
        match token.kind {
            Kind::RightParen if depth == 0 => {
                close = Some(i);
                break;
            }
            Kind::RightBracket if depth == 0 => break,
            Kind::Comma if depth == 0 => {
                items.push(current.take());
                seen_comma = true;
                continue;
            }
            Kind::LeftParen | Kind::LeftBracket => depth += 1,
            Kind::RightParen | Kind::RightBracket => depth -= 1,
            _ => {}
        }
        current = Some(current.map_or((i, i), |(first, _)| (first, i)));
    }
    if current.is_some() || seen_comma {
        items.push(current);
    }
    Args { items, close }
}

fn mode_of(kind: Kind) -> Option<ParamMode> {
    match kind {
        Kind::Input => Some(ParamMode::Input),
        Kind::Output => Some(ParamMode::Output),
        Kind::InputOutput => Some(ParamMode::InputOutput),
        _ => None,
    }
}

fn param_mode(direction: &ParameterDirection) -> Option<ParamMode> {
    match direction {
        ParameterDirection::Input => Some(ParamMode::Input),
        ParameterDirection::Output => Some(ParamMode::Output),
        ParameterDirection::InputOutput => Some(ParamMode::InputOutput),
        ParameterDirection::Return => None,
    }
}

fn param_name(source: &Source, statement: &Statement) -> Option<String> {
    let StatementKind::DefineParameter { param_type, .. } = &statement.kind else {
        return None;
    };
    let name = match param_type {
        ParameterType::Variable { name, .. }
        | ParameterType::Handle { name, .. }
        | ParameterType::Buffer { name, .. } => name,
    };
    // The parser names an unnamed prototype parameter `$N`.
    let name = source.name(name);
    (!name.starts_with('$')).then(|| name.to_string())
}

/// Whether the parameter lists of one declaration, read from its text, and
/// one the analysis holds for the same method are the same, as far as the
/// text shows: the same modes and shapes, and the same primitive types where
/// both are known. A class name reads as `Unknown` in the text.
fn same_list(text: &[CallParam], analysed: &[CallParam]) -> bool {
    let same_type = |a: &ResolvedType, b: &ResolvedType| match (a, b) {
        (
            ResolvedType::Unknown | ResolvedType::Class(_),
            ResolvedType::Unknown | ResolvedType::Class(_),
        ) => true,
        // The text cannot say what `LIKE` or a class name means.
        (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => true,
        (ResolvedType::Class(_), _) | (_, ResolvedType::Class(_)) => false,
        (a, b) => a == b,
    };
    text.len() == analysed.len()
        && text.iter().zip(analysed).all(|(a, b)| {
            a.mode == b.mode
                && match (&a.kind, &b.kind) {
                    (CallParamKind::Value(x), CallParamKind::Value(y)) => same_type(x, y),
                    (x, y) => std::mem::discriminant(x) == std::mem::discriminant(y),
                }
        })
}

/// Reads the routine a call reaches.
struct Resolver<'n, 's, 'a> {
    nav: &'n Navigation<'s, 'a>,
    files: RefCell<HashMap<PathBuf, Option<Rc<Loaded>>>>,
}

impl<'n, 's, 'a> Resolver<'n, 's, 'a> {
    fn new(nav: &'n Navigation<'s, 'a>) -> Self {
        Resolver {
            nav,
            files: RefCell::new(HashMap::new()),
        }
    }

    /// A file's text and statements. `None` when it cannot be read, or the
    /// parser recovered from an error in it: its declarations are then not
    /// all known.
    fn load(&self, path: &Path) -> Option<Rc<Loaded>> {
        if let Some(found) = self.files.borrow().get(path) {
            return found.clone();
        }
        let loaded = self.nav.run.file_system().read(path).ok().and_then(|text| {
            let parsed = Parsed::of(&text);
            parsed
                .program
                .errors
                .is_empty()
                .then(|| Rc::new(Loaded::new(text, parsed)))
        });
        self.files
            .borrow_mut()
            .insert(path.to_path_buf(), loaded.clone());
        loaded
    }

    /// The navigation sites that end where the callee's name ends and start
    /// inside it.
    fn sites_of(&self, tokens: &[Token], call: &Call) -> Vec<&'n oxabl_semantic::Site> {
        let (first, last) = (
            tokens[call.name.0].start as u32,
            tokens[call.name.1].end as u32,
        );
        let all = self.nav.model.semantic.symbols.sites();
        self.nav
            .site_index()
            .holding(first)
            .map(|at| &all[at])
            .filter(|site| site.span.end == last && site.span.start >= first)
            .collect()
    }

    fn resolve(&self, tokens: &[Token], text: &str, call: &Call) -> Option<Vec<Sig>> {
        let references = &self.nav.model.semantic.references;
        match call.kind {
            CalleeKind::Function | CalleeKind::Method => {
                for site in self.sites_of(tokens, call) {
                    let SiteTarget::Reference(node) = site.target else {
                        continue;
                    };
                    return match references.get(node)? {
                        Resolution::Resolved(sym) => self.symbol_sigs(node, *sym),
                        Resolution::Unresolved {
                            name,
                            reason: UnresolvedReason::External,
                        } if call.kind == CalleeKind::Function => {
                            self.builtin_sigs(node, name.as_ref())
                        }
                        Resolution::Unresolved { .. } => None,
                    };
                }
                None
            }
            CalleeKind::New => {
                let site = self
                    .sites_of(tokens, call)
                    .into_iter()
                    .find_map(|site| match &site.target {
                        SiteTarget::Type(Resolution::Resolved(sym)) => Some(*sym),
                        _ => None,
                    })?;
                self.constructor_sigs(site, ConstructorAccess::Public)
            }
            CalleeKind::Run => self.run_sigs(tokens, text, call),
            CalleeKind::ThisObject | CalleeKind::Super => {
                self.chained_constructor_sigs(tokens, call)
            }
        }
    }

    /// The declarations of the function or method `sym`.
    fn symbol_sigs(&self, node: NodeId, sym: SymbolId) -> Option<Vec<Sig>> {
        let symbols = &self.nav.model.semantic.symbols;
        if symbols.get(sym).kind != SymbolKind::Function || symbols.is_untyped_member(node) {
            return None;
        }
        let mut sigs: Vec<Sig> = Vec::new();
        let mut by_file: Vec<(oxabl_semantic::IndexedFileId, Vec<SymbolId>)> = Vec::new();
        // Candidates that neither the unit nor a class file of the workspace
        // declares.
        let mut catalog: Vec<SymbolId> = Vec::new();
        for candidate in call_candidates(symbols, sym) {
            let symbol = symbols.get(candidate);
            if let Some(file) = symbols.member_file(candidate) {
                match by_file.iter_mut().find(|(known, _)| *known == file) {
                    Some((_, list)) => list.push(candidate),
                    None => by_file.push((file, vec![candidate])),
                }
            } else if symbol.declaration != NodeId::DUMMY {
                sigs.push(self.unit_sig(symbol.name_span)?);
            } else {
                catalog.push(candidate);
            }
        }
        let name = symbols.get(sym).name.as_ref();
        for (file, candidates) in by_file {
            sigs.extend(self.member_sigs(file, name, &candidates)?);
        }
        if !catalog.is_empty() {
            sigs.extend(self.builtin_class_sigs(&catalog)?);
        }
        dedupe(&mut sigs);
        (!sigs.is_empty()).then_some(sigs)
    }

    /// The declaration of the routine whose name is at `name` in the unit.
    fn unit_sig(&self, name: VirtualSpan) -> Option<Sig> {
        let parsed = self.nav.parsed();
        let folding = self.nav.folding();
        let source = Source::new(
            &self.nav.expanded.text,
            &parsed.tokens,
            &parsed.program.pools,
        )
        .folded(&folding);
        let found = find_declaration(&parsed.program.statements, name)?;
        sig_of(
            &source,
            found.statement,
            &parsed.program.statements,
            None,
            None,
        )
    }

    /// The declarations of method `name` in class file `file` that match the
    /// analysed `candidates`. `None` when a candidate has no match in the text,
    /// or its parameter list is not known: the file's text and the analysis
    /// then disagree.
    fn member_sigs(
        &self,
        file: oxabl_semantic::IndexedFileId,
        name: &str,
        candidates: &[SymbolId],
    ) -> Option<Vec<Sig>> {
        let symbols = &self.nav.model.semantic.symbols;
        let path = self.nav.path_of(file)?;
        let loaded = self.load(&path)?;
        let scan = loaded.class_scan()?;
        if scan.hides_members {
            return None;
        }
        let includes = &scan.includes;
        let view = loaded.view(self.nav, &path, includes.in_parameters)?;
        let source = view.source();
        let (_, class, body) = class_of(&source, view.statements)?;
        let declared: Vec<Sig> = body
            .iter()
            .filter(|member| {
                matches!(&member.kind, StatementKind::Method { name: n, .. }
                    if source.name(n).eq_ignore_ascii_case(name))
                    && view.written_here(member.span.start)
            })
            .filter_map(|member| sig_of(&source, member, &[], None, Some(class.as_str())))
            .collect();
        let mut out = Vec::new();
        for &candidate in candidates {
            let analysed = symbols.method_params(candidate)?;
            let mut matched = false;
            for sig in &declared {
                if sig
                    .call
                    .as_deref()
                    .is_some_and(|text| same_list(text, analysed))
                {
                    matched = true;
                    out.push(sig_clone(sig));
                }
            }
            if !matched {
                return None;
            }
        }
        Some(out)
    }

    /// The signatures of `candidates`, the methods of a built-in class that no
    /// file declares. A catalog of the members of built-in classes answers
    /// here. None does yet, so such a call has no signature.
    fn builtin_class_sigs(&self, _candidates: &[SymbolId]) -> Option<Vec<Sig>> {
        None
    }

    /// The built-in function `name` called at `node`.
    fn builtin_sigs(&self, node: NodeId, name: &str) -> Option<Vec<Sig>> {
        let full = full_function_name(name);
        let doc = find_doc(DocKind::Function, full)?;
        if !doc.reserved && self.nav.may_hide_routines(node) {
            return None;
        }
        let documentation = doc_markdown(doc);
        let sigs: Vec<Sig> = syntaxes(full)
            .map(|builtin| Sig {
                info: SignatureInfo {
                    label: builtin.label.to_string(),
                    parameters: parameter_ranges(builtin),
                    active_parameter: None,
                    documentation: Some(documentation.clone()),
                },
                params: builtin
                    .params
                    .iter()
                    .map(|_| Param {
                        name: None,
                        mode: None,
                    })
                    .collect(),
                slots: builtin
                    .params
                    .iter()
                    .map(|_| Slot {
                        width: 1,
                        include: None,
                    })
                    .collect(),
                call: None,
                repeats: builtin.repeats,
                max_args: builtin.max_args,
                modes_known: false,
            })
            .collect();
        (!sigs.is_empty()).then_some(sigs)
    }

    /// The constructors of class `class` that `access` reaches.
    fn constructor_sigs(&self, class: SymbolId, access: ConstructorAccess) -> Option<Vec<Sig>> {
        let symbols = &self.nav.model.semantic.symbols;
        let symbol = symbols.get(class);
        if let Some(file) = symbols.class_file(class) {
            let path = self.nav.path_of(file)?;
            let loaded = self.load(&path)?;
            let scan = loaded.class_scan()?;
            if scan.hides_members {
                return None;
            }
            // An include outside the parameter lists may declare a constructor.
            let includes = &scan.includes;
            if !includes.elsewhere.is_empty() {
                return None;
            }
            let view = loaded.view(self.nav, &path, includes.in_parameters)?;
            let source = view.source();
            let (_, name, body) = class_of(&source, view.statements)?;
            return constructors(
                &source,
                &name,
                body,
                access,
                self.caller_class().as_deref(),
                &|at| view.written_here(at),
            );
        }
        if symbol.declaration == NodeId::DUMMY {
            return self.builtin_constructor_sigs(class);
        }
        if symbol.kind != SymbolKind::Class {
            return None;
        }
        let parsed = self.nav.parsed();
        let folding = self.nav.folding();
        let source = Source::new(
            &self.nav.expanded.text,
            &parsed.tokens,
            &parsed.program.pools,
        )
        .folded(&folding);
        let found = find_declaration(&parsed.program.statements, symbol.name_span)?;
        let StatementKind::Class { name, body, .. } = &found.statement.kind else {
            return None;
        };
        if class_may_hide_members(&source, found.statement, true) {
            return None;
        }
        constructors(
            &source,
            source.name(name),
            body,
            ConstructorAccess::Own,
            None,
            &|_| true,
        )
    }

    /// The constructors of `class`, a built-in class that no file declares.
    /// The catalog of the members of built-in classes answers here, as it does
    /// for the methods in `builtin_class_sigs`. None does yet, so `NEW` and
    /// `SUPER(` of such a class have no signature. The signatures it gives
    /// state no modes (`Sig::modes_known` is `false`).
    fn builtin_constructor_sigs(&self, _class: SymbolId) -> Option<Vec<Sig>> {
        None
    }

    /// The declared name of the class that the unit defines.
    fn caller_class(&self) -> Option<String> {
        let parsed = self.nav.parsed();
        let source = Source::new(
            &self.nav.expanded.text,
            &parsed.tokens,
            &parsed.program.pools,
        );
        class_of(&source, &parsed.program.statements).map(|(_, name, _)| name)
    }

    /// The constructors that `SUPER(` or `THIS-OBJECT(` may call.
    fn chained_constructor_sigs(&self, tokens: &[Token], call: &Call) -> Option<Vec<Sig>> {
        let at = tokens[call.open].start as u32;
        let parsed = self.nav.parsed();
        let class = parsed.program.statements.iter().find(|s| {
            matches!(s.kind, StatementKind::Class { .. }) && s.span.start <= at && at < s.span.end
        })?;
        let StatementKind::Class {
            name,
            inherits,
            body,
            ..
        } = &class.kind
        else {
            return None;
        };
        if call.kind == CalleeKind::ThisObject {
            let folding = self.nav.folding();
            let source = Source::new(
                &self.nav.expanded.text,
                &parsed.tokens,
                &parsed.program.pools,
            )
            .folded(&folding);
            if class_may_hide_members(&source, class, true) {
                return None;
            }
            return constructors(
                &source,
                source.name(name),
                body,
                ConstructorAccess::Own,
                None,
                &|_| true,
            );
        }
        let parent = inherits.as_ref()?;
        let symbols = &self.nav.model.semantic.symbols;
        let sym = symbols.sites().iter().find_map(|site| match &site.target {
            SiteTarget::Type(Resolution::Resolved(sym))
                if site.span.start == parent.span.start && site.span.end == parent.span.end =>
            {
                Some(*sym)
            }
            _ => None,
        })?;
        self.constructor_sigs(sym, ConstructorAccess::Inherited)
    }

    /// The parameters of the procedure that a `RUN` reaches.
    fn run_sigs(&self, tokens: &[Token], text: &str, call: &Call) -> Option<Vec<Sig>> {
        let references = &self.nav.model.semantic.references;
        let symbols = &self.nav.model.semantic.symbols;
        let written: String = (call.name.0..=call.name.1)
            .filter(|&i| tokens[i].kind != Kind::Comment)
            .map(|i| text_of(tokens, text, i))
            .collect();
        let name_span = VirtualSpan::new(
            tokens[call.name.0].start as u32,
            tokens[call.name.1].end as u32,
        );
        for site in self.sites_of(tokens, call) {
            let SiteTarget::Run(node) = site.target else {
                continue;
            };
            let Some(Resolution::Resolved(sym)) = references.get(node) else {
                if let Some(targets) = self.nav.handle_call_targets(name_span) {
                    return self.handle_sig(&targets);
                }
                return None;
            };
            if let Some(file) = symbols.program_file(*sym) {
                let path = self.nav.path_of(file)?;
                return self.program_sig(&path, &written);
            }
            if let Some(targets) = self.nav.handle_call_targets(name_span) {
                return self.handle_sig(&targets);
            }
            let symbol = symbols.get(*sym);
            if symbol.kind == SymbolKind::Procedure && symbol.declaration != NodeId::DUMMY {
                return Some(vec![self.unit_sig(symbol.name_span)?]);
            }
            return None;
        }
        None
    }

    /// The one procedure that `RUN p IN h` runs.
    fn handle_sig(&self, targets: &[super::Target]) -> Option<Vec<Sig>> {
        let [
            super::Target {
                path,
                span: Some(span),
            },
        ] = targets
        else {
            return None;
        };
        let loaded = self.load(path)?;
        let raw = Source::new(
            &loaded.text,
            &loaded.parsed.tokens,
            &loaded.parsed.program.pools,
        );
        let written = find_declaration(
            &loaded.parsed.program.statements,
            VirtualSpan::new(span.start, span.end),
        )?;
        let (from, to) = (written.statement.span.start, written.statement.span.end);
        if raw.may_expand(from, to, &[], false) {
            return None;
        }
        let includes = raw.includes_in(from, to, &[]);
        let view = loaded.view(self.nav, path, !includes.is_empty())?;
        let found = find_declaration(view.statements, view.locate(*span)?)?;
        let sig = sig_of(&view.source(), found.statement, view.statements, None, None)?;
        accounts_for(&sig, &includes).then(|| vec![sig])
    }

    /// The parameters that the main block of the program at `path` defines.
    fn program_sig(&self, path: &Path, written: &str) -> Option<Vec<Sig>> {
        let loaded = self.load(path)?;
        let statements = &loaded.parsed.program.statements;
        let raw = Source::new(
            &loaded.text,
            &loaded.parsed.tokens,
            &loaded.parsed.program.pools,
        );
        if program_may_hide_parameters(&raw, statements, false) {
            return None;
        }
        let includes = program_includes(&raw, statements);
        let view = loaded.view(self.nav, path, !includes.is_empty())?;
        let sig = signature(
            &view.source(),
            written,
            None,
            parameters_of(view.statements),
            None,
        )?;
        // An include in the main block that declares no parameter may still
        // declare something the compiler reads as one.
        accounts_for(&sig, &includes).then(|| vec![sig])
    }

    /// The type of the argument in `range`, when its text alone gives it: a
    /// literal, or a name that resolves to a variable or parameter.
    fn arg_type(
        &self,
        tokens: &[Token],
        text: &str,
        range: Option<(usize, usize)>,
    ) -> ResolvedType {
        let Some((mut first, last)) = range else {
            return ResolvedType::Unknown;
        };
        if mode_of(tokens[first].kind).is_some() {
            match after(tokens, first) {
                Some(next) if next <= last => first = next,
                _ => return ResolvedType::Unknown,
            }
        }
        if first != last {
            return ResolvedType::Unknown;
        }
        use PrimitiveTy::*;
        let token = &tokens[first];
        let primitive = |p| ResolvedType::Primitive(p);
        match token.kind {
            Kind::IntegerLiteral => primitive(Integer),
            Kind::BigIntLiteral => primitive(Int64),
            Kind::DecimalLiteral => primitive(Decimal),
            Kind::StringLiteral => primitive(Character),
            Kind::DateLiteral => primitive(Date),
            Kind::Yes | Kind::No => primitive(Logical),
            Kind::Identifier => {
                let word = text_of(tokens, text, first);
                if word.eq_ignore_ascii_case("TRUE") || word.eq_ignore_ascii_case("FALSE") {
                    return primitive(Logical);
                }
                let semantic = &self.nav.model.semantic;
                let span = (token.start as u32, token.end as u32);
                self.nav
                    .site_index()
                    .holding(span.0)
                    .map(|at| &semantic.symbols.sites()[at])
                    .filter(|site| (site.span.start, site.span.end) == span)
                    .find_map(|site| match site.target {
                        SiteTarget::Reference(node) => match semantic.references.get(node)? {
                            Resolution::Resolved(sym) => {
                                let symbol = semantic.symbols.get(*sym);
                                matches!(
                                    symbol.kind,
                                    SymbolKind::Variable
                                        | SymbolKind::Parameter
                                        | SymbolKind::Property
                                )
                                .then(|| symbol.data_type.clone())
                                .flatten()
                            }
                            Resolution::Unresolved { .. } => None,
                        },
                        _ => None,
                    })
                    .unwrap_or(ResolvedType::Unknown)
            }
            _ => ResolvedType::Unknown,
        }
    }

    /// Which of `sigs` the arguments reach, as indices.
    fn reach(
        &self,
        tokens: &[Token],
        text: &str,
        sigs: &[Sig],
        args: &Args,
    ) -> oxabl_semantic::CallReach {
        let types: Vec<ResolvedType> = args
            .items
            .iter()
            .map(|range| self.arg_type(tokens, text, *range))
            .collect();
        let lists: Vec<(Option<&[CallParam]>, bool)> = sigs
            .iter()
            .map(|s| (s.call.as_deref(), s.modes_known))
            .collect();
        reachable_overloads(
            &lists,
            &types,
            ClassLattice::new(&self.nav.model.semantic.symbols),
        )
    }
}

/// Which constructors a call may reach.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ConstructorAccess {
    /// `NEW` from outside the class.
    Public,
    /// `SUPER(`: the parent's.
    Inherited,
    /// `THIS-OBJECT(`, and `NEW` of the unit's own class.
    Own,
}

impl ConstructorAccess {
    /// Whether a constructor declared `declared` in `class` is reached from
    /// the class `caller` (`None` outside a class). `None` when oxabl cannot
    /// tell: a package access in the unnamed package.
    fn reaches(self, declared: AccessModifier, class: &str, caller: Option<&str>) -> Option<bool> {
        use AccessModifier::*;
        let package = |name: &str| {
            name.rsplit_once('.')
                .map(|(package, _)| package.to_string())
        };
        // Both in packages: the same one or not. One in a package: not the
        // same. Both in the unnamed package: a package access there is
        // compile error 19477, so no answer.
        let same_package = || match (package(class), caller.and_then(package)) {
            (Some(a), Some(b)) => Some(a.eq_ignore_ascii_case(&b)),
            (Some(_), None) | (None, Some(_)) => Some(false),
            (None, None) => None,
        };
        match (self, declared) {
            (ConstructorAccess::Own, _) | (_, Public) => Some(true),
            (_, Private) => Some(false),
            (ConstructorAccess::Inherited, Protected | PackageProtected) => Some(true),
            (ConstructorAccess::Public, Protected) => Some(false),
            (_, PackagePrivate | PackageProtected) => same_package(),
        }
    }
}

/// The non-static constructors in `body` that `access` reaches from the class
/// `caller`. `None` when one is reached only by a package access that oxabl cannot
/// compare, or when none is reached.
fn constructors(
    source: &Source,
    class: &str,
    body: &[Statement],
    access: ConstructorAccess,
    caller: Option<&str>,
    written_here: &dyn Fn(u32) -> bool,
) -> Option<Vec<Sig>> {
    let short = class.rsplit('.').next().unwrap_or(class);
    let mut sigs: Vec<Sig> = Vec::new();
    for member in body {
        let StatementKind::Constructor {
            access: declared,
            is_static: false,
            ..
        } = &member.kind
        else {
            continue;
        };
        // A constructor that an include declares is not read.
        if !written_here(member.span.start) {
            continue;
        }
        if access.reaches(*declared, class, caller)? {
            sigs.push(sig_of(source, member, &[], Some(short), None)?);
        }
    }
    dedupe(&mut sigs);
    (!sigs.is_empty()).then_some(sigs)
}

/// The first class or interface in `statements`: the statement, its name and
/// its body.
fn class_of<'s>(
    source: &Source,
    statements: &'s [Statement],
) -> Option<(&'s Statement, String, &'s [Statement])> {
    statements.iter().find_map(|s| match &s.kind {
        StatementKind::Class { name, body, .. } | StatementKind::Interface { name, body, .. } => {
            Some((s, source.name(name).to_string(), body.as_slice()))
        }
        _ => None,
    })
}

/// The signature of a procedure, function, method or constructor.
/// `class` names the constructor's class; `owner` the class that declares a
/// member.
///
/// `scope` holds the statements that a `FORWARD` prototype of a function may
/// be among.
fn sig_of(
    source: &Source,
    statement: &Statement,
    scope: &[Statement],
    class: Option<&str>,
    owner: Option<&str>,
) -> Option<Sig> {
    let (name, parameters, returns): (String, Vec<&Statement>, Option<String>) =
        match &statement.kind {
            StatementKind::Method {
                name,
                parameters,
                return_type,
                ..
            } => (
                source.name(name).to_string(),
                parameters_of(parameters),
                match return_type {
                    None => Some("VOID".to_string()),
                    Some(data_type) => type_name(data_type),
                },
            ),
            StatementKind::Function {
                name, return_type, ..
            } => {
                if source.may_expand(statement.span.start, statement.span.end, &[], true) {
                    return None;
                }
                (
                    source.name(name).to_string(),
                    function_parameters(source, statement, scope),
                    type_name(return_type),
                )
            }
            StatementKind::Procedure { name, body, .. } => {
                if source.may_expand(statement.span.start, statement.span.end, &[], true) {
                    return None;
                }
                (source.name(name).to_string(), parameters_of(body), None)
            }
            StatementKind::Constructor { parameters, .. } => {
                (class?.to_string(), parameters_of(parameters), None)
            }
            _ => return None,
        };
    signature(source, &name, returns, parameters, owner)
}

/// The procedures and functions of a main block, as byte ranges.
fn routine_ranges(statements: &[Statement]) -> Vec<(u32, u32)> {
    statements
        .iter()
        .filter(|s| {
            matches!(
                s.kind,
                StatementKind::Procedure { .. } | StatementKind::Function { .. }
            )
        })
        .map(|s| (s.span.start, s.span.end))
        .collect()
}

/// Whether the main block of a file may define parameters the text does not
/// show: a `{&name}` reference or `&IF` outside its procedures and functions,
/// and when `includes` is set an include.
fn program_may_hide_parameters(source: &Source, statements: &[Statement], includes: bool) -> bool {
    source.may_expand(0, u32::MAX, &routine_ranges(statements), includes)
}

/// The includes that the main block of a file writes outside its procedures
/// and functions.
fn program_includes(source: &Source, statements: &[Statement]) -> Vec<Span> {
    source.includes_in(0, u32::MAX, &routine_ranges(statements))
}

/// Whether every include of `includes` is one entry of the parameter list of
/// `sig`: an include that declares no parameter is not known to declare none.
fn accounts_for(sig: &Sig, includes: &[Span]) -> bool {
    includes.iter().all(|site| {
        sig.slots
            .iter()
            .any(|slot| slot.include.as_ref().is_some_and(|(_, at)| at == site))
    })
}

/// The parameters of the function `statement`: those of its `FORWARD` (or
/// `IN`) prototype, those its body defines, or, for a definition that leaves
/// out its list after a `FORWARD` in `scope`, those of that prototype.
fn function_parameters<'a>(
    source: &Source,
    statement: &'a Statement,
    scope: &'a [Statement],
) -> Vec<&'a Statement> {
    let StatementKind::Function {
        name,
        body,
        prototype_parameters: prototype,
        ..
    } = &statement.kind
    else {
        return Vec::new();
    };
    let name = source.name(name);
    if !prototype.is_empty() {
        return parameters_of(prototype);
    }
    let own = parameters_of(body);
    if !own.is_empty() {
        return own;
    }
    let forward = scope.iter().find_map(|s| match &s.kind {
        StatementKind::Function {
            name: other,
            body,
            prototype_parameters,
            ..
        } if s.id != statement.id
            && body.is_empty()
            && source.name(other).eq_ignore_ascii_case(name) =>
        {
            Some(prototype_parameters)
        }
        _ => None,
    });
    forward.map_or_else(Vec::new, |list| parameters_of(list))
}

/// `name(INPUT a AS INTEGER, OUTPUT b AS CHARACTER): VOID`.
fn signature(
    source: &Source,
    name: &str,
    returns: Option<String>,
    parameters: Vec<&Statement>,
    owner: Option<&str>,
) -> Option<Sig> {
    let mut label = format!("{name}(");
    let mut ranges = Vec::new();
    let mut params = Vec::new();
    let mut slots = Vec::new();
    let mut call = Some(Vec::new());
    for entry in source.entries(parameters)? {
        let (text, statements, include) = match entry {
            Entry::Param(statement) => (source.parameter(statement)?, vec![statement], None),
            Entry::Include(group) => {
                if group.count() == 0 {
                    continue;
                }
                let include = (group.text.clone(), group.site);
                (group.label(), group.params, Some(include))
            }
        };
        let mut width = 0;
        for statement in statements {
            let StatementKind::DefineParameter { direction, .. } = &statement.kind else {
                return None;
            };
            // A `RETURN` parameter is not an argument.
            let Some(mode) = param_mode(direction) else {
                continue;
            };
            width += 1;
            params.push(Param {
                name: param_name(source, statement),
                mode: Some(mode),
            });
            call = match (call, call_param_of(statement, source.pools())) {
                (Some(mut list), Some(param)) => {
                    list.push(param);
                    Some(list)
                }
                _ => None,
            };
        }
        if width == 0 {
            continue;
        }
        if !ranges.is_empty() {
            label.push_str(", ");
        }
        let start = label.len();
        label.push_str(&text);
        ranges.push(start..label.len());
        slots.push(Slot { width, include });
    }
    label.push(')');
    if let Some(returns) = returns {
        label.push_str(": ");
        label.push_str(&returns);
    }
    Some(Sig {
        info: SignatureInfo {
            label,
            parameters: ranges,
            active_parameter: None,
            documentation: owner.map(|class| format!("Declared in {class}.")),
        },
        params,
        slots,
        call,
        repeats: false,
        max_args: None,
        modes_known: true,
    })
}

fn sig_clone(sig: &Sig) -> Sig {
    Sig {
        info: sig.info.clone(),
        params: sig
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                mode: p.mode,
            })
            .collect(),
        slots: sig.slots.clone(),
        call: sig.call.clone(),
        repeats: sig.repeats,
        max_args: sig.max_args,
        modes_known: sig.modes_known,
    }
}

/// Keep the first of the signatures that have one label: an override and the
/// method it overrides read the same.
fn dedupe(sigs: &mut Vec<Sig>) {
    let mut seen: Vec<String> = Vec::new();
    sigs.retain(|sig| {
        let own = sig.info.label.clone();
        let fresh = !seen.contains(&own);
        seen.push(own);
        fresh
    });
}

/// Whether a token of this kind can be the name of a function that a `(`
/// calls. A keyword that is no function (`IF`, `NOT`, `AND`, `WHEN`) opens a
/// grouping, not a call, unless a user function has its name.
fn can_name_a_call(kind: Kind, word: &str) -> bool {
    kind == Kind::Identifier
        || oxabl_lexer::is_callable_kind(kind)
        || oxabl_lexer::is_builtin_function(&word.to_ascii_lowercase())
}

/// The index of the declaration the call reaches, when it is sure and the
/// declaration states its modes: the pick reads them.
fn picked_sig(sigs: &[Sig], reach: &oxabl_semantic::CallReach) -> Option<usize> {
    reach.picked.filter(|&at| sigs[at].modes_known)
}

/// Whether an argument that is the name `argument` repeats what the hint for
/// parameter `parameter` would say. That is the same name, or the same name
/// with the prefix of the naming convention of ABL code (`pcName` for
/// `cName`, `ipiCount` for `iCount`), or a name that is the last word of the
/// other (`Name` and `pcName`; `custName` and `Name`; `cust-num` and
/// `ip-cust-num`). A word starts at the start of the name, after `-` or `_`,
/// or at an upper-case letter after a lower-case one. Ignores case.
fn says_the_name(argument: &str, parameter: &str) -> bool {
    if argument.eq_ignore_ascii_case(parameter) {
        return true;
    }
    // A name that short is more likely a coincidence than a repeat.
    const SHORT: usize = 3;
    let ends_with = |name: &str, end: &str| {
        let Some(start) = name
            .len()
            .checked_sub(end.len())
            .filter(|_| end.len() >= SHORT)
        else {
            return false;
        };
        let bytes = name.as_bytes();
        let at_word_start = start == 0
            || matches!(bytes[start - 1], b'-' | b'_')
            || (bytes[start - 1].is_ascii_lowercase() && bytes[start].is_ascii_uppercase());
        name.is_char_boundary(start) && at_word_start && name[start..].eq_ignore_ascii_case(end)
    };
    if ends_with(parameter, argument) {
        return true;
    }
    let lower = parameter.to_ascii_lowercase();
    let Some(prefix) = ["iop", "ip", "op", "p"]
        .into_iter()
        .find(|p| lower.starts_with(p) && lower.len() > p.len())
    else {
        return false;
    };
    let unprefixed = &parameter[prefix.len()..];
    if argument.eq_ignore_ascii_case(unprefixed) {
        return true;
    }
    // One letter for the type, then the word that names the value.
    match unprefixed.as_bytes() {
        [type_letter, next, ..]
            if type_letter.is_ascii_lowercase() && next.is_ascii_uppercase() =>
        {
            ends_with(argument, &unprefixed[1..])
        }
        _ => false,
    }
}

impl Navigation<'_, '_> {
    /// The signatures of the call around byte `offset` of `file`, and the
    /// argument the cursor is in. `None` when the cursor is in no call, the
    /// call does not resolve, or oxabl cannot tell all the declarations it may
    /// reach.
    pub fn signature_help(&self, file: &Path, offset: u32) -> Option<SignatureHelp> {
        let file = search::normalize_lexically(file);
        // The cursor directly before an include of arguments asks about its
        // first argument, so only a cursor inside the reference is refused.
        if self.include_inside(&file, offset).is_some() {
            return None;
        }
        let virt = self.expanded_at(&file, offset).or_else(|| {
            self.expanded_at(&file, offset.checked_sub(1)?)
                .map(|v| v + 1)
        })?;
        let tokens = &self.parsed().tokens;
        let text = self.expanded.text.as_str();
        let end = tokens.partition_point(|t| t.start < virt as usize);
        let resolver = Resolver::new(self);
        for (open, commas) in enclosing_opens(tokens, text, end) {
            let Some(call) = call_at(tokens, text, open) else {
                continue;
            };
            // `IF (`, `NOT (`, `WHEN (`: a keyword that is no function and has no
            // site in the analysis opens no call, so the call around it is the
            // one asked for. A user function may have a keyword's name.
            if call.kind == CalleeKind::Function
                && !can_name_a_call(tokens[call.name.1].kind, text_of(tokens, text, call.name.1))
                && resolver.sites_of(tokens, &call).is_empty()
            {
                continue;
            }
            let mut sigs = resolver.resolve(tokens, text, &call)?;
            let args = arguments(tokens, text, open);
            let active = match sigs.len() {
                1 => Some(0),
                _ if args.items.iter().any(Option::is_none) => None,
                _ => picked_sig(&sigs, &resolver.reach(tokens, text, &sigs, &args)),
            };
            if let Some(at) = active.filter(|at| *at > 0) {
                let picked = sigs.remove(at);
                sigs.insert(0, picked);
            }
            let signatures = sigs
                .into_iter()
                .map(|sig| {
                    let count = sig.info.parameters.len();
                    let active_parameter = match sig.slot_of(commas) {
                        Some((slot, _)) => Some(slot),
                        None if sig.repeats
                            && count > 0
                            && sig.max_args.is_none_or(|m| commas < m) =>
                        {
                            Some(count - 1)
                        }
                        None => None,
                    };
                    SignatureInfo {
                        active_parameter,
                        ..sig.info
                    }
                })
                .collect();
            return Some(SignatureHelp {
                signatures,
                active_signature: active.map(|_| 0),
            });
        }
        None
    }

    /// The parameter-name hints of `file`: before each argument of a call
    /// whose one target is known, the name of the parameter it fills.
    pub fn parameter_hints(&self, file: &Path) -> Vec<InlayHint> {
        let file = search::normalize_lexically(file);
        let tokens = &self.parsed().tokens;
        let text = self.expanded.text.as_str();
        let resolver = Resolver::new(self);
        let mut hints = Vec::new();
        for (open, token) in tokens.iter().enumerate() {
            if token.kind != Kind::LeftParen {
                continue;
            }
            let Some(call) = call_at(tokens, text, open) else {
                continue;
            };
            let args = arguments(tokens, text, open);
            if args.close.is_none() || args.items.iter().any(Option::is_none) {
                continue;
            }
            let Some(sigs) = resolver.resolve(tokens, text, &call) else {
                continue;
            };
            let target = match sigs.len() {
                1 => Some(0),
                _ => picked_sig(&sigs, &resolver.reach(tokens, text, &sigs, &args)),
            };
            let Some(sig) = target.map(|at| &sigs[at]).filter(|sig| sig.modes_known) else {
                continue;
            };
            for (index, arg) in args.items.iter().enumerate() {
                let (Some((first, last)), Some((slot, starts_slot))) = (arg, sig.slot_of(index))
                else {
                    continue;
                };
                let included = sig.slots[slot].include.as_ref();
                // The arguments that stand for the parameters of an include get
                // one hint, on the first of them.
                if included.is_some() && !starts_slot {
                    continue;
                }
                let param = &sig.params[index];
                let written = mode_of(tokens[*first].kind);
                let body = if written.is_some() {
                    after(tokens, *first).filter(|next| next <= last)
                } else {
                    Some(*first)
                };
                let (label, tooltip) = match (included, &param.name) {
                    (Some((text, _)), _) => (
                        format!("{text}:"),
                        format!("Parameters declared by `{text}` in `{}`.", sig.info.label),
                    ),
                    (None, Some(name)) => {
                        // The argument already says the name.
                        if body == Some(*last) && says_the_name(text_of(tokens, text, *last), name)
                        {
                            continue;
                        }
                        let label = match (param.mode, written) {
                            (Some(ParamMode::Output), None) => format!("OUTPUT {name}:"),
                            (Some(ParamMode::InputOutput), None) => {
                                format!("INPUT-OUTPUT {name}:")
                            }
                            _ => format!("{name}:"),
                        };
                        (
                            label,
                            format!("Parameter `{name}` of `{}`.", sig.info.label),
                        )
                    }
                    (None, None) => continue,
                };
                let Some(place) = self.place(VirtualSpan::new(
                    tokens[*first].start as u32,
                    tokens[*first].end as u32,
                )) else {
                    continue;
                };
                let (Some(span), true) = (place.span, place.path == file) else {
                    continue;
                };
                hints.push(InlayHint {
                    at: span.start,
                    kind: InlayKind::Parameter,
                    label,
                    tooltip,
                    after: false,
                });
            }
        }
        hints.sort_by_key(|hint| hint.at);
        hints.dedup();
        hints
    }
}

/// `text` with a call that is still being typed closed, so that it parses:
/// the parentheses still open at the end of the cursor's line, a period, and
/// an unknown value for an empty argument.
/// `None` when the cursor is in no call. Every offset before the cursor keeps
/// its place.
///
/// A statement that lacks its period swallows the statement after it, and the
/// analysis then has no name for the call, so no signature. An editor closes
/// the parenthesis it opens; the period comes last.
pub fn close_open_call(text: &str, offset: u32) -> Option<String> {
    if !text.is_char_boundary(offset as usize) {
        return None;
    }
    let tokens = oxabl_lexer::tokenize(text);
    let end = tokens.partition_point(|t| t.start < offset as usize);
    enclosing_opens(&tokens, text, end)
        .into_iter()
        .find(|(open, _)| call_at(&tokens, text, *open).is_some())?;
    let line_end = text[offset as usize..]
        .find('\n')
        .map_or(text.len(), |at| offset as usize + at);
    let line_end = if text[..line_end].ends_with('\r') {
        line_end - 1
    } else {
        line_end
    };
    // From the start of the statement to the end of the line.
    let mut first = end;
    let mut read = 0usize;
    while first > 0 && read < MAX_SCAN {
        let before = first - 1;
        if ends_statement(&tokens, text, before) {
            break;
        }
        first = before;
        read += 1;
    }
    let mut open = 0usize;
    for token in &tokens[first..] {
        if token.start >= line_end {
            break;
        }
        match token.kind {
            Kind::LeftParen => open += 1,
            Kind::RightParen => open = open.saturating_sub(1),
            _ => {}
        }
    }
    // An empty argument (`f(1, )`) does not parse in a `RUN`.
    let empty = before(&tokens, end).is_some_and(|b| tokens[b].kind == Kind::Comma)
        && tokens
            .get(end)
            .filter(|t| t.start < line_end)
            .is_none_or(|t| matches!(t.kind, Kind::RightParen | Kind::Comma));
    // The call and its statement end on this line: closing changes nothing,
    // so a second analysis would give the answer of the first.
    let last = (first..tokens.len())
        .take_while(|&i| tokens[i].start < line_end)
        .filter(|&i| tokens[i].kind != Kind::Comment)
        .last();
    let terminated = last.is_some_and(|i| ends_statement(&tokens, text, i));
    if open == 0 && !empty && terminated {
        return None;
    }
    let mut closed = String::with_capacity(text.len() + open + 2);
    closed.push_str(&text[..offset as usize]);
    if empty {
        closed.push('?');
    }
    closed.push_str(&text[offset as usize..line_end]);
    closed.push_str(&")".repeat(open));
    closed.push('.');
    closed.push_str(&text[line_end..]);
    Some(closed)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sig(modes_known: bool) -> Sig {
        Sig {
            info: SignatureInfo {
                label: String::new(),
                parameters: Vec::new(),
                active_parameter: None,
                documentation: None,
            },
            params: Vec::new(),
            slots: Vec::new(),
            call: None,
            repeats: false,
            max_args: None,
            modes_known,
        }
    }

    #[test]
    fn a_declaration_with_no_modes_is_never_picked() {
        let reach = |picked| oxabl_semantic::CallReach {
            possible: vec![0, 1],
            picked,
        };
        let sigs = [sig(true), sig(false)];
        assert_eq!(picked_sig(&sigs, &reach(Some(0))), Some(0));
        assert_eq!(picked_sig(&sigs, &reach(Some(1))), None);
        assert_eq!(picked_sig(&sigs, &reach(None)), None);
    }

    #[test]
    fn an_argument_that_says_the_parameter_name_gets_no_hint() {
        for (argument, parameter) in [
            ("name", "name"),
            ("NAME", "name"),
            ("cName", "pcName"),
            ("iCount", "ipiCount"),
            ("lFlag", "oplFlag"),
            ("cName", "iopcName"),
            ("cCustName", "pcName"),
            ("custName", "pcName"),
            ("Name", "pcName"),
            ("name", "customerName"),
            ("qty", "pdQty"),
            ("cX", "pcX"),
            ("cust-num", "ip-cust-num"),
            ("cust_num", "ip_cust_num"),
        ] {
            assert!(says_the_name(argument, parameter), "{argument} {parameter}");
        }
        for (argument, parameter) in [
            ("cOther", "pcName"),
            ("x", "pcName"),
            // Too short to tell a repeat from a coincidence.
            ("e", "pcName"),
            ("me", "pcName"),
            ("iTotal", "piCount"),
            // The end of a word, not a word.
            ("iDiscount", "piCount"),
            ("count", "discount"),
            ("age", "message"),
            ("pending", "pcName"),
            ("", "pcName"),
            ("cName", ""),
            ("é", "pcName"),
        ] {
            assert!(
                !says_the_name(argument, parameter),
                "{argument} {parameter}"
            );
        }
    }

    #[test]
    fn a_closed_call_is_not_closed_again() {
        let at = |text: &str, marker: &str| text.find(marker).unwrap() as u32;
        // Nothing to close: the call and its statement end on the line.
        let done = "x = f(1, 2).\n";
        assert_eq!(close_open_call(done, at(done, "2")), None);
        // The period is missing.
        let no_period = "x = f(1, 2)\ny = 3.\n";
        let closed = close_open_call(no_period, at(no_period, "2")).unwrap();
        assert!(closed.starts_with("x = f(1, 2)."), "{closed}");
        // The parenthesis is missing.
        let open = "x = f(1, \ny = 3.\n";
        assert!(close_open_call(open, at(open, "\n")).is_some());
        // An empty argument does not parse in a `RUN`.
        let empty = "RUN p (1, ).\n";
        assert!(close_open_call(empty, at(empty, ")")).is_some());
    }
}
