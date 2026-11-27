//! The class or method at a cursor, for go to implementation, go to super
//! method and the type hierarchy, and the declaration spans of a class file.
//!
//! The cursor is resolved with go to definition ([`Navigation::definitions`]),
//! so these features never disagree with it about what a name means. The
//! workspace-wide part (supertypes, subtypes, overrides) is in
//! `oxabl_index::hierarchy`; this module only says which class or method the
//! cursor stands for, and where a class file writes its declarations.

use std::path::{Path, PathBuf};

use oxabl_ast::Span;

use crate::outline::{OutlineKind, OutlineSymbol, outline};
use crate::references::{find_selected, is_class_file};
use crate::{Navigation, Target};

/// A class, interface or enum, by the file that declares it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAt {
    /// The class file.
    pub path: PathBuf,
    /// The class name in its header, in the bytes of `path`.
    pub selection: Span,
}

/// The overloads of a method that the cursor can mean.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodAt {
    /// The class file that declares them.
    pub class_path: PathBuf,
    pub name: String,
    /// Which of the methods of this name in the class, in declaration order.
    pub ordinals: Vec<usize>,
    /// How many methods of this name the class file writes.
    pub total: usize,
}

/// What go to definition finds at a cursor, read as a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodsAt {
    /// The cursor is on no method, or on a name that does not resolve.
    None,
    /// The cursor is on a method that oxabl does not read here, and why.
    Unsupported(String),
    Found(MethodAt),
}

/// One method written in a class file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDecl {
    pub name: String,
    /// The method name, in the bytes of the file.
    pub selection: Span,
}

/// A class, interface or enum, as its file writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDecl {
    pub kind: OutlineKind,
    /// The whole declaration.
    pub span: Span,
    /// The class name in the header.
    pub selection: Span,
    /// Its methods, in declaration order.
    pub methods: Vec<MethodDecl>,
}

/// The class a class file declares, from its text. `None` when the text
/// declares none, or cannot be outlined.
pub fn class_decl(text: &str) -> Option<ClassDecl> {
    let symbols = outline(text).ok()?;
    symbols.iter().find_map(class_of)
}

/// How much of a file's head `class_name_span` lexes.
const HEAD_BYTES: usize = 4096;

/// The span of the name `name` in the `CLASS`, `INTERFACE` or `ENUM`
/// statement at the head of `text`, found by lexing the head only: it costs
/// far less than the parse of [`class_decl`] and gives the same selection.
/// `None` when the head does not show the name as that of the first such
/// statement, with no preprocessor construct before it: use [`class_decl`]
/// then.
pub fn class_name_span(text: &str, name: &str) -> Option<Span> {
    let mut end = text.len().min(HEAD_BYTES);
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    let head = &text[..end];
    let tokens: Vec<_> = oxabl_lexer::tokenize(head)
        .into_iter()
        .filter(|token| token.kind != oxabl_lexer::Kind::Comment)
        .collect();
    let word = |at: usize| tokens[at].slice(head);
    let at = tokens.iter().position(|token| {
        matches!(
            token.kind,
            oxabl_lexer::Kind::Class | oxabl_lexer::Kind::Interface | oxabl_lexer::Kind::KwEnum
        )
    })?;
    let starts_statement = match at.checked_sub(1) {
        None => true,
        Some(before) => {
            tokens[before].kind == oxabl_lexer::Kind::Period
                || ["final", "abstract", "serializable"]
                    .iter()
                    .any(|modifier| word(before).eq_ignore_ascii_case(modifier))
        }
    };
    let hidden = (0..at).any(|before| word(before).starts_with(['&', '{']));
    if !starts_statement || hidden {
        return None;
    }
    let first = tokens.get(at + 1)?.start as usize;
    let written = head.get(first..first + name.len())?;
    let continues = head[first + name.len()..]
        .chars()
        .next()
        .is_some_and(|c| c.is_alphanumeric() || "_-.$#%&".contains(c));
    (written.eq_ignore_ascii_case(name) && !continues).then(|| Span {
        start: first as u32,
        end: (first + name.len()) as u32,
    })
}

fn class_of(symbol: &OutlineSymbol) -> Option<ClassDecl> {
    matches!(
        symbol.kind,
        OutlineKind::Class | OutlineKind::Interface | OutlineKind::Enum
    )
    .then(|| ClassDecl {
        kind: symbol.kind,
        span: symbol.span,
        selection: symbol.selection_span,
        methods: symbol
            .children
            .iter()
            .filter(|child| child.kind == OutlineKind::Method)
            .map(|child| MethodDecl {
                name: child.name.clone(),
                selection: child.selection_span,
            })
            .collect(),
    })
}

impl ClassDecl {
    /// The methods of `name`, in declaration order.
    pub fn methods_named<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a MethodDecl> {
        self.methods
            .iter()
            .filter(move |method| method.name.eq_ignore_ascii_case(name))
    }
}

impl Navigation<'_, '_> {
    /// The classes the name at byte `offset` of `file` refers to: the targets
    /// of go to definition that are the name of a class, interface or enum in
    /// a class file. Empty on any other name.
    pub fn types_at(&self, file: &Path, offset: u32) -> Vec<TypeAt> {
        self.definitions(file, offset)
            .into_iter()
            .filter_map(|target| self.type_of(&target))
            .collect()
    }

    fn type_of(&self, target: &Target) -> Option<TypeAt> {
        let span = target.span?;
        if !is_class_file(&target.path) {
            return None;
        }
        let symbols = self.outline_of(&target.path)?;
        let (symbol, parent) = find_selected(&symbols, span)?;
        (parent.is_none()
            && matches!(
                symbol.kind,
                OutlineKind::Class | OutlineKind::Interface | OutlineKind::Enum
            ))
        .then(|| TypeAt {
            path: target.path.clone(),
            selection: symbol.selection_span,
        })
    }

    /// The class that holds byte `offset` of `file`: the one a class file
    /// declares, when the offset is inside it.
    pub fn enclosing_type(&self, file: &Path, offset: u32) -> Option<TypeAt> {
        if !is_class_file(file) {
            return None;
        }
        let symbols = self.outline_of(file)?;
        let class = symbols.iter().find_map(|symbol| {
            class_of(symbol).filter(|class| class.span.start <= offset && offset <= class.span.end)
        })?;
        Some(TypeAt {
            path: oxabl_index::search::normalize_lexically(file),
            selection: class.selection,
        })
    }

    /// The methods the name at byte `offset` of `file` refers to: a method
    /// name at its declaration, or a call. A call of an overloaded name means
    /// each overload, as go to definition lists them.
    pub fn methods_at(&self, file: &Path, offset: u32) -> MethodsAt {
        let targets = self.definitions(file, offset);
        let mut class_path: Option<PathBuf> = None;
        let mut name: Option<String> = None;
        let mut ordinals: Vec<usize> = Vec::new();
        let mut total = 0;
        for target in &targets {
            let Some(span) = target.span else {
                return MethodsAt::None;
            };
            let Some(symbols) = self.outline_of(&target.path) else {
                return MethodsAt::None;
            };
            let Some((symbol, parent)) = find_selected(&symbols, span) else {
                return MethodsAt::None;
            };
            if symbol.kind != OutlineKind::Method
                || !matches!(parent, Some(OutlineKind::Class | OutlineKind::Interface))
            {
                return MethodsAt::None;
            }
            if !is_class_file(&target.path) {
                return MethodsAt::Unsupported(format!(
                    "the method `{}` is declared in an include of its class: oxabl does not \
                     compare it",
                    symbol.name
                ));
            }
            if class_path
                .as_ref()
                .is_some_and(|known| *known != target.path)
                || name
                    .as_ref()
                    .is_some_and(|known| !known.eq_ignore_ascii_case(&symbol.name))
            {
                return MethodsAt::Unsupported(
                    "the name means methods of more than one class".to_string(),
                );
            }
            let Some(class) = symbols.iter().find_map(class_of) else {
                return MethodsAt::None;
            };
            let same_name: Vec<&MethodDecl> = class.methods_named(&symbol.name).collect();
            total = same_name.len();
            if let Some(ordinal) = same_name
                .iter()
                .position(|method| method.selection == symbol.selection_span)
                && !ordinals.contains(&ordinal)
            {
                ordinals.push(ordinal);
            }
            class_path = Some(target.path.clone());
            name = Some(symbol.name.clone());
        }
        match (class_path, name) {
            (Some(class_path), Some(name)) => {
                ordinals.sort_unstable();
                MethodsAt::Found(MethodAt {
                    class_path,
                    name,
                    ordinals,
                    total,
                })
            }
            _ => MethodsAt::None,
        }
    }
}
