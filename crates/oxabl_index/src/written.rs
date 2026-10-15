//! A token scan for the class and program names a stretch of ABL text writes,
//! without parsing it.
//!
//! For text the parser never sees: a branch of `&IF` the preprocessor did not
//! take. That text is often not a whole statement, so it is read token by token,
//! dispatching on the lexer's [`Kind`]s, and a name is kept only where its
//! syntax says what it names.

use oxabl_ast::Span;
use oxabl_lexer::{Kind, Lexer, Token, TokenValue, tokenize};

use crate::deps::EdgeKind;

/// The syntax that names a [`WrittenReference`]'s target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ReferenceSyntax {
    /// `NEW x(...)`.
    New,
    /// `AS x` or `RETURNS x`. The name may be a data type the lexer does not
    /// know.
    TypeName,
    /// `AS CLASS x` or `RETURNS CLASS x`.
    ClassTypeName,
    /// `INHERITS x`.
    Inherits,
    /// `IMPLEMENTS x[, y]`.
    Implements,
    /// `CAST(expr, x)`.
    Cast,
    /// `TYPE-OF(expr, x)`.
    TypeOf,
    /// `USING x` at the start of a statement; a package (`x.*`) is not listed.
    Using,
    /// `x:member`, with no space around the colon. The name may also be a
    /// variable or a handle.
    StaticAccess,
    /// `RUN x` or `RUN "x"`. A name with no extension and no path may be an
    /// internal procedure.
    Run,
}

impl ReferenceSyntax {
    /// Stable snake-case tag for a wire format.
    pub fn as_str(self) -> &'static str {
        match self {
            ReferenceSyntax::New => "new",
            ReferenceSyntax::TypeName => "type_name",
            ReferenceSyntax::ClassTypeName => "class_type_name",
            ReferenceSyntax::Inherits => "inherits",
            ReferenceSyntax::Implements => "implements",
            ReferenceSyntax::Cast => "cast",
            ReferenceSyntax::TypeOf => "type_of",
            ReferenceSyntax::Using => "using",
            ReferenceSyntax::StaticAccess => "static_access",
            ReferenceSyntax::Run => "run",
        }
    }

    /// Whether the syntax names a class and nothing else, so a name that
    /// resolves to no file is still a class reference.
    pub fn names_only_a_class(self) -> bool {
        matches!(
            self,
            ReferenceSyntax::New
                | ReferenceSyntax::ClassTypeName
                | ReferenceSyntax::Inherits
                | ReferenceSyntax::Implements
                | ReferenceSyntax::Cast
                | ReferenceSyntax::TypeOf
                | ReferenceSyntax::Using
        )
    }
}

/// One class or program name a stretch of text writes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrittenReference {
    /// [`EdgeKind::ClassReference`] or [`EdgeKind::ProgramReference`].
    pub kind: EdgeKind,
    pub syntax: ReferenceSyntax,
    /// The name as written; for a quoted `RUN` target, without the quotes.
    pub name: String,
    /// Where the name is written, relative to the start of the text.
    pub span: Span,
}

/// The class and program names `text` writes, in text order.
///
/// A name built from a preprocessor reference (`{&x}`, `{1}`) is not listed, nor
/// is `RUN VALUE(...)`, `RUN x IN h` or `NEW` without a parenthesis after the
/// name (`DEFINE NEW SHARED`).
pub fn written_references(text: &str) -> Vec<WrittenReference> {
    let tokens: Vec<Token> = tokenize(text)
        .into_iter()
        .filter(|t| t.kind != Kind::Comment)
        .collect();
    let scan = Scan {
        text,
        tokens: &tokens,
    };
    let mut out = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        i = scan.at(i, &mut out);
    }
    out
}

/// The names `text`'s `USING` statements import, in order. A package import is
/// written with its `.*` (`app.util.*`).
///
/// ABL allows `USING` only ahead of every other statement but `BLOCK-LEVEL` and
/// `ROUTINE-LEVEL`, so the lexer stops at the first other statement.
pub fn written_imports(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut statement: Vec<Token> = Vec::new();
    for token in Lexer::new(text) {
        match token.kind {
            Kind::Comment => continue,
            // An include left unexpanded starts no statement of its own.
            Kind::IncludeReference | Kind::Preprop if statement.is_empty() => continue,
            Kind::Eof => break,
            _ => {}
        }
        let ends = token.kind == Kind::Period
            && text
                .as_bytes()
                .get(token.end)
                .is_none_or(u8::is_ascii_whitespace);
        statement.push(token);
        if !ends {
            continue;
        }
        match statement[0].kind {
            Kind::Using => {
                let scan = Scan {
                    text,
                    tokens: &statement,
                };
                if let Some(mut name) = scan.type_name(1) {
                    if scan.is_package(name.next) {
                        name.text.push_str(".*");
                    }
                    out.push(name.text);
                }
            }
            Kind::BlockLevel | Kind::RoutineLevel => {}
            _ => break,
        }
        statement.clear();
    }
    out
}

struct Scan<'a> {
    text: &'a str,
    tokens: &'a [Token],
}

/// A name read from the token stream: its text, its span, and the index of the
/// token after it.
struct Name {
    text: String,
    span: Span,
    next: usize,
}

impl Scan<'_> {
    fn kind(&self, i: usize) -> Kind {
        self.tokens.get(i).map_or(Kind::Eof, |t| t.kind)
    }

    /// Whether token `i` starts where token `i - 1` ends.
    fn touches(&self, i: usize) -> bool {
        i > 0
            && self
                .tokens
                .get(i)
                .is_some_and(|t| t.start == self.tokens[i - 1].end)
    }

    /// Read the reference at token `i`, if any, and return the next token to
    /// look at.
    fn at(&self, i: usize, out: &mut Vec<WrittenReference>) -> usize {
        match self.kind(i) {
            Kind::New => match self.type_name(i + 1) {
                Some(name) if self.kind(name.next) == Kind::LeftParen => {
                    let next = name.next;
                    push_class(out, ReferenceSyntax::New, name);
                    next
                }
                _ => i + 1,
            },
            Kind::KwAs | Kind::Returns => {
                let explicit = self.kind(i + 1) == Kind::Class;
                let start = i + 1 + usize::from(explicit);
                match self.type_name(start) {
                    Some(name) if explicit || self.could_name_a_class(start, &name) => {
                        let next = name.next;
                        let syntax = if explicit {
                            ReferenceSyntax::ClassTypeName
                        } else {
                            ReferenceSyntax::TypeName
                        };
                        push_class(out, syntax, name);
                        next
                    }
                    _ => i + 1,
                }
            }
            Kind::Inherits => match self.type_name(i + 1) {
                Some(name) => {
                    let next = name.next;
                    push_class(out, ReferenceSyntax::Inherits, name);
                    next
                }
                None => i + 1,
            },
            Kind::Implements => {
                let mut next = i + 1;
                while let Some(name) = self.type_name(next) {
                    next = name.next;
                    push_class(out, ReferenceSyntax::Implements, name);
                    if self.kind(next) != Kind::Comma {
                        break;
                    }
                    next += 1;
                }
                next
            }
            Kind::Cast | Kind::TypeOf if self.kind(i + 1) == Kind::LeftParen => {
                let syntax = if self.kind(i) == Kind::Cast {
                    ReferenceSyntax::Cast
                } else {
                    ReferenceSyntax::TypeOf
                };
                match self
                    .second_argument(i + 2)
                    .and_then(|at| self.type_name(at))
                {
                    Some(name) if self.kind(name.next) == Kind::RightParen => {
                        let next = name.next;
                        push_class(out, syntax, name);
                        next
                    }
                    _ => i + 1,
                }
            }
            Kind::Using if self.starts_statement(i) => match self.type_name(i + 1) {
                Some(name) if !self.is_package(name.next) => {
                    let next = name.next;
                    push_class(out, ReferenceSyntax::Using, name);
                    next
                }
                _ => i + 1,
            },
            Kind::Run => self.run(i, out),
            Kind::Identifier if !self.continues_a_name(i) => match self.type_name(i) {
                Some(name) if self.static_access_at(name.next) => {
                    let next = name.next;
                    push_class(out, ReferenceSyntax::StaticAccess, name);
                    next
                }
                Some(name) => name.next,
                None => i + 1,
            },
            _ => i + 1,
        }
    }

    /// Whether token `i` is written against a `.`, `:` or `::` before it, so it
    /// is the tail of a longer name or a member.
    fn continues_a_name(&self, i: usize) -> bool {
        self.touches(i)
            && matches!(
                self.kind(i - 1),
                Kind::Period | Kind::Colon | Kind::DoubleColon
            )
    }

    /// Whether a name ending before token `i` is followed by `:member`, written
    /// with no space.
    fn static_access_at(&self, i: usize) -> bool {
        self.kind(i) == Kind::Colon
            && self.touches(i)
            && self.touches(i + 1)
            && is_word(self.kind(i + 1))
    }

    /// Whether a bare type name could be a class rather than a data type: it is
    /// dotted, or its one word is no keyword.
    fn could_name_a_class(&self, start: usize, name: &Name) -> bool {
        name.next > start + 1 || self.kind(start) == Kind::Identifier
    }

    /// Whether token `i` begins a statement: the first token, or one after a
    /// statement-ending period or a block colon.
    fn starts_statement(&self, i: usize) -> bool {
        i == 0 || (matches!(self.kind(i - 1), Kind::Period | Kind::Colon) && !self.touches(i))
    }

    /// Whether the name before token `i` goes on as `.*`, naming a package.
    fn is_package(&self, i: usize) -> bool {
        self.kind(i) == Kind::Period
            && self.touches(i)
            && self.kind(i + 1) == Kind::Star
            && self.touches(i + 1)
    }

    /// The token after the first top-level comma of the argument list whose
    /// first token is `i`.
    fn second_argument(&self, i: usize) -> Option<usize> {
        let mut depth = 0usize;
        for (at, token) in self.tokens.iter().enumerate().skip(i) {
            match token.kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen if depth == 0 => return None,
                Kind::RightParen => depth -= 1,
                Kind::Comma if depth == 0 => return Some(at + 1),
                // A statement-ending period ends the search.
                Kind::Period if depth == 0 && !self.touches(at + 1) => return None,
                _ => {}
            }
        }
        None
    }

    /// The dotted type name whose first token is `i`. `None` when token `i` is
    /// no word, or the name holds a preprocessor reference.
    fn type_name(&self, i: usize) -> Option<Name> {
        if !is_word(self.kind(i)) {
            return None;
        }
        let mut next = i + 1;
        while self.kind(next) == Kind::Period
            && self.touches(next)
            && self.touches(next + 1)
            && is_word(self.kind(next + 1))
        {
            next += 2;
        }
        if is_reference(self.kind(next)) && self.touches(next) {
            return None;
        }
        Some(self.name(i, next))
    }

    fn name(&self, first: usize, next: usize) -> Name {
        let start = self.tokens[first].start;
        let end = self.tokens[next - 1].end;
        Name {
            text: self.text[start..end].to_string(),
            span: Span {
                start: start as u32,
                end: end as u32,
            },
            next,
        }
    }

    /// `RUN` at token `i`: the target, unless it is computed or an internal
    /// procedure of a handle.
    fn run(&self, i: usize, out: &mut Vec<WrittenReference>) -> usize {
        let first = i + 1;
        let name = match self.kind(first) {
            Kind::StringLiteral => {
                let token = &self.tokens[first];
                let TokenValue::String(value) = &token.value else {
                    return first;
                };
                Name {
                    text: value.to_string(),
                    span: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                    next: first + 1,
                }
            }
            Kind::Value => return first,
            kind if is_word(kind) || matches!(kind, Kind::Slash | Kind::Period) => {
                match self.program_name(first) {
                    Some(name) => name,
                    None => return first,
                }
            }
            _ => return first,
        };
        if self.kind(name.next) == Kind::KwIn {
            return name.next;
        }
        let next = name.next;
        out.push(WrittenReference {
            kind: EdgeKind::ProgramReference,
            syntax: ReferenceSyntax::Run,
            name: name.text,
            span: name.span,
        });
        next
    }

    /// A `RUN` target written as a path: tokens that touch, up to the first
    /// whitespace. A `.` ends it unless a word or a number touches it on the
    /// right. `None` when it holds a preprocessor reference or is a bare
    /// number.
    fn program_name(&self, first: usize) -> Option<Name> {
        let mut next = first;
        loop {
            let kind = self.kind(next);
            if next > first && !self.touches(next) {
                break;
            }
            if is_reference(kind) {
                return None;
            }
            let part = is_word(kind)
                || matches!(
                    kind,
                    Kind::Slash | Kind::IntegerLiteral | Kind::DecimalLiteral
                )
                || (kind == Kind::Period
                    && self.touches(next + 1)
                    && (is_word(self.kind(next + 1))
                        || matches!(self.kind(next + 1), Kind::Period | Kind::Slash)
                        || self.kind(next + 1) == Kind::IntegerLiteral));
            if !part {
                break;
            }
            next += 1;
        }
        if next == first {
            return None;
        }
        let name = self.name(first, next);
        if name.text.bytes().all(|b| b.is_ascii_digit() || b == b'.') {
            return None;
        }
        Some(name)
    }
}

fn push_class(out: &mut Vec<WrittenReference>, syntax: ReferenceSyntax, name: Name) {
    out.push(WrittenReference {
        kind: EdgeKind::ClassReference,
        syntax,
        name: name.text,
        span: name.span,
    });
}

/// Whether `kind` is a preprocessor or include reference.
fn is_reference(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Preprop | Kind::IncludeReference | Kind::IncludeArgReference
    )
}

/// Whether `kind` is a word: an identifier or a keyword, which may stand in a
/// name.
fn is_word(kind: Kind) -> bool {
    !matches!(
        kind,
        Kind::Eof
            | Kind::Invalid
            | Kind::Comment
            | Kind::PreprocEnd
            | Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::StringLiteral
            | Kind::Plus
            | Kind::Minus
            | Kind::Period
            | Kind::Star
            | Kind::Slash
            | Kind::Colon
            | Kind::Question
            | Kind::At
            | Kind::LeftBracket
            | Kind::RightBracket
            | Kind::Caret
            | Kind::LessThan
            | Kind::Equals
            | Kind::GreaterThan
            | Kind::GreaterThanOrEqual
            | Kind::LessThanOrEqual
            | Kind::NotEqual
            | Kind::PlusEquals
            | Kind::MinusEquals
            | Kind::StarEquals
            | Kind::SlashEquals
            | Kind::DoubleColon
            | Kind::LeftParen
            | Kind::RightParen
            | Kind::LeftBrace
            | Kind::RightBrace
            | Kind::Comma
            | Kind::KwTrue
            | Kind::KwFalse
            | Kind::Preprop
            | Kind::IncludeReference
            | Kind::IncludeArgReference
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn refs(text: &str) -> Vec<(ReferenceSyntax, String)> {
        written_references(text)
            .into_iter()
            .map(|r| {
                assert_eq!(
                    &text[r.span.start as usize..r.span.end as usize].trim_matches('"'),
                    &r.name
                );
                (r.syntax, r.name)
            })
            .collect()
    }

    fn pairs(list: &[(ReferenceSyntax, &str)]) -> Vec<(ReferenceSyntax, String)> {
        list.iter().map(|(s, n)| (*s, n.to_string())).collect()
    }

    #[test]
    fn new_and_static_access_name_classes() {
        assert_eq!(
            refs("o = NEW app.util.Cache(1).\nx = AppSettings:region.\n"),
            pairs(&[
                (ReferenceSyntax::New, "app.util.Cache"),
                (ReferenceSyntax::StaticAccess, "AppSettings"),
            ])
        );
    }

    #[test]
    fn define_new_shared_is_no_class() {
        assert!(refs("DEFINE NEW SHARED VARIABLE x AS INTEGER NO-UNDO.\n").is_empty());
    }

    #[test]
    fn a_data_type_is_no_class_and_as_class_always_is() {
        assert_eq!(
            refs(
                "DEFINE VARIABLE a AS CHARACTER NO-UNDO.\n\
                 DEFINE VARIABLE b AS app.Thing NO-UNDO.\n\
                 DEFINE VARIABLE c AS CLASS Widget NO-UNDO.\n\
                 DEFINE VARIABLE d AS Gadget NO-UNDO.\n"
            ),
            pairs(&[
                (ReferenceSyntax::TypeName, "app.Thing"),
                (ReferenceSyntax::ClassTypeName, "Widget"),
                (ReferenceSyntax::TypeName, "Gadget"),
            ])
        );
    }

    #[test]
    fn inherits_implements_cast_and_type_of() {
        assert_eq!(
            refs(
                "CLASS a.B INHERITS a.Base IMPLEMENTS a.I1, a.I2:\n\
                 x = CAST(y, a.C).\n\
                 IF TYPE-OF(y, a.D) THEN .\n"
            ),
            pairs(&[
                (ReferenceSyntax::Inherits, "a.Base"),
                (ReferenceSyntax::Implements, "a.I1"),
                (ReferenceSyntax::Implements, "a.I2"),
                (ReferenceSyntax::Cast, "a.C"),
                (ReferenceSyntax::TypeOf, "a.D"),
            ])
        );
    }

    #[test]
    fn using_names_a_class_but_not_a_package() {
        assert_eq!(
            refs("USING app.Cache.\nUSING app.util.* FROM PROPATH.\n"),
            pairs(&[(ReferenceSyntax::Using, "app.Cache")])
        );
    }

    #[test]
    fn run_names_a_program_unless_computed_or_in_a_handle() {
        assert_eq!(
            refs(
                "RUN sub/do-it.p (INPUT 1).\nRUN \"q.p\".\nRUN VALUE(x).\n\
                 RUN proc IN h.\nRUN local-proc.\nRUN {&prog}.\n"
            ),
            pairs(&[
                (ReferenceSyntax::Run, "sub/do-it.p"),
                (ReferenceSyntax::Run, "q.p"),
                (ReferenceSyntax::Run, "local-proc"),
            ])
        );
    }

    #[test]
    fn handles_members_and_labels_are_no_static_access() {
        assert!(
            refs("blk:\nDO:\n  hb:BUFFER-FIELD(1).\n  THIS-OBJECT:x = 1.\nEND.\n")
                .iter()
                .all(|(_, name)| name == "hb"),
        );
        assert!(
            refs("a.b:c = 1.\nx = a:b:c.\n")
                .iter()
                .all(|(_, n)| n != "b")
        );
    }

    #[test]
    fn a_name_built_from_a_reference_is_not_listed() {
        assert!(refs("o = NEW {&pkg}.Cache().\n").is_empty());
    }

    #[test]
    fn imports_keep_a_package_with_its_star() {
        assert_eq!(
            written_imports(
                "/* header */\nUSING app.Cache.\nBLOCK-LEVEL ON ERROR UNDO, THROW.\n\
                 USING app.util.* FROM PROPATH.\nFIND c USING x.\nUSING late.Name.\n"
            ),
            ["app.Cache", "app.util.*"]
        );
    }

    #[test]
    fn a_fragment_that_is_no_statement_still_scans() {
        assert_eq!(
            refs("  , NEW a.B()\n"),
            pairs(&[(ReferenceSyntax::New, "a.B")])
        );
    }
}
