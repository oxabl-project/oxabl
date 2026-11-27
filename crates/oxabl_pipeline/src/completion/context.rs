//! What the text before the cursor asks for: the word that completion
//! replaces, the statement window, and the context.
//!
//! The finder reads the tokens of the current text, never the base: the
//! statement at the cursor is not complete, and the parser drops it. Keywords
//! are compared by [`Kind`], so every legal abbreviation and case matches.

use oxabl_lexer::{Kind, Token};

use super::keywords::is_define_modifier;

/// Where the record names of a [`Context::Records`] come from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RecordsAfter {
    /// `DEFINE BUFFER b FOR`: tables and temp-tables.
    BufferFor,
    /// `DEFINE BUFFER b FOR TEMP-TABLE`: temp-tables.
    BufferForTempTable,
    /// `LIKE`: tables, temp-tables and buffers.
    Like,
    /// Every other record position: buffers, temp-tables and tables.
    Record,
}

/// What the cursor needs: one row of the context table of
/// `docs/design/completion.md`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Context {
    /// `{&`: the preprocessor names.
    PreprocessorNames,
    /// `{` and digits in an include: its arguments.
    IncludeArguments,
    /// An unclosed `{`: include files.
    IncludeFiles,
    /// A receiver joined to `:`.
    Members,
    Records(RecordsAfter),
    /// A name joined to `.`: the fields of a record or the tables of a
    /// database. `qualifier` is the bytes of the name before the `.` (which
    /// can hold dots itself, as `shop.Customer`); `after_record` is true right
    /// after a record keyword (`FIND FIRST shop.`), where only a database
    /// name qualifies.
    Qualified {
        qualifier: (u32, u32),
        after_record: bool,
    },
    /// An expression in the `WHERE` of a record phrase.
    Where,
    /// The target of `RUN`.
    Run,
    Types,
    /// After `DEFINE` and the modifiers it holds.
    DefineKeywords {
        modifiers: Vec<Kind>,
    },
    StatementStart,
    /// An expression. `target` is the token of the name an assignment writes,
    /// when the window is `name =`.
    Values {
        target: Option<usize>,
    },
}

impl Context {
    /// The cursor is inside a `{...}` reference, which the expansion replaced.
    pub(crate) fn in_reference(&self) -> bool {
        matches!(
            self,
            Context::PreprocessorNames | Context::IncludeArguments | Context::IncludeFiles
        )
    }

    /// Whether a client that asks because the person typed `trigger` gets a
    /// list here: each trigger character asks for the contexts it starts.
    pub(crate) fn takes_trigger(&self, trigger: char) -> bool {
        match trigger {
            ':' => *self == Context::Members,
            '.' => matches!(self, Context::Qualified { .. }),
            '{' => matches!(self, Context::IncludeFiles | Context::IncludeArguments),
            '&' => *self == Context::PreprocessorNames,
            '/' => matches!(self, Context::Run | Context::IncludeFiles),
            _ => true,
        }
    }
}

/// The text around the cursor and what it asks for.
#[derive(Debug)]
pub(crate) struct Found {
    /// The word completion replaces: from its start up to the cursor.
    pub word_start: u32,
    /// The end of the word after the cursor.
    pub word_end: u32,
    /// The first byte of the statement window (the word start when the window
    /// is empty).
    pub window_start: u32,
    /// The window holds tokens and reaches the start of the text.
    pub reaches_start: bool,
    /// The window starts after `THEN`, `ELSE` or `OTHERWISE`: the statement is
    /// the branch of another one.
    pub after_branch: bool,
    /// The index of the first token at or after the word.
    pub before: usize,
    pub context: Context,
}

/// A character of a word.
pub(crate) fn is_word_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'#' | b'$' | b'%')
}

/// A character of a path: a word, `/`, `\` and `.`.
pub(super) fn is_path_byte(byte: u8) -> bool {
    is_word_byte(byte) || matches!(byte, b'/' | b'\\' | b'.')
}

/// What the text asks for at byte `cursor`. `None` when oxabl does not know:
/// a comment, a string, right after a statement end, a window that holds an
/// include or macro reference, or a position no row of the table names.
pub(crate) fn find(text: &str, tokens: &[Token], cursor: u32) -> Option<Found> {
    let bytes = text.as_bytes();
    let cursor_at = (cursor as usize).min(bytes.len());
    let mut start = cursor_at;
    while start > 0 && is_word_byte(bytes[start - 1]) {
        start -= 1;
    }
    // A name starts with neither `-` nor a digit.
    while start < cursor_at && (bytes[start] == b'-' || bytes[start].is_ascii_digit()) {
        start += 1;
    }
    let mut end = cursor_at;
    while end < bytes.len() && is_word_byte(bytes[end]) {
        end += 1;
    }
    let before = tokens.partition_point(|token| token.end <= start && token.kind != Kind::Eof);
    let found = |context, window_start: u32, reaches_start| Found {
        word_start: start as u32,
        word_end: end as u32,
        window_start,
        reaches_start,
        after_branch: false,
        before,
        context,
    };

    // An include reference is not a statement, and the preprocessor reads it
    // in a string too.
    if let Some(open) = unclosed_brace(bytes, cursor_at)
        && !in_kind(tokens, open as u32, Kind::Comment)
    {
        let inside = &bytes[open + 1..cursor_at];
        let mut word_start = open + 1;
        let context = if inside.first() == Some(&b'&') {
            if !inside[1..].iter().all(|b| is_word_byte(*b)) {
                return None;
            }
            word_start += 1;
            Context::PreprocessorNames
        } else if !inside.is_empty() && inside.iter().all(u8::is_ascii_digit) {
            Context::IncludeArguments
        } else if inside.iter().all(|b| is_path_byte(*b)) {
            Context::IncludeFiles
        } else {
            // The arguments of an include reference.
            return None;
        };
        let mut end = cursor_at;
        while end < bytes.len() && is_path_byte(bytes[end]) {
            end += 1;
        }
        return Some(Found {
            word_start: word_start as u32,
            word_end: end as u32,
            ..found(context, open as u32, false)
        });
    }
    if inside_text(tokens, bytes, cursor) {
        return None;
    }

    // The character before the word.
    if start > 0 {
        match bytes[start - 1] {
            b':' => {
                let mut previous = tokens[..before]
                    .iter()
                    .rev()
                    .filter(|t| t.kind != Kind::Comment);
                let joined = previous
                    .next()
                    .filter(|t| t.kind == Kind::Colon && t.end == start)
                    .and_then(|_| previous.next().map(|t| (t, previous.next())))
                    .filter(|(t, _)| t.end + 1 == start);
                return match joined {
                    Some((t, before_it))
                        if matches!(
                            t.kind,
                            Kind::Identifier | Kind::RightParen | Kind::ThisObject
                        ) || ends_a_path(bytes, t, before_it) =>
                    {
                        Some(found(Context::Members, start as u32, false))
                    }
                    // `DO:` and its kind, or a colon on its own.
                    _ => None,
                };
            }
            b'.' => {
                return qualified(text, tokens, start)
                    .map(|(context, window_start, reaches)| found(context, window_start, reaches));
            }
            _ => {}
        }
    }

    let window = statement_window(text, tokens, before)?;
    let window_start = window
        .tokens
        .first()
        .map_or(start as u32, |at| tokens[*at].start as u32);
    let reaches_start = !window.boundary && !window.tokens.is_empty();
    let context = classify(text, tokens, &window.tokens, start)?;
    if context == Context::Run {
        // The word of `RUN` is the whole path.
        let path_from = path_start(bytes, start);
        let mut end = cursor_at;
        while end < bytes.len()
            && (is_path_byte(bytes[end]) && bytes[end] != b'.'
                || bytes[end] == b'.' && bytes.get(end + 1).is_some_and(|b| is_word_byte(*b)))
        {
            end += 1;
        }
        return Some(Found {
            word_start: path_from as u32,
            word_end: end as u32,
            ..found(context, window_start, reaches_start)
        });
    }
    Some(Found {
        after_branch: window.after_branch && window.tokens.is_empty(),
        ..found(context, window_start, reaches_start)
    })
}

/// Whether `token`, which a keyword spells (`Color`, `Message`), is the last
/// part of a dotted name or the name of a member: it touches a `.` or a `:`
/// on its left. `DO:` and `ELSE:` have neither.
fn ends_a_path(bytes: &[u8], token: &Token, before_it: Option<&Token>) -> bool {
    let word = &bytes[token.start..token.end];
    !word.is_empty()
        && word.iter().all(|b| is_word_byte(*b))
        && word[0].is_ascii_alphabetic()
        && before_it
            .is_some_and(|t| matches!(t.kind, Kind::Period | Kind::Colon) && t.end == token.start)
}

/// The tokens of a statement window, from the statement start up to (not
/// with) token `before`.
struct Window {
    tokens: Vec<usize>,
    /// A statement end closes the window on the left; without one it reaches
    /// the start of the text.
    boundary: bool,
    /// The window starts after `THEN`, `ELSE` or `OTHERWISE`.
    after_branch: bool,
}

/// The statement window that ends before token `before`. `None` when it
/// holds an include or macro reference: oxabl cannot read the statement as
/// the compiler sees it.
fn statement_window(text: &str, tokens: &[Token], before: usize) -> Option<Window> {
    let bytes = text.as_bytes();
    let mut window: Vec<usize> = Vec::new();
    let mut boundary = false;
    let mut after_branch = false;
    let mut i = before;
    while i > 0 {
        i -= 1;
        let token = &tokens[i];
        match token.kind {
            Kind::Comment => continue,
            Kind::Period if !joined_both(bytes, token) => {
                boundary = true;
                break;
            }
            Kind::Colon if followed_by_space(bytes, token.end) => {
                boundary = true;
                break;
            }
            Kind::Then | Kind::KwElse | Kind::Otherwise if !in_expression(tokens, i) => {
                boundary = true;
                after_branch = true;
                break;
            }
            kind if is_directive(kind) => {
                // The window starts on the line after the directive.
                let line_end = text[token.end..]
                    .find('\n')
                    .map_or(text.len(), |at| token.end + at + 1);
                window.retain(|at| tokens[*at].start >= line_end);
                boundary = true;
                break;
            }
            _ => window.push(i),
        }
    }
    window.reverse();
    if window.iter().any(|at| {
        matches!(
            tokens[*at].kind,
            Kind::IncludeReference | Kind::IncludeArgReference | Kind::Preprop
        )
    }) {
        return None;
    }
    Some(Window {
        tokens: window,
        boundary,
        after_branch,
    })
}

/// The context of a word after `name.`, whose word starts at `start`, the
/// start of its statement window, and whether the window reaches the start of
/// the text. `None` when the text before the `.` is not a name.
fn qualified(text: &str, tokens: &[Token], start: usize) -> Option<(Context, u32, bool)> {
    let bytes = text.as_bytes();
    let dot = start - 1;
    let mut from = dot;
    while from > 0 && (is_word_byte(bytes[from - 1]) || bytes[from - 1] == b'.') {
        from -= 1;
    }
    // A name starts with a letter or `_`, and has no empty part.
    let name = &text[from..dot];
    if !name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
        || name.split('.').any(str::is_empty)
    {
        return None;
    }
    let before = tokens.partition_point(|token| token.end <= from && token.kind != Kind::Eof);
    let window = statement_window(text, tokens, before)?;
    let window_start = window
        .tokens
        .first()
        .map_or(from as u32, |at| tokens[*at].start as u32);
    let after_record = matches!(
        classify(text, tokens, &window.tokens, from),
        Some(Context::Records(after)) if after != RecordsAfter::Like
    );
    Some((
        Context::Qualified {
            qualifier: (from as u32, dot as u32),
            after_record,
        },
        window_start,
        !window.boundary && !window.tokens.is_empty(),
    ))
}

/// The row of the context table that the window ends with.
fn classify(text: &str, tokens: &[Token], window: &[usize], word_start: usize) -> Option<Context> {
    let kinds: Vec<Kind> = window.iter().map(|at| tokens[*at].kind).collect();
    let n = kinds.len();
    let Some(&last) = kinds.last() else {
        return Some(Context::StatementStart);
    };
    let prev = n.checked_sub(2).map(|at| kinds[at]);
    let prev2 = n.checked_sub(3).map(|at| kinds[at]);
    let first = kinds[0];

    // `RUN` and its target, while the target is written.
    if first == Kind::Run {
        let path_start = path_start(text.as_bytes(), word_start);
        let target_from = window.get(1).map_or(word_start, |at| tokens[*at].start);
        return (path_start <= target_from && path_start > tokens[window[0]].end)
            .then_some(Context::Run);
    }

    // `DEFINE` and its modifiers.
    if first == Kind::Define && kinds[1..].iter().all(|kind| is_define_modifier(*kind)) {
        return Some(Context::DefineKeywords {
            modifiers: kinds[1..].to_vec(),
        });
    }

    // Records.
    let records = match (prev2, prev, last) {
        (_, _, Kind::Find) => true,
        (
            _,
            Some(Kind::Find),
            Kind::First | Kind::Last | Kind::Next | Kind::Prev | Kind::Current,
        ) => true,
        (_, Some(Kind::KwFor), Kind::Each | Kind::First | Kind::Last) => true,
        (_, Some(Kind::Comma), Kind::Each | Kind::First | Kind::Last) => {
            matches!(first, Kind::KwFor | Kind::Open | Kind::Do | Kind::Repeat)
        }
        (_, Some(Kind::Preselect), Kind::Each | Kind::First | Kind::Last) => true,
        (_, Some(Kind::CanFind), Kind::LeftParen) => true,
        (Some(Kind::CanFind), Some(Kind::LeftParen), Kind::First | Kind::Last) => true,
        (_, _, Kind::Available | Kind::Of) => true,
        (_, None, Kind::Create | Kind::Delete | Kind::Release | Kind::Validate) => true,
        (_, None, Kind::BufferCopy | Kind::BufferCompare) => true,
        (_, _, Kind::To) => matches!(first, Kind::BufferCopy | Kind::BufferCompare),
        _ => false,
    };
    if last == Kind::KwFor && n == 1 {
        return Some(Context::Records(RecordsAfter::Record));
    }
    if last == Kind::KwFor {
        if first == Kind::Define && kinds.contains(&Kind::Buffer) {
            return Some(Context::Records(RecordsAfter::BufferFor));
        }
        if matches!(prev, Some(Kind::Do | Kind::Repeat)) {
            return Some(Context::Records(RecordsAfter::Record));
        }
    }
    if last == Kind::TempTable && prev == Some(Kind::KwFor) && first == Kind::Define {
        return Some(Context::Records(RecordsAfter::BufferForTempTable));
    }
    if last == Kind::Like {
        return Some(Context::Records(RecordsAfter::Like));
    }
    if records {
        return Some(Context::Records(RecordsAfter::Record));
    }

    // An expression in a WHERE.
    if let Some(at) = kinds.iter().rposition(|kind| *kind == Kind::KwWhere) {
        let record_phrase = matches!(
            first,
            Kind::Find | Kind::KwFor | Kind::Open | Kind::Preselect | Kind::Do | Kind::Repeat
        ) || kinds[..at].contains(&Kind::CanFind);
        if record_phrase && (at == n - 1 || opens_operand(last)) {
            return Some(Context::Where);
        }
    }

    // Types.
    let types = match last {
        Kind::New
        | Kind::KwAs
        | Kind::Returns
        | Kind::Inherits
        | Kind::Implements
        | Kind::Using => true,
        Kind::Class => prev == Some(Kind::KwAs),
        Kind::Comma => {
            kinds.contains(&Kind::Implements)
                && kinds[kinds.iter().rposition(|k| *k == Kind::Implements)?..]
                    .iter()
                    .all(|k| {
                        matches!(
                            k,
                            Kind::Implements | Kind::Comma | Kind::Identifier | Kind::Period
                        )
                    })
                || open_call(&kinds).is_some_and(|open| {
                    open > 0 && matches!(kinds[open - 1], Kind::Cast | Kind::TypeOf)
                })
        }
        _ => false,
    };
    if types {
        return Some(Context::Types);
    }
    if first == Kind::Method && kinds[1..].iter().all(|kind| is_method_modifier(*kind)) {
        return Some(Context::Types);
    }

    // An operator that needs a left operand and has none: the start of a
    // comment (`/*`, `//`) or of no statement.
    if n == 1 && opens_operand(last) && !is_prefix(last) {
        return None;
    }

    // An expression.
    let values = opens_operand(last)
        || matches!(last, Kind::KwIf | Kind::KwReturn | Kind::When | Kind::Then)
        || last == Kind::Comma && open_call(&kinds).is_some();
    if values {
        let target = (n == 2 && last == Kind::Equals && first == Kind::Identifier)
            .then(|| window[0])
            .or_else(|| {
                (n == 3
                    && first == Kind::Assign
                    && last == Kind::Equals
                    && kinds[1] == Kind::Identifier)
                    .then(|| window[1])
            });
        return Some(Context::Values { target });
    }
    None
}

/// The index of the innermost `(` of the window that is not closed.
fn open_call(kinds: &[Kind]) -> Option<usize> {
    let mut depth = 0i32;
    for (at, kind) in kinds.iter().enumerate().rev() {
        match kind {
            Kind::RightParen => depth += 1,
            Kind::LeftParen if depth == 0 => return Some(at),
            Kind::LeftParen => depth -= 1,
            _ => {}
        }
    }
    None
}

/// A token after which an operand comes.
fn opens_operand(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Equals
            | Kind::Plus
            | Kind::Add
            | Kind::Minus
            | Kind::Star
            | Kind::Slash
            | Kind::LessThan
            | Kind::GreaterThan
            | Kind::GreaterThanOrEqual
            | Kind::LessThanOrEqual
            | Kind::NotEqual
            | Kind::PlusEquals
            | Kind::MinusEquals
            | Kind::StarEquals
            | Kind::SlashEquals
            | Kind::Eq
            | Kind::Ne
            | Kind::Lt
            | Kind::Le
            | Kind::Gt
            | Kind::Ge
            | Kind::Begins
            | Kind::Matches
            | Kind::Contains
            | Kind::Modulo
            | Kind::And
            | Kind::Or
            | Kind::Not
            | Kind::LeftParen
            | Kind::KwWhere
    )
}

/// An operator that can stand before its only operand.
fn is_prefix(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Minus | Kind::Plus | Kind::Add | Kind::Not | Kind::LeftParen
    )
}

fn is_method_modifier(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Private
            | Kind::Protected
            | Kind::Public
            | Kind::PackagePrivate
            | Kind::PackageProtected
            | Kind::KwStatic
            | Kind::Abstract
            | Kind::Override
            | Kind::Final
    )
}

fn is_directive(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::PreprocIf
            | Kind::PreprocThen
            | Kind::PreprocElse
            | Kind::PreprocElseif
            | Kind::PreprocEndif
            | Kind::PreprocGlobalDefine
            | Kind::PreprocScopedDefine
            | Kind::PreprocUndefine
            | Kind::PreprocMessage
            | Kind::PreprocWebstream
            | Kind::PreprocEnd
    )
}

/// The start of the path that ends at `word_start`: the word with the `/`,
/// `\` and `.` parts before it.
fn path_start(bytes: &[u8], word_start: usize) -> usize {
    let mut start = word_start;
    while start > 0
        && (is_word_byte(bytes[start - 1]) || matches!(bytes[start - 1], b'/' | b'\\' | b'.'))
    {
        start -= 1;
    }
    start
}

/// A period inside a name, as in `Customer.Name` or `pkg.Class`.
fn joined_both(bytes: &[u8], token: &Token) -> bool {
    token.start > 0
        && is_word_byte(bytes[token.start - 1])
        && bytes.get(token.end).is_some_and(|b| is_word_byte(*b))
}

/// Whether whitespace, a comment or the end of the text comes at `at`.
fn followed_by_space(bytes: &[u8], at: usize) -> bool {
    match bytes.get(at) {
        None => true,
        Some(b) if b.is_ascii_whitespace() => true,
        Some(b'/') => matches!(bytes.get(at + 1), Some(b'*' | b'/')),
        _ => false,
    }
}

/// Whether the `THEN` or `ELSE` at `at` belongs to an `IF` that is an
/// expression (`x = IF a THEN b ELSE c`), not a statement.
fn in_expression(tokens: &[Token], at: usize) -> bool {
    let mut depth = 0usize;
    let mut i = at;
    while i > 0 {
        i -= 1;
        match tokens[i].kind {
            Kind::Comment => continue,
            Kind::Then | Kind::KwElse => depth += 1,
            Kind::KwIf if depth > 0 => depth -= 1,
            Kind::KwIf => {
                let before = tokens[..i].iter().rev().find(|t| t.kind != Kind::Comment);
                return before.is_some_and(|t| {
                    opens_operand(t.kind) || matches!(t.kind, Kind::Comma | Kind::KwReturn)
                });
            }
            Kind::Period | Kind::Colon => return false,
            _ => {}
        }
    }
    false
}

/// The `{` before the cursor on its line that no `}` closes.
fn unclosed_brace(bytes: &[u8], cursor: usize) -> Option<usize> {
    let mut at = cursor;
    while at > 0 {
        at -= 1;
        match bytes[at] {
            b'}' | b'\n' => return None,
            b'{' => return Some(at),
            _ => {}
        }
    }
    None
}

/// Whether byte `at` is inside a token of `kind`.
fn in_kind(tokens: &[Token], at: u32, kind: Kind) -> bool {
    let at = at as usize;
    let index = tokens.partition_point(|token| token.start <= at);
    index > 0 && {
        let token = &tokens[index - 1];
        token.kind == kind && at < token.end
    }
}

/// Whether the cursor is inside a comment or a string: after its first byte,
/// and before its end or at the end of one that is not closed.
fn inside_text(tokens: &[Token], bytes: &[u8], cursor: u32) -> bool {
    let at = cursor as usize;
    let index = tokens.partition_point(|token| token.start < at);
    if index == 0 {
        return false;
    }
    let token = &tokens[index - 1];
    let body = &bytes[token.start..token.end];
    // A comment that is not closed lexes as one invalid token.
    let open_comment = token.kind == Kind::Invalid && body.starts_with(b"/*");
    if !(matches!(token.kind, Kind::Comment | Kind::StringLiteral) || open_comment)
        || at > token.end
    {
        return false;
    }
    if at < token.end || open_comment {
        return true;
    }
    match token.kind {
        Kind::Comment => body.starts_with(b"//") || !(body.len() >= 4 && body.ends_with(b"*/")),
        _ => {
            let quote = body[0];
            body.len() < 2 || body.last() != Some(&quote)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn context(source: &str) -> Option<Context> {
        let at = source.find('|').expect("a cursor");
        let text = source.replacen('|', "", 1);
        let tokens = oxabl_lexer::tokenize(&text);
        find(&text, &tokens, at as u32).map(|found| found.context)
    }

    #[test]
    fn the_rows_of_the_table() {
        use Context::*;
        assert_eq!(context("|"), Some(StatementStart));
        assert_eq!(context("x = 1.\nfi|"), Some(StatementStart));
        assert_eq!(context("DO:\n  fi|"), Some(StatementStart));
        assert_eq!(context("IF x THEN fi|"), Some(StatementStart));
        assert_eq!(context("DEF |"), Some(DefineKeywords { modifiers: vec![] }));
        assert_eq!(
            context("DEFINE NEW SHARED va|"),
            Some(DefineKeywords {
                modifiers: vec![Kind::New, Kind::Shared]
            })
        );
        assert_eq!(context("FIND FIRST |"), Some(Records(RecordsAfter::Record)));
        assert_eq!(context("for each Cu|"), Some(Records(RecordsAfter::Record)));
        assert_eq!(
            context("DEFINE BUFFER b FOR |"),
            Some(Records(RecordsAfter::BufferFor))
        );
        assert_eq!(
            context("DEFINE BUFFER b FOR TEMP-TABLE |"),
            Some(Records(RecordsAfter::BufferForTempTable))
        );
        assert_eq!(
            context("x = Customer.|"),
            Some(Qualified {
                qualifier: (4, 12),
                after_record: false
            })
        );
        assert_eq!(
            context("FIND FIRST shop.|"),
            Some(Qualified {
                qualifier: (11, 15),
                after_record: true
            })
        );
        assert_eq!(
            context("DISPLAY shop.Customer.Na|"),
            Some(Qualified {
                qualifier: (8, 21),
                after_record: false
            })
        );
        assert_eq!(context("obj:|"), Some(Members));
        assert_eq!(context("FIND FIRST c WHERE |"), Some(Where));
        assert_eq!(context("FIND FIRST c WHERE c.x = |"), Some(Where));
        assert_eq!(context("RUN |"), Some(Run));
        assert_eq!(context("RUN util/he|"), Some(Run));
        assert_eq!(context("RUN x IN |"), None);
        assert_eq!(context("DEFINE VARIABLE x AS |"), Some(Types));
        assert_eq!(context("x = NEW |"), Some(Types));
        assert_eq!(context("METHOD PUBLIC |"), Some(Types));
        assert_eq!(context("x = |"), Some(Values { target: Some(0) }));
        assert_eq!(context("x = y + |"), Some(Values { target: None }));
        assert_eq!(context("x = IF a THEN |"), Some(Values { target: None }));
        assert_eq!(context("RETURN |"), Some(Values { target: None }));
        assert_eq!(context("{&na|"), Some(PreprocessorNames));
        assert_eq!(context("{1|"), Some(IncludeArguments));
        assert_eq!(context("{inc/|"), Some(IncludeFiles));
        assert_eq!(context("MESSAGE \"{&na|"), Some(PreprocessorNames));
    }

    #[test]
    fn a_branch_of_then_else_or_otherwise_is_marked() {
        let after = |source: &str| {
            let at = source.find('|').expect("a cursor");
            let text = source.replacen('|', "", 1);
            let tokens = oxabl_lexer::tokenize(&text);
            find(&text, &tokens, at as u32).map(|found| found.after_branch)
        };
        assert_eq!(after("IF x THEN |"), Some(true));
        assert_eq!(after("IF x THEN y = 1. ELSE |"), Some(true));
        assert_eq!(after("x = 1.\n|"), Some(false));
        assert_eq!(after("IF x THEN DO:\n|"), Some(false));
    }

    #[test]
    fn a_trigger_character_asks_for_the_contexts_it_starts() {
        assert!(Context::Members.takes_trigger(':'));
        assert!(!Context::StatementStart.takes_trigger('/'));
        assert!(!(Context::Values { target: None }).takes_trigger('/'));
        assert!(Context::Run.takes_trigger('/'));
        assert!(!Context::StatementStart.takes_trigger('.'));
        assert!(Context::StatementStart.takes_trigger('a'));
    }

    #[test]
    fn no_context_where_oxabl_does_not_know() {
        assert_eq!(context("/* fi| */"), None);
        assert_eq!(context("// fi|"), None);
        assert_eq!(context("/* fi|"), None);
        assert_eq!(context("MESSAGE \"fi|\"."), None);
        assert_eq!(context("FIND FIRST x NO-LOCK |"), None);
        assert_eq!(context("x = x |"), None);
        assert_eq!(context("MESSAGE \"a\".|"), None);
        assert_eq!(context("DO:|\nEND."), None);
        assert_eq!(context("{inc.i} |"), None);
        assert_eq!(context("x = {&y} + |"), None);
        assert_eq!(context("x = 1.\n/|"), None, "the start of a comment");
        assert_eq!(context("x = 1.|"), None, "a number is no qualifier");
        assert_eq!(
            context("{inc/x.i arg|"),
            None,
            "the arguments of an include"
        );
        assert_eq!(context("x = 1.\n*|"), None);
        assert_eq!(context("x = -|"), Some(Context::Values { target: None }));
    }
}
