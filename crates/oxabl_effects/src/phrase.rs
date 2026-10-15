//! Reading a record phrase from the tokens: the `OF` table, the `WHERE`
//! criteria, `USE-INDEX`, `TABLE-SCAN`, and the `BY` fields of the statement.

use std::ops::Range;

use oxabl_lexer::{Kind, Token};

/// A significant token (no comments).
#[derive(Debug, Clone, Copy)]
pub(crate) struct Tok {
    pub kind: Kind,
    pub start: u32,
    pub end: u32,
}

pub(crate) fn significant(tokens: &[Token]) -> Vec<Tok> {
    tokens
        .iter()
        .take_while(|t| t.kind != Kind::Eof)
        .filter(|t| t.kind != Kind::Comment)
        .map(|t| Tok {
            kind: t.kind,
            start: t.start as u32,
            end: t.end as u32,
        })
        .collect()
}

/// What a record phrase says about how its records are found.
#[derive(Debug, Default)]
pub(crate) struct Phrase {
    /// `EACH` before the buffer name.
    pub each: bool,
    /// The buffer name `OF` names, as written.
    pub of: Option<String>,
    /// The `WHERE` criteria, as a range of token indexes.
    pub criteria: Option<Range<usize>>,
    pub use_index: Option<String>,
    pub table_scan: bool,
    /// `FIND t constant`.
    pub key_value: bool,
    /// Text the preprocessor did not expand is in the phrase.
    pub unexpanded: bool,
    /// Where the phrase ends (an offset).
    pub end: u32,
}

pub(crate) struct Reader<'a> {
    pub source: &'a str,
    pub toks: &'a [Tok],
}

impl<'a> Reader<'a> {
    pub fn text(&self, i: usize) -> &'a str {
        let t = self.toks[i];
        self.source
            .get(t.start as usize..t.end as usize)
            .unwrap_or("")
    }

    /// The index of the token that starts at `at`.
    pub fn at(&self, at: u32) -> Option<usize> {
        let i = self.toks.partition_point(|t| t.start < at);
        (self.toks.get(i)?.start == at).then_some(i)
    }

    /// Whether the token at `i` is followed directly by the next one.
    fn touches_next(&self, i: usize) -> bool {
        self.toks
            .get(i + 1)
            .is_some_and(|n| n.start == self.toks[i].end)
    }

    /// Whether the `.` or `:` at `i` ends the statement or opens a body: it is
    /// followed by white space or nothing.
    fn ends(&self, i: usize) -> bool {
        let end = self.toks[i].end as usize;
        self.source[end..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace)
    }

    fn is_word(&self, i: usize, word: &str) -> bool {
        self.toks[i].kind == Kind::Identifier && self.text(i).eq_ignore_ascii_case(word)
    }

    /// The end of a possibly dotted name starting at `i`, and its text.
    pub fn name(&self, i: usize) -> (usize, String) {
        let mut j = i;
        let mut s = self.text(i).to_string();
        while self.touches_next(j)
            && self.toks.get(j + 1).map(|t| t.kind) == Some(Kind::Period)
            && self.touches_next(j + 1)
            && !self.ends(j + 1)
        {
            s.push('.');
            s.push_str(self.text(j + 2));
            j += 2;
        }
        (j + 1, s)
    }

    /// Read the phrase whose buffer name starts at token `name`, up to `limit`
    /// (an offset). `find`: a `FIND`, which may name a key value.
    pub fn phrase(&self, name: usize, limit: u32, find: bool) -> Phrase {
        let mut p = Phrase {
            each: name > 0 && self.toks[name - 1].kind == Kind::Each,
            ..Phrase::default()
        };
        let (mut i, _) = self.name(name);
        let mut depth = 0i32;
        let mut where_start: Option<usize> = None;
        let mut first = true;
        let close_where = |p: &mut Phrase, ws: &mut Option<usize>, at: usize| {
            if let Some(s) = ws.take() {
                p.criteria = Some(s..at);
            }
        };
        while i < self.toks.len() && self.toks[i].start < limit {
            let k = self.toks[i].kind;
            if matches!(
                k,
                Kind::Preprop | Kind::IncludeReference | Kind::IncludeArgReference
            ) {
                p.unexpanded = true;
            }
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket if depth > 0 => depth -= 1,
                _ if depth > 0 => {}
                Kind::RightParen | Kind::RightBracket | Kind::Comma => break,
                Kind::Colon | Kind::Period if self.ends(i) => break,
                Kind::By | Kind::KwBreak | Kind::Group | Kind::KwWhile | Kind::With => break,
                Kind::Transaction | Kind::On if where_start.is_none() => break,
                Kind::KwWhere => {
                    close_where(&mut p, &mut where_start, i);
                    where_start = Some(i + 1);
                }
                Kind::Of if where_start.is_none() => {
                    if i + 1 < self.toks.len() {
                        let (end, of) = self.name(i + 1);
                        p.of = Some(of);
                        i = end;
                        first = false;
                        continue;
                    }
                }
                Kind::UseIndex => {
                    close_where(&mut p, &mut where_start, i);
                    if i + 1 < self.toks.len() {
                        p.use_index = Some(self.text(i + 1).to_string());
                        i += 2;
                        first = false;
                        continue;
                    }
                }
                Kind::NoLock
                | Kind::ShareLock
                | Kind::ExclusiveLock
                | Kind::NoWait
                | Kind::NoPrefetch
                | Kind::NoError
                | Kind::QueryTuning => close_where(&mut p, &mut where_start, i),
                Kind::Fields | Kind::Except if where_start.is_none() => {}
                Kind::Identifier if self.is_word(i, "table-scan") => {
                    close_where(&mut p, &mut where_start, i);
                    p.table_scan = true;
                }
                Kind::Identifier
                    if where_start.is_some()
                        && (self.is_word(i, "outer-join") || self.is_word(i, "left")) =>
                {
                    close_where(&mut p, &mut where_start, i);
                }
                _ if first && find && where_start.is_none() => p.key_value = true,
                _ => {}
            }
            first = false;
            i += 1;
        }
        close_where(&mut p, &mut where_start, i);
        p.end = self.toks.get(i).map_or(limit, |t| t.start.min(limit));
        p
    }

    /// The `BY` fields of a block header or `OPEN QUERY` statement, from
    /// token `from` up to `limit`.
    pub fn by_fields(&self, from: usize, limit: u32) -> Vec<String> {
        let mut out = Vec::new();
        let mut depth = 0i32;
        let mut i = from;
        while i < self.toks.len() && self.toks[i].start < limit {
            match self.toks[i].kind {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                Kind::Colon | Kind::Period if depth == 0 && self.ends(i) => break,
                Kind::By
                    if depth == 0
                        && i + 1 < self.toks.len()
                        && self.toks[i + 1].kind != Kind::LeftParen =>
                {
                    let (end, name) = self.name(i + 1);
                    // A BY expression (`BY f + 1`, `BY SUBSTRING(...)`) sorts on
                    // no field.
                    let plain = self.toks.get(end).is_none_or(|t| {
                        matches!(
                            t.kind,
                            Kind::By
                                | Kind::Colon
                                | Kind::Period
                                | Kind::Descending
                                | Kind::Ascending
                                | Kind::KwWhile
                                | Kind::Transaction
                                | Kind::With
                                | Kind::On
                                | Kind::KwBreak
                        ) || t.start >= limit
                    });
                    out.push(if plain { name } else { String::new() });
                    i = end;
                    continue;
                }
                _ => {}
            }
            i += 1;
        }
        out
    }
}
