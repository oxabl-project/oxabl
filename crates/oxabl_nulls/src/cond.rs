//! What a condition says about records: which buffers have a record on the
//! edge where it is true, and which on the edge where it is false (section
//! 9.3, G1, G2, G5-G7).

use oxabl_lexer::Kind;
use oxabl_scope::BufferId;
use smallvec::SmallVec;

use crate::analysis::Tok;

/// What one edge of a condition proves.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Proves {
    /// Buffers that have a record.
    pub buffers: SmallVec<[BufferId; 2]>,
    /// Buffers named in a form the grammar does not read: Unknown (G7).
    pub unknown: SmallVec<[BufferId; 2]>,
    /// `ERROR-STATUS:ERROR` is false.
    pub no_error: bool,
}

/// What a condition proves on each edge.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Cond {
    pub yes: Proves,
    pub no: Proves,
    /// `ERROR-STATUS` in a form the grammar does not read: the finds it
    /// links are Unknown on both edges.
    pub es_unknown: bool,
}

/// A reference at a token: its buffer, and whether it names a field.
pub(crate) type RefAt<'a> = &'a dyn Fn(u32) -> Option<(BufferId, bool, u32)>;

/// Read the condition in `toks` (comments removed).
pub(crate) fn read(toks: &[Tok], source: &str, refs: RefAt<'_>) -> Cond {
    let mut p = Parser {
        toks,
        source,
        refs,
        i: 0,
    };
    let c = p.or();
    if p.i < toks.len() {
        // Text the grammar does not follow: every buffer it names is Unknown.
        p.i = 0;
        return p.mentions(0, toks.len());
    }
    c
}

struct Parser<'a> {
    toks: &'a [Tok],
    source: &'a str,
    refs: RefAt<'a>,
    i: usize,
}

impl Parser<'_> {
    fn peek(&self) -> Option<Kind> {
        self.toks.get(self.i).map(|t| t.kind)
    }

    fn or(&mut self) -> Cond {
        let mut a = self.and();
        while self.peek() == Some(Kind::Or) {
            self.i += 1;
            let b = self.and();
            a = Cond {
                yes: common(&a.yes, &b.yes),
                no: union(&a.no, &b.no),
                es_unknown: a.es_unknown || b.es_unknown,
            };
        }
        a
    }

    fn and(&mut self) -> Cond {
        let mut a = self.not();
        while self.peek() == Some(Kind::And) {
            self.i += 1;
            let b = self.not();
            a = Cond {
                yes: union(&a.yes, &b.yes),
                no: common(&a.no, &b.no),
                es_unknown: a.es_unknown || b.es_unknown,
            };
        }
        a
    }

    fn not(&mut self) -> Cond {
        if self.peek() == Some(Kind::Not) {
            self.i += 1;
            let c = self.not();
            return Cond {
                yes: c.no,
                no: c.yes,
                es_unknown: c.es_unknown,
            };
        }
        self.primary()
    }

    fn primary(&mut self) -> Cond {
        let start = self.i;
        let c = match self.peek() {
            Some(Kind::LeftParen) => {
                self.i += 1;
                let c = self.or();
                (self.peek() == Some(Kind::RightParen)).then(|| {
                    self.i += 1;
                    c
                })
            }
            Some(Kind::Available) => self.available(),
            Some(Kind::Locked) => self.locked(),
            Some(Kind::ErrorStatus) => self.error_status(),
            _ => None,
        };
        if let Some(c) = c
            && matches!(
                self.peek(),
                None | Some(Kind::And | Kind::Or | Kind::RightParen)
            )
        {
            return c;
        }
        self.i = start;
        self.opaque()
    }

    /// `AVAILABLE b`, `AVAILABLE(b)`, `AVAILABLE db.b`.
    fn available(&mut self) -> Option<Cond> {
        let buffer = self.buffer_operand()?;
        Some(Cond {
            yes: Proves {
                buffers: smallvec::smallvec![buffer],
                ..Proves::default()
            },
            ..Cond::default()
        })
    }

    /// `LOCKED b`: true says `b` is empty, false says nothing (G6).
    fn locked(&mut self) -> Option<Cond> {
        self.buffer_operand()?;
        Some(Cond::default())
    }

    /// The buffer operand of `AVAILABLE` or `LOCKED`, after the keyword.
    fn buffer_operand(&mut self) -> Option<BufferId> {
        self.i += 1;
        let paren = self.peek() == Some(Kind::LeftParen);
        if paren {
            self.i += 1;
        }
        let at = self.toks.get(self.i)?.start;
        let (buffer, field, end) = (self.refs)(at)?;
        if field {
            return None;
        }
        while self.toks.get(self.i).is_some_and(|t| t.start < end) {
            self.i += 1;
        }
        if paren {
            if self.peek() != Some(Kind::RightParen) {
                return None;
            }
            self.i += 1;
        }
        Some(buffer)
    }

    /// `ERROR-STATUS:ERROR`, alone or compared with a logical constant
    /// (`= TRUE`, `<> NO`, ...).
    fn error_status(&mut self) -> Option<Cond> {
        let colon = self.toks.get(self.i + 1)?;
        let name = self.toks.get(self.i + 2)?;
        let text = self.source.get(name.start as usize..name.end as usize)?;
        if colon.kind != Kind::Colon || !text.eq_ignore_ascii_case("error") {
            return None;
        }
        self.i += 3;
        let mut error = true;
        if let Some(op) = self
            .peek()
            .filter(|k| matches!(k, Kind::Equals | Kind::Eq | Kind::NotEqual | Kind::Ne))
        {
            let value = match self.toks.get(self.i + 1).map(|t| t.kind)? {
                Kind::KwTrue | Kind::Yes => true,
                Kind::KwFalse | Kind::No => false,
                _ => return None,
            };
            self.i += 2;
            error = value == matches!(op, Kind::Equals | Kind::Eq);
        }
        let no_error = Proves {
            no_error: true,
            ..Proves::default()
        };
        Some(if error {
            Cond {
                no: no_error,
                ..Cond::default()
            }
        } else {
            Cond {
                yes: no_error,
                ..Cond::default()
            }
        })
    }

    /// An operand the grammar does not read, up to the next `AND` or `OR` at
    /// its level: the buffers it names at record level are Unknown on both
    /// edges (G7).
    fn opaque(&mut self) -> Cond {
        let start = self.i;
        let mut depth = 0u32;
        while let Some(k) = self.peek() {
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket if depth == 0 => break,
                Kind::RightParen | Kind::RightBracket => depth -= 1,
                Kind::And | Kind::Or if depth == 0 => break,
                _ => {}
            }
            self.i += 1;
        }
        if self.i == start && self.i < self.toks.len() {
            self.i += 1;
        }
        self.mentions(start, self.i)
    }

    fn mentions(&self, from: usize, to: usize) -> Cond {
        let mut unknown: SmallVec<[BufferId; 2]> = SmallVec::new();
        let mut es_unknown = false;
        for t in &self.toks[from..to] {
            es_unknown |= t.kind == Kind::ErrorStatus;
            if let Some((b, false, _)) = (self.refs)(t.start)
                && !unknown.contains(&b)
            {
                unknown.push(b);
            }
        }
        let p = Proves {
            unknown,
            ..Proves::default()
        };
        Cond {
            yes: p.clone(),
            no: p,
            es_unknown,
        }
    }
}

/// Both lists, once each.
fn merged(a: &[BufferId], b: &[BufferId]) -> SmallVec<[BufferId; 2]> {
    let mut out: SmallVec<[BufferId; 2]> = a.into();
    for x in b {
        if !out.contains(x) {
            out.push(*x);
        }
    }
    out
}

/// What two conditions prove when both hold. Unknown on either side stays
/// Unknown.
fn union(a: &Proves, b: &Proves) -> Proves {
    Proves {
        buffers: merged(&a.buffers, &b.buffers),
        unknown: merged(&a.unknown, &b.unknown),
        no_error: a.no_error || b.no_error,
    }
}

/// What two conditions prove when either holds.
fn common(a: &Proves, b: &Proves) -> Proves {
    Proves {
        buffers: a
            .buffers
            .iter()
            .filter(|x| b.buffers.contains(x))
            .copied()
            .collect(),
        unknown: merged(&a.unknown, &b.unknown),
        no_error: a.no_error && b.no_error,
    }
}
