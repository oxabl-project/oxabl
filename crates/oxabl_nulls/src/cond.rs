//! What a condition says about records: which buffers have a record on the
//! edge where it is true, and which on the edge where it is false (section
//! 9.3, G1, G2, G5, G7).

use oxabl_lexer::Kind;
use oxabl_scope::BufferId;
use smallvec::SmallVec;

use crate::analysis::Tok;

/// What one edge of a condition proves.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Proves {
    /// Buffers that have a record (or are Unknown, G7).
    pub buffers: SmallVec<[BufferId; 2]>,
    /// `ERROR-STATUS:ERROR` is false.
    pub no_error: bool,
}

/// What a condition proves on each edge.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Cond {
    pub yes: Proves,
    pub no: Proves,
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
        Some(Cond {
            yes: Proves {
                buffers: smallvec::smallvec![buffer],
                no_error: false,
            },
            no: Proves::default(),
        })
    }

    /// `ERROR-STATUS:ERROR`.
    fn error_status(&mut self) -> Option<Cond> {
        let colon = self.toks.get(self.i + 1)?;
        let name = self.toks.get(self.i + 2)?;
        let text = self.source.get(name.start as usize..name.end as usize)?;
        if colon.kind != Kind::Colon || !text.eq_ignore_ascii_case("error") {
            return None;
        }
        self.i += 3;
        Some(Cond {
            yes: Proves::default(),
            no: Proves {
                buffers: SmallVec::new(),
                no_error: true,
            },
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
        let mut buffers: SmallVec<[BufferId; 2]> = SmallVec::new();
        for t in &self.toks[from..to] {
            if let Some((b, false, _)) = (self.refs)(t.start)
                && !buffers.contains(&b)
            {
                buffers.push(b);
            }
        }
        let p = Proves {
            buffers,
            no_error: false,
        };
        Cond {
            yes: p.clone(),
            no: p,
        }
    }
}

/// What two conditions prove when both hold.
fn union(a: &Proves, b: &Proves) -> Proves {
    let mut buffers = a.buffers.clone();
    for x in &b.buffers {
        if !buffers.contains(x) {
            buffers.push(*x);
        }
    }
    Proves {
        buffers,
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
        no_error: a.no_error && b.no_error,
    }
}
