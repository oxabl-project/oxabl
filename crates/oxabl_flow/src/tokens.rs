//! Reads block-header phrases and control statements from tokens.
//!
//! The AST drops what the CFG needs here: block headers keep no `ON` phrases,
//! `FOR` keeps no qualifier and `REPEAT`/`FOR` no `TRANSACTION`, and `UNDO`,
//! `QUIT` and `STOP` become empty or expression statements. So the lexer runs
//! again over the statement's own span. The span is in the same (expanded)
//! text the parser saw.

use oxabl_ast::Span;
use oxabl_lexer::{Kind, Lexer, Token};
use smallvec::SmallVec;

use crate::model::{Action, Condition, ErrorDirective, ForQualifier, Handling, Origin, ReturnKind};

/// The significant (non-comment) tokens of a span.
pub(crate) struct Toks<'s> {
    text: &'s str,
    toks: Vec<Token>,
}

impl<'s> Toks<'s> {
    /// Lex `span` of `src`, keeping at most `limit` tokens.
    pub(crate) fn new(src: &'s str, span: Span, limit: usize) -> Self {
        let text = slice(src, span);
        let toks = Lexer::new(text)
            .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof))
            .take(limit)
            .collect();
        Toks { text, toks }
    }

    fn len(&self) -> usize {
        self.toks.len()
    }

    fn kind(&self, i: usize) -> Option<Kind> {
        self.toks.get(i).map(|t| t.kind)
    }

    fn text(&self, i: usize) -> &'s str {
        self.toks
            .get(i)
            .and_then(|t| self.text.get(t.start..t.end))
            .unwrap_or("")
    }

    fn is_word(&self, i: usize, word: &str) -> bool {
        self.text(i).eq_ignore_ascii_case(word)
    }

    /// A token that can be a block label in this position.
    fn is_label(&self, i: usize) -> bool {
        let Some(kind) = self.kind(i) else {
            return false;
        };
        if matches!(
            kind,
            Kind::Comma
                | Kind::Colon
                | Kind::Period
                | Kind::On
                | Kind::With
                | Kind::KwWhile
                | Kind::Transaction
                | Kind::KwFor
                | Kind::By
                | Kind::KwBreak
                | Kind::Leave
                | Kind::Next
                | Kind::Retry
                | Kind::KwReturn
                | Kind::Throw
                | Kind::Undo
        ) {
            return false;
        }
        self.text(i)
            .bytes()
            .next()
            .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
    }

    fn label(&self, i: usize) -> Option<String> {
        self.is_label(i).then(|| self.text(i).to_string())
    }

    /// Whether token `i` starts where token `i - 1` ends.
    fn adjacent(&self, i: usize) -> bool {
        i > 0
            && matches!(
                (self.toks.get(i - 1), self.toks.get(i)),
                (Some(a), Some(b)) if a.end == b.start
            )
    }

    /// A buffer or table name at `i`, joining a `db.table` written as
    /// adjacent tokens.
    fn name(&self, i: usize) -> Option<String> {
        if !self.is_label(i) {
            return None;
        }
        let mut name = self.text(i).to_string();
        let mut j = i + 1;
        while self.kind(j) == Some(Kind::Period)
            && self.adjacent(j)
            && self.adjacent(j + 1)
            && self.is_label(j + 1)
        {
            name.push('.');
            name.push_str(self.text(j + 1));
            j += 2;
        }
        Some(name)
    }

    /// Parse `UNDO [label] [, action]` starting at the `UNDO` token. Returns
    /// the undo label, the action if one was written, and the next index.
    fn undo_clause(&self, at: usize) -> (Option<String>, Option<Action>, usize) {
        let mut j = at + 1;
        let undo = self.label(j);
        if undo.is_some() {
            j += 1;
        }
        let mut action = None;
        if self.kind(j) == Some(Kind::Comma) {
            j += 1;
            match self.kind(j) {
                Some(k @ (Kind::Leave | Kind::Next | Kind::Retry)) => {
                    j += 1;
                    let label = self.label(j);
                    if label.is_some() {
                        j += 1;
                    }
                    action = Some(match k {
                        Kind::Leave => Action::Leave(label),
                        Kind::Next => Action::Next(label),
                        _ => Action::Retry(label),
                    });
                }
                Some(Kind::Throw) => {
                    j += 1;
                    action = Some(Action::Throw);
                }
                Some(Kind::KwReturn) => {
                    j += 1;
                    let kind = if self.is_word(j, "error") {
                        j += 1;
                        ReturnKind::Error
                    } else if self.kind(j) == Some(Kind::NoApply) {
                        j += 1;
                        ReturnKind::NoApply
                    } else {
                        ReturnKind::Normal
                    };
                    action = Some(Action::Return(kind));
                }
                _ => {}
            }
        }
        (undo, action, j)
    }
}

fn slice(src: &str, span: Span) -> &str {
    let end = (span.end as usize).min(src.len());
    let start = (span.start as usize).min(end);
    src.get(start..end).unwrap_or("")
}

/// The phrases of a `DO`/`REPEAT`/`FOR` header that the AST does not keep.
#[derive(Debug, Default)]
pub(crate) struct Header {
    pub transaction: bool,
    pub stop_after: bool,
    pub has_while: bool,
    pub has_to: bool,
    pub for_qualifier: Option<ForQualifier>,
    /// `FOR` record-phrase buffers, or `DO FOR`/`REPEAT FOR` buffers.
    pub buffers: Vec<String>,
    pub phrases: SmallVec<[(Condition, Handling); 2]>,
}

/// Read the header of the block statement whose header occupies `span`.
pub(crate) fn read_header(src: &str, span: Span) -> Header {
    let t = Toks::new(src, span, usize::MAX);
    let mut h = Header::default();
    let for_block = t.kind(0) == Some(Kind::KwFor);
    if for_block {
        h.for_qualifier = Some(match t.kind(1) {
            Some(Kind::Each) => ForQualifier::Each,
            Some(Kind::First) => ForQualifier::First,
            Some(Kind::Last) => ForQualifier::Last,
            Some(Kind::Next) => ForQualifier::Next,
            Some(Kind::Prev) => ForQualifier::Prev,
            _ => ForQualifier::Unqualified,
        });
        let at = if h.for_qualifier == Some(ForQualifier::Unqualified) {
            1
        } else {
            2
        };
        h.buffers.extend(t.name(at));
    }
    let mut depth = 0u32;
    let mut i = 1;
    while i < t.len() {
        let kind = t.kind(i).unwrap_or(Kind::Eof);
        match kind {
            Kind::LeftParen | Kind::LeftBracket => depth += 1,
            Kind::RightParen | Kind::RightBracket => depth = depth.saturating_sub(1),
            _ if depth > 0 => {}
            // A joined record phrase: `, EACH order OF customer`.
            Kind::Comma
                if for_block
                    && matches!(
                        t.kind(i + 1),
                        Some(Kind::Each | Kind::First | Kind::Last | Kind::Next | Kind::Prev)
                    ) =>
            {
                h.buffers.extend(t.name(i + 2));
            }
            // `DO FOR b1, b2` / `REPEAT FOR b1`: strongly scoped buffers.
            Kind::KwFor if !for_block => {
                let mut j = i + 1;
                while let Some(name) = t.name(j) {
                    h.buffers.push(name);
                    j += 1;
                    while matches!(t.kind(j), Some(Kind::Period)) && t.adjacent(j) {
                        j += 2;
                    }
                    if t.kind(j) != Some(Kind::Comma) {
                        break;
                    }
                    j += 1;
                }
                i = j;
                continue;
            }
            Kind::On => {
                if let Some((condition, handling, next)) = on_phrase(&t, i) {
                    h.phrases.push((condition, handling));
                    i = next;
                    continue;
                }
            }
            Kind::Transaction if transaction_keyword(&t, i) => h.transaction = true,
            Kind::KwWhile => h.has_while = true,
            Kind::To => h.has_to = true,
            // The frame phrase is last and holds nothing the CFG needs.
            Kind::With => break,
            _ if t.is_word(i, "stop-after") => h.stop_after = true,
            _ => {}
        }
        i += 1;
    }
    h
}

/// `TRANSACTION` is also a function; the keyword is not an operand.
fn transaction_keyword(t: &Toks, i: usize) -> bool {
    !matches!(
        t.kind(i - 1),
        Some(
            Kind::KwWhile
                | Kind::KwWhere
                | Kind::And
                | Kind::Or
                | Kind::Not
                | Kind::Equals
                | Kind::Eq
                | Kind::Ne
                | Kind::Lt
                | Kind::Le
                | Kind::Gt
                | Kind::Ge
                | Kind::LessThan
                | Kind::GreaterThan
                | Kind::LessThanOrEqual
                | Kind::GreaterThanOrEqual
                | Kind::NotEqual
                | Kind::Comma
                | Kind::LeftParen
        )
    )
}

/// `ON {ERROR|ENDKEY|STOP|QUIT} [UNDO [label]] [, action]` at `i`.
fn on_phrase(t: &Toks, i: usize) -> Option<(Condition, Handling, usize)> {
    let condition = match t.kind(i + 1) {
        Some(Kind::Endkey) => Condition::Endkey,
        _ if t.is_word(i + 1, "end-key") => Condition::Endkey,
        Some(Kind::Quit) => Condition::Quit,
        _ if t.is_word(i + 1, "error") => Condition::Error,
        _ if t.is_word(i + 1, "stop") => Condition::Stop,
        _ => return None,
    };
    let (undo, action, next) = if t.kind(i + 2) == Some(Kind::Undo) {
        t.undo_clause(i + 2)
    } else {
        // `ON QUIT, LEAVE` and the like: no UNDO, only an action.
        let (_, action, next) = t.undo_clause(i + 1);
        (None, action, next)
    };
    let handling = Handling {
        undo,
        action: action.unwrap_or(Action::Retry(None)),
        origin: Origin::Phrase,
    };
    Some((condition, handling, next.max(i + 2)))
}

/// A statement the AST keeps lossily, read from its tokens.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Control {
    /// Nothing to model: `.`, an orphan `END`, a stray preprocessor token.
    Plain,
    /// `UNDO [label] [, action]`.
    Undo {
        label: Option<String>,
        action: Option<Action>,
    },
    Quit,
    Stop,
    /// `THROW expr` or `UNDO, THROW [expr]`.
    Throw,
    /// `RETURN ERROR [expr]` or `UNDO, RETURN ERROR [expr]`.
    ReturnError,
    Directive(ErrorDirective),
    /// Tokens that are none of the above.
    Unrecognized,
}

/// Classify the statement at `span`.
pub(crate) fn read_control(src: &str, span: Span) -> Control {
    let t = Toks::new(src, span, 16);
    match t.kind(0) {
        None | Some(Kind::Period) => Control::Plain,
        Some(
            Kind::End
            | Kind::PreprocEndif
            | Kind::PreprocElse
            | Kind::PreprocElseif
            | Kind::PreprocThen
            // An `ENUM` type: declarations only.
            | Kind::KwEnum,
        ) => Control::Plain,
        Some(Kind::Quit) => Control::Quit,
        Some(Kind::Throw) => Control::Throw,
        Some(Kind::Undo) => {
            let (label, action, _) = t.undo_clause(0);
            match action {
                Some(Action::Throw) => Control::Throw,
                Some(Action::Return(ReturnKind::Error)) => Control::ReturnError,
                action => Control::Undo { label, action },
            }
        }
        Some(Kind::KwReturn) if t.is_word(1, "error") => Control::ReturnError,
        // `BLOCK-LEVEL ON ERROR UNDO , THROW`
        Some(Kind::BlockLevel) if t.kind(5) == Some(Kind::Throw) => {
            Control::Directive(ErrorDirective::BlockLevel)
        }
        Some(Kind::RoutineLevel) if t.kind(5) == Some(Kind::Throw) => {
            Control::Directive(ErrorDirective::RoutineLevel)
        }
        _ if t.is_word(0, "stop") && matches!(t.kind(1), None | Some(Kind::Period)) => {
            Control::Stop
        }
        _ => Control::Unrecognized,
    }
}

/// How an unmodelled statement reads input.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Input {
    None,
    /// Reads a stream or waits for a key (`IMPORT`, `PAUSE`): may raise ENDKEY.
    Endkey,
    /// Blocks for user input: may raise ENDKEY, and makes `RETRY` real.
    Blocking,
}

pub(crate) fn skipped_input(src: &str, span: Span) -> Input {
    let t = Toks::new(src, span, 2);
    match t.kind(0) {
        Some(
            Kind::Update
            | Kind::Set
            | Kind::PromptFor
            | Kind::WaitFor
            | Kind::Readkey
            | Kind::Choose,
        ) => Input::Blocking,
        Some(Kind::Insert) if t.kind(1) != Some(Kind::Into) => Input::Blocking,
        Some(Kind::Import | Kind::Pause) => Input::Endkey,
        _ => Input::None,
    }
}

/// Whether the statement text ends in `NO-ERROR` (before the period). For
/// statement forms whose AST does not record the option.
pub(crate) fn ends_with_no_error(src: &str, span: Span) -> bool {
    let text = slice(src, span).trim_end();
    let text = text.strip_suffix('.').unwrap_or(text).trim_end();
    const WORD: &str = "no-error";
    let Some(cut) = text.len().checked_sub(WORD.len()) else {
        return false;
    };
    text.is_char_boundary(cut)
        && text[cut..].eq_ignore_ascii_case(WORD)
        && text[..cut]
            .bytes()
            .next_back()
            .is_none_or(|b| !(b.is_ascii_alphanumeric() || b == b'-' || b == b'_'))
}

/// Whether a `FUNCTION` statement with no body has a block (`:` ... `END`)
/// rather than being a `FORWARD` or `IN` declaration.
pub(crate) fn function_has_block(src: &str, span: Span) -> bool {
    slice(src, span).contains(':')
}

/// Whether an expression statement is the `STOP` statement.
pub(crate) fn is_stop_statement(src: &str, span: Span) -> bool {
    let text = slice(src, span).trim();
    let text = text.strip_suffix('.').unwrap_or(text).trim_end();
    text.eq_ignore_ascii_case("stop")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(src: &str) -> Span {
        Span {
            start: 0,
            end: src.len() as u32,
        }
    }

    fn header(src: &str) -> Header {
        read_header(src, span(src))
    }

    #[test]
    fn reads_every_on_phrase_with_labels() {
        let h = header(
            "DO ON ERROR UNDO outer, RETRY outer ON END-KEY UNDO, LEAVE \
             ON STOP UNDO, RETURN ERROR ON QUIT, NEXT:",
        );
        let got: Vec<(Condition, Option<&str>, &Action)> = h
            .phrases
            .iter()
            .map(|(c, h)| (*c, h.undo.as_deref(), &h.action))
            .collect();
        assert_eq!(
            got,
            [
                (
                    Condition::Error,
                    Some("outer"),
                    &Action::Retry(Some("outer".into()))
                ),
                (Condition::Endkey, None, &Action::Leave(None)),
                (Condition::Stop, None, &Action::Return(ReturnKind::Error)),
                (Condition::Quit, None, &Action::Next(None)),
            ]
        );
    }

    #[test]
    fn an_on_phrase_without_an_action_retries() {
        let h = header("REPEAT ON ERROR UNDO:");
        assert_eq!(h.phrases[0].1.action, Action::Retry(None));
    }

    #[test]
    fn transaction_keyword_but_not_function() {
        assert!(header("DO TRANSACTION:").transaction);
        assert!(header("FOR EACH c NO-LOCK TRANSACTION BY c.x:").transaction);
        assert!(!header("DO WHILE TRANSACTION:").transaction);
        assert!(!header("DO WHILE f(TRANSACTION):").transaction);
    }

    #[test]
    fn for_qualifier_and_loop_phrases() {
        assert_eq!(
            header("FOR LAST c, EACH o OF c:").for_qualifier,
            Some(ForQualifier::Last)
        );
        assert_eq!(
            header("FOR c WHERE c.x = 1:").for_qualifier,
            Some(ForQualifier::Unqualified)
        );
        assert!(header("REPEAT i = 1 TO 3:").has_to);
        assert!(header("REPEAT WHILE x:").has_while);
        assert!(header("DO STOP-AFTER 5:").stop_after);
        assert_eq!(
            header("FOR EACH c NO-LOCK, EACH o OF c, FIRST sports.item:").buffers,
            ["c", "o", "sports.item"]
        );
        assert_eq!(
            header("DO FOR customer, sports.order TRANSACTION:").buffers,
            ["customer", "sports.order"]
        );
        assert!(header("DO TRANSACTION:").buffers.is_empty());
        // Nothing in parentheses or after WITH is a phrase.
        let h = header("DO WHILE f(\"ON ERROR\") WITH FRAME f:");
        assert!(h.phrases.is_empty());
    }

    #[test]
    fn control_statements() {
        let c = |s: &str| read_control(s, span(s));
        assert_eq!(c("."), Control::Plain);
        assert_eq!(c("END."), Control::Plain);
        assert_eq!(c("QUIT."), Control::Quit);
        assert_eq!(c("THROW e."), Control::Throw);
        assert_eq!(c("UNDO, THROW."), Control::Throw);
        assert_eq!(c("RETURN ERROR."), Control::ReturnError);
        assert_eq!(c("UNDO lbl, RETURN ERROR \"x\"."), Control::ReturnError);
        assert_eq!(
            c("UNDO lbl, NEXT lbl."),
            Control::Undo {
                label: Some("lbl".into()),
                action: Some(Action::Next(Some("lbl".into())))
            }
        );
        assert_eq!(
            c("UNDO."),
            Control::Undo {
                label: None,
                action: None
            }
        );
        assert_eq!(
            c("BLOCK-LEVEL ON ERROR UNDO, THROW."),
            Control::Directive(ErrorDirective::BlockLevel)
        );
        assert_eq!(
            c("ROUTINE-LEVEL ON ERROR UNDO, THROW."),
            Control::Directive(ErrorDirective::RoutineLevel)
        );
        assert_eq!(c("garbage here."), Control::Unrecognized);
        assert!(is_stop_statement("STOP .", span("STOP .")));
    }

    #[test]
    fn no_error_suffix() {
        let t = |s: &str| ends_with_no_error(s, span(s));
        assert!(t("x = 1 NO-ERROR."));
        assert!(t("h:Foo() no-error ."));
        assert!(!t("x = my-no-error."));
        assert!(!t("x = 1."));
        assert!(!t("."));
    }

    #[test]
    fn input_statements() {
        let i = |s: &str| skipped_input(s, span(s));
        assert_eq!(i("UPDATE x."), Input::Blocking);
        assert_eq!(i("INSERT Customer."), Input::Blocking);
        assert_eq!(i("INSERT INTO t VALUES (1)."), Input::None);
        assert_eq!(i("IMPORT x."), Input::Endkey);
        assert_eq!(i("PUT x."), Input::None);
    }
}
