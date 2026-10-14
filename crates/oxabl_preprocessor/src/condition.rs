use crate::PreprocVarTable;
use crate::var_table::Origin;

/// What an `&IF` condition is evaluated against.
pub struct Environment<'a> {
    pub vars: &'a PreprocVarTable,
    /// The `OPSYS` function. Unlike `{&OPSYS}` a `&SCOPED-DEFINE` cannot shadow it.
    pub opsys: &'a str,
    /// The `PROVERSION` function; `None` when the environment does not say.
    pub proversion: Option<&'a str>,
}

/// A part of a condition that was not decided from facts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Gap {
    /// `PROVERSION` was read and the environment gives no version.
    UnsetProversion,
    /// A function, name, operator or shape the evaluator does not model. Holds
    /// the text to show.
    Unmodelled(String),
}

/// The outcome of one condition: the branch taken and every gap behind it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Evaluation {
    pub value: bool,
    pub gaps: Vec<Gap>,
}

/// Evaluate a preprocessor `&IF` condition.
///
/// Modelled:
/// - `{&name}` references, expanded first (an undefined one is empty)
/// - literals: strings (with an ignored `:U`-style attribute), integers, decimals
///   (compared only), `TRUE`/`YES`, `FALSE`/`NO`
/// - `=` `<>` `<` `>` `<=` `>=` (and `EQ` `NE` `LT` `GT` `LE` `GE`), `BEGINS`, `MATCHES`
/// - `NOT`, `AND`, `OR`, parentheses
/// - `+` `-` `*` `MODULO` on integers, `+` on strings
/// - `DEFINED(name)` (0 undefined, 1 `&GLOBAL-DEFINE`, 2 include argument,
///   3 `&SCOPED-DEFINE`), `OPSYS`, `PROVERSION`, `LOGICAL`, `INTEGER`,
///   `DECIMAL` (integral values only), `STRING`, `SUBSTRING`, `LENGTH`, `TRIM`,
///   `LEFT-TRIM`, `RIGHT-TRIM`, `LC`, `INDEX`, `R-INDEX`, `ENTRY`,
///   `NUM-ENTRIES`, `LOOKUP`, `ABSOLUTE`, `MAXIMUM`, `MINIMUM`
///
/// Comparisons are case-insensitive (ABL semantics). Two strings compare as text
/// and two numbers as numbers; a string against a number, or either against a
/// logical, is a [`Gap`], as is a `MAXIMUM` or `MINIMUM` over mixed types.
///
/// Everything else is a [`Gap`]: the evaluator still returns a branch, but the
/// caller must treat it as a guess. That covers every other function (`SEARCH`
/// asks whether a file exists, which no evaluator here can know), every bare
/// name, `/`, arithmetic on decimals, and any token left over once the expression ends.
pub fn evaluate(condition: &str, env: &Environment<'_>) -> Evaluation {
    let expanded = expand_refs(condition, env.vars);
    let tokens = tokenize(&expanded);
    let mut parser = CondParser {
        tokens: &tokens,
        pos: 0,
        env,
        gaps: Vec::new(),
    };
    let value = parser.parse_or();
    if parser.pos < tokens.len() {
        parser.gap(format!("unexpected {}", describe(&tokens[parser.pos])));
    }
    let value = parser.truth(&value);
    Evaluation {
        value,
        gaps: parser.gaps,
    }
}

/// Expand `{&name}` references in the condition string.
fn expand_refs(input: &str, vars: &PreprocVarTable) -> String {
    let mut result = String::with_capacity(input.len());
    let mut rest = input;

    while let Some(open) = rest.find("{&") {
        let Some(close) = rest[open..].find('}') else {
            break;
        };
        result.push_str(&rest[..open]);
        if let Some(val) = vars.get(&rest[open + 2..open + close]) {
            result.push_str(val);
        }
        // If not defined, expand to empty string (ABL behaviour)
        rest = &rest[open + close + 1..];
    }
    result.push_str(rest);
    result
}

#[derive(Debug, Clone, PartialEq)]
enum CondToken {
    /// `DEFINED(name)`, with the name as written (it may hold dots).
    Defined(String),
    /// `DEFINED` not followed by a parenthesised name.
    BareDefined,
    Not,
    And,
    Or,
    True,
    False,
    Eq,      // = or EQ
    Neq,     // <> or NE
    Lt,      // < or LT
    Gt,      // > or GT
    Le,      // <= or LE
    Ge,      // >= or GE
    Matches, // MATCHES (glob-style pattern matching)
    Begins,  // BEGINS (string prefix matching)
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    Comma,
    LParen,
    RParen,
    StringLit(String),
    Number(i64),
    /// A number with a fractional part, kept as written.
    Decimal(String),
    Ident(String),
    /// A character no part of the grammar uses.
    Other(char),
}

fn describe(token: &CondToken) -> String {
    match token {
        CondToken::Ident(name) => format!("name '{name}'"),
        CondToken::Decimal(text) => format!("decimal '{text}'"),
        CondToken::Other(c) => format!("character '{c}'"),
        CondToken::Plus => "'+'".into(),
        CondToken::Minus => "'-'".into(),
        CondToken::Star => "'*'".into(),
        CondToken::Slash => "'/'".into(),
        CondToken::Comma => "','".into(),
        CondToken::LParen => "'('".into(),
        CondToken::RParen => "')'".into(),
        other => format!("{other:?}").to_ascii_uppercase(),
    }
}

/// Whether a token ends a value, so a following `-` is a subtraction.
fn ends_value(token: Option<&CondToken>) -> bool {
    matches!(
        token,
        Some(
            CondToken::RParen
                | CondToken::StringLit(_)
                | CondToken::Number(_)
                | CondToken::Decimal(_)
                | CondToken::Ident(_)
                | CondToken::True
                | CondToken::False
        )
    )
}

fn tokenize(input: &str) -> Vec<CondToken> {
    let mut tokens = Vec::new();
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b' ' | b'\t' | b'\r' | b'\n' => i += 1,
            b'(' => {
                tokens.push(CondToken::LParen);
                i += 1;
            }
            b')' => {
                tokens.push(CondToken::RParen);
                i += 1;
            }
            b',' => {
                tokens.push(CondToken::Comma);
                i += 1;
            }
            b'+' => {
                tokens.push(CondToken::Plus);
                i += 1;
            }
            b'*' => {
                tokens.push(CondToken::Star);
                i += 1;
            }
            b'/' if bytes.get(i + 1) == Some(&b'*') => i = skip_comment(bytes, i),
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                i += bytes[i..]
                    .iter()
                    .position(|b| *b == b'\n')
                    .unwrap_or(bytes.len() - i);
            }
            b'/' => {
                tokens.push(CondToken::Slash);
                i += 1;
            }
            b'=' => {
                tokens.push(CondToken::Eq);
                i += 1;
            }
            b'<' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    tokens.push(CondToken::Neq);
                    i += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(CondToken::Le);
                    i += 2;
                } else {
                    tokens.push(CondToken::Lt);
                    i += 1;
                }
            }
            b'>' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(CondToken::Ge);
                    i += 2;
                } else {
                    tokens.push(CondToken::Gt);
                    i += 1;
                }
            }
            b'"' | b'\'' => {
                let quote = bytes[i];
                i += 1;
                let start = i;
                while i < bytes.len() && bytes[i] != quote {
                    i += 1;
                }
                let s = String::from_utf8_lossy(&bytes[start..i]).into_owned();
                tokens.push(CondToken::StringLit(s));
                if i < bytes.len() {
                    i += 1; // skip closing quote
                } else {
                    tokens.push(CondToken::Other(quote as char));
                }
                // ABL string attributes after a *quoted literal* (`"x":U`,
                // `"":U`) are compile-time markers (untranslated, etc.). Ignore
                // them in preprocessor conditions so `"":U = "":U` compares as
                // equal (ADM2 fn/fnarg).
                //
                // Scope note: this only strips `:U`/`:L`/… immediately after a
                // string literal token. A bare `{&macro}:U` that expands to an
                // *unquoted* value (e.g. define `x` = `hello`, condition
                // `{&x}:U = "hello"`) is not covered — ADM2 always writes the
                // quoted form `"{N}":U` / `"{&name}":U`, which we handle.
                if i < bytes.len() && bytes[i] == b':' {
                    i += 1;
                    while i < bytes.len() && bytes[i].is_ascii_alphabetic() {
                        i += 1;
                    }
                }
            }
            b'0'..=b'9' => {
                let (token, end) = number(input, i, false);
                tokens.push(token);
                i = end;
            }
            b'-' => {
                if i + 1 < bytes.len()
                    && bytes[i + 1].is_ascii_digit()
                    && !ends_value(tokens.last())
                {
                    let (token, end) = number(input, i + 1, true);
                    tokens.push(token);
                    i = end;
                } else {
                    tokens.push(CondToken::Minus);
                    i += 1;
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let start = i;
                while i < bytes.len()
                    && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'-')
                {
                    i += 1;
                }
                let word = &input[start..i];
                match word.to_ascii_uppercase().as_str() {
                    "DEFINED" => {
                        let (token, end) = defined(input, i);
                        tokens.push(token);
                        i = end;
                    }
                    "NOT" => tokens.push(CondToken::Not),
                    "AND" => tokens.push(CondToken::And),
                    "OR" => tokens.push(CondToken::Or),
                    "TRUE" | "YES" => tokens.push(CondToken::True),
                    "FALSE" | "NO" => tokens.push(CondToken::False),
                    "EQ" => tokens.push(CondToken::Eq),
                    "NE" => tokens.push(CondToken::Neq),
                    "LT" => tokens.push(CondToken::Lt),
                    "GT" => tokens.push(CondToken::Gt),
                    "LE" => tokens.push(CondToken::Le),
                    "GE" => tokens.push(CondToken::Ge),
                    "MATCHES" => tokens.push(CondToken::Matches),
                    "BEGINS" => tokens.push(CondToken::Begins),
                    "MODULO" => tokens.push(CondToken::Modulo),
                    _ => tokens.push(CondToken::Ident(word.to_string())),
                }
            }
            _ => {
                let c = input[i..].chars().next().unwrap_or('\u{fffd}');
                tokens.push(CondToken::Other(c));
                i += c.len_utf8();
            }
        }
    }

    tokens
}

/// The offset after the block comment that starts at `start`. Comments nest.
pub(crate) fn skip_comment(bytes: &[u8], start: usize) -> usize {
    let mut depth = 0;
    let mut i = start;
    while i < bytes.len() {
        if bytes[i..].starts_with(b"/*") {
            depth += 1;
            i += 2;
        } else if bytes[i..].starts_with(b"*/") {
            depth -= 1;
            i += 2;
            if depth == 0 {
                return i;
            }
        } else {
            i += 1;
        }
    }
    bytes.len()
}

/// Read the argument of the `DEFINED` that ends at `at`.
fn defined(input: &str, at: usize) -> (CondToken, usize) {
    let rest = &input[at..];
    let open = rest.len() - rest.trim_start_matches(' ').len();
    if !rest[open..].starts_with('(') {
        return (CondToken::BareDefined, at);
    }
    let Some(close) = rest[open..].find(')') else {
        return (CondToken::BareDefined, at);
    };
    let name = rest[open + 1..open + close].trim();
    let name = name.trim_matches(['"', '\'']).trim();
    (CondToken::Defined(name.to_string()), at + open + close + 1)
}

/// Read the number whose digits start at `start`.
fn number(input: &str, start: usize, negative: bool) -> (CondToken, usize) {
    let bytes = input.as_bytes();
    let mut end = start;
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    if end + 1 < bytes.len() && bytes[end] == b'.' && bytes[end + 1].is_ascii_digit() {
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
        let sign = if negative { "-" } else { "" };
        return (
            CondToken::Decimal(format!("{sign}{}", &input[start..end])),
            end,
        );
    }
    match input[start..end].parse::<i64>() {
        Ok(n) => (CondToken::Number(if negative { -n } else { n }), end),
        Err(_) => (CondToken::Decimal(input[start..end].to_string()), end),
    }
}

/// A decimal literal, compared exactly.
#[derive(Debug, Clone)]
struct Decimal {
    text: String,
    negative: bool,
    /// Digits before the point, without leading zeros.
    int: String,
    /// Digits after the point, without trailing zeros.
    frac: String,
}

impl Decimal {
    /// Read `[-]digits[.digits]`; `None` for anything else.
    fn parse(text: &str) -> Option<Decimal> {
        let text = text.trim();
        let (negative, digits) = match text.strip_prefix('-') {
            Some(rest) => (true, rest),
            None => (false, text),
        };
        let (int, frac) = digits.split_once('.').unwrap_or((digits, ""));
        let all_digits = |s: &str| s.bytes().all(|b| b.is_ascii_digit());
        if int.is_empty() || !all_digits(int) || !all_digits(frac) {
            return None;
        }
        Some(Decimal {
            text: text.to_string(),
            negative,
            int: int.trim_start_matches('0').to_string(),
            frac: frac.trim_end_matches('0').to_string(),
        })
    }

    fn is_zero(&self) -> bool {
        self.int.is_empty() && self.frac.is_empty()
    }

    fn cmp(&self, other: &Decimal) -> std::cmp::Ordering {
        use std::cmp::Ordering::{Equal, Greater, Less};
        let (a_neg, b_neg) = (
            self.negative && !self.is_zero(),
            other.negative && !other.is_zero(),
        );
        if a_neg != b_neg {
            return if a_neg { Less } else { Greater };
        }
        let magnitude = self
            .int
            .len()
            .cmp(&other.int.len())
            .then_with(|| self.int.cmp(&other.int))
            .then_with(|| self.frac.cmp(&other.frac));
        match (a_neg, magnitude) {
            (_, Equal) => Equal,
            (false, m) => m,
            (true, m) => m.reverse(),
        }
    }
}

#[derive(Debug, Clone)]
enum CondValue {
    /// A decimal with a fractional part; compared, never computed with.
    Dec(Decimal),
    Bool(bool),
    Str(String),
    Num(i64),
    /// A number known only to be above zero: `DEFINED` of a name that is more
    /// than one of a global, a scoped define and an include argument.
    Positive,
    /// The value of a part that was not modelled; a gap is already recorded for it.
    Unknown,
}

/// Recursive-descent parser for preprocessor conditions.
///
/// Precedence (lowest to highest): OR, AND, NOT, comparison, `+ -`,
/// `* / MODULO`, primary.
struct CondParser<'a> {
    tokens: &'a [CondToken],
    pos: usize,
    env: &'a Environment<'a>,
    gaps: Vec<Gap>,
}

impl<'a> CondParser<'a> {
    fn peek(&self) -> Option<&CondToken> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&CondToken> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn eat(&mut self, expected: &CondToken) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn gap(&mut self, what: String) {
        let gap = Gap::Unmodelled(what);
        if !self.gaps.contains(&gap) {
            self.gaps.push(gap);
        }
    }

    /// A value in a place that needs a truth value.
    fn truth(&mut self, v: &CondValue) -> bool {
        match v {
            CondValue::Bool(b) => *b,
            CondValue::Num(n) => *n != 0,
            CondValue::Dec(d) => !d.is_zero(),
            CondValue::Unknown => false,
            CondValue::Positive => true,
            CondValue::Str(s) => match s.trim().to_ascii_lowercase().as_str() {
                "yes" | "true" => true,
                "no" | "false" => false,
                _ => {
                    self.gap("a string used as a logical".into());
                    !s.is_empty()
                }
            },
        }
    }

    /// OR has lowest precedence.
    fn parse_or(&mut self) -> CondValue {
        let first = self.parse_and();
        if self.peek() != Some(&CondToken::Or) {
            return first;
        }
        let mut result = self.truth(&first);
        while self.eat(&CondToken::Or) {
            let rhs = self.parse_and();
            let rhs = self.truth(&rhs);
            result = result || rhs;
        }
        CondValue::Bool(result)
    }

    fn parse_and(&mut self) -> CondValue {
        let first = self.parse_not();
        if self.peek() != Some(&CondToken::And) {
            return first;
        }
        let mut result = self.truth(&first);
        while self.eat(&CondToken::And) {
            let rhs = self.parse_not();
            let rhs = self.truth(&rhs);
            result = result && rhs;
        }
        CondValue::Bool(result)
    }

    fn parse_not(&mut self) -> CondValue {
        if self.eat(&CondToken::Not) {
            let v = self.parse_not();
            return CondValue::Bool(!self.truth(&v));
        }
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> CondValue {
        let lhs = self.parse_additive();
        match self.peek() {
            Some(
                CondToken::Eq
                | CondToken::Neq
                | CondToken::Lt
                | CondToken::Gt
                | CondToken::Le
                | CondToken::Ge
                | CondToken::Matches
                | CondToken::Begins,
            ) => {
                let op = self.advance().unwrap().clone();
                let rhs = self.parse_additive();
                CondValue::Bool(self.compare(&lhs, &op, &rhs))
            }
            _ => lhs,
        }
    }

    fn parse_additive(&mut self) -> CondValue {
        let mut lhs = self.parse_term();
        while let Some(op @ (CondToken::Plus | CondToken::Minus)) = self.peek().cloned() {
            self.pos += 1;
            let rhs = self.parse_term();
            let result = match (&lhs, &op, &rhs) {
                (CondValue::Unknown, ..) | (_, _, CondValue::Unknown) => Some(CondValue::Unknown),
                (CondValue::Num(a), CondToken::Plus, CondValue::Num(b)) => {
                    a.checked_add(*b).map(CondValue::Num)
                }
                (CondValue::Num(a), CondToken::Minus, CondValue::Num(b)) => {
                    a.checked_sub(*b).map(CondValue::Num)
                }
                (CondValue::Str(a), CondToken::Plus, CondValue::Str(b)) => {
                    Some(CondValue::Str(format!("{a}{b}")))
                }
                _ => None,
            };
            lhs = result.unwrap_or_else(|| {
                self.gap(format!("the operator {}", describe(&op)));
                CondValue::Unknown
            });
        }
        lhs
    }

    fn parse_term(&mut self) -> CondValue {
        let mut lhs = self.parse_primary();
        while let Some(op @ (CondToken::Star | CondToken::Slash | CondToken::Modulo)) =
            self.peek().cloned()
        {
            self.pos += 1;
            let rhs = self.parse_primary();
            let result = match (&lhs, &op, &rhs) {
                (CondValue::Unknown, ..) | (_, _, CondValue::Unknown) => Some(CondValue::Unknown),
                (CondValue::Num(a), CondToken::Star, CondValue::Num(b)) => {
                    a.checked_mul(*b).map(CondValue::Num)
                }
                (CondValue::Num(a), CondToken::Modulo, CondValue::Num(b)) if *b != 0 => {
                    a.checked_rem(*b).map(CondValue::Num)
                }
                // `/` gives a decimal in ABL.
                _ => None,
            };
            lhs = result.unwrap_or_else(|| {
                self.gap(format!("the operator {}", describe(&op)));
                CondValue::Unknown
            });
        }
        lhs
    }

    fn parse_primary(&mut self) -> CondValue {
        let Some(token) = self.advance().cloned() else {
            self.gap("an expression that ends early".into());
            return CondValue::Unknown;
        };
        match token {
            CondToken::Defined(name) => match self.env.vars.origin(&name) {
                None => CondValue::Num(0),
                Some(Origin::Global) => CondValue::Num(1),
                Some(Origin::Argument) => CondValue::Num(2),
                Some(Origin::Scoped) => CondValue::Num(3),
                // The exact answer for a name that is several is not known.
                Some(Origin::Mixed) => CondValue::Positive,
            },
            CondToken::BareDefined => {
                self.gap("DEFINED without a name in parentheses".into());
                CondValue::Unknown
            }
            CondToken::LParen => {
                let result = self.parse_or();
                if !self.eat(&CondToken::RParen) {
                    self.gap("a missing ')'".into());
                }
                result
            }
            CondToken::StringLit(s) => CondValue::Str(s),
            CondToken::Number(n) => CondValue::Num(n),
            CondToken::Decimal(text) => match Decimal::parse(&text) {
                Some(d) => CondValue::Dec(d),
                None => {
                    self.gap(format!("the number '{text}'"));
                    CondValue::Unknown
                }
            },
            CondToken::True => CondValue::Bool(true),
            CondToken::False => CondValue::Bool(false),
            CondToken::Not => {
                self.pos -= 1;
                self.parse_not()
            }
            CondToken::Minus => match self.parse_primary() {
                CondValue::Num(n) if n != i64::MIN => CondValue::Num(-n),
                _ => {
                    self.gap("the operator '-'".into());
                    CondValue::Unknown
                }
            },
            CondToken::Ident(name) => self.parse_name(&name),
            other => {
                self.gap(format!("unexpected {}", describe(&other)));
                CondValue::Unknown
            }
        }
    }

    /// A bare name: a zero-argument function or a function call.
    fn parse_name(&mut self, name: &str) -> CondValue {
        let upper = name.to_ascii_uppercase();
        if self.peek() != Some(&CondToken::LParen) {
            return match upper.as_str() {
                "OPSYS" => CondValue::Str(self.env.opsys.to_string()),
                "PROVERSION" => match self.env.proversion {
                    Some(version) => CondValue::Str(version.to_string()),
                    None => {
                        if !self.gaps.contains(&Gap::UnsetProversion) {
                            self.gaps.push(Gap::UnsetProversion);
                        }
                        CondValue::Unknown
                    }
                },
                _ => match name.parse::<i64>() {
                    Ok(n) => CondValue::Num(n),
                    Err(_) => {
                        self.gap(format!("the name '{name}'"));
                        CondValue::Unknown
                    }
                },
            };
        }
        self.pos += 1; // `(`
        let mut args = Vec::new();
        if !self.eat(&CondToken::RParen) {
            loop {
                args.push(self.parse_or());
                if self.eat(&CondToken::Comma) {
                    continue;
                }
                if !self.eat(&CondToken::RParen) {
                    self.gap("a missing ')'".into());
                }
                break;
            }
        }
        if args.iter().any(|a| matches!(a, CondValue::Unknown)) {
            return CondValue::Unknown;
        }
        match call(&upper, &args) {
            Some(v) => v,
            None => {
                self.gap(format!("the function {upper}"));
                CondValue::Unknown
            }
        }
    }

    fn compare(&mut self, lhs: &CondValue, op: &CondToken, rhs: &CondValue) -> bool {
        if matches!(lhs, CondValue::Unknown) || matches!(rhs, CondValue::Unknown) {
            return false;
        }
        for (positive, other, flipped) in [(lhs, rhs, false), (rhs, lhs, true)] {
            if !matches!(positive, CondValue::Positive) {
                continue;
            }
            // Only a comparison with zero does not need the number itself.
            let (Some(0), true) = (as_number(other), !matches!(other, CondValue::Positive)) else {
                self.gap("DEFINED of a name that is defined in more than one way".into());
                return false;
            };
            let op = match (op, flipped) {
                (CondToken::Lt, true) => &CondToken::Gt,
                (CondToken::Gt, true) => &CondToken::Lt,
                (CondToken::Le, true) => &CondToken::Ge,
                (CondToken::Ge, true) => &CondToken::Le,
                (op, _) => op,
            };
            return matches!(op, CondToken::Neq | CondToken::Gt | CondToken::Ge);
        }
        match (type_of(lhs), type_of(rhs)) {
            // The AVM rejects it. The branch is a guess, read the loose way.
            (Some(a), Some(b)) if a != b => {
                self.gap(format!(
                    "a comparison of {} with {}",
                    a.describe(),
                    b.describe()
                ));
                compare_values(lhs, op, rhs)
            }
            // Two character values compare as text, however they look.
            _ => match (lhs, rhs) {
                (CondValue::Str(l), CondValue::Str(r)) => compare_text(l, op, r),
                _ => compare_values(lhs, op, rhs),
            },
        }
    }
}

/// The data type of a value, as far as a comparison cares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueType {
    Text,
    Number,
    Logical,
}

impl ValueType {
    fn describe(self) -> &'static str {
        match self {
            ValueType::Text => "a string",
            ValueType::Number => "a number",
            ValueType::Logical => "a logical",
        }
    }
}

fn type_of(v: &CondValue) -> Option<ValueType> {
    match v {
        CondValue::Str(_) => Some(ValueType::Text),
        CondValue::Num(_) | CondValue::Dec(_) => Some(ValueType::Number),
        CondValue::Bool(_) => Some(ValueType::Logical),
        CondValue::Positive | CondValue::Unknown => None,
    }
}

fn compare_values(lhs: &CondValue, op: &CondToken, rhs: &CondValue) -> bool {
    if (matches!(lhs, CondValue::Dec(_)) || matches!(rhs, CondValue::Dec(_)))
        && let (Some(l), Some(r)) = (as_decimal(lhs), as_decimal(rhs))
    {
        use std::cmp::Ordering::{Equal, Greater, Less};
        let ord = l.cmp(&r);
        match op {
            CondToken::Eq => return ord == Equal,
            CondToken::Neq => return ord != Equal,
            CondToken::Lt => return ord == Less,
            CondToken::Gt => return ord == Greater,
            CondToken::Le => return ord != Greater,
            CondToken::Ge => return ord != Less,
            _ => {}
        }
    }
    // Only reached for two numbers, two logicals, or a mixed guess.
    if let (Some(l), Some(r)) = (as_number(lhs), as_number(rhs)) {
        match op {
            CondToken::Eq => return l == r,
            CondToken::Neq => return l != r,
            CondToken::Lt => return l < r,
            CondToken::Gt => return l > r,
            CondToken::Le => return l <= r,
            CondToken::Ge => return l >= r,
            _ => {}
        }
    }

    compare_text(&as_string(lhs), op, &as_string(rhs))
}

/// Case-insensitive comparison as text.
fn compare_text(lhs: &str, op: &CondToken, rhs: &str) -> bool {
    let l = lhs.to_ascii_lowercase();
    let r = rhs.to_ascii_lowercase();
    match op {
        CondToken::Eq => l == r,
        CondToken::Neq => l != r,
        CondToken::Lt => l < r,
        CondToken::Gt => l > r,
        CondToken::Le => l <= r,
        CondToken::Ge => l >= r,
        CondToken::Matches => glob_matches(&l, &r),
        CondToken::Begins => l.starts_with(&r),
        _ => false,
    }
}

/// ABL MATCHES operator: `*` matches any sequence, `.` matches any single char.
/// Both strings should already be lowercased for case-insensitive comparison.
fn glob_matches(text: &str, pattern: &str) -> bool {
    let t = text.as_bytes();
    let p = pattern.as_bytes();
    let (tlen, plen) = (t.len(), p.len());
    let mut ti = 0;
    let mut pi = 0;
    let mut star_pi = usize::MAX;
    let mut star_ti = 0;

    while ti < tlen {
        if pi < plen && (p[pi] == b'.' || p[pi] == t[ti]) {
            ti += 1;
            pi += 1;
        } else if pi < plen && p[pi] == b'*' {
            star_pi = pi;
            star_ti = ti;
            pi += 1;
        } else if star_pi != usize::MAX {
            pi = star_pi + 1;
            star_ti += 1;
            ti = star_ti;
        } else {
            return false;
        }
    }

    while pi < plen && p[pi] == b'*' {
        pi += 1;
    }

    pi == plen
}

fn as_number(v: &CondValue) -> Option<i64> {
    match v {
        CondValue::Num(n) => Some(*n),
        CondValue::Str(s) => s.trim().parse().ok(),
        CondValue::Bool(b) => Some(i64::from(*b)),
        CondValue::Dec(_) | CondValue::Positive | CondValue::Unknown => None,
    }
}

fn as_decimal(v: &CondValue) -> Option<Decimal> {
    match v {
        CondValue::Dec(d) => Some(d.clone()),
        CondValue::Num(n) => Decimal::parse(&n.to_string()),
        CondValue::Str(s) => Decimal::parse(s),
        CondValue::Bool(_) | CondValue::Positive | CondValue::Unknown => None,
    }
}

fn as_string(v: &CondValue) -> String {
    match v {
        CondValue::Str(s) => s.clone(),
        CondValue::Num(n) => n.to_string(),
        CondValue::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        CondValue::Dec(d) => d.text.clone(),
        CondValue::Positive | CondValue::Unknown => String::new(),
    }
}

// --- Functions ---------------------------------------------------------------

/// A character argument. A number is read as its digits, since an expanded
/// `{&name}` loses its quotes.
fn text(v: &CondValue) -> Option<String> {
    match v {
        CondValue::Str(s) => Some(s.clone()),
        CondValue::Num(n) => Some(n.to_string()),
        CondValue::Dec(d) => Some(d.text.clone()),
        CondValue::Bool(_) | CondValue::Positive | CondValue::Unknown => None,
    }
}

fn int(v: &CondValue) -> Option<i64> {
    match v {
        CondValue::Num(n) => Some(*n),
        CondValue::Str(s) => s.trim().parse().ok(),
        CondValue::Bool(_) | CondValue::Dec(_) | CondValue::Positive | CondValue::Unknown => None,
    }
}

/// Lowercase for a case-insensitive search. Non-ASCII text folds by codepage
/// rules this evaluator does not carry, so it declines.
fn fold(s: &str) -> Option<String> {
    s.is_ascii().then(|| s.to_ascii_lowercase())
}

/// The list delimiter: the argument if given (one character), else a comma.
fn delimiter(arg: Option<&CondValue>) -> Option<char> {
    let Some(arg) = arg else { return Some(',') };
    let s = text(arg)?;
    let mut chars = s.chars();
    let c = chars.next()?;
    chars.next().is_none().then_some(c)
}

/// Evaluate a function whose semantics are modelled exactly. `None` means the
/// function, or this use of it, is not modelled.
fn call(name: &str, args: &[CondValue]) -> Option<CondValue> {
    use std::cmp::Ordering::{self, Greater, Less};
    let one = |f: fn(&CondValue) -> Option<CondValue>| match args {
        [a] => f(a),
        _ => None,
    };
    match name {
        "LOGICAL" => one(|a| match a {
            CondValue::Bool(b) => Some(CondValue::Bool(*b)),
            CondValue::Num(n) => Some(CondValue::Bool(*n != 0)),
            CondValue::Positive => Some(CondValue::Bool(true)),
            CondValue::Dec(_) | CondValue::Unknown => None,
            CondValue::Str(s) => match s.trim().to_ascii_lowercase().as_str() {
                "yes" | "true" => Some(CondValue::Bool(true)),
                "no" | "false" => Some(CondValue::Bool(false)),
                _ => None,
            },
        }),
        "INTEGER" | "INT" | "DECIMAL" | "DEC" => one(|a| match a {
            CondValue::Bool(b) => Some(CondValue::Num(i64::from(*b))),
            other => int(other).map(CondValue::Num),
        }),
        "STRING" => one(|a| match a {
            CondValue::Bool(b) => Some(CondValue::Str(if *b { "yes" } else { "no" }.into())),
            other => text(other).map(CondValue::Str),
        }),
        "LENGTH" => one(|a| Some(CondValue::Num(text(a)?.chars().count() as i64))),
        "TRIM" => one(|a| Some(CondValue::Str(trim_blanks(&text(a)?, true, true)))),
        "LEFT-TRIM" => one(|a| Some(CondValue::Str(trim_blanks(&text(a)?, true, false)))),
        "RIGHT-TRIM" => one(|a| Some(CondValue::Str(trim_blanks(&text(a)?, false, true)))),
        "LC" => one(|a| Some(CondValue::Str(fold(&text(a)?)?))),
        "ABSOLUTE" => one(|a| int(a)?.checked_abs().map(CondValue::Num)),
        // All numbers or all strings; a string pick compares as text.
        "MAXIMUM" | "MINIMUM" if args.len() >= 2 => {
            let beats = |order: Ordering| order == if name == "MAXIMUM" { Greater } else { Less };
            let (first, rest) = args.split_first()?;
            match first {
                CondValue::Num(first) => {
                    let mut pick = first;
                    for arg in rest {
                        let CondValue::Num(n) = arg else { return None };
                        if beats(n.cmp(pick)) {
                            pick = n;
                        }
                    }
                    Some(CondValue::Num(*pick))
                }
                CondValue::Str(first) => {
                    let mut pick = first;
                    for arg in rest {
                        let CondValue::Str(s) = arg else { return None };
                        if beats(s.to_ascii_lowercase().cmp(&pick.to_ascii_lowercase())) {
                            pick = s;
                        }
                    }
                    Some(CondValue::Str(pick.clone()))
                }
                _ => None,
            }
        }
        "SUBSTRING" | "SUBSTR" => {
            let (s, start, len) = match args {
                [s, start] => (text(s)?, int(start)?, None),
                [s, start, len] => (text(s)?, int(start)?, Some(int(len)?)),
                _ => return None,
            };
            if start < 1 || len.is_some_and(|l| l < 0) {
                return None;
            }
            let rest = s.chars().skip(start as usize - 1);
            Some(CondValue::Str(match len {
                Some(l) => rest.take(l as usize).collect(),
                None => rest.collect(),
            }))
        }
        "INDEX" | "R-INDEX" => {
            let [s, target] = args else { return None };
            let (s, target) = (fold(&text(s)?)?, fold(&text(target)?)?);
            if target.is_empty() {
                return None;
            }
            let at = if name == "INDEX" {
                s.find(&target)
            } else {
                s.rfind(&target)
            };
            Some(CondValue::Num(
                at.map_or(0, |b| s[..b].chars().count() as i64 + 1),
            ))
        }
        "NUM-ENTRIES" => {
            let list = text(args.first()?)?;
            let delim = delimiter(args.get(1))?;
            if args.len() > 2 {
                return None;
            }
            let n = if list.is_empty() {
                0
            } else {
                list.split(delim).count()
            };
            Some(CondValue::Num(n as i64))
        }
        "ENTRY" => {
            let n = int(args.first()?)?;
            let list = text(args.get(1)?)?;
            let delim = delimiter(args.get(2))?;
            if args.len() > 3 || n < 1 {
                return None;
            }
            let entry = list.split(delim).nth(n as usize - 1)?;
            // An empty list has no entries at all.
            (!list.is_empty()).then(|| CondValue::Str(entry.to_string()))
        }
        "LOOKUP" => {
            let item = fold(&text(args.first()?)?)?;
            let list = fold(&text(args.get(1)?)?)?;
            let delim = delimiter(args.get(2))?;
            if args.len() > 3 {
                return None;
            }
            let at = if list.is_empty() {
                None
            } else {
                list.split(delim).position(|entry| entry == item)
            };
            Some(CondValue::Num(at.map_or(0, |p| p as i64 + 1)))
        }
        _ => None,
    }
}

/// `TRIM` and friends strip blank, tab, line feed and carriage return.
fn trim_blanks(s: &str, left: bool, right: bool) -> String {
    let blank = |c: char| matches!(c, ' ' | '\t' | '\n' | '\r');
    let s = if left { s.trim_start_matches(blank) } else { s };
    let s = if right { s.trim_end_matches(blank) } else { s };
    s.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_in(condition: &str, vars: &PreprocVarTable, proversion: Option<&str>) -> Evaluation {
        super::evaluate(
            condition,
            &Environment {
                vars,
                opsys: "UNIX",
                proversion,
            },
        )
    }

    fn evaluate_with_defined(condition: &str, vars: &PreprocVarTable) -> bool {
        evaluate_exact(condition, vars)
    }

    fn eval(condition: &str) -> Evaluation {
        eval_in(condition, &PreprocVarTable::new(), Some("12.8"))
    }

    /// The branch, asserting that no part of the condition was a guess.
    fn evaluate_exact(condition: &str, vars: &PreprocVarTable) -> bool {
        let e = eval_in(condition, vars, Some("12.8"));
        assert!(e.gaps.is_empty(), "{condition}: {:?}", e.gaps);
        e.value
    }

    fn vars_with(pairs: &[(&str, &str)]) -> PreprocVarTable {
        let mut table = PreprocVarTable::new();
        for (k, v) in pairs {
            table.define(k, v);
        }
        table
    }

    #[test]
    fn true_literal() {
        assert!(evaluate_exact("TRUE", &PreprocVarTable::new()));
    }

    #[test]
    fn false_literal() {
        assert!(!evaluate_exact("FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn string_equality() {
        let vars = vars_with(&[("foo", "bar")]);
        assert!(evaluate_exact(r#""{&foo}" = "bar""#, &vars));
        assert!(!evaluate_exact(r#""{&foo}" = "baz""#, &vars));
    }

    #[test]
    fn string_inequality() {
        let vars = vars_with(&[("foo", "bar")]);
        assert!(evaluate_exact(r#""{&foo}" <> "baz""#, &vars));
        assert!(!evaluate_exact(r#""{&foo}" <> "bar""#, &vars));
    }

    #[test]
    fn string_case_insensitive() {
        let vars = vars_with(&[("mode", "DEBUG")]);
        assert!(evaluate_exact(r#""{&mode}" = "debug""#, &vars));
    }

    #[test]
    fn numeric_comparison() {
        let vars = vars_with(&[("ver", "10")]);
        assert!(evaluate_exact("{&ver} > 5", &vars));
        assert!(!evaluate_exact("{&ver} < 5", &vars));
        assert!(evaluate_exact("{&ver} >= 10", &vars));
        assert!(evaluate_exact("{&ver} <= 10", &vars));
        assert!(evaluate_exact("{&ver} = 10", &vars));
    }

    #[test]
    fn not_operator() {
        assert!(evaluate_exact("NOT FALSE", &PreprocVarTable::new()));
        assert!(!evaluate_exact("NOT TRUE", &PreprocVarTable::new()));
    }

    #[test]
    fn and_operator() {
        assert!(evaluate_exact("TRUE AND TRUE", &PreprocVarTable::new()));
        assert!(!evaluate_exact("TRUE AND FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn or_operator() {
        assert!(evaluate_exact("FALSE OR TRUE", &PreprocVarTable::new()));
        assert!(!evaluate_exact("FALSE OR FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn combined_logic() {
        assert!(evaluate_exact(
            "TRUE AND (FALSE OR TRUE)",
            &PreprocVarTable::new()
        ));
        assert!(!evaluate_exact(
            "NOT (TRUE OR FALSE)",
            &PreprocVarTable::new()
        ));
    }

    #[test]
    fn defined_true() {
        let vars = vars_with(&[("MY_FLAG", "1")]);
        assert!(evaluate_with_defined("DEFINED(MY_FLAG)", &vars));
    }

    #[test]
    fn defined_false() {
        assert!(!evaluate_with_defined(
            "DEFINED(MY_FLAG)",
            &PreprocVarTable::new()
        ));
    }

    #[test]
    fn defined_with_and() {
        let vars = vars_with(&[("A", "1")]);
        assert!(evaluate_with_defined("DEFINED(A) AND TRUE", &vars));
        assert!(!evaluate_with_defined("DEFINED(A) AND DEFINED(B)", &vars));
    }

    #[test]
    fn undefined_variable_expands_to_empty() {
        // {&missing} expands to "" which equals ""
        assert!(evaluate_exact(
            r#""{&missing}" = """#,
            &PreprocVarTable::new()
        ));
    }

    #[test]
    fn expand_refs_basic() {
        let vars = vars_with(&[("x", "hello")]);
        assert_eq!(expand_refs("{&x}", &vars), "hello");
        assert_eq!(expand_refs("a{&x}b", &vars), "ahellob");
    }

    #[test]
    fn expand_refs_missing() {
        assert_eq!(expand_refs("{&missing}", &PreprocVarTable::new()), "");
    }

    #[test]
    fn tokenize_basic() {
        let tokens = tokenize(r#"DEFINED(foo) AND "bar" = "baz""#);
        // DEFINED(foo) AND "bar" = "baz" → 5 tokens
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], CondToken::Defined("foo".into()));
        assert_eq!(tokens[1], CondToken::And);
    }

    #[test]
    fn matches_operator() {
        let vars = vars_with(&[("mode", "Def input param")]);
        assert!(evaluate_exact(
            r#""{&mode}" matches "*Def* *inp* *param*""#,
            &vars
        ));
        assert!(!evaluate_exact(r#""{&mode}" matches "*xyz*""#, &vars));
    }

    #[test]
    fn matches_wildcard_patterns() {
        let empty = PreprocVarTable::new();
        assert!(evaluate_exact(r#""hello" matches "h*""#, &empty));
        assert!(evaluate_exact(r#""hello" matches "*llo""#, &empty));
        assert!(evaluate_exact(r#""hello" matches "*ell*""#, &empty));
        assert!(evaluate_exact(r#""hello" matches "h.llo""#, &empty));
        assert!(!evaluate_exact(r#""hello" matches "h.lo""#, &empty));
    }

    #[test]
    fn begins_operator() {
        let empty = PreprocVarTable::new();
        assert!(evaluate_exact(r#""hello world" begins "hello""#, &empty));
        assert!(!evaluate_exact(r#""hello world" begins "world""#, &empty));
    }

    #[test]
    fn keyword_comparison_operators() {
        let vars = vars_with(&[("ver", "10")]);
        assert!(evaluate_exact("{&ver} GT 5", &vars));
        assert!(evaluate_exact("{&ver} GE 10", &vars));
        assert!(!evaluate_exact("{&ver} LT 5", &vars));
        assert!(evaluate_exact("{&ver} LE 10", &vars));
        assert!(evaluate_exact("{&ver} EQ 10", &vars));
        assert!(evaluate_exact("{&ver} NE 5", &vars));
    }

    // --- String attributes (`:U`, etc.) — #65 ADM2 conditions ---

    #[test]
    fn string_attribute_u_empty_eq() {
        // Real fnarg condition shape after empty `{3}` expansion.
        assert!(evaluate_exact(r#""":U = "":U"#, &PreprocVarTable::new()));
    }

    #[test]
    fn string_attribute_u_equal() {
        assert!(evaluate_exact(r#""x":U = "x":U"#, &PreprocVarTable::new()));
    }

    #[test]
    fn string_attribute_u_unequal() {
        assert!(!evaluate_exact(r#""x":U = "y":U"#, &PreprocVarTable::new()));
    }

    #[test]
    fn string_attribute_does_not_break_plain_string_eq() {
        assert!(evaluate_exact(r#""a" = "a""#, &PreprocVarTable::new()));
        assert!(!evaluate_exact(r#""a" = "b""#, &PreprocVarTable::new()));
    }

    #[test]
    fn string_attribute_l_ignored() {
        assert!(evaluate_exact(
            r#""hello":L = "hello":L"#,
            &PreprocVarTable::new()
        ));
    }

    // --- Functions and gaps ---

    fn exact(condition: &str) -> bool {
        evaluate_exact(condition, &PreprocVarTable::new())
    }

    fn gaps(condition: &str) -> Vec<Gap> {
        eval(condition).gaps
    }

    #[test]
    fn opsys_and_proversion_come_from_the_environment() {
        let vars = PreprocVarTable::new();
        let win = |c: &str| {
            let env = Environment {
                vars: &vars,
                opsys: "WIN32",
                proversion: Some("12.8"),
            };
            let e = super::evaluate(c, &env);
            assert!(e.gaps.is_empty(), "{c}: {:?}", e.gaps);
            e.value
        };
        assert!(win(r#"OPSYS = "WIN32""#));
        assert!(!win(r#"OPSYS = "UNIX""#));
        assert!(win(r#"OPSYS = "win32" AND PROVERSION >= "12""#));
        assert!(!win(r#"PROVERSION >= "13""#));
    }

    #[test]
    fn an_unset_proversion_is_a_gap() {
        let e = eval_in(r#"PROVERSION >= "12""#, &PreprocVarTable::new(), None);
        assert_eq!(e.gaps, [Gap::UnsetProversion]);
    }

    #[test]
    fn a_define_does_not_shadow_the_opsys_function() {
        let vars = vars_with(&[("OPSYS", "WIN32")]);
        assert!(evaluate_exact(r#"OPSYS = "UNIX""#, &vars));
    }

    #[test]
    fn logical_of_defined_follows_the_table() {
        let vars = vars_with(&[("page-flow", "yes")]);
        assert!(evaluate_exact("LOGICAL(DEFINED(page-flow))", &vars));
        assert!(!evaluate_exact("NOT LOGICAL(DEFINED(page-flow))", &vars));
        assert!(!exact("LOGICAL(DEFINED(page-flow))"));
        assert!(exact("NOT LOGICAL(DEFINED(page-flow))"));
    }

    #[test]
    fn defined_tells_a_define_from_an_include_argument() {
        let mut vars = PreprocVarTable::new();
        vars.define("s", "x");
        vars.define_global("g", "x");
        vars.define_argument("a", "x");
        assert!(evaluate_exact("DEFINED(g) = 1", &vars));
        assert!(evaluate_exact("DEFINED(a) = 2", &vars));
        assert!(evaluate_exact("DEFINED(s) = 3", &vars));
        assert!(!evaluate_exact("DEFINED(s) = 1", &vars));
        assert!(!evaluate_exact("DEFINED(g) = 3", &vars));
        assert!(evaluate_exact("DEFINED(s) >= 2 AND DEFINED(g) < 2", &vars));
        assert!(evaluate_exact("DEFINED(a) <> 1 AND DEFINED(a) > 0", &vars));
        assert!(evaluate_exact("DEFINED(nothing) = 0", &vars));
        // More than one at once: only "above zero" is certain.
        vars.define("a", "y");
        assert!(evaluate_exact("DEFINED(a) > 0 AND DEFINED(a) <> 0", &vars));
        assert!(evaluate_exact("0 < DEFINED(a)", &vars));
        assert!(!evaluate_exact("DEFINED(a) = 0", &vars));
        assert!(evaluate_exact("LOGICAL(DEFINED(a))", &vars));
        assert_eq!(eval_in("DEFINED(a) = 2", &vars, None).gaps.len(), 1);
    }

    #[test]
    fn comments_inside_a_condition_are_ignored() {
        assert!(exact("1 = 1 /* a note: 20000 means yes */"));
        assert!(exact("/* a /* nested */ still */ 2 > 1"));
        assert!(exact(r#""a/*b" = "a/*b" /* c */"#));
    }

    #[test]
    fn a_line_comment_ends_at_the_line() {
        assert!(exact("1 = 1 // Allow short forms\n AND 2 > 1"));
    }

    #[test]
    fn decimals_compare_exactly() {
        assert!(exact("12.8 >= 12"));
        assert!(exact("12.10 < 12.8"));
        assert!(exact("12.80 = 12.8"));
        assert!(exact("-1.5 < 0"));
    }

    #[test]
    fn two_strings_compare_as_text_however_they_look() {
        assert!(exact(r#""9" >= "10""#));
        assert!(!exact(r#""9" < "10""#));
        assert!(exact(r#""12.8" > "12.10""#));
        assert!(exact(r#"ENTRY(1, "9.1", ".") >= "10""#));
        assert!(exact(r#""007" <> "7""#));
        assert!(exact(r#"STRING(9) > STRING(10)"#));
        let vars = vars_with(&[("a", "9"), ("b", "10")]);
        assert!(evaluate_exact(r#""{&a}" > "{&b}""#, &vars));
        // Two numbers still compare as numbers.
        assert!(exact("9 < 10"));
        assert!(!exact("9 >= 10"));
        assert!(exact("12.8 > 12.10"));
        assert!(exact("INTEGER(ENTRY(1, \"9.1\", \".\")) < 10"));
    }

    #[test]
    fn a_string_and_a_number_are_not_comparable() {
        for condition in [
            r#"12.8 >= "12""#,
            r#""5" = 5"#,
            r#"LENGTH("ab") = "2""#,
            r#"PROVERSION >= 12"#,
            "TRUE = 1",
            r#"LOGICAL("yes") = "yes""#,
        ] {
            assert_eq!(gaps(condition).len(), 1, "{condition}");
        }
    }

    #[test]
    fn maximum_and_minimum_keep_the_value_type() {
        assert!(exact(r#"MAXIMUM("10", "9") = "9""#));
        assert!(exact(r#"MINIMUM("10", "9") = "10""#));
        assert!(exact(r#"MAXIMUM("a", "B", "c") = "c""#));
        assert!(exact("MAXIMUM(10, 9) = 10"));
        assert!(exact("MINIMUM(10, 9, 11) = 9"));
        assert_eq!(gaps(r#"MAXIMUM(1, "2") = 2"#).len(), 1);
        assert_eq!(gaps(r#"MINIMUM("1", 2) = 1"#).len(), 1);
    }

    #[test]
    fn logical_reads_yes_and_no_only() {
        assert!(exact(r#"LOGICAL("yes")"#));
        assert!(!exact(r#"LOGICAL("NO")"#));
        assert!(exact("LOGICAL(1)"));
        assert_eq!(gaps(r#"LOGICAL("maybe")"#).len(), 1);
    }

    #[test]
    fn string_functions() {
        assert!(exact(r#"LENGTH("abc") = 3"#));
        assert!(exact(r#"SUBSTRING("abcdef", 2, 3) = "bcd""#));
        assert!(exact(r#"SUBSTRING("abcdef", 4) = "def""#));
        assert!(exact(r#"SUBSTR("abc", 9) = """#));
        assert!(exact(r#"TRIM("  a b ") = "a b""#));
        assert!(exact(r#"LEFT-TRIM("  a ") = "a ""#));
        assert!(exact(r#"RIGHT-TRIM("  a ") = "  a""#));
        assert!(exact(r#"LC("AbC") = "abc""#));
        assert!(exact(r#"INDEX("Hello", "LL") = 3"#));
        assert!(exact(r#"INDEX("Hello", "z") = 0"#));
        assert!(exact(r#"R-INDEX("a-b-c", "-") = 4"#));
        assert!(exact(r#"STRING(12) = "12""#));
        assert!(exact(r#"STRING(TRUE) = "yes""#));
        assert!(exact(r#"INTEGER("42") = 42"#));
        assert!(exact(r#"INT(" 7 ") + 1 = 8"#));
    }

    #[test]
    fn list_functions() {
        assert!(exact(r#"NUM-ENTRIES("a,b,c") = 3"#));
        assert!(exact(r#"NUM-ENTRIES("") = 0"#));
        assert!(exact(r#"NUM-ENTRIES("a|b", "|") = 2"#));
        assert!(exact(r#"ENTRY(2, "a,b,c") = "b""#));
        assert!(exact(r#"ENTRY(1, "x;y", ";") = "x""#));
        assert!(exact(r#"LOOKUP("B", "a,b,c") = 2"#));
        assert!(exact(r#"LOOKUP("z", "a,b,c") = 0"#));
        assert_eq!(gaps(r#"ENTRY(4, "a,b,c") = "x""#).len(), 1);
        assert_eq!(gaps(r#"ENTRY(1, "a,b", ";,") = "x""#).len(), 1);
    }

    #[test]
    fn arithmetic() {
        assert!(exact("1 + 2 * 3 = 7"));
        assert!(exact("(1 + 2) * 3 = 9"));
        assert!(exact("7 MODULO 4 = 3"));
        assert!(exact("5 - 2 = 3"));
        assert!(exact("5 -2 = 3"));
        assert!(exact("-3 + 5 = 2"));
        assert!(exact(r#""a" + "b" = "ab""#));
        assert!(exact("MAXIMUM(1, 5, 3) = 5"));
        assert!(exact("MINIMUM(4, 2) = 2"));
        assert!(exact("ABSOLUTE(-4) = 4"));
        assert_eq!(gaps("4 / 2 = 2").len(), 1);
        assert_eq!(gaps(r#"1 + "a" = 1"#).len(), 1);
    }

    #[test]
    fn anything_unmodelled_is_a_gap_not_a_value() {
        for condition in [
            r#"SEARCH("a.i") <> ?"#,
            r#"KEYWORD("def") = "DEFINE""#,
            "SOMETHING = 1",
            r#"PROVERSION(1) = "12""#,
            "1 = 1 2",
            "12.5 + 3 > 3",
            r#"REPLACE("a", "a", "b") = "b""#,
            "",
            "(1 = 1",
            r#""a" = "a" ?"#,
            r#""yes-ish""#,
        ] {
            assert!(!gaps(condition).is_empty(), "{condition}");
        }
    }

    #[test]
    fn an_unmodelled_call_swallows_its_arguments() {
        let e = eval(r#"SEARCH("a,b.i") = ? OR TRUE"#);
        assert!(e.value);
        assert_eq!(e.gaps.len(), 2);
    }

    #[test]
    fn non_ascii_text_survives_expansion() {
        let vars = vars_with(&[("v", "café")]);
        assert!(evaluate_exact(r#""{&v}" = "café""#, &vars));
    }
}
