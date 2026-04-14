use crate::PreprocVarTable;

/// Evaluate a preprocessor `&IF` condition against the current variable table.
///
/// ABL preprocessor conditions support:
/// - `DEFINED(name)` — true if the variable is defined
/// - `"{&name}" = "value"` / `"{&name}" <> "value"` — string comparison
/// - `{&name} = number` / `{&name} <> number` — numeric comparison
/// - `{&name} > number`, `< number`, `>= number`, `<= number`
/// - `NOT expr`, `expr AND expr`, `expr OR expr`
/// - Parenthesized sub-expressions
/// - `TRUE` and `FALSE` literals
///
/// All comparisons are case-insensitive (matching ABL semantics).
pub fn evaluate(condition: &str, vars: &PreprocVarTable) -> bool {
    let expanded = expand_refs(condition, vars);
    let tokens = tokenize(&expanded);
    let mut parser = CondParser::new(&tokens);
    parser.parse_or()
}

/// Expand `{&name}` references in the condition string.
fn expand_refs(input: &str, vars: &PreprocVarTable) -> String {
    let mut result = String::with_capacity(input.len());
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'{' && bytes[i + 1] == b'&' {
            // Find the closing brace
            if let Some(close) = input[i..].find('}') {
                let name = &input[i + 2..i + close];
                if let Some(val) = vars.get(name) {
                    result.push_str(val);
                }
                // If not defined, expand to empty string (ABL behaviour)
                i += close + 1;
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }

    result
}

#[derive(Debug, Clone, PartialEq)]
enum CondToken {
    Defined,
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
    LParen,
    RParen,
    StringLit(String),
    Number(i64),
    Ident(String),
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
                }
            }
            b'0'..=b'9' => {
                let start = i;
                let negative = false;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
                let s = &input[start..i];
                let val = if negative {
                    -(s.parse::<i64>().unwrap_or(0))
                } else {
                    s.parse::<i64>().unwrap_or(0)
                };
                tokens.push(CondToken::Number(val));
            }
            b'-' if i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() => {
                i += 1;
                let start = i;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
                let val = input[start..i].parse::<i64>().unwrap_or(0);
                tokens.push(CondToken::Number(-val));
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
                    "DEFINED" => tokens.push(CondToken::Defined),
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
                    _ => tokens.push(CondToken::Ident(word.to_string())),
                }
            }
            _ => i += 1, // skip unknown chars
        }
    }

    tokens
}

/// Recursive-descent parser for preprocessor conditions.
///
/// Precedence (lowest to highest): OR, AND, NOT, comparison, primary.
struct CondParser<'a> {
    tokens: &'a [CondToken],
    pos: usize,
}

impl<'a> CondParser<'a> {
    fn new(tokens: &'a [CondToken]) -> Self {
        CondParser { tokens, pos: 0 }
    }

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

    fn expect(&mut self, expected: &CondToken) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// OR has lowest precedence.
    fn parse_or(&mut self) -> bool {
        let mut result = self.parse_and();
        while self.peek() == Some(&CondToken::Or) {
            self.advance();
            let rhs = self.parse_and();
            result = result || rhs;
        }
        result
    }

    fn parse_and(&mut self) -> bool {
        let mut result = self.parse_not();
        while self.peek() == Some(&CondToken::And) {
            self.advance();
            let rhs = self.parse_not();
            result = result && rhs;
        }
        result
    }

    fn parse_not(&mut self) -> bool {
        if self.peek() == Some(&CondToken::Not) {
            self.advance();
            return !self.parse_not();
        }
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> bool {
        let lhs = self.parse_primary_value();

        match self.peek() {
            Some(CondToken::Eq)
            | Some(CondToken::Neq)
            | Some(CondToken::Lt)
            | Some(CondToken::Gt)
            | Some(CondToken::Le)
            | Some(CondToken::Ge)
            | Some(CondToken::Matches)
            | Some(CondToken::Begins) => {
                let op = self.advance().unwrap().clone();
                let rhs = self.parse_primary_value();
                compare_values(&lhs, &op, &rhs)
            }
            _ => value_to_bool(&lhs),
        }
    }

    fn parse_primary_value(&mut self) -> CondValue {
        match self.peek() {
            Some(CondToken::Defined) => {
                self.advance();
                self.expect(&CondToken::LParen);
                // Consume the name (ident or string)
                match self.peek() {
                    Some(CondToken::Ident(_)) | Some(CondToken::StringLit(_)) => {
                        self.advance();
                    }
                    _ => {}
                }
                self.expect(&CondToken::RParen);
                // DEFINED() is normally replaced by replace_defined() before
                // evaluate() is called. If it reaches here, treat as false.
                CondValue::Defined
            }
            Some(CondToken::LParen) => {
                self.advance();
                let result = self.parse_or();
                self.expect(&CondToken::RParen);
                CondValue::Bool(result)
            }
            Some(CondToken::StringLit(_)) => {
                if let Some(CondToken::StringLit(s)) = self.advance().cloned() {
                    CondValue::Str(s)
                } else {
                    CondValue::Str(String::new())
                }
            }
            Some(CondToken::Number(_)) => {
                if let Some(CondToken::Number(n)) = self.advance().cloned() {
                    CondValue::Num(n)
                } else {
                    CondValue::Num(0)
                }
            }
            Some(CondToken::True) => {
                self.advance();
                CondValue::Bool(true)
            }
            Some(CondToken::False) => {
                self.advance();
                CondValue::Bool(false)
            }
            Some(CondToken::Not) => {
                // NOT in a value position — treat as boolean
                CondValue::Bool(self.parse_not())
            }
            Some(CondToken::Ident(_)) => {
                if let Some(CondToken::Ident(s)) = self.advance().cloned() {
                    // Bare identifier — could be an unexpanded variable or a
                    // numeric value. Try to parse as number first.
                    if let Ok(n) = s.parse::<i64>() {
                        CondValue::Num(n)
                    } else {
                        CondValue::Str(s)
                    }
                } else {
                    CondValue::Str(String::new())
                }
            }
            _ => {
                self.advance(); // consume unknown token
                CondValue::Str(String::new())
            }
        }
    }
}

#[derive(Debug, Clone)]
enum CondValue {
    Bool(bool),
    Str(String),
    Num(i64),
    Defined,
}

fn value_to_bool(v: &CondValue) -> bool {
    match v {
        CondValue::Bool(b) => *b,
        CondValue::Str(s) => !s.is_empty(),
        CondValue::Num(n) => *n != 0,
        // DEFINED as a standalone expression — we can't evaluate it here
        // because we've already expanded refs. The preprocessor evaluates
        // DEFINED() before calling evaluate() by injecting TRUE/FALSE.
        CondValue::Defined => false,
    }
}

fn compare_values(lhs: &CondValue, op: &CondToken, rhs: &CondValue) -> bool {
    // Try numeric comparison first
    if let (Some(l), Some(r)) = (as_number(lhs), as_number(rhs)) {
        return match op {
            CondToken::Eq => l == r,
            CondToken::Neq => l != r,
            CondToken::Lt => l < r,
            CondToken::Gt => l > r,
            CondToken::Le => l <= r,
            CondToken::Ge => l >= r,
            _ => false,
        };
    }

    // Fall back to case-insensitive string comparison
    let l = as_string(lhs).to_ascii_lowercase();
    let r = as_string(rhs).to_ascii_lowercase();
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
        CondValue::Bool(b) => Some(if *b { 1 } else { 0 }),
        CondValue::Defined => None,
    }
}

fn as_string(v: &CondValue) -> String {
    match v {
        CondValue::Str(s) => s.clone(),
        CondValue::Num(n) => n.to_string(),
        CondValue::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        CondValue::Defined => String::new(),
    }
}

/// Evaluate a condition with `DEFINED()` support.
///
/// This is the main entry point used by the preprocessor. It handles
/// `DEFINED(name)` by checking the variable table before evaluating
/// the rest of the condition.
pub fn evaluate_with_defined(condition: &str, vars: &PreprocVarTable) -> bool {
    // First pass: replace DEFINED(name) with TRUE/FALSE
    let processed = replace_defined(condition, vars);
    evaluate(&processed, vars)
}

/// Replace `DEFINED(name)` occurrences with TRUE or FALSE.
fn replace_defined(input: &str, vars: &PreprocVarTable) -> String {
    let mut result = String::with_capacity(input.len());
    let upper = input.to_ascii_uppercase();
    let bytes = upper.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if i + 7 < bytes.len() && &bytes[i..i + 7] == b"DEFINED" {
            // Look for DEFINED(name)
            let after_defined = i + 7;
            // Skip whitespace
            let mut j = after_defined;
            while j < bytes.len() && bytes[j] == b' ' {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'(' {
                j += 1;
                // Skip whitespace
                while j < bytes.len() && bytes[j] == b' ' {
                    j += 1;
                }
                let name_start = j;
                while j < bytes.len() && bytes[j] != b')' && bytes[j] != b' ' {
                    j += 1;
                }
                let name = input[name_start..j].trim();
                // Skip to closing paren
                while j < bytes.len() && bytes[j] != b')' {
                    j += 1;
                }
                if j < bytes.len() {
                    j += 1; // skip ')'
                }
                if vars.is_defined(name) {
                    result.push_str("TRUE");
                } else {
                    result.push_str("FALSE");
                }
                i = j;
                continue;
            }
        }
        result.push(input.as_bytes()[i] as char);
        i += 1;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vars_with(pairs: &[(&str, &str)]) -> PreprocVarTable {
        let mut table = PreprocVarTable::new();
        for (k, v) in pairs {
            table.define(k, v);
        }
        table
    }

    #[test]
    fn true_literal() {
        assert!(evaluate("TRUE", &PreprocVarTable::new()));
    }

    #[test]
    fn false_literal() {
        assert!(!evaluate("FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn string_equality() {
        let vars = vars_with(&[("foo", "bar")]);
        assert!(evaluate(r#""{&foo}" = "bar""#, &vars));
        assert!(!evaluate(r#""{&foo}" = "baz""#, &vars));
    }

    #[test]
    fn string_inequality() {
        let vars = vars_with(&[("foo", "bar")]);
        assert!(evaluate(r#""{&foo}" <> "baz""#, &vars));
        assert!(!evaluate(r#""{&foo}" <> "bar""#, &vars));
    }

    #[test]
    fn string_case_insensitive() {
        let vars = vars_with(&[("mode", "DEBUG")]);
        assert!(evaluate(r#""{&mode}" = "debug""#, &vars));
    }

    #[test]
    fn numeric_comparison() {
        let vars = vars_with(&[("ver", "10")]);
        assert!(evaluate("{&ver} > 5", &vars));
        assert!(!evaluate("{&ver} < 5", &vars));
        assert!(evaluate("{&ver} >= 10", &vars));
        assert!(evaluate("{&ver} <= 10", &vars));
        assert!(evaluate("{&ver} = 10", &vars));
    }

    #[test]
    fn not_operator() {
        assert!(evaluate("NOT FALSE", &PreprocVarTable::new()));
        assert!(!evaluate("NOT TRUE", &PreprocVarTable::new()));
    }

    #[test]
    fn and_operator() {
        assert!(evaluate("TRUE AND TRUE", &PreprocVarTable::new()));
        assert!(!evaluate("TRUE AND FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn or_operator() {
        assert!(evaluate("FALSE OR TRUE", &PreprocVarTable::new()));
        assert!(!evaluate("FALSE OR FALSE", &PreprocVarTable::new()));
    }

    #[test]
    fn combined_logic() {
        assert!(evaluate(
            "TRUE AND (FALSE OR TRUE)",
            &PreprocVarTable::new()
        ));
        assert!(!evaluate("NOT (TRUE OR FALSE)", &PreprocVarTable::new()));
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
        assert!(evaluate(r#""{&missing}" = """#, &PreprocVarTable::new()));
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
        // DEFINED ( foo ) AND "bar" = "baz" → 8 tokens
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0], CondToken::Defined);
        assert_eq!(tokens[1], CondToken::LParen);
    }

    #[test]
    fn matches_operator() {
        let vars = vars_with(&[("mode", "Def input param")]);
        assert!(evaluate(
            r#""{&mode}" matches "*Def* *inp* *param*""#,
            &vars
        ));
        assert!(!evaluate(
            r#""{&mode}" matches "*xyz*""#,
            &vars
        ));
    }

    #[test]
    fn matches_wildcard_patterns() {
        let empty = PreprocVarTable::new();
        assert!(evaluate(r#""hello" matches "h*""#, &empty));
        assert!(evaluate(r#""hello" matches "*llo""#, &empty));
        assert!(evaluate(r#""hello" matches "*ell*""#, &empty));
        assert!(evaluate(r#""hello" matches "h.llo""#, &empty));
        assert!(!evaluate(r#""hello" matches "h.lo""#, &empty));
    }

    #[test]
    fn begins_operator() {
        let empty = PreprocVarTable::new();
        assert!(evaluate(r#""hello world" begins "hello""#, &empty));
        assert!(!evaluate(r#""hello world" begins "world""#, &empty));
    }

    #[test]
    fn keyword_comparison_operators() {
        let vars = vars_with(&[("ver", "10")]);
        assert!(evaluate("{&ver} GT 5", &vars));
        assert!(evaluate("{&ver} GE 10", &vars));
        assert!(!evaluate("{&ver} LT 5", &vars));
        assert!(evaluate("{&ver} LE 10", &vars));
        assert!(evaluate("{&ver} EQ 10", &vars));
        assert!(evaluate("{&ver} NE 5", &vars));
    }
}
