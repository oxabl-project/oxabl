//! The name a class source declares.

use oxabl_lexer::{Kind, Token, tokenize};

const MODIFIERS: [&str; 3] = ["final", "abstract", "serializable"];

fn is_word(text: &str) -> bool {
    text.starts_with(|c: char| c.is_alphabetic() || c == '_')
}

/// What a class source says about its own name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declared {
    /// The name of the first statement, as written.
    Name(String),
    /// The statement or its name sits in a preprocessor condition: the name
    /// depends on which branch the compile takes.
    Conditional,
    /// The source has no such statement in its own text, for example when an
    /// include supplies it.
    Absent,
}

impl Declared {
    pub fn name(&self) -> Option<&str> {
        match self {
            Declared::Name(name) => Some(name),
            _ => None,
        }
    }
}

fn is_directive(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::PreprocIf
            | Kind::PreprocElseif
            | Kind::PreprocElse
            | Kind::PreprocEndif
            | Kind::PreprocThen
    )
}

/// The name in the first `CLASS`, `INTERFACE` or `ENUM` statement of `source`.
pub fn declared_name(source: &str) -> Declared {
    let tokens: Vec<Token> = tokenize(source)
        .into_iter()
        .filter(|token| token.kind != Kind::Comment)
        .collect();
    let text = |token: &Token| &source[token.start..token.end];
    let mut depth = 0usize;
    // Inside the line of an `&UNDEFINE` or `&MESSAGE`, which have no end token.
    let mut in_line_directive = false;
    for (at, token) in tokens.iter().enumerate() {
        let before = at.checked_sub(1).map(|before| &tokens[before]);
        let after_line_directive = in_line_directive
            && before.is_some_and(|before| source[before.end..token.start].contains('\n'));
        if after_line_directive {
            in_line_directive = false;
        }
        match token.kind {
            Kind::PreprocIf => depth += 1,
            Kind::PreprocEndif => depth = depth.saturating_sub(1),
            Kind::PreprocUndefine | Kind::PreprocMessage => in_line_directive = true,
            _ => {}
        }
        if !matches!(token.kind, Kind::Class | Kind::Interface | Kind::KwEnum) {
            continue;
        }
        let starts_statement = match before {
            None => true,
            Some(before) if before.kind == Kind::Period => {
                let inside_name = before.end == token.start
                    && at
                        .checked_sub(2)
                        .is_some_and(|word| tokens[word].end == before.start);
                !inside_name
            }
            Some(before) => {
                matches!(
                    before.kind,
                    Kind::PreprocThen | Kind::PreprocElse | Kind::PreprocEndif | Kind::PreprocEnd
                ) || after_line_directive
                    || MODIFIERS.contains(&text(before).to_ascii_lowercase().as_str())
            }
        };
        if !starts_statement {
            continue;
        }
        if depth > 0 {
            return Declared::Conditional;
        }
        let Some(first) = tokens.get(at + 1) else {
            return Declared::Absent;
        };
        if is_directive(first.kind) {
            return Declared::Conditional;
        }
        if !is_word(text(first)) {
            return Declared::Absent;
        }
        let first = text(first);
        let mut name = first.to_string();
        let mut next = at + 2;
        while let (Some(dot), Some(word)) = (tokens.get(next), tokens.get(next + 1)) {
            let joined = dot.kind == Kind::Period
                && dot.start == tokens[next - 1].end
                && word.start == dot.end
                && is_word(text(word));
            if !joined {
                break;
            }
            name.push('.');
            name.push_str(text(word));
            next += 2;
        }
        let cut_by_directive =
            tokens
                .get(next)
                .zip(tokens.get(next + 1))
                .is_some_and(|(dot, word)| {
                    dot.kind == Kind::Period
                        && dot.start == tokens[next - 1].end
                        && is_directive(word.kind)
                });
        return if cut_by_directive {
            Declared::Conditional
        } else {
            Declared::Name(name)
        };
    }
    Declared::Absent
}
