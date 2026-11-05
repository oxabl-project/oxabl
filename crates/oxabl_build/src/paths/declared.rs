//! The name a class source declares.

use oxabl_lexer::{Kind, Token, tokenize};

const MODIFIERS: [&str; 3] = ["final", "abstract", "serializable"];

fn is_word(text: &str) -> bool {
    text.starts_with(|c: char| c.is_alphabetic() || c == '_')
}

/// The name in the first `CLASS`, `INTERFACE` or `ENUM` statement of `source`,
/// as written. `None` when the source has no such statement in its own text,
/// for example when an include supplies it.
pub fn declared_name(source: &str) -> Option<String> {
    let tokens: Vec<Token> = tokenize(source)
        .into_iter()
        .filter(|token| token.kind != Kind::Comment)
        .collect();
    let text = |token: &Token| &source[token.start..token.end];
    for (at, token) in tokens.iter().enumerate() {
        if !matches!(token.kind, Kind::Class | Kind::Interface | Kind::KwEnum) {
            continue;
        }
        let starts_statement = match at.checked_sub(1).map(|before| &tokens[before]) {
            None => true,
            Some(before) if before.kind == Kind::Period => {
                let inside_name = before.end == token.start
                    && at
                        .checked_sub(2)
                        .is_some_and(|word| tokens[word].end == before.start);
                !inside_name
            }
            Some(before) => MODIFIERS.contains(&text(before).to_ascii_lowercase().as_str()),
        };
        if !starts_statement {
            continue;
        }
        let first = tokens.get(at + 1).map(text).filter(|t| is_word(t))?;
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
        return Some(name);
    }
    None
}
