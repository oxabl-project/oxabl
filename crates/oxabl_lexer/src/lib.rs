use std::str::Chars;
extern crate string_cache;
mod oxabl_atom {
    include!(concat!(env!("OUT_DIR"), "/oxabl_atom.rs"));
}
use crate::oxabl_atom::OxablAtom;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// Token Type
    pub kind: Kind,

    /// Start offset in source
    pub start: usize,

    /// End offset in source
    pub end: usize,

    pub value: TokenValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    None,
    Number(f64),
    String(OxablAtom),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Eof, // End of file

    // operators
    Plus, // +
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    AddAndAssign,
    SubtractAndAssign,
    MultiplyAndAssign,
    DivideAndAssign,

    // literals
    Integer,
    Decimal,
    String,

    // keywords
    If,
    While,
    For,
    Do,

    Identifier,
    Unknown,
}

// A "+" sign gives us:
//  [
//      Token { kind: Kind::Plus, start: 0, end: 1 },
//      Token { kind: Kind::Eof, start: 1, end: 1 }
//  ]

struct Lexer<'a> {
    /// Source Text
    source: &'a str,

    /// The remaining characters
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
        }
    }

    fn read_next_kind(&mut self) -> Kind {
        while let Some(c) = self.chars.next() {
            match c {
                // whitspace, skip
                ' ' | '\t' | '\n' => continue,

                // if it starts with an operator it must be one
                '+' => {
                    return Kind::Plus;
                }
                '-' => {
                    return Kind::Subtract;
                }
                '*' => {
                    return Kind::Multiply;
                }
                '/' => {
                    return Kind::Divide;
                }
                '%' => {
                    return Kind::Modulo;
                }

                // If it starts with a quote, must be a string literal
                '"' | '\'' => return Kind::String, // TODO - need to actually read the string

                // If it starts with a digit, must be a number literal
                '0'..='9' => {
                    return Kind::Integer; // TODO - need to read the Integer or Decimal
                }

                // If it starts with a letter or underscoe, could be keyword or identifier
                'a'..'z' | 'A'..'Z' | '_' => {
                    self.read_identifier_or_keyword();
                }

                _ => {
                    return Kind::Unknown;
                }
            }
        }
        Kind::Eof
    }

    fn read_next_token(&mut self) -> Token {
        let start = self.offset();
        let kind = self.read_next_kind();
        let end = self.offset();
        let value = TokenValue::String(OxablAtom::from(self.source[start..end].to_string()));
        Token {
            kind,
            start,
            end,
            value,
        }
    }

    /// Get the length offset from the source text, in UTF-8 bytes
    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    /// Doesn't advance the original chars to peek at the next char for multi-character symbols
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Returns a reserved keyword or Kind::Identifier if it's not a keyword
    fn resolve_keyword(&self, ident: &str) -> Kind {
        match ident {
            "if" => Kind::If,
            "while" => Kind::While,
            "for" => Kind::For,
            "do" => Kind::Do,
            _ => Kind::Identifier,
        }
    }

    /// Reads the word that will resolve to either a keyword or identifer
    fn read_identifier_or_keyword(&mut self) -> Kind {
        // Keep consuming alphanumeric chars
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
            self.chars.next();
        }

        // NOW we have the full word - check if it's a keyword
        let text = "";
        self.resolve_keyword(text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identify_plus() {
        let source = "+";
        let mut lexer = Lexer::new(source);
        let token = lexer.read_next_token();
        assert_eq!(
            token,
            Token {
                kind: Kind::Plus,
                start: 0,
                end: 1,
                value: TokenValue::String(atom!("+"))
            },
            "We are ensuring a source of '+' creates the expected token *plus*. Token created: {:?}",
            token
        );
    }
}
