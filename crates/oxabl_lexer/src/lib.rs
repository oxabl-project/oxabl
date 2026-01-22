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
    Add, // +
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

    fn read_next_kind(&mut self, start: usize) -> Kind {
        while let Some(c) = self.chars.next() {
            match c {
                // whitspace, skip
                ' ' | '\t' | '\n' => continue,

                // if it starts with an operator it must be one
                '+' => match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        return Kind::AddAndAssign;
                    }
                    _ => {
                        return Kind::Add;
                    }
                },
                '-' => match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        return Kind::SubtractAndAssign;
                    }
                    _ => {
                        return Kind::Subtract;
                    }
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        return Kind::MultiplyAndAssign;
                    }
                    _ => {
                        return Kind::Multiply;
                    }
                },
                '/' => match self.peek() {
                    Some('=') => {
                        self.chars.next();
                        return Kind::DivideAndAssign;
                    }
                    _ => {
                        return Kind::Divide;
                    }
                },
                '%' => {
                    return Kind::Modulo;
                }
                '=' => {
                    return Kind::Equal;
                }
                '>' => {
                    return Kind::GreaterThan;
                }
                // TODO finish symbols

                // If it starts with a quote, must be a string literal
                '"' | '\'' => {
                    return self.read_string_literal(c);
                }
                // If it starts with a digit, must be a number literal
                '0'..='9' => {
                    return self.read_number_literal(); 
                }

                // If it starts with a letter or underscoe, could be keyword or identifier
                'a'..='z' | 'A'..='Z' | '_' => {
                    return self.read_identifier_or_keyword(start);
                }

                _ => {
                    return Kind::Unknown;
                }
            }
        }
        Kind::Eof
    }

    // Read and return the next token in the source
    fn read_next_token(&mut self) -> Token {
        let start = self.offset();
        let kind = self.read_next_kind(start);
        let end = self.offset();
        // TODO - parse the value correctly. Store numbers, strings, chars, correctly.
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
        // TODO - make case insensitive
        match ident {
            "if" => Kind::If,
            "while" => Kind::While,
            "for" => Kind::For,
            "do" => Kind::Do,
            _ => Kind::Identifier,
        }
    }

    // TODO - handle comments
    // TODO - include more keywords
    // TODO - more test coverage

    /// Reads the word that will resolve to either a keyword or identifer
    fn read_identifier_or_keyword(&mut self, start: usize) -> Kind {
        // Keep consuming alphanumeric chars
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
            self.chars.next();
        }

        // NOW we have the full word - check if it's a keyword
        let text = &self.source[start..self.offset()];
        self.resolve_keyword(text)
    }

    /// Reads the word till the end of whatever quotes we started with
    fn read_string_literal(&mut self, quote_type: char) -> Kind {
        loop {
            match self.peek() {
                Some(c) if c == quote_type => {
                    self.chars.next(); //consume closing quote
                    return Kind::String;
                }
                Some('~') => {
                    self.chars.next(); // consume tilde
                    self.chars.next(); // consume escaped char (whatever it is)
                }
                Some(_) => {
                    self.chars.next(); // consume regular char
                }
                None => {
                    // Unterminated String - error?
                    return Kind::Unknown;
                }
            }
        }
    }

    // Read the number until the end, ensuring we consume the full integer 
    // or decimal without accidentally consuming something like a method call
    fn read_number_literal(&mut self) -> Kind {
        // Consume all leading digits
        while matches!(self.peek(), Some('0'..='9')) {
            self.chars.next();
        }

        // Check for decimal point followed by digit
        if matches!(self.peek(), Some('.')) {
            // Need to peek TWO ahead - clone the iterator
            let mut lookahead = self.chars.clone();
            lookahead.next(); // skip the '.'
            if matches!(lookahead.next(), Some('0'..='9')) {
                // It's a decimal
                self.chars.next(); // consume the '.'
                while matches!(self.peek(), Some('0'..='9')) {
                    self.chars.next();
                }
                return Kind::Decimal;
            }
        }

        Kind::Integer
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
                kind: Kind::Add,
                start: 0,
                end: 1,
                value: TokenValue::String(atom!("+"))
            },
            "We are ensuring a source of '+' creates the expected token *plus*. Token created: {:?}",
            token
        );
    }
}
