/// OxAbl Lexer
/// A Lexer written in Rust for Progress ABL
// TODO - include more keywords
// TODO - more test coverage
use std::str::Chars;
extern crate string_cache;
mod oxabl_atom {
    include!(concat!(env!("OUT_DIR"), "/oxabl_atom.rs"));
}
use rust_decimal::Decimal;

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
    Integer(i32),
    BigInt(i64),
    Decimal(Decimal),
    String(OxablAtom),
    Boolean(bool),
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
    Not,
    And,
    Or,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    AddAndAssign,
    SubtractAndAssign,
    MultiplyAndAssign,
    DivideAndAssign,
    Colon,
    DoubleColon,

    // literals
    Integer,
    BigInt,
    Decimal,
    String,
    True,
    False,
    UnknownValue, // as in '?' the literal null/unknown

    // keywords
    If,
    While,
    For,
    Do,
    Define,
    Variable,

    // Punctuation
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    StatementEnd, // '.' periods

    Comment,
    Identifier,
    Invalid, // Meaning we could not resolve it
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

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(' ' | '\t' | '\n')) {
            self.advance();
        }
    }

    fn read_next_kind(&mut self, start: usize) -> Kind {
        while let Some(c) = self.advance() {
            match c {
                // whitspace, skip (we skip earlier, this is a sanity check)
                ' ' | '\t' | '\n' => continue,

                // if it starts with an operator it must be one
                '+' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::AddAndAssign;
                    }
                    _ => {
                        return Kind::Add;
                    }
                },
                '-' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::SubtractAndAssign;
                    }
                    _ => {
                        return Kind::Subtract;
                    }
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::MultiplyAndAssign;
                    }
                    _ => {
                        return Kind::Multiply;
                    }
                },
                // could be divide, could be a comment
                '/' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::DivideAndAssign;
                    }
                    Some('/') => {
                        self.advance();
                        return self.skip_line_comment();
                    }
                    Some('*') => {
                        self.advance();
                        return self.skip_block_comment();
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
                '>' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::GreaterThanOrEqual;
                    }
                    _ => {
                        return Kind::GreaterThan;
                    }
                },
                '<' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::LessThanOrEqual;
                    }
                    Some('>') => {
                        self.advance();
                        return Kind::NotEqual;
                    }
                    _ => {
                        return Kind::LessThan;
                    }
                },
                '?' => {
                    return Kind::UnknownValue;
                }

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

                // If it doesn't start with a decimal or letter,
                // and it's a '.', must be a terminator
                '.' => {
                    return Kind::StatementEnd;
                }

                // Colons are always on their own, can't be used in identifiers or keywords
                ':' => match self.peek() {
                    Some(':') => {
                        self.advance();
                        return Kind::DoubleColon;
                    }
                    _ => {
                        return Kind::Colon;
                    }
                },

                // Punctuation
                '(' => {
                    return Kind::LeftParen;
                }
                ')' => {
                    return Kind::RightParen;
                }
                '[' => {
                    return Kind::LeftBracket;
                }
                ']' => {
                    return Kind::RightBracket;
                }
                '{' => {
                    return Kind::LeftBrace;
                }
                '}' => {
                    return Kind::RightBrace;
                }
                ',' => {
                    return Kind::Comma;
                }
                _ => {
                    return Kind::Invalid;
                }
            }
        }
        Kind::Eof
    }

    // Read and return the next token in the source
    fn read_next_token(&mut self) -> Token {
        self.skip_whitespace();
        let start = self.offset();
        let kind = self.read_next_kind(start);
        let end = self.offset();
        let mut value = TokenValue::None;
        match kind {
            Kind::Integer => {
                let parsed_int = self.source[start..end].parse();
                match parsed_int {
                    Ok(int) => {
                        value = TokenValue::Integer(int);
                    }
                    Err(e) => {
                        println!("Error parsing integer: {:?}", e);
                    }
                }
            }
            Kind::BigInt => {
                let parsed_big_int = self.source[start..end].parse();
                match parsed_big_int {
                    Ok(big_int) => {
                        value = TokenValue::BigInt(big_int);
                    }
                    Err(e) => {
                        println!("Error parsing big integer: {:?}", e);
                    }
                }
            }
            Kind::Decimal => {
                let parsed_decimal = self.source[start..end].parse();
                match parsed_decimal {
                    Ok(decimal) => {
                        value = TokenValue::Decimal(decimal);
                    }
                    Err(e) => {
                        println!("Error parsing decimal: {:?}", e);
                    }
                }
            }
            Kind::String => {
                // use +1 and -1 to remove the quotes from our string literal
                // NOTE - We store escaped characters (~n) as-is to retain
                // original source mapping, any escapes can be handled later on
                value = TokenValue::String(OxablAtom::from(
                    self.source[start + 1..end - 1].to_string(),
                ));
            }
            Kind::True => value = TokenValue::Boolean(true),
            Kind::False => value = TokenValue::Boolean(false),
            // Tokens with no value (operators and keywords) just don't set a value
            _ => {}
        }

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

    /// Consume the next char
    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Doesn't advance the original chars to peek at the next char for multi-character
    /// symbols
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Returns a reserved keyword or Kind::Identifier if it's not a keyword
    fn resolve_keyword(&self, ident: &str) -> Kind {
        let lower = ident.to_lowercase();

        if Self::matches_keyword(&lower, "define", 3) {
            return Kind::Define;
        }
        if Self::matches_keyword(&lower, "variable", 3) {
            return Kind::Variable;
        }

        match lower.as_str() {
            "if" => Kind::If,
            "while" => Kind::While,
            "for" => Kind::For,
            "do" => Kind::Do,
            "eq" => Kind::Equal,
            "ne" => Kind::NotEqual,
            "lt" => Kind::LessThan,
            "gt" => Kind::GreaterThan,
            "le" => Kind::LessThanOrEqual,
            "ge" => Kind::GreaterThanOrEqual,
            "or" => Kind::Or,
            "and" => Kind::And,
            "not" => Kind::Not,
            "true" | "yes" => Kind::True,
            "false" | "no" => Kind::False,
            _ => Kind::Identifier,
        }
    }

    // Progress allows keywords to be short-form, i.e. def == define, need to match them up
    // "def", "defi", "defin", "define" all match DEFINE
    fn matches_keyword(input: &str, full_keyword: &str, min_length: usize) -> bool {
        input.len() >= min_length && full_keyword.starts_with(&input)
    }

    /// Reads the word that will resolve to either a keyword or identifer
    fn read_identifier_or_keyword(&mut self, start: usize) -> Kind {
        // Keep consuming alphanumeric chars, underscores, and hyphens
        // This shouldn't match "var1 - var2" because there is no
        // whitespace here, and "var1-var2" is not valid ABL, but in
        // our case, we would think it's an identifier. It would fail
        // compilation and get flagged later on in the parser.
        // It should still match "my-var1" though.
        while matches!(
            self.peek(),
            Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-')
        ) {
            self.advance();
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
                    self.advance(); //consume closing quote
                    return Kind::String;
                }
                Some('~') => {
                    self.advance(); // consume tilde
                    self.advance(); // consume escaped char (whatever it is)
                }
                Some(_) => {
                    self.advance(); // consume regular char
                }
                None => {
                    // Unterminated String - error?
                    return Kind::Invalid;
                }
            }
        }
    }

    // Read the number until the end, ensuring we consume the full integer
    // or decimal without accidentally consuming something like a method call
    fn read_number_literal(&mut self) -> Kind {
        // Consume all leading digits
        while matches!(self.peek(), Some('0'..='9')) {
            self.advance();
        }

        // Check for decimal point followed by digit
        if matches!(self.peek(), Some('.')) {
            // Need to peek TWO ahead - clone the iterator
            let mut lookahead = self.chars.clone();
            lookahead.next(); // skip the '.'
            if matches!(lookahead.next(), Some('0'..='9')) {
                // It's a decimal
                self.advance(); // consume the '.'
                while matches!(self.peek(), Some('0'..='9')) {
                    self.advance();
                }
                return Kind::Decimal;
            }
        }

        Kind::Integer
    }

    // TODO review
    fn skip_line_comment(&mut self) -> Kind {
        loop {
            // consume chars until we hit a new line (or EoF)
            match self.advance() {
                Some('\n') | None => {
                    return Kind::Comment;
                }
                _ => continue,
            }
        }
    }

    // TODO review
    fn skip_block_comment(&mut self) -> Kind {
        loop {
            // consume chars until we hit a *
            match self.advance() {
                // check the next char, if it's /, comment is done
                Some('*') if self.peek() == Some('/') => {
                    self.advance();
                    return Kind::Comment; // or skip and recurse to read_next_kind
                }
                // if it's None, there are no more chars to read, EoF basically
                None => return Kind::Invalid, // unterminated comment
                // else just keep going
                _ => continue,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identify_add() {
        let source = "+";
        let mut lexer = Lexer::new(source);
        let token = lexer.read_next_token();
        println!("Token created: {:?}", token);
        assert_eq!(
            token,
            Token {
                kind: Kind::Add,
                start: 0,
                end: 1,
                value: TokenValue::None
            },
            "Ensure + characters become tokens of Kind::Add, value None"
        );
    }
}
