//! OxAbl Lexer
//! A Lexer written in Rust for Progress ABL
//!
//! Produces tokens from ABL source code.
//! TODO - remove BigInt, we don't *really* need the distinction.
use std::str::Chars;
extern crate string_cache;
#[allow(dead_code)]
pub mod oxabl_atom {
    include!(concat!(env!("OUT_DIR"), "/oxabl_atom.rs"));
}
use rust_decimal::Decimal;

mod callable;
mod kind;
use crate::{kind::match_keyword, oxabl_atom::OxablAtom};
pub use callable::{CALLABLE_FUNCTION_KINDS, is_callable_kind};
pub use kind::Kind;

/// Tokenize ABL source code into a vector of tokens.
///
/// This is the main public entry point for the lexer.
pub fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.read_next_token();
        let is_eof = token.kind == Kind::Eof;
        tokens.push(token);
        if is_eof {
            break;
        }
    }
    tokens
}

/// A representation of an token created from ABL source code.
///
///
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token this is, a list can be found in `crates/oxabl_lexer/kind.rs`.
    /// Valid kinds are operators, identifiers, or keywords
    pub kind: Kind,

    /// Start offset in source
    pub start: usize,

    /// End offset in source
    pub end: usize,

    /// The value of the token.
    /// For literals it will be the value of the literal (2, 1.3, "Thing", true)
    /// For identifiers and keywords the value will be None
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

pub struct Lexer<'a> {
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
        while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
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
                        return Kind::PlusEquals;
                    }
                    _ => {
                        return Kind::Add;
                    }
                },
                '-' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::MinusEquals;
                    }
                    _ => {
                        return Kind::Minus;
                    }
                },
                '*' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::StarEquals;
                    }
                    _ => {
                        return Kind::Star;
                    }
                },
                // could be divide, could be a comment
                '/' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        return Kind::SlashEquals;
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
                        return Kind::Slash;
                    }
                },
                '%' => {
                    return Kind::Percent;
                }
                '=' => {
                    return Kind::Equals;
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
                    return Kind::Question;
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
                    return Kind::Period;
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
                '{' => match self.peek() {
                    // Preprocessor references {&thing}
                    Some('&') => {
                        self.advance(); // consume '&'
                        return self.read_preprocessor_reference(start);
                    }
                    _ => {
                        return Kind::LeftBrace;
                    }
                },
                '}' => {
                    return Kind::RightBrace;
                }
                ',' => {
                    return Kind::Comma;
                }

                // Preprocessor directives like &if, &scoped-define
                '&' => {
                    return self.read_preprocessor_directive(start);
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
        let mut kind = self.read_next_kind(start);
        let end = self.offset();
        let mut value = TokenValue::None;
        match kind {
            Kind::IntegerLiteral => {
                let text = &self.source[start..end];
                match text.parse::<i32>() {
                    Ok(int) => {
                        value = TokenValue::Integer(int);
                    }
                    Err(_) => match text.parse::<i64>() {
                        Ok(big_int) => {
                            kind = Kind::BigIntLiteral;
                            value = TokenValue::BigInt(big_int);
                        }
                        Err(e) => {
                            println!("Error parsing integer: {:?}", e);
                        }
                    },
                }
            }
            Kind::BigIntLiteral => {
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
            Kind::DecimalLiteral => {
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
            Kind::StringLiteral => {
                // use +1 and -1 to remove the quotes from our string literal
                // NOTE - We store escaped characters (~n) as-is to retain
                // original source mapping, any escapes can be handled later on
                value = TokenValue::String(OxablAtom::from(
                    self.source[start + 1..end - 1].to_string(),
                ));
            }
            Kind::KwTrue => value = TokenValue::Boolean(true),
            Kind::KwFalse => value = TokenValue::Boolean(false),
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
        let keyword = match_keyword(text);
        keyword.unwrap_or(Kind::Identifier)
    }

    /// Reads the word till the end of whatever quotes we started with
    fn read_string_literal(&mut self, quote_type: char) -> Kind {
        loop {
            match self.peek() {
                Some(c) if c == quote_type => {
                    self.advance(); //consume closing quote
                    return Kind::StringLiteral;
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
                return Kind::DecimalLiteral;
            }
        }

        Kind::IntegerLiteral
    }

    /// Reads a preprocessor reference like {&variable} or {&batch-mode}
    /// Called after '{&' has been consumed, consumes up to and including '}'
    fn read_preprocessor_reference(&mut self, start: usize) -> Kind {
        // Consume characters until we hit '}' or EOF
        while let Some(c) = self.peek() {
            if c == '}' {
                self.advance(); // consume the closing '}'
                break;
            }
            self.advance();
        }

        // Try to match the full text including {& and } against known preprocessor keywords
        let text = &self.source[start..self.offset()];
        match_keyword(text).unwrap_or(Kind::Preprop)
    }

    /// Reads a preprocessor directive like &if, &scoped-define
    /// Called after '&' has been consumed
    fn read_preprocessor_directive(&mut self, start: usize) -> Kind {
        // Consume alphanumeric characters and hyphens (like regular identifiers)
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '0'..='9' | '-')) {
            self.advance();
        }

        // Try to match the full text including & against known preprocessor keywords
        let text = &self.source[start..self.offset()];
        match_keyword(text).unwrap_or(Kind::Invalid)
    }

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

    fn collect_tokens(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.read_next_token();
            let is_eof = token.kind == Kind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    /// Helper to assert a token matches expected kind, offsets, and value
    fn assert_token(
        token: &Token,
        expected_kind: Kind,
        expected_start: usize,
        expected_end: usize,
        expected_value: TokenValue,
        source: &str,
    ) {
        assert_eq!(
            token.kind,
            expected_kind,
            "Kind mismatch at {}..{}: expected {:?}, got {:?}. Source slice: {:?}",
            expected_start,
            expected_end,
            expected_kind,
            token.kind,
            &source[token.start..token.end]
        );
        assert_eq!(
            token.start, expected_start,
            "Start offset mismatch for {:?}: expected {}, got {}",
            expected_kind, expected_start, token.start
        );
        assert_eq!(
            token.end, expected_end,
            "End offset mismatch for {:?}: expected {}, got {}",
            expected_kind, expected_end, token.end
        );
        assert_eq!(
            token.value, expected_value,
            "Value mismatch for {:?}: expected {:?}, got {:?}",
            expected_kind, expected_value, token.value
        );
    }

    #[test]
    fn single_operator() {
        let source = "+";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2); // + and Eof
        assert_token(&tokens[0], Kind::Add, 0, 1, TokenValue::None, source);
        assert_token(&tokens[1], Kind::Eof, 1, 1, TokenValue::None, source);
    }

    #[test]
    fn variable_definition_with_assignment() {
        // No leading newline - offsets are straightforward
        // "def var myCount as int no-undo."
        //  0123456789...
        let source = "def var myCount as int no-undo.\nassign myCount = 42.";
        let tokens = collect_tokens(source);

        // Line 1: def var myCount as int no-undo.
        // def: 0-3, var: 4-7, myCount: 8-15, as: 16-18, int: 19-22, no-undo: 23-30, .: 30-31
        // Line 2 starts at 32: assign myCount = 42.
        // assign: 32-38, myCount: 39-46, =: 47-48, 42: 49-51, .: 51-52

        let expected = vec![
            (Kind::Define, 0, 3, TokenValue::None),
            (Kind::Identifier, 4, 7, TokenValue::None), // var (not reserved)
            (Kind::Identifier, 8, 15, TokenValue::None), // myCount
            (Kind::KwAs, 16, 18, TokenValue::None),
            (Kind::Identifier, 19, 22, TokenValue::None), // int (not reserved)
            (Kind::NoUndo, 23, 30, TokenValue::None),
            (Kind::Period, 30, 31, TokenValue::None),
            (Kind::Assign, 32, 38, TokenValue::None),
            (Kind::Identifier, 39, 46, TokenValue::None), // myCount
            (Kind::Equals, 47, 48, TokenValue::None),
            (Kind::IntegerLiteral, 49, 51, TokenValue::Integer(42)),
            (Kind::Period, 51, 52, TokenValue::None),
            (Kind::Eof, 52, 52, TokenValue::None),
        ];

        assert_eq!(tokens.len(), expected.len(), "Token count mismatch");
        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn procedure_with_control_flow() {
        // Simpler version without deep nesting to keep offsets manageable
        let source = "do i = 1 to 10:\n    message \"hello\".\nend.";
        let tokens = collect_tokens(source);

        // do: 0-2, i: 3-4, =: 5-6, 1: 7-8, to: 9-11, 10: 12-14, :: 14-15
        // \n at 15, spaces 16-19
        // message: 20-27, "hello": 28-35, .: 35-36
        // \n at 36
        // end: 37-40, .: 40-41

        let expected = vec![
            (Kind::Do, 0, 2, TokenValue::None),
            (Kind::Identifier, 3, 4, TokenValue::None), // i
            (Kind::Equals, 5, 6, TokenValue::None),
            (Kind::IntegerLiteral, 7, 8, TokenValue::Integer(1)),
            (Kind::To, 9, 11, TokenValue::None),
            (Kind::IntegerLiteral, 12, 14, TokenValue::Integer(10)),
            (Kind::Colon, 14, 15, TokenValue::None),
            (Kind::Message, 20, 27, TokenValue::None),
            (
                Kind::StringLiteral,
                28,
                35,
                TokenValue::String(OxablAtom::from("hello".to_string())),
            ),
            (Kind::Period, 35, 36, TokenValue::None),
            (Kind::End, 37, 40, TokenValue::None),
            (Kind::Period, 40, 41, TokenValue::None),
            (Kind::Eof, 41, 41, TokenValue::None),
        ];

        assert_eq!(
            tokens.len(),
            expected.len(),
            "Token count mismatch. Got: {:?}",
            tokens
        );
        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn keyword_abbreviations() {
        // Test that abbreviations produce correct tokens with correct offsets
        let test_cases = vec![
            ("def", Kind::Define, 3),
            ("defi", Kind::Define, 4),
            ("defin", Kind::Define, 5),
            ("define", Kind::Define, 6),
            ("disp", Kind::Display, 4),
            ("displ", Kind::Display, 5),
            ("displa", Kind::Display, 6),
            ("display", Kind::Display, 7),
            ("avail", Kind::Available, 5),
            ("availa", Kind::Available, 6),
            ("availab", Kind::Available, 7),
            ("availabl", Kind::Available, 8),
            ("available", Kind::Available, 9),
        ];

        for (source, expected_kind, expected_len) in test_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}",
                source,
                tokens.len()
            );
            assert_token(
                &tokens[0],
                expected_kind,
                0,
                expected_len,
                TokenValue::None,
                source,
            );
        }
    }

    #[test]
    fn decimal_number() {
        let source = "123.456";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::DecimalLiteral,
            0,
            7,
            TokenValue::Decimal("123.456".parse().unwrap()),
            source,
        );
    }

    #[test]
    fn string_with_escapes() {
        // ABL uses ~ for escapes, e.g. ~n for newline
        let source = r#""hello~nworld""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::StringLiteral,
            0,
            14,
            TokenValue::String(OxablAtom::from("hello~nworld".to_string())),
            source,
        );
    }

    #[test]
    fn comments_line() {
        let source = "def // this is a comment\nvar";
        let tokens = collect_tokens(source);
        // def, comment, var, eof
        assert_eq!(tokens.len(), 4, "Got: {:?}", tokens);
        assert_token(&tokens[0], Kind::Define, 0, 3, TokenValue::None, source);
        assert_token(&tokens[1], Kind::Comment, 4, 25, TokenValue::None, source);
        assert_token(
            &tokens[2],
            Kind::Identifier,
            25,
            28,
            TokenValue::None,
            source,
        ); // var (not reserved)
    }

    #[test]
    fn comments_block() {
        let source = "def /* block */ var";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4, "Got: {:?}", tokens);
        assert_token(&tokens[0], Kind::Define, 0, 3, TokenValue::None, source);
        assert_token(&tokens[1], Kind::Comment, 4, 15, TokenValue::None, source);
        assert_token(
            &tokens[2],
            Kind::Identifier,
            16,
            19,
            TokenValue::None,
            source,
        ); // var (not reserved)
    }

    #[test]
    fn operators_comparison() {
        let source = "> >= < <= <> =";
        let tokens = collect_tokens(source);
        let expected = vec![
            (Kind::GreaterThan, 0, 1),
            (Kind::GreaterThanOrEqual, 2, 4),
            (Kind::LessThan, 5, 6),
            (Kind::LessThanOrEqual, 7, 9),
            (Kind::NotEqual, 10, 12),
            (Kind::Equals, 13, 14),
            (Kind::Eof, 14, 14),
        ];
        assert_eq!(tokens.len(), expected.len());
        for (i, (kind, start, end)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, TokenValue::None, source);
        }
    }

    #[test]
    fn complex_procedure_file() {
        let source = r#"/* my test file */

/*
this is a multi line comment
*/

// my test procedure
procedure my_test_proc:
   var int MyInt = 1.
   var int MyOtherInt = 2.
   var int result.
   result = MyOtherInt - MyInt.
   return result.
end."#;

        let tokens = collect_tokens(source);

        // Calculate offsets:
        // Line 1: "/* my test file */" = 0-17 (18 chars), \n at 18
        // Line 2: empty \n at 19
        // Line 3-5: "/*\nthis is a multi line comment\n*/" = 20-53, \n at 54
        // Line 6: empty \n at 55
        // Line 7: "// my test procedure" = 56-75 (20 chars), \n at 76
        // Line 8: "procedure" 77-85, " " 86, "my_test_proc" 87-98, ":" 99, \n 100
        // Line 9: "   " 101-103, "var" 104-106, " " 107, "int" 108-110, " " 111,
        //         "MyInt" 112-116, " " 117, "=" 118, " " 119, "1" 120, "." 121, \n 122
        // Line 10: "   " 123-125, "var" 126-128, " " 129, "int" 130-132, " " 133,
        //          "MyOtherInt" 134-143, " " 144, "=" 145, " " 146, "2" 147, "." 148, \n 149
        // Line 11: "   " 150-152, "var" 153-155, " " 156, "int" 157-159, " " 160,
        //          "result" 161-166, "." 167, \n 168
        // Line 12: "   " 169-171, "result" 172-177, " " 178, "=" 179, " " 180,
        //          "MyOtherInt" 181-190, " " 191, "-" 192, " " 193, "MyInt" 194-198, "." 199, \n 200
        // Line 13: "   " 201-203, "return" 204-209, " " 210, "result" 211-216, "." 217, \n 218
        // Line 14: "end" 219-221, "." 222

        let expected = vec![
            // Block comment 1
            (Kind::Comment, 0, 18, TokenValue::None),
            // Block comment 2 (multi-line)
            (Kind::Comment, 20, 54, TokenValue::None),
            // Line comment
            (Kind::Comment, 56, 77, TokenValue::None),
            // procedure my_test_proc:
            (Kind::Identifier, 77, 86, TokenValue::None), // procedure (not reserved)
            (Kind::Identifier, 87, 99, TokenValue::None), // my_test_proc
            (Kind::Colon, 99, 100, TokenValue::None),
            // var int MyInt = 1.
            (Kind::Identifier, 104, 107, TokenValue::None), // var (not reserved)
            (Kind::Identifier, 108, 111, TokenValue::None), // int (not reserved)
            (Kind::Identifier, 112, 117, TokenValue::None), // MyInt
            (Kind::Equals, 118, 119, TokenValue::None),
            (Kind::IntegerLiteral, 120, 121, TokenValue::Integer(1)),
            (Kind::Period, 121, 122, TokenValue::None),
            // var int MyOtherInt = 2.
            (Kind::Identifier, 126, 129, TokenValue::None), // var (not reserved)
            (Kind::Identifier, 130, 133, TokenValue::None), // int (not reserved)
            (Kind::Identifier, 134, 144, TokenValue::None), // MyOtherInt
            (Kind::Equals, 145, 146, TokenValue::None),
            (Kind::IntegerLiteral, 147, 148, TokenValue::Integer(2)),
            (Kind::Period, 148, 149, TokenValue::None),
            // var int result.
            (Kind::Identifier, 153, 156, TokenValue::None), // var (not reserved)
            (Kind::Identifier, 157, 160, TokenValue::None), // int (not reserved)
            (Kind::Identifier, 161, 167, TokenValue::None), // result
            (Kind::Period, 167, 168, TokenValue::None),
            // result = MyOtherInt - MyInt.
            (Kind::Identifier, 172, 178, TokenValue::None), // result
            (Kind::Equals, 179, 180, TokenValue::None),
            (Kind::Identifier, 181, 191, TokenValue::None), // MyOtherInt
            (Kind::Minus, 192, 193, TokenValue::None),
            (Kind::Identifier, 194, 199, TokenValue::None), // MyInt
            (Kind::Period, 199, 200, TokenValue::None),
            // return result.
            (Kind::KwReturn, 204, 210, TokenValue::None),
            (Kind::Identifier, 211, 217, TokenValue::None), // result
            (Kind::Period, 217, 218, TokenValue::None),
            // end.
            (Kind::End, 219, 222, TokenValue::None),
            (Kind::Period, 222, 223, TokenValue::None),
            (Kind::Eof, 223, 223, TokenValue::None),
        ];

        assert_eq!(
            tokens.len(),
            expected.len(),
            "Token count mismatch. Expected {}, got {}.\nTokens: {:?}",
            expected.len(),
            tokens.len(),
            tokens
        );

        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn preprocessor_directive_ampersand() {
        // Test &if, &else, &endif, &scoped-define, &global-define
        let source = "&if defined(test) &then\n&scoped-define myvar 1\n&endif";
        let tokens = collect_tokens(source);

        // &if: 0-3, defined: 4-11, (: 11-12, test: 12-16, ): 16-17
        // &then: 18-23, \n at 23
        // &scoped-define: 24-38 (14 chars), myvar: 39-44, 1: 45-46, \n at 46
        // &endif: 47-53

        let expected = vec![
            (Kind::PreprocIf, 0, 3, TokenValue::None),
            (Kind::Identifier, 4, 11, TokenValue::None), // defined (not reserved)
            (Kind::LeftParen, 11, 12, TokenValue::None),
            (Kind::Identifier, 12, 16, TokenValue::None), // test
            (Kind::RightParen, 16, 17, TokenValue::None),
            (Kind::PreprocThen, 18, 23, TokenValue::None),
            (Kind::PreprocScopedDefine, 24, 38, TokenValue::None),
            (Kind::Identifier, 39, 44, TokenValue::None), // myvar
            (Kind::IntegerLiteral, 45, 46, TokenValue::Integer(1)),
            (Kind::PreprocEndif, 47, 53, TokenValue::None),
            (Kind::Eof, 53, 53, TokenValue::None),
        ];

        assert_eq!(
            tokens.len(),
            expected.len(),
            "Token count mismatch. Got: {:?}",
            tokens
        );
        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn preprocessor_reference_braces() {
        // Test {&variable} and {&batch-mode}
        let source = "message {&myvar}.\nif {&batch-mode} then quit.";
        let tokens = collect_tokens(source);

        // message: 0-7, {&myvar}: 8-16 (Preprop - user variable), .: 16-17, \n at 17
        // if: 18-20, {&batch-mode}: 21-34 (PreprocBatchMode), then: 35-39, quit: 40-44, .: 44-45

        let expected = vec![
            (Kind::Message, 0, 7, TokenValue::None),
            (Kind::Preprop, 8, 16, TokenValue::None), // {&myvar} - user-defined
            (Kind::Period, 16, 17, TokenValue::None),
            (Kind::KwIf, 18, 20, TokenValue::None),
            (Kind::Preprop, 21, 34, TokenValue::None), // {&batch-mode} - also Preprop (not reserved)
            (Kind::Then, 35, 39, TokenValue::None),
            (Kind::Quit, 40, 44, TokenValue::None),
            (Kind::Period, 44, 45, TokenValue::None),
            (Kind::Eof, 45, 45, TokenValue::None),
        ];

        assert_eq!(
            tokens.len(),
            expected.len(),
            "Token count mismatch. Got: {:?}",
            tokens
        );
        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn preprocessor_abbreviations() {
        // Test that preprocessor abbreviations work like regular keywords
        let test_cases = vec![
            ("&glob", Kind::PreprocGlobalDefine, 5),
            ("&global-define", Kind::PreprocGlobalDefine, 14),
            ("&scop", Kind::PreprocScopedDefine, 5),
            ("&scoped-define", Kind::PreprocScopedDefine, 14),
            ("&undef", Kind::PreprocUndefine, 6),
            ("&undefine", Kind::PreprocUndefine, 9),
        ];

        for (source, expected_kind, expected_len) in test_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}",
                source,
                tokens.len()
            );
            assert_token(
                &tokens[0],
                expected_kind,
                0,
                expected_len,
                TokenValue::None,
                source,
            );
        }
    }

    #[test]
    fn compound_assignment_operators() {
        let source = "+= -= *= /=";
        let tokens = collect_tokens(source);
        let expected = vec![
            (Kind::PlusEquals, 0, 2),
            (Kind::MinusEquals, 3, 5),
            (Kind::StarEquals, 6, 8),
            (Kind::SlashEquals, 9, 11),
            (Kind::Eof, 11, 11),
        ];
        assert_eq!(tokens.len(), expected.len());
        for (i, (kind, start, end)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, TokenValue::None, source);
        }
    }

    #[test]
    fn double_colon_static_access() {
        let source = "MyClass::StaticMethod";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4);
        assert_token(&tokens[0], Kind::Identifier, 0, 7, TokenValue::None, source);
        assert_token(
            &tokens[1],
            Kind::DoubleColon,
            7,
            9,
            TokenValue::None,
            source,
        );
        assert_token(
            &tokens[2],
            Kind::Identifier,
            9,
            21,
            TokenValue::None,
            source,
        );
    }

    #[test]
    fn single_quote_string() {
        let source = "'hello world'";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::StringLiteral,
            0,
            13,
            TokenValue::String(OxablAtom::from("hello world".to_string())),
            source,
        );
    }

    #[test]
    fn empty_string() {
        let source = "\"\"";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::StringLiteral,
            0,
            2,
            TokenValue::String(OxablAtom::from("".to_string())),
            source,
        );
    }

    #[test]
    fn integer_followed_by_period_not_decimal() {
        // 42. followed by identifier should be integer + period + identifier
        // Using "foo" instead of "method" since "method" is a keyword
        let source = "42.foo";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4);
        assert_token(
            &tokens[0],
            Kind::IntegerLiteral,
            0,
            2,
            TokenValue::Integer(42),
            source,
        );
        assert_token(&tokens[1], Kind::Period, 2, 3, TokenValue::None, source);
        assert_token(&tokens[2], Kind::Identifier, 3, 6, TokenValue::None, source);
    }

    #[test]
    fn brackets_and_braces() {
        let source = "arr[0] = {&var}";
        let tokens = collect_tokens(source);
        let expected = vec![
            (Kind::Identifier, 0, 3, TokenValue::None),
            (Kind::LeftBracket, 3, 4, TokenValue::None),
            (Kind::IntegerLiteral, 4, 5, TokenValue::Integer(0)),
            (Kind::RightBracket, 5, 6, TokenValue::None),
            (Kind::Equals, 7, 8, TokenValue::None),
            (Kind::Preprop, 9, 15, TokenValue::None),
            (Kind::Eof, 15, 15, TokenValue::None),
        ];
        assert_eq!(tokens.len(), expected.len());
        for (i, (kind, start, end, value)) in expected.into_iter().enumerate() {
            assert_token(&tokens[i], kind, start, end, value, source);
        }
    }

    #[test]
    fn boolean_literals() {
        let source = "true false";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 3);
        assert_token(
            &tokens[0],
            Kind::KwTrue,
            0,
            4,
            TokenValue::Boolean(true),
            source,
        );
        assert_token(
            &tokens[1],
            Kind::KwFalse,
            5,
            10,
            TokenValue::Boolean(false),
            source,
        );
    }

    #[test]
    fn question_mark_unknown() {
        let source = "var int x = ?.";
        let tokens = collect_tokens(source);
        // var, int, x, =, ?, ., eof
        assert_eq!(tokens.len(), 7);
        assert_token(&tokens[4], Kind::Question, 12, 13, TokenValue::None, source);
    }

    #[test]
    fn unterminated_string() {
        let source = "\"hello";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, Kind::Invalid);
    }

    #[test]
    fn unterminated_block_comment() {
        let source = "def /* unterminated";
        let tokens = collect_tokens(source);
        // def, then invalid comment
        assert_eq!(tokens[1].kind, Kind::Invalid);
    }

    #[test]
    fn hyphenated_identifier() {
        // ABL allows hyphens in identifiers
        let source = "my-variable-name";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::Identifier,
            0,
            16,
            TokenValue::None,
            source,
        );
    }

    #[test]
    fn modulo_operator() {
        let source = "10 % 3";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4);
        assert_token(&tokens[1], Kind::Percent, 3, 4, TokenValue::None, source);
    }

    #[test]
    fn bigint_overflow() {
        // Test that integers larger than i32::MAX become BigInt
        let source = "3000000000";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::BigIntLiteral,
            0,
            10,
            TokenValue::BigInt(3000000000),
            source,
        );
    }

    #[test]
    fn temp_table_definition() {
        let source = "def temp-table tt field f1 as int.";
        let tokens = collect_tokens(source);
        // Verify it tokenizes without errors and has correct structure
        assert!(tokens.iter().all(|t| t.kind != Kind::Invalid));
        assert_eq!(tokens[0].kind, Kind::Define);
        assert_eq!(tokens[1].kind, Kind::Identifier); // temp-table (not reserved)
    }
}
