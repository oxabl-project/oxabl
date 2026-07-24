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

mod builtins;
mod callable;
mod kind;
use crate::{kind::match_keyword, oxabl_atom::OxablAtom};
pub use builtins::{BUILTIN_FUNCTIONS, is_builtin_function};
pub use callable::{CALLABLE_FUNCTION_KINDS, is_callable_kind};
pub use kind::Kind;

/// Tokenize ABL source code into a vector of tokens.
///
/// This is the main batch entry point for the lexer. Its output — every token up
/// to and including the terminal [`Kind::Eof`] — is identical to iterating a
/// [`Lexer`] to exhaustion (see the [`Iterator`] impl); memory-constrained
/// consumers that want lazy tokenization can iterate a `Lexer` directly.
///
/// The batch path deliberately drives [`read_next_token`](Lexer::read_next_token)
/// in a tight loop rather than going through the `Iterator` adaptor: `tokenize`
/// is a lexer hot path, and the adaptor's per-token `Option`/EOF-state overhead
/// measurably regressed it. The `Iterator` impl reuses the same
/// `read_next_token`, and a test asserts the two stay byte-identical.
pub fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    // Pre-allocate based on source length to avoid repeated realloc/mmap calls.
    // ABL source averages ~1 token per 5–8 bytes; dividing by 5 is conservative
    // (slight over-allocation is cheaper than multiple heap growths).
    let mut tokens = Vec::with_capacity(source.len() / 5);
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

    /// True while tokenizing a `&SCOPED-DEFINE` or `&GLOBAL-DEFINE` value.
    /// When set, newlines emit `Kind::PreprocEnd` instead of being skipped.
    in_directive: bool,

    /// True once the terminal [`Kind::Eof`] token has been yielded through the
    /// [`Iterator`] impl, so iteration stops instead of re-reading past EOF.
    finished: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            in_directive: false,
            finished: false,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' if !self.in_directive => {
                    self.advance();
                }
                // Tilde (~) or backslash (\) followed by optional spaces/tabs then a newline
                // is a line-continuation. Handle it here so that `start` in
                // read_next_kind(start) correctly points to the first character of the NEXT
                // token (not the continuation marker), which matters for keyword matching.
                '~' | '\\' => {
                    let is_continuation = {
                        let mut look = self.chars.clone();
                        look.next(); // skip the marker itself (peek() didn't advance)
                        loop {
                            match look.next() {
                                Some(' ') | Some('\t') | Some('\r') => {}
                                Some('\n') => break true,
                                _ => break false,
                            }
                        }
                    };
                    if is_continuation {
                        self.advance(); // consume ~ or \
                        while matches!(self.peek(), Some(' ') | Some('\t') | Some('\r')) {
                            self.advance();
                        }
                        if matches!(self.peek(), Some('\n')) {
                            self.advance(); // consume \n
                        }
                        // Continue the loop — there may be more leading whitespace on the next line
                    } else {
                        break; // not followed by newline — stop, let read_next_kind handle it
                    }
                }
                _ => break,
            }
        }
    }

    fn read_next_kind(&mut self, start: usize) -> Kind {
        while let Some(c) = self.advance() {
            match c {
                // whitespace, skip (we skip earlier, this is a sanity check)
                ' ' | '\t' => continue,
                '\n' if self.in_directive => {
                    self.in_directive = false;
                    return Kind::PreprocEnd;
                }
                '\n' => continue,

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

                // A '.' followed by a digit is a decimal literal like .01, .5, .123
                // Otherwise it's a statement terminator.
                '.' => {
                    if matches!(self.peek(), Some('0'..='9')) {
                        // Consume digits after the decimal point
                        while matches!(self.peek(), Some('0'..='9')) {
                            self.advance();
                        }
                        return Kind::DecimalLiteral;
                    }
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
                    // Include positional argument references {0}, {1}, {2}, etc.
                    Some('0'..='9') => {
                        return self.read_include_arg_reference(start);
                    }
                    // Include file references {file.i}, {path/file.i args}
                    // Also handles {{&var}...} where include path starts with a preproc variable
                    Some('a'..='z' | 'A'..='Z' | '/' | '.' | '"' | '{') => {
                        return self.read_include_reference(start);
                    }
                    // Leading whitespace inside braces - look ahead to determine type
                    Some(c) if c.is_ascii_whitespace() => {
                        // Peek past whitespace to find first meaningful char
                        let mut lookahead = self.chars.clone();
                        let first_non_ws = loop {
                            match lookahead.next() {
                                Some(ws) if ws.is_ascii_whitespace() => continue,
                                other => break other,
                            }
                        };
                        match first_non_ws {
                            Some('0'..='9') => return self.read_include_arg_reference(start),
                            Some('a'..='z' | 'A'..='Z' | '/' | '.' | '"' | '{') => {
                                return self.read_include_reference(start);
                            }
                            _ => return Kind::LeftBrace,
                        }
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
                // '@' is ABL's field-format reference operator in DISPLAY (same as keyword AT)
                '@' => {
                    return Kind::At;
                }
                // Tilde (~) is ABL's line-continuation character.
                // Inside &SCOPED-DEFINE / &GLOBAL-DEFINE: skip ~ and \n without ending directive.
                // Outside directives: also skip ~ followed by \n as a line continuation.
                '~' => {
                    if matches!(self.peek(), Some('\r')) {
                        self.advance(); // consume \r
                    }
                    if matches!(self.peek(), Some('\n')) {
                        self.advance(); // consume \n
                        if self.in_directive {
                            // Stay in directive; do not emit PreprocEnd
                        }
                        // else: \n was already consumed; loop continues to next char
                    }
                    continue; // keep reading
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

        // If EOF reached while inside a &DEFINE directive, emit PreprocEnd first.
        // The next call will produce Eof.
        if kind == Kind::Eof && self.in_directive {
            self.in_directive = false;
            kind = Kind::PreprocEnd;
        }

        let end = self.offset();
        let mut value = TokenValue::None;
        match kind {
            Kind::IntegerLiteral => {
                let text = &self.source[start..end];
                // Handle ABL trailing-minus notation: "119999-" → -119999
                let (digits, neg) = if let Some(stripped) = text.strip_suffix('-') {
                    (stripped, true)
                } else {
                    (text, false)
                };
                match digits.parse::<i32>() {
                    Ok(int) => {
                        value = TokenValue::Integer(if neg { -int } else { int });
                    }
                    Err(_) => match digits.parse::<i64>() {
                        Ok(big_int) => {
                            kind = Kind::BigIntLiteral;
                            value = TokenValue::BigInt(if neg { -big_int } else { big_int });
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
                let text = &self.source[start..end];
                // Handle ABL trailing-minus notation: "1.0-" → -1.0
                let (digits, neg) = if let Some(stripped) = text.strip_suffix('-') {
                    (stripped, true)
                } else {
                    (text, false)
                };
                match digits.parse::<Decimal>() {
                    Ok(decimal) => {
                        value = TokenValue::Decimal(if neg { -decimal } else { decimal });
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
                // ABL translation suffixes like :U or :T are consumed into the token
                // span but excluded from the stored value.
                let bytes = self.source.as_bytes();
                let content_end =
                    if end >= 4 && bytes[end - 2] == b':' && bytes[end - 1].is_ascii_alphabetic() {
                        end - 3 // strip closing quote + :X suffix
                    } else {
                        end - 1 // strip closing quote only
                    };
                value = TokenValue::String(OxablAtom::from(
                    self.source[start + 1..content_end].to_string(),
                ));
            }
            Kind::KwTrue => value = TokenValue::Boolean(true),
            Kind::KwFalse => value = TokenValue::Boolean(false),
            Kind::IncludeReference => {
                // Store trimmed content between braces (excluding { and })
                let inner = &self.source[start + 1..end - 1];
                let trimmed = inner.trim();
                value = TokenValue::String(OxablAtom::from(trimmed));
            }
            Kind::IncludeArgReference => {
                // Store the positional argument index
                let digits = &self.source[start + 1..end - 1];
                if let Ok(index) = digits.parse::<i32>() {
                    value = TokenValue::Integer(index);
                }
            }
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

    /// Try to read a space-separated lock type (e.g., "NO LOCK", "SHARE LOCK", "EXCLUSIVE LOCK").
    /// Called after reading a word that could be the first part of a lock type.
    /// Returns Some(Kind) if successful, None if not a lock type (iterator unchanged).
    fn try_read_space_separated_lock(&mut self, first_word: &str) -> Option<Kind> {
        // Check if first word is one that could start a lock type
        let lock_kind = if first_word.eq_ignore_ascii_case("no") {
            Kind::NoLock
        } else if first_word.eq_ignore_ascii_case("share") {
            Kind::ShareLock
        } else if first_word.eq_ignore_ascii_case("exclusive") {
            Kind::ExclusiveLock
        } else {
            return None;
        };

        // Save iterator state for potential rollback
        let saved_chars = self.chars.clone();

        // Skip horizontal whitespace (spaces and tabs, NOT newlines)
        let mut has_whitespace = false;
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
            has_whitespace = true;
        }

        // Must have at least some whitespace between words
        if !has_whitespace {
            self.chars = saved_chars;
            return None;
        }

        // Check if next word is "lock" (case-insensitive)
        let word_start = self.offset();
        while matches!(
            self.peek(),
            Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-')
        ) {
            self.advance();
        }

        let next_word = &self.source[word_start..self.offset()];
        if next_word.eq_ignore_ascii_case("lock") {
            // Success - we consumed "WORD <whitespace> LOCK"
            Some(lock_kind)
        } else {
            // Not a lock type - rollback
            self.chars = saved_chars;
            None
        }
    }

    /// Reads the word that will resolve to either a keyword or identifer
    fn read_identifier_or_keyword(&mut self, start: usize) -> Kind {
        // Keep consuming alphanumeric chars, underscores, hyphens, '#', and '$'.
        // ABL allows '#' and '$' in identifiers (e.g. pPrintPO#, v-tot-mtd$).
        // This shouldn't match "var1 - var2" because there is no
        // whitespace here, and "var1-var2" is not valid ABL, but in
        // our case, we would think it's an identifier. It would fail
        // compilation and get flagged later on in the parser.
        // It should still match "my-var1" though.
        while matches!(
            self.peek(),
            Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '#' | '$')
        ) {
            self.advance();
        }

        // NOW we have the full word
        let text = &self.source[start..self.offset()];

        // Check for space-separated lock types (e.g., "NO LOCK", "SHARE LOCK")
        // This must be done BEFORE checking keywords since "no", "share", etc.
        // are not keywords on their own.
        if let Some(lock_kind) = self.try_read_space_separated_lock(text) {
            return lock_kind;
        }

        // Check if it's a keyword
        let keyword = match_keyword(text);
        keyword.unwrap_or(Kind::Identifier)
    }

    /// Peek two chars ahead of the current position and decide whether the
    /// quote at `peek()` should be treated as an escaped quote (`\"`) or as
    /// the terminator of the string literal. Only called after we've already
    /// verified `peek()` is a quote. If the char after the quote looks like
    /// an expression terminator (whitespace, `)`, `,`, `.`, `+`, `=`, etc.),
    /// assume the quote is closing the string.
    fn quote_is_escape_lookalike(&self) -> bool {
        let mut la = self.chars.clone();
        la.next(); // skip the quote
        match la.next() {
            // Clear expression-terminator context — quote is the terminator.
            None | Some(')') | Some(',') | Some('.') | Some('+') | Some(';') | Some(':')
            | Some('=') | Some(' ') | Some('\t') | Some('\r') | Some('\n') => false,
            // Anything else (letters, digits, angle brackets, slashes,
            // quotes, etc.) looks like continued string content — treat the
            // quote as escaped.
            _ => true,
        }
    }

    /// Reads the word till the end of whatever quotes we started with
    fn read_string_literal(&mut self, quote_type: char) -> Kind {
        // Legacy ABL corpora occasionally embed HTML/JS with `\"` as an
        // escaped quote (e.g. `"<td style=\"x\">"`), even though ABL's
        // official escape character is tilde. We tolerate `\"` / `\'` as an
        // escaped quote *only* when the backslash is not itself preceded by
        // another backslash — so `"\\"` (two literal backslashes, common
        // in Windows paths) still terminates correctly at its closing quote.
        let mut prev_was_backslash = false;
        loop {
            match self.peek() {
                Some(c) if c == quote_type => {
                    self.advance(); //consume closing quote
                    // ABL supports translation suffixes like :U (untranslatable), :T (translatable)
                    // and integer-width specifiers like :10 (column width).
                    // Consume the colon and the suffix if present.
                    if matches!(self.peek(), Some(':')) {
                        let mut chars_clone = self.chars.clone();
                        chars_clone.next(); // skip ':'
                        match chars_clone.next() {
                            Some(c) if c.is_ascii_alphabetic() => {
                                self.advance(); // consume ':'
                                self.advance(); // consume suffix letter (U/T)
                                // Consume any trailing digits (e.g. the "1" in ":U1")
                                while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                                    self.advance();
                                }
                            }
                            Some(c) if c.is_ascii_digit() => {
                                self.advance(); // consume ':'
                                // consume all digits of the integer width
                                while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                                    self.advance();
                                }
                            }
                            _ => {}
                        }
                    }
                    return Kind::StringLiteral;
                }
                Some('~') => {
                    self.advance(); // consume tilde
                    self.advance(); // consume escaped char (whatever it is)
                    prev_was_backslash = false;
                }
                Some('\\') => {
                    self.advance(); // consume backslash
                    // Tolerate `\"` / `\'` as escaped quote only when:
                    //   1. The backslash is not itself preceded by another
                    //      backslash (so `"\\"` is a 2-backslash literal).
                    //   2. The char immediately following the quote looks
                    //      like string-interior content, not an expression
                    //      terminator. This disambiguates `"/\")` (path
                    //      ending in backslash, closes at the quote) from
                    //      `"<td style=\"x\">"` (HTML embedded escape).
                    let is_quote = matches!(self.peek(), Some('"') | Some('\''));
                    if !prev_was_backslash && is_quote && self.quote_is_escape_lookalike() {
                        self.advance(); // consume escaped quote
                        prev_was_backslash = false;
                    } else {
                        prev_was_backslash = true;
                    }
                }
                Some(_) => {
                    self.advance(); // consume regular char
                    prev_was_backslash = false;
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
                // Check for ABL trailing-minus notation on decimal (e.g. 1.0- means -1.0)
                if self.is_trailing_minus() {
                    self.advance(); // consume '-'
                }
                return Kind::DecimalLiteral;
            }
        }

        // Check for ABL trailing-minus notation (e.g. 119999- means -119999)
        if self.is_trailing_minus() {
            self.advance(); // consume '-'
        }

        Kind::IntegerLiteral
    }

    /// Returns true if the next char is `-` and the char after it is not a digit,
    /// identifier-start character, or another `-`. This identifies trailing-minus
    /// notation (ABL allows `119999-` to mean `-119999`).
    fn is_trailing_minus(&self) -> bool {
        if !matches!(self.peek(), Some('-')) {
            return false;
        }
        let mut look = self.chars.clone();
        look.next(); // skip '-'
        !matches!(
            look.next(),
            Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '-')
        )
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

    /// Reads an include file reference like {file.i} or {file.i arg1 arg2}
    /// Called after '{' has been consumed and we've peeked an alpha, '/', '.', or '"' char.
    /// Consumes up to and including the matching '}', tracking nested braces so that
    /// argument expressions like `{file.i &arg={&var}}` are consumed as a single token.
    fn read_include_reference(&mut self, _start: usize) -> Kind {
        let mut depth = 1usize;
        while let Some(c) = self.peek() {
            match c {
                '{' => {
                    self.advance();
                    depth += 1;
                }
                '}' => {
                    self.advance();
                    depth -= 1;
                    if depth == 0 {
                        return Kind::IncludeReference;
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        // Unterminated include reference
        Kind::Invalid
    }

    /// Reads an include positional argument reference like {1} or {0}
    /// Called after '{' has been consumed and we've peeked a digit.
    /// Consumes up to and including '}'.
    fn read_include_arg_reference(&mut self, _start: usize) -> Kind {
        // Consume digits
        while matches!(self.peek(), Some('0'..='9')) {
            self.advance();
        }

        // Expect closing '}'
        match self.peek() {
            Some('}') => {
                self.advance(); // consume '}'
                Kind::IncludeArgReference
            }
            _ => Kind::Invalid,
        }
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
        let kind = match_keyword(text).unwrap_or(Kind::Invalid);

        // For define directives, enter directive mode so newlines emit PreprocEnd
        if matches!(kind, Kind::PreprocScopedDefine | Kind::PreprocGlobalDefine) {
            self.in_directive = true;
        }

        // Unknown &-directives (e.g. AppBuilder's &ANALYZE-SUSPEND/&ANALYZE-RESUME) are
        // not part of standard ABL syntax. Treat them as line comments: skip to EOL.
        if kind == Kind::Invalid {
            while !matches!(self.peek(), Some('\n') | None) {
                self.advance();
            }
            return Kind::Comment;
        }

        kind
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
        // ABL supports nested block comments: /* outer /* inner */ still-outer */
        let mut depth: u32 = 1;
        loop {
            match self.advance() {
                // Opening a nested comment
                Some('/') if self.peek() == Some('*') => {
                    self.advance(); // consume '*'
                    depth += 1;
                }
                // Closing a comment level
                Some('*') if self.peek() == Some('/') => {
                    self.advance(); // consume '/'
                    depth -= 1;
                    if depth == 0 {
                        return Kind::Comment;
                    }
                }
                None => return Kind::Invalid, // unterminated comment
                _ => continue,
            }
        }
    }
}

/// Streaming, lazy tokenization: a [`Lexer`] yields one [`Token`] at a time up
/// to and including the terminal [`Kind::Eof`], then stops. The token sequence
/// is identical to [`tokenize`], which is itself `Lexer::new(src).collect()`.
///
/// A memory-constrained consumer can drive the lexer without materializing the
/// whole `Vec<Token>` — e.g. `for tok in Lexer::new(src) { … }`.
impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.finished {
            return None;
        }
        let token = self.read_next_token();
        if token.kind == Kind::Eof {
            self.finished = true;
        }
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_functions_registry() {
        // Dominant #58 offenders must be recognized.
        for name in [
            "length",
            "entry",
            "substring",
            "trim",
            "round",
            "num-entries",
            "string",
            "available",
            // #58 second-pass residuals (unreserved abbrevs + VALUE phrase)
            "max",
            "min",
            "abs",
            "value",
            "maximum",
            "minimum",
            "absolute",
        ] {
            assert!(is_builtin_function(name), "`{name}` should be a built-in");
        }
        // Non-functions must not be.
        assert!(!is_builtin_function("frobnicate"));
        assert!(!is_builtin_function("define"));
    }

    #[test]
    fn builtin_functions_slice_is_sorted() {
        // `is_builtin_function` relies on `binary_search`, which requires the
        // generated slice to be sorted with no duplicates.
        assert!(
            BUILTIN_FUNCTIONS.windows(2).all(|w| w[0] < w[1]),
            "BUILTIN_FUNCTIONS must be sorted and deduped for binary_search"
        );
        // No entry may contain whitespace: ABL identifiers never do, so a
        // space signals a multi-word documentation phrase leaked into the
        // registry (an unmatchable junk entry). Every entry must also be
        // ASCII-lowercased, since callers pass case-folded atoms.
        assert!(
            BUILTIN_FUNCTIONS
                .iter()
                .all(|n| !n.contains(char::is_whitespace)),
            "BUILTIN_FUNCTIONS entries must not contain whitespace"
        );
        assert!(
            BUILTIN_FUNCTIONS
                .iter()
                .all(|n| *n == n.to_ascii_lowercase()),
            "BUILTIN_FUNCTIONS entries must be ASCII-lowercased"
        );
    }

    #[test]
    fn builtin_abbreviations_are_registered() {
        // Reserved-keyword built-in functions may be called by any prefix down
        // to their documented minimum abbreviation. Each entry below is
        // (min_abbreviation, full_name) for every such function; both ends of
        // the prefix range must resolve.
        let abbreviable: &[(&str, &str)] = &[
            ("avail", "available"),
            ("ambig", "ambiguous"),
            ("dbrest", "dbrestrictions"),
            ("dbvers", "dbversion"),
            ("gateway", "gateways"),
            ("is-attr", "is-attr-space"),
            ("is-lead", "is-lead-byte"),
            ("keyfunc", "keyfunction"),
            ("line-count", "line-counter"),
            ("num-ali", "num-aliases"),
            ("page-num", "page-number"),
            ("proc-ha", "proc-handle"),
            ("proc-st", "proc-status"),
            ("provers", "proversion"),
            ("setuser", "setuserid"),
            ("term", "terminal"),
            // Data type conversion functions are reserved keywords too.
            ("dec", "decimal"),
            ("int", "integer"),
            ("log", "logical"),
        ];
        for (abbrev, full) in abbreviable {
            assert!(
                is_builtin_function(abbrev),
                "min-abbreviation `{abbrev}` should be a built-in"
            );
            assert!(
                is_builtin_function(full),
                "full name `{full}` should be a built-in"
            );
        }
        // Note: `is-attr` (IS-ATTR-SPACE) and `is-lead` (IS-LEAD-BYTE) are
        // distinct shortest forms, not a shared prefix — both must resolve.

        // Unreserved function abbreviations (not on the reserved-keyword list,
        // so expansion comes from the curated UNRESERVED_FUNCTION_ABBREVS table).
        let unreserved_abbreviable: &[(&str, &str)] = &[
            ("abs", "absolute"),
            ("max", "maximum"),
            ("min", "minimum"),
            ("substr", "substring"),
            ("os-drive", "os-drives"),
            ("return-val", "return-value"),
            ("subst", "substitute"),
            ("trunc", "truncate"),
        ];
        for (abbrev, full) in unreserved_abbreviable {
            assert!(
                is_builtin_function(abbrev),
                "unreserved min-abbreviation `{abbrev}` should be a built-in"
            );
            assert!(
                is_builtin_function(full),
                "full name `{full}` should be a built-in"
            );
        }

        // Fragments below the minimum abbreviation must NOT resolve.
        for too_short in [
            "avai", "ambi", "is-att", "ter", "de", "in", "lo", "ab", "ma", "mi",
        ] {
            assert!(
                !is_builtin_function(too_short),
                "below-minimum fragment `{too_short}` must not be a built-in"
            );
        }
    }

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
            (Kind::Variable, 4, 7, TokenValue::None), // var
            (Kind::Identifier, 8, 15, TokenValue::None), // myCount
            (Kind::KwAs, 16, 18, TokenValue::None),
            (Kind::Integer, 19, 22, TokenValue::None), // int
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
            // TERM has its own reserved-word row in the keyword index but is
            // just TERMINAL's minimum abbreviation; the standalone entry is
            // removed via keyword_overrides.toml so the prefix wins.
            ("term", Kind::Terminal, 4),
            ("termi", Kind::Terminal, 5),
            ("terminal", Kind::Terminal, 8),
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
    fn string_with_literal_backslashes() {
        // Backslash is a literal byte in ABL strings — not an escape.
        // Windows path fragments like "\\" must tokenize as one literal
        // covering exactly the two-backslash payload plus the quotes.
        let source = r#""\\""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::StringLiteral);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 4);
    }

    #[test]
    fn string_with_backslashes_in_path() {
        // Paths with literal backslashes tokenize as a single literal and
        // terminate at the first quote (since `\` has no escape meaning).
        let source = r#""c:\path\to\file""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::StringLiteral);
        assert_eq!(tokens[0].end, source.len());
    }

    #[test]
    fn string_with_tilde_escaped_quotes() {
        // Tilde is ABL's official string escape. HTML/JS snippets that need
        // embedded quotes must use `~"` — e.g. `"<td style=~"x~">"` — and
        // tokenize as a single string literal spanning the whole source.
        let source = r#""<td style=~"x~">""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::StringLiteral);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, source.len());
    }

    #[test]
    fn string_with_html_backslash_escaped_quotes() {
        // Real ABL corpora embed HTML/JS snippets with `\"` as an escaped
        // quote (e.g. `"<td style=\"x\">"`). Because the leading `\` is
        // preceded by a non-backslash character, it tokenizes as a single
        // literal spanning the whole source.
        let source = r#""<td style=\"x\">""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::StringLiteral);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, source.len());
    }

    #[test]
    fn string_backslash_before_quote_terminates_when_next_is_expr() {
        // `"/\")` — the `"` after `\` is the string terminator because the
        // following char `)` is an expression terminator. This covers the
        // legacy-path-argument pattern `right-trim(x, "/\")`.
        let source = r#""/\")"#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens[0].kind, Kind::StringLiteral);
        assert_eq!(
            tokens[0].end, 4,
            "`\"/\\\\\"` closes at offset 4 before `)`"
        );
        assert_eq!(tokens[1].kind, Kind::RightParen);
    }

    #[test]
    fn string_with_double_backslash_then_escaped_quote() {
        // `"\\\"x"` — first `\\` is two literal backslashes (the second is
        // preceded by a backslash, so no escape), then `\"` is an escaped
        // quote because the backslash is preceded by `\` — wait no. The
        // key invariant: a run of N backslashes followed by `"` terminates
        // the string iff N is even (every `\` pairs up as literal). Our
        // toggling implementation handles the common even/odd cases that
        // appear in the legacy corpus: `"\\"` closes, `"\""` escapes.
        let source = r#""\\""#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].end, 4, "`\"\\\\\"` should close at offset 4");
    }

    #[test]
    fn comments_line() {
        let source = "def // this is a comment\nvar";
        let tokens = collect_tokens(source);
        // def, comment, var, eof
        assert_eq!(tokens.len(), 4, "Got: {:?}", tokens);
        assert_token(&tokens[0], Kind::Define, 0, 3, TokenValue::None, source);
        assert_token(&tokens[1], Kind::Comment, 4, 25, TokenValue::None, source);
        assert_token(&tokens[2], Kind::Variable, 25, 28, TokenValue::None, source); // var
    }

    #[test]
    fn comments_block() {
        let source = "def /* block */ var";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4, "Got: {:?}", tokens);
        assert_token(&tokens[0], Kind::Define, 0, 3, TokenValue::None, source);
        assert_token(&tokens[1], Kind::Comment, 4, 15, TokenValue::None, source);
        assert_token(&tokens[2], Kind::Variable, 16, 19, TokenValue::None, source); // var
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
            (Kind::Procedure, 77, 86, TokenValue::None), // procedure (not reserved)
            (Kind::Identifier, 87, 99, TokenValue::None), // my_test_proc
            (Kind::Colon, 99, 100, TokenValue::None),
            // var int MyInt = 1.
            (Kind::Variable, 104, 107, TokenValue::None), // var
            (Kind::Integer, 108, 111, TokenValue::None),  // int
            (Kind::Identifier, 112, 117, TokenValue::None), // MyInt
            (Kind::Equals, 118, 119, TokenValue::None),
            (Kind::IntegerLiteral, 120, 121, TokenValue::Integer(1)),
            (Kind::Period, 121, 122, TokenValue::None),
            // var int MyOtherInt = 2.
            (Kind::Variable, 126, 129, TokenValue::None), // var
            (Kind::Integer, 130, 133, TokenValue::None),  // int
            (Kind::Identifier, 134, 144, TokenValue::None), // MyOtherInt
            (Kind::Equals, 145, 146, TokenValue::None),
            (Kind::IntegerLiteral, 147, 148, TokenValue::Integer(2)),
            (Kind::Period, 148, 149, TokenValue::None),
            // var int result.
            (Kind::Variable, 153, 156, TokenValue::None), // var
            (Kind::Integer, 157, 160, TokenValue::None),  // int
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
            (Kind::PreprocEnd, 46, 47, TokenValue::None), // newline ends directive
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
        // Test that preprocessor abbreviations work like regular keywords.
        // Define directives (&glob, &scop, &global-define, &scoped-define) set
        // in_directive mode, so they emit PreprocEnd at EOF → 3 tokens total.
        // Non-define directives (&undef*) don't → 2 tokens total.
        let define_cases = vec![
            ("&glob", Kind::PreprocGlobalDefine, 5),
            ("&global-define", Kind::PreprocGlobalDefine, 14),
            ("&scop", Kind::PreprocScopedDefine, 5),
            ("&scoped-define", Kind::PreprocScopedDefine, 14),
        ];
        let non_define_cases = vec![
            ("&undef", Kind::PreprocUndefine, 6),
            ("&undefine", Kind::PreprocUndefine, 9),
        ];

        for (source, expected_kind, expected_len) in define_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                3,
                "Expected 3 tokens for '{}' (directive + PreprocEnd + Eof), got {}",
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
            assert_eq!(tokens[1].kind, Kind::PreprocEnd);
            assert_eq!(tokens[2].kind, Kind::Eof);
        }

        for (source, expected_kind, expected_len) in non_define_cases {
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
    fn string_with_translation_suffix() {
        // ABL allows :U (untranslatable) and :T (translatable) suffixes on string literals
        let source = r#""adjust":U"#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::StringLiteral,
            0,
            10,
            TokenValue::String(OxablAtom::from("adjust".to_string())),
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
        let source = "10 mod 3";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 4);
        assert_token(&tokens[1], Kind::Modulo, 3, 6, TokenValue::None, source);
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
        assert_eq!(tokens[1].kind, Kind::TempTable); // temp-table is a keyword
    }

    // =========================================================================
    // Lock Type Tests
    // =========================================================================

    #[test]
    fn lock_type_hyphenated() {
        // Hyphenated forms should produce single tokens
        let test_cases = vec![
            ("NO-LOCK", Kind::NoLock),
            ("no-lock", Kind::NoLock),
            ("SHARE-LOCK", Kind::ShareLock),
            ("share-lock", Kind::ShareLock),
            ("EXCLUSIVE-LOCK", Kind::ExclusiveLock),
            ("exclusive-lock", Kind::ExclusiveLock),
        ];

        for (source, expected_kind) in test_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}",
                source,
                tokens.len()
            );
            assert_eq!(
                tokens[0].kind, expected_kind,
                "Wrong kind for '{}': expected {:?}, got {:?}",
                source, expected_kind, tokens[0].kind
            );
            assert_eq!(tokens[0].start, 0, "Wrong start for '{}'", source);
            assert_eq!(tokens[0].end, source.len(), "Wrong end for '{}'", source);
        }
    }

    #[test]
    fn lock_type_space_separated() {
        // Space-separated forms should produce single tokens
        let test_cases = vec![
            ("NO LOCK", Kind::NoLock, 7),
            ("no lock", Kind::NoLock, 7),
            ("SHARE LOCK", Kind::ShareLock, 10),
            ("share lock", Kind::ShareLock, 10),
            ("EXCLUSIVE LOCK", Kind::ExclusiveLock, 14),
            ("exclusive lock", Kind::ExclusiveLock, 14),
        ];

        for (source, expected_kind, expected_end) in test_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}: {:?}",
                source,
                tokens.len(),
                tokens
            );
            assert_eq!(
                tokens[0].kind, expected_kind,
                "Wrong kind for '{}': expected {:?}, got {:?}",
                source, expected_kind, tokens[0].kind
            );
            assert_eq!(tokens[0].start, 0, "Wrong start for '{}'", source);
            assert_eq!(
                tokens[0].end, expected_end,
                "Wrong end for '{}': expected {}, got {}",
                source, expected_end, tokens[0].end
            );
        }
    }

    #[test]
    fn lock_type_multiple_spaces() {
        // Multiple spaces between words should still work
        let source = "NO    LOCK";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, Kind::NoLock);
        assert_eq!(tokens[0].end, 10);
    }

    #[test]
    fn lock_type_with_tabs() {
        // Tabs between words should work
        let source = "SHARE\tLOCK";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, Kind::ShareLock);
    }

    #[test]
    fn lock_type_newline_does_not_match() {
        // Newline between words should NOT match as lock type
        let source = "NO\nLOCK";
        let tokens = collect_tokens(source);
        // Bare NO is the boolean-literal keyword (Kind::No); LOCK alone is not a keyword.
        // Space-separated NO LOCK only matches when whitespace is horizontal (no newline).
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].kind, Kind::No);
        assert_eq!(tokens[1].kind, Kind::Identifier); // LOCK alone is not a keyword
    }

    #[test]
    fn lock_type_standalone_words_are_identifiers() {
        // "share" alone (without -lock suffix) is an identifier.
        // Bare "no"/"NO" is Kind::No (boolean literal); NO LOCK still uses
        // space-separated lock lookahead before keyword matching.
        let identifier_cases = vec!["share", "SHARE"];
        for source in identifier_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}",
                source,
                tokens.len()
            );
            assert_eq!(
                tokens[0].kind,
                Kind::Identifier,
                "Expected Identifier for '{}', got {:?}",
                source,
                tokens[0].kind
            );
        }
        for source in ["no", "NO"] {
            let tokens = collect_tokens(source);
            assert_eq!(tokens.len(), 2);
            assert_eq!(
                tokens[0].kind,
                Kind::No,
                "Expected Kind::No for '{}', got {:?}",
                source,
                tokens[0].kind
            );
        }

        // "exclusive" / "EXCLUSIVE" alone is a valid abbreviation for EXCLUSIVE-LOCK (ABL standard)
        for source in ["exclusive", "EXCLUSIVE"] {
            let tokens = collect_tokens(source);
            assert_eq!(tokens.len(), 2);
            assert_eq!(
                tokens[0].kind,
                Kind::ExclusiveLock,
                "Expected ExclusiveLock for '{}', got {:?}",
                source,
                tokens[0].kind
            );
        }
    }

    #[test]
    fn lock_type_in_for_each_context() {
        // Test lock types in a FOR EACH statement context
        let source = "for each customer no-lock:";
        let tokens = collect_tokens(source);
        // for, each, customer, no-lock, :, eof
        assert_eq!(tokens.len(), 6, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::KwFor);
        assert_eq!(tokens[1].kind, Kind::Each);
        assert_eq!(tokens[2].kind, Kind::Identifier); // customer
        assert_eq!(tokens[3].kind, Kind::NoLock);
        assert_eq!(tokens[4].kind, Kind::Colon);
    }

    #[test]
    fn lock_type_space_in_for_each_context() {
        // Test space-separated lock types in FOR EACH
        let source = "for each customer no lock:";
        let tokens = collect_tokens(source);
        // for, each, customer, no lock, :, eof
        assert_eq!(tokens.len(), 6, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::KwFor);
        assert_eq!(tokens[1].kind, Kind::Each);
        assert_eq!(tokens[2].kind, Kind::Identifier); // customer
        assert_eq!(tokens[3].kind, Kind::NoLock);
        assert_eq!(tokens[4].kind, Kind::Colon);
    }

    #[test]
    fn lock_type_followed_by_other_tokens() {
        // Ensure lock type doesn't consume more than it should
        let source = "no-lock where x = 1";
        let tokens = collect_tokens(source);
        // no-lock, where, x, =, 1, eof
        assert_eq!(tokens.len(), 6, "Got: {:?}", tokens);
        assert_eq!(tokens[0].kind, Kind::NoLock);
        assert_eq!(tokens[1].kind, Kind::KwWhere);
    }

    #[test]
    fn lock_type_case_insensitive() {
        // Mixed case should work
        let test_cases = vec![
            ("No Lock", Kind::NoLock),
            ("nO lOcK", Kind::NoLock),
            ("Share Lock", Kind::ShareLock),
            ("Exclusive Lock", Kind::ExclusiveLock),
        ];

        for (source, expected_kind) in test_cases {
            let tokens = collect_tokens(source);
            assert_eq!(
                tokens.len(),
                2,
                "Expected 2 tokens for '{}', got {}",
                source,
                tokens.len()
            );
            assert_eq!(tokens[0].kind, expected_kind, "Wrong kind for '{}'", source);
        }
    }

    // ==================== Include File Reference Tests ====================

    #[test]
    fn include_simple_file() {
        let source = "{file.i}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2); // IncludeReference + Eof
        assert_token(
            &tokens[0],
            Kind::IncludeReference,
            0,
            8,
            TokenValue::String(OxablAtom::from("file.i")),
            source,
        );
    }

    #[test]
    fn include_file_with_path() {
        let source = "{mod/subdir/file.i}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeReference,
            0,
            19,
            TokenValue::String(OxablAtom::from("mod/subdir/file.i")),
            source,
        );
    }

    #[test]
    fn include_file_with_positional_args() {
        let source = "{file.i NEW shared}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeReference,
            0,
            19,
            TokenValue::String(OxablAtom::from("file.i NEW shared")),
            source,
        );
    }

    #[test]
    fn include_file_with_named_args() {
        let source = r#"{file.i &name="value" &other=test}"#;
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeReference,
            0,
            34,
            TokenValue::String(OxablAtom::from(r#"file.i &name="value" &other=test"#)),
            source,
        );
    }

    #[test]
    fn include_file_whitespace_trimmed() {
        let source = "{ file.i }";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeReference,
            0,
            10,
            TokenValue::String(OxablAtom::from("file.i")),
            source,
        );
    }

    #[test]
    fn include_arg_reference_zero() {
        let source = "{0}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeArgReference,
            0,
            3,
            TokenValue::Integer(0),
            source,
        );
    }

    #[test]
    fn include_arg_reference_one() {
        let source = "{1}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeArgReference,
            0,
            3,
            TokenValue::Integer(1),
            source,
        );
    }

    #[test]
    fn include_arg_reference_multi_digit() {
        let source = "{10}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_token(
            &tokens[0],
            Kind::IncludeArgReference,
            0,
            4,
            TokenValue::Integer(10),
            source,
        );
    }

    #[test]
    fn include_preprop_regression() {
        // Ensure {&var} still works as Preprop
        let source = "{&myvar}";
        let tokens = collect_tokens(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, Kind::Preprop);
    }

    #[test]
    fn include_unterminated() {
        let source = "{file.i";
        let tokens = collect_tokens(source);
        assert_eq!(tokens[0].kind, Kind::Invalid);
    }

    #[test]
    fn include_in_statement_context() {
        // {file.i} followed by a period (statement terminator)
        let source = "{globals/globals.i} message \"hello\".";
        let tokens = collect_tokens(source);
        assert_eq!(tokens[0].kind, Kind::IncludeReference);
        assert_eq!(
            tokens[0].value,
            TokenValue::String(OxablAtom::from("globals/globals.i"))
        );
        assert_eq!(tokens[1].kind, Kind::Message);
    }

    #[test]
    fn include_arg_in_define() {
        // Simulates: DEF {1} SHARED TEMP-TABLE ...
        let source = "def {1} shared";
        let tokens = collect_tokens(source);
        assert_eq!(tokens[0].kind, Kind::Define);
        assert_eq!(tokens[1].kind, Kind::IncludeArgReference);
        assert_eq!(tokens[1].value, TokenValue::Integer(1));
        assert_eq!(tokens[2].kind, Kind::Shared);
    }

    // ── PreprocEnd / in_directive tests ──────────────────────────────

    #[test]
    fn scoped_define_emits_preproc_end_on_newline() {
        // "&scoped-define FOO 42\n" should emit:
        //   PreprocScopedDefine, Identifier(FOO), IntegerLiteral(42), PreprocEnd, Eof
        let source = "&scoped-define FOO 42\nDISPLAY.";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocScopedDefine,
                Kind::Identifier,     // FOO
                Kind::IntegerLiteral, // 42
                Kind::PreprocEnd,
                Kind::Display,
                Kind::Period,
                Kind::Eof,
            ]
        );
    }

    #[test]
    fn global_define_emits_preproc_end_on_newline() {
        let source = "&global-define BAR yes\nRUN foo.";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocGlobalDefine,
                Kind::Identifier, // BAR
                Kind::Yes,        // yes is a keyword
                Kind::PreprocEnd,
                Kind::Run,
                Kind::Identifier, // foo
                Kind::Period,
                Kind::Eof,
            ]
        );
    }

    #[test]
    fn define_value_with_operators() {
        let source = "&scoped-define EXPR 1 + 2\nEND.";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocScopedDefine,
                Kind::Identifier,     // EXPR
                Kind::IntegerLiteral, // 1
                Kind::Add,            // +
                Kind::IntegerLiteral, // 2
                Kind::PreprocEnd,
                Kind::End,
                Kind::Period,
                Kind::Eof,
            ]
        );
    }

    #[test]
    fn define_with_no_value_emits_preproc_end() {
        let source = "&scoped-define EMPTY\nEND.";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocScopedDefine,
                Kind::Empty, // EMPTY is now a keyword
                Kind::PreprocEnd,
                Kind::End,
                Kind::Period,
                Kind::Eof,
            ]
        );
    }

    #[test]
    fn define_at_eof_emits_preproc_end() {
        // No trailing newline — PreprocEnd should still be emitted before Eof
        let source = "&scoped-define NAME hello";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocScopedDefine,
                Kind::Identifier, // NAME
                Kind::Identifier, // hello
                Kind::PreprocEnd,
                Kind::Eof,
            ]
        );
    }

    #[test]
    fn non_define_preproc_does_not_emit_preproc_end() {
        // &IF should NOT set in_directive — no PreprocEnd expected
        let source = "&IF TRUE &THEN\nDISPLAY.";
        let tokens = collect_tokens(source);

        let kinds: Vec<Kind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                Kind::PreprocIf,
                Kind::KwTrue,
                Kind::PreprocThen,
                Kind::Display,
                Kind::Period,
                Kind::Eof,
            ]
        );
    }
}

#[cfg(test)]
mod iterator_tests {
    use super::*;

    #[test]
    fn iterator_matches_tokenize() {
        for src in [
            "MESSAGE \"hello\".",
            "",
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nFOR EACH cust: DISPLAY cust. END.",
            "&SCOPED-DEFINE FOO 1\nMESSAGE {&FOO}.",
        ] {
            let batch = tokenize(src);
            let streamed: Vec<Token> = Lexer::new(src).collect();
            assert_eq!(
                streamed, batch,
                "iterator diverged from tokenize on {src:?}"
            );
        }
    }

    #[test]
    fn iterator_stops_after_eof() {
        let mut lexer = Lexer::new("MESSAGE \"x\".");
        let mut saw_eof = false;
        for tok in lexer.by_ref() {
            if tok.kind == Kind::Eof {
                saw_eof = true;
            }
        }
        assert!(saw_eof, "iteration should yield the terminal Eof token");
        // Exhausted: no tokens past Eof.
        assert!(lexer.next().is_none());
    }

    #[test]
    fn partial_consumption_is_lazy() {
        // Taking the first two tokens must not require lexing the whole input.
        let first_two: Vec<Token> = Lexer::new("MESSAGE \"x\". DISPLAY y.").take(2).collect();
        assert_eq!(first_two.len(), 2);
        assert_eq!(first_two[0].kind, tokenize("MESSAGE \"x\".")[0].kind);
    }
}
