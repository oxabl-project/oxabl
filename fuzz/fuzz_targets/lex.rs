#![no_main]

use libfuzzer_sys::fuzz_target;
use oxabl_lexer::{Kind, tokenize};

// Lexing never panics, ends with one Eof, and its spans walk the source in order.
fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    let tokens = tokenize(&source);
    let last = tokens.last().expect("tokenize returns at least Eof");
    assert_eq!(last.kind, Kind::Eof);
    let mut at = 0;
    for token in &tokens {
        assert!(token.start <= token.end && token.end <= source.len());
        assert!(token.start >= at, "token starts before the previous one ends");
        assert!(source.is_char_boundary(token.start) && source.is_char_boundary(token.end));
        at = token.end;
    }
});
