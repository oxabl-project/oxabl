//! Safe-default guarantees (U9 / R6.2 / R6.4 / S4).
//!
//! A curated conforming file formats to itself byte-for-byte, and no fixture's
//! identifiers or keywords are altered under `default_base()` — the safe first
//! pass only tidies layout.

use std::fs;
use std::path::PathBuf;

use oxabl_formatter::format;
use oxabl_lexer::{Kind, tokenize};
use oxabl_parser::{Parser, Program};
use oxabl_style::StyleGuide;

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

/// The verbatim text of every non-trivia token (comments/EOF dropped).
fn token_texts(src: &str) -> Vec<String> {
    tokenize(src)
        .into_iter()
        .filter(|t| t.kind != Kind::Comment && t.kind != Kind::Eof)
        .map(|t| src[t.start..t.end].to_string())
        .collect()
}

fn fixtures() -> Vec<(String, String)> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let mut out = Vec::new();
    for entry in fs::read_dir(&dir).expect("fixtures dir") {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) == Some("p") {
            let name = path.file_name().unwrap().to_string_lossy().into_owned();
            out.push((name, fs::read_to_string(&path).unwrap()));
        }
    }
    out
}

#[test]
fn curated_clean_file_is_a_fixpoint() {
    // Already in normal form under default_base: 4-space structural indent,
    // single blank between sections, bare ENDs, keywords as written.
    let clean = "\
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

DO iCount = 1 TO 3:
    MESSAGE iCount.
END.
";
    let program = parse(clean);
    assert!(program.is_ok());
    let out = format(clean, &program, &StyleGuide::default_base()).unwrap();
    assert_eq!(out, clean, "a conforming file must format to itself (S4)");
}

#[test]
fn default_base_never_mangles_identifiers_or_keywords() {
    for (name, src) in fixtures() {
        let program = parse(&src);
        assert!(program.is_ok(), "{name} parse: {:?}", program.errors);
        let out = format(&src, &program, &StyleGuide::default_base()).unwrap();
        assert_eq!(
            token_texts(&src),
            token_texts(&out),
            "{name}: default_base changed a token's text (mangled identifier/keyword)"
        );
    }
}
