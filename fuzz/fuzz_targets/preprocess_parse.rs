#![no_main]

use std::path::PathBuf;

use libfuzzer_sys::fuzz_target;
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

// The raw parse and the expanded parse (preprocess, then lex and parse) never panic.
fuzz_target!(|data: &[u8]| {
    oxabl_fuzz::on_analysis_stack(data, check);
});

fn check(data: &[u8]) {
    let source = String::from_utf8_lossy(data);

    let tokens = tokenize(&source);
    let _ = Parser::new(&tokens, &source).parse_program();

    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/inc/a.i"),
        "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n&SCOPED-DEFINE X 1\n",
    );
    fs.insert(PathBuf::from("/inc/b.i"), "{a.i}\n{1} {2} {*} {&X}\n");
    fs.insert(PathBuf::from("/inc/loop.i"), "{loop.i}\n");
    let include_paths = vec![PathBuf::from("/inc")];
    let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
    if let Ok(expanded) = pp.process(FileId::new(1), &source) {
        let text = expanded.to_text();
        let tokens = tokenize(&text);
        let _ = Parser::new(&tokens, &text).parse_program();
    }
}
