//! The body scan on texts that stress its jumps to the next byte it acts on:
//! comments and strings that do not end, `~` at the end, nested comments,
//! braces in strings, CRLF and multi-byte characters beside trigger bytes. All
//! fixtures are synthetic ABL.

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_preprocessor::{PreprocessedFile, Preprocessor};
use oxabl_workspace::InMemoryFileSystem;

fn run(source: &str) -> PreprocessedFile {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/inc/in.i"), "INC");
    let paths = vec![PathBuf::from("/inc")];
    Preprocessor::new(&fs, &paths)
        .process(FileId::new(1), source)
        .unwrap()
}

/// The source with `&SCOPED-DEFINE v X` in front, and the text after it.
fn expand(body: &str) -> String {
    let result = run(&format!("&SCOPED-DEFINE v X\n{body}"));
    result.to_text().to_string()
}

#[track_caller]
fn expands_to(body: &str, expected: &str) {
    assert_eq!(expand(body), expected, "body {body:?}");
}

#[test]
fn a_reference_in_a_string_expands_and_an_escaped_one_does_not() {
    expands_to("y = \"a{&v}b~{&v}c\".", "y = \"aXb~{&v}c\".");
    expands_to("y = 'it\"s {&v}'.", "y = 'it\"s X'.");
    expands_to("y = \"say ~\"{&v}~\" now\".", "y = \"say ~\"X~\" now\".");
}

#[test]
fn a_string_that_never_ends_runs_to_the_end() {
    expands_to("y = \"open {&v} {&v}", "y = \"open X X");
    expands_to("y = '{&v}~", "y = 'X~");
    expands_to("y = \"~", "y = \"~");
    expands_to("\"", "\"");
}

#[test]
fn a_tilde_at_the_end_of_the_text_is_kept() {
    expands_to("a~", "a~");
    expands_to("{&v}~", "X~");
}

#[test]
fn a_reference_in_a_comment_stays_text() {
    expands_to(
        "/* {&v} /* {&v} */ {&v} */ {&v}",
        "/* {&v} /* {&v} */ {&v} */ X",
    );
    expands_to("// {&v} \" '\n{&v}", "// {&v} \" '\nX");
    expands_to("// {&v}\r\n{&v}\r\n", "// {&v}\r\nX\r\n");
}

#[test]
fn a_comment_that_never_ends_runs_to_the_end() {
    expands_to("/* {&v} ", "/* {&v} ");
    expands_to("/* a /* b */ {&v}", "/* a /* b */ {&v}");
    expands_to("// {&v}", "// {&v}");
    expands_to("/*", "/*");
    expands_to("{&v}/*", "X/*");
}

#[test]
fn comment_marks_that_overlap_are_read_left_to_right() {
    expands_to("/*/ {&v} */ {&v}", "/*/ {&v} */ X");
    expands_to("/**/ {&v}", "/**/ X");
    expands_to("/*/*/ {&v} */ {&v}", "/*/*/ {&v} */ {&v}");
    expands_to("a / b // c\nd /* e */ {&v}", "a / b // c\nd /* e */ X");
}

#[test]
fn a_quote_in_a_comment_does_not_open_a_string() {
    expands_to("/* it's */ {&v}", "/* it's */ X");
    expands_to("// it's\n{&v}", "// it's\nX");
    expands_to("{&v} /* \" */ {&v}", "X /* \" */ X");
}

#[test]
fn multi_byte_characters_beside_trigger_bytes() {
    expands_to("é{&v}é\"é{&v}é\"é{&v}", "éXé\"éXé\"éX");
    expands_to(
        "日本/* {&v} */日{&v}語'~語{&v}'",
        "日本/* {&v} */日X語'~語X'",
    );
    expands_to("é//é\né{&v}", "é//é\néX");
}

#[test]
fn a_file_include_in_a_string_stays_text_and_one_in_code_expands() {
    expands_to("\"{in.i}\" {in.i}", "\"{in.i}\" INC");
    expands_to("'{in.i}' // {in.i}\n{in.i}", "'{in.i}' // {in.i}\nINC");
}

#[test]
fn a_branch_that_is_not_taken_hides_text_but_keeps_the_scan_in_step() {
    expands_to(
        "&IF FALSE &THEN\n\"{&v}\" '{in.i}' /* &ENDIF */ // x\n&ENDIF\n{&v}",
        "X",
    );
    expands_to("&IF FALSE &THEN\n\"never ends {&v}\n&ENDIF\n{&v}", "");
}

#[test]
fn braces_close_at_the_first_unescaped_close() {
    expands_to("{in.i ~}} {&v}", "INC X");
    expands_to("{in.i '}' x} {&v}", "INC' x} X");
}

#[test]
fn a_run_of_open_braces_and_tildes_ends() {
    let source = format!("{}{}", "{".repeat(5000), "~{".repeat(5000));
    let _ = run(&source);
}
