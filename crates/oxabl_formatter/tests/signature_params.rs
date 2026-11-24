//! Already-formatted signatures keep their layout: the parameters of a
//! `METHOD`, `CONSTRUCTOR` or `FUNCTION` header and of a property
//! `SET (...)` belong to the header line, not to the body. Continuation lines
//! stay aligned under the first parameter, and comments around them stay put.

use oxabl_formatter::format;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_style::StyleGuide;

const SIZES: [usize; 2] = [3, 4];

fn style(size: usize) -> StyleGuide {
    StyleGuide {
        indent_size: size,
        ..StyleGuide::default_base()
    }
}

fn run(src: &str, style: &StyleGuide) -> String {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    assert!(program.is_ok(), "fixture must parse: {:?}", program.errors);
    format(src, &program, style).unwrap_or_else(|e| panic!("unexpected bail: {e}\n{src}"))
}

/// `template` is already formatted at every indent size: it must come back
/// unchanged. `\t` marks one indent level and `{N}` N columns of alignment
/// added to the indentation before it.
fn assert_stable(template: &str) {
    for size in SIZES {
        let src = expand(template, size);
        let out = run(&src, &style(size));
        assert_eq!(out, src, "reformatted at indent {size}:\n{src}");
    }
}

fn expand(template: &str, size: usize) -> String {
    let mut out = String::new();
    let mut rest = template;
    while let Some(i) = rest.find(['\t', '{']) {
        out.push_str(&rest[..i]);
        if rest.as_bytes()[i] == b'\t' {
            out.push_str(&" ".repeat(size));
            rest = &rest[i + 1..];
        } else {
            let end = rest[i..].find('}').expect("unclosed alignment") + i;
            let n: usize = rest[i + 1..end].parse().expect("alignment width");
            out.push_str(&" ".repeat(n));
            rest = &rest[end + 1..];
        }
    }
    out.push_str(rest);
    out
}

#[test]
fn constructor_and_set_accessor_parameters_stay_in_the_header() {
    // `CONSTRUCTOR PUBLIC Foo(` is 23 columns wide.
    assert_stable(
        "CLASS Foo:\n\
         \tCONSTRUCTOR PUBLIC Foo(INPUT a AS INTEGER,\n\
         \t{23}INPUT b AS INTEGER):\n\
         \tEND CONSTRUCTOR.\n\
         \tDEFINE PUBLIC PROPERTY P AS CHARACTER NO-UNDO\n\
         \tGET.\n\
         \tSET (INPUT pv AS CHARACTER):\n\
         \tEND SET.\n\
         END CLASS.\n",
    );
}

#[test]
fn method_parameters_with_comments_stay_in_the_header() {
    // `METHOD PUBLIC VOID M(` is 21 columns wide.
    assert_stable(
        "CLASS Foo:\n\
         \tMETHOD PUBLIC VOID M(INPUT a AS INTEGER, /* first */\n\
         \t{21}INPUT b AS INTEGER, // second\n\
         \t{21}OUTPUT c AS CHARACTER):\n\
         \t\tMESSAGE 1.\n\
         \tEND METHOD.\n\
         END CLASS.\n",
    );
}

// A known limitation, kept as master has it: an own-line comment inside a
// parameter list moves to the body depth, so `format --check` reports an
// aligned list that holds one.
#[test]
fn own_line_comment_in_a_parameter_list_moves_to_the_body_depth() {
    let src = expand(
        "CLASS Foo:\n\
         \tMETHOD PUBLIC VOID M(INPUT a AS INTEGER,\n\
         \t{21}/* own line */\n\
         \t{21}INPUT b AS INTEGER):\n\
         \t\tMESSAGE 1.\n\
         \tEND METHOD.\n\
         END CLASS.\n",
        4,
    );
    let expected = expand(
        "CLASS Foo:\n\
         \tMETHOD PUBLIC VOID M(INPUT a AS INTEGER,\n\
         \t\t/* own line */\n\
         \t{21}INPUT b AS INTEGER):\n\
         \t\tMESSAGE 1.\n\
         \tEND METHOD.\n\
         END CLASS.\n",
        4,
    );
    assert_eq!(run(&src, &style(4)), expected);
}

#[test]
fn function_parameters_stay_in_the_header() {
    // `FUNCTION f RETURNS INTEGER (` is 28 columns wide.
    assert_stable(
        "FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER,\n\
         {28}INPUT b AS INTEGER):\n\
         \tRETURN 1.\n\
         END FUNCTION.\n",
    );
}
