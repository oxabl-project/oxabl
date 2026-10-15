//! End-to-end coverage for the built-in function registry audit.
//!
//! Built-ins that read as a value (`GET-SIZE(m)`) and as an assignment target
//! (`SET-SIZE(m) = 16`) must both resolve, and an assignment through a
//! pseudo-function must credit its operand with the access it makes.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// Full pipeline, returning `(code, message)` per diagnostic.
fn lint_all(source: &str) -> Vec<(String, String)> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema)
        .with_lint_severities(LintSeverityMap::new());
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0.to_string(), d.message))
        .collect()
}

fn codes(diags: &[(String, String)], code: &str) -> usize {
    diags.iter().filter(|(c, _)| c == code).count()
}

const DECLS: &str = "\
DEFINE VARIABLE mp AS MEMPTR NO-UNDO.
DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ia AS INTEGER EXTENT NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
";

#[test]
fn assignable_pseudo_functions_resolve_as_targets() {
    for stmt in [
        "FIX-CODEPAGE(lc) = \"UTF-8\".",
        "SET-SIZE(mp) = 16.",
        "PUT-BYTE(mp, 1) = 65.",
        "PUT-BYTES(mp, 1) = mp.",
        "PUT-STRING(mp, 1) = \"x\".",
        "PUT-SHORT(mp, 1) = 1.",
        "PUT-LONG(mp, 1) = 1.",
        "PUT-FLOAT(mp, 1) = 1.",
        "PUT-DOUBLE(mp, 1) = 1.",
        "PUT-BITS(mp, 1, 2) = 1.",
        "PUT-INT64(mp, 1) = 1.",
        "PUT-UNSIGNED-SHORT(mp, 1) = 1.",
        "PUT-UNSIGNED-LONG(mp, 1) = 1.",
        "SET-BYTE-ORDER(mp) = BIG-ENDIAN.",
        "SET-BYTE-ORDER(mp) = LITTLE-ENDIAN.",
        "SET-BYTE-ORDER(mp) = HOST-BYTE-ORDER.",
        "SET-POINTER-VALUE(mp) = 0.",
        "ENTRY(1, c) = \"a\".",
        "SUBSTRING(c, 1, 2) = \"ab\".",
        "SUBSTR(c, 1, 2) = \"ab\".",
        "OVERLAY(c, 1, 2) = \"ab\".",
        "OVERLAY(c, 1, 2, \"CHARACTER\") = \"ab\".",
        "LENGTH(c) = 3.",
        "EXTENT(ia) = 5.",
    ] {
        let diags = lint_all(&format!("{DECLS}{stmt}\n"));
        assert_eq!(codes(&diags, LINT0001), 0, "{stmt}: {diags:?}");
    }
}

#[test]
fn value_pseudo_functions_resolve_as_values() {
    for stmt in [
        "i = GET-SIZE(mp) + GET-BYTE(mp, 1) + GET-SHORT(mp, 1).",
        "i = GET-BYTE-ORDER(mp).",
        "i = HOST-BYTE-ORDER.",
        "i = BIG-ENDIAN.",
        "i = LITTLE-ENDIAN.",
        "c = OVERLAY(c, 1, 2).",
        "c = SUBSTRING(c, 1, 2) + SUBSTR(c, 1).",
        "l = NEW(c).",
        "h = WIDGET-H(\"1\").",
        "h = WIDGET-HANDLE(\"1\").",
    ] {
        let diags = lint_all(&format!("{DECLS}{stmt}\n"));
        assert_eq!(codes(&diags, LINT0001), 0, "{stmt}: {diags:?}");
    }
}

#[test]
fn input_from_os_dir_resolves() {
    let src = "\
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE STREAM s.
INPUT FROM OS-DIR(\".\").
INPUT STREAM s FROM OS-DIR(\".\" + \"x\") NO-ECHO.
INPUT CLOSE.
";
    let diags = lint_all(src);
    assert_eq!(codes(&diags, LINT0001), 0, "{diags:?}");
}

#[test]
fn undefined_name_still_fires_beside_builtin_targets() {
    let diags = lint_all(&format!("{DECLS}SET-SIZE(mp) = ghost.\n"));
    assert_eq!(codes(&diags, LINT0001), 1, "{diags:?}");
}

#[test]
fn write_only_pseudo_function_target_is_a_dead_store() {
    for (stmt, var) in [
        ("FIX-CODEPAGE(lc) = \"UTF-8\".", "lc"),
        ("SET-SIZE(mp) = 16.", "mp"),
        ("PUT-BYTE(mp, 1) = 65.", "mp"),
    ] {
        let diags = lint_all(&format!("{DECLS}{stmt}\n"));
        let needle = format!("`{var}`");
        assert!(
            diags
                .iter()
                .any(|(code, msg)| code == LINT0006 && msg.contains(&needle)),
            "{stmt}: expected a dead store for {var}: {diags:?}"
        );
    }
}

#[test]
fn partial_assignment_target_is_also_read() {
    // `SUBSTRING(c, ...) = v` keeps the rest of `c`, so `c` is not a dead store.
    for stmt in [
        "SUBSTRING(c, 1, 2) = \"ab\".",
        "OVERLAY(c, 1, 2) = \"ab\".",
        "ENTRY(1, c) = \"a\".",
        "LENGTH(c) = 3.",
    ] {
        let diags = lint_all(&format!("{DECLS}{stmt}\n"));
        assert!(
            !diags
                .iter()
                .any(|(code, msg)| code == LINT0006 && msg.contains("`c`")),
            "{stmt}: {diags:?}"
        );
    }
}
