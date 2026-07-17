//! End-to-end LINT0001 coverage for the #58 residual language gaps.
//!
//! Each test drives real source through the full pipeline
//! (tokenize → parse → analyze → lint) and asserts `undefined-symbol`
//! stays silent (or fires) exactly as the plan's repros demand.

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, undefined_symbol};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

fn lint0001(source: &str) -> Vec<String> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "parse errors for {source:?}: {:?}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    undefined_symbol::run(&program.statements, &sem, &ctx)
        .into_iter()
        .filter(|d| d.code.0 == LINT0001)
        .map(|d| d.message)
        .collect()
}

// ---------------------------------------------------------------------
// Item A — system handles
// ---------------------------------------------------------------------

#[test]
fn system_handle_member_access_is_silent() {
    for src in [
        "MESSAGE THIS-PROCEDURE:HANDLE.",
        "IF WEB-CONTEXT:IS-LOGGED-IN THEN MESSAGE \"in\".",
        "CURRENT-WINDOW:TITLE = \"x\".",
        "MESSAGE SESSION:BATCH-MODE.",
        "MESSAGE ERROR-STATUS:GET-MESSAGE(1).",
        "MESSAGE SOURCE-PROCEDURE:FILE-NAME.",
        "MESSAGE TARGET-PROCEDURE:FILE-NAME.",
        "MESSAGE LOG-MANAGER:LOGFILE-NAME.",
        "MESSAGE FILE-INFO:FULL-PATHNAME.",
        "MESSAGE AUDIT-CONTROL:APPL-CONTEXT-ID.",
        "MESSAGE SECURITY-POLICY:LOCKED.",
        "MESSAGE COM-SELF:Name.",
        "MESSAGE CLIPBOARD:VALUE.",
        "MESSAGE LAST-EVENT:LABEL.",
        "MESSAGE COLOR-TABLE:NUM-ENTRIES.",
        "MESSAGE FONT-TABLE:NUM-ENTRIES.",
        "MESSAGE RCODE-INFO:FILE-NAME.",
        "MESSAGE COMPILER:ERROR.",
        "MESSAGE DEBUGGER:VISIBLE.",
        "MESSAGE PROFILER:ENABLED.",
        "MESSAGE WEB-CONTEXT:EXCLUSIVE-ID.",
        "MESSAGE ACTIVE-WINDOW:TITLE.",
        "MESSAGE DEFAULT-WINDOW:TITLE.",
        "MESSAGE FOCUS:NAME.",
        "MESSAGE TERMINAL.",
    ] {
        let diags = lint0001(src);
        assert!(
            diags.is_empty(),
            "expected no LINT0001 for {src:?}: {diags:?}"
        );
    }
}

#[test]
fn undefined_symbol_still_fires_next_to_system_handles() {
    let diags = lint0001("MESSAGE THIS-PROCEDURE:HANDLE ghost.");
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(diags[0].contains("ghost"));
}

// ---------------------------------------------------------------------
// Item E1 — logical `no` literal
// ---------------------------------------------------------------------

#[test]
fn bare_no_as_boolean_is_silent() {
    let diags = lint0001("DEFINE VARIABLE f AS LOGICAL NO-UNDO.\nf = no.");
    assert!(diags.is_empty(), "{diags:?}");
}

// ---------------------------------------------------------------------
// Item E3 — SUBSTR builtin
// ---------------------------------------------------------------------

#[test]
fn substr_builtin_call_is_silent() {
    let diags = lint0001(r#"MESSAGE SUBSTR("abcdef", 1, 3)."#);
    assert!(diags.is_empty(), "{diags:?}");
}

// ---------------------------------------------------------------------
// Item C — QUERY handle syntax
// ---------------------------------------------------------------------

#[test]
fn query_handle_method_without_define_is_silent() {
    // Dynamic handle: no local qh — receiver softens to External.
    let diags = lint0001(r#"QUERY qh:QUERY-PREPARE("FOR EACH cust")."#);
    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn query_handle_resolves_when_variable_defined() {
    let diags = lint0001(
        r#"
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
QUERY qh:QUERY-PREPARE("FOR EACH cust").
"#,
    );
    assert!(diags.is_empty(), "{diags:?}");
}

// ---------------------------------------------------------------------
// Item D — static / package-qualified receivers
// ---------------------------------------------------------------------

#[test]
fn package_qualified_static_method_is_silent() {
    let diags = lint0001("acme.security.Auth:CheckUser(INPUT uid).");
    // `uid` is undefined and should still fire; receiver must not.
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(diags[0].contains("uid"), "{diags:?}");
}

#[test]
fn static_class_member_receiver_is_silent() {
    let diags = lint0001("MyStatics:CurrentCompany = coId.");
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(diags[0].contains("coId"), "{diags:?}");
}

// ---------------------------------------------------------------------
// Item B — PROPERTY SET accessor parameter
// ---------------------------------------------------------------------

#[test]
fn property_set_parameter_is_in_scope() {
    let src = r#"
CLASS Foo:
  DEFINE PUBLIC PROPERTY Title AS CHARACTER
    GET.
    SET (INPUT pv AS CHARACTER):
      MESSAGE pv.
    END SET.
END CLASS.
"#;
    let diags = lint0001(src);
    assert!(
        !diags.iter().any(|m| m.contains("`pv`")),
        "SET param pv must not be undefined: {diags:?}"
    );
}
