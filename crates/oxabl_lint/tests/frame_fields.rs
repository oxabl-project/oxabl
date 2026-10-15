//! Frame fields: a `name AS type` or `name LIKE field` item of a FORM or
//! DEFINE FRAME declares a variable, scoped like `DEFINE VARIABLE`, that later
//! statements reference by name.
//!
//! Every fixture here is synthetic.

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{
    AnalysisContext, NamespaceId, PrimitiveTy, ResolvedType, SymbolKind, analyze_file,
};

const DF: &str = r#"
ADD TABLE "item"
ADD FIELD "descr" OF "item" AS character
"#;

struct Run {
    sem: oxabl_semantic::Semantic,
    diags: Vec<Diagnostic>,
}

fn run(source: &str) -> Run {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    let schema: Schema = schema_from_df(DF);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    ctx.schema_loaded = true;
    let sem = analyze_file(&program.statements, &ctx);
    let mut diags = sem.diagnostics.clone();
    diags.extend(lint_file(&program.statements, &sem, &ctx));
    Run { sem, diags }
}

fn undefined(diags: &[Diagnostic]) -> Vec<String> {
    diags
        .iter()
        .filter(|d| d.code.0 == LINT0001)
        .map(|d| d.message.clone())
        .collect()
}

/// The declared variables named `name`, with their types.
fn variables(r: &Run, name: &str) -> Vec<Option<ResolvedType>> {
    r.sem
        .symbols
        .iter()
        .map(|(_, s)| s)
        .filter(|s| {
            s.kind == SymbolKind::Variable
                && s.namespace == NamespaceId::Values
                && s.name.as_ref() == name
        })
        .map(|s| s.data_type.clone())
        .collect()
}

#[test]
fn a_define_frame_field_is_a_variable() {
    let r = run("DEFINE FRAME f-main
    \"Order:\" VIEW-AS TEXT AT ROW 1 COL 1
    v-order AS CHARACTER FORMAT \"x(20)\" VIEW-AS FILL-IN SIZE 20 BY 1 AT ROW 1 COL 8
    v-desc LIKE item.descr
    WITH NO-LABELS SIZE 40 BY 4.
ON LEAVE OF v-order IN FRAME f-main DO:
    IF LENGTH(v-order) = 0 THEN RETURN NO-APPLY.
END.
ASSIGN v-order = \"\" v-desc = \"x\".
DISPLAY v-order v-desc WITH FRAME f-main.
ENABLE v-order WITH FRAME f-main.
APPLY \"ENTRY\" TO v-order.
");
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(
        variables(&r, "v-order"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Character))]
    );
    assert_eq!(variables(&r, "v-desc"), vec![None]);
}

#[test]
fn a_form_field_is_a_variable() {
    let r = run("FORM v-qty AS INTEGER LABEL \"Qty\" v-note LIKE item.descr
    WITH FRAME f SIDE-LABELS.
v-qty = 1.
v-note = \"x\".
DISPLAY v-qty v-note WITH FRAME f.
");
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(
        variables(&r, "v-qty"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Integer))]
    );
}

#[test]
fn a_frame_field_is_scoped_to_its_routine() {
    let r = run("PROCEDURE p1:
    FORM v-local AS CHARACTER WITH FRAME f.
    DO WHILE TRUE:
        FORM v-inner AS INTEGER WITH FRAME g.
        LEAVE.
    END.
    DISPLAY v-local v-inner WITH FRAME f.
END PROCEDURE.
PROCEDURE p2:
    DISPLAY v-local.
END PROCEDURE.
");
    assert_eq!(
        undefined(&r.diags),
        vec!["undefined symbol `v-local`".to_string()]
    );
}

#[test]
fn a_frame_field_that_names_a_variable_uses_it() {
    let r = run("DEFINE VARIABLE v-name AS CHARACTER NO-UNDO.
&IF 1 = 1 &THEN
DEFINE FRAME f v-code AS CHARACTER v-name AS CHARACTER WITH NO-LABELS.
&ELSE
DEFINE FRAME f v-code AS CHARACTER FORMAT \"x(4)\" WITH NO-LABELS.
&ENDIF
FORM v-code AS CHARACTER WITH FRAME g.
v-name = \"a\".
v-code = \"b\".
DISPLAY v-name v-code WITH FRAME f.
");
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(variables(&r, "v-code").len(), 1);
    assert_eq!(variables(&r, "v-name").len(), 1);
}

#[test]
fn a_message_update_as_field_is_a_variable() {
    let r = run(
        r#"MESSAGE "Fix?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-fix AS LOGICAL.
IF v-fix THEN MESSAGE "fixing".
"#,
    );
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(
        variables(&r, "v-fix"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Logical))]
    );
}

#[test]
fn a_message_update_like_field_is_a_variable() {
    let r = run(r#"MESSAGE "Enter:" UPDATE v-copy LIKE item.descr.
DISPLAY v-copy.
"#);
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(variables(&r, "v-copy"), vec![None]);
}

#[test]
fn a_message_update_target_without_as_is_not_declared() {
    // Without AS or LIKE the phrase declares nothing: the name stays a write
    // target, so a later read of it is still an undefined symbol.
    let r = run(r#"MESSAGE "x" UPDATE v-undeclared.
DISPLAY v-undeclared.
"#);
    assert!(variables(&r, "v-undeclared").is_empty());
    assert_eq!(
        undefined(&r.diags),
        vec!["undefined symbol `v-undeclared`".to_string()]
    );
}

#[test]
fn an_undeclared_frame_item_is_still_undefined() {
    let r = run(
        "DEFINE FRAME f v-known AS CHARACTER v-unknown WITH NO-LABELS.
DISPLAY v-known v-unknown WITH FRAME f.
",
    );
    assert_eq!(
        undefined(&r.diags),
        vec!["undefined symbol `v-unknown`".to_string()]
    );
}

#[test]
fn an_update_or_set_field_is_a_variable() {
    let r = run("UPDATE v-ok AS LOGICAL LABEL \"Sure?\".
IF v-ok THEN DISPLAY \"yes\".
SET v-ok2 AS LOGICAL.
DISPLAY v-ok2.
");
    assert!(
        r.diags.is_empty(),
        "expected no findings, got: {:#?}",
        r.diags
    );
    assert_eq!(
        variables(&r, "v-ok"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Logical))]
    );
    assert_eq!(
        variables(&r, "v-ok2"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Logical))]
    );
}

#[test]
fn an_undeclared_update_item_is_still_undefined() {
    let r = run("UPDATE v-known AS LOGICAL v-unknown.\n");
    assert_eq!(
        undefined(&r.diags),
        vec!["undefined symbol `v-unknown`".to_string()]
    );
}
