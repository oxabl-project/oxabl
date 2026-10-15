//! The expressions inside the format options of UPDATE, SET, FORM and ENABLE
//! (`VALIDATE(cond, msg)`, `WHEN cond`) are read like any other expression:
//! their table, field and variable references resolve, and the rules see them.
//!
//! Every fixture here is synthetic.

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0003, LINT0006, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{AnalysisContext, Resolution, Semantic, SymbolKind, analyze_file};

const DF: &str = r#"
ADD TABLE "account"
ADD FIELD "num" OF "account" AS integer
"#;

/// A statement head that takes the option, and the form of option to add after
/// the `v` item.
const STATEMENTS: [&str; 4] = ["UPDATE", "SET", "FORM", "ENABLE"];
const OPTIONS: [fn(&str) -> String; 2] = [
    |cond| format!("VALIDATE({cond}, \"bad\")"),
    |cond| format!("WHEN {cond}"),
];

fn analyze(source: &str) -> (Semantic, Vec<Diagnostic>) {
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
    let diags = lint_file(&program.statements, &sem, &ctx);
    (sem, diags)
}

/// The source of every statement shape, with `cond` as the option expression
/// and `prelude` declared first.
fn fixtures(prelude: &str, cond: &str) -> Vec<String> {
    let mut out = Vec::new();
    for stmt in STATEMENTS {
        for option in OPTIONS {
            out.push(format!(
                "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n{prelude}\n{stmt} v {} WITH FRAME f.\n",
                option(cond)
            ));
        }
    }
    out
}

fn messages(diags: &[Diagnostic], code: &str) -> Vec<String> {
    diags
        .iter()
        .filter(|d| d.code.0 == code)
        .map(|d| d.message.clone())
        .collect()
}

fn resolved_to(sem: &Semantic, name: &str, kind: SymbolKind) -> usize {
    sem.references
        .iter()
        .filter(|(_, r)| match r {
            Resolution::Resolved(sym) => {
                let s = sem.symbols.get(*sym);
                s.name.as_ref() == name && s.kind == kind
            }
            Resolution::Unresolved { .. } => false,
        })
        .count()
}

#[test]
fn a_table_and_field_in_an_option_expression_resolve() {
    for source in fixtures("", "CAN-FIND(account WHERE account.num = v)") {
        let (sem, diags) = analyze(&source);
        assert_eq!(
            resolved_to(&sem, "num", SymbolKind::Field),
            1,
            "{source}: {diags:?}"
        );
        assert!(
            sem.symbols
                .iter()
                .any(|(_, s)| s.table_id.is_some() && s.name.as_ref() == "account"),
            "{source}: no schema-bound symbol for the table"
        );
        assert!(messages(&diags, LINT0003).is_empty(), "{source}: {diags:?}");
    }
}

#[test]
fn an_unknown_field_in_an_option_expression_is_reported() {
    for source in fixtures("", "CAN-FIND(account WHERE account.nope = v)") {
        let (_, diags) = analyze(&source);
        assert_eq!(
            messages(&diags, LINT0003),
            vec!["unknown field `nope`".to_string()],
            "{source}"
        );
    }
}

#[test]
fn an_unknown_name_in_an_option_expression_is_reported() {
    for source in fixtures("", "v > v-missing") {
        let (_, diags) = analyze(&source);
        assert_eq!(messages(&diags, LINT0001).len(), 1, "{source}: {diags:?}");
    }
}

#[test]
fn a_variable_read_only_in_an_option_expression_is_not_a_dead_store() {
    for source in fixtures(
        "DEFINE VARIABLE lim AS INTEGER NO-UNDO.\nlim = 5.",
        "v < lim",
    ) {
        let (_, diags) = analyze(&source);
        assert!(messages(&diags, LINT0006).is_empty(), "{source}: {diags:?}");
    }
}
