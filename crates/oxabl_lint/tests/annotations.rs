//! Annotations are metadata: they must neither raise findings nor count as
//! uses of the symbols whose names their attributes happen to spell.

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0002, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

fn lint_codes(source: &str) -> Vec<&'static str> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema)
        .with_lint_severities(LintSeverityMap::new());
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .collect()
}

#[test]
fn annotations_alone_raise_no_findings() {
    let src = "\
CLASS Sample:
    @Test.
    METHOD PUBLIC VOID ItWorks():
    END METHOD.

    @Test (expected=\"Progress.Lang.AppError\").
    @strict.effects(reads=\"Customer\", writes=\"Order\").
    METHOD PUBLIC VOID Other():
    END METHOD.
END CLASS.
";
    assert_eq!(lint_codes(src), Vec::<&str>::new());
}

#[test]
fn attribute_names_do_not_resolve_or_use_symbols() {
    let src = "\
PROCEDURE p:
    @strict.effects(reads=\"Customer\").
    DEFINE VARIABLE reads AS INTEGER NO-UNDO.
    MESSAGE \"x\".
END PROCEDURE.
";
    let codes = lint_codes(src);
    assert_eq!(codes, vec![LINT0002], "{codes:?}");
    assert!(!codes.contains(&LINT0001));
}
