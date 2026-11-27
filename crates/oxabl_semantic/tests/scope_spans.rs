//! The text each scope covers, and the scope at an offset.

use oxabl_common::FileId;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, ScopeKind, analyze_file_untyped};

/// The kind of the innermost scope at the offset of `|` in `source`.
fn kind_at(source: &str) -> ScopeKind {
    let at = source.find('|').expect("a cursor") as u32;
    let text = source.replace('|', " ");
    let tokens = oxabl_lexer::tokenize(&text);
    let program = oxabl_parser::Parser::new(&tokens, &text).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, &text, &schema, &program.pools)
        .with_unclosed_blocks(&program.unclosed_blocks);
    let semantic = analyze_file_untyped(&program.statements, &ctx);
    let tree = &semantic.scope_tree;
    tree.get(tree.innermost_at(at)).kind()
}

#[test]
fn a_procedure_without_end_holds_the_end_of_the_text() {
    let source = "PROCEDURE p:\n    DEFINE VARIABLE x AS INTEGER NO-UNDO.\n    |";
    assert_eq!(kind_at(source), ScopeKind::Procedure);
}

#[test]
fn a_closed_procedure_ends_at_its_end() {
    assert_eq!(
        kind_at("PROCEDURE p:\n    |\nEND PROCEDURE.\n"),
        ScopeKind::Procedure
    );
    assert_eq!(kind_at("PROCEDURE p:\nEND PROCEDURE.\n|"), ScopeKind::File);
}

#[test]
fn nested_blocks_give_the_innermost_scope() {
    let source = "CLASS Box:\n    METHOD PUBLIC VOID m():\n        DO WHILE TRUE:\n            |\n        END.\n    END METHOD.\nEND CLASS.\n";
    assert_eq!(kind_at(source), ScopeKind::Block);
    let source = "CLASS Box:\n    METHOD PUBLIC VOID m():\n        DO WHILE TRUE:\n        END.\n        |\n    END METHOD.\nEND CLASS.\n";
    assert_eq!(kind_at(source), ScopeKind::Method);
    let source = "CLASS Box:\n    METHOD PUBLIC VOID m():\n    END METHOD.\n    |\nEND CLASS.\n";
    assert_eq!(kind_at(source), ScopeKind::Class);
}

#[test]
fn each_property_accessor_holds_its_own_body() {
    let class = |get: &str, set: &str| {
        format!(
            "CLASS Box:\n    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO\n        GET():\n            {get}\n        END GET.\n        SET(INPUT arg AS INTEGER):\n            {set}\n        END SET.\nEND CLASS.\n"
        )
    };
    assert_eq!(kind_at(&class("x = |1.", "")), ScopeKind::PropertyGet);
    assert_eq!(kind_at(&class("", "x = |1.")), ScopeKind::PropertySet);
    // An auto getter before a computed setter.
    let auto = "CLASS Box:\n    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO\n        GET.\n        SET(INPUT arg AS INTEGER):\n            x = |1.\n        END SET.\nEND CLASS.\n";
    assert_eq!(kind_at(auto), ScopeKind::PropertySet);
    // A getter closed by a bare `END.`, with a block that ends with `END.` in it.
    let bare = "CLASS Box:\n    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO\n        GET():\n            DO WHILE FALSE:\n            END.\n            x = |1.\n        END.\n        SET(INPUT arg AS INTEGER):\n            y = 1.\n        END.\nEND CLASS.\n";
    assert_eq!(kind_at(bare), ScopeKind::PropertyGet);
    let bare_set = bare
        .replace("x = |1.", "x = 1.")
        .replace("y = 1.", "y = |1.");
    assert_eq!(kind_at(&bare_set), ScopeKind::PropertySet);
}
