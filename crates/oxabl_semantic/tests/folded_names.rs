//! Names are case-insensitive, and only in ASCII: every spelling of a name
//! that differs in the case of ASCII letters is one symbol, however long the
//! name is, and a name that differs in anything else is another.

use oxabl_common::FileId;
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, NamespaceId, Resolution, ScopeId, Semantic, SymbolId, UnresolvedReason,
    analyze_file,
};

fn analyze(source: &str) -> Semantic {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema, &program.pools);
    analyze_file(&program.statements, &ctx)
}

/// What each `MESSAGE` reference of `source` resolves to, in source order.
fn resolutions(semantic: &Semantic) -> Vec<Resolution> {
    semantic
        .references
        .iter()
        .map(|(_, resolution)| resolution.clone())
        .collect()
}

fn resolved(resolution: &Resolution) -> Option<SymbolId> {
    match resolution {
        Resolution::Resolved(sym) => Some(*sym),
        Resolution::Unresolved { .. } => None,
    }
}

fn not_in_scope(resolution: &Resolution) -> bool {
    matches!(
        resolution,
        Resolution::Unresolved {
            reason: UnresolvedReason::NotInScope,
            ..
        }
    )
}

#[test]
fn every_case_of_a_name_is_one_symbol() {
    let semantic = analyze(
        "DEFINE VARIABLE Order-Total AS INTEGER NO-UNDO.\n\
         MESSAGE order-total ORDER-TOTAL Order-Total oRdEr-ToTaL.\n",
    );
    let found = resolutions(&semantic);
    assert_eq!(found.len(), 4);
    let first = resolved(&found[0]).expect("the lower case spelling resolves");
    assert!(found.iter().all(|r| resolved(r) == Some(first)));
    assert_eq!(semantic.symbols.get(first).name.as_ref(), "order-total");
}

#[test]
fn a_hyphen_is_part_of_the_name() {
    let semantic = analyze(
        "DEFINE VARIABLE order-total AS INTEGER NO-UNDO.\n\
         MESSAGE order_total.\n\
         MESSAGE order.\n\
         MESSAGE order-total.\n",
    );
    let found = resolutions(&semantic);
    assert_eq!(found.len(), 3);
    assert!(not_in_scope(&found[0]), "an underscore is not a hyphen");
    assert!(not_in_scope(&found[1]), "a prefix is not an abbreviation");
    assert!(resolved(&found[2]).is_some());
}

#[test]
fn a_name_longer_than_any_fixed_buffer_folds_whole() {
    let stem = "Customer-Order-Line-Total-Amount-Before-Discount-And-Tax-For-The-Whole-Year";
    assert!(stem.len() > 64);
    let long = format!("{stem}-{}", "X".repeat(150));
    let lower = long.to_ascii_lowercase();
    let near = format!("{lower}y");
    let source = format!(
        "DEFINE VARIABLE {long} AS INTEGER NO-UNDO.\n\
         MESSAGE {lower}.\n\
         MESSAGE {long}.\n\
         MESSAGE {near}.\n"
    );
    let semantic = analyze(&source);
    let found = resolutions(&semantic);
    assert_eq!(found.len(), 3);
    let symbol = resolved(&found[0]).expect("the folded spelling resolves");
    assert_eq!(resolved(&found[1]), Some(symbol));
    assert!(not_in_scope(&found[2]), "a longer name is another name");
    assert_eq!(semantic.symbols.get(symbol).name.as_ref(), lower);
}

/// The lexer reads no non-ASCII identifier, so the AST is built by hand: a
/// stream named `Ärger`, `ärger` and `ÄRGER`.
#[test]
fn a_non_ascii_letter_does_not_fold() {
    use oxabl_ast::{AstPools, Identifier, Span, Statement, StatementKind};
    let mut pools = AstPools::new();
    let mut stream = |name: &str| {
        let name = Identifier {
            span: Span { start: 0, end: 1 },
            name: pools.names.push(name),
        };
        Statement::new(StatementKind::DefineStream { name })
    };
    let program = [stream("Ärger"), stream("ärger"), stream("ÄRGER")];
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema, &pools);
    let semantic = analyze_file(&program, &ctx);
    let tree = &semantic.scope_tree;
    let find = |name: &str| tree.resolve_name(ScopeId::ROOT, NamespaceId::Streams, name);
    let upper = find("ÄRGER").expect("the ASCII letters of ÄRGER fold to Ärger");
    assert_eq!(find("Ärger"), Some(upper));
    let lower = find("ärger").expect("ärger is a stream of its own");
    assert_ne!(upper, lower, "ä is not the lower case of Ä here");
    assert_eq!(semantic.symbols.get(upper).name.as_ref(), "Ärger");
    assert_eq!(semantic.symbols.get(lower).name.as_ref(), "ärger");
    assert!(
        semantic.diagnostics.is_empty(),
        "{:?}",
        semantic.diagnostics
    );
}

#[test]
fn a_built_in_handle_resolves_in_every_case_and_a_declaration_may_shadow_it() {
    let semantic = analyze(
        "MESSAGE Session SESSION session.\n\
         DEFINE VARIABLE Error-Status AS INTEGER NO-UNDO.\n\
         MESSAGE error-status.\n",
    );
    let found = resolutions(&semantic);
    assert_eq!(found.len(), 4);
    let handle = resolved(&found[0]).expect("SESSION is seeded");
    assert_eq!(resolved(&found[1]), Some(handle));
    assert_eq!(resolved(&found[2]), Some(handle));
    let shadow = resolved(&found[3]).expect("the declaration resolves");
    assert_ne!(shadow, handle, "the file's own declaration wins");
}

#[test]
fn a_name_of_the_scope_tree_is_found_by_any_spelling() {
    let semantic = analyze(
        "DEFINE VARIABLE Order-Total AS INTEGER NO-UNDO.\n\
         PROCEDURE Calc-Tax:\n    DEFINE VARIABLE Local-Rate AS INTEGER NO-UNDO.\nEND PROCEDURE.\n",
    );
    let tree = &semantic.scope_tree;
    let root = ScopeId::ROOT;
    let sym = tree
        .resolve_name(root, NamespaceId::Values, "ORDER-TOTAL")
        .expect("any case finds the variable");
    assert_eq!(semantic.symbols.get(sym).name.as_ref(), "order-total");
    assert!(
        tree.resolve_name(root, NamespaceId::Values, "local-rate")
            .is_none(),
        "a name of a procedure is not bound in the file scope"
    );
    assert!(
        tree.resolve_name(root, NamespaceId::Procedures, "CALC-tax")
            .is_some()
    );
    assert!(
        tree.resolve_name(root, NamespaceId::Values, "never-written")
            .is_none()
    );
}
