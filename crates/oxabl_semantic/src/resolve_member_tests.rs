//! Member lookup and member types of classes: the facts the resolve pass
//! records for the type-check pass.

use oxabl_common::FileId;
use oxabl_schema::Schema;

use crate::{
    AnalysisContext, CallParam, CallParamKind, ParamMode, PrimitiveTy, ResolvedType, Semantic,
    SymbolId, SymbolKind, analyze_file,
};

fn analyze(source: &str) -> Semantic {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    analyze_file(&program.statements, &ctx)
}

/// The method symbols named `name`, in declaration order.
fn methods(sem: &Semantic, name: &str) -> Vec<SymbolId> {
    sem.symbols
        .iter()
        .filter(|(_, s)| s.kind == SymbolKind::Function && s.name.as_ref() == name)
        .map(|(id, _)| id)
        .collect()
}

fn class(sem: &Semantic, name: &str) -> SymbolId {
    sem.symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Class && s.name.as_ref() == name)
        .map(|(id, _)| id)
        .expect("the class is declared")
}

#[test]
fn a_local_method_records_its_parameter_list_with_class_names_resolved() {
    let sem = analyze(
        "CLASS Thing:\n\
         METHOD PUBLIC INTEGER Score(INPUT a AS INTEGER, OUTPUT b AS Thing):\n\
         END METHOD.\n\
         METHOD PUBLIC Thing Score(INPUT a AS CHARACTER):\n\
         END METHOD.\n\
         END CLASS.\n",
    );
    let thing = class(&sem, "thing");
    let score = methods(&sem, "score");
    assert_eq!(score.len(), 2);
    assert_eq!(
        sem.symbols.method_params(score[0]),
        Some(
            &[
                CallParam {
                    mode: ParamMode::Input,
                    kind: CallParamKind::Value(ResolvedType::Primitive(PrimitiveTy::Integer)),
                },
                CallParam {
                    mode: ParamMode::Output,
                    kind: CallParamKind::Value(ResolvedType::Class(thing)),
                },
            ][..]
        )
    );
    assert_eq!(
        sem.symbols.method_param_decls(score[1]).map(|p| p.len()),
        Some(1)
    );
    assert_eq!(
        sem.symbols.get(score[1]).data_type,
        Some(ResolvedType::Class(thing)),
        "the class-typed return type of the second overload is upgraded"
    );
    assert_eq!(
        sem.symbols.get(score[0]).data_type,
        Some(ResolvedType::Primitive(PrimitiveTy::Integer))
    );
}

/// The resolution of each `MemberAccess` / `MethodCall` in walk order, as
/// `(member name, resolved symbol name or None, untyped)`.
fn member_refs(source: &str) -> Vec<(String, Option<String>, bool)> {
    use oxabl_ast::{Expression, ExpressionKind, Statement, StatementKind};
    fn walk_expr(e: &Expression, out: &mut Vec<(oxabl_ast::NodeId, String)>) {
        match &e.kind {
            ExpressionKind::MemberAccess { object, member } => {
                walk_expr(object, out);
                out.push((e.id, member.name.clone()));
            }
            ExpressionKind::MethodCall {
                object,
                method,
                arguments,
            } => {
                walk_expr(object, out);
                arguments.iter().for_each(|a| walk_expr(a, out));
                out.push((e.id, method.name.clone()));
            }
            _ => {}
        }
    }
    fn walk(stmts: &[Statement], out: &mut Vec<(oxabl_ast::NodeId, String)>) {
        for s in stmts {
            match &s.kind {
                StatementKind::Class { body, .. } | StatementKind::Method { body, .. } => {
                    walk(body, out)
                }
                StatementKind::Assignment { value, .. } => walk_expr(value, out),
                _ => {}
            }
        }
    }
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    let mut nodes = Vec::new();
    walk(&program.statements, &mut nodes);
    nodes
        .into_iter()
        .map(|(id, name)| {
            let resolved = match sem.references.get(id) {
                Some(crate::Resolution::Resolved(sym)) => {
                    Some(sem.symbols.get(*sym).name.to_string())
                }
                _ => None,
            };
            (name, resolved, sem.symbols.is_untyped_member(id))
        })
        .collect()
}

#[test]
fn receivers_of_a_class_of_the_analysed_file() {
    let refs = member_refs(
        "CLASS Thing:\n\
         DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.\n\
         DEFINE PRIVATE PROPERTY Secret AS CHARACTER NO-UNDO GET. SET.\n\
         DEFINE PUBLIC STATIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.\n\
         DEFINE PUBLIC PROPERTY Next AS Thing NO-UNDO GET. SET.\n\
         METHOD PUBLIC Thing Make():\n    RETURN NEW Thing().\nEND METHOD.\n\
         METHOD PUBLIC VOID T():\n\
         DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE o AS Thing NO-UNDO.\n\
         c = THIS-OBJECT:Name.\n\
         c = THIS-OBJECT:Secret.\n\
         c = o:Next:Name.\n\
         c = THIS-OBJECT:Make():Name.\n\
         c = NEW Thing():Name.\n\
         c = CAST(o, Thing):Name.\n\
         c = Thing:Count.\n\
         c = o:Count.\n\
         c = THIS-OBJECT:Nope.\n\
         END METHOD.\n\
         END CLASS.\n",
    );
    let r = |name: &str, resolved: Option<&str>, untyped: bool| {
        (name.to_string(), resolved.map(str::to_string), untyped)
    };
    assert_eq!(
        refs,
        [
            r("Name", Some("name"), false),
            r("Secret", Some("secret"), false),
            r("Next", Some("next"), false),
            r("Name", Some("name"), false),
            r("Make", Some("make"), false),
            r("Name", Some("name"), false),
            r("Name", Some("name"), false),
            r("Name", Some("name"), false),
            r("Count", Some("count"), false),
            r("Count", Some("count"), true),
            r("Nope", None, false),
        ]
    );
}
