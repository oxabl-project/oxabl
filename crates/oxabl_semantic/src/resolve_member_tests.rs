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
