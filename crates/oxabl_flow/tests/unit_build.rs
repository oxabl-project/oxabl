//! `UnitBuilder` builds one unit, equal to the same unit of `build_file`.

use std::path::Path;

use oxabl_ast::NodeId;
use oxabl_flow::{Cfg, FileFlow, UnitBuilder};

fn shape(cfg: &Cfg) -> String {
    format!(
        "{:?} {:?} {:?} {:?}",
        cfg.id(),
        cfg.nodes().collect::<Vec<_>>(),
        cfg.edges(),
        cfg.blocks().collect::<Vec<_>>()
    )
}

/// The unit `stmt` defines, else the first unit that has a block or a node
/// for it.
fn expected(flow: &FileFlow, stmt: NodeId) -> Option<&Cfg> {
    flow.units()
        .iter()
        .find(|c| c.unit().stmt == stmt)
        .or_else(|| {
            flow.units().iter().find(|c| {
                c.block_of(stmt).is_some()
                    || !c.nodes_of(stmt).is_empty()
                    || c.blocks().any(|(_, b)| b.stmt == stmt)
            })
        })
}

#[test]
fn a_statement_finds_its_own_unit_only() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let mut checked = 0;
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if !matches!(path.extension().and_then(|e| e.to_str()), Some("p" | "cls")) {
            continue;
        }
        let source = std::fs::read_to_string(&path).unwrap();
        let tokens = oxabl_lexer::tokenize(&source);
        let program = oxabl_parser::Parser::new(&tokens, &source).parse_program();
        let flow = oxabl_flow::build_file(&program.statements, &source);
        let builder = UnitBuilder::new(
            &program.statements,
            &source,
            &oxabl_flow::FlowOptions::default(),
        );
        for cfg in flow.units() {
            let stmts = cfg
                .blocks()
                .map(|(_, b)| b.stmt)
                .chain(cfg.nodes().filter_map(|(_, n)| n.stmt));
            for stmt in stmts {
                let unit = builder
                    .unit_of(stmt)
                    .unwrap_or_else(|| panic!("{}: no unit for {stmt:?}", path.display()));
                let want = expected(&flow, stmt).unwrap();
                assert_eq!(unit, want.id(), "{}: {stmt:?}", path.display());
                assert_eq!(
                    shape(&builder.build(unit)),
                    shape(want),
                    "{}: {stmt:?}",
                    path.display()
                );
                checked += 1;
            }
        }
    }
    assert!(checked > 100, "{checked}");
}

#[test]
fn a_statement_in_no_unit_has_none() {
    let source = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    let builder = UnitBuilder::new(&program.statements, source, &Default::default());
    let missing = oxabl_ast::Statement::new(oxabl_ast::StatementKind::Empty).id;
    assert!(builder.unit_of(missing).is_none());
}
