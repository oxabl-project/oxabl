//! Names of buffers and temp-tables that a superclass defines
//! (`analyze_tokens_inherited`), on source that stands for a subclass.

use oxabl_scope::{Inherited, InheritedKind, InheritedRecord, InheritedTable, ScopeOptions, Table};

const SCHEMA: &str = "ADD TABLE \"customer\"\nADD FIELD \"name\" OF \"customer\" AS character\n";

fn record(name: &str, kind: InheritedKind, table: InheritedTable) -> InheritedRecord {
    InheritedRecord {
        name: name.to_string(),
        kind,
        table,
    }
}

fn scopes(source: &str, inherited: &Inherited) -> oxabl_scope::FileScopes {
    let schema = oxabl_schema::test_support::schema_from_df(SCHEMA);
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    oxabl_scope::analyze_tokens_inherited(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        Some(&schema),
        &ScopeOptions::default(),
        inherited,
    )
}

fn tables_of_reads(scopes: &oxabl_scope::FileScopes) -> Vec<Table> {
    scopes.reads().iter().map(|r| r.table.clone()).collect()
}

#[test]
fn an_inherited_temp_table_has_no_fields_of_a_local_one_with_its_name() {
    // The file's own `ttRoot` is not the table `rootTt_` is a buffer of.
    let source = "DEFINE TEMP-TABLE ttRoot NO-UNDO FIELD zzz AS INTEGER.\nFOR EACH rootTt_:\n    zzz = 1.\nEND.\n";
    let inherited = Inherited {
        records: vec![record(
            "roottt_",
            InheritedKind::Buffer,
            InheritedTable::TempTable("ttRoot".into()),
        )],
        opaque: false,
    };
    let scopes = scopes(source, &inherited);
    assert!(
        scopes.references().iter().all(|r| !r.field),
        "{}",
        scopes.dump(source)
    );
}

#[test]
fn a_name_no_record_has_is_a_schema_table_unless_a_class_was_not_read() {
    let source = "FIND FIRST customer NO-LOCK.\n";
    let read = scopes(source, &Inherited::default());
    assert_eq!(tables_of_reads(&read), [Table::Database("customer".into())]);
    let opaque = Inherited {
        records: Vec::new(),
        opaque: true,
    };
    let blind = scopes(source, &opaque);
    assert_eq!(tables_of_reads(&blind), [Table::Unknown("customer".into())]);
}

#[test]
fn a_buffer_for_a_name_is_bound_to_a_schema_table_only_when_nothing_hides_it() {
    let source = "DEFINE BUFFER b FOR customer.\nFIND FIRST b NO-LOCK.\n";
    let inherited = Inherited {
        records: vec![record(
            "customer",
            InheritedKind::TempTable,
            InheritedTable::TempTable("customer".into()),
        )],
        opaque: false,
    };
    let hidden = scopes(source, &inherited);
    assert!(
        matches!(tables_of_reads(&hidden)[..], [Table::TempTable { .. }]),
        "{}",
        hidden.dump(source)
    );
    // A buffer of the same name is not a table: it hides nothing.
    let buffer = Inherited {
        records: vec![record(
            "customer",
            InheritedKind::Buffer,
            InheritedTable::Named("customer".into()),
        )],
        opaque: false,
    };
    let bound = scopes(source, &buffer);
    assert_eq!(
        tables_of_reads(&bound),
        [Table::Database("customer".into())]
    );
}
