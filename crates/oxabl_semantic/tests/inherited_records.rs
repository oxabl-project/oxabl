//! `inherited_records` over an index that answers from a table of classes.

use std::collections::HashMap;
use std::sync::Arc;

use oxabl_ast::AccessModifier;
use oxabl_semantic::{
    ClassDescriptor, ClassKind, IndexAnswer, IndexName, IndexRevision, IndexedFileId,
    InheritedBinding, MemberDescriptor, MemberRecord, MemberType, SymbolKind, WorkspaceIndex,
    inherited_records,
};

struct Classes(HashMap<String, (Option<&'static str>, Vec<MemberDescriptor>)>);

impl WorkspaceIndex for Classes {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        match self.0.get(name.as_atom().as_ref()) {
            None => IndexAnswer::NotFound,
            Some((parent, _)) => IndexAnswer::Found(Arc::new(ClassDescriptor {
                name: name.clone(),
                file: IndexedFileId::new(1),
                kind: ClassKind::Class,
                inherits: parent.map(IndexName::new),
                implements: Vec::new(),
                imports: Default::default(),
            })),
        }
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        match self.0.get(class.as_atom().as_ref()) {
            None => IndexAnswer::NotFound,
            Some((_, members)) => IndexAnswer::Found(Arc::from(members.clone())),
        }
    }

    fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn revision(&self) -> IndexRevision {
        IndexRevision::new(1)
    }
}

fn member(
    name: &str,
    kind: SymbolKind,
    access: AccessModifier,
    record: Option<MemberRecord>,
) -> MemberDescriptor {
    MemberDescriptor {
        name: IndexName::new(name),
        kind,
        ty: MemberType::Untyped,
        access,
        is_static: false,
        record,
    }
}

fn buffer_for(table: &str) -> Option<MemberRecord> {
    Some(MemberRecord {
        table: IndexName::new(table),
        temp_table: false,
    })
}

fn records_of(classes: Classes) -> oxabl_semantic::InheritedRecords {
    let source = "CLASS app.sub INHERITS pkg.base:\nEND CLASS.\n";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    inherited_records(&program.statements, &classes)
}

#[test]
fn a_buffer_whose_table_the_index_does_not_give_is_unknown() {
    let classes = Classes(HashMap::from([(
        "pkg.base".to_string(),
        (
            None,
            vec![member(
                "cust_",
                SymbolKind::Buffer,
                AccessModifier::Protected,
                None,
            )],
        ),
    )]));
    let found = records_of(classes);
    assert_eq!(found.records.len(), 1);
    assert_eq!(
        found.records[0].binding,
        InheritedBinding::Unknown("cust_".into())
    );
    assert!(!found.opaque);
}

#[test]
fn the_nearest_reachable_member_gives_the_binding() {
    let classes = Classes(HashMap::from([
        (
            "pkg.base".to_string(),
            (
                Some("pkg.top"),
                vec![
                    member(
                        "b_",
                        SymbolKind::Buffer,
                        AccessModifier::Private,
                        buffer_for("item"),
                    ),
                    member("t_", SymbolKind::TempTable, AccessModifier::Protected, None),
                ],
            ),
        ),
        (
            "pkg.top".to_string(),
            (
                None,
                vec![
                    member(
                        "b_",
                        SymbolKind::Buffer,
                        AccessModifier::Protected,
                        buffer_for("customer"),
                    ),
                    member("t_", SymbolKind::TempTable, AccessModifier::Private, None),
                    member("p_", SymbolKind::TempTable, AccessModifier::Private, None),
                ],
            ),
        ),
    ]));
    let found = records_of(classes);
    let binding = |name: &str| {
        found
            .records
            .iter()
            .find(|r| r.name.as_atom().as_ref() == name)
            .map(|r| r.binding.clone())
    };
    // The private `b_` of `pkg.base` is out of reach, so `pkg.top`'s is the one.
    assert_eq!(
        binding("b_"),
        Some(InheritedBinding::Table("customer".into()))
    );
    assert_eq!(
        binding("t_"),
        Some(InheritedBinding::TempTable("t_".into()))
    );
    assert_eq!(binding("p_"), Some(InheritedBinding::Unknown("p_".into())));
    assert!(!found.opaque);
}

#[test]
fn a_chain_that_loops_ends_without_the_class_itself() {
    let classes = Classes(HashMap::from([
        ("pkg.base".to_string(), (Some("app.sub"), Vec::new())),
        (
            "app.sub".to_string(),
            (
                Some("pkg.base"),
                vec![member(
                    "own_",
                    SymbolKind::TempTable,
                    AccessModifier::Protected,
                    None,
                )],
            ),
        ),
    ]));
    let found = records_of(classes);
    assert!(found.records.is_empty(), "{found:?}");
}
