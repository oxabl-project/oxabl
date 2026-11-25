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
                members_complete: true,
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
        params: None,
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

#[test]
fn a_long_chain_with_many_records_binds_in_linear_time() {
    const LEVELS: usize = 5;
    const PER_LEVEL: usize = 8_000;
    let mut classes = HashMap::new();
    for level in 1..=LEVELS {
        let mut members = Vec::new();
        for n in 0..PER_LEVEL {
            members.push(member(
                &format!("tt{level}_{n}"),
                SymbolKind::TempTable,
                AccessModifier::Protected,
                None,
            ));
            members.push(member(
                &format!("b{level}_{n}"),
                SymbolKind::Buffer,
                AccessModifier::Protected,
                buffer_for("customer"),
            ));
        }
        let parent =
            (level < LEVELS).then(|| &*Box::leak(format!("pkg.l{}", level + 1).into_boxed_str()));
        classes.insert(format!("pkg.l{level}"), (parent, members));
    }
    let source = "CLASS app.sub INHERITS pkg.l1:\nEND CLASS.\n";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    let classes = Classes(classes);

    let start = std::time::Instant::now();
    let found = inherited_records(&program.statements, &classes);
    let elapsed = start.elapsed();

    assert_eq!(found.records.len(), LEVELS * PER_LEVEL * 2);
    assert!(
        elapsed < std::time::Duration::from_secs(3),
        "binding {} records took {elapsed:?}",
        found.records.len()
    );
}

#[test]
fn a_database_qualified_buffer_target_is_a_table_of_the_schema() {
    let classes = Classes(HashMap::from([(
        "pkg.base".to_string(),
        (
            None,
            vec![
                member(
                    "customer",
                    SymbolKind::TempTable,
                    AccessModifier::Protected,
                    None,
                ),
                member(
                    "q_",
                    SymbolKind::Buffer,
                    AccessModifier::Protected,
                    buffer_for("db.customer"),
                ),
            ],
        ),
    )]));
    let found = records_of(classes);
    let binding = found
        .records
        .iter()
        .find(|r| r.name.as_atom().as_ref() == "q_")
        .map(|r| r.binding.clone());
    assert_eq!(binding, Some(InheritedBinding::Table("db.customer".into())));
}

#[test]
fn a_qualified_buffer_target_stays_a_table_when_the_chain_is_not_read() {
    let classes = Classes(HashMap::from([(
        "pkg.base".to_string(),
        (
            Some("pkg.absent"),
            vec![member(
                "q_",
                SymbolKind::Buffer,
                AccessModifier::Protected,
                buffer_for("db.customer"),
            )],
        ),
    )]));
    let found = records_of(classes);
    assert!(found.opaque);
    assert_eq!(
        found.records[0].binding,
        InheritedBinding::Table("db.customer".into())
    );
}
