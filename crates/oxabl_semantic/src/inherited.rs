//! The buffers and temp-tables a class inherits, for analyses that read the
//! syntax and not the symbol table.
//!
//! One walk of the `INHERITS` chain, through the workspace index. It reads a
//! supertype name as the member walk of the resolver does
//! ([`lookup_supertype`]) and ends where that walk makes a surface opaque.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use oxabl_ast::{Statement, StatementKind};

use crate::resolve::{SupertypeLookup, lookup_supertype};
use crate::{
    ImportContext, IndexAnswer, IndexName, MemberDescriptor, MemberRecord, SymbolKind,
    WorkspaceIndex,
};

/// What an inherited record member is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InheritedKind {
    TempTable,
    Buffer,
}

/// The table an inherited record stands for in a subclass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InheritedBinding {
    /// A temp-table, by its name as written.
    TempTable(String),
    /// A table of the schema, by the name the declaring class wrote.
    Table(String),
    /// The subclass cannot tell: the member is `PRIVATE`, its declaration names
    /// no table, or a class of the chain is not readable.
    Unknown(String),
}

/// One buffer or temp-table name a subclass can mean by an inherited member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InheritedRecord {
    /// The member name, folded.
    pub name: IndexName,
    pub kind: InheritedKind,
    pub binding: InheritedBinding,
}

/// The buffers and temp-tables a class inherits.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct InheritedRecords {
    /// One record for each name some superclass defines. A `PROTECTED` or `PUBLIC`
    /// member of the nearest superclass that has one gives the binding. A name
    /// that only a `PRIVATE` member has is [`InheritedBinding::Unknown`].
    pub records: Vec<InheritedRecord>,
    /// The chain ends at a class the index cannot read, or at a shipped class:
    /// it may define any other name.
    pub opaque: bool,
}

/// Where the walk starts.
pub(crate) enum ChainStart<'a> {
    /// The class of the analysed file: its own members are in the file, so the
    /// walk starts at its superclass, written `raw` under `imports`.
    Parent {
        imports: Arc<ImportContext>,
        raw: &'a str,
    },
    /// An indexed class and its superclasses.
    Class(IndexName),
}

/// The buffers and temp-tables the class of `program` inherits through its
/// `INHERITS` chain. Empty for a file with no class, or a class with no
/// superclass.
pub fn inherited_records(program: &[Statement], index: &dyn WorkspaceIndex) -> InheritedRecords {
    let Some((own, Some(parent))) = program.iter().find_map(|s| match &s.kind {
        StatementKind::Class { name, inherits, .. } => Some((&name.name, inherits.as_ref())),
        _ => None,
    }) else {
        return InheritedRecords::default();
    };
    chain_records(
        index,
        Some(IndexName::new(own)),
        ChainStart::Parent {
            imports: Arc::new(ImportContext::from_statements(program)),
            raw: &parent.name,
        },
    )
}

struct Member {
    name: IndexName,
    kind: InheritedKind,
    record: Option<MemberRecord>,
    reachable: bool,
}

pub(crate) fn chain_records(
    index: &dyn WorkspaceIndex,
    own: Option<IndexName>,
    start: ChainStart<'_>,
) -> InheritedRecords {
    let mut levels: Vec<Vec<Member>> = Vec::new();
    let mut opaque = false;
    let mut visited: HashSet<IndexName> = own.into_iter().collect();
    let mut next = match start {
        ChainStart::Parent { imports, raw } => match lookup_supertype(index, &imports, raw) {
            SupertypeLookup::Class(name) => Some(name),
            SupertypeLookup::Assembly => {
                opaque = true;
                None
            }
            SupertypeLookup::Missing { root_object, .. } => {
                opaque = !root_object;
                None
            }
        },
        ChainStart::Class(name) => Some(name),
    };
    while let Some(name) = next.take() {
        if !visited.insert(name.clone()) {
            break;
        }
        let IndexAnswer::Found(descriptor) = index.class(&name) else {
            opaque = true;
            break;
        };
        let IndexAnswer::Found(members) = index.class_members(&name) else {
            opaque = true;
            break;
        };
        levels.push(members.iter().filter_map(member_of).collect());
        let Some(parent) = &descriptor.inherits else {
            break;
        };
        match lookup_supertype(index, &descriptor.imports, parent.as_written()) {
            SupertypeLookup::Class(name) => next = Some(name),
            SupertypeLookup::Assembly => opaque = true,
            SupertypeLookup::Missing { root_object, .. } => opaque = !root_object,
        }
    }
    InheritedRecords {
        records: bind(&levels, opaque),
        opaque,
    }
}

fn member_of(member: &MemberDescriptor) -> Option<Member> {
    let kind = match member.kind {
        SymbolKind::TempTable => InheritedKind::TempTable,
        SymbolKind::Buffer => InheritedKind::Buffer,
        _ => return None,
    };
    Some(Member {
        name: member.name.clone(),
        kind,
        record: member.record.clone(),
        reachable: member.inherited_by_subclass(),
    })
}

/// The temp-tables of the chain by folded name, each with the depths of the
/// levels that define it, nearest first.
type TempTables<'a> = HashMap<&'a str, Vec<(usize, &'a Member)>>;

fn bind(levels: &[Vec<Member>], opaque: bool) -> Vec<InheritedRecord> {
    // The nearest member of each name, and the nearest one a subclass can reach.
    let mut nearest: HashMap<&IndexName, &Member> = HashMap::new();
    let mut reachable: HashMap<&IndexName, (usize, &Member)> = HashMap::new();
    let mut temp_tables: TempTables<'_> = HashMap::new();
    let mut order: Vec<&IndexName> = Vec::new();
    for (depth, level) in levels.iter().enumerate() {
        for member in level {
            if let std::collections::hash_map::Entry::Vacant(e) = nearest.entry(&member.name) {
                e.insert(member);
                order.push(&member.name);
            }
            if member.reachable {
                reachable.entry(&member.name).or_insert((depth, member));
            }
            if member.kind == InheritedKind::TempTable {
                temp_tables
                    .entry(last_segment(member.name.as_atom()))
                    .or_default()
                    .push((depth, member));
            }
        }
    }
    order
        .into_iter()
        .map(|name| {
            let (at, chosen) = reachable.get(name).copied().unwrap_or((0, nearest[name]));
            let written = chosen.name.as_written().to_string();
            let binding = if !chosen.reachable {
                InheritedBinding::Unknown(written)
            } else {
                match (chosen.kind, &chosen.record) {
                    (InheritedKind::TempTable, _) => InheritedBinding::TempTable(written),
                    (InheritedKind::Buffer, None) => InheritedBinding::Unknown(written),
                    (InheritedKind::Buffer, Some(record)) => {
                        buffer_binding(&temp_tables, at, record, opaque)
                    }
                }
            };
            InheritedRecord {
                name: chosen.name.clone(),
                kind: chosen.kind,
                binding,
            }
        })
        .collect()
}

/// The table a buffer is `FOR`, declared at level `at` of the chain.
///
/// The declaring class binds the name: a temp-table of its own, or of a class
/// above it, hides a table of the schema. A temp-table of a class below it does
/// not. A name with a database qualifier is a table of the schema.
fn buffer_binding(
    temp_tables: &TempTables<'_>,
    at: usize,
    record: &MemberRecord,
    opaque: bool,
) -> InheritedBinding {
    let written = record.table.as_written().to_string();
    if record.temp_table {
        return InheritedBinding::TempTable(written);
    }
    if !written.contains('.') {
        let key = last_segment(record.table.as_atom());
        let hit = temp_tables
            .get(key)
            .and_then(|hits| hits.iter().find(|(depth, _)| *depth >= at));
        match hit {
            // A private one hides the table from a class above its own.
            Some((depth, m)) if *depth == at || m.reachable => {
                return InheritedBinding::TempTable(m.name.as_written().to_string());
            }
            Some(_) => return InheritedBinding::Unknown(written),
            None => {}
        }
    }
    if opaque && !written.contains('.') {
        InheritedBinding::Unknown(written)
    } else {
        InheritedBinding::Table(written)
    }
}

fn last_segment(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}
