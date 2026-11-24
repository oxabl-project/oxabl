//! The fields and indexes a temp-table has, with what its `LIKE` clause
//! inherits.
//!
//! This is the one definition of those rules; the hover and the effects
//! analysis both read it. The rules are the ones OpenEdge 12.8.9 applies:
//!
//! - `LIKE src` takes all fields and indexes of `src`.
//! - `USE-INDEX` takes only the named indexes, in the order of `src`. The one
//!   marked `AS PRIMARY` is primary. With none marked, the primary index of
//!   `src` stays primary when it is taken.
//! - An own `INDEX` is added to the inherited ones. An own `IS PRIMARY` index
//!   replaces the inherited primary index.
//! - A table has one primary index. When none is marked, the first is.

use oxabl_ast::{TempTableIndex, UseIndex};

use crate::schema::{Index, Table};

/// One index of a temp-table or a database table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectiveIndex {
    pub name: String,
    pub primary: bool,
    pub unique: bool,
    pub word: bool,
    pub inactive: bool,
    /// The names of the fields, as written.
    pub fields: Vec<String>,
}

impl EffectiveIndex {
    pub fn from_schema(index: &Index) -> EffectiveIndex {
        EffectiveIndex {
            name: index.display_name.clone(),
            primary: index.primary,
            unique: index.unique,
            word: index.word,
            inactive: index.inactive,
            fields: index
                .fields
                .iter()
                .map(|part| part.display_name.clone())
                .collect(),
        }
    }

    pub fn from_temp_table(index: &TempTableIndex) -> EffectiveIndex {
        EffectiveIndex {
            name: index.name.name.clone(),
            primary: index.is_primary,
            unique: index.is_unique,
            word: index.is_word_index,
            inactive: false,
            fields: index
                .fields
                .iter()
                .map(|field| field.name.name.clone())
                .collect(),
        }
    }
}

/// The effective fields and indexes of a table.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EffectiveTable {
    /// The names of the fields, as written.
    pub fields: Vec<String>,
    pub indexes: Vec<EffectiveIndex>,
}

impl EffectiveTable {
    /// A database table, the source of a `LIKE`.
    pub fn from_schema(table: &Table) -> EffectiveTable {
        EffectiveTable {
            fields: table
                .fields
                .iter()
                .map(|field| field.display_name.clone())
                .collect(),
            indexes: table
                .indexes
                .iter()
                .map(EffectiveIndex::from_schema)
                .collect(),
        }
    }

    /// The temp-table that a `DEFINE TEMP-TABLE` declares. `source` is the
    /// effective table of its `LIKE` clause, and `None` when the statement has
    /// none. The caller decides what an unknown source means: it must not pass
    /// an empty table for one.
    pub fn declare<'a>(
        source: Option<EffectiveTable>,
        use_indexes: &[UseIndex],
        fields: impl IntoIterator<Item = &'a str>,
        indexes: &[TempTableIndex],
    ) -> EffectiveTable {
        let (mut all_fields, mut inherited) = match source {
            Some(source) if use_indexes.is_empty() => (source.fields, source.indexes),
            Some(source) => (source.fields, used_indexes(source.indexes, use_indexes)),
            None => (Vec::new(), Vec::new()),
        };
        all_fields.extend(fields.into_iter().map(str::to_string));
        let own = indexes.iter().map(EffectiveIndex::from_temp_table);
        let own_primary = indexes.iter().any(|index| index.is_primary);
        if own_primary {
            for index in &mut inherited {
                index.primary = false;
            }
        }
        inherited.extend(own);
        if !inherited.iter().any(|index| index.primary)
            && let Some(first) = inherited.first_mut()
        {
            first.primary = true;
        }
        EffectiveTable {
            fields: all_fields,
            indexes: inherited,
        }
    }
}

/// The inherited indexes that `USE-INDEX` names, in the order of the source
/// table.
fn used_indexes(inherited: Vec<EffectiveIndex>, used: &[UseIndex]) -> Vec<EffectiveIndex> {
    let named = |index: &EffectiveIndex, only_primary: bool| {
        used.iter().any(|wanted| {
            index.name.eq_ignore_ascii_case(&wanted.name.name)
                && (!only_primary || wanted.as_primary)
        })
    };
    let chosen = inherited.iter().position(|index| named(index, true));
    inherited
        .into_iter()
        .enumerate()
        .filter(|(_, index)| named(index, false))
        .map(|(at, mut index)| {
            if chosen.is_some() {
                index.primary = Some(at) == chosen;
            }
            index
        })
        .collect()
}
