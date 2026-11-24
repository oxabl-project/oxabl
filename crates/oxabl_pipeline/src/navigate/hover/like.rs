//! The fields and indexes a temp-table has, with what its `LIKE` clause
//! inherits.
//!
//! The source of a `LIKE` is the record that the analysis resolved for the
//! clause, so the hover and the diagnostics read the same table. A source that
//! the analysis cannot reach is a gap, never an empty table.

use oxabl_ast::{Statement, StatementKind, TempTableIndex, UseIndex};
use oxabl_semantic::{RecordSource, SymbolId};

use super::{Navigation, find_declaration};
use crate::schema_hover::{IndexView, index_lines, inline};

/// The longest `LIKE` chain that is followed.
const MAX_CHAIN: usize = 8;

/// The effective fields and indexes of a temp-table.
struct Shape {
    fields: usize,
    indexes: Vec<IndexView>,
}

/// Why the part that a `LIKE` inherits is not known.
struct Gap {
    /// The table that could not be read.
    name: String,
    reason: &'static str,
}

impl Navigation<'_, '_> {
    /// The text under the declaration of a temp-table: how many fields it has
    /// and its indexes. `symbol` is the temp-table's symbol; `None` for a
    /// declaration read from another file, whose `LIKE` source is not resolved
    /// here.
    pub(super) fn temp_table_shape(
        &self,
        statement: &Statement,
        symbol: Option<SymbolId>,
    ) -> Option<String> {
        let StatementKind::DefineTempTable {
            like_table,
            fields,
            indexes,
            ..
        } = &statement.kind
        else {
            return None;
        };
        let gap = match self.shape(statement, symbol, 0) {
            Ok(shape) => {
                let mut out = format!(
                    "Fields: {} · Indexes: {}",
                    shape.fields,
                    shape.indexes.len()
                );
                if !shape.indexes.is_empty() {
                    out.push_str("\n\nIndexes:");
                    index_lines(&mut out, &shape.indexes);
                }
                return Some(out);
            }
            Err(gap) => gap,
        };
        // The part that is written in this statement is still known.
        let source = like_table.as_ref()?.name.as_str();
        let why = if gap.name.eq_ignore_ascii_case(source) {
            gap.reason.to_string()
        } else {
            format!("{}: {}", inline(&gap.name), gap.reason)
        };
        let inherited = format!("those of {} ({why})", inline(source));
        let own: Vec<IndexView> = indexes.iter().map(own_index).collect();
        let mut out = format!(
            "Fields: {} own + {inherited} · Indexes: {} own + {inherited}",
            fields.len(),
            own.len()
        );
        if !own.is_empty() {
            out.push_str("\n\nOwn indexes:");
            index_lines(&mut out, &own);
        }
        Some(out)
    }

    /// The shape of the temp-table that `statement` declares, or the gap that
    /// stops it.
    fn shape(
        &self,
        statement: &Statement,
        symbol: Option<SymbolId>,
        depth: usize,
    ) -> Result<Shape, Gap> {
        let StatementKind::DefineTempTable {
            like_table,
            use_indexes,
            fields,
            indexes,
            ..
        } = &statement.kind
        else {
            return Err(Gap {
                name: String::new(),
                reason: "its declaration is not found",
            });
        };
        let own: Vec<IndexView> = indexes.iter().map(own_index).collect();
        let Some(like) = like_table else {
            return Ok(finish(fields.len(), Vec::new(), own));
        };
        let gap = |reason| Gap {
            name: like.name.clone(),
            reason,
        };
        let Some(symbol) = symbol else {
            return Err(gap("its source is not resolved from here"));
        };
        if depth >= MAX_CHAIN {
            return Err(gap("the LIKE chain does not end"));
        }
        let symbols = &self.semantic.symbols;
        let inherited = match symbols.temp_table_like(symbol) {
            Some(RecordSource::Schema(table)) => {
                let table = self
                    .run
                    .config()
                    .schema
                    .get_by_id(table)
                    .ok_or_else(|| gap("not in the loaded schema"))?;
                Shape {
                    fields: table.fields.len(),
                    indexes: table.indexes.iter().map(IndexView::from_schema).collect(),
                }
            }
            Some(RecordSource::TempTable(source)) => {
                let name_span = symbols.get(source).name_span;
                let found = find_declaration(&self.parsed().program.statements, name_span)
                    .ok_or_else(|| gap("its declaration is not found"))?;
                self.shape(found.statement, Some(source), depth + 1)?
            }
            None if self.run.config().schema.is_empty() => return Err(gap("no schema is loaded")),
            None => return Err(gap("not in the loaded schema")),
        };
        let inherited_indexes = if use_indexes.is_empty() {
            inherited.indexes
        } else {
            used_indexes(inherited.indexes, use_indexes)
        };
        Ok(finish(
            inherited.fields + fields.len(),
            inherited_indexes,
            own,
        ))
    }
}

/// The indexes of a table from its inherited and its own indexes. A table has
/// one primary index: an own one replaces an inherited one, and the first index
/// is the primary one when none is marked.
fn finish(fields: usize, mut inherited: Vec<IndexView>, own: Vec<IndexView>) -> Shape {
    if own.iter().any(|index| index.primary) {
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
    Shape {
        fields,
        indexes: inherited,
    }
}

/// The inherited indexes that `USE-INDEX` names, in the order written. The
/// one marked `AS PRIMARY` is the primary one; [`finish`] makes the first one
/// primary when none is.
fn used_indexes(inherited: Vec<IndexView>, used: &[UseIndex]) -> Vec<IndexView> {
    let mut taken: Vec<(IndexView, bool)> = used
        .iter()
        .filter_map(|wanted| {
            inherited
                .iter()
                .find(|index| index.name.eq_ignore_ascii_case(&wanted.name.name))
                .map(|index| (index.clone(), wanted.as_primary))
        })
        .collect();
    if let Some(chosen) = taken.iter().position(|(_, as_primary)| *as_primary) {
        for (at, (index, _)) in taken.iter_mut().enumerate() {
            index.primary = at == chosen;
        }
    }
    taken.into_iter().map(|(index, _)| index).collect()
}

fn own_index(index: &TempTableIndex) -> IndexView {
    IndexView {
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
