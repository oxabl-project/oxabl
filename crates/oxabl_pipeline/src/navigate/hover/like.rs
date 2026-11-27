//! The fields and indexes a temp-table has, with what its `LIKE` clause
//! inherits.
//!
//! The source of a `LIKE` is the record that the analysis resolved for the
//! clause, so the hover and the diagnostics read the same table. A source that
//! the analysis cannot reach is a gap, never an empty table.

use oxabl_ast::{AstPools, Statement, StatementKind};
use oxabl_schema::{EffectiveIndex, EffectiveTable};
use oxabl_semantic::{RecordSource, SymbolId};

use super::{Navigation, find_declaration};
use crate::schema_hover::{index_lines, inline};

/// The longest `LIKE` chain that is followed.
const MAX_CHAIN: usize = 8;

/// Why the part that a `LIKE` inherits is not known.
struct Gap {
    /// The table that could not be read.
    name: String,
    reason: &'static str,
}

impl Navigation<'_, '_> {
    /// The text under the declaration of a temp-table: how many fields it has
    /// and its indexes. `pools` are those of the parse that holds `statement`.
    /// `symbol` is the temp-table's symbol; `None` for a declaration read from
    /// another file, whose `LIKE` source is not resolved here.
    pub(super) fn temp_table_shape(
        &self,
        statement: &Statement,
        pools: &AstPools,
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
        let gap = match self.shape(statement, pools, symbol, 0) {
            Ok(shape) => {
                let mut out = format!(
                    "Fields: {} · Indexes: {}",
                    shape.fields.len(),
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
        let source = pools.ident(*like_table.as_ref()?);
        let why = if gap.name.eq_ignore_ascii_case(source) {
            gap.reason.to_string()
        } else {
            format!("{}: {}", inline(&gap.name), gap.reason)
        };
        let inherited = format!("those of {} ({why})", inline(source));
        let own: Vec<EffectiveIndex> = indexes
            .iter()
            .map(|index| EffectiveIndex::from_temp_table(index, &pools.names))
            .collect();
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
        pools: &AstPools,
        symbol: Option<SymbolId>,
        depth: usize,
    ) -> Result<EffectiveTable, Gap> {
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
        let fields = fields.iter().map(|field| pools.ident(field.name));
        let Some(like) = like_table else {
            return Ok(EffectiveTable::declare(
                None,
                &[],
                fields,
                indexes,
                &pools.names,
            ));
        };
        let gap = |reason| Gap {
            name: pools.ident(*like).to_string(),
            reason,
        };
        let Some(symbol) = symbol else {
            return Err(gap("its source is not resolved from here"));
        };
        if depth >= MAX_CHAIN {
            return Err(gap("the LIKE chain does not end"));
        }
        let symbols = &self.model.semantic.symbols;
        let inherited = match symbols.temp_table_like(symbol) {
            Some(RecordSource::Schema(table)) => {
                let table = self
                    .run
                    .config()
                    .schema
                    .get_by_id(table)
                    .ok_or_else(|| gap("not in the loaded schema"))?;
                EffectiveTable::from_schema(table)
            }
            Some(RecordSource::TempTable(source)) => {
                let name_span = symbols.get(source).name_span;
                let program = &self.parsed().program;
                let found = find_declaration(&program.statements, name_span)
                    .ok_or_else(|| gap("its declaration is not found"))?;
                self.shape(found.statement, &program.pools, Some(source), depth + 1)?
            }
            None if self.run.config().schema.is_empty() => return Err(gap("no schema is loaded")),
            None => return Err(gap("not in the loaded schema")),
        };
        Ok(EffectiveTable::declare(
            Some(inherited),
            use_indexes,
            fields,
            indexes,
            &pools.names,
        ))
    }
}
