//! Records and fields: the tables of the schema and its databases, the fields
//! after `Customer.`, the tables after a database name, and the fields of the
//! records of a `WHERE`.
//!
//! The logical name of a database is the file name of its `.df` without the
//! extension, as the preprocessor names it when `[workspace.preprocessor]`
//! gives no databases.

use std::collections::HashSet;
use std::path::Path;

use oxabl_ast::NodeId;
use oxabl_lexer::{Kind, Token};
use oxabl_schema::{Field, Schema, TableId, fold_atom};
use oxabl_semantic::{
    ClassLattice, NamespaceId, RecordSource, ResolvedType, ScopeKind, SymbolId, assignable,
};

use super::context::RecordsAfter;
use super::names::Wanted;
use super::rank::{fit, nearness};
use super::snippets::SnippetDef;
use super::{Candidate, CompletionBase, Engine, ItemData, ItemKind, base};
use crate::schema_hover::type_name;

/// The longest `LIKE` chain of temp-tables that is followed.
const MAX_LIKE_CHAIN: usize = 8;

/// The most tables a snippet offers as a choice.
const MAX_CHOICE_TABLES: usize = 30;

/// What a record name stands for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Record {
    Table(TableId),
    TempTable(SymbolId),
}

/// The databases of `schema`: the logical name of each `.df` that defines a
/// table, and its path, in the order of their first table.
fn databases(schema: &Schema) -> Vec<(String, &Path)> {
    let mut found: Vec<(String, &Path)> = Vec::new();
    for (id, _) in schema.tables() {
        if let Some((path, _)) = schema.table_source(id)
            && !found.iter().any(|(_, known)| *known == path)
            && let Some(name) = path.file_stem().and_then(|stem| stem.to_str())
        {
            found.push((name.to_string(), path));
        }
    }
    found
}

/// The type of a schema field, its extent and its format, as an item's detail.
fn field_detail(field: &Field) -> Option<String> {
    let mut parts: Vec<String> = Vec::new();
    parts.extend(type_name(&field.data_type));
    match field.extent {
        Some(0) => parts.push("EXTENT".to_string()),
        Some(n) => parts.push(format!("EXTENT {n}")),
        None => {}
    }
    if let Some(format) = field.format.as_deref().filter(|f| !f.trim().is_empty()) {
        parts.push(format!("FORMAT \"{format}\""));
    }
    (!parts.is_empty()).then(|| parts.join(" "))
}

impl Engine<'_, '_, '_, '_> {
    /// The schema completion reads: the base's, whose table ids the symbols of
    /// the base hold, else the request's.
    pub(super) fn schema(&self) -> &Schema {
        match self.base {
            Some(base) => &base.navigation.run.config().schema,
            None => &self.request.config.schema,
        }
    }

    /// The tables of the schema a record position takes, and the databases
    /// when there are several. A table of the OpenEdge metaschema (`_file`,
    /// ...) only when the word starts with `_`.
    pub(super) fn tables(&self, wanted: Wanted) -> Vec<Candidate> {
        if matches!(wanted, Wanted::Records(RecordsAfter::BufferForTempTable)) {
            return Vec::new();
        }
        let schema = self.schema();
        let databases = databases(schema);
        let several = databases.len() > 1;
        let word = &self.request.text[self.found.word_start as usize..self.request.offset as usize];
        let mut items: Vec<Candidate> = schema
            .tables()
            .filter(|(_, table)| !table.name.as_ref().starts_with('_') || word.starts_with('_'))
            .map(|(id, table)| {
                let database = several
                    .then(|| schema.table_source(id))
                    .flatten()
                    .and_then(|(path, _)| databases.iter().find(|(_, known)| *known == path))
                    .map(|(name, _)| name.as_str());
                table_item(id, &table.display_name, database)
            })
            .collect();
        if word.starts_with('_') && schema.metaschema_enabled() {
            items.extend(
                oxabl_schema::metaschema()
                    .tables()
                    .filter_map(|(_, table)| {
                        let id = schema.table_id(&table.name)?;
                        // A table of the `.df` with the same name is listed.
                        std::ptr::eq(schema.get_by_id(id)?, table)
                            .then(|| table_item(id, &table.display_name, None))
                    }),
            );
        }
        if several
            && matches!(
                wanted,
                Wanted::Values
                    | Wanted::StatementStart
                    | Wanted::Records(RecordsAfter::Record | RecordsAfter::Like)
            )
        {
            items.extend(databases.iter().map(|(name, _)| Candidate {
                label: name.clone(),
                insert: name.clone(),
                kind: ItemKind::Module,
                detail: Some("database".to_string()),
                snippet: false,
                data: ItemData::None,
                fit: fit::OTHER,
                nearness: nearness::SCHEMA,
            }));
        }
        items
    }

    /// The items after `qualifier.`: the fields of a record, or the tables of
    /// a database. Right after a record keyword only a database qualifies.
    /// `None` when the qualifier is neither, or both.
    pub(super) fn qualified(
        &self,
        (from, to): (u32, u32),
        after_record: bool,
    ) -> Option<(Vec<Candidate>, bool)> {
        let name = &self.request.text[from as usize..to as usize];
        let schema = self.schema();
        let databases = databases(schema);
        let database = |name: &str| {
            databases
                .iter()
                .find(|(known, _)| known.eq_ignore_ascii_case(name))
                .map(|(_, path)| *path)
        };
        let parts: Vec<&str> = name.split('.').collect();
        match parts.as_slice() {
            [db] if after_record => Some((self.database_tables(database(db)?), false)),
            [one] => {
                self.based()?;
                match (self.record_named(one), database(one)) {
                    (Some(record), None) => self.fields(record, None),
                    (None, Some(path)) => {
                        Some((self.database_tables(path), self.base_incomplete()))
                    }
                    _ => None,
                }
            }
            [db, table] if !after_record => {
                let id = table_of(schema, database(db)?, table)?;
                self.fields(Record::Table(id), None)
            }
            _ => None,
        }
    }

    /// The tables the `.df` at `path` defines.
    fn database_tables(&self, path: &Path) -> Vec<Candidate> {
        let schema = self.schema();
        schema
            .tables()
            .filter(|(id, _)| schema.table_source(*id).is_some_and(|(of, _)| of == path))
            .map(|(id, table)| table_item(id, &table.display_name, None))
            .collect()
    }

    /// The record that `name` stands for at the cursor: a buffer or
    /// temp-table the cursor can see, else a table of the schema (also
    /// `db.table`). `None` when it is none, or not the same at each place of
    /// the cursor.
    pub(super) fn record_named(&self, name: &str) -> Option<Record> {
        if let Some((db, table)) = name.split_once('.') {
            let schema = self.schema();
            let databases = databases(schema);
            let (_, path) = databases
                .iter()
                .find(|(known, _)| known.eq_ignore_ascii_case(db))?;
            return table_of(schema, path, table).map(Record::Table);
        }
        let (base, mapped) = self.based()?;
        let mut found: Option<Option<Record>> = None;
        for place in &mapped.places {
            let record = self.record_at(base, *place, name);
            match found {
                None => found = Some(record),
                Some(known) if known != record => return None,
                Some(_) => {}
            }
        }
        found.flatten()
    }

    fn record_at(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        virt: u32,
        name: &str,
    ) -> Option<Record> {
        let semantic = &base.navigation.model.semantic;
        let tree = &semantic.scope_tree;
        let symbols = &semantic.symbols;
        let atom = fold_atom(name);
        for id in tree.ancestors(base::scope_at(base, virt)) {
            let held = tree.get(id);
            let members = matches!(held.kind, ScopeKind::Class | ScopeKind::Interface);
            let Some(sym) = held.get_in(NamespaceId::Buffers, &atom) else {
                continue;
            };
            let symbol = symbols.get(sym);
            // A name the cursor cannot see does not hide an outer one.
            if symbol.declaration == NodeId::DUMMY
                || !(members || symbol.name_span.end <= virt)
                || self
                    .mapped
                    .is_some_and(|m| m.in_changed(base, symbol.name_span.start))
            {
                continue;
            }
            return match symbols.record_source(sym)? {
                RecordSource::Schema(table) => Some(Record::Table(table)),
                RecordSource::TempTable(temp_table) => Some(Record::TempTable(temp_table)),
            };
        }
        self.schema().resolve_table(&atom).map(Record::Table)
    }

    /// The fields of `record`, and whether some can be missing. A field
    /// whose type fits `expected` comes first.
    pub(super) fn fields(
        &self,
        record: Record,
        expected: Option<&ResolvedType>,
    ) -> Option<(Vec<Candidate>, bool)> {
        let mut items = Vec::new();
        let incomplete = match record {
            Record::Table(id) => {
                self.schema_fields(id, expected, &mut items)?;
                false
            }
            Record::TempTable(temp_table) => {
                let (base, _) = self.based()?;
                self.temp_table_fields(base, temp_table, expected, 0, &mut items)
                    || self.base_incomplete()
            }
        };
        let mut seen = HashSet::new();
        items.retain(|item| seen.insert(item.label.to_ascii_lowercase()));
        Some((items, incomplete))
    }

    fn schema_fields(
        &self,
        id: TableId,
        expected: Option<&ResolvedType>,
        items: &mut Vec<Candidate>,
    ) -> Option<()> {
        let table = self.schema().get_by_id(id)?;
        items.extend(table.fields.iter().enumerate().map(|(at, field)| {
            let fits = expected.is_some_and(|expected| {
                self.fits(&ResolvedType::from_schema_field(field), expected)
            });
            Candidate {
                label: field.display_name.clone(),
                insert: field.display_name.clone(),
                kind: ItemKind::Field,
                detail: field_detail(field),
                snippet: false,
                data: ItemData::Field {
                    table: id.raw(),
                    field: at as u32,
                },
                fit: if fits { fit::FITS } else { fit::OTHER },
                nearness: nearness::SCHEMA,
            }
        }));
        Some(())
    }

    /// The fields of a temp-table: those its `LIKE` clause gives, then its
    /// own. True when some can be missing: a `LIKE` source oxabl does not
    /// know.
    fn temp_table_fields(
        &self,
        base: &CompletionBase<'_, '_, '_>,
        temp_table: SymbolId,
        expected: Option<&ResolvedType>,
        depth: usize,
        items: &mut Vec<Candidate>,
    ) -> bool {
        let semantic = &base.navigation.model.semantic;
        let symbols = &semantic.symbols;
        let tree = &semantic.scope_tree;
        let symbol = symbols.get(temp_table);
        if symbol.declaration == NodeId::DUMMY {
            return true;
        }
        let mut incomplete = match symbols.temp_table_like(temp_table) {
            Some(RecordSource::Schema(id)) => self.schema_fields(id, expected, items).is_none(),
            Some(RecordSource::TempTable(source)) if depth < MAX_LIKE_CHAIN => {
                self.temp_table_fields(base, source, expected, depth + 1, items)
            }
            Some(RecordSource::TempTable(_)) => true,
            None => declares_like(&base.navigation.expanded.text, symbol.name_span.end),
        };
        let Some(scope) = symbols.temp_table_fields(temp_table) else {
            return incomplete;
        };
        let schema = self.schema();
        let mut own: Vec<SymbolId> = tree.get(scope).bindings[NamespaceId::Values.index()]
            .iter()
            .map(|(_, sym)| sym)
            .collect();
        own.sort_unstable_by_key(|sym| sym.raw());
        for sym in own {
            let field = symbols.get(sym);
            if self
                .mapped
                .is_some_and(|m| m.in_changed(base, field.name_span.start))
            {
                incomplete = true;
                continue;
            }
            let Some(label) = self.spelling(base, field) else {
                continue;
            };
            let ty = field
                .data_type
                .as_ref()
                .filter(|ty| !matches!(ty, ResolvedType::Unknown | ResolvedType::Error));
            let fits =
                matches!((ty, expected), (Some(ty), Some(expected)) if self.fits(ty, expected));
            items.push(Candidate {
                label: label.to_string(),
                insert: label.to_string(),
                kind: ItemKind::Field,
                detail: ty.map(|ty| {
                    ty.display_abl(symbols, schema)
                        .to_string()
                        .to_ascii_uppercase()
                }),
                snippet: false,
                data: ItemData::Symbol(sym.raw()),
                fit: if fits { fit::FITS } else { fit::OTHER },
                nearness: nearness::SCOPE,
            });
        }
        incomplete
    }

    /// Whether a value of type `ty` fits a place that expects `expected`.
    fn fits(&self, ty: &ResolvedType, expected: &ResolvedType) -> bool {
        self.base.is_some_and(|base| {
            assignable(
                ty,
                expected,
                ClassLattice::new(&base.navigation.model.semantic.symbols),
            )
        })
    }

    /// The type of field `name` of `record`.
    fn field_type(&self, record: Record, name: &str, depth: usize) -> Option<ResolvedType> {
        match record {
            Record::Table(id) => Some(ResolvedType::from_schema_field(
                self.schema().get_by_id(id)?.field(name)?,
            )),
            Record::TempTable(temp_table) => {
                let (base, _) = self.based()?;
                let semantic = &base.navigation.model.semantic;
                let symbols = &semantic.symbols;
                if let Some(scope) = symbols.temp_table_fields(temp_table)
                    && let Some(sym) = semantic
                        .scope_tree
                        .get(scope)
                        .get_in(NamespaceId::Values, &fold_atom(name))
                {
                    return symbols.get(sym).data_type.clone();
                }
                match symbols.temp_table_like(temp_table)? {
                    RecordSource::Schema(id) => self.field_type(Record::Table(id), name, depth),
                    RecordSource::TempTable(source) if depth < MAX_LIKE_CHAIN => {
                        self.field_type(Record::TempTable(source), name, depth + 1)
                    }
                    RecordSource::TempTable(_) => None,
                }
            }
        }
    }

    /// An expression in the `WHERE` of a record phrase: the fields of the
    /// records the statement names, then the values. A field is inserted
    /// without a qualifier only when no other record the cursor can reach has
    /// a field of that name; else the compiler would find the name ambiguous,
    /// and it is inserted with the name of its record (`Order.CustNum`), once
    /// for each record of the statement that has it.
    pub(super) fn where_names(&self) -> Option<(Vec<Candidate>, bool)> {
        let text = self.request.text;
        let window: Vec<&Token> = self.tokens[..self.found.before]
            .iter()
            .filter(|token| {
                token.start as u32 >= self.found.window_start && token.kind != Kind::Comment
            })
            .collect();
        let mut incomplete = false;
        let mut records: Vec<(&str, Record)> = Vec::new();
        for name in statement_records(text, &window) {
            match self.record_named(name) {
                Some(record) => records.push((name, record)),
                None => incomplete = true,
            }
        }
        let only: Vec<Record> = records.iter().map(|(_, record)| *record).collect();
        let expected = self.compared_type(text, &window, &only);
        let (mut items, names_incomplete) = self.names_with(Wanted::Values, expected.as_ref())?;
        incomplete |= names_incomplete;
        let mut lists = Vec::new();
        for (name, record) in &records {
            let Some((fields, fields_incomplete)) = self.fields(*record, expected.as_ref()) else {
                incomplete = true;
                continue;
            };
            incomplete |= fields_incomplete;
            lists.push((*name, *record, fields));
        }
        // The fields of each other record the cursor can reach; `None` for a
        // record whose fields are not all known, which can have any name.
        let mut reach: Vec<(Option<Record>, Option<HashSet<String>>)> = Vec::new();
        let mut add = |record: Option<Record>, engine: &Self| {
            let Some(record) = record else {
                reach.push((None, None));
                return;
            };
            if reach.iter().any(|(known, _)| *known == Some(record)) {
                return;
            }
            let names = engine
                .fields(record, None)
                .and_then(|(fields, incomplete)| {
                    (!incomplete).then(|| {
                        fields
                            .iter()
                            .map(|field| field.label.to_ascii_lowercase())
                            .collect()
                    })
                });
            reach.push((Some(record), names));
        };
        for (_, record, _) in &lists {
            add(Some(*record), self);
        }
        for record in self.records_in_reach() {
            add(record, self);
        }
        let mut seen = HashSet::new();
        for (name, record, fields) in lists {
            for field in fields {
                let key = field.label.to_ascii_lowercase();
                let shared = reach.iter().any(|(other, names)| {
                    *other != Some(record)
                        && names.as_ref().is_none_or(|names| names.contains(&key))
                });
                let fit = field.fit.min(fit::STATEMENT_FIELD);
                if shared {
                    let qualified = format!("{name}.{}", field.label);
                    if seen.insert(qualified.to_ascii_lowercase()) {
                        items.push(Candidate {
                            label: qualified.clone(),
                            insert: qualified,
                            fit,
                            ..field
                        });
                    }
                } else if seen.insert(key) {
                    items.push(Candidate { fit, ..field });
                }
            }
        }
        Some((items, incomplete))
    }

    /// The records of the buffers bound in the scope at the cursor and its
    /// ancestors, at each place of the cursor; `None` for a buffer whose
    /// record is not known.
    fn records_in_reach(&self) -> Vec<Option<Record>> {
        let Some((base, mapped)) = self.based() else {
            return Vec::new();
        };
        let semantic = &base.navigation.model.semantic;
        let tree = &semantic.scope_tree;
        let symbols = &semantic.symbols;
        let mut out = Vec::new();
        for place in &mapped.places {
            for id in tree.ancestors(base::scope_at(base, *place)) {
                for (_, sym) in tree.get(id).bindings[NamespaceId::Buffers.index()].iter() {
                    out.push(symbols.record_source(sym).map(|source| match source {
                        RecordSource::Schema(table) => Record::Table(table),
                        RecordSource::TempTable(temp_table) => Record::TempTable(temp_table),
                    }));
                }
            }
        }
        out
    }

    /// The type of the field on the left of the comparison the window ends
    /// with (`Customer.CustNum >`, or `CustNum =` for a field of a record of
    /// the statement).
    fn compared_type(
        &self,
        text: &str,
        window: &[&Token],
        records: &[Record],
    ) -> Option<ResolvedType> {
        let (last, before) = window.split_last()?;
        if !is_comparison(last.kind) {
            return None;
        }
        let spelled = |token: &Token| &text[token.start..token.end];
        match before {
            [.., q, dot, f]
                if q.kind == Kind::Identifier
                    && dot.kind == Kind::Period
                    && q.end == dot.start
                    && dot.end == f.start =>
            {
                let mut from = q.start;
                // `db.table.field`
                if let [.., db, db_dot, _, _, _] = before
                    && db.kind == Kind::Identifier
                    && db_dot.kind == Kind::Period
                    && db.end == db_dot.start
                    && db_dot.end == q.start
                {
                    from = db.start;
                }
                let record = self.record_named(&text[from..q.end])?;
                self.field_type(record, spelled(f), 0)
            }
            [.., f] if f.kind == Kind::Identifier => {
                // A name that two records of the statement have is ambiguous.
                let mut types = records
                    .iter()
                    .filter_map(|record| self.field_type(*record, spelled(f), 0));
                let ty = types.next()?;
                types.next().is_none().then_some(ty)
            }
            _ => None,
        }
    }

    /// `body` with the table placeholder of the `find` and `foreach` snippets
    /// made a choice of the tables of the schema, when it has at most
    /// [`MAX_CHOICE_TABLES`].
    pub(super) fn table_choice(&self, snippet: &SnippetDef, body: String) -> String {
        const PLACEHOLDER: &str = "${1:Customer}";
        if !matches!(snippet.prefix, "find" | "foreach") || !body.contains(PLACEHOLDER) {
            return body;
        }
        let mut names: Vec<&str> = self
            .schema()
            .tables()
            .map(|(_, table)| table.display_name.as_str())
            .collect();
        if names.is_empty() || names.len() > MAX_CHOICE_TABLES {
            return body;
        }
        names.sort_by_key(|name| name.to_ascii_lowercase());
        let escaped: Vec<String> = names
            .iter()
            .map(|name| {
                name.replace('\\', "\\\\")
                    .replace(',', "\\,")
                    .replace('|', "\\|")
            })
            .collect();
        let choice = format!("${{1|{}|}}", escaped.join(","));
        // The first place is the choice, the others mirror it.
        body.replacen(PLACEHOLDER, &choice, 1)
            .replace(PLACEHOLDER, "$1")
    }
}

/// The item of a schema table, with its database when there are several.
fn table_item(id: TableId, name: &str, database: Option<&str>) -> Candidate {
    Candidate {
        label: name.to_string(),
        insert: name.to_string(),
        kind: ItemKind::Struct,
        detail: Some(match database {
            Some(database) => format!("Database table, {database}"),
            None => "Database table".to_string(),
        }),
        snippet: false,
        data: ItemData::Table(id.raw()),
        fit: fit::OTHER,
        nearness: nearness::SCHEMA,
    }
}

/// The table `name` that the `.df` at `path` defines.
fn table_of(schema: &Schema, path: &Path, name: &str) -> Option<TableId> {
    let atom = fold_atom(name);
    schema
        .tables()
        .find(|(id, table)| {
            table.name == atom && schema.table_source(*id).is_some_and(|(of, _)| of == path)
        })
        .map(|(id, _)| id)
}

/// The record names of a statement window: the name after `FIND [FIRST|...]`,
/// after `EACH`, `FIRST` or `LAST` of a `FOR`, `PRESELECT` or `OPEN QUERY`
/// header, and after `CAN-FIND(`, with a database qualifier when it has one.
fn statement_records<'t>(text: &'t str, window: &[&Token]) -> Vec<&'t str> {
    let mut names = Vec::new();
    for (at, token) in window.iter().enumerate() {
        let before = at.checked_sub(1).map(|i| window[i].kind);
        let opens = match token.kind {
            Kind::Find => true,
            Kind::First | Kind::Last | Kind::Next | Kind::Prev | Kind::Current => matches!(
                before,
                Some(Kind::Find | Kind::KwFor | Kind::Comma | Kind::Preselect | Kind::LeftParen)
            ),
            Kind::Each => true,
            Kind::LeftParen => before == Some(Kind::CanFind),
            _ => false,
        };
        let Some(name) = window.get(at + 1).filter(|t| t.kind == Kind::Identifier) else {
            continue;
        };
        if !opens {
            continue;
        }
        let mut end = name.end;
        // `db.table`
        if let (Some(dot), Some(table)) = (window.get(at + 2), window.get(at + 3))
            && dot.kind == Kind::Period
            && dot.start == name.end
            && table.start == dot.end
            && table.kind == Kind::Identifier
        {
            end = table.end;
        }
        names.push(&text[name.start..end]);
    }
    names
}

/// Whether `kind` compares its two operands.
fn is_comparison(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Equals
            | Kind::LessThan
            | Kind::GreaterThan
            | Kind::GreaterThanOrEqual
            | Kind::LessThanOrEqual
            | Kind::NotEqual
            | Kind::Eq
            | Kind::Ne
            | Kind::Lt
            | Kind::Le
            | Kind::Gt
            | Kind::Ge
            | Kind::Begins
            | Kind::Matches
            | Kind::Contains
    )
}

/// Whether the `DEFINE TEMP-TABLE` whose name ends at `name_end` of `text`
/// has a `LIKE` clause: a `LIKE` before its first `FIELD` or `INDEX`.
fn declares_like(text: &str, name_end: u32) -> bool {
    let from = (name_end as usize).min(text.len());
    let mut to = (from + 4096).min(text.len());
    while !text.is_char_boundary(to) {
        to -= 1;
    }
    oxabl_lexer::tokenize(&text[from..to])
        .iter()
        .map(|token| token.kind)
        .find(|kind| {
            matches!(
                kind,
                Kind::Like | Kind::LikeSequential | Kind::Field | Kind::Index | Kind::Period
            )
        })
        .is_some_and(|kind| matches!(kind, Kind::Like | Kind::LikeSequential))
}
