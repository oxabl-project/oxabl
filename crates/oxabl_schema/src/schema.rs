use oxabl_common::FileSpan;
use oxabl_lexer::oxabl_atom::OxablAtom;
use rustc_hash::FxHashMap;

use crate::atom::fold_atom;

/// Monotonic identifier for a `Schema` snapshot.
///
/// Semantic-layer results tagged with a `SchemaRevision` become stale after a
/// reload. Constructed only by `Schema` — external callers receive opaque
/// values for comparison and display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SchemaRevision(u32);

impl SchemaRevision {
    pub(crate) const fn new(value: u32) -> Self {
        SchemaRevision(value)
    }

    pub(crate) fn bump(self) -> Self {
        debug_assert!(self.0 < u32::MAX, "SchemaRevision wraparound");
        SchemaRevision(self.0.saturating_add(1))
    }

    /// Stable integer for serialization and display. Do not fabricate.
    pub fn raw(self) -> u32 {
        self.0
    }
}

/// Dense identifier for a `Table` within a single `Schema`. Values are stable
/// within one `Schema` but must not be shared across `SchemaRevision`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TableId(u32);

impl TableId {
    #[inline]
    pub const fn new(value: u32) -> Self {
        TableId(value)
    }

    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// Primitive `.df` field data type.
///
/// Non-standard or unrecognised type spellings round-trip under `Unknown` so
/// that `.df` format drift never hard-errors the loader. `Error` is produced
/// only by the loader when two merged `.df` files disagree on a field type
/// (`SCHEMA0012`) — downstream consumers treat an `Error`-typed field as
/// poisoned.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaType {
    Integer,
    Int64,
    Decimal,
    Character,
    Longchar,
    Logical,
    Date,
    Datetime,
    DatetimeTz,
    Handle,
    Raw,
    Recid,
    Rowid,
    Blob,
    Clob,
    Unknown(OxablAtom),
    Error,
}

impl SchemaType {
    /// Classify a `.df` `AS <type>` token. ASCII case-insensitive; unknown
    /// spellings round-trip as `Unknown`.
    pub fn classify(token: &str) -> Self {
        // Exhaustive match over the fourteen Progress primitives observable
        // in `.df` dumps. Ordered alphabetically for legibility; the compiler
        // lowers this to a jump table.
        let atom = fold_atom(token);
        if atom == fold_atom("integer") {
            SchemaType::Integer
        } else if atom == fold_atom("int64") {
            SchemaType::Int64
        } else if atom == fold_atom("decimal") {
            SchemaType::Decimal
        } else if atom == fold_atom("character") {
            SchemaType::Character
        } else if atom == fold_atom("longchar") {
            SchemaType::Longchar
        } else if atom == fold_atom("logical") {
            SchemaType::Logical
        } else if atom == fold_atom("date") {
            SchemaType::Date
        } else if atom == fold_atom("datetime") {
            SchemaType::Datetime
        } else if atom == fold_atom("datetime-tz") {
            SchemaType::DatetimeTz
        } else if atom == fold_atom("handle") {
            SchemaType::Handle
        } else if atom == fold_atom("raw") {
            SchemaType::Raw
        } else if atom == fold_atom("recid") {
            SchemaType::Recid
        } else if atom == fold_atom("rowid") {
            SchemaType::Rowid
        } else if atom == fold_atom("blob") {
            SchemaType::Blob
        } else if atom == fold_atom("clob") {
            SchemaType::Clob
        } else {
            SchemaType::Unknown(atom)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: OxablAtom,
    pub display_name: String,
    pub data_type: SchemaType,
    pub extent: Option<u32>,
    pub mandatory: bool,
    pub case_sensitive: bool,
    pub format: Option<String>,
    pub label: Option<String>,
    pub initial: Option<String>,
    pub description: Option<String>,
    pub decimals: Option<u32>,
    pub position: Option<u32>,
    pub order: Option<u32>,
    pub max_width: Option<u32>,
    pub help: Option<String>,
    pub valexp: Option<String>,
    pub valmsg: Option<String>,
    /// Round-tripped unknown or rarely-used attributes. Preserves their raw
    /// textual representation; the loader never fails on drift.
    pub extras: Vec<(OxablAtom, String)>,
    pub source: FileSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexField {
    pub name: OxablAtom,
    pub display_name: String,
    pub ascending: bool,
    pub abbreviated: bool,
    pub unsorted: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Index {
    pub name: OxablAtom,
    pub display_name: String,
    pub unique: bool,
    pub primary: bool,
    pub word: bool,
    pub inactive: bool,
    pub area: Option<String>,
    pub description: Option<String>,
    pub fields: Vec<IndexField>,
    pub source: FileSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    pub name: OxablAtom,
    pub display_name: String,
    pub fields: Vec<Field>,
    pub indexes: Vec<Index>,
    pub area: Option<String>,
    pub description: Option<String>,
    pub dump_name: Option<String>,
    pub frozen: bool,
    pub hidden: bool,
    pub label: Option<String>,
    pub valexp: Option<String>,
    pub valmsg: Option<String>,
    pub source: FileSpan,
}

impl Table {
    /// Case-insensitive lookup of a field within this table.
    pub fn get_field(&self, name: &OxablAtom) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == *name)
    }

    /// Case-insensitive lookup of an index within this table.
    pub fn get_index(&self, name: &OxablAtom) -> Option<&Index> {
        self.indexes.iter().find(|i| i.name == *name)
    }
}

/// In-memory schema populated from one or more `.df` files.
///
/// Tables are keyed case-insensitively via `OxablAtom`. `TableId` values are
/// dense and stable within a single `Schema`; cross-schema use requires a
/// matching `SchemaRevision`.
#[derive(Debug, Clone)]
pub struct Schema {
    revision: SchemaRevision,
    tables: FxHashMap<OxablAtom, TableId>,
    arena: Vec<Table>,
}

impl Schema {
    /// Construct an empty schema. `schema_loaded` in the semantic layer is
    /// driven off `is_empty`, so an empty schema is the equivalent of
    /// "schema absent" and silences schema-dependent diagnostics.
    pub fn empty() -> Self {
        Schema {
            revision: SchemaRevision::new(0),
            tables: FxHashMap::default(),
            arena: Vec::new(),
        }
    }

    pub fn revision(&self) -> SchemaRevision {
        self.revision
    }

    pub fn is_empty(&self) -> bool {
        self.arena.is_empty()
    }

    pub fn len(&self) -> usize {
        self.arena.len()
    }

    /// Case-insensitive lookup by folded atom.
    pub fn get(&self, name: &OxablAtom) -> Option<&Table> {
        let id = *self.tables.get(name)?;
        self.arena.get(id.raw() as usize)
    }

    /// Lookup by dense id. Returns `None` if `id` was not produced by this
    /// `Schema`.
    pub fn get_by_id(&self, id: TableId) -> Option<&Table> {
        self.arena.get(id.raw() as usize)
    }

    pub fn table_id(&self, name: &OxablAtom) -> Option<TableId> {
        self.tables.get(name).copied()
    }

    pub fn tables(&self) -> impl Iterator<Item = (TableId, &Table)> {
        self.arena
            .iter()
            .enumerate()
            .map(|(i, t)| (TableId::new(i as u32), t))
    }

    pub(crate) fn bump_revision(&mut self) {
        self.revision = self.revision.bump();
    }

    pub(crate) fn insert_table(&mut self, table: Table) -> TableId {
        let id = TableId::new(self.arena.len() as u32);
        self.tables.insert(table.name.clone(), id);
        self.arena.push(table);
        id
    }

    pub(crate) fn replace_table(&mut self, id: TableId, table: Table) {
        let slot = self
            .arena
            .get_mut(id.raw() as usize)
            .expect("valid TableId");
        *slot = table;
    }

    pub(crate) fn table_mut(&mut self, id: TableId) -> &mut Table {
        &mut self.arena[id.raw() as usize]
    }
}

impl Default for Schema {
    fn default() -> Self {
        Schema::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::Span;
    use oxabl_common::FileId;

    fn span() -> FileSpan {
        FileSpan {
            file: FileId::UNKNOWN,
            span: Span { start: 0, end: 0 },
        }
    }

    fn new_table(name: &str) -> Table {
        Table {
            name: fold_atom(name),
            display_name: name.to_string(),
            fields: Vec::new(),
            indexes: Vec::new(),
            area: None,
            description: None,
            dump_name: None,
            frozen: false,
            hidden: false,
            label: None,
            valexp: None,
            valmsg: None,
            source: span(),
        }
    }

    #[test]
    fn empty_schema_reports_empty() {
        let s = Schema::empty();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
        assert_eq!(s.revision().raw(), 0);
    }

    #[test]
    fn insert_and_lookup_case_insensitive() {
        let mut s = Schema::empty();
        let id = s.insert_table(new_table("Customer"));
        assert_eq!(s.len(), 1);
        assert_eq!(s.table_id(&fold_atom("CUSTOMER")), Some(id));
        assert_eq!(
            s.get(&fold_atom("customer")).unwrap().display_name,
            "Customer"
        );
        assert_eq!(s.get_by_id(id).unwrap().display_name, "Customer");
    }

    #[test]
    fn revision_bumps_monotonically() {
        let mut s = Schema::empty();
        assert_eq!(s.revision().raw(), 0);
        s.bump_revision();
        assert_eq!(s.revision().raw(), 1);
        s.bump_revision();
        assert_eq!(s.revision().raw(), 2);
    }

    #[test]
    fn schema_type_classifies_primitives() {
        assert_eq!(SchemaType::classify("integer"), SchemaType::Integer);
        assert_eq!(SchemaType::classify("INTEGER"), SchemaType::Integer);
        assert_eq!(SchemaType::classify("Int64"), SchemaType::Int64);
        assert_eq!(SchemaType::classify("datetime-tz"), SchemaType::DatetimeTz);
        assert_eq!(SchemaType::classify("CHARACTER"), SchemaType::Character);
        assert_eq!(SchemaType::classify("blob"), SchemaType::Blob);
    }

    #[test]
    fn schema_type_classifies_unknown() {
        match SchemaType::classify("com-handle") {
            SchemaType::Unknown(atom) => assert_eq!(atom, fold_atom("com-handle")),
            other => panic!("expected Unknown, got {other:?}"),
        }
    }

    #[test]
    fn table_field_lookup_case_insensitive() {
        let mut t = new_table("Order");
        t.fields.push(Field {
            name: fold_atom("CustNum"),
            display_name: "CustNum".into(),
            data_type: SchemaType::Integer,
            extent: None,
            mandatory: false,
            case_sensitive: false,
            format: None,
            label: None,
            initial: None,
            description: None,
            decimals: None,
            position: None,
            order: None,
            max_width: None,
            help: None,
            valexp: None,
            valmsg: None,
            extras: Vec::new(),
            source: span(),
        });
        assert!(t.get_field(&fold_atom("custnum")).is_some());
        assert!(t.get_field(&fold_atom("other")).is_none());
    }
}
