//! The binary form of a loaded schema and the diagnostics of its load.
//!
//! A cache keeps this form, so it is read as untrusted: every count and length
//! is checked against the bytes that remain, and `decode` gives `None` for
//! anything it does not fully understand. `encode` gives `None` for a schema
//! it cannot give back exactly, so the loader then writes no entry.
//!
//! Names are written once, as the `.df` spelled them: the folded atom of each
//! is made again on reading. `encode` checks that each atom is the fold of its
//! spelling, which every loaded schema holds.
//!
//! The text of every string is in one pool before the records, which refer to
//! it by length, so reading checks the UTF-8 of the whole pool once.

use std::path::PathBuf;

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, DiagnosticCode, FileId, FileSpan, Label, Severity};
use oxabl_lexer::oxabl_atom::OxablAtom;

use super::{Field, Index, IndexField, Schema, SchemaRevision, SchemaType, Sequence, Table};
use crate::atom::fold_atom;
use crate::diagnostics::{
    SCHEMA0001, SCHEMA0010, SCHEMA0011, SCHEMA0012, SCHEMA0020, SCHEMA0021, SCHEMA0030, SCHEMA0031,
};

const MAGIC: &[u8; 8] = b"OXSCHEMA";
/// Raise it with any change of the layout below.
const VERSION: u32 = 1;

const CODES: [&str; 8] = [
    SCHEMA0001, SCHEMA0010, SCHEMA0011, SCHEMA0012, SCHEMA0020, SCHEMA0021, SCHEMA0030, SCHEMA0031,
];

/// The bytes of `schema` and `diagnostics`, or `None` when they do not
/// round-trip.
pub(crate) fn encode(schema: &Schema, diagnostics: &[Diagnostic]) -> Option<Vec<u8>> {
    let mut w = Writer {
        records: Vec::with_capacity(1 << 16),
        pool: String::with_capacity(1 << 16),
    };
    w.u32(schema.revision.raw());
    w.u8(u8::from(schema.metaschema));
    w.len(schema.arena.len());
    for table in &schema.arena {
        w.table(table)?;
    }
    w.len(schema.sequences.len());
    for sequence in &schema.sequences {
        w.sequence(sequence)?;
    }
    w.len(schema.source_files.len());
    for (file, path) in &schema.source_files {
        w.u32(file.raw());
        w.str(path.to_str()?);
    }
    w.len(diagnostics.len());
    for diagnostic in diagnostics {
        w.diagnostic(diagnostic)?;
    }
    let mut out = Vec::with_capacity(MAGIC.len() + 8 + w.pool.len() + w.records.len());
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&VERSION.to_le_bytes());
    out.extend_from_slice(&u32::try_from(w.pool.len()).ok()?.to_le_bytes());
    out.extend_from_slice(w.pool.as_bytes());
    out.extend_from_slice(&w.records);
    Some(out)
}

/// The schema and the load diagnostics that `encode` wrote, or `None` when
/// `bytes` are not exactly such a form.
pub(crate) fn decode(bytes: &[u8]) -> Option<(Schema, Vec<Diagnostic>)> {
    let mut r = Reader {
        pool: "",
        records: bytes,
    };
    if r.take(MAGIC.len())? != MAGIC || r.u32()? != VERSION {
        return None;
    }
    let pool = r.u32()? as usize;
    r.pool = std::str::from_utf8(r.take(pool)?).ok()?;
    let mut schema = Schema::empty();
    schema.revision = SchemaRevision::new(r.u32()?);
    schema.metaschema = match r.u8()? {
        0 => false,
        1 => true,
        _ => return None,
    };
    let tables = r.count(MIN_TABLE)?;
    schema.arena.reserve_exact(tables);
    schema.tables.reserve(tables);
    for at in 0..tables {
        let table = r.table()?;
        if schema
            .tables
            .insert(table.name.clone(), super::TableId::new(at as u32))
            .is_some()
        {
            return None;
        }
        schema.arena.push(table);
    }
    schema.by_name = (0..tables as u32).map(super::TableId::new).collect();
    let arena = &schema.arena;
    schema.by_name.sort_unstable_by(|a, b| {
        arena[a.index()]
            .name
            .as_ref()
            .cmp(arena[b.index()].name.as_ref())
    });
    let sequences = r.count(MIN_SEQUENCE)?;
    for _ in 0..sequences {
        let sequence = r.sequence()?;
        if schema.insert_sequence(sequence).is_some() {
            return None;
        }
    }
    let files = r.count(8)?;
    for _ in 0..files {
        let file = FileId::new(r.u32()?);
        schema.record_source_file(file, PathBuf::from(r.str()?));
    }
    let count = r.count(MIN_DIAGNOSTIC)?;
    let mut diagnostics = Vec::with_capacity(count);
    for _ in 0..count {
        diagnostics.push(r.diagnostic()?);
    }
    (r.records.is_empty() && r.pool.is_empty()).then_some((schema, diagnostics))
}

// The fewest bytes of one element of each list, so a count that the bytes
// cannot hold is refused before anything is reserved for it.
const MIN_TABLE: usize = 4 + 1 + 12 + 4 + 4;
const MIN_FIELD: usize = 4 + 1 + 2 + 4 + 12;
const MIN_INDEX: usize = 4 + 1 + 4 + 12;
const MIN_SEQUENCE: usize = 4 + 1 + 12;
const MIN_DIAGNOSTIC: usize = 1 + 4 + 4 + 12 + 4 + 1;

// Presence and truth bits of a table, a field, an index and a sequence.
const T_FROZEN: u8 = 1;
const T_HIDDEN: u8 = 1 << 1;
const T_AREA: u8 = 1 << 2;
const T_DESCRIPTION: u8 = 1 << 3;
const T_DUMP_NAME: u8 = 1 << 4;
const T_LABEL: u8 = 1 << 5;
const T_VALEXP: u8 = 1 << 6;
const T_VALMSG: u8 = 1 << 7;

const F_MANDATORY: u16 = 1;
const F_CASE_SENSITIVE: u16 = 1 << 1;
const F_EXTENT: u16 = 1 << 2;
const F_FORMAT: u16 = 1 << 3;
const F_LABEL: u16 = 1 << 4;
const F_INITIAL: u16 = 1 << 5;
const F_DESCRIPTION: u16 = 1 << 6;
const F_DECIMALS: u16 = 1 << 7;
const F_POSITION: u16 = 1 << 8;
const F_ORDER: u16 = 1 << 9;
const F_MAX_WIDTH: u16 = 1 << 10;
const F_HELP: u16 = 1 << 11;
const F_VALEXP: u16 = 1 << 12;
const F_VALMSG: u16 = 1 << 13;

const I_UNIQUE: u8 = 1;
const I_PRIMARY: u8 = 1 << 1;
const I_WORD: u8 = 1 << 2;
const I_INACTIVE: u8 = 1 << 3;
const I_AREA: u8 = 1 << 4;
const I_DESCRIPTION: u8 = 1 << 5;

const X_ASCENDING: u8 = 1;
const X_ABBREVIATED: u8 = 1 << 1;
const X_UNSORTED: u8 = 1 << 2;

const S_CYCLE: u8 = 1;
const S_INITIAL: u8 = 1 << 1;
const S_INCREMENT: u8 = 1 << 2;
const S_MIN_VAL: u8 = 1 << 3;
const S_MAX_VAL: u8 = 1 << 4;

const TYPES: [SchemaType; 15] = [
    SchemaType::Integer,
    SchemaType::Int64,
    SchemaType::Decimal,
    SchemaType::Character,
    SchemaType::Longchar,
    SchemaType::Logical,
    SchemaType::Date,
    SchemaType::Datetime,
    SchemaType::DatetimeTz,
    SchemaType::Handle,
    SchemaType::Raw,
    SchemaType::Recid,
    SchemaType::Rowid,
    SchemaType::Blob,
    SchemaType::Clob,
];
const TYPE_UNKNOWN: u8 = TYPES.len() as u8;
const TYPE_ERROR: u8 = TYPE_UNKNOWN + 1;

fn has_upper(s: &str) -> bool {
    s.bytes().any(|b| b.is_ascii_uppercase())
}

struct Writer {
    records: Vec<u8>,
    pool: String,
}

impl Writer {
    fn u8(&mut self, v: u8) {
        self.records.push(v);
    }

    fn u16(&mut self, v: u16) {
        self.records.extend_from_slice(&v.to_le_bytes());
    }

    fn u32(&mut self, v: u32) {
        self.records.extend_from_slice(&v.to_le_bytes());
    }

    fn i64(&mut self, v: i64) {
        self.records.extend_from_slice(&v.to_le_bytes());
    }

    fn len(&mut self, n: usize) {
        self.u32(u32::try_from(n).unwrap_or(u32::MAX));
    }

    fn str(&mut self, s: &str) {
        self.len(s.len());
        self.pool.push_str(s);
    }

    fn opt_str(&mut self, s: &Option<String>) {
        if let Some(s) = s {
            self.str(s);
        }
    }

    fn opt_u32(&mut self, v: Option<u32>) {
        if let Some(v) = v {
            self.u32(v);
        }
    }

    fn span(&mut self, s: FileSpan) {
        self.u32(s.file.raw());
        self.u32(s.span.start);
        self.u32(s.span.end);
    }

    /// `name` must be the fold of `shown`, or the entry would not give it back.
    fn name(&mut self, name: &OxablAtom, shown: &str) -> Option<()> {
        let name: &str = name;
        (name.len() == shown.len() && name.eq_ignore_ascii_case(shown) && !has_upper(name))
            .then(|| self.str(shown))
    }

    fn table(&mut self, t: &Table) -> Option<()> {
        self.name(&t.name, &t.display_name)?;
        let mut bits = 0u8;
        for (set, bit) in [
            (t.frozen, T_FROZEN),
            (t.hidden, T_HIDDEN),
            (t.area.is_some(), T_AREA),
            (t.description.is_some(), T_DESCRIPTION),
            (t.dump_name.is_some(), T_DUMP_NAME),
            (t.label.is_some(), T_LABEL),
            (t.valexp.is_some(), T_VALEXP),
            (t.valmsg.is_some(), T_VALMSG),
        ] {
            if set {
                bits |= bit;
            }
        }
        self.u8(bits);
        for s in [
            &t.area,
            &t.description,
            &t.dump_name,
            &t.label,
            &t.valexp,
            &t.valmsg,
        ] {
            self.opt_str(s);
        }
        self.span(t.source);
        self.len(t.fields.len());
        for field in &t.fields {
            self.field(field)?;
        }
        self.len(t.indexes.len());
        for index in &t.indexes {
            self.index(index)?;
        }
        Some(())
    }

    fn field(&mut self, f: &Field) -> Option<()> {
        self.name(&f.name, &f.display_name)?;
        match &f.data_type {
            SchemaType::Unknown(atom) => {
                self.u8(TYPE_UNKNOWN);
                self.str(atom);
            }
            SchemaType::Error => self.u8(TYPE_ERROR),
            known => self.u8(TYPES.iter().position(|t| t == known)? as u8),
        }
        let mut bits = 0u16;
        for (set, bit) in [
            (f.mandatory, F_MANDATORY),
            (f.case_sensitive, F_CASE_SENSITIVE),
            (f.extent.is_some(), F_EXTENT),
            (f.format.is_some(), F_FORMAT),
            (f.label.is_some(), F_LABEL),
            (f.initial.is_some(), F_INITIAL),
            (f.description.is_some(), F_DESCRIPTION),
            (f.decimals.is_some(), F_DECIMALS),
            (f.position.is_some(), F_POSITION),
            (f.order.is_some(), F_ORDER),
            (f.max_width.is_some(), F_MAX_WIDTH),
            (f.help.is_some(), F_HELP),
            (f.valexp.is_some(), F_VALEXP),
            (f.valmsg.is_some(), F_VALMSG),
        ] {
            if set {
                bits |= bit;
            }
        }
        self.u16(bits);
        self.opt_u32(f.extent);
        self.opt_str(&f.format);
        self.opt_str(&f.label);
        self.opt_str(&f.initial);
        self.opt_str(&f.description);
        self.opt_u32(f.decimals);
        self.opt_u32(f.position);
        self.opt_u32(f.order);
        self.opt_u32(f.max_width);
        self.opt_str(&f.help);
        self.opt_str(&f.valexp);
        self.opt_str(&f.valmsg);
        self.len(f.extras.len());
        for (key, value) in &f.extras {
            self.str(key);
            self.str(value);
        }
        self.span(f.source);
        Some(())
    }

    fn index(&mut self, i: &Index) -> Option<()> {
        self.name(&i.name, &i.display_name)?;
        let mut bits = 0u8;
        for (set, bit) in [
            (i.unique, I_UNIQUE),
            (i.primary, I_PRIMARY),
            (i.word, I_WORD),
            (i.inactive, I_INACTIVE),
            (i.area.is_some(), I_AREA),
            (i.description.is_some(), I_DESCRIPTION),
        ] {
            if set {
                bits |= bit;
            }
        }
        self.u8(bits);
        self.opt_str(&i.area);
        self.opt_str(&i.description);
        self.len(i.fields.len());
        for field in &i.fields {
            self.name(&field.name, &field.display_name)?;
            let mut bits = 0u8;
            for (set, bit) in [
                (field.ascending, X_ASCENDING),
                (field.abbreviated, X_ABBREVIATED),
                (field.unsorted, X_UNSORTED),
            ] {
                if set {
                    bits |= bit;
                }
            }
            self.u8(bits);
        }
        self.span(i.source);
        Some(())
    }

    fn sequence(&mut self, s: &Sequence) -> Option<()> {
        self.name(&s.name, &s.display_name)?;
        let mut bits = 0u8;
        for (set, bit) in [
            (s.cycle_on_limit, S_CYCLE),
            (s.initial.is_some(), S_INITIAL),
            (s.increment.is_some(), S_INCREMENT),
            (s.min_val.is_some(), S_MIN_VAL),
            (s.max_val.is_some(), S_MAX_VAL),
        ] {
            if set {
                bits |= bit;
            }
        }
        self.u8(bits);
        for v in [s.initial, s.increment, s.min_val, s.max_val]
            .into_iter()
            .flatten()
        {
            self.i64(v);
        }
        self.span(s.source);
        Some(())
    }

    fn diagnostic(&mut self, d: &Diagnostic) -> Option<()> {
        self.u8(match d.severity {
            Severity::Error => 0,
            Severity::Warning => 1,
            Severity::Info => 2,
            Severity::Hint => 3,
        });
        CODES.contains(&d.code.0).then(|| self.str(d.code.0))?;
        self.str(&d.message);
        self.span(d.span);
        self.len(d.labels.len());
        for label in &d.labels {
            self.span(label.span);
            self.str(&label.message);
        }
        match &d.help {
            Some(help) => {
                self.u8(1);
                self.str(help);
            }
            None => self.u8(0),
        }
        Some(())
    }
}

struct Reader<'a> {
    /// The text of the strings not read yet.
    pool: &'a str,
    records: &'a [u8],
}

impl<'a> Reader<'a> {
    fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        let (head, rest) = self.records.split_at_checked(n)?;
        self.records = rest;
        Some(head)
    }

    fn u8(&mut self) -> Option<u8> {
        Some(self.take(1)?[0])
    }

    fn u16(&mut self) -> Option<u16> {
        Some(u16::from_le_bytes(self.take(2)?.try_into().ok()?))
    }

    fn u32(&mut self) -> Option<u32> {
        Some(u32::from_le_bytes(self.take(4)?.try_into().ok()?))
    }

    fn i64(&mut self) -> Option<i64> {
        Some(i64::from_le_bytes(self.take(8)?.try_into().ok()?))
    }

    /// A list length, refused when `min` bytes for each element are not left.
    fn count(&mut self, min: usize) -> Option<usize> {
        let n = self.u32()? as usize;
        (n.checked_mul(min)? <= self.records.len()).then_some(n)
    }

    /// The next string of the pool; `None` when it ends inside a character.
    fn str(&mut self) -> Option<&'a str> {
        let n = self.u32()? as usize;
        let (head, rest) = (self.pool.get(..n)?, self.pool.get(n..)?);
        self.pool = rest;
        Some(head)
    }

    fn string(&mut self) -> Option<String> {
        self.str().map(str::to_owned)
    }

    fn opt_string(&mut self, present: bool) -> Option<Option<String>> {
        if present {
            self.string().map(Some)
        } else {
            Some(None)
        }
    }

    fn opt_u32(&mut self, present: bool) -> Option<Option<u32>> {
        if present {
            self.u32().map(Some)
        } else {
            Some(None)
        }
    }

    fn span(&mut self) -> Option<FileSpan> {
        let file = FileId::new(self.u32()?);
        let start = self.u32()?;
        let end = self.u32()?;
        Some(FileSpan {
            file,
            span: Span { start, end },
        })
    }

    fn table(&mut self) -> Option<Table> {
        let display_name = self.string()?;
        let bits = self.u8()?;
        let has = |bit: u8| bits & bit != 0;
        let area = self.opt_string(has(T_AREA))?;
        let description = self.opt_string(has(T_DESCRIPTION))?;
        let dump_name = self.opt_string(has(T_DUMP_NAME))?;
        let label = self.opt_string(has(T_LABEL))?;
        let valexp = self.opt_string(has(T_VALEXP))?;
        let valmsg = self.opt_string(has(T_VALMSG))?;
        let source = self.span()?;
        let count = self.count(MIN_FIELD)?;
        let mut fields = Vec::with_capacity(count);
        for _ in 0..count {
            fields.push(self.field()?);
        }
        let count = self.count(MIN_INDEX)?;
        let mut indexes = Vec::with_capacity(count);
        for _ in 0..count {
            indexes.push(self.index()?);
        }
        Some(Table {
            name: fold_atom(&display_name),
            display_name,
            fields,
            indexes,
            area,
            description,
            dump_name,
            frozen: has(T_FROZEN),
            hidden: has(T_HIDDEN),
            label,
            valexp,
            valmsg,
            source,
        })
    }

    fn field(&mut self) -> Option<Field> {
        let display_name = self.string()?;
        let data_type = match self.u8()? {
            TYPE_UNKNOWN => SchemaType::Unknown(OxablAtom::from(self.str()?)),
            TYPE_ERROR => SchemaType::Error,
            known => TYPES.get(known as usize)?.clone(),
        };
        let bits = self.u16()?;
        let has = |bit: u16| bits & bit != 0;
        let extent = self.opt_u32(has(F_EXTENT))?;
        let format = self.opt_string(has(F_FORMAT))?;
        let label = self.opt_string(has(F_LABEL))?;
        let initial = self.opt_string(has(F_INITIAL))?;
        let description = self.opt_string(has(F_DESCRIPTION))?;
        let decimals = self.opt_u32(has(F_DECIMALS))?;
        let position = self.opt_u32(has(F_POSITION))?;
        let order = self.opt_u32(has(F_ORDER))?;
        let max_width = self.opt_u32(has(F_MAX_WIDTH))?;
        let help = self.opt_string(has(F_HELP))?;
        let valexp = self.opt_string(has(F_VALEXP))?;
        let valmsg = self.opt_string(has(F_VALMSG))?;
        let count = self.count(8)?;
        let mut extras = Vec::with_capacity(count);
        for _ in 0..count {
            extras.push((OxablAtom::from(self.str()?), self.string()?));
        }
        let source = self.span()?;
        Some(Field {
            name: fold_atom(&display_name),
            display_name,
            data_type,
            extent,
            mandatory: has(F_MANDATORY),
            case_sensitive: has(F_CASE_SENSITIVE),
            format,
            label,
            initial,
            description,
            decimals,
            position,
            order,
            max_width,
            help,
            valexp,
            valmsg,
            extras,
            source,
        })
    }

    fn index(&mut self) -> Option<Index> {
        let display_name = self.string()?;
        let bits = self.u8()?;
        let has = |bit: u8| bits & bit != 0;
        let area = self.opt_string(has(I_AREA))?;
        let description = self.opt_string(has(I_DESCRIPTION))?;
        let count = self.count(5)?;
        let mut fields = Vec::with_capacity(count);
        for _ in 0..count {
            let display_name = self.string()?;
            let bits = self.u8()?;
            fields.push(IndexField {
                name: fold_atom(&display_name),
                display_name,
                ascending: bits & X_ASCENDING != 0,
                abbreviated: bits & X_ABBREVIATED != 0,
                unsorted: bits & X_UNSORTED != 0,
            });
        }
        let source = self.span()?;
        Some(Index {
            name: fold_atom(&display_name),
            display_name,
            unique: has(I_UNIQUE),
            primary: has(I_PRIMARY),
            word: has(I_WORD),
            inactive: has(I_INACTIVE),
            area,
            description,
            fields,
            source,
        })
    }

    fn sequence(&mut self) -> Option<Sequence> {
        let display_name = self.string()?;
        let bits = self.u8()?;
        let mut value = |bit: u8| -> Option<Option<i64>> {
            if bits & bit != 0 {
                self.i64().map(Some)
            } else {
                Some(None)
            }
        };
        let initial = value(S_INITIAL)?;
        let increment = value(S_INCREMENT)?;
        let min_val = value(S_MIN_VAL)?;
        let max_val = value(S_MAX_VAL)?;
        let source = self.span()?;
        Some(Sequence {
            name: fold_atom(&display_name),
            display_name,
            initial,
            increment,
            min_val,
            max_val,
            cycle_on_limit: bits & S_CYCLE != 0,
            source,
        })
    }

    fn diagnostic(&mut self) -> Option<Diagnostic> {
        let severity = match self.u8()? {
            0 => Severity::Error,
            1 => Severity::Warning,
            2 => Severity::Info,
            3 => Severity::Hint,
            _ => return None,
        };
        let code = self.str()?;
        let code = DiagnosticCode(CODES.iter().find(|known| **known == code)?);
        let message = self.string()?;
        let span = self.span()?;
        let count = self.count(13)?;
        let mut labels = Vec::with_capacity(count);
        for _ in 0..count {
            labels.push(Label {
                span: self.span()?,
                message: self.string()?,
            });
        }
        let help = match self.u8()? {
            0 => None,
            1 => Some(self.string()?),
            _ => return None,
        };
        Some(Diagnostic {
            severity,
            code,
            message,
            span,
            labels,
            help,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table(name: &str, display: &str) -> Table {
        Table {
            name: fold_atom(name),
            display_name: display.to_string(),
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
            source: FileSpan {
                file: FileId::UNKNOWN,
                span: Span { start: 0, end: 0 },
            },
        }
    }

    fn encoded(name: &str, display: &str) -> Option<Vec<u8>> {
        let mut schema = Schema::empty();
        schema.insert_table(table(name, display));
        encode(&schema, &[])
    }

    #[test]
    fn a_name_that_is_not_the_fold_of_its_spelling_is_not_encoded() {
        assert!(encoded("customer", "Customer").is_some());
        assert!(encoded("customer", "CUSTOMER").is_some());
        assert!(encoded("customer", "Custom").is_none());
        assert!(encoded("customer", "Customes").is_none());
        // The atom must be the folded spelling, not just equal to it ignoring case.
        let mut schema = Schema::empty();
        let mut mixed = table("customer", "Customer");
        mixed.name = OxablAtom::from("Customer");
        schema.insert_table(mixed);
        assert!(encode(&schema, &[]).is_none());
    }

    #[test]
    fn two_tables_of_one_name_do_not_decode() {
        let mut schema = Schema::empty();
        schema.insert_table(table("a", "A"));
        schema.insert_table(table("a", "A"));
        let bytes = encode(&schema, &[]).expect("encodes");
        assert!(decode(&bytes).is_none());
    }

    #[test]
    fn a_diagnostic_of_a_foreign_code_is_not_encoded() {
        let span = FileSpan {
            file: FileId::UNKNOWN,
            span: Span { start: 0, end: 0 },
        };
        let known = Diagnostic::error(SCHEMA0001, "m".to_string(), span);
        let foreign = Diagnostic::error("OTHER001", "m".to_string(), span);
        assert!(encode(&Schema::empty(), &[known]).is_some());
        assert!(encode(&Schema::empty(), &[foreign]).is_none());
    }
}
