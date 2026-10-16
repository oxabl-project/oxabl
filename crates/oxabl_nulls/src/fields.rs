//! Database fields as sources of `?` (NL004).
//!
//! A field is a source when the loaded schema says it starts at `?` and may
//! stay there: its `INITIAL` is `?`, it is not `MANDATORY`, and it has no
//! extent. A record created without assigning it holds `?`.
use oxabl_ast::NodeId;
use oxabl_flow::UnitId;
use oxabl_schema::{FieldResolution, Schema};
use oxabl_scope::{BufferId, FileScopes, Table};
use rustc_hash::{FxHashMap, FxHashSet};

/// What the field analysis reads besides the program: the scope analysis of
/// the same statements, and the schema that says which fields start at `?`.
#[derive(Debug, Clone, Copy)]
pub struct FieldSources<'a> {
    pub scopes: &'a FileScopes,
    pub schema: &'a Schema,
}

/// One field of one buffer.
#[derive(Debug, Clone)]
pub(crate) struct FieldVar {
    pub buffer: BufferId,
    /// Unique per buffer and field.
    pub key: String,
    /// `buffer.field`, as written at the first reference.
    pub name: String,
    /// The buffer is shared with other code, or used from another unit.
    pub shared: bool,
}

/// The nullable fields a unit references.
#[derive(Debug, Default)]
pub(crate) struct UnitFields {
    pub vars: Vec<FieldVar>,
    /// Expression span to the key of its field.
    pub spans: FxHashMap<(u32, u32), String>,
    /// Indices into `FileScopes::references` and `FileScopes::reads` of the
    /// unit, in order.
    pub references: Vec<usize>,
    pub reads: Vec<usize>,
}

/// The statement-level effects on field values, by variable index.
#[derive(Debug, Default)]
pub(crate) struct Effects {
    /// A read or `CREATE` refills the buffer: its fields may be `?` again.
    pub refill: Vec<usize>,
    /// The record changed in a way the model does not follow.
    pub forget: Vec<usize>,
    /// A `FOR` header refills its buffers on the way into the body.
    pub body_refill: Vec<usize>,
    /// ... and loses them when it exits.
    pub done_forget: Vec<usize>,
}

fn is_word(w: &str) -> bool {
    !w.is_empty()
        && w.bytes().all(|b| {
            b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'#' | b'$' | b'%' | b'&')
        })
}

/// The nullable field references of each unit.
pub(crate) fn collect(source: &str, input: &FieldSources<'_>) -> FxHashMap<UnitId, UnitFields> {
    let scopes = input.scopes;
    let mut units_of: FxHashMap<BufferId, Vec<UnitId>> = FxHashMap::default();
    for r in scopes.references() {
        let units = units_of.entry(r.buffer).or_default();
        if !units.contains(&r.unit) {
            units.push(r.unit);
        }
    }
    let mut out: FxHashMap<UnitId, UnitFields> = FxHashMap::default();
    let mut seen: FxHashSet<(UnitId, String)> = FxHashSet::default();
    for r in scopes.references() {
        if !r.field {
            continue;
        }
        let buffer = scopes.buffer(r.buffer);
        let Table::Database(table) = &buffer.table else {
            continue;
        };
        if !buffer.unknown.is_empty() {
            continue;
        }
        let Some(text) = source.get(r.at.start as usize..r.at.end as usize) else {
            continue;
        };
        let word = text.rsplit('.').next().unwrap_or("").trim();
        if !is_word(word) {
            continue;
        }
        let Some(table) = input.schema.table(table) else {
            continue;
        };
        let FieldResolution::Unique(field) = table.resolve_field_by_name(word) else {
            continue;
        };
        if field.extent.is_some() || field.mandatory || !field.initial_is_unknown() {
            continue;
        }
        let key = format!("{}\0{}", r.buffer.index(), field.name.as_ref());
        let unit = out.entry(r.unit).or_default();
        if seen.insert((r.unit, key.clone())) {
            let shared =
                buffer.is_external() || units_of.get(&r.buffer).is_some_and(|u| u.len() > 1);
            unit.vars.push(FieldVar {
                buffer: r.buffer,
                key: key.clone(),
                name: format!("{}.{}", buffer.name, field.display_name),
                shared,
            });
        }
        unit.spans.insert((r.at.start, r.at.end), key);
    }
    for (i, r) in scopes.references().iter().enumerate() {
        if let Some(unit) = out.get_mut(&r.unit) {
            unit.references.push(i);
        }
    }
    for (i, r) in scopes.reads().iter().enumerate() {
        if let Some(unit) = out.get_mut(&r.unit) {
            unit.reads.push(i);
        }
    }
    out
}

/// The innermost span of `spans` (sorted by start, outer first) holding each
/// position (positions ascending).
pub(crate) fn owners(spans: &[(u32, u32, NodeId)], positions: &[u32]) -> Vec<Option<NodeId>> {
    let mut stack: Vec<usize> = Vec::new();
    let mut next = 0;
    let mut out = Vec::with_capacity(positions.len());
    for &p in positions {
        while next < spans.len() && spans[next].0 <= p {
            while stack.last().is_some_and(|&t| spans[t].1 <= spans[next].0) {
                stack.pop();
            }
            stack.push(next);
            next += 1;
        }
        while stack.last().is_some_and(|&t| spans[t].1 <= p) {
            stack.pop();
        }
        out.push(stack.last().map(|&t| spans[t].2));
    }
    out
}
