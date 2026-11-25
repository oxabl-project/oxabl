//! Normalise the scraped built-in class index into a typed model.
//!
//! The scrape of the Progress documentation has gaps and errors (see
//! `docs/design/builtin-classes.md`, "Quirks and errors"). This module applies
//! the checked overrides of `resources/progress_builtin_overrides.toml`, reads
//! every member type into a typed value, drops the members it cannot read, and
//! computes the trust and completeness flags of each type. It never guesses a
//! signature: a member it cannot read is dropped and its type is not trusted.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use serde::Deserialize;

use crate::progress_docs::{BuiltinIndex, Member, Param, TypeEntry, TypeKind};

/// The overrides file, relative to the workspace root.
pub const OVERRIDES_PATH: &str = "resources/progress_builtin_overrides.toml";

/// The documented primitive names and the `PrimitiveTy` variant of each.
const PRIMITIVES: &[(&str, &str)] = &[
    ("CHARACTER", "Character"),
    ("LONGCHAR", "Longchar"),
    ("INTEGER", "Integer"),
    ("INT64", "Int64"),
    ("DECIMAL", "Decimal"),
    ("LOGICAL", "Logical"),
    ("DATE", "Date"),
    ("DATETIME", "Datetime"),
    ("DATETIME-TZ", "DatetimeTz"),
    ("HANDLE", "Handle"),
    ("WIDGET-HANDLE", "Handle"),
    ("COM-HANDLE", "ComHandle"),
    ("ROWID", "Rowid"),
    ("RECID", "Recid"),
    ("RAW", "Raw"),
    ("MEMPTR", "Memptr"),
    ("BLOB", "Blob"),
    ("CLOB", "Clob"),
];

/// The parameter shapes a built-in parameter can have, and their names in the
/// generated table.
const SHAPES: &[(&str, &str)] = &[
    ("TABLE", "Table"),
    ("TABLE-HANDLE", "TableHandle"),
    ("DATASET", "Dataset"),
    ("DATASET-HANDLE", "DatasetHandle"),
    ("BUFFER", "Buffer"),
];

/// The package prefixes of .NET types (lowercase).
const DOTNET_PREFIXES: &[&str] = &["system.", "telerik.", "infragistics.", "microsoft."];

/// A member type, read.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NType {
    Void,
    /// The `PrimitiveTy` variant name.
    Primitive(&'static str),
    PrimitiveArray(&'static str),
    /// The index of a type in [`Catalog::types`].
    Class(usize),
    ClassArray(usize),
    /// The shape name (`Table`, `DatasetHandle`, ...).
    Shape(&'static str),
    /// A type parameter of the owning type, by its position.
    TypeParam(u8),
    /// A generic instance, a .NET type, any type.
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NMemberKind {
    Constructor,
    Method,
    Property,
    Event,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NParam {
    /// Empty when the catalog has no name.
    pub name: String,
    pub ty: NType,
    /// The type as the scrape gives it, before a conflated name is read as
    /// `Unknown` (see [`Names::conflated`]). Compared by the interface check.
    pub scraped: NType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NMember {
    pub name: String,
    pub kind: NMemberKind,
    pub is_static: bool,
    pub is_abstract: bool,
    pub is_final: bool,
    /// The return type of a method, the type of a property; `Void` for a
    /// constructor and an event.
    pub ty: NType,
    /// [`Self::ty`] as the scrape gives it (see [`NParam::scraped`]).
    pub scraped: NType,
    pub params: Vec<NParam>,
}

/// A supertype as the catalog names it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NSuper {
    /// A catalog type (its index).
    Catalog(usize),
    /// A .NET type.
    External(String),
    /// A name that names no catalog type.
    Unresolved(String),
}

#[derive(Debug, Clone)]
pub struct NTypeEntry {
    /// The documented name without generic parameters.
    pub name: String,
    pub kind: TypeKind,
    pub type_params: Vec<String>,
    /// The page of the type in its documentation bundle.
    pub page: String,
    pub is_abstract: bool,
    pub is_final: bool,
    pub parent: Option<NSuper>,
    pub interfaces: Vec<NSuper>,
    pub members: Vec<NMember>,
    pub enum_members: Vec<String>,
    /// No member was dropped as damaged.
    pub members_trusted: bool,
    /// Every supertype is a catalog type (or there is none to have).
    pub header_known: bool,
    pub complete: bool,
}

/// The normalised catalog, sorted by key.
#[derive(Debug, Clone)]
pub struct Catalog {
    pub version: String,
    pub types: Vec<NTypeEntry>,
}

impl Catalog {
    #[cfg(test)]
    pub fn get(&self, name: &str) -> Option<&NTypeEntry> {
        let key = name.to_ascii_lowercase();
        self.types
            .binary_search_by(|t| t.name.to_ascii_lowercase().cmp(&key))
            .ok()
            .map(|i| &self.types[i])
    }
}

// =============================================================================
// Overrides
// =============================================================================

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverridesFile {
    #[serde(rename = "override", default)]
    overrides: Vec<Override>,
}

/// One checked fix. `type` names the type; exactly one of the other keys
/// says what changes.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Override {
    #[serde(rename = "type")]
    type_name: String,
    /// The page the fix was checked against.
    checked_against: String,
    inherits: Option<String>,
    /// The one member of this kind and name.
    drop_member: Option<OverrideTarget>,
    /// The one overload, by its scraped parameter types, whose return is an
    /// `EXTENT` of a primitive that the scrape read as a scalar: its return
    /// gets `[]`.
    extent_return: Option<OverrideOverload>,
    /// The one property, with its scraped type, that is an `EXTENT` the
    /// scrape read as a scalar (the scrape writes no `EXTENT` on any
    /// property): its type gets `[]`.
    extent_property: Option<OverrideProperty>,
    /// The type of the parameters with this name and this scraped type, in
    /// the overloads of a member.
    param_type: Option<OverrideParamType>,
    add_member: Option<OverrideMember>,
    /// The one member of this kind and name.
    replace_member: Option<OverrideMember>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideTarget {
    kind: String,
    name: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideOverload {
    member: String,
    /// The scraped type of each parameter, in order.
    params: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideProperty {
    name: String,
    /// The scraped type.
    from: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideMember {
    kind: String,
    name: String,
    #[serde(rename = "type")]
    type_name: Option<String>,
    #[serde(default)]
    params: Vec<OverrideParam>,
    #[serde(default)]
    is_static: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideParamType {
    member: String,
    param: String,
    /// The scraped type of the parameter, without `[]`.
    from: String,
    #[serde(rename = "type")]
    type_name: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OverrideParam {
    name: String,
    #[serde(rename = "type")]
    type_name: String,
}

fn member_list<'a>(entry: &'a mut TypeEntry, kind: &str) -> Result<&'a mut Vec<Member>, String> {
    Ok(match kind {
        "constructor" => &mut entry.constructors,
        "method" => &mut entry.methods,
        "property" => &mut entry.properties,
        "event" => &mut entry.events,
        _ => return Err(format!("unknown member kind `{kind}`")),
    })
}

fn to_member(m: &OverrideMember) -> Member {
    Member {
        name: m.name.clone(),
        return_type: m.type_name.clone(),
        params: m
            .params
            .iter()
            .map(|p| Param {
                name: Some(p.name.clone()),
                type_name: p.type_name.clone(),
            })
            .collect(),
        is_static: m.is_static,
        is_abstract: false,
        is_final: false,
    }
}

fn all_members(entry: &mut TypeEntry) -> [&mut Vec<Member>; 4] {
    [
        &mut entry.constructors,
        &mut entry.methods,
        &mut entry.properties,
        &mut entry.events,
    ]
}

/// The position of the one member named `name` (any case) in `list`. None,
/// or more than one, is an error: an override names exactly one row.
fn exactly_one(list: &[Member], name: &str, type_name: &str, kind: &str) -> Result<usize, String> {
    let hits: Vec<usize> = list
        .iter()
        .enumerate()
        .filter(|(_, m)| m.name.eq_ignore_ascii_case(name))
        .map(|(i, _)| i)
        .collect();
    match hits[..] {
        [at] => Ok(at),
        _ => Err(format!(
            "stale override: `{type_name}` has {} {kind} rows `{name}`, not one",
            hits.len()
        )),
    }
}

/// The folded signature of each row of `entry`, by kind and folded name,
/// with the number of rows.
type Rows = BTreeMap<(usize, String), (usize, BTreeSet<Vec<String>>)>;

fn distinct_rows(entry: &TypeEntry) -> Rows {
    let mut out: Rows = BTreeMap::new();
    let lists = [
        &entry.constructors,
        &entry.methods,
        &entry.properties,
        &entry.events,
    ];
    for (kind, list) in lists.into_iter().enumerate() {
        for m in list {
            let mut sig: Vec<String> = m
                .params
                .iter()
                .map(|p| p.type_name.to_ascii_lowercase())
                .collect();
            if kind == 0
                && m.return_type
                    .as_deref()
                    .is_some_and(|r| r.trim().eq_ignore_ascii_case("STATIC"))
            {
                sig.push("static".into());
            }
            let slot = out.entry((kind, m.name.to_ascii_lowercase())).or_default();
            slot.0 += 1;
            slot.1.insert(sig);
        }
    }
    out
}

/// Drop each row that the page lists twice: the same member, with the same
/// names and types. Returns how many it dropped. Run on the scrape, before the
/// overrides, so that a row an override makes equal to another is the
/// collision error of [`normalise`], not a silent drop.
pub fn drop_repeated_rows(index: &mut BuiltinIndex) -> usize {
    let mut dropped = 0;
    for entry in index.types.values_mut() {
        for list in all_members(entry) {
            let mut kept: Vec<Member> = Vec::with_capacity(list.len());
            for m in list.drain(..) {
                if kept.contains(&m) {
                    dropped += 1;
                } else {
                    kept.push(m);
                }
            }
            *list = kept;
        }
    }
    dropped
}

/// [`drop_repeated_rows`], then [`apply_overrides`], then a check that the
/// overrides kept every member's rows: for each type, kind and name, the same
/// number of rows and at least as many distinct signatures as before (an
/// override may tell two scraped rows apart, never make two rows one), except
/// where an override drops or adds a member.
pub fn prepare(index: &mut BuiltinIndex, text: &str) -> Result<(), String> {
    drop_repeated_rows(index);
    let before: BTreeMap<String, _> = index
        .types
        .iter()
        .map(|(name, entry)| (name.clone(), distinct_rows(entry)))
        .collect();
    let touched = apply_overrides(index, text)?;
    for (name, entry) in &index.types {
        let after = distinct_rows(entry);
        for (key, rows) in &before[name] {
            if touched.contains(&(name.clone(), key.1.clone())) {
                continue;
            }
            let (count, distinct) = after
                .get(key)
                .map_or((0, 0), |(count, sigs)| (*count, sigs.len()));
            if count != rows.0 || distinct < rows.1.len() {
                return Err(format!(
                    "the overrides of `{name}` turn {} `{}` rows ({} distinct) into {count} \
                     ({distinct} distinct)",
                    rows.0,
                    key.1,
                    rows.1.len()
                ));
            }
        }
    }
    Ok(())
}

/// Apply the overrides in `text` to `index`. An entry that matches nothing is
/// an error, so a later scrape cannot leave a stale fix behind.
/// Returns the `(type, folded member name)` of each member that a
/// `drop_member` or `add_member` changes the number of rows of.
pub fn apply_overrides(
    index: &mut BuiltinIndex,
    text: &str,
) -> Result<BTreeSet<(String, String)>, String> {
    let file: OverridesFile =
        toml::from_str(text).map_err(|e| format!("parsing {OVERRIDES_PATH}: {e}"))?;
    let mut touched = BTreeSet::new();
    for o in &file.overrides {
        if let Some(name) = o
            .drop_member
            .as_ref()
            .map(|t| &t.name)
            .or(o.add_member.as_ref().map(|m| &m.name))
        {
            touched.insert((o.type_name.clone(), name.to_ascii_lowercase()));
        }
        if o.checked_against.trim().is_empty() {
            return Err(format!(
                "override of `{}` names no page it was checked against",
                o.type_name
            ));
        }
        let what = [
            o.inherits.is_some(),
            o.drop_member.is_some(),
            o.extent_return.is_some(),
            o.extent_property.is_some(),
            o.param_type.is_some(),
            o.add_member.is_some(),
            o.replace_member.is_some(),
        ];
        if what.iter().filter(|b| **b).count() != 1 {
            return Err(format!(
                "override of `{}` must name exactly one of inherits, drop_member, extent_return, extent_property, param_type, add_member, replace_member",
                o.type_name
            ));
        }
        let entry = index
            .types
            .get_mut(&o.type_name)
            .ok_or_else(|| format!("override names no type: `{}`", o.type_name))?;
        if let Some(parent) = &o.inherits {
            if let Some(documented) = &entry.inherits {
                return Err(format!(
                    "stale override: `{}` already documents the supertype `{documented}`",
                    o.type_name
                ));
            }
            entry.inherits = Some(parent.clone());
        } else if let Some(t) = &o.drop_member {
            let list = member_list(entry, &t.kind)?;
            let at = exactly_one(list, &t.name, &o.type_name, &t.kind)?;
            list.remove(at);
        } else if let Some(t) = &o.extent_return {
            let hits: Vec<usize> = entry
                .methods
                .iter()
                .enumerate()
                .filter(|(_, m)| {
                    m.name.eq_ignore_ascii_case(&t.member)
                        && m.params.len() == t.params.len()
                        && m.params
                            .iter()
                            .zip(&t.params)
                            .all(|(p, want)| p.type_name.eq_ignore_ascii_case(want))
                })
                .map(|(i, _)| i)
                .collect();
            let [at] = hits[..] else {
                return Err(format!(
                    "stale override: `{}` has {} methods `{}({})`, not one",
                    o.type_name,
                    hits.len(),
                    t.member,
                    t.params.join(", ")
                ));
            };
            match &mut entry.methods[at].return_type {
                Some(rt)
                    if PRIMITIVES
                        .iter()
                        .any(|(p, _)| p.eq_ignore_ascii_case(rt.trim())) =>
                {
                    rt.push_str("[]")
                }
                _ => {
                    return Err(format!(
                        "stale override: `{}:{}({})` has no scalar primitive return",
                        o.type_name,
                        t.member,
                        t.params.join(", ")
                    ));
                }
            }
        } else if let Some(t) = &o.extent_property {
            let at = exactly_one(&entry.properties, &t.name, &o.type_name, "property")?;
            match &mut entry.properties[at].return_type {
                Some(rt) if rt.trim().eq_ignore_ascii_case(&t.from) => rt.push_str("[]"),
                other => {
                    return Err(format!(
                        "stale override: `{}:{}` is {other:?}, not `{}`",
                        o.type_name, t.name, t.from
                    ));
                }
            }
        } else if let Some(t) = &o.param_type {
            let mut found = false;
            for list in [&mut entry.constructors, &mut entry.methods] {
                for m in list
                    .iter_mut()
                    .filter(|m| m.name.eq_ignore_ascii_case(&t.member))
                {
                    for p in m.params.iter_mut().filter(|p| {
                        p.name.as_deref() == Some(t.param.as_str())
                            && p.type_name
                                .trim_end()
                                .trim_end_matches("[]")
                                .eq_ignore_ascii_case(&t.from)
                    }) {
                        // An `EXTENT` parameter keeps its marker.
                        p.type_name = match p.type_name.trim_end().ends_with("[]") {
                            true => format!("{}[]", t.type_name),
                            false => t.type_name.clone(),
                        };
                        found = true;
                    }
                }
            }
            if !found {
                return Err(format!(
                    "stale override: `{}` has no member `{}` with a parameter `{}` of type `{}`",
                    o.type_name, t.member, t.param, t.from
                ));
            }
        } else if let Some(m) = &o.add_member {
            let list = member_list(entry, &m.kind)?;
            let added = to_member(m);
            let same_types = |x: &Member| {
                x.params.len() == added.params.len()
                    && x.params
                        .iter()
                        .zip(&added.params)
                        .all(|(a, b)| a.type_name.eq_ignore_ascii_case(&b.type_name))
            };
            if list
                .iter()
                .any(|x| x.name.eq_ignore_ascii_case(&m.name) && same_types(x))
            {
                return Err(format!(
                    "stale override: `{}` already has a {} `{}` with these parameter types",
                    o.type_name, m.kind, m.name
                ));
            }
            list.push(added);
        } else if let Some(m) = &o.replace_member {
            let list = member_list(entry, &m.kind)?;
            let at = exactly_one(list, &m.name, &o.type_name, &m.kind)?;
            list[at] = to_member(m);
        }
    }
    Ok(touched)
}

// =============================================================================
// Names and types
// =============================================================================

/// Split `HashMap<K,V>` into `HashMap` and `["K", "V"]`.
pub fn split_generic(name: &str) -> (&str, Vec<&str>) {
    match name.split_once('<') {
        Some((base, rest)) => (
            base,
            rest.trim_end_matches('>')
                .split(',')
                .map(str::trim)
                .filter(|p| !p.is_empty())
                .collect(),
        ),
        None => (name, Vec::new()),
    }
}

/// An ABL name: `[A-Za-z_][A-Za-z0-9_#$%&-]*`.
fn is_abl_name(s: &str) -> bool {
    let mut bytes = s.bytes();
    matches!(bytes.next(), Some(b) if b.is_ascii_alphabetic() || b == b'_')
        && bytes.all(|b| b.is_ascii_alphanumeric() || b"_#$%&-".contains(&b))
}

/// A lowercase handle placeholder such as `pds-handle`.
fn is_handle_placeholder(s: &str) -> bool {
    s.strip_suffix("-handle").is_some_and(|head| {
        !head.is_empty()
            && head
                .split('-')
                .all(|w| !w.is_empty() && w.bytes().all(|b| b.is_ascii_lowercase()))
    })
}

/// Strip a trailing ASCII word (any case) preceded by whitespace.
fn strip_word_suffix<'a>(s: &'a str, word: &str) -> Option<&'a str> {
    let n = word.len();
    if s.len() <= n || !s.is_char_boundary(s.len() - n) {
        return None;
    }
    let (head, tail) = s.split_at(s.len() - n);
    if !tail.eq_ignore_ascii_case(word) || !head.ends_with(char::is_whitespace) {
        return None;
    }
    Some(head.trim_end())
}

/// Write `X EXTENT`, `X EXTENT 3` and `X class EXTENT` as `X[]`.
fn extent_to_brackets(t: &str) -> String {
    let mut head = t;
    if let Some((h, size)) = t.rsplit_once(char::is_whitespace)
        && !size.is_empty()
        && size.bytes().all(|b| b.is_ascii_digit())
        && strip_word_suffix(h.trim_end(), "EXTENT").is_some()
    {
        head = h.trim_end();
    }
    if let Some(h) = strip_word_suffix(head, "EXTENT") {
        let h = strip_word_suffix(h, "class").unwrap_or(h);
        if !h.is_empty() {
            return format!("{h}[]");
        }
    }
    t.to_string()
}

/// Remove a kind word after the type (`X enumeration`, `X class[]`).
fn strip_kind_word(t: &str) -> String {
    let (body, arr) = match t.strip_suffix("[]") {
        Some(b) => (b, "[]"),
        None => (t, ""),
    };
    for word in ["enumeration", "class", "interface"] {
        if let Some(h) = strip_word_suffix(body, word) {
            return format!("{h}{arr}");
        }
    }
    t.to_string()
}

/// What a raw type string reads as, before arrays are applied.
enum Read {
    Void,
    Primitive(&'static str),
    Shape(&'static str),
    Class(usize),
    TypeParam(u8),
    Unknown,
    Damaged,
}

struct Names {
    /// Folded base name to index.
    by_key: HashMap<String, usize>,
    /// Folded simple name to the indexes of the types that have it.
    by_simple: HashMap<String, Vec<usize>>,
    /// The types whose simple name is the name of an ABL primitive
    /// (`OpenEdge.Core.Memptr`, `Decimal`, `Integer`, `WidgetHandle`). The API
    /// reference links the primitive to the page of such a type, so a scraped
    /// type with this name may be either: it reads as `Unknown`, unless the
    /// text says `CLASS` (an override that knows it is the class).
    conflated: Vec<bool>,
}

impl Names {
    /// A catalog name: as written, then in `package`, then a unique simple name.
    fn lookup(&self, name: &str, package: &str) -> Option<usize> {
        if let Some(i) = self.by_key.get(&name.to_ascii_lowercase()) {
            return Some(*i);
        }
        if name.contains('.') {
            return None;
        }
        let qualified = format!("{package}.{name}").to_ascii_lowercase();
        if let Some(i) = self.by_key.get(&qualified) {
            return Some(*i);
        }
        match self
            .by_simple
            .get(&name.to_ascii_lowercase())
            .map(Vec::as_slice)
        {
            Some([only]) => Some(*only),
            _ => None,
        }
    }
}

struct Owner<'a> {
    index: usize,
    kind: TypeKind,
    package: &'a str,
    type_params: &'a [String],
}

/// `t` without a leading `CLASS ` keyword (any case), when it has one and a
/// type after it.
fn strip_class_keyword(t: &str) -> Option<&str> {
    let head = t.get(..6)?;
    (head.eq_ignore_ascii_case("CLASS ") && t.len() > 6).then(|| &t[6..])
}

/// Read a raw type of a member of `owner`, and the type as scraped: they
/// differ for a conflated name (see [`Names::conflated`]), which reads as
/// `Unknown`.
fn read_type(raw: &str, owner: &Owner, names: &Names) -> (NTypeOrDamaged, NType) {
    let read = read_scraped(raw, owner, names);
    let explicit_class = strip_class_keyword(raw.trim()).is_some();
    match read {
        NTypeOrDamaged::Type(t @ (NType::Class(i) | NType::ClassArray(i)))
            if names.conflated[i] && !explicit_class =>
        {
            (NTypeOrDamaged::Type(NType::Unknown), t)
        }
        NTypeOrDamaged::Type(t) => (NTypeOrDamaged::Type(t), t),
        NTypeOrDamaged::Damaged => (NTypeOrDamaged::Damaged, NType::Unknown),
    }
}

/// [`read_type`] with no rule for conflated names.
fn read_scraped(raw: &str, owner: &Owner, names: &Names) -> NTypeOrDamaged {
    let mut t = raw.trim().to_string();
    if let Some(rest) = strip_class_keyword(&t) {
        t = rest.trim().to_string();
    }
    t = extent_to_brackets(&t);
    t = strip_kind_word(&t);
    let (base, array) = match t.strip_suffix("[]") {
        Some(b) => (b.trim(), true),
        None => (t.as_str(), false),
    };
    let read = read_base(base, owner, names);
    match (read, array) {
        (Read::Damaged, _) => NTypeOrDamaged::Damaged,
        (Read::Unknown, _) | (Read::TypeParam(_), true) => NTypeOrDamaged::Type(NType::Unknown),
        (Read::TypeParam(i), false) => NTypeOrDamaged::Type(NType::TypeParam(i)),
        (Read::Void, false) => NTypeOrDamaged::Type(NType::Void),
        (Read::Primitive(p), false) => NTypeOrDamaged::Type(NType::Primitive(p)),
        (Read::Primitive(p), true) => NTypeOrDamaged::Type(NType::PrimitiveArray(p)),
        (Read::Class(i), false) => NTypeOrDamaged::Type(NType::Class(i)),
        (Read::Class(i), true) => NTypeOrDamaged::Type(NType::ClassArray(i)),
        (Read::Shape(s), false) => NTypeOrDamaged::Type(NType::Shape(s)),
        // An array of VOID or of a parameter shape is not an ABL type.
        (Read::Void | Read::Shape(_), true) => NTypeOrDamaged::Damaged,
    }
}

enum NTypeOrDamaged {
    Type(NType),
    Damaged,
}

fn read_base(b: &str, owner: &Owner, names: &Names) -> Read {
    let lower = b.to_ascii_lowercase();
    if owner.kind == TypeKind::Enum
        && matches!(
            lower.as_str(),
            "enum type" | "flag enum type" | "flag-enum-type"
        )
    {
        return Read::Class(owner.index);
    }
    if lower == "parameter-value" {
        return Read::Unknown;
    }
    if b.eq_ignore_ascii_case("VOID") {
        return Read::Void;
    }
    if let Some((_, v)) = PRIMITIVES.iter().find(|(n, _)| n.eq_ignore_ascii_case(b)) {
        return Read::Primitive(v);
    }
    if let Some((_, v)) = SHAPES.iter().find(|(n, _)| n.eq_ignore_ascii_case(b)) {
        return Read::Shape(v);
    }
    if is_handle_placeholder(b) {
        return Read::Primitive("Handle");
    }
    if let Some(at) = owner.type_params.iter().position(|p| p == b) {
        return Read::TypeParam(at as u8);
    }
    let (generic_base, args) = split_generic(b);
    let generic = b.contains('<');
    if let Some(i) = names.lookup(generic_base, owner.package) {
        // A generic instance is not modelled.
        return if generic || !args.is_empty() {
            Read::Unknown
        } else {
            Read::Class(i)
        };
    }
    if DOTNET_PREFIXES.iter().any(|p| lower.starts_with(p)) {
        return Read::Unknown;
    }
    Read::Damaged
}

fn read_super(raw: &str, package: &str, names: &Names) -> NSuper {
    let mut t = raw.trim();
    if let Some(rest) = strip_class_keyword(t) {
        t = rest.trim();
    }
    let (base, _) = split_generic(t);
    if let Some(i) = names.lookup(base, package) {
        return NSuper::Catalog(i);
    }
    let lower = base.to_ascii_lowercase();
    if DOTNET_PREFIXES.iter().any(|p| lower.starts_with(p)) {
        NSuper::External(raw.to_string())
    } else {
        NSuper::Unresolved(raw.to_string())
    }
}

/// A member name `&ltFile> Files`: the renderer split a generic return type.
fn split_generic_name(name: &str) -> Option<&str> {
    let rest = name.strip_prefix("&lt")?;
    let (_, tail) = rest.split_once("> ")?;
    (!tail.is_empty() && tail.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_'))
        .then_some(tail)
}

// =============================================================================
// The normaliser
// =============================================================================

/// The kind, static-constructor flag, folded name and folded parameter types
/// of a member row.
type Signature = (NMemberKind, bool, String, Vec<String>);

/// Normalise `index` (with the overrides already applied).
pub fn normalise(index: &BuiltinIndex) -> Result<Catalog, String> {
    let mut entries: Vec<(&String, &TypeEntry)> = index.types.iter().collect();
    entries.sort_by_key(|(name, _)| split_generic(name).0.to_ascii_lowercase());

    let mut by_key = HashMap::new();
    let mut by_simple: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, (name, _)) in entries.iter().enumerate() {
        let base = split_generic(name).0;
        if by_key.insert(base.to_ascii_lowercase(), i).is_some() {
            return Err(format!("two built-in types share the lookup key `{base}`"));
        }
        let simple = base.rsplit('.').next().unwrap_or(base);
        by_simple
            .entry(simple.to_ascii_lowercase())
            .or_default()
            .push(i);
    }
    let conflated = entries
        .iter()
        .map(|(name, _)| {
            let base = split_generic(name).0;
            let simple = base.rsplit('.').next().unwrap_or(base).replace('-', "");
            PRIMITIVES
                .iter()
                .any(|(p, _)| p.replace('-', "").eq_ignore_ascii_case(&simple))
        })
        .collect();
    let names = Names {
        by_key,
        by_simple,
        conflated,
    };

    let mut types = Vec::with_capacity(entries.len());
    for (i, (full_name, e)) in entries.iter().enumerate() {
        let (base, params) = split_generic(full_name);
        let type_params: Vec<String> = params.iter().map(|p| p.to_string()).collect();
        let owner = Owner {
            index: i,
            kind: e.kind,
            package: base.rsplit_once('.').map_or("", |(p, _)| p),
            type_params: &type_params,
        };
        let api_bundle = !base.starts_with("Progress.");
        let mut damaged = false;
        let mut members: Vec<NMember> = Vec::new();
        let mut seen: Vec<(Signature, &Member)> = Vec::new();
        let groups = [
            (NMemberKind::Constructor, &e.constructors),
            (NMemberKind::Method, &e.methods),
            (NMemberKind::Property, &e.properties),
            (NMemberKind::Event, &e.events),
        ];
        for (kind, list) in groups {
            for m in list.iter() {
                let split = split_generic_name(&m.name);
                let name = split.unwrap_or(&m.name);
                if !is_abl_name(name) {
                    damaged = true;
                    continue;
                }
                let static_ctor = kind == NMemberKind::Constructor
                    && m.return_type
                        .as_deref()
                        .is_some_and(|r| r.trim().eq_ignore_ascii_case("STATIC"));
                let sig = (
                    kind,
                    static_ctor,
                    name.to_ascii_lowercase(),
                    m.params
                        .iter()
                        .map(|p| p.type_name.to_ascii_lowercase())
                        .collect::<Vec<_>>(),
                );
                // Two rows with one signature are a scrape that lost a type,
                // or an override that made two rows one (an override must
                // say which is which). Rows the page repeats are dropped
                // before the overrides (`drop_repeated_rows`).
                if let Some((_, first)) = seen.iter().find(|(s, _)| *s == sig) {
                    return Err(format!(
                        "`{base}` has two {kind:?} rows `{}` with one signature: \
                         {first:?} and {m:?}; add an override that tells them apart",
                        m.name
                    ));
                }
                seen.push((sig, m));
                let mut is_static = m.is_static;
                let mut scraped = NType::Unknown;
                let ty = match kind {
                    NMemberKind::Constructor => {
                        is_static |= static_ctor;
                        NType::Void
                    }
                    NMemberKind::Event => NType::Void,
                    NMemberKind::Method | NMemberKind::Property => {
                        if split.is_some() {
                            NType::Unknown
                        } else {
                            match m.return_type.as_deref() {
                                None if kind == NMemberKind::Method && api_bundle => NType::Void,
                                // The member exists; its type is not documented.
                                None if kind == NMemberKind::Property => NType::Unknown,
                                None => {
                                    damaged = true;
                                    continue;
                                }
                                Some(raw) => match read_type(raw, &owner, &names) {
                                    (NTypeOrDamaged::Type(t), s) => {
                                        scraped = s;
                                        t
                                    }
                                    (NTypeOrDamaged::Damaged, _) => {
                                        damaged = true;
                                        continue;
                                    }
                                },
                            }
                        }
                    }
                };
                let mut params = Vec::with_capacity(m.params.len());
                let mut bad_param = false;
                for p in &m.params {
                    // A name the scrape broke (`) PUBLIC X (INPUT size`) is damage.
                    if p.name.as_deref().is_some_and(|n| !is_abl_name(n)) {
                        bad_param = true;
                        continue;
                    }
                    match read_type(&p.type_name, &owner, &names) {
                        (NTypeOrDamaged::Type(ty), scraped) => params.push(NParam {
                            name: p.name.clone().unwrap_or_default(),
                            ty,
                            scraped,
                        }),
                        (NTypeOrDamaged::Damaged, _) => bad_param = true,
                    }
                }
                if bad_param {
                    damaged = true;
                    continue;
                }
                members.push(NMember {
                    name: name.to_string(),
                    kind,
                    is_static,
                    is_abstract: m.is_abstract,
                    is_final: m.is_final,
                    scraped: if matches!(ty, NType::Unknown) {
                        scraped
                    } else {
                        ty
                    },
                    ty,
                    params,
                });
            }
        }

        let parent = e
            .inherits
            .as_deref()
            .map(|s| read_super(s, owner.package, &names));
        let interfaces: Vec<NSuper> = e
            .implements
            .iter()
            .map(|s| read_super(s, owner.package, &names))
            .collect();
        let is_object = base.eq_ignore_ascii_case("Progress.Lang.Object");
        let header_known = (parent.is_some() || is_object || e.kind == TypeKind::Interface)
            && parent
                .iter()
                .chain(&interfaces)
                .all(|s| matches!(s, NSuper::Catalog(_)));

        types.push(NTypeEntry {
            name: base.to_string(),
            kind: e.kind,
            type_params,
            page: e.page.clone(),
            is_abstract: e.is_abstract,
            is_final: e.is_final,
            parent,
            interfaces,
            members,
            enum_members: e.enum_members.clone(),
            members_trusted: !damaged,
            header_known,
            complete: false,
        });
    }

    let disagreements = interface_disagreements(&types);
    if !disagreements.is_empty() {
        return Err(format!(
            "members disagree with the interfaces they implement; add overrides:\n{}",
            disagreements.join("\n")
        ));
    }

    // Completeness: own facts, then every supertype complete. A cycle is an
    // error.
    let mut state = vec![0u8; types.len()]; // 0 new, 1 on stack, 2 done
    let mut complete = vec![false; types.len()];
    fn visit(
        i: usize,
        types: &[NTypeEntry],
        state: &mut [u8],
        complete: &mut [bool],
    ) -> Result<bool, String> {
        match state[i] {
            2 => return Ok(complete[i]),
            1 => {
                return Err(format!(
                    "the catalog has a cycle through `{}`",
                    types[i].name
                ));
            }
            _ => {}
        }
        state[i] = 1;
        let t = &types[i];
        let mut ok = t.members_trusted
            && t.header_known
            && t.type_params.is_empty()
            && !(t.kind == TypeKind::Enum && t.enum_members.is_empty());
        for s in t.parent.iter().chain(&t.interfaces) {
            if let NSuper::Catalog(j) = s {
                ok &= visit(*j, types, state, complete)?;
            }
        }
        state[i] = 2;
        complete[i] = ok;
        Ok(ok)
    }
    for i in 0..types.len() {
        visit(i, &types, &mut state, &mut complete)?;
    }
    for (t, c) in types.iter_mut().zip(complete) {
        t.complete = c;
    }

    Ok(Catalog {
        version: index.version.clone(),
        types,
    })
}

/// Each member of a type whose type, as scraped, disagrees with the member
/// of an interface the type implements (directly or through a supertype):
/// a property of another type, or a method with the same parameter types and
/// another return type. ABL requires them to agree, so a disagreement is a
/// scrape that read a type wrongly (`OpenEdge.Core.Memptr:Value` read as the
/// class where `IMemptrHolder:Value` says `MEMPTR`). A type that either side
/// does not know is not compared.
pub fn interface_disagreements(types: &[NTypeEntry]) -> Vec<String> {
    let known = |t: &NType| !matches!(t, NType::Unknown | NType::TypeParam(_));
    let mut out = Vec::new();
    for t in types {
        // Every interface above `t`, each once.
        let mut stack: Vec<usize> = t
            .parent
            .iter()
            .chain(&t.interfaces)
            .filter_map(|s| match s {
                NSuper::Catalog(i) => Some(*i),
                _ => None,
            })
            .collect();
        let mut seen = Vec::new();
        while let Some(i) = stack.pop() {
            if seen.contains(&i) {
                continue;
            }
            seen.push(i);
            let up = &types[i];
            stack.extend(
                up.parent
                    .iter()
                    .chain(&up.interfaces)
                    .filter_map(|s| match s {
                        NSuper::Catalog(j) => Some(*j),
                        _ => None,
                    }),
            );
            if up.kind != TypeKind::Interface {
                continue;
            }
            for wanted in &up.members {
                for own in t
                    .members
                    .iter()
                    .filter(|m| m.kind == wanted.kind && m.name.eq_ignore_ascii_case(&wanted.name))
                {
                    let same_params = own.params.len() == wanted.params.len()
                        && own.params.iter().zip(&wanted.params).all(|(a, b)| {
                            known(&a.scraped) && known(&b.scraped) && a.scraped == b.scraped
                        });
                    let comparable = match wanted.kind {
                        NMemberKind::Property => true,
                        NMemberKind::Method => same_params,
                        NMemberKind::Constructor | NMemberKind::Event => false,
                    };
                    if comparable
                        && known(&own.scraped)
                        && known(&wanted.scraped)
                        && own.scraped != wanted.scraped
                    {
                        out.push(format!(
                            "`{}:{}` is {:?}, but `{}:{}` is {:?}",
                            t.name, own.name, own.scraped, up.name, wanted.name, wanted.scraped
                        ));
                    }
                }
            }
        }
    }
    out
}

/// Read the JSON and the overrides from the workspace `root` and normalise.
pub fn load(root: &std::path::Path) -> Result<Catalog, String> {
    let mut index =
        crate::builtin_class_table::read_index(&root.join(crate::builtin_class_table::JSON_PATH))?;
    let path = root.join(OVERRIDES_PATH);
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {e}", path.display()))?;
    prepare(&mut index, &text)?;
    normalise(&index)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn root() -> std::path::PathBuf {
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
    }

    fn checked_in() -> Catalog {
        load(&root()).unwrap()
    }

    #[test]
    fn counts_on_the_checked_in_catalog() {
        let c = checked_in();
        assert_eq!(c.types.len(), 1248);
        assert_eq!(c.types.iter().filter(|t| t.members_trusted).count(), 1200);
        assert_eq!(c.types.iter().filter(|t| t.complete).count(), 1065);
        let progress: Vec<_> = c
            .types
            .iter()
            .filter(|t| t.name.starts_with("Progress."))
            .collect();
        assert_eq!(progress.len(), 108);
        assert_eq!(progress.iter().filter(|t| t.complete).count(), 36);
    }

    #[test]
    fn the_named_types_have_their_state() {
        let c = checked_in();
        for name in [
            "Progress.Lang.Object",
            "Progress.Lang.Enum",
            "Progress.Lang.FlagsEnum",
            "Progress.Lang.Error",
            "Progress.Lang.ProError",
            "Progress.Lang.AppError",
            "Progress.Lang.SysError",
            "Progress.Json.ObjectModel.JsonObject",
            "Progress.Json.ObjectModel.JsonConstruct",
            "Progress.Json.ObjectModel.JsonDataType",
            "OpenEdge.Core.String",
            "Progress.ApplicationServer.AdapterTypes",
        ] {
            assert!(c.get(name).unwrap().complete, "{name} should be complete");
        }
        for name in [
            "Progress.Lang.Class",
            "Progress.Json.ObjectModel.JsonArray",
            "Progress.Json.ObjectModel.ObjectModelParser",
            "Progress.Collections.List",
            "Progress.Windows.Form",
            "Progress.IO.ByteOrder",
            "OpenEdge.Net.UriSchemeEnum",
        ] {
            assert!(
                !c.get(name).unwrap().complete,
                "{name} should not be complete"
            );
        }
    }

    #[test]
    fn the_overrides_fix_the_error_classes() {
        let c = checked_in();
        let app = c.get("Progress.Lang.AppError").unwrap();
        let pro = c
            .types
            .iter()
            .position(|t| t.name == "Progress.Lang.ProError");
        assert_eq!(app.parent, Some(NSuper::Catalog(pro.unwrap())));
        for name in ["Progress.Lang.ProError", "Progress.Lang.Error"] {
            let t = c.get(name).unwrap();
            let call_stack = t.members.iter().find(|m| m.name == "CallStack").unwrap();
            assert_eq!(call_stack.kind, NMemberKind::Property);
            assert_eq!(call_stack.ty, NType::Primitive("Character"));
            assert!(t.members_trusted);
        }
        // A return the scrape read as a scalar is an EXTENT.
        let json = c.get("Progress.Json.ObjectModel.JsonObject").unwrap();
        let names = json.members.iter().find(|m| m.name == "GetNames").unwrap();
        assert_eq!(names.ty, NType::PrimitiveArray("Character"));
    }

    /// A one-type index named `owner` with the given member JSON.
    fn index_with(types: &[(&str, &str)]) -> BuiltinIndex {
        let types = types
            .iter()
            .map(|(name, entry)| format!("{name:?}: {entry}"))
            .collect::<Vec<_>>()
            .join(",");
        serde_json::from_str(&format!(
            r#"{{"bundle_ids": [], "version": "12.8", "generated_at": "", "packages": [], "types": {{{types}}}}}"#
        ))
        .unwrap()
    }

    const OBJECT: (&str, &str) = (
        "Progress.Lang.Object",
        r#"{"kind": "class", "package": "Progress.Lang", "page": "o.html"}"#,
    );

    fn one(entry: &str) -> NTypeEntry {
        let index = index_with(&[OBJECT, ("A.B", entry)]);
        let c = normalise(&index).unwrap();
        c.get("A.B").unwrap().clone()
    }

    fn method_type(rt: &str) -> Option<NType> {
        let t = one(&format!(
            r#"{{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{{"name": "M", "return_type": {rt:?}}}]}}"#
        ));
        t.members.first().map(|m| m.ty)
    }

    #[test]
    fn class_prefix_is_removed() {
        assert_eq!(
            method_type("CLASS Progress.Lang.Object"),
            Some(NType::Class(1))
        );
        assert_eq!(
            method_type("CLASS Progress.Lang.Object[]"),
            Some(NType::ClassArray(1))
        );
    }

    #[test]
    fn extent_forms_are_arrays() {
        assert_eq!(
            method_type("CHARACTER EXTENT"),
            Some(NType::PrimitiveArray("Character"))
        );
        assert_eq!(
            method_type("CHARACTER EXTENT 3"),
            Some(NType::PrimitiveArray("Character"))
        );
        assert_eq!(
            method_type("Progress.Lang.Object class EXTENT"),
            Some(NType::ClassArray(1))
        );
        assert_eq!(
            method_type("character[]"),
            Some(NType::PrimitiveArray("Character"))
        );
    }

    #[test]
    fn a_kind_word_after_the_type_is_removed() {
        assert_eq!(
            method_type("Progress.Lang.Object enumeration"),
            Some(NType::Class(1))
        );
    }

    #[test]
    fn an_enum_names_itself_as_enum_type() {
        let t = one(
            r#"{"kind": "enum", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "enum_members": ["X"],
                "methods": [{"name": "GetEnum", "return_type": "Enum type"}]}"#,
        );
        assert_eq!(t.members[0].ty, NType::Class(0));
    }

    #[test]
    fn parameter_value_is_any_type_and_placeholders_are_handles() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{"name": "M", "return_type": "VOID",
                  "params": [{"name": "a", "type_name": "parameter-value"},
                             {"name": "b", "type_name": "pds-handle"},
                             {"type_name": "TABLE-HANDLE"}]}]}"#,
        );
        let tys: Vec<_> = t.members[0].params.iter().map(|p| p.ty).collect();
        assert_eq!(
            tys,
            [
                NType::Unknown,
                NType::Primitive("Handle"),
                NType::Shape("TableHandle")
            ]
        );
        assert_eq!(t.members[0].params[2].name, "");
        assert!(t.members_trusted);
    }

    #[test]
    fn a_static_constructor_is_static() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "constructors": [{"name": "B", "return_type": "STATIC"}, {"name": "B"}]}"#,
        );
        assert!(t.members[0].is_static);
        assert!(!t.members[1].is_static);
    }

    #[test]
    fn a_missing_return_type_is_void_in_the_api_bundle_and_damaged_in_progress() {
        assert_eq!(
            one(r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{"name": "M"}]}"#)
            .members[0]
                .ty,
            NType::Void
        );
        let index = index_with(&[
            OBJECT,
            (
                "Progress.X",
                r#"{"kind": "class", "package": "Progress", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "M"}]}"#,
            ),
        ]);
        let c = normalise(&index).unwrap();
        let t = c.get("Progress.X").unwrap();
        assert!(t.members.is_empty());
        assert!(!t.members_trusted);
        assert!(!t.complete);
    }

    #[test]
    fn a_split_generic_name_is_kept_with_an_unknown_type() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "properties": [{"name": "&ltFile> Files", "return_type": "List"}]}"#,
        );
        assert_eq!(t.members[0].name, "Files");
        assert_eq!(t.members[0].ty, NType::Unknown);
        assert!(t.members_trusted);
    }

    #[test]
    fn a_member_name_that_is_no_abl_name_is_damaged() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{"name": "[ return-value = ] x:Invoke", "return_type": "VOID"}]}"#,
        );
        assert!(t.members.is_empty());
        assert!(!t.members_trusted);
    }

    #[test]
    fn unqualified_names_resolve_in_the_package_then_by_unique_simple_name() {
        let index = index_with(&[
            OBJECT,
            (
                "A.Item",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Object"}"#,
            ),
            (
                "B.Item",
                r#"{"kind": "class", "package": "B", "page": "p", "inherits": "Progress.Lang.Object"}"#,
            ),
            (
                "A.User",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "Own", "return_type": "Item"}]}"#,
            ),
            (
                "C.User",
                r#"{"kind": "class", "package": "C", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "Ambiguous", "return_type": "Item"}]}"#,
            ),
        ]);
        let c = normalise(&index).unwrap();
        let a_item = c.types.iter().position(|t| t.name == "A.Item").unwrap();
        assert_eq!(c.get("A.User").unwrap().members[0].ty, NType::Class(a_item));
        assert!(!c.get("C.User").unwrap().members_trusted);
        // The unique simple name `Object` resolves the supertype.
        assert!(c.get("A.Item").unwrap().header_known);
    }

    #[test]
    fn generics_and_dotnet_types_are_type_parameters_or_unknown() {
        let index = index_with(&[
            OBJECT,
            (
                "A.List<T>",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "Get", "return_type": "T"},
                                {"name": "Sub", "return_type": "A.List<T>"},
                                {"name": "Net", "return_type": "System.Object"}]}"#,
            ),
            (
                "A.Form",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "System.Windows.Forms.Form"}"#,
            ),
        ]);
        let c = normalise(&index).unwrap();
        let list = c.get("A.List").unwrap();
        let ty = |name: &str| list.members.iter().find(|m| m.name == name).unwrap().ty;
        assert_eq!(ty("Get"), NType::TypeParam(0));
        assert_eq!(ty("Sub"), NType::Unknown);
        assert_eq!(ty("Net"), NType::Unknown);
        assert!(list.members_trusted);
        assert!(!list.complete);
        let form = c.get("A.Form").unwrap();
        assert!(matches!(form.parent, Some(NSuper::External(_))));
        assert!(!form.header_known);
    }

    #[test]
    fn a_parameter_name_that_is_no_abl_name_is_damaged() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "constructors": [{"name": "B", "params": [{"name": ") PUBLIC B (INPUT size", "type_name": "INT64"}]}]}"#,
        );
        assert!(t.members.is_empty());
        assert!(!t.members_trusted);
    }

    #[test]
    fn an_extent_property_override_makes_the_property_an_array() {
        let mut index = index_with(&[
            OBJECT,
            (
                "A.B",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "properties": [{"name": "Value", "return_type": "Progress.Lang.Object"}]}"#,
            ),
        ]);
        apply_overrides(
            &mut index,
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nextent_property = { name = \"value\", from = \"Progress.Lang.Object\" }\n",
        )
        .unwrap();
        let c = normalise(&index).unwrap();
        assert_eq!(c.get("A.B").unwrap().members[0].ty, NType::ClassArray(1));
    }

    #[test]
    fn a_repeated_row_is_kept_once_and_two_rows_with_one_signature_fail() {
        let repeated = r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{"name": "M", "return_type": "INTEGER", "params": [{"name": "a", "type_name": "CHARACTER"}]},
                            {"name": "M", "return_type": "INTEGER", "params": [{"name": "a", "type_name": "CHARACTER"}]}]}"#;
        let mut index = index_with(&[OBJECT, ("A.B", repeated)]);
        // The repeat is dropped on the scrape; left in, it is a collision.
        assert!(
            normalise(&index)
                .unwrap_err()
                .contains("with one signature")
        );
        assert_eq!(drop_repeated_rows(&mut index), 1);
        assert_eq!(
            normalise(&index).unwrap().get("A.B").unwrap().members.len(),
            1
        );
        let index = index_with(&[
            OBJECT,
            (
                "A.B",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "M", "params": [{"name": "poData", "type_name": "A.C"}]},
                                {"name": "m", "params": [{"name": "pmData", "type_name": "A.C"}]}]}"#,
            ),
            (
                "A.C",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object"}"#,
            ),
        ]);
        assert!(
            normalise(&index)
                .unwrap_err()
                .contains("with one signature")
        );
    }

    /// `A.Memptr` is named like the primitive `MEMPTR`.
    const MEMPTR_CLASS: (&str, &str) = (
        "A.Memptr",
        r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object"}"#,
    );

    #[test]
    fn a_class_named_like_a_primitive_is_not_known_unless_written_class() {
        let index = index_with(&[
            OBJECT,
            MEMPTR_CLASS,
            (
                "A.User",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "Get", "return_type": "A.Memptr"},
                                {"name": "Put", "params": [{"name": "o", "type_name": "CLASS A.Memptr"}]},
                                {"name": "All", "return_type": "A.Memptr[]"}]}"#,
            ),
        ]);
        let c = normalise(&index).unwrap();
        let user = c.get("A.User").unwrap();
        let member = |n: &str| user.members.iter().find(|m| m.name == n).unwrap();
        let memptr = c.types.iter().position(|t| t.name == "A.Memptr").unwrap();
        assert_eq!(member("Get").ty, NType::Unknown);
        assert_eq!(member("Get").scraped, NType::Class(memptr));
        assert_eq!(member("All").ty, NType::Unknown);
        assert_eq!(member("Put").params[0].ty, NType::Class(memptr));
        assert!(user.members_trusted);
    }

    #[test]
    fn a_member_that_disagrees_with_its_interface_fails() {
        let types = [
            OBJECT,
            MEMPTR_CLASS,
            (
                "A.IHolder",
                r#"{"kind": "interface", "package": "A", "page": "p",
                    "properties": [{"name": "Value", "return_type": "MEMPTR"}],
                    "methods": [{"name": "Size", "return_type": "INT64", "params": [{"name": "a", "type_name": "INTEGER"}]}]}"#,
            ),
        ];
        let holder = |value: &str, size: &str| {
            format!(
                r#"{{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "implements": ["A.IHolder"],
                    "properties": [{{"name": "Value", "return_type": "{value}"}}],
                    "methods": [{{"name": "Size", "return_type": "{size}", "params": [{{"name": "a", "type_name": "INTEGER"}}]}},
                                {{"name": "Size", "return_type": "LOGICAL", "params": [{{"name": "a", "type_name": "CHARACTER"}}]}}]}}"#
            )
        };
        let run = |value: &str, size: &str| {
            let entry = holder(value, size);
            let mut all = types.to_vec();
            all.push(("A.Holder", entry.as_str()));
            normalise(&index_with(&all))
        };
        // The scraped class where the interface says MEMPTR, even though the
        // normaliser reads it as not known.
        let err = run("A.Memptr", "INT64").unwrap_err();
        assert!(
            err.contains("`A.Holder:Value`") && err.contains("`A.IHolder:Value`"),
            "{err}"
        );
        let err = run("MEMPTR", "INTEGER").unwrap_err();
        assert!(err.contains("`A.Holder:Size`"), "{err}");
        // An overload with other parameter types is not the interface method.
        assert!(run("MEMPTR", "INT64").is_ok());
    }

    #[test]
    fn a_param_type_override_types_only_the_parameters_of_its_scraped_type() {
        let mut index = index_with(&[
            OBJECT,
            MEMPTR_CLASS,
            (
                "A.W",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "Write", "params": [{"name": "pData", "type_name": "CHARACTER"}]},
                                {"name": "Write", "params": [{"name": "pData", "type_name": "A.Memptr"}]},
                                {"name": "Write", "params": [{"name": "pData", "type_name": "A.Memptr[]"}]}]}"#,
            ),
        ]);
        let text = "[[override]]\ntype = \"A.W\"\nchecked_against = \"x\"\nparam_type = { member = \"write\", param = \"pData\", from = \"A.Memptr\", type = \"MEMPTR\" }\n";
        prepare(&mut index, text).unwrap();
        let c = normalise(&index).unwrap();
        let tys: Vec<_> = c
            .get("A.W")
            .unwrap()
            .members
            .iter()
            .map(|m| m.params[0].ty)
            .collect();
        // The CHARACTER overload keeps its type: three rows stay three.
        assert_eq!(
            tys,
            [
                NType::Primitive("Character"),
                NType::Primitive("Memptr"),
                NType::PrimitiveArray("Memptr")
            ]
        );
        // The same entry again matches no parameter of that scraped type.
        assert!(
            apply_overrides(&mut index, text)
                .unwrap_err()
                .contains("of type `A.Memptr`")
        );
    }

    #[test]
    fn an_override_that_names_a_member_with_two_rows_fails() {
        let mut index = index_with(&[
            OBJECT,
            (
                "A.B",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "methods": [{"name": "M", "return_type": "INTEGER"},
                                {"name": "M", "return_type": "INTEGER", "params": [{"name": "a", "type_name": "CHARACTER"}]}]}"#,
            ),
        ]);
        for text in [
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nreplace_member = { kind = \"method\", name = \"M\", type = \"LOGICAL\" }\n",
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ndrop_member = { kind = \"method\", name = \"M\" }\n",
        ] {
            let err = apply_overrides(&mut index, text).unwrap_err();
            assert!(err.contains("has 2 method rows `M`, not one"), "{err}");
        }
        // An overload named by its parameter types is exact.
        apply_overrides(
            &mut index,
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nextent_return = { member = \"M\", params = [\"CHARACTER\"] }\n",
        )
        .unwrap();
        let rts: Vec<_> = index.types["A.B"]
            .methods
            .iter()
            .map(|m| m.return_type.clone().unwrap())
            .collect();
        assert_eq!(rts, ["INTEGER", "INTEGER[]"]);
    }

    #[test]
    fn an_override_that_makes_two_rows_one_fails() {
        let index = || {
            index_with(&[
                OBJECT,
                (
                    "A.W",
                    r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                        "methods": [{"name": "Write", "params": [{"name": "pData", "type_name": "CHARACTER"}]},
                                    {"name": "Write", "params": [{"name": "pData", "type_name": "LONGCHAR"}]}]}"#,
                ),
            ])
        };
        let text = "[[override]]\ntype = \"A.W\"\nchecked_against = \"x\"\nparam_type = { member = \"Write\", param = \"pData\", from = \"CHARACTER\", type = \"LONGCHAR\" }\n";
        let err = prepare(&mut index(), text).unwrap_err();
        assert!(err.contains("turn 2 `write` rows (2 distinct)"), "{err}");
        // And `normalise` alone refuses the two rows as one signature.
        let mut overridden = index();
        apply_overrides(&mut overridden, text).unwrap();
        assert!(
            normalise(&overridden)
                .unwrap_err()
                .contains("with one signature")
        );
    }

    #[test]
    fn the_checked_in_catalog_agrees_with_its_interfaces() {
        assert_eq!(
            interface_disagreements(&checked_in().types),
            Vec::<String>::new()
        );
    }

    #[test]
    fn a_class_with_no_supertype_has_no_known_header() {
        let t = one(r#"{"kind": "class", "package": "A", "page": "p"}"#);
        assert!(!t.header_known);
        assert!(!t.complete);
        let i = one(r#"{"kind": "interface", "package": "A", "page": "p"}"#);
        assert!(i.header_known);
        assert!(i.complete);
    }

    #[test]
    fn an_enum_with_no_members_is_not_complete() {
        let t = one(
            r#"{"kind": "enum", "package": "A", "page": "p", "inherits": "Progress.Lang.Object"}"#,
        );
        assert!(t.members_trusted);
        assert!(!t.complete);
    }

    #[test]
    fn a_cycle_is_an_error() {
        let index = index_with(&[
            OBJECT,
            (
                "A.X",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "A.Y"}"#,
            ),
            (
                "A.Y",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "A.X"}"#,
            ),
        ]);
        assert!(normalise(&index).unwrap_err().contains("cycle"));
    }

    #[test]
    fn a_stale_override_fails() {
        let mut index = index_with(&[
            OBJECT,
            (
                "A.B",
                r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                    "properties": [{"name": "P", "return_type": "INTEGER"}]}"#,
            ),
        ]);
        for (text, want) in [
            (
                "[[override]]\ntype = \"A.Nope\"\nchecked_against = \"x\"\ninherits = \"Progress.Lang.Object\"\n",
                "names no type",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ninherits = \"Progress.Lang.Object\"\n",
                "already documents",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ndrop_member = { kind = \"property\", name = \"Q\" }\n",
                "has 0 property rows `Q`",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nextent_return = { member = \"P\", params = [] }\n",
                "has 0 methods `P()`",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nextent_property = { name = \"P\", from = \"CHARACTER\" }\n",
                "is Some(\"INTEGER\"), not `CHARACTER`",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nparam_type = { member = \"P\", param = \"a\", from = \"X\", type = \"MEMPTR\" }\n",
                "no member `P` with a parameter `a` of type `X`",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nadd_member = { kind = \"property\", name = \"p\", type = \"INTEGER\" }\n",
                "already has",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nreplace_member = { kind = \"method\", name = \"P\", type = \"INTEGER\" }\n",
                "has 0 method rows `P`",
            ),
        ] {
            let err = apply_overrides(&mut index, text).unwrap_err();
            assert!(err.contains(want), "{err}");
        }
        apply_overrides(
            &mut index,
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ndrop_member = { kind = \"property\", name = \"P\" }\n",
        )
        .unwrap();
        assert!(index.types["A.B"].properties.is_empty());
    }

    #[test]
    fn extent_and_kind_words_parse() {
        assert_eq!(extent_to_brackets("X EXTENT"), "X[]");
        assert_eq!(extent_to_brackets("X extent 10"), "X[]");
        assert_eq!(extent_to_brackets("X class EXTENT"), "X[]");
        assert_eq!(
            extent_to_brackets("OpenEdge.DataAdmin.IExtent"),
            "OpenEdge.DataAdmin.IExtent"
        );
        assert_eq!(strip_kind_word("A.B enumeration"), "A.B");
        assert_eq!(strip_kind_word("A.B class[]"), "A.B[]");
        assert!(is_abl_name("NEXT-SIBLING"));
        assert!(!is_abl_name("file-name at line"));
        assert!(is_handle_placeholder("tt-handle"));
        assert!(!is_handle_placeholder("TABLE-HANDLE"));
        assert_eq!(strip_class_keyword("class A.B"), Some("A.B"));
        assert_eq!(strip_class_keyword("CLASS "), None);
        // A multi-byte character in the first six bytes is no keyword.
        assert_eq!(strip_class_keyword("Ünïcode.Type"), None);
    }
}
