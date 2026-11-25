//! Normalise the scraped built-in class index into a typed model.
//!
//! The scrape of the Progress documentation has gaps and errors (see
//! `docs/design/builtin-classes.md`, "Quirks and errors"). This module applies
//! the checked overrides of `resources/progress_builtin_overrides.toml`, reads
//! every member type into a typed value, drops the members it cannot read, and
//! computes the trust and completeness flags of each type. It never guesses a
//! signature: a member it cannot read is dropped and its type is not trusted.

use std::collections::HashMap;

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
    /// A type parameter, a generic instance, a .NET type, any type.
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
    drop_member: Option<String>,
    /// A method whose return is an `EXTENT` of a primitive that the scrape
    /// read as a scalar: every overload of the name with a scalar primitive
    /// return gets `[]`.
    extent_return: Option<String>,
    add_member: Option<OverrideMember>,
    replace_member: Option<OverrideMember>,
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

/// Apply the overrides in `text` to `index`. An entry that matches nothing is
/// an error, so a later scrape cannot leave a stale fix behind.
pub fn apply_overrides(index: &mut BuiltinIndex, text: &str) -> Result<(), String> {
    let file: OverridesFile =
        toml::from_str(text).map_err(|e| format!("parsing {OVERRIDES_PATH}: {e}"))?;
    for o in &file.overrides {
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
            o.add_member.is_some(),
            o.replace_member.is_some(),
        ];
        if what.iter().filter(|b| **b).count() != 1 {
            return Err(format!(
                "override of `{}` must name exactly one of inherits, drop_member, extent_return, add_member, replace_member",
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
        } else if let Some(name) = &o.drop_member {
            let mut found = false;
            for list in all_members(entry) {
                let before = list.len();
                list.retain(|m| m.name != *name);
                found |= list.len() != before;
            }
            if !found {
                return Err(format!(
                    "stale override: `{}` has no member `{name}` to drop",
                    o.type_name
                ));
            }
        } else if let Some(name) = &o.extent_return {
            let mut found = false;
            for m in entry
                .methods
                .iter_mut()
                .filter(|m| m.name.eq_ignore_ascii_case(name))
            {
                if let Some(rt) = &mut m.return_type
                    && PRIMITIVES
                        .iter()
                        .any(|(p, _)| p.eq_ignore_ascii_case(rt.trim()))
                {
                    rt.push_str("[]");
                    found = true;
                }
            }
            if !found {
                return Err(format!(
                    "stale override: `{}` has no method `{name}` with a scalar primitive return",
                    o.type_name
                ));
            }
        } else if let Some(m) = &o.add_member {
            let list = member_list(entry, &m.kind)?;
            if list.iter().any(|x| x.name.eq_ignore_ascii_case(&m.name)) {
                return Err(format!(
                    "stale override: `{}` already has a {} `{}`",
                    o.type_name, m.kind, m.name
                ));
            }
            list.push(to_member(m));
        } else if let Some(m) = &o.replace_member {
            let list = member_list(entry, &m.kind)?;
            let Some(slot) = list
                .iter_mut()
                .find(|x| x.name.eq_ignore_ascii_case(&m.name))
            else {
                return Err(format!(
                    "stale override: `{}` has no {} `{}` to replace",
                    o.type_name, m.kind, m.name
                ));
            };
            *slot = to_member(m);
        }
    }
    Ok(())
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
    Unknown,
    Damaged,
}

struct Names {
    /// Folded base name to index.
    by_key: HashMap<String, usize>,
    /// Folded simple name to the indexes of the types that have it.
    by_simple: HashMap<String, Vec<usize>>,
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

/// Read a raw type of a member of `owner`.
fn read_type(raw: &str, owner: &Owner, names: &Names) -> NTypeOrDamaged {
    let mut t = raw.trim().to_string();
    if t.len() > 6 && t[..6].eq_ignore_ascii_case("CLASS ") {
        t = t[6..].trim().to_string();
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
        (Read::Unknown, _) => NTypeOrDamaged::Type(NType::Unknown),
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
    if owner.type_params.iter().any(|p| p == b) {
        return Read::Unknown;
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
    if t.len() > 6 && t[..6].eq_ignore_ascii_case("CLASS ") {
        t = t[6..].trim();
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
    let names = Names { by_key, by_simple };

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
        let mut seen: Vec<(NMemberKind, bool, String, Vec<String>)> = Vec::new();
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
                if seen.contains(&sig) {
                    continue;
                }
                seen.push(sig);
                let mut is_static = m.is_static;
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
                                    NTypeOrDamaged::Type(t) => t,
                                    NTypeOrDamaged::Damaged => {
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
                        NTypeOrDamaged::Type(ty) => params.push(NParam {
                            name: p.name.clone().unwrap_or_default(),
                            ty,
                        }),
                        NTypeOrDamaged::Damaged => bad_param = true,
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

/// Read the JSON and the overrides from the workspace `root` and normalise.
pub fn load(root: &std::path::Path) -> Result<Catalog, String> {
    let mut index =
        crate::builtin_class_table::read_index(&root.join(crate::builtin_class_table::JSON_PATH))?;
    let path = root.join(OVERRIDES_PATH);
    let text =
        std::fs::read_to_string(&path).map_err(|e| format!("reading {}: {e}", path.display()))?;
    apply_overrides(&mut index, &text)?;
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
    fn generics_and_dotnet_types_are_unknown() {
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
        assert!(list.members.iter().all(|m| m.ty == NType::Unknown));
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
    fn a_duplicate_signature_is_kept_once() {
        let t = one(
            r#"{"kind": "class", "package": "A", "page": "p", "inherits": "Progress.Lang.Object",
                "methods": [{"name": "M", "return_type": "INTEGER", "params": [{"name": "a", "type_name": "CHARACTER"}]},
                            {"name": "m", "return_type": "LOGICAL", "params": [{"name": "b", "type_name": "character"}]}]}"#,
        );
        assert_eq!(t.members.len(), 1);
        assert_eq!(t.members[0].ty, NType::Primitive("Integer"));
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
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ndrop_member = \"Q\"\n",
                "no member `Q`",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nextent_return = \"P\"\n",
                "no method `P` with a scalar primitive return",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nadd_member = { kind = \"property\", name = \"p\", type = \"INTEGER\" }\n",
                "already has",
            ),
            (
                "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\nreplace_member = { kind = \"method\", name = \"P\", type = \"INTEGER\" }\n",
                "no method `P`",
            ),
        ] {
            let err = apply_overrides(&mut index, text).unwrap_err();
            assert!(err.contains(want), "{err}");
        }
        apply_overrides(
            &mut index,
            "[[override]]\ntype = \"A.B\"\nchecked_against = \"x\"\ndrop_member = \"P\"\n",
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
    }
}
