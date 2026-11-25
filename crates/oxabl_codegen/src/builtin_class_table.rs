//! Compile `resources/progress_builtin_classes.json` and the checked overrides
//! into the static tables of the semantic crate's built-in class catalog.
//!
//! The JSON stays the checked-in source of truth; the tables are flat static
//! arrays (one for types, members, parameters, supertypes and enum members),
//! so a lookup costs no startup parse and pulls no JSON parser into the WASM
//! payload. The rows are written inside brace-delimited macro calls, which
//! rustfmt leaves as they are, so each row stays on one line.

use std::fmt::Write as _;

use crate::builtin_normalise::{Catalog, NMemberKind, NSuper, NType, load};
use crate::progress_docs::{BuiltinIndex, TypeKind};

/// The JSON the table is compiled from, relative to the workspace root.
pub const JSON_PATH: &str = "resources/progress_builtin_classes.json";
/// The generated table, relative to the workspace root.
pub const TABLE_RS_PATH: &str = "crates/oxabl_semantic/src/builtin_classes.rs";

/// Read the JSON index at `path`.
pub fn read_index(path: &std::path::Path) -> Result<BuiltinIndex, String> {
    let text =
        std::fs::read_to_string(path).map_err(|e| format!("reading {}: {e}", path.display()))?;
    serde_json::from_str(&text).map_err(|e| format!("parsing {}: {e}", path.display()))
}

/// Flag bits of a type row; the semantic crate's `builtin.rs` reads them.
const TRUSTED: u8 = 1;
const HEADER_KNOWN: u8 = 2;
const COMPLETE: u8 = 4;
const FINAL: u8 = 8;
const ABSTRACT: u8 = 16;

/// Flag bits of a member row.
const M_STATIC: u8 = 1;
const M_ABSTRACT: u8 = 2;
const M_FINAL: u8 = 4;

fn type_expr(t: NType) -> String {
    match t {
        NType::Void => "T::Void".into(),
        NType::Primitive(p) => format!("T::Primitive(P::{p})"),
        NType::PrimitiveArray(p) => format!("T::PrimitiveArray(P::{p})"),
        NType::Class(i) => format!("T::Class({i})"),
        NType::ClassArray(i) => format!("T::ClassArray({i})"),
        NType::Shape(s) => format!("T::Shape(S::{s})"),
        NType::Unknown => "T::Unknown".into(),
    }
}

fn kind_name(k: NMemberKind) -> &'static str {
    match k {
        NMemberKind::Constructor => "Constructor",
        NMemberKind::Method => "Method",
        NMemberKind::Property => "Property",
        NMemberKind::Event => "Event",
    }
}

fn str_list(items: &[&str]) -> String {
    items
        .iter()
        .map(|s| format!("{s:?}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Generate `builtin_classes.rs` from the JSON index and the overrides under
/// the workspace `root`.
pub fn generate_from(root: &std::path::Path) -> Result<String, String> {
    let catalog = load(root)?;
    let mut index = read_index(&root.join(JSON_PATH))?;
    let overrides = std::fs::read_to_string(root.join(crate::builtin_normalise::OVERRIDES_PATH))
        .map_err(|e| format!("reading overrides: {e}"))?;
    crate::builtin_normalise::apply_overrides(&mut index, &overrides)?;
    generate(&catalog, &index)
}

/// Generate `builtin_classes.rs` from the normalised `catalog` and the
/// (overridden) `index` it was made from, for the documented supertypes.
pub fn generate(catalog: &Catalog, index: &BuiltinIndex) -> Result<String, String> {
    if catalog.types.len() > u16::MAX as usize {
        return Err("too many built-in types for a u16 row index".into());
    }
    let mut members = String::new();
    let mut params = String::new();
    let mut enums = String::new();
    let mut classes = String::new();
    let (mut n_members, mut n_params, mut n_enums) = (0usize, 0usize, 0usize);

    for (row, t) in catalog.types.iter().enumerate() {
        // Constructors first, then the other members sorted by folded name
        // (stable: overloads keep the catalog order).
        let mut ctors: Vec<_> = t
            .members
            .iter()
            .filter(|m| m.kind == NMemberKind::Constructor)
            .collect();
        let mut rest: Vec<_> = t
            .members
            .iter()
            .filter(|m| m.kind != NMemberKind::Constructor)
            .collect();
        ctors.sort_by_key(|m| m.name.to_ascii_lowercase());
        rest.sort_by_key(|m| m.name.to_ascii_lowercase());
        let ctor_start = n_members;
        for m in ctors.iter().chain(&rest) {
            let mut flags = 0u8;
            if m.is_static {
                flags |= M_STATIC;
            }
            if m.is_abstract {
                flags |= M_ABSTRACT;
            }
            if m.is_final {
                flags |= M_FINAL;
            }
            let _ = writeln!(
                members,
                "    ({:?}, {}, {flags}, {}, {row}, {n_params}, {}),",
                m.name,
                kind_name(m.kind),
                type_expr(m.ty),
                m.params.len()
            );
            for p in &m.params {
                let _ = writeln!(params, "    ({:?}, {}),", p.name, type_expr(p.ty));
                n_params += 1;
            }
            n_members += 1;
        }
        let member_start = ctor_start + ctors.len();
        let enum_start = n_enums;
        for e in &t.enum_members {
            let _ = writeln!(enums, "    {e:?},");
            n_enums += 1;
        }

        let mut flags = 0u8;
        if t.members_trusted {
            flags |= TRUSTED;
        }
        if t.header_known {
            flags |= HEADER_KNOWN;
        }
        if t.complete {
            flags |= COMPLETE;
        }
        if t.is_final {
            flags |= FINAL;
        }
        if t.is_abstract {
            flags |= ABSTRACT;
        }
        let parent = match &t.parent {
            Some(NSuper::Catalog(i)) => format!("Some({i})"),
            _ => "None".into(),
        };
        let interfaces: Vec<String> = t
            .interfaces
            .iter()
            .filter_map(|s| match s {
                NSuper::Catalog(i) => Some(i.to_string()),
                _ => None,
            })
            .collect();
        let documented_key = index
            .types
            .keys()
            .find(|k| crate::builtin_normalise::split_generic(k).0 == t.name)
            .ok_or_else(|| format!("`{}` is missing from the index", t.name))?;
        let entry = &index.types[documented_key];
        let implements: Vec<&str> = entry.implements.iter().map(String::as_str).collect();
        let type_params: Vec<&str> = t.type_params.iter().map(String::as_str).collect();
        let inherits = match &entry.inherits {
            Some(s) => format!("Some({s:?})"),
            None => "None".into(),
        };
        let kind = match t.kind {
            TypeKind::Class => "Class",
            TypeKind::Interface => "Interface",
            TypeKind::Enum => "Enum",
        };
        let _ = writeln!(
            classes,
            "    ({:?}, {:?}, {kind}, [{}], {inherits}, [{}], {:?}, {flags}, {parent}, [{}], {ctor_start}, {member_start}, {n_members}, {enum_start}, {n_enums}),",
            t.key,
            t.name,
            str_list(&type_params),
            str_list(&implements),
            t.page_url,
            interfaces.join(", "),
        );
    }

    let mut out = String::new();
    out.push_str("// Generated by oxabl_codegen - DO NOT EDIT\n\n");
    let _ = writeln!(
        out,
        "//! The documented OpenEdge {} built-in classes, interfaces and enums.",
        catalog.version
    );
    out.push_str(
        "//!
//! Compiled from `resources/progress_builtin_classes.json` and
//! `resources/progress_builtin_overrides.toml` by
//! `cargo run -p oxabl_codegen -- builtin-class-table`. The API over these
//! tables is `builtin.rs`.
//!
//! A type row is `(key, name, kind, type_params, inherits, implements, url,
//! flags, parent, interfaces, constructors, members, members_end, enum_members,
//! enum_members_end)`. A member row is `(name, kind, flags, type, class,
//! params, param_count)`. A parameter row is `(name, type)`.

use crate::PrimitiveTy as P;
use crate::builtin::{
    BuiltinClass, BuiltinMember, BuiltinParam, BuiltinShape as S, BuiltinType as T, classes,
    enum_names, members, params,
};

",
    );
    let _ = writeln!(
        out,
        "pub(crate) const VERSION: &str = {:?};\n",
        catalog.version
    );
    let _ = writeln!(
        out,
        "/// Sorted by `key` ({} entries).\npub(crate) static BUILTIN_CLASSES: &[BuiltinClass] = classes! {{\n{classes}}};\n",
        catalog.types.len()
    );
    let _ = writeln!(
        out,
        "/// The constructors, then the members by folded name, of each type ({n_members} entries).\npub(crate) static BUILTIN_MEMBERS: &[BuiltinMember] = members! {{\n{members}}};\n"
    );
    let _ = writeln!(
        out,
        "/// The parameters of each member, in order ({n_params} entries).\npub(crate) static BUILTIN_PARAMS: &[BuiltinParam] = params! {{\n{params}}};\n"
    );
    let _ = write!(
        out,
        "/// The members of each enum, as documented ({n_enums} entries).\npub(crate) static BUILTIN_ENUM_MEMBERS: &[&str] = enum_names! {{\n{enums}}};\n"
    );
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_table_matches_checked_in_catalog() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let expected = std::fs::read_to_string(root.join(TABLE_RS_PATH)).unwrap();
        assert!(
            generate_from(&root).unwrap() == expected,
            "{TABLE_RS_PATH} is stale: run `cargo run -p oxabl_codegen -- builtin-class-table`"
        );
    }
}
