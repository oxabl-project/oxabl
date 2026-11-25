//! The hover of a built-in type and of a built-in member: the catalog of the
//! OpenEdge documentation has no source file, so the text is made from the
//! catalog row (`docs/design/builtin-classes.md`, "Hover and go to
//! definition").

use oxabl_semantic::{
    BUILTIN_CATALOG, BuiltinClass, BuiltinMember, BuiltinMemberKind, BuiltinMemberRef,
    BuiltinShape, BuiltinType, ClassKind, SymbolId, SymbolTable,
};

use crate::schema_hover::{code, inline};

/// At most this many signatures in one hover.
const MAX_SIGNATURES: usize = 10;

fn kind_word(class: &BuiltinClass) -> &'static str {
    match class.kind {
        ClassKind::Class => "class",
        ClassKind::Interface => "interface",
        ClassKind::Enum => "enum",
    }
}

fn docs_link(class: &BuiltinClass) -> String {
    format!("[Progress documentation]({})", class.docs_url())
}

/// The hover of a built-in type.
pub(super) fn type_hover(class: &BuiltinClass) -> String {
    let mut header = match class.kind {
        ClassKind::Class => format!("CLASS {}", class.documented_name()),
        ClassKind::Interface => format!("INTERFACE {}", class.documented_name()),
        ClassKind::Enum => format!("ENUM {}", class.documented_name()),
    };
    if class.kind != ClassKind::Enum {
        if let Some(parent) = class.inherits {
            header.push_str(&format!(" INHERITS {parent}"));
        }
        if !class.implements.is_empty() {
            header.push_str(&format!(" IMPLEMENTS {}", class.implements.join(", ")));
        }
        if class.is_final() {
            header.push_str(" FINAL");
        }
    }
    format!(
        "{}\n\nBuilt-in {} of OpenEdge {}. {}",
        code(&header),
        kind_word(class),
        BUILTIN_CATALOG.version(),
        docs_link(class)
    )
}

/// The type of a member or parameter as ABL writes it.
fn type_text(ty: BuiltinType) -> String {
    match ty {
        BuiltinType::Void => "VOID".to_string(),
        BuiltinType::Primitive(p) => p.abl_keyword().to_string(),
        BuiltinType::PrimitiveArray(p) => format!("{} EXTENT", p.abl_keyword()),
        BuiltinType::Class(row) => BuiltinClass::at(row).documented_name(),
        BuiltinType::ClassArray(row) => {
            format!("{} EXTENT", BuiltinClass::at(row).documented_name())
        }
        BuiltinType::Shape(shape) => match shape {
            BuiltinShape::Table => "TABLE",
            BuiltinShape::TableHandle => "TABLE-HANDLE",
            BuiltinShape::Dataset => "DATASET",
            BuiltinShape::DatasetHandle => "DATASET-HANDLE",
            BuiltinShape::Buffer => "BUFFER",
        }
        .to_string(),
        // A type parameter, a generic instance, a .NET type, or a type the
        // documentation does not give.
        BuiltinType::Unknown => "/* type not documented */".to_string(),
    }
}

fn params_text(member: &BuiltinMember) -> String {
    let params: Vec<String> = member
        .params()
        .iter()
        .map(|p| {
            if p.name.is_empty() {
                type_text(p.ty)
            } else {
                format!("{} AS {}", p.name, type_text(p.ty))
            }
        })
        .collect();
    format!("({})", params.join(", "))
}

/// The declaration of one built-in member, as ABL would write it. No access
/// and no mode are written: the catalog has neither.
fn signature(member: &BuiltinMember) -> String {
    let mut modifiers = String::new();
    if member.is_static() {
        modifiers.push_str(" STATIC");
    }
    if member.is_abstract() {
        modifiers.push_str(" ABSTRACT");
    }
    match member.kind() {
        BuiltinMemberKind::Method => format!(
            "METHOD{modifiers} {} {} {}",
            type_text(member.ty()),
            member.name(),
            params_text(member)
        ),
        BuiltinMemberKind::Property => format!(
            "DEFINE{modifiers} PROPERTY {} AS {}",
            member.name(),
            type_text(member.ty())
        ),
        BuiltinMemberKind::Event => format!(
            "DEFINE{modifiers} EVENT {} SIGNATURE VOID {}",
            member.name(),
            params_text(member)
        ),
        BuiltinMemberKind::Constructor => format!(
            "CONSTRUCTOR{modifiers} {} {}",
            member.name(),
            params_text(member)
        ),
    }
}

fn declared_line(class: &BuiltinClass, has_params: bool) -> String {
    let modes = if has_params {
        " Parameter modes are not known."
    } else {
        ""
    };
    format!(
        "Declared in built-in {} {} (OpenEdge {}).{modes} {}",
        kind_word(class),
        inline(class.name),
        BUILTIN_CATALOG.version(),
        docs_link(class)
    )
}

/// The hover of the built-in member `sym`: each candidate of its overload
/// set, as the pick reads them, nearest level first.
pub(super) fn member_hover(
    symbols: &SymbolTable,
    sym: SymbolId,
    member: BuiltinMemberRef,
) -> String {
    let member = match member {
        BuiltinMemberRef::EnumMember(class, name) => {
            return format!(
                "{}\n\nMember of built-in enum {} (OpenEdge {}). {}",
                code(&format!("{}:{name}", class.name)),
                inline(class.name),
                BUILTIN_CATALOG.version(),
                docs_link(class)
            );
        }
        BuiltinMemberRef::Member(member) => member,
    };
    let candidates: Vec<&'static BuiltinMember> = symbols
        .method_overloads(sym)
        .unwrap_or(std::slice::from_ref(&sym))
        .iter()
        .filter_map(|&c| match symbols.builtin_member(c) {
            Some(BuiltinMemberRef::Member(m)) => Some(m),
            _ => None,
        })
        .collect();
    let candidates = if candidates.is_empty() {
        vec![member]
    } else {
        candidates
    };
    let mut parts: Vec<String> = Vec::new();
    let mut group: Option<&BuiltinClass> = None;
    let mut group_params = false;
    for (shown, m) in candidates.iter().enumerate() {
        if shown == MAX_SIGNATURES {
            parts.push(format!("{} more overloads.", candidates.len() - shown));
            break;
        }
        let class = m.class();
        if let Some(previous) = group
            && !std::ptr::eq(previous, class)
        {
            parts.push(declared_line(previous, group_params));
            group_params = false;
        }
        group = Some(class);
        group_params |= !m.params().is_empty();
        parts.push(code(&signature(m)));
    }
    if let Some(class) = group {
        parts.push(declared_line(class, group_params));
    }
    parts.join("\n\n")
}
