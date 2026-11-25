//! The catalog of documented OpenEdge built-in types and their members.
//!
//! The data is the generated `builtin_classes.rs` (see
//! `docs/design/builtin-classes.md`, "The data form"). This module is the API
//! over it: a type by name, its members by name, its supertypes, and the flags
//! that say how far the catalog can be trusted.

use std::cmp::Ordering;

use crate::builtin_classes::{
    BUILTIN_CLASSES, BUILTIN_ENUM_MEMBERS, BUILTIN_MEMBERS, BUILTIN_PARAMS, VERSION,
};
use crate::{ClassKind, PrimitiveTy};

/// The type of a built-in member or parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    Void,
    Primitive(PrimitiveTy),
    /// An `EXTENT` of a primitive, with no size.
    PrimitiveArray(PrimitiveTy),
    /// A catalog type, by row.
    Class(u16),
    /// An `EXTENT` of a catalog type, with no size.
    ClassArray(u16),
    /// A parameter shape.
    Shape(BuiltinShape),
    /// A type parameter, a generic instance, a .NET type or any type.
    Unknown,
}

/// A parameter shape of a built-in parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinShape {
    Table,
    TableHandle,
    Dataset,
    DatasetHandle,
    Buffer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMemberKind {
    Constructor,
    Method,
    Property,
    Event,
}

const TRUSTED: u8 = 1;
const HEADER_KNOWN: u8 = 2;
const COMPLETE: u8 = 4;
const FINAL: u8 = 8;
const ABSTRACT: u8 = 16;

const M_STATIC: u8 = 1;
const M_ABSTRACT: u8 = 2;
const M_FINAL: u8 = 4;

/// One documented built-in type.
#[derive(Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct BuiltinClass {
    /// The lookup key: the name folded to lowercase.
    pub(crate) key: &'static str,
    /// The qualified name as documented, without generic parameters.
    pub name: &'static str,
    pub kind: ClassKind,
    /// The generic type parameters, `["K", "V"]` for `HashMap<K,V>`.
    pub type_params: &'static [&'static str],
    /// The parent class or super-interface, as documented (after the
    /// overrides). A .NET parent (`System.Object`) names no type in the table.
    pub inherits: Option<&'static str>,
    /// The implemented interfaces, as documented.
    pub implements: &'static [&'static str],
    pub(crate) url: &'static str,
    pub(crate) flags: u8,
    pub(crate) parent: Option<u16>,
    pub(crate) interfaces: &'static [u16],
    pub(crate) ctors: u32,
    pub(crate) members: u32,
    pub(crate) members_end: u32,
    pub(crate) enum_members: u32,
    pub(crate) enum_members_end: u32,
}

/// One documented member of a built-in type.
#[derive(Debug, PartialEq, Eq)]
pub struct BuiltinMember {
    pub(crate) name: &'static str,
    pub(crate) kind: BuiltinMemberKind,
    pub(crate) flags: u8,
    pub(crate) ty: BuiltinType,
    pub(crate) class: u16,
    pub(crate) params: u32,
    pub(crate) param_count: u16,
}

/// One documented parameter. The catalog has no parameter modes.
#[derive(Debug, PartialEq, Eq)]
pub struct BuiltinParam {
    /// Empty when the documentation gives no name.
    pub name: &'static str,
    pub ty: BuiltinType,
}

/// A catalog of built-in types: one OpenEdge version.
#[derive(Debug)]
pub struct BuiltinCatalog {
    version: &'static str,
}

/// The catalog that the scrape gives (OpenEdge 12.8).
pub static BUILTIN_CATALOG: BuiltinCatalog = BuiltinCatalog { version: VERSION };

impl BuiltinCatalog {
    /// The OpenEdge version the catalog documents (`12.8`).
    pub fn version(&self) -> &'static str {
        self.version
    }

    /// The type named `name`, in any case and with or without generic
    /// arguments.
    pub fn class(&self, name: &str) -> Option<&'static BuiltinClass> {
        builtin_class(name)
    }

    /// Every type, sorted by folded name.
    pub fn classes(&self) -> &'static [BuiltinClass] {
        BUILTIN_CLASSES
    }

    /// The types whose qualified name starts with `package` (`"Progress.Lang."`),
    /// in any case.
    pub fn classes_in(&self, package: &str) -> &'static [BuiltinClass] {
        let prefix = package.to_ascii_lowercase();
        let start = BUILTIN_CLASSES.partition_point(|c| c.key < prefix.as_str());
        let len = BUILTIN_CLASSES[start..]
            .iter()
            .take_while(|c| c.key.starts_with(prefix.as_str()))
            .count();
        &BUILTIN_CLASSES[start..start + len]
    }
}

/// `Progress.Lang.Object`, the root of every class and interface.
pub(crate) fn object_class() -> &'static BuiltinClass {
    static OBJECT: std::sync::OnceLock<&'static BuiltinClass> = std::sync::OnceLock::new();
    OBJECT.get_or_init(|| builtin_class("Progress.Lang.Object").expect("in the catalog"))
}

/// `Progress.Lang.Enum`, the supertype of every enum.
pub(crate) fn enum_class() -> &'static BuiltinClass {
    static ENUM: std::sync::OnceLock<&'static BuiltinClass> = std::sync::OnceLock::new();
    ENUM.get_or_init(|| builtin_class("Progress.Lang.Enum").expect("in the catalog"))
}

/// The documented built-in type named `name`, which may be written in any case
/// and with generic arguments.
pub fn builtin_class(name: &str) -> Option<&'static BuiltinClass> {
    builtin_class_in("", name)
}

/// [`builtin_class`] for the name `prefix` followed by `name`, without
/// building the joined string.
pub(crate) fn builtin_class_in(prefix: &str, name: &str) -> Option<&'static BuiltinClass> {
    let name = name.split_once('<').map_or(name, |(base, _)| base);
    BUILTIN_CLASSES
        .binary_search_by(|class| compare_folded(class.key, prefix, name))
        .ok()
        .map(|i| &BUILTIN_CLASSES[i])
}

fn compare_folded(key: &str, prefix: &str, name: &str) -> Ordering {
    let key = key.as_bytes();
    let (p, n) = (prefix.as_bytes(), name.as_bytes());
    let wanted_len = p.len() + n.len();
    for (i, k) in key.iter().enumerate() {
        let w = match p.get(i).or_else(|| n.get(i - p.len())) {
            Some(w) => w.to_ascii_lowercase(),
            None => return Ordering::Greater,
        };
        if *k != w {
            return k.cmp(&w);
        }
    }
    key.len().cmp(&wanted_len)
}

fn compare_name_folded(name: &str, wanted: &str) -> Ordering {
    let (a, b) = (name.as_bytes(), wanted.as_bytes());
    for (x, y) in a.iter().zip(b) {
        let (x, y) = (x.to_ascii_lowercase(), y.to_ascii_lowercase());
        if x != y {
            return x.cmp(&y);
        }
    }
    a.len().cmp(&b.len())
}

impl BuiltinClass {
    /// The row of this type in the catalog.
    pub fn row(&self) -> u16 {
        // Every `BuiltinClass` is an element of `BUILTIN_CLASSES`: the type
        // has private fields and no constructor.
        let offset = (self as *const Self as usize) - (BUILTIN_CLASSES.as_ptr() as usize);
        let row = offset / std::mem::size_of::<Self>();
        debug_assert!(std::ptr::eq(&BUILTIN_CLASSES[row], self));
        row as u16
    }

    /// The type at `row`.
    pub fn at(row: u16) -> &'static BuiltinClass {
        &BUILTIN_CLASSES[row as usize]
    }

    /// The name as documented, with its generic parameters
    /// (`Progress.Collections.List<T>`).
    pub fn documented_name(&self) -> String {
        if self.type_params.is_empty() {
            self.name.to_string()
        } else {
            format!("{}<{}>", self.name, self.type_params.join(","))
        }
    }

    /// The URL of the documentation page.
    pub fn docs_url(&self) -> &'static str {
        self.url
    }

    /// No member of the type was dropped as damaged.
    pub fn members_trusted(&self) -> bool {
        self.flags & TRUSTED != 0
    }

    /// Every supertype is a catalog type.
    pub fn header_known(&self) -> bool {
        self.flags & HEADER_KNOWN != 0
    }

    /// The catalog has every member of the type and of each supertype
    /// (`docs/design/builtin-classes.md`, "Trust and completeness").
    pub fn is_complete(&self) -> bool {
        self.flags & COMPLETE != 0
    }

    pub fn is_final(&self) -> bool {
        self.flags & FINAL != 0
    }

    pub fn is_abstract(&self) -> bool {
        self.flags & ABSTRACT != 0
    }

    pub fn is_generic(&self) -> bool {
        !self.type_params.is_empty()
    }

    /// The constructors, in catalog order.
    pub fn constructors(&self) -> &'static [BuiltinMember] {
        &BUILTIN_MEMBERS[self.ctors as usize..self.members as usize]
    }

    /// The methods, properties and events, sorted by folded name.
    pub fn members(&self) -> &'static [BuiltinMember] {
        &BUILTIN_MEMBERS[self.members as usize..self.members_end as usize]
    }

    /// The members named `name` (any case), overloads in catalog order.
    pub fn members_named(&self, name: &str) -> &'static [BuiltinMember] {
        let all = self.members();
        let start = all.partition_point(|m| compare_name_folded(m.name, name).is_lt());
        let end = all.partition_point(|m| compare_name_folded(m.name, name).is_le());
        &all[start..end]
    }

    /// The members of an enum, as documented. Empty for a type that is not an
    /// enum, and for an enum whose members the catalog does not list.
    pub fn enum_members(&self) -> &'static [&'static str] {
        &BUILTIN_ENUM_MEMBERS[self.enum_members as usize..self.enum_members_end as usize]
    }

    /// The enum member named `name` (any case), as documented.
    pub fn enum_member(&self, name: &str) -> Option<&'static str> {
        self.enum_members()
            .iter()
            .copied()
            .find(|m| m.eq_ignore_ascii_case(name))
    }

    /// The parent, when it is a catalog type.
    pub fn parent(&self) -> Option<&'static BuiltinClass> {
        self.parent.map(BuiltinClass::at)
    }

    /// The interfaces that are catalog types.
    pub fn interfaces(&self) -> impl Iterator<Item = &'static BuiltinClass> + 'static {
        self.interfaces.iter().map(|r| BuiltinClass::at(*r))
    }

    /// The parent, then the interfaces, that are catalog types.
    pub fn supertypes(&self) -> impl Iterator<Item = &'static BuiltinClass> + 'static {
        self.parent().into_iter().chain(self.interfaces())
    }

    /// Whether the AVM supplies the type itself (`Progress.*`): the compiler
    /// binds it whatever the workspace holds.
    pub fn is_avm_native(&self) -> bool {
        self.name.starts_with("Progress.")
    }

    /// Whether the type is `Progress.Lang.Object`.
    pub fn is_object(&self) -> bool {
        self.key == "progress.lang.object"
    }
}

impl BuiltinMember {
    /// The name as documented.
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn kind(&self) -> BuiltinMemberKind {
        self.kind
    }

    pub fn is_static(&self) -> bool {
        self.flags & M_STATIC != 0
    }

    pub fn is_abstract(&self) -> bool {
        self.flags & M_ABSTRACT != 0
    }

    pub fn is_final(&self) -> bool {
        self.flags & M_FINAL != 0
    }

    /// The return type of a method, the type of a property; `Void` for a
    /// constructor and an event.
    pub fn ty(&self) -> BuiltinType {
        self.ty
    }

    /// The parameters, in order.
    pub fn params(&self) -> &'static [BuiltinParam] {
        let start = self.params as usize;
        &BUILTIN_PARAMS[start..start + self.param_count as usize]
    }

    /// The type that declares the member.
    pub fn class(&self) -> &'static BuiltinClass {
        BuiltinClass::at(self.class)
    }
}

/// The macros the generated tables are written with: each row on one line, in
/// a brace-delimited call that rustfmt leaves as it is.
macro_rules! classes {
    ($(($key:literal, $name:literal, $kind:ident, [$($tp:literal),*], $inherits:expr,
        [$($imp:literal),*], $url:literal, $flags:literal, $parent:expr, [$($iface:literal),*],
        $ctors:literal, $members:literal, $members_end:literal, $enums:literal,
        $enums_end:literal)),* $(,)?) => {
        &[$($crate::builtin::BuiltinClass {
            key: $key,
            name: $name,
            kind: $crate::ClassKind::$kind,
            type_params: &[$($tp),*],
            inherits: $inherits,
            implements: &[$($imp),*],
            url: $url,
            flags: $flags,
            parent: $parent,
            interfaces: &[$($iface),*],
            ctors: $ctors,
            members: $members,
            members_end: $members_end,
            enum_members: $enums,
            enum_members_end: $enums_end,
        }),*]
    };
}

macro_rules! members {
    ($(($name:literal, $kind:ident, $flags:literal, $ty:expr, $class:literal, $params:literal,
        $count:literal)),* $(,)?) => {
        &[$($crate::builtin::BuiltinMember {
            name: $name,
            kind: $crate::builtin::BuiltinMemberKind::$kind,
            flags: $flags,
            ty: $ty,
            class: $class,
            params: $params,
            param_count: $count,
        }),*]
    };
}

macro_rules! params {
    ($(($name:literal, $ty:expr)),* $(,)?) => {
        &[$($crate::builtin::BuiltinParam { name: $name, ty: $ty }),*]
    };
}

macro_rules! enum_names {
    ($($name:literal),* $(,)?) => {
        &[$($name),*]
    };
}

pub(crate) use {classes, enum_names, members, params};

#[cfg(test)]
mod tests {
    use super::*;

    fn class(name: &str) -> &'static BuiltinClass {
        builtin_class(name).unwrap_or_else(|| panic!("no built-in type {name}"))
    }

    #[test]
    fn a_member_lookup_folds_case() {
        let pro = class("progress.lang.proerror");
        let found = pro.members_named("getmessage");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].name(), "GetMessage");
        assert_eq!(found[0].kind(), BuiltinMemberKind::Method);
        assert_eq!(
            found[0].ty(),
            BuiltinType::Primitive(PrimitiveTy::Character)
        );
        assert_eq!(found[0].params().len(), 1);
        assert_eq!(found[0].params()[0].name, "MessageIndex");
        assert_eq!(found[0].class().name, "Progress.Lang.ProError");
        assert!(pro.members_named("nosuch").is_empty());
    }

    #[test]
    fn json_object_has_seventeen_add_overloads() {
        let json = class("Progress.Json.ObjectModel.JsonObject");
        let add = json.members_named("Add");
        assert_eq!(add.len(), 17);
        assert!(add.iter().all(|m| m.params().len() == 2));
        assert!(json.is_complete());
    }

    #[test]
    fn a_static_member_is_static() {
        let s = class("OpenEdge.Core.String");
        let quoted = s.members_named("IsQuoted");
        assert!(!quoted.is_empty());
        assert!(quoted.iter().all(BuiltinMember::is_static));
        let number = class("Progress.Json.ObjectModel.JsonDataType").members_named("number");
        assert_eq!(number[0].name(), "Number");
        assert!(number[0].is_static());
    }

    #[test]
    fn an_enum_lists_its_members() {
        let e = class("Progress.ApplicationServer.AdapterTypes");
        assert_eq!(e.kind, ClassKind::Enum);
        assert_eq!(e.enum_member("soap"), Some("SOAP"));
        assert_eq!(e.enum_members().len(), 5);
        assert!(e.is_complete());
        assert!(
            class("OpenEdge.Net.UriSchemeEnum")
                .enum_members()
                .is_empty()
        );
    }

    #[test]
    fn supertypes_are_rows() {
        let app = class("Progress.Lang.AppError");
        assert_eq!(app.parent().unwrap().name, "Progress.Lang.ProError");
        let pro = class("Progress.Lang.ProError");
        assert_eq!(pro.parent().unwrap().name, "Progress.Lang.Object");
        let ifaces: Vec<_> = pro.interfaces().map(|c| c.name).collect();
        assert_eq!(ifaces, ["Progress.Lang.Error"]);
        assert!(class("Progress.IO.ByteOrder").parent().is_none());
        assert!(!class("Progress.IO.ByteOrder").header_known());
        assert_eq!(BuiltinClass::at(app.row()), app);
    }

    #[test]
    fn constructors_are_their_own_range() {
        let app = class("Progress.Lang.AppError");
        assert_eq!(app.constructors().len(), 3);
        assert!(
            app.constructors()
                .iter()
                .all(|m| m.kind() == BuiltinMemberKind::Constructor)
        );
        assert!(
            app.members()
                .iter()
                .all(|m| m.kind() != BuiltinMemberKind::Constructor)
        );
    }

    #[test]
    fn the_url_names_the_bundle() {
        assert_eq!(
            class("Progress.Lang.AppError").docs_url(),
            "https://docs-be.progress.com/bundle/abl-reference/page/Progress.Lang.AppError-class.html"
        );
        assert_eq!(
            class("OpenEdge.Core.String").docs_url(),
            "https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page/OpenEdge.Core.String.html"
        );
    }

    #[test]
    fn a_generic_type_has_its_documented_name() {
        let list = class("Progress.Collections.List<OpenEdge.Core.String>");
        assert_eq!(list.documented_name(), "Progress.Collections.List<T>");
        assert!(list.is_generic());
        assert!(!list.is_complete());
    }

    #[test]
    fn a_package_lists_its_types() {
        let lang = BUILTIN_CATALOG.classes_in("progress.lang.");
        assert!(lang.iter().any(|c| c.name == "Progress.Lang.AppError"));
        assert!(lang.iter().all(|c| c.key.starts_with("progress.lang.")));
        assert!(BUILTIN_CATALOG.classes_in("nosuch.").is_empty());
        assert_eq!(BUILTIN_CATALOG.version(), "12.8");
    }
}
