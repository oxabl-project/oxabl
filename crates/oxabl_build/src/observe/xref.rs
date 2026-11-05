//! What a compile's `XREF-XML` says it read: the `Source` files, the include
//! references and any use of a class.

use std::path::Path;

/// The parts of one `XREF-XML` document that the build uses.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Xref {
    /// The `File-name` of each `Source` element, as the compiler resolved it.
    pub sources: Vec<String>,
    /// The include file name of each `INCLUDE` reference, as written.
    pub includes: Vec<String>,
    /// The first reference that shows a class, as `<type> <identifier>`.
    pub class: Option<String>,
}

/// Reference types that name a class, an interface, an enum or a member of
/// one. The compiler reads such a type from a file that no `Source` names.
const CLASS_TYPES: [&str; 15] = [
    "CLASS",
    "INTERFACE",
    "ENUM",
    "INHERITS",
    "IMPLEMENTS",
    "NEW",
    "INVOKE",
    "REFERENCE",
    "CONSTRUCTOR",
    "DESTRUCTOR",
    "METHOD",
    "PROPERTY",
    "DATA-MEMBER",
    "EVENT",
    "DELEGATE",
];

/// Element names that describe a class or an interface.
const CLASS_ELEMENTS: [&str; 2] = ["<Class-ref", "<Interface-ref"];

/// Reads the `XREF-XML` file at `path`.
pub fn read(path: &Path) -> Result<Xref, String> {
    let text = std::fs::read_to_string(path)
        .map_err(|error| format!("cannot read the XREF-XML of the compile: {error}"))?;
    parse(&text).map_err(|error| format!("the XREF-XML of the compile is unusable: {error}"))
}

/// Parses one document. A `Reference` element with no `Reference-type`, or an
/// `INCLUDE` with no name, is an error: nothing is guessed.
pub fn parse(xml: &str) -> Result<Xref, String> {
    let sources = oxabl_effects::xref_xml::source_files(xml).map_err(|error| error.to_string())?;
    let mut xref = Xref {
        sources,
        ..Xref::default()
    };
    for element in CLASS_ELEMENTS {
        if xml.contains(element) && xref.class.is_none() {
            xref.class = Some(element[1..].to_string());
        }
    }
    let mut rest = xml;
    while let Some(at) = rest.find("<Reference") {
        let after = &rest[at + "<Reference".len()..];
        rest = after;
        if !after.starts_with(|c: char| c.is_whitespace() || c == '/' || c == '>') {
            continue;
        }
        let end = after
            .find('>')
            .ok_or_else(|| "a Reference element is not closed".to_string())?;
        let tag = &after[..end];
        rest = &after[end..];
        let kind = attribute(tag, "Reference-type")
            .ok_or_else(|| "a Reference element has no Reference-type".to_string())?;
        let identifier = attribute(tag, "Object-identifier").unwrap_or_default();
        let kind = kind.trim().to_ascii_uppercase();
        if kind == "INCLUDE" {
            let name = include_name(&identifier)
                .ok_or_else(|| format!("an INCLUDE reference names no file: {identifier:?}"))?;
            xref.includes.push(name);
        } else if (CLASS_TYPES.contains(&kind.as_str())
            // A member of a class reads `<class>:<member>`; a string can hold `:`.
            || (kind != "STRING" && identifier.contains(':')))
            && xref.class.is_none()
        {
            xref.class = Some(format!("{kind} {identifier}"));
        }
    }
    Ok(xref)
}

/// The file name of an include reference as the compiler writes it: the first
/// word, also inside quotes (`"inc/x.i &a = 1"` gives `inc/x.i`).
fn include_name(identifier: &str) -> Option<String> {
    let text = identifier.trim();
    let text = match text.strip_prefix('"') {
        Some(quoted) => quoted.split('"').next().unwrap_or(""),
        None => text,
    };
    let name = text.split_whitespace().next()?;
    Some(name.to_string())
}

fn unescape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(at) = rest.find('&') {
        out.push_str(&rest[..at]);
        rest = &rest[at..];
        let Some(end) = rest.find(';') else { break };
        let entity = &rest[1..end];
        let decoded = match entity {
            "amp" => Some('&'),
            "lt" => Some('<'),
            "gt" => Some('>'),
            "quot" => Some('"'),
            "apos" => Some('\''),
            _ => entity
                .strip_prefix("#x")
                .and_then(|hex| u32::from_str_radix(hex, 16).ok())
                .or_else(|| entity.strip_prefix('#').and_then(|dec| dec.parse().ok()))
                .and_then(char::from_u32),
        };
        match decoded {
            Some(c) => out.push(c),
            None => out.push_str(&rest[..=end]),
        }
        rest = &rest[end + 1..];
    }
    out.push_str(rest);
    out
}

/// The value of attribute `name` in the start tag `tag`.
fn attribute(tag: &str, name: &str) -> Option<String> {
    let mut rest = tag;
    while let Some(at) = rest.find(name) {
        let before = rest[..at].chars().next_back();
        let after = &rest[at + name.len()..];
        rest = after;
        if before.is_some_and(|c| !c.is_whitespace()) {
            continue;
        }
        let Some(value) = after.trim_start().strip_prefix('=') else {
            continue;
        };
        let value = value.trim_start();
        let quote = value.chars().next().filter(|c| *c == '"' || *c == '\'')?;
        let body = &value[1..];
        let end = body.find(quote)?;
        return Some(unescape(&body[..end]));
    }
    None
}
