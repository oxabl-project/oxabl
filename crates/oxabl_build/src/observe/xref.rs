//! What a compile's `XREF-XML` says it read: the `Source` files and the
//! include references.
//!
//! The document says nothing reliable about classes: a class used only as a
//! type gives no reference at all (measured on OpenEdge 12.8), so nothing here
//! looks for one.

use std::path::Path;

/// The parts of one `XREF-XML` document that the build uses.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Xref {
    /// The `File-name` of each `Source` element, as the compiler resolved it.
    pub sources: Vec<String>,
    /// The include file name of each `INCLUDE` reference, as written.
    pub includes: Vec<String>,
}

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
    let mut rest = xml;
    while let Some(at) = rest.find("<Reference") {
        let after = &rest[at + "<Reference".len()..];
        rest = after;
        if !after.starts_with(|c: char| c.is_whitespace() || c == '/' || c == '>') {
            continue;
        }
        let end = oxabl_effects::xref_xml::tag_end(after)
            .ok_or_else(|| "a Reference element is not closed".to_string())?;
        let tag = &after[..end];
        rest = &after[end..];
        let kind = attribute(tag, "Reference-type")
            .ok_or_else(|| "a Reference element has no Reference-type".to_string())?;
        if !kind.trim().eq_ignore_ascii_case("INCLUDE") {
            continue;
        }
        let identifier = attribute(tag, "Object-identifier").unwrap_or_default();
        let name = include_name(&identifier)
            .ok_or_else(|| format!("an INCLUDE reference names no file: {identifier:?}"))?;
        xref.includes.push(name);
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

#[cfg(test)]
mod tests {
    use super::parse;

    fn document(references: &str) -> String {
        format!(
            "<?xml version=\"1.0\"?>\n<Cross-reference>\n<Source File-num=\"1\" File-name=\"src/a.p\"/>\n{references}</Cross-reference>\n"
        )
    }

    /// The shapes OpenEdge 12.8 writes: a plain name, arguments after the
    /// name, and the whole text in quotes with arguments and many spaces.
    #[test]
    fn include_names_are_the_first_word_of_the_identifier() {
        let xml = document(concat!(
            "<Reference Reference-type=\"INCLUDE\" Object-identifier=\"ms/workspace.i\"/>\n",
            "<Reference Reference-type=\"INCLUDE\" Object-identifier=\"gl/global-input.i &amp;PASS\"/>\n",
            "<Reference Reference-type=\"INCLUDE\" Object-identifier=\"&quot;se/decrypt.i               &amp;company  = v-company&quot;\"/>\n",
            "<Reference Reference-type=\"STRING\" Object-identifier=\"&quot;a:b&quot;\"/>\n",
        ));

        let xref = parse(&xml).unwrap();

        assert_eq!(xref.sources, ["src/a.p"]);
        assert_eq!(
            xref.includes,
            ["ms/workspace.i", "gl/global-input.i", "se/decrypt.i"]
        );
    }

    /// OpenEdge 12.8 escapes `<` in an attribute value but leaves `>`, as an
    /// ADM2 `{set ...}` reference shows.
    #[test]
    fn a_greater_than_sign_inside_an_identifier_does_not_end_the_reference() {
        let xml = document(concat!(
            "<Reference Reference-type=\"INCLUDE\" Object-identifier=\"set PhysicalObjectName &quot;(IF '':U &lt;> '':U THEN '':U ELSE THIS-PROCEDURE:FILE-NAME)&quot;\">\n",
            "<File-num>31</File-num>\n</Reference>\n",
            "<Reference Reference-type=\"INCLUDE\" Object-identifier=\"ms/workspace.i\"/>\n",
        ));

        let xref = parse(&xml).unwrap();

        assert_eq!(xref.includes, ["set", "ms/workspace.i"]);
    }

    #[test]
    fn an_include_reference_without_a_name_is_an_error() {
        let xml = document("<Reference Reference-type=\"INCLUDE\" Object-identifier=\"\"/>\n");

        assert!(parse(&xml).is_err());
    }
}
