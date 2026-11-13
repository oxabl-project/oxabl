//! What a compile's cross-reference says it read: the files that hold a
//! reference and the include references. The private output is the text
//! `XREF`; a requested `XREF-XML` or text `XREF` serves as well.
//!
//! Neither form says anything reliable about classes: a class used only as a
//! type gives no reference at all (measured on OpenEdge 12.8), so nothing here
//! looks for one.

use std::path::Path;

pub use oxabl_workspace::XrefFormat as Format;

/// The parts of one cross-reference that the build uses.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Xref {
    /// The file of each `Source` element (`File-name`) of an `XREF-XML`, or
    /// each file in the second column of a text `XREF` row, as the compiler
    /// resolved it.
    pub sources: Vec<String>,
    /// The include file name of each `INCLUDE` reference, as written.
    pub includes: Vec<String>,
}

/// Reads the cross-reference file at `path`, in `format`.
pub fn read(path: &Path, format: Format) -> Result<Xref, String> {
    let name = match format {
        Format::Xml => "XREF-XML",
        Format::Text => "XREF",
    };
    let text = std::fs::read_to_string(path)
        .map_err(|error| format!("cannot read the {name} of the compile: {error}"))?;
    let parsed = match format {
        Format::Xml => parse(&text),
        Format::Text => parse_text(&text),
    };
    parsed.map_err(|error| format!("the {name} of the compile is unusable: {error}"))
}

/// Parses one `XREF-XML` document. A `Reference` element with no `Reference-type`, or an
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

/// Parses a text `XREF`: one row for each reference, `<including file>
/// <file of the reference> <line> <TYPE> <details>`. A name with a space is
/// in double quotes and a quote inside a field is doubled. The line is a
/// number or `IMPLICIT` (the call of a constructor that the source does not
/// write). A `STRING` row, whose value can hold a line break, can span lines
/// and holds the length of the value after it. Nothing is skipped: a row of
/// another shape, a field that is not closed, an empty file or a file that
/// does not end with a line break is an error. The compiler writes no
/// `COMPILE` row for encrypted source, so the first row is not checked.
pub fn parse_text(text: &str) -> Result<Xref, String> {
    let mut xref = Xref::default();
    let mut rest = text;
    let mut empty = true;
    while !rest.is_empty() {
        let (_, after) = field(rest)?;
        let (file, after) = field(after)?;
        let (line, after) = field(after)?;
        if line != "IMPLICIT" && !line.bytes().all(|byte| byte.is_ascii_digit()) {
            return Err(format!("a row has the line {line:?}, which is not a number"));
        }
        let (kind, after) = field(after)?;
        empty = false;
        let end = row_end(after)?;
        let details = &after[..end];
        if details.contains('\n') && kind != "STRING" {
            return Err(format!("a {kind} row spans lines"));
        }
        match kind.as_str() {
            "INCLUDE" => {
                let (identifier, _) = field(details)?;
                let name = include_name(&identifier)
                    .ok_or_else(|| format!("an INCLUDE row names no file: {identifier:?}"))?;
                xref.includes.push(name);
            }
            "STRING" => {
                let (_, after) = field(details)?;
                let (length, _) = field(after)?;
                if !length.bytes().all(|byte| byte.is_ascii_digit()) {
                    return Err(format!("a STRING row has the length {length:?}"));
                }
            }
            _ => {}
        }
        if !xref.sources.contains(&file) {
            xref.sources.push(file);
        }
        rest = &after[end + 1..];
    }
    if empty {
        return Err("the file is empty".to_string());
    }
    Ok(xref)
}

/// The next field of a row and what follows it: a word, or a quoted string
/// with `""` for a quote. A word ends at a space or a line break, and the
/// space before a field is skipped. An empty field is an error.
fn field(text: &str) -> Result<(String, &str), String> {
    let text = text.trim_start_matches(' ');
    let mut value = String::new();
    let end = if text.starts_with('"') {
        let mut chars = text.char_indices().skip(1);
        loop {
            match chars.next() {
                None => return Err("a quoted field is not closed".to_string()),
                Some((at, '"')) if text[at + 1..].starts_with('"') => {
                    value.push('"');
                    chars.next();
                }
                Some((at, '"')) => break at + 1,
                Some((_, c)) => value.push(c),
            }
        }
    } else {
        let end = text.find([' ', '\n']).unwrap_or(text.len());
        value.push_str(&text[..end]);
        end
    };
    if value.is_empty() {
        return Err("a row has an empty field".to_string());
    }
    Ok((value, &text[end..]))
}

/// The offset of the line break that ends the row whose details start at the
/// beginning of `text`: the first one outside quotes.
fn row_end(text: &str) -> Result<usize, String> {
    let mut quoted = false;
    for (at, c) in text.char_indices() {
        match c {
            '"' => quoted = !quoted,
            '\n' if !quoted => return Ok(at),
            _ => {}
        }
    }
    Err("the last row has no line break, or a quote is not closed".to_string())
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
