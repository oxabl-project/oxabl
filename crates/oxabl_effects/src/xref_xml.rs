//! The compiler's `COMPILE ... XREF-XML` output: the files it opened.
//!
//! [`source_files`] returns the `File-name` of every `Source` element, in
//! order, as the compiler resolved each name. Nothing else of the document is
//! read.

/// The error of a document that is not a cross-reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotXref(pub String);

impl std::fmt::Display for NotXref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for NotXref {}

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

/// The value of attribute `name` in the start tag `tag` (the text between `<`
/// and `>`).
fn attribute(tag: &str, name: &str) -> Option<String> {
    let mut rest = tag;
    while let Some(at) = rest.find(name) {
        let before = rest[..at].chars().next_back();
        let after = &rest[at + name.len()..];
        rest = after;
        if before.is_some_and(|c| !c.is_whitespace()) {
            continue;
        }
        let value = after.trim_start().strip_prefix('=')?.trim_start();
        let quote = value.chars().next().filter(|c| *c == '"' || *c == '\'')?;
        let body = &value[1..];
        let end = body.find(quote)?;
        return Some(unescape(&body[..end]));
    }
    None
}

/// The `File-name` of each `Source` element of an `XREF-XML` document. A
/// document with no `Cross-reference` root, or a `Source` element with no
/// `File-name`, is an error: nothing is guessed about what the compiler read.
pub fn source_files(xml: &str) -> Result<Vec<String>, NotXref> {
    if !xml.contains("<Cross-reference") {
        return Err(NotXref("no Cross-reference element".to_string()));
    }
    let mut files = Vec::new();
    let mut rest = xml;
    while let Some(at) = rest.find("<Source") {
        let tag_start = &rest[at + "<Source".len()..];
        rest = tag_start;
        if !tag_start.starts_with(|c: char| c.is_whitespace() || c == '/' || c == '>') {
            continue;
        }
        let Some(end) = tag_start.find('>') else {
            return Err(NotXref("a Source element is not closed".to_string()));
        };
        let tag = &tag_start[..end];
        match attribute(tag, "File-name") {
            Some(name) if !name.is_empty() => files.push(name),
            _ => return Err(NotXref("a Source element has no File-name".to_string())),
        }
        rest = &tag_start[end..];
    }
    Ok(files)
}
