//! Counts the `#[allow]` and `#[expect]` attributes that name a tracked lint,
//! or a lint group that holds one. Such an attribute hides a finding from
//! clippy, so the ratchet counts it under its own key, `allow:<lint>`.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The key prefix of an attribute count in `tigerstyle-ratchet.txt`.
pub(super) const KEY_PREFIX: &str = "allow:";

/// Lint groups that hold at least one tracked lint: to allow one hides it.
const GROUPS: [&str; 7] = [
    "warnings",
    "unused",
    "clippy::all",
    "clippy::style",
    "clippy::complexity",
    "clippy::pedantic",
    "clippy::restriction",
];

/// Add the attribute counts of one package to `counts`, keyed by
/// `allow:<lint>`. Reads `build.rs` and every `.rs` file below `src/`: the
/// code that `clippy --lib --bins` checks.
pub(super) fn count_package(
    dir: &Path,
    tracked: &[&str],
    counts: &mut BTreeMap<String, u64>,
) -> Result<(), String> {
    let mut files = rust_files(&dir.join("src"))?;
    let build = dir.join("build.rs");
    if build.is_file() {
        files.push(build);
    }
    for file in files {
        let text = std::fs::read_to_string(&file)
            .map_err(|err| format!("cannot read {}: {err}\n", file.display()))?;
        for lint in allowed_lints(&text) {
            if tracked.contains(&lint.as_str()) || GROUPS.contains(&lint.as_str()) {
                *counts.entry(format!("{KEY_PREFIX}{lint}")).or_insert(0) += 1;
            }
        }
    }
    Ok(())
}

fn rust_files(root: &Path) -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();
    let mut pending = vec![root.to_path_buf()];
    while let Some(dir) = pending.pop() {
        let entries = std::fs::read_dir(&dir)
            .map_err(|err| format!("cannot list {}: {err}\n", dir.display()))?;
        for entry in entries {
            let path = entry
                .map_err(|err| format!("cannot list {}: {err}\n", dir.display()))?
                .path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

/// The lint names in every `allow(...)` and `expect(...)` of every attribute
/// of `source`, also inside `cfg_attr`. Comments and string literals are
/// skipped, so code that writes an attribute into generated text counts none.
pub(super) fn allowed_lints(source: &str) -> Vec<String> {
    let bytes = source.as_bytes();
    let mut lints = Vec::new();
    let mut at = 0;
    while at < bytes.len() {
        at = match bytes[at] {
            b'/' if bytes.get(at + 1) == Some(&b'/') => line_end(bytes, at),
            b'/' if bytes.get(at + 1) == Some(&b'*') => block_comment_end(bytes, at),
            b'"' => string_end(bytes, at),
            b'r' if raw_string_starts(bytes, at) => raw_string_end(bytes, at),
            b'\'' => char_end(source, at),
            b'#' => match attribute_body(bytes, at) {
                Some((open, close)) => {
                    lints.extend(lints_in_attribute(&source[open..close]));
                    close + 1
                }
                None => at + 1,
            },
            _ => at + 1,
        };
    }
    lints
}

fn line_end(bytes: &[u8], at: usize) -> usize {
    bytes[at..]
        .iter()
        .position(|b| *b == b'\n')
        .map_or(bytes.len(), |n| at + n + 1)
}

fn block_comment_end(bytes: &[u8], at: usize) -> usize {
    let mut depth = 0_u32;
    let mut i = at;
    while i + 1 < bytes.len() {
        match (bytes[i], bytes[i + 1]) {
            (b'/', b'*') => {
                depth += 1;
                i += 2;
            }
            (b'*', b'/') => {
                depth -= 1;
                i += 2;
                if depth == 0 {
                    return i;
                }
            }
            _ => i += 1,
        }
    }
    bytes.len()
}

/// The index after the closing quote of the string that opens at `at`.
fn string_end(bytes: &[u8], at: usize) -> usize {
    let mut i = at + 1;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => i += 2,
            b'"' => return i + 1,
            _ => i += 1,
        }
    }
    bytes.len()
}

fn is_ident(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

/// `r"`, `r#"`, `br"`, ... start a raw string; `for"` or `bar#` do not.
fn raw_string_starts(bytes: &[u8], at: usize) -> bool {
    let free = |i: usize| i == 0 || !is_ident(bytes[i - 1]);
    let starts = free(at) || (bytes[at - 1] == b'b' && free(at - 1));
    let hashes = bytes[at + 1..].iter().take_while(|b| **b == b'#').count();
    starts && bytes.get(at + 1 + hashes) == Some(&b'"')
}

fn raw_string_end(bytes: &[u8], at: usize) -> usize {
    let hashes = bytes[at + 1..].iter().take_while(|b| **b == b'#').count();
    let mut closing = vec![b'"'];
    closing.extend(std::iter::repeat_n(b'#', hashes));
    let body = at + 2 + hashes;
    bytes[body..]
        .windows(closing.len())
        .position(|window| window == closing.as_slice())
        .map_or(bytes.len(), |n| body + n + closing.len())
}

/// Skip a char literal (`'a'`, `'\n'`, `'→'`); a lifetime skips its quote only.
fn char_end(source: &str, at: usize) -> usize {
    let bytes = source.as_bytes();
    if bytes.get(at + 1) == Some(&b'\\') {
        // The escaped character is at `at + 2`; the closing quote follows it.
        let from = (at + 3).min(bytes.len());
        return bytes[from..]
            .iter()
            .position(|b| *b == b'\'')
            .map_or(bytes.len(), |n| from + n + 1);
    }
    let Some(c) = source.get(at + 1..).and_then(|rest| rest.chars().next()) else {
        return at + 1;
    };
    let after = at + 1 + c.len_utf8();
    if bytes.get(after) == Some(&b'\'') {
        after + 1
    } else {
        at + 1
    }
}

/// For `#[...]` or `#![...]` at `at`, the range inside the brackets.
fn attribute_body(bytes: &[u8], at: usize) -> Option<(usize, usize)> {
    let mut i = at + 1;
    if bytes.get(i) == Some(&b'!') {
        i += 1;
    }
    if bytes.get(i) != Some(&b'[') {
        return None;
    }
    let open = i + 1;
    let mut depth = 1_u32;
    let mut j = open;
    while j < bytes.len() {
        match bytes[j] {
            b'"' => {
                j = string_end(bytes, j);
                continue;
            }
            b'[' => depth += 1,
            b']' => {
                depth -= 1;
                if depth == 0 {
                    return Some((open, j));
                }
            }
            _ => {}
        }
        j += 1;
    }
    None
}

/// The names in each `allow(...)` and `expect(...)` of one attribute body.
fn lints_in_attribute(body: &str) -> Vec<String> {
    let text = without_strings(body);
    let bytes = text.as_bytes();
    let mut lints = Vec::new();
    for word in ["allow", "expect"] {
        let mut from = 0;
        while let Some(n) = text[from..].find(word) {
            let start = from + n;
            from = start + word.len();
            let before_free = start == 0 || !is_ident(bytes[start - 1]);
            let rest = text[from..].trim_start();
            if !before_free || !rest.starts_with('(') {
                continue;
            }
            let open = text.len() - rest.len() + 1;
            lints.extend(list_items(&text[open..]));
        }
    }
    lints
}

/// The comma-separated names up to the `)` that closes the list; `reason =
/// ...` and other `key = value` items are not lints.
fn list_items(list: &str) -> Vec<String> {
    let mut depth = 0_u32;
    let mut end = list.len();
    for (i, c) in list.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth == 0 => {
                end = i;
                break;
            }
            ')' => depth -= 1,
            _ => {}
        }
    }
    list[..end]
        .split(',')
        .map(|item| {
            item.chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
        })
        .filter(|item| !item.is_empty() && !item.contains('=') && !item.contains('('))
        .collect()
}

/// `body` with the text of each string literal removed (the quotes stay).
fn without_strings(body: &str) -> String {
    let bytes = body.as_bytes();
    let mut out = String::with_capacity(body.len());
    let mut at = 0;
    while let Some(n) = body[at..].find('"') {
        let open = at + n;
        out.push_str(&body[at..=open]);
        let close = string_end(bytes, open);
        out.push('"');
        at = close.min(body.len());
    }
    out.push_str(&body[at..]);
    out
}

#[cfg(test)]
mod tests {
    use super::allowed_lints;

    #[test]
    fn finds_allow_and_expect_in_attributes() {
        let source = r#"
#![allow(clippy::too_many_lines)]
#[allow(dead_code, clippy::too_many_arguments)]
fn a() {}
#[expect(clippy::unwrap_used, reason = "the allow(clippy::todo) here is text")]
fn b() {}
#[cfg_attr(test, allow(clippy::expect_used))]
fn c() {}
#[allow(
    clippy::cast_possible_truncation,
    clippy :: cast_sign_loss,
)]
fn d() {}
"#;
        assert_eq!(
            allowed_lints(source),
            [
                "clippy::too_many_lines",
                "dead_code",
                "clippy::too_many_arguments",
                "clippy::unwrap_used",
                "clippy::expect_used",
                "clippy::cast_possible_truncation",
                "clippy::cast_sign_loss",
            ]
        );
    }

    #[test]
    fn skips_comments_strings_and_lifetimes() {
        let source = r####"
// #[allow(clippy::too_many_lines)]
/* #[allow(clippy::todo)] /* nested */ #[allow(clippy::todo)] */
fn emit<'a>(out: &'a mut String) {
    out.push_str("#[allow(clippy::too_many_lines)]\n");
    out.push_str(r#"#[allow(clippy::dbg_macro)]"#);
    let quote = '"';
    let escaped = '\'';
    let hash = '#';
}
#[allow(unsafe_code)]
fn e() {}
"####;
        assert_eq!(allowed_lints(source), ["unsafe_code"]);
    }

    #[test]
    fn ignores_words_that_only_contain_allow() {
        let source = "#[derive(Allowance)]\n#[serde(disallow(x))]\nfn f() {}\n";
        assert!(allowed_lints(source).is_empty());
    }
}
