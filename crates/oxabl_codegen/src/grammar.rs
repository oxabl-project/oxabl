//! Generates the VS Code TextMate grammar from a hand-written template and the
//! ABL keyword data in `resources/`.
//!
//! The template is JSON. Inside any string, `{{word:NAME}}` becomes a regex for
//! the keyword `NAME` and each of its legal abbreviations, and
//! `{{keywords:CATEGORY}}` a regex for every spelling in a category. Both are
//! built as a trie of lower-case spellings, for use inside `(?i)` patterns.
//!
//! Keywords are the union of the HTML keyword index, the reserved list and the
//! `[[add]]` overrides, less the `[[remove]]` overrides. Only plain words take
//! part; the abbreviation of a keyword is the shortest one a source gives that
//! is a prefix of the name. Each spelling belongs to the first category that
//! holds it, in the order of the `categories` list.
//!
//! A name that is unreserved, not in the overrides and documented only as an
//! attribute, method, property or event is *member-only*: it is left out of
//! `other` when it is a single word that is not a widget option, and the
//! grammar highlights it only after `:`. Built-in functions form a separate
//! set, `function`, that the grammar applies only before `(`.

use scraper::{Html, Selector};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::{
    JsonEntry, PHRASE_FUNCTIONS, UNINDEXED_FUNCTIONS, UNRESERVED_FUNCTION_ABBREVS, load_overrides,
    parse_reserved_keywords,
};

/// Control-flow keywords.
const CONTROL: &[&str] = &[
    "IF",
    "THEN",
    "ELSE",
    "DO",
    "END",
    "FOR",
    "EACH",
    "REPEAT",
    "WHILE",
    "CASE",
    "WHEN",
    "OTHERWISE",
    "LEAVE",
    "NEXT",
    "RETURN",
    "UNDO",
    "RETRY",
    "CATCH",
    "FINALLY",
    "THROW",
    "STOP",
    "QUIT",
];

/// Data types.
const TYPES: &[&str] = &[
    "BLOB",
    "BYTE",
    "CHARACTER",
    "CLOB",
    "COM-HANDLE",
    "DATASET-HANDLE",
    "DATE",
    "DATETIME",
    "DATETIME-TZ",
    "DECIMAL",
    "DOUBLE",
    "FLOAT",
    "HANDLE",
    "INT64",
    "INTEGER",
    "LOGICAL",
    "LONG",
    "LONGCHAR",
    "MEMPTR",
    "RAW",
    "RECID",
    "ROWID",
    "SHORT",
    "TABLE-HANDLE",
    "UNSIGNED-BYTE",
    "UNSIGNED-INT64",
    "UNSIGNED-INTEGER",
    "UNSIGNED-LONG",
    "UNSIGNED-SHORT",
    "VOID",
    "WIDGET-HANDLE",
];

/// Access, storage and parameter-mode modifiers.
const MODIFIERS: &[&str] = &[
    "ABSTRACT",
    "FINAL",
    "GLOBAL",
    "INPUT",
    "INPUT-OUTPUT",
    "NO-UNDO",
    "NON-SERIALIZABLE",
    "OUTPUT",
    "OVERRIDE",
    "PACKAGE-PRIVATE",
    "PACKAGE-PROTECTED",
    "PRIVATE",
    "PROTECTED",
    "PUBLIC",
    "SERIALIZABLE",
    "SHARED",
    "STATIC",
];

/// Single-word names the docs list only as members that are also widget
/// options, so they stay keywords outside member access.
const MEMBER_OPTIONS: &[&str] = &[
    "EXPANDABLE",
    "HIDDEN",
    "HORIZONTAL",
    "LARGE",
    "MOVABLE",
    "MULTIPLE",
    "RESIZABLE",
    "SCROLLABLE",
    "SELECTABLE",
    "SENSITIVE",
    "SORT",
    "TOOLTIP",
    "TRANSPARENT",
    "VISIBLE",
    "WIDTH",
];

/// Roles in the JSON index that only a member access can have.
const MEMBER_ROLES: &[&str] = &["attribute", "method", "property", "event"];

/// Functions the grammar adds to the built-in set that `builtins.rs` lacks.
const GRAMMAR_FUNCTIONS: &[&str] = &["defined"];

const SYSTEM_HANDLE_SUFFIX: &str = " system handle";
const SYSTEM_REFERENCE_SUFFIX: &str = " system reference";

/// Plain words: `&?[A-Za-z_][A-Za-z0-9_#$%&-]*`.
fn is_word(s: &str) -> bool {
    let mut chars = s.strip_prefix('&').unwrap_or(s).chars();
    matches!(chars.next(), Some(c) if c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || "_#$%&-".contains(c))
}

/// Records `name` with the shortest spelling `abbreviation` allows. An
/// abbreviation that is not a prefix of the name (an index typo) is ignored.
fn admit(min_len: &mut HashMap<String, usize>, name: &str, abbreviation: Option<&str>) {
    if !is_word(name) {
        return;
    }
    let up = name.to_uppercase();
    let mut n = up.len();
    if let Some(ab) = abbreviation.filter(|ab| !ab.is_empty())
        && up.starts_with(&ab.to_uppercase())
        && is_word(ab)
    {
        n = ab.len();
    }
    let entry = min_len.entry(up.clone()).or_insert(up.len());
    *entry = (*entry).min(n);
}

/// A keyword name and its minimum abbreviation, if any.
type IndexRow = (String, Option<String>);

/// Keyword names and their abbreviations from the HTML index, and the names it
/// marks reserved. A row is `tr.row` with three cells: name, reserved mark
/// (an image), minimum abbreviation (`–` for none).
fn html_index(path: &Path) -> Result<(Vec<IndexRow>, HashSet<String>), String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let document = Html::parse_document(&content);
    let rows = Selector::parse("tr.row").expect("valid selector");
    let cells = Selector::parse("td").expect("valid selector");

    let mut index = Vec::new();
    let mut reserved = HashSet::new();
    for row in document.select(&rows) {
        let tds: Vec<_> = row.select(&cells).collect();
        if tds.len() != 3 {
            continue;
        }
        let text = |i: usize| tds[i].text().collect::<String>().trim().to_string();
        let name = text(0);
        let abbreviation = text(2);
        let abbreviation =
            (!abbreviation.is_empty() && abbreviation != "\u{2013}").then_some(abbreviation);
        if tds[1].inner_html().contains("check mark") {
            reserved.insert(name.to_uppercase());
        }
        index.push((name, abbreviation));
    }
    Ok((index, reserved))
}

/// Every title in the JSON index, trimmed, children included.
fn json_titles(path: &Path) -> Result<Vec<String>, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let entries: Vec<JsonEntry> = serde_json::from_str(&content)
        .map_err(|e| format!("cannot parse {}: {e}", path.display()))?;

    fn walk(entries: &[JsonEntry], titles: &mut Vec<String>) {
        for entry in entries {
            titles.push(entry.title.trim().to_string());
            walk(&entry.child_entries, titles);
        }
    }

    let mut titles = Vec::new();
    walk(&entries, &mut titles);
    Ok(titles)
}

/// Splits a title like `NAME attribute` or `NAME () method` into the name and
/// its role (`[a-z][a-z -]*`).
fn split_role(title: &str) -> Option<(&str, &str)> {
    let end = title.find(|c: char| c.is_whitespace() || c == '(')?;
    if end == 0 {
        return None;
    }
    let (name, rest) = title.split_at(end);

    fn role_after_space(s: &str) -> Option<&str> {
        let role = s.trim_start();
        if role.len() == s.len() {
            return None;
        }
        let mut chars = role.chars();
        let first = chars.next()?;
        (first.is_ascii_lowercase()
            && chars.all(|c| c.is_ascii_lowercase() || c == ' ' || c == '-'))
        .then_some(role)
    }

    let empty_parens = rest
        .trim_start()
        .strip_prefix('(')
        .and_then(|r| r.trim_start().strip_prefix(')'));
    let role = empty_parens
        .and_then(role_after_space)
        .or_else(|| role_after_space(rest))?;
    Some((name, role))
}

/// The name of a `NAME function` or `NAME function (qualifier)` title.
fn function_name(title: &str) -> Option<&str> {
    let end = title.find(char::is_whitespace)?;
    let name = &title[..end];
    let rest = title[end..].strip_prefix(" function")?;
    let ok = rest.is_empty() || (rest.starts_with(" (") && rest.ends_with(')') && rest.len() >= 3);
    (end > 0 && ok).then_some(name)
}

/// Every prefix of `name` from `min` characters to the full name.
fn prefixes(name: &str, min: usize) -> Vec<String> {
    (min..=name.len()).map(|i| name[..i].to_string()).collect()
}

#[derive(Default)]
struct Trie {
    end: bool,
    kids: BTreeMap<char, Trie>,
}

/// Lower-case regex alternation of `words`, with shared prefixes factored out.
fn trie_regex(words: &[String]) -> String {
    fn escape(c: char) -> String {
        if ".^$|?*+()[]{}\\".contains(c) {
            format!("\\{c}")
        } else {
            c.to_string()
        }
    }

    fn emit(node: &Trie) -> String {
        let alts: Vec<String> = node
            .kids
            .iter()
            .map(|(c, kid)| escape(*c) + &emit(kid))
            .collect();
        let (body, grouped) = match alts.as_slice() {
            [] => return String::new(),
            [only] => {
                let len = only.chars().count();
                let atom = len == 1 || (len == 2 && only.starts_with('\\'));
                let grouped = if atom {
                    only.clone()
                } else {
                    format!("(?:{only})")
                };
                (only.clone(), grouped)
            }
            _ => {
                let group = format!("(?:{})", alts.join("|"));
                (group.clone(), group)
            }
        };
        if node.end { grouped + "?" } else { body }
    }

    let mut root = Trie::default();
    for word in words {
        let mut node = &mut root;
        for c in word.to_lowercase().chars() {
            node = node.kids.entry(c).or_default();
        }
        node.end = true;
    }
    emit(&root)
}

/// The keyword data a template is filled from.
struct Keywords {
    /// Uppercase keyword name to the length of its shortest legal spelling.
    min_len: HashMap<String, usize>,
    /// Category name to its sorted spellings, `function` included.
    by_category: BTreeMap<&'static str, Vec<String>>,
}

impl Keywords {
    fn load(resources: &Path) -> Result<Self, String> {
        let overrides = load_overrides(&resources.join("keyword_overrides.toml"))
            .ok_or("cannot read keyword_overrides.toml")?;

        let mut min_len: HashMap<String, usize> = HashMap::new();
        let (index, mut reserved) = html_index(&resources.join("abl_keyword_index.html"))?;
        for (name, abbreviation) in &index {
            admit(&mut min_len, name, abbreviation.as_deref());
        }
        for keyword in parse_reserved_keywords(&resources.join("abl_reserved_keywords.txt")) {
            admit(
                &mut min_len,
                &keyword.name,
                keyword.min_abbreviation.as_deref(),
            );
            reserved.insert(keyword.name.to_uppercase());
        }
        let mut curated = HashSet::new();
        for add in &overrides.add {
            admit(&mut min_len, &add.name, add.min_abbreviation.as_deref());
            curated.insert(add.name.to_uppercase());
        }
        for ov in &overrides.r#override {
            if min_len.contains_key(&ov.name.to_uppercase())
                && let Some(ab) = ov.min_abbreviation.as_deref()
            {
                admit(&mut min_len, &ov.name, Some(ab));
            }
        }
        for rm in &overrides.remove {
            min_len.remove(&rm.name.to_uppercase());
        }

        let spellings = |name: &str| -> Vec<String> {
            let up = name.to_uppercase();
            let lo = min_len.get(&up).copied().unwrap_or(up.len());
            prefixes(&up, lo)
        };

        let titles = json_titles(&resources.join("abl_keyword_index.json"))?;
        let mut roles: HashMap<String, HashSet<&str>> = HashMap::new();
        for title in &titles {
            if let Some((name, role)) = split_role(title) {
                roles.entry(name.to_uppercase()).or_default().insert(role);
            }
        }
        let member_only = |n: &str| -> bool {
            let Some(r) = roles.get(n) else {
                return false;
            };
            if reserved.contains(n)
                || curated.contains(n)
                || !r.iter().all(|role| MEMBER_ROLES.contains(role))
            {
                return false;
            }
            !n.contains('-') && !MEMBER_OPTIONS.contains(&n)
        };

        let handles: BTreeSet<String> = titles
            .iter()
            .filter_map(|t| {
                t.strip_suffix(SYSTEM_HANDLE_SUFFIX)
                    .or_else(|| t.strip_suffix(SYSTEM_REFERENCE_SUFFIX))
            })
            .map(str::to_uppercase)
            .collect();

        let mut functions: BTreeSet<String> = titles
            .iter()
            .filter_map(|t| function_name(t))
            .map(str::to_uppercase)
            .collect();
        functions.extend(
            PHRASE_FUNCTIONS
                .iter()
                .chain(UNINDEXED_FUNCTIONS)
                .chain(GRAMMAR_FUNCTIONS)
                .map(|f| f.to_uppercase()),
        );
        let mut function_spellings: BTreeSet<String> = BTreeSet::new();
        for f in functions.iter().filter(|f| is_word(f)) {
            if min_len.contains_key(f) {
                function_spellings.extend(spellings(f));
            } else {
                function_spellings.insert(f.clone());
            }
            if let Some((_, abbreviation)) = UNRESERVED_FUNCTION_ABBREVS
                .iter()
                .find(|(full, _)| full.eq_ignore_ascii_case(f))
            {
                function_spellings.extend(prefixes(f, abbreviation.len()));
            }
        }

        let strings = |names: &[&str]| names.iter().map(|n| n.to_string()).collect::<Vec<_>>();
        let mut other: Vec<String> = min_len
            .keys()
            .filter(|n| !n.starts_with('&') && !member_only(n))
            .cloned()
            .collect();
        other.sort();
        let categories: Vec<(&'static str, Vec<String>)> = vec![
            ("boolean", strings(&["TRUE", "FALSE", "YES", "NO"])),
            ("operator-logical", strings(&["AND", "OR", "NOT"])),
            (
                "operator-comparison",
                strings(&["EQ", "NE", "LT", "LE", "GT", "GE"]),
            ),
            (
                "operator-pattern",
                strings(&["BEGINS", "MATCHES", "CONTAINS"]),
            ),
            ("operator-arithmetic", strings(&["MODULO"])),
            ("control", strings(CONTROL)),
            ("type", strings(TYPES)),
            ("modifier", strings(MODIFIERS)),
            ("handle", handles.into_iter().collect()),
            ("other", other),
        ];

        let mut owner: HashMap<String, &'static str> = HashMap::new();
        for (category, names) in &categories {
            for name in names {
                for spelling in spellings(name) {
                    owner.entry(spelling).or_insert(category);
                }
            }
        }
        let mut by_category: BTreeMap<&'static str, Vec<String>> = categories
            .iter()
            .map(|(category, _)| (*category, Vec::new()))
            .collect();
        for (spelling, category) in owner {
            by_category.entry(category).or_default().push(spelling);
        }
        for spellings in by_category.values_mut() {
            spellings.sort();
        }
        by_category.insert("function", function_spellings.into_iter().collect());

        Ok(Self {
            min_len,
            by_category,
        })
    }

    fn word(&self, name: &str) -> Result<String, String> {
        let up = name.to_uppercase();
        let lo = *self
            .min_len
            .get(&up)
            .ok_or_else(|| format!("{{{{word:{name}}}}}: not a keyword"))?;
        Ok(trie_regex(&prefixes(&up, lo)))
    }

    fn fill(&self, s: &str) -> Result<String, String> {
        let mut out = String::new();
        let mut rest = s;
        while let Some(start) = rest.find("{{") {
            out.push_str(&rest[..start]);
            let after = &rest[start + 2..];
            let end = after
                .find("}}")
                .ok_or_else(|| format!("unclosed `{{{{` in {s:?}"))?;
            let placeholder = &after[..end];
            match placeholder.split_once(':') {
                Some(("keywords", category)) => {
                    let spellings = self
                        .by_category
                        .get(category)
                        .ok_or_else(|| format!("unknown keyword category {category:?}"))?;
                    out.push_str(&trie_regex(spellings));
                }
                Some(("word", name)) => out.push_str(&self.word(name)?),
                _ => return Err(format!("unknown placeholder {{{{{placeholder}}}}}")),
            }
            rest = &after[end + 2..];
        }
        out.push_str(rest);
        Ok(out)
    }

    fn fill_value(&self, value: &mut Value) -> Result<(), String> {
        match value {
            Value::String(s) => *s = self.fill(s)?,
            Value::Array(items) => {
                for item in items {
                    self.fill_value(item)?;
                }
            }
            Value::Object(map) => {
                for item in map.values_mut() {
                    self.fill_value(item)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}

/// Fills `template` from the keyword data in `resources` and returns the
/// grammar JSON.
pub fn generate(resources: &Path, template: &Path) -> Result<String, String> {
    let keywords = Keywords::load(resources)?;
    for (category, spellings) in &keywords.by_category {
        eprintln!("  {category}: {} spellings", spellings.len());
    }

    let text = fs::read_to_string(template)
        .map_err(|e| format!("cannot read {}: {e}", template.display()))?;
    let mut grammar: Value = serde_json::from_str(&text)
        .map_err(|e| format!("cannot parse {}: {e}", template.display()))?;
    keywords.fill_value(&mut grammar)?;

    let mut json = serde_json::to_string_pretty(&grammar).map_err(|e| e.to_string())?;
    json.push('\n');
    Ok(json)
}
