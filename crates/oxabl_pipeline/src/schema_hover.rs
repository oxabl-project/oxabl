//! What the loaded schema says about one table or one field, as short
//! Markdown for a hover.
//!
//! Each function reads one table of an `oxabl_schema::Schema` and nothing
//! else, so another request that shows a table (a schema browser) can use the
//! same text.

use oxabl_schema::{EffectiveIndex, Field, SchemaType, Table};

/// The most indexes a hover lists; the rest are counted.
const MAX_INDEXES: usize = 8;

/// A table: its name, label and description, how many fields it has, and its
/// indexes with their fields.
pub fn table_markdown(table: &Table) -> String {
    let mut out = code(&format!("TABLE {}", table.display_name));
    let mut facts = Vec::new();
    if let Some(label) = non_empty(&table.label) {
        facts.push(format!("Label: {}", inline(label)));
    }
    facts.push(format!("Fields: {}", table.fields.len()));
    out.push_str("\n\n");
    out.push_str(&facts.join(" · "));
    if let Some(description) = non_empty(&table.description) {
        out.push_str("\n\n");
        out.push_str(&plain(description));
    }
    let indexes: Vec<EffectiveIndex> = table
        .indexes
        .iter()
        .map(EffectiveIndex::from_schema)
        .collect();
    if !indexes.is_empty() {
        out.push_str("\n\nIndexes:");
        index_lines(&mut out, &indexes);
    }
    out
}

/// A field of `table`: its type, format, label, initial value and
/// description, and the indexes of `table` it takes part in.
pub fn field_markdown(table: &Table, field: &Field) -> String {
    let mut line = format!("FIELD {}.{}", table.display_name, field.display_name);
    let typed = type_name(&field.data_type);
    if let Some(type_name) = &typed {
        line.push_str(&format!(" AS {type_name}"));
    }
    match field.extent {
        Some(0) => line.push_str(" EXTENT"),
        Some(n) => line.push_str(&format!(" EXTENT {n}")),
        None => {}
    }
    let mut out = code(&line);
    let mut facts = Vec::new();
    if typed.is_none() {
        facts.push("Type: unknown".to_string());
    }
    if let Some(format) = non_empty(&field.format) {
        facts.push(format!("Format: {}", inline(format)));
    }
    if let Some(label) = non_empty(&field.label) {
        facts.push(format!("Label: {}", inline(label)));
    }
    if field.initial_is_unknown() {
        facts.push(format!("Initial: {}", inline("?")));
    } else if let Some(initial) = non_empty(&field.initial) {
        facts.push(format!("Initial: {}", inline(initial)));
    }
    if field.mandatory {
        facts.push("Mandatory".to_string());
    }
    if !facts.is_empty() {
        out.push_str("\n\n");
        out.push_str(&facts.join(" · "));
    }
    if let Some(description) = non_empty(&field.description) {
        out.push_str("\n\n");
        out.push_str(&plain(description));
    }
    let indexes: Vec<EffectiveIndex> = table
        .indexes
        .iter()
        .filter(|index| index.fields.iter().any(|part| part.name == field.name))
        .map(EffectiveIndex::from_schema)
        .collect();
    out.push_str("\n\n");
    if indexes.is_empty() {
        out.push_str("In no index.");
    } else {
        out.push_str("Indexes:");
        index_lines(&mut out, &indexes);
    }
    out
}

/// One list line per index: its name, its flags and its fields.
pub(crate) fn index_lines(out: &mut String, indexes: &[EffectiveIndex]) {
    for index in indexes.iter().take(MAX_INDEXES) {
        let mut flags = Vec::new();
        for (set, flag) in [
            (index.primary, "primary"),
            (index.unique, "unique"),
            (index.word, "word"),
            (index.inactive, "inactive"),
        ] {
            if set {
                flags.push(flag);
            }
        }
        out.push_str(&format!("\n- {}", inline(&index.name)));
        if !flags.is_empty() {
            out.push_str(&format!(" ({})", flags.join(", ")));
        }
        if !index.fields.is_empty() {
            out.push_str(&format!(": {}", index.fields.join(", ")));
        }
    }
    if indexes.len() > MAX_INDEXES {
        out.push_str(&format!("\n- and {} more", indexes.len() - MAX_INDEXES));
    }
}

/// The ABL keyword of a schema type; `None` for a type the `.df` did not
/// give in a form oxabl reads.
fn type_name(data_type: &SchemaType) -> Option<String> {
    let name = match data_type {
        SchemaType::Integer => "INTEGER",
        SchemaType::Int64 => "INT64",
        SchemaType::Decimal => "DECIMAL",
        SchemaType::Character => "CHARACTER",
        SchemaType::Longchar => "LONGCHAR",
        SchemaType::Logical => "LOGICAL",
        SchemaType::Date => "DATE",
        SchemaType::Datetime => "DATETIME",
        SchemaType::DatetimeTz => "DATETIME-TZ",
        SchemaType::Handle => "HANDLE",
        SchemaType::Raw => "RAW",
        SchemaType::Recid => "RECID",
        SchemaType::Rowid => "ROWID",
        SchemaType::Blob => "BLOB",
        SchemaType::Clob => "CLOB",
        SchemaType::Unknown(written) => return Some(written.as_ref().to_ascii_uppercase()),
        SchemaType::Error => return None,
    };
    Some(name.to_string())
}

fn non_empty(value: &Option<String>) -> Option<&str> {
    value.as_deref().map(str::trim).filter(|v| !v.is_empty())
}

/// An ABL code block. Its fence is longer than any run of backticks in
/// `text`, so no text closes it.
pub(crate) fn code(text: &str) -> String {
    let fence = "`".repeat((longest_backtick_run(text) + 1).max(3));
    format!("{fence}abl\n{text}\n{fence}")
}

/// `text` as a Markdown code span, on one line. The fence is longer than any
/// run of backticks in the text, and a space pads a text that starts or ends
/// with a backtick.
pub(crate) fn inline(text: &str) -> String {
    let text = one_line(text);
    let fence = "`".repeat(longest_backtick_run(&text) + 1);
    let pad = if text.starts_with('`') || text.ends_with('`') {
        " "
    } else {
        ""
    };
    format!("{fence}{pad}{text}{pad}{fence}")
}

fn longest_backtick_run(text: &str) -> usize {
    text.split(|c| c != '`').map(str::len).max().unwrap_or(0)
}

/// `text` as plain Markdown on one line, with the characters that Markdown
/// reads as markup escaped.
pub(crate) fn plain(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for c in one_line(text).chars() {
        if matches!(
            c,
            '\\' | '`' | '*' | '_' | '[' | ']' | '<' | '>' | '#' | '|' | '!'
        ) {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

/// The longest text kept on one line, in characters.
const MAX_LINE: usize = 160;

fn one_line(text: &str) -> String {
    let mut line = text.split_whitespace().collect::<Vec<_>>().join(" ");
    if line.chars().count() > MAX_LINE {
        line = line.chars().take(MAX_LINE).collect::<String>() + "…";
    }
    line
}
