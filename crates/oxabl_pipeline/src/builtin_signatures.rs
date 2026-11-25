//! The syntax of the common built-in functions, for signature help.
//!
//! The keyword reference in `resources/` holds a title and a link for each
//! built-in function and no parameters, so this table is written by hand from
//! the documented syntax. A function that is not in it gets no signature.
//! Each row has the title of its reference page in a comment above it.
//! `tests` below checks that comment, each name against the function registry
//! and each parameter against its label.

/// One syntax of a built-in function.
pub(crate) struct Builtin {
    /// The function name, upper case, as `KEYWORD_DOCS` spells it.
    pub name: &'static str,
    /// The syntax as the documentation writes it; optional parts are in
    /// brackets.
    pub label: &'static str,
    /// Each parameter as the label spells it, in order.
    pub params: &'static [&'static str],
    /// The last parameter may repeat (`arg` of `SUBSTITUTE`).
    pub repeats: bool,
    /// The most arguments the function takes, when the documentation says.
    pub max_args: Option<usize>,
}

const fn row(name: &'static str, label: &'static str, params: &'static [&'static str]) -> Builtin {
    Builtin {
        name,
        label,
        params,
        repeats: false,
        max_args: None,
    }
}

const fn repeating(
    name: &'static str,
    label: &'static str,
    params: &'static [&'static str],
    max_args: Option<usize>,
) -> Builtin {
    Builtin {
        name,
        label,
        params,
        repeats: true,
        max_args,
    }
}

/// Sorted by name; the syntaxes of one name keep their order.
pub(crate) static BUILTINS: &[Builtin] = &[
    // ABSOLUTE function
    row("ABSOLUTE", "ABSOLUTE(expression)", &["expression"]),
    // ADD-INTERVAL function
    row(
        "ADD-INTERVAL",
        "ADD-INTERVAL(date-time, interval, unit)",
        &["date-time", "interval", "unit"],
    ),
    // ASC function
    row(
        "ASC",
        "ASC(expression [, target-codepage [, source-codepage]])",
        &["expression", "target-codepage", "source-codepage"],
    ),
    // BASE64-DECODE function
    row("BASE64-DECODE", "BASE64-DECODE(string)", &["string"]),
    // BASE64-ENCODE function
    row("BASE64-ENCODE", "BASE64-ENCODE(raw-data)", &["raw-data"]),
    // CAN-DO function
    row(
        "CAN-DO",
        "CAN-DO(mask-list [, user-id])",
        &["mask-list", "user-id"],
    ),
    // CAPS function
    row("CAPS", "CAPS(string)", &["string"]),
    // CAST function
    row(
        "CAST",
        "CAST(object-reference, type-name)",
        &["object-reference", "type-name"],
    ),
    // CHR function
    row(
        "CHR",
        "CHR(expression [, target-codepage [, source-codepage]])",
        &["expression", "target-codepage", "source-codepage"],
    ),
    // CODEPAGE-CONVERT function
    row(
        "CODEPAGE-CONVERT",
        "CODEPAGE-CONVERT(source [, target-codepage [, source-codepage]])",
        &["source", "target-codepage", "source-codepage"],
    ),
    // COMPARE function
    row(
        "COMPARE",
        "COMPARE(string1, relational-operator, string2, strength [, collation])",
        &[
            "string1",
            "relational-operator",
            "string2",
            "strength",
            "collation",
        ],
    ),
    // DATE function
    row("DATE", "DATE(month, day, year)", &["month", "day", "year"]),
    // DATE function
    row("DATE", "DATE(expression)", &["expression"]),
    // DATETIME function
    row(
        "DATETIME",
        "DATETIME(date-exp [, mtime-exp])",
        &["date-exp", "mtime-exp"],
    ),
    // DATETIME function
    row("DATETIME", "DATETIME(string)", &["string"]),
    // DATETIME function
    row(
        "DATETIME",
        "DATETIME(month, day, year, hours, minutes [, seconds [, milliseconds]])",
        &[
            "month",
            "day",
            "year",
            "hours",
            "minutes",
            "seconds",
            "milliseconds",
        ],
    ),
    // DAY function
    row("DAY", "DAY(date)", &["date"]),
    // DECIMAL function
    row("DECIMAL", "DECIMAL(expression)", &["expression"]),
    // ENTRY function
    row(
        "ENTRY",
        "ENTRY(element, list [, delimiter])",
        &["element", "list", "delimiter"],
    ),
    // EXP function
    row("EXP", "EXP(base, exponent)", &["base", "exponent"]),
    // EXTENT function
    row("EXTENT", "EXTENT(array)", &["array"]),
    // FILL function
    row(
        "FILL",
        "FILL(expression, repeats)",
        &["expression", "repeats"],
    ),
    // INDEX function
    row(
        "INDEX",
        "INDEX(source, target [, starting])",
        &["source", "target", "starting"],
    ),
    // INT64 function
    row("INT64", "INT64(expression)", &["expression"]),
    // INTEGER function
    row("INTEGER", "INTEGER(expression)", &["expression"]),
    // INTERVAL function
    row(
        "INTERVAL",
        "INTERVAL(date-time1, date-time2, unit)",
        &["date-time1", "date-time2", "unit"],
    ),
    // LC function
    row("LC", "LC(string)", &["string"]),
    // LEFT-TRIM function
    row(
        "LEFT-TRIM",
        "LEFT-TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    // LENGTH function
    row("LENGTH", "LENGTH(string [, type])", &["string", "type"]),
    // LOG function
    row("LOG", "LOG(expression [, base])", &["expression", "base"]),
    // LOGICAL function
    row(
        "LOGICAL",
        "LOGICAL(expression [, format])",
        &["expression", "format"],
    ),
    // LOOKUP function
    row(
        "LOOKUP",
        "LOOKUP(expression, list [, delimiter])",
        &["expression", "list", "delimiter"],
    ),
    // MAXIMUM function
    repeating(
        "MAXIMUM",
        "MAXIMUM(expression, expression [, expression]...)",
        &["expression", "expression", "expression"],
        None,
    ),
    // MINIMUM function
    repeating(
        "MINIMUM",
        "MINIMUM(expression, expression [, expression]...)",
        &["expression", "expression", "expression"],
        None,
    ),
    // MONTH function
    row("MONTH", "MONTH(date)", &["date"]),
    // NUM-ENTRIES function
    row(
        "NUM-ENTRIES",
        "NUM-ENTRIES(list [, delimiter])",
        &["list", "delimiter"],
    ),
    // OS-GETENV function
    row("OS-GETENV", "OS-GETENV(name)", &["name"]),
    // PROGRAM-NAME function
    row("PROGRAM-NAME", "PROGRAM-NAME(level)", &["level"]),
    // QUOTER function
    row(
        "QUOTER",
        "QUOTER(expression [, quote-char [, null-string]])",
        &["expression", "quote-char", "null-string"],
    ),
    // R-INDEX function
    row(
        "R-INDEX",
        "R-INDEX(source, target [, starting])",
        &["source", "target", "starting"],
    ),
    // RANDOM function
    row("RANDOM", "RANDOM(low, high)", &["low", "high"]),
    // REPLACE function
    row(
        "REPLACE",
        "REPLACE(source-string, from-string, to-string)",
        &["source-string", "from-string", "to-string"],
    ),
    // RIGHT-TRIM function
    row(
        "RIGHT-TRIM",
        "RIGHT-TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    // ROUND function
    row(
        "ROUND",
        "ROUND(expression, precision)",
        &["expression", "precision"],
    ),
    // SQRT function
    row("SQRT", "SQRT(expression)", &["expression"]),
    // STRING function
    row(
        "STRING",
        "STRING(expression [, format])",
        &["expression", "format"],
    ),
    // SUBSTITUTE function
    repeating(
        "SUBSTITUTE",
        "SUBSTITUTE(base-string [, arg]...)",
        &["base-string", "arg"],
        Some(10),
    ),
    // SUBSTRING function
    row(
        "SUBSTRING",
        "SUBSTRING(string, position [, length [, type]])",
        &["string", "position", "length", "type"],
    ),
    // TRIM function
    row(
        "TRIM",
        "TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    // TRUNCATE function
    row(
        "TRUNCATE",
        "TRUNCATE(expression, decimal-places)",
        &["expression", "decimal-places"],
    ),
    // VALID-HANDLE function
    row("VALID-HANDLE", "VALID-HANDLE(handle)", &["handle"]),
    // VALID-OBJECT function
    row(
        "VALID-OBJECT",
        "VALID-OBJECT(object-reference)",
        &["object-reference"],
    ),
    // WEEKDAY function
    row("WEEKDAY", "WEEKDAY(date)", &["date"]),
    // YEAR function
    row("YEAR", "YEAR(date)", &["date"]),
];

/// The syntaxes of the built-in function `name` (full name, any case).
pub(crate) fn syntaxes(name: &str) -> impl Iterator<Item = &'static Builtin> + '_ {
    let upper = name.to_ascii_uppercase();
    let at = BUILTINS.partition_point(|b| b.name < upper.as_str());
    BUILTINS[at..]
        .iter()
        .take_while(move |b| b.name.eq_ignore_ascii_case(name))
}

/// The byte range of each parameter in `builtin.label`, found in order.
pub(crate) fn parameter_ranges(builtin: &Builtin) -> Vec<std::ops::Range<usize>> {
    let mut from = builtin.label.find('(').map_or(0, |at| at + 1);
    builtin
        .params
        .iter()
        .map(|param| {
            let start = from
                + builtin.label[from..]
                    .find(param)
                    .expect("a parameter is in its label");
            from = start + param.len();
            start..from
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_table_is_sorted() {
        assert!(BUILTINS.windows(2).all(|w| w[0].name <= w[1].name));
    }

    #[test]
    fn every_name_is_a_documented_function() {
        for builtin in BUILTINS {
            let lower = builtin.name.to_ascii_lowercase();
            assert!(
                oxabl_lexer::is_builtin_function(&lower),
                "{} is not a built-in function",
                builtin.name
            );
            assert!(
                crate::keyword_docs::KEYWORD_DOCS
                    .iter()
                    .any(|doc| doc.kind == crate::keyword_docs::DocKind::Function
                        && doc.words == [builtin.name]),
                "{} has no documentation entry",
                builtin.name
            );
        }
    }

    /// Each row has the title of its reference page in a comment above it,
    /// and that title is the documentation entry of the function.
    #[test]
    fn every_row_names_its_reference_page() {
        let source = include_str!("builtin_signatures.rs");
        let table = source
            .split("pub(crate) static BUILTINS")
            .nth(1)
            .and_then(|rest| rest.split("\n];").next())
            .expect("the table is in this file");
        let lines: Vec<&str> = table.lines().map(str::trim).collect();
        let mut rows = 0;
        for (at, line) in lines.iter().enumerate() {
            if !(line.starts_with("row(") || line.starts_with("repeating(")) {
                continue;
            }
            rows += 1;
            let name = lines[at..]
                .iter()
                .find_map(|l| l.split('"').nth(1))
                .expect("a row has a name");
            let page = lines[..at]
                .last()
                .and_then(|l| l.strip_prefix("// "))
                .unwrap_or_else(|| panic!("the row of {name} has no source comment above it"));
            let title = crate::keyword_docs::KEYWORD_DOCS
                .iter()
                .find(|doc| {
                    doc.kind == crate::keyword_docs::DocKind::Function && doc.words == [name]
                })
                .map(|doc| doc.title);
            assert_eq!(
                title,
                Some(page),
                "the source of the row of {name} is not its reference page"
            );
        }
        assert_eq!(rows, BUILTINS.len(), "every row was checked");
    }

    #[test]
    fn every_parameter_is_in_its_label() {
        for builtin in BUILTINS {
            assert!(builtin.label.starts_with(builtin.name), "{}", builtin.label);
            let ranges = parameter_ranges(builtin);
            assert_eq!(ranges.len(), builtin.params.len());
        }
    }

    #[test]
    fn a_name_has_all_its_syntaxes() {
        assert_eq!(syntaxes("datetime").count(), 3);
        assert_eq!(syntaxes("Length").count(), 1);
        assert_eq!(syntaxes("no-such").count(), 0);
    }
}
