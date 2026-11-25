//! The syntax of the common built-in functions, for signature help.
//!
//! The keyword reference in `resources/` holds a title and a link for each
//! built-in function and no parameters, so this table is written by hand from
//! the documented syntax. A function that is not in it gets no signature.
//! `tests` below checks each name against the function registry and each
//! parameter against its label.

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
}

const fn row(name: &'static str, label: &'static str, params: &'static [&'static str]) -> Builtin {
    Builtin {
        name,
        label,
        params,
        repeats: false,
    }
}

const fn repeating(
    name: &'static str,
    label: &'static str,
    params: &'static [&'static str],
) -> Builtin {
    Builtin {
        name,
        label,
        params,
        repeats: true,
    }
}

/// Sorted by name; the syntaxes of one name keep their order.
pub(crate) static BUILTINS: &[Builtin] = &[
    row("ABSOLUTE", "ABSOLUTE(expression)", &["expression"]),
    row(
        "ADD-INTERVAL",
        "ADD-INTERVAL(date-time, interval, unit)",
        &["date-time", "interval", "unit"],
    ),
    row(
        "ASC",
        "ASC(expression [, target-codepage [, source-codepage]])",
        &["expression", "target-codepage", "source-codepage"],
    ),
    row("BASE64-DECODE", "BASE64-DECODE(string)", &["string"]),
    row("BASE64-ENCODE", "BASE64-ENCODE(raw-data)", &["raw-data"]),
    row(
        "CAN-DO",
        "CAN-DO(mask-list [, user-id])",
        &["mask-list", "user-id"],
    ),
    row("CAPS", "CAPS(string)", &["string"]),
    row(
        "CAST",
        "CAST(object-reference, type-name)",
        &["object-reference", "type-name"],
    ),
    row(
        "CHR",
        "CHR(expression [, target-codepage [, source-codepage]])",
        &["expression", "target-codepage", "source-codepage"],
    ),
    row(
        "CODEPAGE-CONVERT",
        "CODEPAGE-CONVERT(source [, target-codepage [, source-codepage]])",
        &["source", "target-codepage", "source-codepage"],
    ),
    row(
        "COMPARE",
        "COMPARE(string1, operator, string2, strength)",
        &["string1", "operator", "string2", "strength"],
    ),
    row("DATE", "DATE(month, day, year)", &["month", "day", "year"]),
    row("DATE", "DATE(expression)", &["expression"]),
    row("DATETIME", "DATETIME(date [, mtime])", &["date", "mtime"]),
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
    row("DAY", "DAY(date)", &["date"]),
    row("DECIMAL", "DECIMAL(expression)", &["expression"]),
    row(
        "ENTRY",
        "ENTRY(element, list [, delimiter])",
        &["element", "list", "delimiter"],
    ),
    row("EXP", "EXP(base, exponent)", &["base", "exponent"]),
    row("EXTENT", "EXTENT(array)", &["array"]),
    row(
        "FILL",
        "FILL(expression, repeats)",
        &["expression", "repeats"],
    ),
    row(
        "INDEX",
        "INDEX(source, target [, starting])",
        &["source", "target", "starting"],
    ),
    row("INT64", "INT64(expression)", &["expression"]),
    row("INTEGER", "INTEGER(expression)", &["expression"]),
    row(
        "INTERVAL",
        "INTERVAL(date-time1, date-time2, unit)",
        &["date-time1", "date-time2", "unit"],
    ),
    row("LC", "LC(string)", &["string"]),
    row(
        "LEFT-TRIM",
        "LEFT-TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    row("LENGTH", "LENGTH(string [, type])", &["string", "type"]),
    row("LOG", "LOG(expression [, base])", &["expression", "base"]),
    row(
        "LOGICAL",
        "LOGICAL(expression [, format])",
        &["expression", "format"],
    ),
    row(
        "LOOKUP",
        "LOOKUP(expression, list [, delimiter])",
        &["expression", "list", "delimiter"],
    ),
    repeating(
        "MAXIMUM",
        "MAXIMUM(expression, expression [, expression]...)",
        &["expression", "expression", "expression"],
    ),
    repeating(
        "MINIMUM",
        "MINIMUM(expression, expression [, expression]...)",
        &["expression", "expression", "expression"],
    ),
    row("MONTH", "MONTH(date)", &["date"]),
    row(
        "NUM-ENTRIES",
        "NUM-ENTRIES(list [, delimiter])",
        &["list", "delimiter"],
    ),
    row("OS-GETENV", "OS-GETENV(name)", &["name"]),
    row("PROGRAM-NAME", "PROGRAM-NAME(level)", &["level"]),
    row(
        "QUOTER",
        "QUOTER(expression [, delimiter [, quote-char]])",
        &["expression", "delimiter", "quote-char"],
    ),
    row(
        "R-INDEX",
        "R-INDEX(source, target [, starting])",
        &["source", "target", "starting"],
    ),
    row("RANDOM", "RANDOM(low, high)", &["low", "high"]),
    row(
        "REPLACE",
        "REPLACE(source-string, from-string, to-string)",
        &["source-string", "from-string", "to-string"],
    ),
    row(
        "RIGHT-TRIM",
        "RIGHT-TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    row(
        "ROUND",
        "ROUND(expression, precision)",
        &["expression", "precision"],
    ),
    row("SQRT", "SQRT(expression)", &["expression"]),
    row(
        "STRING",
        "STRING(expression [, format])",
        &["expression", "format"],
    ),
    repeating(
        "SUBSTITUTE",
        "SUBSTITUTE(string [, arg]...)",
        &["string", "arg"],
    ),
    row(
        "SUBSTRING",
        "SUBSTRING(string, position [, length [, type]])",
        &["string", "position", "length", "type"],
    ),
    row(
        "TRIM",
        "TRIM(string [, trim-chars])",
        &["string", "trim-chars"],
    ),
    row(
        "TRUNCATE",
        "TRUNCATE(expression, decimal-places)",
        &["expression", "decimal-places"],
    ),
    row("VALID-HANDLE", "VALID-HANDLE(handle)", &["handle"]),
    row(
        "VALID-OBJECT",
        "VALID-OBJECT(object-reference)",
        &["object-reference"],
    ),
    row("WEEKDAY", "WEEKDAY(date)", &["date"]),
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
        assert_eq!(syntaxes("datetime").count(), 2);
        assert_eq!(syntaxes("Length").count(), 1);
        assert_eq!(syntaxes("no-such").count(), 0);
    }
}
