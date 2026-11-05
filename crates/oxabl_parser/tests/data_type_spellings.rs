//! Every spelling the compiler accepts for a data type is that type in every
//! position that takes one, and every spelling it rejects stays a class name.

use oxabl_ast::{DataType, ParameterType, Statement, StatementKind, TypeSource};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;

/// Each data type with its accepted spellings (`<shortest>` through the full
/// name), as the compiler takes them in a variable, a parameter, a return type,
/// a temp-table field, a property and a method.
const ACCEPTED: &[(&str, DataType, &[&str])] = &[
    (
        "CHARACTER",
        DataType::Character,
        &[
            "c",
            "ch",
            "cha",
            "char",
            "chara",
            "charac",
            "charact",
            "characte",
            "character",
        ],
    ),
    (
        "INTEGER",
        DataType::Integer,
        &["i", "in", "int", "inte", "integ", "intege", "integer"],
    ),
    ("INT64", DataType::Int64, &["int64"]),
    (
        "LOGICAL",
        DataType::Logical,
        &["l", "lo", "log", "logi", "logic", "logica", "logical"],
    ),
    (
        "DECIMAL",
        DataType::Decimal,
        &["de", "dec", "deci", "decim", "decima", "decimal"],
    ),
    ("DATE", DataType::Date, &["da", "dat", "date"]),
    ("DATETIME", DataType::DateTime, &["datetime"]),
    ("DATETIME-TZ", DataType::DateTimeTz, &["datetime-tz"]),
    ("HANDLE", DataType::Handle, &["handle"]),
    (
        "WIDGET-HANDLE",
        DataType::Handle,
        &[
            "widg",
            "widge",
            "widget",
            "widget-",
            "widget-h",
            "widget-ha",
            "widget-han",
            "widget-hand",
            "widget-handl",
            "widget-handle",
        ],
    ),
    ("COM-HANDLE", DataType::Com, &["com-handle"]),
    ("RAW", DataType::Raw, &["ra", "raw"]),
    ("RECID", DataType::Recid, &["re", "rec", "reci", "recid"]),
    ("ROWID", DataType::Rowid, &["row", "rowi", "rowid"]),
    (
        "MEMPTR",
        DataType::Memptr,
        &["m", "me", "mem", "memp", "mempt", "memptr"],
    ),
    (
        "LONGCHAR",
        DataType::Longchar,
        &["longch", "longcha", "longchar"],
    ),
];

/// Spellings the compiler rejects. A class can have any of these names.
const REJECTED: &[&str] = &[
    "d",
    "int6",
    "datet",
    "dateti",
    "datetim",
    "datetime-",
    "datetime-t",
    "h",
    "ha",
    "han",
    "hand",
    "handl",
    "w",
    "wi",
    "wid",
    "co",
    "com",
    "com-",
    "com-h",
    "com-handl",
    "r",
    "ro",
    "lon",
    "long",
    "longc",
];

/// Every data type written in `statements`, in source order.
fn data_types(statements: &[Statement], out: &mut Vec<DataType>) {
    for statement in statements {
        match &statement.kind {
            StatementKind::VariableDeclaration { type_source, .. } => explicit(type_source, out),
            StatementKind::VariableDeclarationList(children) => data_types(children, out),
            StatementKind::DefineParameter {
                param_type: ParameterType::Variable { type_source, .. },
                ..
            } => explicit(type_source, out),
            StatementKind::DefineTempTable { fields, .. } => {
                for field in fields {
                    explicit(&field.type_source, out);
                }
            }
            StatementKind::Function {
                return_type,
                body,
                prototype_parameters,
                ..
            } => {
                out.push(return_type.clone());
                data_types(prototype_parameters, out);
                data_types(body, out);
            }
            StatementKind::Method {
                return_type,
                parameters,
                body,
                ..
            } => {
                out.extend(return_type.clone());
                data_types(parameters, out);
                data_types(body, out);
            }
            StatementKind::Property {
                data_type,
                set_parameters,
                ..
            } => {
                out.push(data_type.clone());
                data_types(set_parameters, out);
            }
            StatementKind::Constructor {
                parameters, body, ..
            } => {
                data_types(parameters, out);
                data_types(body, out);
            }
            StatementKind::TriggerProcedure {
                new_value,
                old_value_param,
                ..
            } => {
                for param in new_value.iter().chain(old_value_param) {
                    out.push(param.data_type.clone());
                }
            }
            StatementKind::Class { body, .. } => data_types(body, out),
            _ => {}
        }
    }
}

fn explicit(type_source: &TypeSource, out: &mut Vec<DataType>) {
    if let TypeSource::Explicit(data_type) = type_source {
        out.push(data_type.clone());
    }
}

/// The data types of `source`, and the parse errors it raised.
fn parse(source: &str) -> (Vec<DataType>, usize) {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let mut types = Vec::new();
    data_types(&program.statements, &mut types);
    (types, program.errors.len())
}

/// One program per position, each with `{t}` where a data type goes. Every
/// template holds the number of data types in the second field.
const POSITIONS: &[(&str, &str, usize)] = &[
    ("variable", "DEFINE VARIABLE v AS {t} NO-UNDO.", 1),
    ("variable, extent", "DEFINE VARIABLE v AS {t} EXTENT 3.", 1),
    ("input parameter", "DEFINE INPUT PARAMETER p AS {t}.", 1),
    (
        "output parameter",
        "DEFINE OUTPUT PARAMETER p AS {t} NO-UNDO.",
        1,
    ),
    ("var statement", "VAR {t} v.", 1),
    (
        "function return and parameter",
        "FUNCTION f RETURNS {t} (INPUT p AS {t}):\n  RETURN ?.\nEND FUNCTION.",
        2,
    ),
    (
        "function return, no parameter",
        "FUNCTION f RETURNS {t}:\n  RETURN ?.\nEND FUNCTION.",
        1,
    ),
    (
        "function prototype",
        "FUNCTION f RETURNS {t} (INPUT p AS {t}) FORWARD.",
        2,
    ),
    (
        "temp-table field",
        "DEFINE TEMP-TABLE tt FIELD f AS {t}.",
        1,
    ),
    (
        "work-table field",
        "DEFINE WORK-TABLE wt FIELD f AS {t}.",
        1,
    ),
    (
        "property",
        "CLASS c1:\n  DEFINE PUBLIC PROPERTY p AS {t} NO-UNDO GET. SET.\nEND CLASS.",
        1,
    ),
    (
        "method return and parameters",
        "CLASS c1:\n  METHOD PUBLIC {t} m(INPUT a AS {t}, OUTPUT b AS {t}):\n    RETURN ?.\n  END METHOD.\nEND CLASS.",
        3,
    ),
    (
        "constructor parameter",
        "CLASS c1:\n  CONSTRUCTOR PUBLIC c1(INPUT a AS {t}):\n  END CONSTRUCTOR.\nEND CLASS.",
        1,
    ),
    (
        "trigger assign parameter",
        "TRIGGER PROCEDURE FOR ASSIGN NEW VALUE nv AS {t} OLD VALUE ov AS {t}.",
        2,
    ),
];

#[test]
fn accepted_spellings_are_the_data_type_in_every_position() {
    let mut failures = Vec::new();
    for (name, expected, spellings) in ACCEPTED {
        for spelling in *spellings {
            for (position, template, count) in POSITIONS {
                let source = template.replace("{t}", spelling);
                let (types, errors) = parse(&source);
                if errors != 0 || types != vec![expected.clone(); *count] {
                    failures.push(format!(
                        "{name} `{spelling}` in {position}: got {types:?} with {errors} errors"
                    ));
                }
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn rejected_spellings_stay_class_names() {
    let mut failures = Vec::new();
    for spelling in REJECTED {
        for (position, template, count) in POSITIONS {
            let source = template.replace("{t}", spelling);
            let (types, _) = parse(&source);
            let class = DataType::Class((*spelling).to_string());
            if types != vec![class; *count] {
                failures.push(format!("`{spelling}` in {position}: got {types:?}"));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn short_type_spellings_are_case_insensitive() {
    let (types, errors) = parse("DEFINE VARIABLE v AS I NO-UNDO.\nDEFINE VARIABLE w AS De.");
    assert_eq!(errors, 0);
    assert_eq!(types, vec![DataType::Integer, DataType::Decimal]);
}

#[test]
fn a_short_type_word_before_a_class_qualifier_is_a_class() {
    let (types, _) = parse("DEFINE VARIABLE v AS c.Foo NO-UNDO.");
    assert_eq!(types, vec![DataType::Class("c.Foo".to_string())]);
}

#[test]
fn a_word_before_the_statement_period_is_a_type() {
    let (types, errors) = parse("DEFINE VARIABLE v AS i.\nDEFINE VARIABLE w AS c.");
    assert_eq!(errors, 0);
    assert_eq!(types, vec![DataType::Integer, DataType::Character]);
}

/// `i`, `c` and `l` are common names; the type rule must not reach them.
#[test]
fn short_type_words_stay_ordinary_names_elsewhere() {
    let source = "DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
                  DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
                  DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
                  DO i = 1 TO 10:\n  c = c + \"x\".\n  l = NOT l.\nEND.";
    let (types, errors) = parse(source);
    assert_eq!(errors, 0);
    assert_eq!(
        types,
        vec![DataType::Integer, DataType::Character, DataType::Logical]
    );
}
