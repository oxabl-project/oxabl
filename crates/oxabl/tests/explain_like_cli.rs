//! `oxabl explain --json` names the right index for a search of a temp-table
//! that is defined `LIKE` another table. The rules are the ones OpenEdge 12.8.9
//! applies to the indexes that `LIKE` inherits.

use std::process::Command;

use serde_json::Value;

const SCHEMA: &str = "ADD TABLE \"Customer\"\n\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\n\
    ADD FIELD \"CustName\" OF \"Customer\" AS character\n\n\
    ADD INDEX \"CustNum\" ON \"Customer\"\n  UNIQUE\n  PRIMARY\n\
    \x20 INDEX-FIELD \"CustNum\" ASCENDING\n\n\
    ADD INDEX \"CustName\" ON \"Customer\"\n\
    \x20 INDEX-FIELD \"CustName\" ASCENDING\n";

const CONFIG: &str = "[workspace]\nname = \"like\"\n[workspace.schema]\nfiles = [\"schema.df\"]\n";

const SOURCE: &str = "DEFINE TEMP-TABLE tt1 NO-UNDO FIELD a AS INTEGER FIELD b AS CHARACTER\n  \
    INDEX ia IS PRIMARY UNIQUE a INDEX ib b.\n";

/// One search: the `WHERE` field (`None` for no `WHERE`) and the index that
/// the compiler's rules give.
type Expect = (Option<&'static str>, Option<&'static str>);

struct Case {
    name: &'static str,
    define: &'static str,
    /// The table's name in `define`.
    table: &'static str,
    searches: &'static [Expect],
}

const CASES: &[Case] = &[
    Case {
        name: "plain LIKE",
        define: "DEFINE TEMP-TABLE tt2 NO-UNDO LIKE tt1.",
        table: "tt2",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (None, Some("ia")),
        ],
    },
    Case {
        name: "one USE-INDEX keeps only that index and makes it primary",
        define: "DEFINE TEMP-TABLE tt3 NO-UNDO LIKE tt1 USE-INDEX ib.",
        table: "tt3",
        searches: &[
            (Some("a"), Some("ib")),
            (Some("b"), Some("ib")),
            (None, Some("ib")),
        ],
    },
    Case {
        name: "own INDEX is added to the inherited ones",
        define: "DEFINE TEMP-TABLE tt4 NO-UNDO LIKE tt1 FIELD c AS LOGICAL INDEX ic c.",
        table: "tt4",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (Some("c"), Some("ic")),
            (None, Some("ia")),
        ],
    },
    Case {
        name: "USE-INDEX with an own INDEX gives the used one and the own one",
        define: "DEFINE TEMP-TABLE tt5 NO-UNDO LIKE tt1 USE-INDEX ia FIELD c AS LOGICAL INDEX ic c.",
        table: "tt5",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("c"), Some("ic")),
            (Some("b"), Some("ia")),
            (None, Some("ia")),
        ],
    },
    Case {
        name: "the first index is primary when the used ones do not include the primary",
        define: "DEFINE TEMP-TABLE tt17 NO-UNDO LIKE tt1 USE-INDEX ib FIELD c AS LOGICAL INDEX a0 c.",
        table: "tt17",
        searches: &[(Some("c"), Some("a0")), (None, Some("ib"))],
    },
    Case {
        name: "chain",
        define: "DEFINE TEMP-TABLE tt2 NO-UNDO LIKE tt1.\n\
            DEFINE TEMP-TABLE tt6 NO-UNDO LIKE tt2 FIELD d AS DECIMAL.",
        table: "tt6",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (None, Some("ia")),
        ],
    },
    Case {
        name: "LIKE-SEQUENTIAL",
        define: "DEFINE TEMP-TABLE tt7 NO-UNDO LIKE-SEQUENTIAL tt1 FIELD c AS LOGICAL.",
        table: "tt7",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (None, Some("ia")),
        ],
    },
    Case {
        name: "USE-INDEX AS PRIMARY",
        define: "DEFINE TEMP-TABLE tt8 NO-UNDO LIKE tt1 USE-INDEX ib AS PRIMARY.",
        table: "tt8",
        searches: &[(Some("b"), Some("ib")), (None, Some("ib"))],
    },
    Case {
        name: "own INDEX IS PRIMARY replaces the inherited primary",
        define: "DEFINE TEMP-TABLE tt9 NO-UNDO LIKE tt1 FIELD c AS LOGICAL INDEX ic IS PRIMARY c.",
        table: "tt9",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (Some("c"), Some("ic")),
            (None, Some("ic")),
        ],
    },
    Case {
        name: "several USE-INDEX, one AS PRIMARY",
        define: "DEFINE TEMP-TABLE tt10 NO-UNDO LIKE tt1 USE-INDEX ia USE-INDEX ib AS PRIMARY.",
        table: "tt10",
        searches: &[
            (Some("a"), Some("ia")),
            (Some("b"), Some("ib")),
            (None, Some("ib")),
        ],
    },
    Case {
        name: "several USE-INDEX, none AS PRIMARY: the inherited primary stays",
        define: "DEFINE TEMP-TABLE tt11 NO-UNDO LIKE tt1 USE-INDEX ia USE-INDEX ib.",
        table: "tt11",
        searches: &[(Some("b"), Some("ib")), (None, Some("ia"))],
    },
    Case {
        name: "several USE-INDEX in another order: the inherited primary stays",
        define: "DEFINE TEMP-TABLE tt12 NO-UNDO LIKE tt1 USE-INDEX ib USE-INDEX ia.",
        table: "tt12",
        searches: &[(Some("b"), Some("ib")), (None, Some("ia"))],
    },
    Case {
        name: "LIKE a database table",
        define: "DEFINE TEMP-TABLE tt13 NO-UNDO LIKE Customer FIELD extra AS CHARACTER INDEX ix extra.",
        table: "tt13",
        searches: &[
            (Some("CustNum"), Some("CustNum")),
            (Some("CustName"), Some("CustName")),
            (Some("extra"), Some("ix")),
            (None, Some("CustNum")),
        ],
    },
    Case {
        name: "LIKE a database table with USE-INDEX",
        define: "DEFINE TEMP-TABLE tt14 NO-UNDO LIKE Customer USE-INDEX CustName.",
        table: "tt14",
        searches: &[
            (Some("CustName"), Some("CustName")),
            (None, Some("CustName")),
        ],
    },
    Case {
        name: "an unknown source names no index",
        define: "DEFINE TEMP-TABLE tt15 NO-UNDO LIKE Ghost FIELD x AS INTEGER INDEX ix x.",
        table: "tt15",
        searches: &[(Some("x"), None), (None, None)],
    },
    Case {
        name: "a chain from an unknown source names no index",
        define: "DEFINE TEMP-TABLE tt15 NO-UNDO LIKE Ghost FIELD x AS INTEGER INDEX ix x.\n\
            DEFINE TEMP-TABLE tt16 NO-UNDO LIKE tt15 FIELD y AS INTEGER.",
        table: "tt16",
        searches: &[(Some("x"), None), (None, None)],
    },
];

/// The index that `explain` names for each search of each case.
fn explain(case: &Case) -> Vec<(Expect, Option<String>)> {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.df"), SCHEMA).unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    let mut program = format!("{SOURCE}{}\n", case.define);
    let mut lines = Vec::new();
    for &(field, expected) in case.searches {
        let criteria = match field {
            Some(field) => format!(" WHERE {}.{field} = 1", case.table),
            None => String::new(),
        };
        program.push_str(&format!("FIND FIRST {}{criteria} NO-LOCK.\n", case.table));
        lines.push(((field, expected), program.lines().count() as u64));
    }
    std::fs::write(dir.path().join("main.p"), program).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir.path())
        .args(["explain", "main.p", "--json"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}: {}",
        case.name,
        String::from_utf8_lossy(&output.stderr)
    );
    let value: Value = serde_json::from_slice(&output.stdout).unwrap();
    let effects = value["units"][0]["effects"].as_array().unwrap();
    lines
        .into_iter()
        .map(|(search, line)| {
            let effect = effects
                .iter()
                .find(|effect| effect["line"] == line)
                .unwrap_or_else(|| panic!("{}: no effect at line {line}: {effects:?}", case.name));
            (search, effect["index"].as_str().map(str::to_string))
        })
        .collect()
}

#[test]
fn like_tables_name_the_indexes_the_compiler_gives_them() {
    let mut failures = Vec::new();
    for case in CASES {
        for ((field, expected), actual) in explain(case) {
            if actual.as_deref() != expected {
                failures.push(format!(
                    "{}: `{}` WHERE {field:?}: expected {expected:?}, got {actual:?}",
                    case.name, case.table
                ));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
