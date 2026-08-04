//! Diagnostic messages name ABL, never an internal identifier.
//!
//! LINT0004 once interpolated a `ResolvedType` with `{:?}`, so a buffer
//! mismatch rendered `Buffer(SymbolId(7))` at the user. The rule-level tests
//! pin the new wording for the specific pairs; this file pins the *property*,
//! because the same defect can reappear anywhere a type is interpolated into a
//! message.
//!
//! Two guards, deliberately different in kind. The first renders a battery of
//! sources through every rule and reads the messages that come out — it catches
//! a leak wherever it happens, but only for shapes the battery covers. The
//! second scans the rule sources for a debug interpolation outside their test
//! modules, which catches shapes no battery would think of.

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_index::BatchIndex;
use oxabl_lexer::tokenize;
use oxabl_lint::lint_file;
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::customer_schema};
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

/// Fragments that only ever appear in a message by way of a `{:?}` on an
/// internal type.
const LEAKS: &[&str] = &[
    "SymbolId(",
    "Primitive(",
    "ResolvedType",
    "NodeId(",
    "TableId(",
];

/// Sources chosen to drive a type into a message: a buffer, a schema field, a
/// class, an array, and the two primitive ladders that narrow.
const BATTERY: &[&str] = &[
    // Buffer into a character variable.
    "DEFINE BUFFER bCust FOR Customer.\nDEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = bCust.",
    // Schema field into the wrong primitive.
    "DEFINE BUFFER bCust FOR Customer.\nDEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = bCust.CustNum.",
    // Class into an integer.
    "CLASS pkg.Thing:\nEND CLASS.",
    "DEFINE VARIABLE t AS CLASS pkg.Thing NO-UNDO.\nDEFINE VARIABLE n AS INTEGER NO-UNDO.\nn = t.",
    // Array target.
    "DEFINE VARIABLE arr AS INTEGER EXTENT 3 NO-UNDO.\narr = \"hi\".",
    // Narrowing, which fires between two primitives and so would slip past a
    // `SymbolId(` scan.
    "DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\nDEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = lc.",
    "DEFINE VARIABLE dt AS DATETIME NO-UNDO.\nDEFINE VARIABLE d AS DATE NO-UNDO.\nd = dt.",
    // Logical/integer, the plainest mismatch there is.
    "DEFINE VARIABLE b AS LOGICAL NO-UNDO.\nb = 1.",
];

/// The cross-file sibling the last battery entry resolves against, so a
/// synthesized symbol's type also passes through a message.
const SIBLING: (&str, &str) = (
    "/src/pkg/Thing.cls",
    "CLASS pkg.Thing:\n  METHOD PUBLIC LONGCHAR buildPage():\n    RETURN \"\".\n  END METHOD.\nEND CLASS.",
);

fn messages(source: &str, schema: &Schema, with_index: bool) -> Vec<String> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let stmts = program.statements;

    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(SIBLING.0), SIBLING.1);
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);

    let base = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    let ctx = if with_index {
        base.with_index(&index)
    } else {
        base
    };
    let sem = analyze_file(&stmts, &ctx);
    let mut out: Vec<String> = sem.diagnostics.iter().map(|d| d.message.clone()).collect();
    out.extend(lint_file(&stmts, &sem, &ctx).into_iter().map(|d| d.message));
    out
}

#[test]
fn no_diagnostic_message_leaks_an_internal_id() {
    let loaded = customer_schema();
    let empty = Schema::empty();
    for source in BATTERY {
        for schema in [&loaded, &empty] {
            for with_index in [true, false] {
                for message in messages(source, schema, with_index) {
                    for leak in LEAKS {
                        assert!(
                            !message.contains(leak),
                            "message leaks `{leak}`: {message}\nfor source:\n{source}"
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn the_battery_actually_produces_messages() {
    // A guard that never sees a message proves nothing. Kept separate so a
    // battery that silently stops firing is a failure rather than a pass.
    let loaded = customer_schema();
    let total: usize = BATTERY
        .iter()
        .map(|s| messages(s, &loaded, true).len())
        .sum();
    assert!(
        total >= BATTERY.len() - 2,
        "the battery produced only {total} messages; it is no longer exercising the renderer"
    );
}

#[test]
fn no_rule_debug_formats_a_value_into_a_message() {
    // Rule code above its own `#[cfg(test)]` module has no legitimate reason to
    // reach for `{:?}` — everything it formats ends up in front of a user. Test
    // modules use it freely in assertion messages, so the scan stops there.
    let rules = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/rules");
    let mut scanned = 0;
    for entry in std::fs::read_dir(&rules).expect("rules directory") {
        let path = entry.expect("dir entry").path();
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let text = std::fs::read_to_string(&path).expect("read rule source");
        let production = match text.find("#[cfg(test)]") {
            Some(i) => &text[..i],
            None => &text[..],
        };
        scanned += 1;
        for (n, line) in production.lines().enumerate() {
            // `{:?}` inside a doc comment is prose about the defect, not a
            // format string.
            if line.trim_start().starts_with("//") {
                continue;
            }
            assert!(
                !line.contains("{:?}"),
                "{}:{} debug-formats a value in rule code: {line}",
                path.display(),
                n + 1
            );
        }
    }
    assert!(scanned > 1, "the scan found no rule sources");
}
