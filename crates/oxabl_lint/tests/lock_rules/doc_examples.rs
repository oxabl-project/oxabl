//! The two `abl` blocks of each LK, TX, DB and NL rule's module docs: the first is the
//! bad example and must fire; the second is the fixed one and must not, for
//! any of the ten rules.

use oxabl_lint::{
    default_lock, field_after_failed_find, find_in_loop, implicit_transaction,
    lock_outlives_transaction, lock_upgrade_in_loop, procedure_wide_transaction, scope_widened,
    transaction_spans_call, transaction_spans_ui,
};

use super::{RuleFn, check};

/// The `abl` blocks of a module's `//!` docs.
fn abl_blocks(module: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut current: Option<String> = None;
    for line in module.lines() {
        let Some(doc) = line.strip_prefix("//!") else {
            continue;
        };
        let doc = doc.strip_prefix(' ').unwrap_or(doc);
        match (&mut current, doc.trim_end()) {
            (None, "```abl") => current = Some(String::new()),
            (Some(_), "```") => blocks.extend(current.take()),
            (Some(code), _) => {
                code.push_str(doc);
                code.push('\n');
            }
            (None, _) => {}
        }
    }
    blocks
}

fn examples(name: &str, rule: RuleFn, module: &str) {
    let blocks = abl_blocks(module);
    assert_eq!(
        blocks.len(),
        2,
        "{name}: expected a bad and a fixed example"
    );
    let bad = check(rule, &blocks[0]);
    assert!(
        !bad.is_empty(),
        "{name}: the bad example gives no finding\n{}",
        blocks[0]
    );
    let rules: [(&str, RuleFn); 13] = [
        ("LK001", default_lock::run),
        ("LK002", lock_outlives_transaction::run),
        ("LK003", lock_upgrade_in_loop::run),
        ("TX001", implicit_transaction::run),
        ("TX002", procedure_wide_transaction::run),
        ("TX003", transaction_spans_ui::run),
        ("TX004", transaction_spans_call::run),
        ("TX005", scope_widened::run),
        ("DB001", find_in_loop::run),
        ("NL001", oxabl_lint::unknown_in_comparison::run),
        ("NL002", oxabl_lint::unknown_propagation::run),
        ("NL003", field_after_failed_find::run),
        ("NL004", oxabl_lint::nullable_db_field::run),
    ];
    for (other, f) in rules {
        let fixed = check(f, &blocks[1]);
        assert!(
            fixed.is_empty(),
            "{name}: the fixed example gives {other} {:?}\n{}",
            fixed.iter().map(|d| &d.message).collect::<Vec<_>>(),
            blocks[1]
        );
    }
}

#[test]
fn lk001_examples() {
    examples(
        "LK001",
        default_lock::run,
        include_str!("../../src/rules/default_lock.rs"),
    );
}

#[test]
fn lk002_examples() {
    examples(
        "LK002",
        lock_outlives_transaction::run,
        include_str!("../../src/rules/lock_outlives_transaction.rs"),
    );
}

#[test]
fn lk003_examples() {
    examples(
        "LK003",
        lock_upgrade_in_loop::run,
        include_str!("../../src/rules/lock_upgrade_in_loop.rs"),
    );
}

#[test]
fn tx001_examples() {
    examples(
        "TX001",
        implicit_transaction::run,
        include_str!("../../src/rules/implicit_transaction.rs"),
    );
}

#[test]
fn tx002_examples() {
    examples(
        "TX002",
        procedure_wide_transaction::run,
        include_str!("../../src/rules/procedure_wide_transaction.rs"),
    );
}

#[test]
fn tx003_examples() {
    examples(
        "TX003",
        transaction_spans_ui::run,
        include_str!("../../src/rules/transaction_spans_ui.rs"),
    );
}

#[test]
fn tx004_examples() {
    examples(
        "TX004",
        transaction_spans_call::run,
        include_str!("../../src/rules/transaction_spans_call.rs"),
    );
}

#[test]
fn tx005_examples() {
    examples(
        "TX005",
        scope_widened::run,
        include_str!("../../src/rules/scope_widened.rs"),
    );
}

#[test]
fn db001_examples() {
    examples(
        "DB001",
        find_in_loop::run,
        include_str!("../../src/rules/find_in_loop.rs"),
    );
}

#[test]
fn nl003_examples() {
    examples(
        "NL003",
        field_after_failed_find::run,
        include_str!("../../src/rules/field_after_failed_find.rs"),
    );
}

#[test]
fn nl001_examples() {
    examples(
        "NL001",
        oxabl_lint::unknown_in_comparison::run,
        include_str!("../../src/rules/unknown_in_comparison.rs"),
    );
}

#[test]
fn nl002_examples() {
    examples(
        "NL002",
        oxabl_lint::unknown_propagation::run,
        include_str!("../../src/rules/unknown_propagation.rs"),
    );
}

#[test]
fn nl004_examples() {
    examples(
        "NL004",
        oxabl_lint::nullable_db_field::run,
        include_str!("../../src/rules/nullable_db_field.rs"),
    );
}
