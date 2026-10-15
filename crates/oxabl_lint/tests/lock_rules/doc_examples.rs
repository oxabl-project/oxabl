//! The two `abl` blocks of each LK rule's module docs: the first is the bad
//! example and must fire; the second is the fixed one and must not, for any
//! of the three rules.

use oxabl_lint::{default_lock, lock_outlives_transaction, lock_upgrade_in_loop};

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
    let rules: [(&str, RuleFn); 3] = [
        ("LK001", default_lock::run),
        ("LK002", lock_outlives_transaction::run),
        ("LK003", lock_upgrade_in_loop::run),
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
