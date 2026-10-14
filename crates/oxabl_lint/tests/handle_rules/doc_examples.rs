//! The two `abl` blocks of each HD rule's module docs: the first is the bad
//! example and must fire; the second is the fixed one and must not, for its own
//! rule and for the leak rules HD001-HD004 (HD005 is advice that a fixed
//! example of another rule need not follow).

use oxabl_lint::{
    delete_not_in_finally, handle_not_deleted, memptr_not_freed, missing_widget_pool,
    persistent_procedure_leak,
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
    let leak_rules: [(&str, RuleFn); 4] = [
        ("HD001", handle_not_deleted::run),
        ("HD002", delete_not_in_finally::run),
        ("HD003", persistent_procedure_leak::run),
        ("HD004", memptr_not_freed::run),
    ];
    for (other, f) in std::iter::once((name, rule)).chain(leak_rules) {
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
fn hd001_examples() {
    examples(
        "HD001",
        handle_not_deleted::run,
        include_str!("../../src/rules/handle_not_deleted.rs"),
    );
}

#[test]
fn hd002_examples() {
    examples(
        "HD002",
        delete_not_in_finally::run,
        include_str!("../../src/rules/delete_not_in_finally.rs"),
    );
}

#[test]
fn hd003_examples() {
    examples(
        "HD003",
        persistent_procedure_leak::run,
        include_str!("../../src/rules/persistent_procedure_leak.rs"),
    );
}

#[test]
fn hd004_examples() {
    examples(
        "HD004",
        memptr_not_freed::run,
        include_str!("../../src/rules/memptr_not_freed.rs"),
    );
}

#[test]
fn hd005_examples() {
    examples(
        "HD005",
        missing_widget_pool::run,
        include_str!("../../src/rules/missing_widget_pool.rs"),
    );
}
