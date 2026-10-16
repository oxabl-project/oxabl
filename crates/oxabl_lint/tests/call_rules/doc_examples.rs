//! The `abl` blocks of CL003 and CL004's module docs: the first is the bad
//! example and must fire its rule; the second is the fixed one and must not.

use oxabl_lint::{signature_mismatch, unresolved_call};

use super::{RuleFn, Setup, run_with};

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
    let setup = Setup::default();
    let bad = run_with(rule, &blocks[0], &setup);
    assert!(
        !bad.is_empty(),
        "{name}: the bad example gives no finding\n{}",
        blocks[0]
    );
    for (other, f) in [
        ("CL003", signature_mismatch::run as RuleFn),
        ("CL004", unresolved_call::run as RuleFn),
    ] {
        let fixed = run_with(f, &blocks[1], &setup);
        assert!(
            fixed.is_empty(),
            "{name}: the fixed example gives {other} {:?}\n{}",
            fixed.iter().map(|d| &d.message).collect::<Vec<_>>(),
            blocks[1]
        );
    }
}

#[test]
fn cl003_examples() {
    examples(
        "CL003",
        signature_mismatch::run,
        include_str!("../../src/rules/signature_mismatch.rs"),
    );
}

#[test]
fn cl004_examples() {
    examples(
        "CL004",
        unresolved_call::run,
        include_str!("../../src/rules/unresolved_call.rs"),
    );
}
