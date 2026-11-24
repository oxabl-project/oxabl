//! `docs/overview.md` lists every opt-in rule of the built-in registry, with its
//! name and group.

use oxabl_lint::Registry;

#[test]
fn the_strict_rule_table_matches_the_registry() {
    let overview = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../docs/overview.md"),
    )
    .unwrap();
    let mut missing = Vec::new();
    for rule in Registry::builtin().rules() {
        let meta = rule.meta();
        if meta.group == "default" {
            continue;
        }
        let row = format!("| {} | `{}` | `{}` |", meta.code, meta.name, meta.group);
        if !overview.contains(&row) {
            missing.push(row);
        }
    }
    assert!(
        missing.is_empty(),
        "docs/overview.md lacks rows: {missing:#?}"
    );
}
