//! The rules through the registry: one analysis, the group switches and the
//! metadata.

use oxabl_common::{FileId, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_lint::{Registry, Stability, groups};
use oxabl_parser::Parser;
use oxabl_semantic::{AnalysisContext, analyze_file};

use super::schema;

const SRC: &str = "\
FIND FIRST Customer.
DO TRANSACTION:
    FIND FIRST Item EXCLUSIVE-LOCK.
    Item.Price = 1.
END.
FOR EACH Order NO-LOCK:
    FIND CURRENT Order EXCLUSIVE-LOCK.
END.
MESSAGE \"done\".
";

const TX_SRC: &str = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
FOR EACH Customer NO-LOCK:
END.
FIND FIRST Customer NO-LOCK.
DO TRANSACTION:
    UPDATE c.
    RUN other.p.
END.
CREATE Item.
PROCEDURE p:
    REPEAT:
        CREATE Order.
        LEAVE.
    END.
END PROCEDURE.
";

fn codes(map: LintSeverityMap) -> Vec<&'static str> {
    codes_of(SRC, map, "LK")
}

fn codes_of(src: &str, map: LintSeverityMap, prefix: &str) -> Vec<&'static str> {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    let schema = schema();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, src, &schema).with_lint_severities(map);
    let sem = analyze_file(&program.statements, &ctx);
    Registry::builtin()
        .run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .filter(|c| c.starts_with(prefix))
        .collect()
}

#[test]
fn nothing_runs_until_the_group_is_enabled() {
    assert!(codes(LintSeverityMap::new()).is_empty());
}

#[test]
fn the_group_enables_the_lock_rules() {
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_LOCKS);
    assert_eq!(codes(map), ["LK001", "LK002", "LK003"]);
}

#[test]
fn the_group_enables_the_transaction_rules() {
    assert!(codes_of(TX_SRC, LintSeverityMap::new(), "TX").is_empty());
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_TRANSACTIONS);
    assert_eq!(
        codes_of(TX_SRC, map, "TX"),
        ["TX001", "TX002", "TX003", "TX004", "TX005"]
    );
}

#[test]
fn one_rule_can_be_enabled_alone() {
    let mut map = LintSeverityMap::new();
    map.set("lock-upgrade-in-loop", Some(Severity::Error));
    assert_eq!(codes(map), ["LK003"]);
}

#[test]
fn the_rules_are_preview_warnings_routed_like_the_handle_rules() {
    let registry = Registry::builtin();
    let found: Vec<_> = registry
        .rules()
        .map(|r| *r.meta())
        .filter(|m| m.group == groups::STRICT_LOCKS || m.code < "TX006" && m.code > "TX000")
        .collect();
    let codes: Vec<_> = found.iter().map(|m| m.code).collect();
    assert_eq!(
        codes,
        [
            "LK001", "LK002", "LK003", "TX001", "TX002", "TX003", "TX004", "TX005"
        ]
    );
    for m in found {
        assert_eq!(m.default_severity, Severity::Warning, "{}", m.code);
        assert_eq!(m.stability, Stability::Preview, "{}", m.code);
        assert!(m.context_free, "{}", m.code);
        assert!(m.use_site_in_includer, "{}", m.code);
        assert!(!m.requires_compilation_unit, "{}", m.code);
    }
}
