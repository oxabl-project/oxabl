//! The work budget bounds the time of a unit with many locals and a long
//! loop; past it the unit is Unknown.

use std::time::{Duration, Instant};

use oxabl_flow::NodeKind;
use oxabl_nulls::{FileNulls, NullOptions, ValueState};

/// `n` date and `n` integer locals, and `k` statements that test and write
/// them, in one `REPEAT` when `looped`.
fn wide(n: usize, k: usize, looped: bool) -> String {
    let mut seed = 5u64;
    let mut next = |m: usize| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((seed >> 33) as usize) % m
    };
    let mut lines: Vec<String> = (0..n)
        .map(|i| format!("DEFINE VARIABLE d{i} AS DATE NO-UNDO INITIAL ?."))
        .chain((0..n).map(|i| format!("DEFINE VARIABLE i{i} AS INTEGER NO-UNDO.")))
        .collect();
    if looped {
        lines.push("REPEAT:".into());
    }
    for _ in 0..k {
        let (r, a, b) = (next(10), next(n), next(n));
        lines.push(match r {
            0..=2 => format!("IF d{a} <> ? AND i{b} <> ? THEN DO: i{b} = ?. d{a} = TODAY. END."),
            3..=5 => format!("IF i{a} = ? THEN i{a} = 1. ELSE d{b} = ?."),
            _ => format!("i{a} = i{b} + 1."),
        });
    }
    if looped {
        lines.push("END.".into());
    }
    lines.join("\n")
}

fn run(source: &str) -> (FileNulls, oxabl_flow::FileFlow, Duration) {
    let started = Instant::now();
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty());
    let flow =
        oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &Default::default());
    let nulls = FileNulls::default().with_values(
        source,
        &tokens,
        &program.statements,
        &flow,
        &NullOptions::default(),
    );
    (nulls, flow, started.elapsed())
}

#[test]
fn a_wide_long_loop_is_unknown_within_the_time_limit() {
    let source = wide(250, 3000, true);
    let (nulls, flow, took) = run(&source);
    let limit = if cfg!(debug_assertions) { 8 } else { 1 };
    assert!(took < Duration::from_secs(limit), "took {took:?}");
    let cfg = &flow.units()[0];
    assert_eq!(nulls.exhausted(), [cfg.id()]);
    assert!(nulls.values().is_empty());
    let node = cfg
        .nodes()
        .find(|(_, n)| n.kind == NodeKind::Stmt)
        .map(|(n, _)| n)
        .expect("a statement");
    assert_eq!(
        nulls.value_before(cfg.id(), node, "d0"),
        Some(ValueState::Unknown)
    );
}

#[test]
fn a_unit_within_the_limit_is_still_analysed() {
    let source = wide(20, 100, true);
    let (nulls, _, _) = run(&source);
    assert!(nulls.exhausted().is_empty());
    assert!(!nulls.values().is_empty());
}

/// `k` failed finds over `n` buffers of one table, with guards and reads, in
/// one `REPEAT`.
fn finds(n: usize, k: usize) -> String {
    let mut seed = 7u64;
    let mut next = |m: usize| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((seed >> 33) as usize) % m
    };
    let mut lines = vec!["DEFINE VARIABLE c AS CHARACTER NO-UNDO.".to_string()];
    lines.extend((0..n).map(|i| format!("DEFINE BUFFER b{i} FOR Customer.")));
    lines.push("REPEAT:".into());
    for _ in 0..k {
        let (r, a, b) = (next(10), next(n), next(n));
        lines.push(match r {
            0..=3 => format!("FIND FIRST b{a} WHERE b{a}.CustNum = b{b}.CustNum NO-LOCK NO-ERROR."),
            4..=6 => format!("IF AVAILABLE b{a} THEN c = b{a}.Name."),
            _ => format!("c = b{a}.Name."),
        });
    }
    lines.push("END.".into());
    lines.join("\n")
}

fn analyse_finds(source: &str) -> (oxabl_nulls::FileNulls, Duration) {
    let df = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/schema.df"),
    )
    .expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let started = Instant::now();
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let options = oxabl_scope::ScopeOptions::default();
    let flow = oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        source,
        &tokens,
        &flow,
        Some(&schema),
        &options,
    );
    let nulls = oxabl_nulls::analyze_file(source, &tokens, &flow, &scopes, &NullOptions::default());
    (nulls, started.elapsed())
}

#[test]
fn many_failed_finds_in_a_loop_are_unknown_within_the_time_limit() {
    let (nulls, took) = analyse_finds(&finds(150, 4000));
    let limit = if cfg!(debug_assertions) { 8 } else { 1 };
    assert!(took < Duration::from_secs(limit), "took {took:?}");
    assert_eq!(nulls.exhausted().len(), 1);
    assert!(nulls.missing().is_empty());
}

#[test]
fn a_few_failed_finds_in_a_loop_are_still_analysed() {
    let (nulls, _) = analyse_finds(&finds(10, 200));
    assert!(nulls.exhausted().is_empty());
    assert!(!nulls.missing().is_empty());
}

/// `k` tests, guards, refills and writes of nullable fields over `n` buffers
/// of one table, in one `REPEAT`.
fn fields(n: usize, k: usize) -> String {
    let mut seed = 11u64;
    let mut next = |m: usize| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((seed >> 33) as usize) % m
    };
    let names = [
        "DateA", "DateB", "DateC", "DateD", "DateE", "DateF", "Weight", "Memo",
    ];
    let mut lines: Vec<String> = (0..n)
        .map(|i| format!("DEFINE BUFFER s{i} FOR Shipment."))
        .collect();
    lines.push("REPEAT:".into());
    for _ in 0..k {
        let (r, a, f, g) = (next(10), next(n), names[next(8)], names[next(8)]);
        lines.push(match r {
            0..=2 => format!("IF s{a}.{f} <> ? AND s{a}.{g} > s{a}.{f} THEN s{a}.{g} = ?."),
            3..=4 => format!("IF s{a}.{f} = ? THEN NEXT."),
            5 => format!("FIND NEXT s{a} NO-LOCK NO-ERROR."),
            6 => format!("s{a}.{f} = s{a}.{g}."),
            _ => format!("MESSAGE s{a}.{f} + s{a}.{g}."),
        });
    }
    lines.push("END.".into());
    lines.join("\n")
}

fn analyse_fields(source: &str) -> (FileNulls, Duration) {
    let df = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/schema.df"),
    )
    .expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let started = Instant::now();
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let options = oxabl_scope::ScopeOptions::default();
    let flow = oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        source,
        &tokens,
        &flow,
        Some(&schema),
        &options,
    );
    let nulls = oxabl_nulls::analyze_fields(
        source,
        &tokens,
        &program.statements,
        &flow,
        &oxabl_nulls::FieldSources {
            scopes: &scopes,
            schema: &schema,
        },
        &NullOptions::default(),
    );
    (nulls, started.elapsed())
}

#[test]
fn many_fields_in_a_long_loop_are_unknown_within_the_time_limit() {
    let (nulls, took) = analyse_fields(&fields(120, 3000));
    let limit = if cfg!(debug_assertions) { 20 } else { 3 };
    assert!(took < Duration::from_secs(limit), "took {took:?}");
    assert_eq!(nulls.exhausted().len(), 1);
    assert!(nulls.field_values().is_empty());
}

#[test]
fn a_few_fields_in_a_loop_are_still_analysed() {
    let (nulls, _) = analyse_fields(&fields(3, 60));
    assert!(nulls.exhausted().is_empty());
    assert!(!nulls.field_values().is_empty());
}
