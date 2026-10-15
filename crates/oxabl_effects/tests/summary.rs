//! Transitive summaries over the call graph (section 8.6).

use oxabl_effects::summary::{self, Node, Partial, SummaryEffect, SummaryLock};
use oxabl_effects::{EffectUnknown, Op};
use oxabl_index::calls::{Callee, UnknownCall};
use oxabl_scope::LockMode;

fn effect(table: &str, op: Op, depth: u8) -> SummaryEffect {
    SummaryEffect {
        table: table.to_string(),
        op,
        lock: if op == Op::Read {
            SummaryLock::Mode(LockMode::NoLock)
        } else {
            SummaryLock::None
        },
        depth,
        via: None,
    }
}

fn node(effects: Vec<SummaryEffect>, calls: Vec<(Callee, u8)>) -> Node {
    Node {
        effects,
        calls,
        unknown: Vec::new(),
    }
}

#[test]
fn effects_flow_up_with_the_depth_of_the_call() {
    // 0 calls 1 per row; 1 calls 2 once; 2 updates Item.
    let nodes = vec![
        node(
            vec![effect("Order", Op::Read, 0)],
            vec![(Callee::Node(1), 1)],
        ),
        node(Vec::new(), vec![(Callee::Node(2), 0)]),
        node(vec![effect("Item", Op::Update, 0)], Vec::new()),
    ];
    let s = summary::summarize(&nodes, &summary::graph_of(&nodes));
    assert!(s[0].partial.is_empty());
    assert_eq!(s[0].effects.len(), 2);
    let item = s[0].effects.iter().find(|e| e.table == "Item").unwrap();
    assert_eq!((item.op, item.depth, item.via), (Op::Update, 1, Some(1)));
    assert!(s[0].writes());
    assert!(!s[0].known_no_writes());
    assert!(s[1].writes());
}

#[test]
fn a_cycle_reaches_a_fixed_point() {
    // 0 and 1 call each other per row; 1 reads Customer.
    let nodes = vec![
        node(Vec::new(), vec![(Callee::Node(1), 1)]),
        node(
            vec![effect("Customer", Op::Read, 0)],
            vec![(Callee::Node(0), 1)],
        ),
    ];
    let s = summary::summarize(&nodes, &summary::graph_of(&nodes));
    assert!(s[0].recursive && s[1].recursive);
    assert!(s[0].partial.is_empty(), "recursion alone is not partial");
    // Customer is one call from 0 and two from 1, then every two calls more,
    // up to MAX_DEPTH.
    let depths = |n: usize| s[n].effects.iter().map(|e| e.depth).collect::<Vec<u8>>();
    assert_eq!(depths(0), [1, 3, 4]);
    assert_eq!(depths(1), [0, 2, 4]);
    assert!(s[0].known_no_writes());
}

#[test]
fn unknown_callees_and_units_make_partial_summaries() {
    let mut opaque = node(Vec::new(), Vec::new());
    opaque.unknown = vec![EffectUnknown::Dynamic];
    let nodes = vec![
        node(
            Vec::new(),
            vec![
                (Callee::Unknown(UnknownCall::Handle), 0),
                (Callee::Node(1), 0),
            ],
        ),
        opaque,
    ];
    let s = summary::summarize(&nodes, &summary::graph_of(&nodes));
    assert_eq!(
        s[0].partial,
        [
            Partial::Unit(EffectUnknown::Dynamic),
            Partial::Call(UnknownCall::Handle)
        ]
    );
    assert!(!s[0].known_no_writes());
    assert_eq!(s[1].partial, [Partial::Unit(EffectUnknown::Dynamic)]);
}

#[test]
fn a_direct_effect_wins_over_the_same_inherited_one() {
    let nodes = vec![
        node(
            vec![effect("Item", Op::Read, 0)],
            vec![(Callee::Node(1), 0)],
        ),
        node(vec![effect("Item", Op::Read, 0)], Vec::new()),
    ];
    let s = summary::summarize(&nodes, &summary::graph_of(&nodes));
    assert_eq!(s[0].effects, [effect("Item", Op::Read, 0)]);
}
