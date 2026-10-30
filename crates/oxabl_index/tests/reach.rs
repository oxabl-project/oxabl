//! The entry-point search over the routines of many files.

use oxabl_index::calls::{Callee, UnknownCall};
use oxabl_index::reach::{Parent, ReachGraph, Seed};

/// 0 -a-> 1 -b-> 2, 0 -c-> 3 (unknown from 3), 4 alone.
fn graph() -> ReachGraph {
    let mut g = ReachGraph::new(5);
    g.add(0, 10, Callee::Node(1));
    g.add(1, 11, Callee::Node(2));
    g.add(0, 12, Callee::Node(3));
    g.add(3, 13, Callee::Unknown(UnknownCall::Dynamic));
    g
}

#[test]
fn every_routine_an_entry_calls_is_reached_with_the_entries_that_reach_it() {
    let g = graph();
    let seeds = [
        Seed {
            node: 0,
            entry: 7,
            gated: false,
        },
        Seed {
            node: 2,
            entry: 8,
            gated: false,
        },
    ];
    let r = g.reach(&seeds, 4);
    assert_eq!(r.entries(0).ids(), [7]);
    assert_eq!(r.entries(2).ids(), [7, 8]);
    assert!(r.entries(4).ids().is_empty());
    assert!(r.is_reached(3));
    assert!(!r.is_reached(4));
    assert_eq!(r.unknown(), [UnknownCall::Dynamic]);
}

#[test]
fn the_entry_list_is_capped_and_says_so() {
    let g = graph();
    let seeds: Vec<Seed> = (0..6)
        .map(|e| Seed {
            node: 0,
            entry: e,
            gated: false,
        })
        .collect();
    let r = g.reach(&seeds, 4);
    assert_eq!(r.entries(2).ids().len(), 4);
    assert!(r.entries(2).more());
    assert!(!g.reach(&seeds[..2], 4).entries(2).more());
}

#[test]
fn a_gate_before_a_call_holds_for_everything_after_it() {
    let g = graph();
    let seeds = [Seed {
        node: 0,
        entry: 0,
        gated: false,
    }];
    // Node 1 passes a gate before its call at site 11.
    let s = g.gated(&seeds, |node, site| node == 1 && site == 11);
    assert!(s.ungated(1).is_some());
    assert!(s.ungated(2).is_none(), "every path to 2 passes the gate");
    assert!(s.gated(2).is_some());
    assert!(s.ungated(3).is_some());
    assert!(s.ungated(4).is_none() && s.gated(4).is_none());
}

#[test]
fn an_entry_that_is_authorised_starts_gated() {
    let g = graph();
    let s = g.gated(
        &[Seed {
            node: 0,
            entry: 0,
            gated: true,
        }],
        |_, _| false,
    );
    assert!(s.ungated(2).is_none());
    assert!(s.gated(2).is_some());
}

#[test]
fn one_ungated_path_is_enough() {
    // 0 -> 1 -> 2 and 0 -> 2 directly; only the path through 1 is gated.
    let mut g = ReachGraph::new(3);
    g.add(0, 1, Callee::Node(1));
    g.add(1, 2, Callee::Node(2));
    g.add(0, 3, Callee::Node(2));
    let s = g.gated(
        &[Seed {
            node: 0,
            entry: 5,
            gated: false,
        }],
        |node, _| node == 1,
    );
    let path = s.path(2, false).expect("an ungated path");
    assert_eq!(path.entry, 5);
    assert_eq!(path.calls, [(0, 3)], "the direct call, as (caller, site)");
}

#[test]
fn a_path_names_its_calls_from_the_entry() {
    let g = graph();
    let s = g.gated(
        &[Seed {
            node: 0,
            entry: 9,
            gated: false,
        }],
        |_, _| false,
    );
    let path = s.path(2, false).unwrap();
    assert_eq!(path.entry, 9);
    assert_eq!(path.calls, [(0, 10), (1, 11)]);
    assert!(matches!(s.ungated(0), Some(Parent::Entry(9))));
}

#[test]
fn a_cycle_ends() {
    let mut g = ReachGraph::new(2);
    g.add(0, 1, Callee::Node(1));
    g.add(1, 2, Callee::Node(0));
    let seeds = [Seed {
        node: 0,
        entry: 0,
        gated: false,
    }];
    let r = g.reach(&seeds, 4);
    assert!(r.is_reached(1));
    let s = g.gated(&seeds, |_, _| false);
    assert!(s.ungated(1).is_some());
}
