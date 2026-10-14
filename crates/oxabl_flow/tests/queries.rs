//! The query layer: paths, block exits, dominators and the dataflow solver,
//! asked the questions the strict analyses will ask.

use oxabl_ast::{ExpressionKind, StatementKind};
use oxabl_flow::dataflow::{Analysis, BitSet, Direction, solve};
use oxabl_flow::{
    Action, BlockId, BlockKind, Cfg, Condition, EdgeKind, ErrorDirective, ExitKind, FileFlow,
    FlowOptions, ForQualifier, NodeIx, NodeKind, OpaqueReason, Origin, RetryBehavior, UnitKind,
};

struct Parsed {
    source: String,
    program: oxabl_parser::Program,
}

fn parse(source: &str) -> Parsed {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    Parsed {
        source: source.to_string(),
        program,
    }
}

impl Parsed {
    fn flow(&self) -> FileFlow {
        oxabl_flow::build_file(&self.program.statements, &self.source)
    }

    /// Statement and header nodes of `cfg` whose text starts with `prefix`.
    fn nodes(&self, cfg: &Cfg, prefix: &str) -> Vec<NodeIx> {
        let found: Vec<NodeIx> = cfg
            .nodes()
            .filter(|(_, n)| matches!(n.kind, NodeKind::Stmt | NodeKind::Head))
            .filter(|(_, n)| {
                self.source[n.span.start as usize..n.span.end as usize].starts_with(prefix)
            })
            .map(|(i, _)| i)
            .collect();
        assert!(!found.is_empty(), "no node starts with {prefix:?}");
        found
    }

    fn node(&self, cfg: &Cfg, prefix: &str) -> NodeIx {
        let found = self.nodes(cfg, prefix);
        assert_eq!(found.len(), 1, "{prefix:?} has {} nodes", found.len());
        found[0]
    }
}

fn is_exit(cfg: &Cfg) -> impl Fn(NodeIx) -> bool + '_ {
    |n| matches!(cfg.node(n).kind, NodeKind::Exit(_))
}

const LEAKY: &str = "\
PROCEDURE leaky:
    DEFINE VARIABLE h AS HANDLE NO-UNDO.
    CREATE QUERY h.
    RUN other.p.
    DELETE OBJECT h.
END PROCEDURE.
";

const SAFE: &str = "\
PROCEDURE safe:
    DEFINE VARIABLE h AS HANDLE NO-UNDO.
    CREATE QUERY h.
    IF NOT VALID-HANDLE(h) THEN RETURN.
    RUN other.p.
    FINALLY:
        DELETE OBJECT h.
    END FINALLY.
END PROCEDURE.
";

#[test]
fn delete_on_the_success_path_only_leaks_on_the_error_path() {
    let p = parse(LEAKY);
    let flow = p.flow();
    let cfg = &flow.units()[1];
    let create = p.node(cfg, "CREATE QUERY");
    let delete = p.node(cfg, "DELETE OBJECT");

    // A path from the create to an exit that skips the delete (the CREATE
    // itself may raise, so the shortest one starts there)...
    assert!(
        cfg.find_path(create, is_exit(cfg), |n| n == delete)
            .is_some()
    );
    // ...and one from the RUN, through its error edge.
    let run = p.node(cfg, "RUN other.p");
    let path = cfg
        .find_path(run, is_exit(cfg), |n| n == delete)
        .expect("the RUN can raise past the delete");
    let first = cfg.successors(run).find(|e| e.to == path[1]).unwrap();
    assert_eq!(first.kind, EdgeKind::Raise(Condition::Error));
    assert!(!cfg.runs_on_every_exit(BlockId::ROUTINE, &[delete]));
    assert!(!cfg.post_dominators().dominates(delete, create));
}

#[test]
fn finally_runs_on_every_exit_including_errors() {
    let p = parse(SAFE);
    let flow = p.flow();
    let cfg = &flow.units()[1];
    let create = p.node(cfg, "CREATE QUERY");
    // One copy per way out: the RETURN, the normal end, the error leave.
    let deletes = p.nodes(cfg, "DELETE OBJECT");
    assert_eq!(deletes.len(), 3);
    assert!(
        cfg.find_path(create, is_exit(cfg), |n| deletes.contains(&n))
            .is_none()
    );
    assert!(cfg.runs_on_every_exit(BlockId::ROUTINE, &deletes));
    // The FINALLY statement itself maps to its block.
    let finally = cfg.block(BlockId::ROUTINE).finally.expect("finally block");
    assert_eq!(cfg.block(finally).kind, BlockKind::Finally);
}

#[test]
fn finally_runs_when_a_nested_block_throws() {
    let p = parse(
        "\
BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
blk:
DO:
    CREATE QUERY h.
    FOR EACH Customer NO-LOCK:
        IF Customer.CustNum = 1 THEN LEAVE blk.
        RUN other.p.
    END.
    FINALLY:
        DELETE OBJECT h.
    END FINALLY.
END.
",
    );
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let blk = cfg.block_of(p.program.statements[2].id).or_else(|| {
        cfg.blocks()
            .find(|(_, b)| b.label.as_deref() == Some("blk"))
            .map(|(id, _)| id)
    });
    let blk = blk.expect("labelled block");
    let deletes = p.nodes(cfg, "DELETE OBJECT");
    assert!(cfg.runs_on_every_exit(blk, &deletes));
    // The block is left normally, by LEAVE, and by an error that the FOR
    // block throws on; each exit goes through a FINALLY copy.
    let kinds: Vec<EdgeKind> = cfg.block_exits(blk).iter().map(|e| e.kind).collect();
    assert!(kinds.contains(&EdgeKind::Normal));
    assert!(kinds.contains(&EdgeKind::Leave));
    assert!(kinds.contains(&EdgeKind::Throw));
    // Every exit leaves from the FINALLY: its end, or the DELETE raising.
    for e in cfg.block_exits(blk) {
        let from = cfg.node(e.from);
        assert_eq!(cfg.block(from.block).kind, BlockKind::Finally);
    }
    // Unhandled at the routine too, so the error leaves the unit.
    assert!(cfg.exit(ExitKind::Error).is_some());
}

/// The reproducer of the guard-clause false positive in LINT0005.
const GUARD: &str = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-amount AS DECIMAL NO-UNDO.
IF NOT valid THEN DO:
    MESSAGE \"bad input\".
    RETURN.
END.
ELSE DO:
    v-amount = 1.
END.
DISPLAY v-amount.
";

#[test]
fn the_only_branch_that_continues_dominates_the_read() {
    let p = parse(GUARD);
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let write = p.node(cfg, "v-amount = 1");
    let read = p.node(cfg, "DISPLAY");
    let message = p.node(cfg, "MESSAGE");
    let dom = cfg.dominators();
    assert!(dom.dominates(write, read));
    assert!(!dom.dominates(message, read));
    assert!(dom.dominates(cfg.entry(), read));
}

/// Definite assignment of variables named in plain `x = ...` statements.
struct Assigned<'a> {
    parsed: &'a Parsed,
    names: Vec<String>,
}

impl Assigned<'_> {
    fn written(&self, cfg: &Cfg, n: NodeIx) -> Option<usize> {
        let node = cfg.node(n);
        if node.kind != NodeKind::Stmt {
            return None;
        }
        let text = &self.parsed.source[node.span.start as usize..node.span.end as usize];
        let (lhs, _) = text.split_once('=')?;
        self.names.iter().position(|v| v == lhs.trim())
    }
}

impl Analysis for Assigned<'_> {
    type Fact = BitSet;

    fn direction(&self) -> Direction {
        Direction::Forward
    }

    fn initial(&self, _: &Cfg) -> BitSet {
        BitSet::new_full(self.names.len())
    }

    fn boundary(&self, _: &Cfg) -> BitSet {
        BitSet::new_empty(self.names.len())
    }

    fn join(&self, acc: &mut BitSet, other: &BitSet) {
        acc.intersect_with(other);
    }

    fn transfer(&self, cfg: &Cfg, node: NodeIx, fact: &mut BitSet) {
        if let Some(v) = self.written(cfg, node) {
            fact.insert(v);
        }
    }

    fn transfer_edge(&self, cfg: &Cfg, node: NodeIx, edge: &oxabl_flow::Edge, fact: &mut BitSet) {
        // A statement that raised did not complete its assignment.
        if !matches!(edge.kind, EdgeKind::Raise(_)) {
            self.transfer(cfg, node, fact);
        }
    }
}

#[test]
fn definite_assignment_follows_the_guard_clause() {
    let p = parse(GUARD);
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let a = Assigned {
        parsed: &p,
        names: vec!["v-amount".into()],
    };
    let solved = solve(cfg, &a);
    let read = p.node(cfg, "DISPLAY");
    assert!(solved.before(read).contains(0));

    // Without the RETURN the read may see the default value.
    let p = parse(&GUARD.replace("    RETURN.\n", ""));
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let a = Assigned {
        parsed: &p,
        names: vec!["v-amount".into()],
    };
    let solved = solve(cfg, &a);
    assert!(!solved.before(p.node(cfg, "DISPLAY")).contains(0));
}

#[test]
fn an_assignment_that_raises_is_not_complete_on_the_error_path() {
    let p = parse(
        "\
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DO ON ERROR UNDO, LEAVE:
    x = INTEGER(\"a\").
END.
MESSAGE x.
",
    );
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let a = Assigned {
        parsed: &p,
        names: vec!["x".into()],
    };
    let solved = solve(cfg, &a);
    let write = p.node(cfg, "x = INTEGER");
    assert!(solved.after(write).contains(0));
    assert!(!solved.before(p.node(cfg, "MESSAGE")).contains(0));
}

/// Backward may-analysis: can a `RUN` still happen from here?
struct RunAhead<'a>(&'a Parsed);

impl Analysis for RunAhead<'_> {
    type Fact = bool;

    fn direction(&self) -> Direction {
        Direction::Backward
    }

    fn initial(&self, _: &Cfg) -> bool {
        false
    }

    fn boundary(&self, _: &Cfg) -> bool {
        false
    }

    fn join(&self, acc: &mut bool, other: &bool) {
        *acc |= *other;
    }

    fn transfer(&self, cfg: &Cfg, node: NodeIx, fact: &mut bool) {
        let n = cfg.node(node);
        if n.kind == NodeKind::Stmt && self.0.source[n.span.start as usize..].starts_with("RUN") {
            *fact = true;
        }
    }
}

#[test]
fn backward_analysis_sees_through_loops() {
    let p = parse(
        "\
DEFINE VARIABLE i AS INTEGER NO-UNDO.
i = 0.
DO WHILE i < 3:
    i = i + 1.
    RUN step.p.
END.
MESSAGE i.
",
    );
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let solved = solve(cfg, &RunAhead(&p));
    assert!(*solved.before(p.node(cfg, "i = 0")));
    assert!(*solved.before(p.node(cfg, "i = i + 1")));
    assert!(!*solved.before(p.node(cfg, "MESSAGE")));
}

#[test]
fn block_properties_come_from_the_header() {
    let p = parse(
        "\
main-loop:
FOR FIRST Customer EXCLUSIVE-LOCK TRANSACTION ON ERROR UNDO main-loop, RETRY main-loop:
END.
REPEAT ON ENDKEY UNDO, LEAVE ON STOP UNDO, RETURN ERROR ON QUIT UNDO, NEXT:
    LEAVE.
END.
DO ON ERROR UNDO, THROW:
END.
DO TRANSACTION:
END.
DO:
END.
",
    );
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let blocks: Vec<_> = cfg.blocks().map(|(_, b)| b).collect();

    let f = blocks[1];
    assert_eq!(f.kind, BlockKind::For(ForQualifier::First));
    assert_eq!(f.label.as_deref(), Some("main-loop"));
    assert!(f.iterating && f.transaction);
    let on_error = f.handling(Condition::Error).unwrap();
    assert_eq!(on_error.undo.as_deref(), Some("main-loop"));
    assert_eq!(on_error.action, Action::Retry(Some("main-loop".into())));
    assert_eq!(on_error.origin, Origin::Phrase);
    assert_eq!(f.retry_becomes(), RetryBehavior::Next);

    let r = blocks[2];
    assert_eq!(r.kind, BlockKind::Repeat);
    assert_eq!(
        r.handling(Condition::Stop).unwrap().action,
        Action::Return(oxabl_flow::ReturnKind::Error)
    );
    assert_eq!(
        r.handling(Condition::Quit).unwrap().action,
        Action::Next(None)
    );
    assert_eq!(
        r.handling(Condition::Error).unwrap().origin,
        Origin::Default
    );

    assert_eq!(
        blocks[3].handling(Condition::Error).unwrap().action,
        Action::Throw
    );
    let tx = blocks[4];
    assert!(tx.transaction && !tx.iterating);
    assert_eq!(tx.retry_becomes(), RetryBehavior::Leave);
    assert!(blocks[5].handling(Condition::Error).is_none());
    assert!(blocks[5].handling(Condition::Endkey).is_none());
}

#[test]
fn directives_change_the_default_and_the_option_supplies_one() {
    let source = "\
PROCEDURE p:
    FOR EACH Customer NO-LOCK:
    END.
END PROCEDURE.
";
    let p = parse(source);
    let plain = p.flow();
    let routine = &plain.units()[1];
    assert_eq!(
        routine
            .block(BlockId::ROUTINE)
            .handling(Condition::Error)
            .unwrap()
            .action,
        Action::Retry(None)
    );

    let opts = FlowOptions {
        default_directive: Some(ErrorDirective::RoutineLevel),
    };
    let flow = oxabl_flow::build_file_with(&p.program.statements, &p.source, &opts);
    assert_eq!(flow.directive(), Some(ErrorDirective::RoutineLevel));
    let cfg = &flow.units()[1];
    let routine_error = cfg
        .block(BlockId::ROUTINE)
        .handling(Condition::Error)
        .unwrap();
    assert_eq!(routine_error.action, Action::Throw);
    assert_eq!(routine_error.origin, Origin::RoutineLevel);
    let for_error = cfg
        .block(BlockId::from_index(1))
        .handling(Condition::Error)
        .unwrap();
    assert_eq!(for_error.origin, Origin::Default);

    // In the file, BLOCK-LEVEL beats the ROUTINE-LEVEL option.
    let p = parse(&format!("BLOCK-LEVEL ON ERROR UNDO, THROW.\n{source}"));
    let flow = oxabl_flow::build_file_with(&p.program.statements, &p.source, &opts);
    assert_eq!(flow.directive(), Some(ErrorDirective::BlockLevel));
    let cfg = &flow.units()[1];
    let for_error = cfg
        .block(BlockId::from_index(1))
        .handling(Condition::Error)
        .unwrap();
    assert_eq!(for_error.origin, Origin::BlockLevel);
}

#[test]
fn units_and_their_statements() {
    let p = parse(
        "\
DEFINE VARIABLE i AS INTEGER NO-UNDO.
ON CHOOSE OF i DO:
    i = 1.
END.
PROCEDURE p:
    i = 2.
    RETURN.
    i = 3.
END PROCEDURE.
",
    );
    let flow = p.flow();
    let kinds: Vec<UnitKind> = flow.units().iter().map(|c| c.unit().kind).collect();
    assert_eq!(
        kinds,
        [UnitKind::Main, UnitKind::Trigger, UnitKind::Procedure]
    );
    let proc_stmt = p.program.statements[2].id;
    assert_eq!(flow.units_defined_by(proc_stmt).count(), 1);
    let assign = match &p.program.statements[2].kind {
        StatementKind::Procedure { body, .. } => body[0].id,
        _ => unreachable!(),
    };
    let (cfg, nodes) = flow.find_stmt(assign).expect("statement has a node");
    assert_eq!(cfg.unit().kind, UnitKind::Procedure);
    assert_eq!(nodes.len(), 1);
    let dead = cfg.unreachable_stmts();
    assert_eq!(dead, p.nodes(cfg, "i = 3"));
    // The procedure body is not part of the main block.
    assert!(flow.units()[0].nodes_of(assign).is_empty());
}

#[test]
fn skipped_statements_are_opaque_but_still_flow() {
    let p = parse(
        "\
DEFINE VARIABLE i AS INTEGER NO-UNDO.
PUT UNFORMATTED i SKIP.
LEAVE nowhere.
",
    );
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let reasons: Vec<OpaqueReason> = cfg.opaque_nodes().map(|(_, r)| r).collect();
    assert_eq!(
        reasons,
        [
            OpaqueReason::SkippedStatement,
            OpaqueReason::UnresolvedLabel
        ]
    );
    let put = p.node(cfg, "PUT");
    let kinds: Vec<EdgeKind> = cfg.successors(put).map(|e| e.kind).collect();
    assert_eq!(kinds, [EdgeKind::Raise(Condition::Error), EdgeKind::Normal]);
    assert!(cfg.is_opaque());
}

#[test]
fn nodes_line_up_with_the_ast() {
    // A smoke test that node ids line up with the AST: every top-level
    // executable statement of the main block has exactly one node.
    let p = parse(GUARD);
    let flow = p.flow();
    let cfg = &flow.units()[0];
    for s in &p.program.statements {
        let nodes = cfg.nodes_of(s.id);
        assert_eq!(nodes.len(), 1, "{:?}", s.kind);
        let span = cfg.node(nodes[0]).span;
        assert_eq!(span.start, s.span.start);
        // An IF node covers its test only.
        if !matches!(s.kind, StatementKind::If { .. }) {
            assert_eq!(span, s.span);
        }
        if let StatementKind::Display { items, .. } = &s.kind {
            assert!(matches!(
                items[0].expression.kind,
                ExpressionKind::Identifier(_)
            ));
        }
    }
}

/// The target of the only edge out of `n`, and its kind.
fn only_edge(cfg: &Cfg, n: NodeIx) -> (NodeIx, EdgeKind) {
    let edges: Vec<_> = cfg.successors(n).collect();
    assert_eq!(edges.len(), 1, "{:?}", edges);
    (edges[0].to, edges[0].kind)
}

fn block_of_head(p: &Parsed, cfg: &Cfg, prefix: &str) -> BlockId {
    cfg.node(p.node(cfg, prefix)).block
}

/// The handler a `throw` edge of `n` reaches.
fn throw_handler(cfg: &Cfg, n: NodeIx) -> NodeIx {
    let (to, kind) = only_edge(cfg, n);
    assert_eq!(kind, EdgeKind::Throw);
    assert_eq!(cfg.node(to).kind, NodeKind::Handler(Condition::Error));
    to
}

fn hands_to(cfg: &Cfg, handler: NodeIx, catch_head: NodeIx) -> bool {
    cfg.successors(handler)
        .any(|e| e.kind == EdgeKind::Catch && e.to == catch_head)
}

#[test]
fn an_unlabelled_undo_in_a_catch_undoes_the_block_the_catch_belongs_to() {
    let p = parse(include_str!("fixtures/undo_catch.p"));
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let with_catch: Vec<BlockId> = cfg
        .blocks()
        .filter(|(_, b)| b.kind == BlockKind::Do && b.catches.len() == 1)
        .map(|(i, _)| i)
        .collect();
    let (do1, do3) = (with_catch[0], with_catch[1]);
    let each = cfg
        .blocks()
        .find(|(_, b)| b.kind == BlockKind::For(ForQualifier::Each))
        .expect("FOR EACH")
        .0;
    // Where the single edge out of a node goes, as (kind, node kind).
    let goes = |n: NodeIx| {
        let (to, kind) = only_edge(cfg, n);
        (kind, cfg.node(to).kind)
    };

    // `UNDO, LEAVE` in the CATCH of a DO inside a REPEAT leaves the DO, not
    // the REPEAT; in a FOR EACH it leaves the loop; a plain DO inside the
    // CATCH does not stop the search.
    let leaves = p.nodes(cfg, "UNDO, LEAVE.");
    let undone: Vec<_> = leaves.iter().map(|&n| cfg.node(n).undoes).collect();
    assert_eq!(undone, [Some(do1), Some(each), Some(do3)]);
    for (&n, b) in leaves.iter().zip([do1, each, do3]) {
        assert_eq!(goes(n), (EdgeKind::Leave, NodeKind::After(b)));
    }
    let message = p.node(cfg, "MESSAGE \"after the DO");
    assert!(
        cfg.reachable_from(&[leaves[0]], |_| false)
            .contains(message.index())
    );

    // NEXT and RETRY go to the next iteration of an iterating owner; a
    // non-iterating owner is left.
    let nexts = p.nodes(cfg, "UNDO, NEXT.");
    assert_eq!(goes(nexts[0]), (EdgeKind::Next, NodeKind::End));
    assert_eq!(cfg.node(nexts[0]).undoes, Some(each));
    assert_eq!(goes(nexts[1]), (EdgeKind::Leave, NodeKind::After(do3)));
    for text in ["UNDO, RETRY.", "UNDO."] {
        let n = p.node(cfg, text);
        assert_eq!(cfg.node(n).undoes, Some(each), "{text}");
        assert_eq!(goes(n), (EdgeKind::Next, NodeKind::End), "{text}");
    }
}

#[test]
fn undo_throw_undoes_a_block_and_raises_outside_it() {
    let p = parse(include_str!("fixtures/undo_throw.p"));
    let flow = p.flow();
    let cfg = &flow.units()[0];

    // Default target: the FOR EACH. The error goes to the DO around it, and
    // that DO's CATCH runs.
    let undo = p.node(cfg, "UNDO, THROW NEW");
    let for_each = cfg.node(p.node(cfg, "FOR EACH")).block;
    assert_eq!(cfg.node(undo).undoes, Some(for_each));
    let handler = throw_handler(cfg, undo);
    assert_eq!(cfg.node(handler).block, cfg.block(for_each).parent.unwrap());
    assert!(hands_to(cfg, handler, p.node(cfg, "CATCH e AS")));

    // A label names the block to undo; the error leaves that block, and the
    // routine (not the labelled block's CATCH) gets it.
    let undo = p.node(cfg, "UNDO outer, THROW");
    let outer = cfg
        .blocks()
        .find(|(_, b)| b.label.as_deref() == Some("outer"))
        .expect("labelled block")
        .0;
    assert_eq!(cfg.node(undo).undoes, Some(outer));
    let handler = throw_handler(cfg, undo);
    assert_eq!(cfg.node(handler).block, BlockId::ROUTINE);
    assert!(!hands_to(cfg, handler, p.node(cfg, "CATCH e2 AS")));

    // The rethrow in a CATCH reaches the enclosing block's CATCH.
    let undo = p.node(cfg, "UNDO, THROW e3");
    let handler = throw_handler(cfg, undo);
    assert!(hands_to(cfg, handler, p.node(cfg, "CATCH e4 AS")));

    // The bare THROW statement is raised in its own block and stays there.
    let bare = p.node(cfg, "THROW NEW");
    assert_eq!(cfg.node(bare).undoes, None);
    let handler = throw_handler(cfg, bare);
    assert!(hands_to(cfg, handler, p.node(cfg, "CATCH e5 AS")));
}

#[test]
fn undo_without_an_action_does_the_blocks_error_action() {
    let p = parse(include_str!("fixtures/defaults_undo_statement.p"));
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let nodes = p.nodes(cfg, "UNDO.");
    let kinds: Vec<EdgeKind> = nodes.iter().map(|&n| only_edge(cfg, n).1).collect();
    // BLOCK-LEVEL throws; an explicit ON ERROR phrase wins.
    assert_eq!(kinds, [EdgeKind::Throw, EdgeKind::Leave, EdgeKind::Throw]);
}

#[test]
fn for_first_and_for_last_run_their_body_once() {
    let p = parse(include_str!("fixtures/for_first.p"));
    let flow = p.flow();
    let cfg = &flow.units()[0];
    let loop_backs: Vec<_> = cfg
        .edges()
        .iter()
        .filter(|e| e.kind == EdgeKind::LoopBack)
        .collect();
    // Only the FOR FIRST that joins an EACH goes round again.
    assert_eq!(loop_backs.len(), 1);
    let each = block_of_head(&p, cfg, "FOR FIRST Customer NO-LOCK, EACH");
    assert_eq!(cfg.node(loop_backs[0].from).block, each);

    for prefix in [
        "FOR FIRST Customer NO-LOCK:",
        "FOR LAST Customer NO-LOCK:",
        "FOR FIRST Customer NO-LOCK, LAST",
    ] {
        let b = block_of_head(&p, cfg, prefix);
        assert!(
            cfg.block(b).iterating,
            "{prefix}: NEXT and RETRY still apply"
        );
        let end = cfg
            .nodes()
            .find(|(_, n)| n.kind == NodeKind::End && n.block == b)
            .expect("end node")
            .0;
        let (to, kind) = only_edge(cfg, end);
        assert_eq!(kind, EdgeKind::LoopDone, "{prefix}");
        assert_eq!(cfg.node(to).kind, NodeKind::After(b), "{prefix}");
    }

    // A NEXT in a FOR LAST ends the loop.
    let next = p.node(cfg, "NEXT.");
    let (end, kind) = only_edge(cfg, next);
    assert_eq!((kind, cfg.node(end).kind), (EdgeKind::Next, NodeKind::End));
}
