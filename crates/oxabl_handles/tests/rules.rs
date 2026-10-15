//! One small consumer per `strict.handles` rule, to show that each rule is a
//! thin query over the model. These are not the lint rules; they return the
//! lines a rule would report.

use oxabl_handles::{FileHandles, Object, ObjectKind, Placement, Verdict};

fn analyze(source: &str) -> (FileHandles, Vec<usize>) {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let starts = std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect();
    (
        oxabl_handles::analyze_file(&program.statements, source),
        starts,
    )
}

fn line(starts: &[usize], at: u32) -> usize {
    starts.partition_point(|&s| s <= at as usize)
}

/// Lines of the create sites of the objects `rule` selects.
fn report(source: &str, rule: impl Fn(&Object) -> bool) -> Vec<usize> {
    let (handles, starts) = analyze(source);
    handles
        .objects()
        .filter(|(_, o)| rule(o))
        .map(|(_, o)| line(&starts, o.create.span.start))
        .collect()
}

/// HD001 handle-not-deleted: a handle-based object, widget or COM object lost
/// on a path that raises nothing.
fn hd001(o: &Object) -> bool {
    !matches!(o.kind, ObjectKind::PersistentProcedure | ObjectKind::Memptr)
        && o.leaks_on_normal_path()
}

/// HD002 delete-not-in-finally: freed on the success path, lost only where a
/// condition is raised.
fn hd002(o: &Object) -> bool {
    o.leaks_on_abnormal_path_only() && !o.frees.is_empty()
}

/// HD003 persistent-procedure-leak.
fn hd003(o: &Object) -> bool {
    o.kind == ObjectKind::PersistentProcedure && o.leaks_on_normal_path()
}

/// HD004 memptr-not-freed.
fn hd004(o: &Object) -> bool {
    o.kind == ObjectKind::Memptr && o.leaks_on_normal_path()
}

/// HD005 missing-widget-pool: an object that goes into a pool, with no pool of
/// this procedure in scope.
fn hd005(o: &Object) -> bool {
    o.kind.uses_widget_pool() && o.placement == Placement::Unscoped
}

#[test]
fn hd001_reports_an_object_lost_on_a_normal_path() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hFreed AS HANDLE NO-UNDO.
DEFINE VARIABLE hGuarded AS HANDLE NO-UNDO.
DEFINE VARIABLE lStop AS LOGICAL NO-UNDO.
CREATE QUERY hQuery.
CREATE BUFFER hFreed FOR TABLE \"customer\".
CREATE QUERY hGuarded.
IF lStop THEN RETURN.
DELETE OBJECT hFreed.
IF VALID-HANDLE(hGuarded) THEN DELETE OBJECT hGuarded.
";
    // hFreed and hGuarded leak too, on the early RETURN.
    assert_eq!(report(src, hd001), vec![5, 6, 7]);
}

#[test]
fn hd001_ignores_escapes_and_finally() {
    let src = "\
DEFINE OUTPUT PARAMETER phOut AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY phOut.
DO ON ERROR UNDO, THROW:
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    FINALLY:
        DELETE OBJECT hQuery NO-ERROR.
    END FINALLY.
END.
";
    assert_eq!(report(src, hd001), Vec::<usize>::new());
    let (handles, _) = analyze(src);
    let verdicts: Vec<Verdict> = handles.objects().map(|(_, o)| o.verdict).collect();
    assert_eq!(verdicts, vec![Verdict::Escapes, Verdict::Freed]);
}

#[test]
fn hd002_reports_a_delete_on_the_success_path_only() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    DELETE OBJECT hQuery.
END PROCEDURE.
";
    assert_eq!(report(src, hd002), vec![3]);
    assert_eq!(report(src, hd001), Vec::<usize>::new());
    let (handles, starts) = analyze(src);
    let (_, o) = handles.objects().next().unwrap();
    let via = o.leaks[0].via.expect("an abnormal leak names its raise");
    assert_eq!(line(&starts, via.span.start), 4);
}

#[test]
fn hd003_reports_a_persistent_procedure_never_deleted() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE hTool AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
RUN tool.p PERSISTENT SET hTool.
RUN singleton.p PERSISTENT.
DELETE PROCEDURE hTool.
";
    assert_eq!(report(src, hd003), vec![3]);
    // A RUN PERSISTENT without SET is told apart, for HD003 to treat on its own.
    let uncaptured = report(src, |o| o.verdict == Verdict::Uncaptured);
    assert_eq!(uncaptured, vec![5]);
}

#[test]
fn hd004_reports_a_memptr_never_set_back_to_zero() {
    let src = "\
DEFINE VARIABLE mFreed AS MEMPTR NO-UNDO.
DEFINE VARIABLE mLeak AS MEMPTR NO-UNDO.
SET-SIZE(mFreed) = 16.
SET-SIZE(mLeak) = 16.
SET-SIZE(mFreed) = 0.
";
    assert_eq!(report(src, hd004), vec![4]);
}

#[test]
fn hd005_reports_objects_outside_any_pool_of_the_procedure() {
    let pooled = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE WIDGET-POOL.
CREATE QUERY hQuery.
PROCEDURE p:
    DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    CREATE BUFFER hBuffer FOR TABLE \"customer\".
END PROCEDURE.
";
    assert_eq!(report(pooled, hd005), Vec::<usize>::new());
    // The pool frees what is left in it: not an HD001 leak.
    assert_eq!(report(pooled, hd001), Vec::<usize>::new());

    let unpooled = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
RUN lib.p PERSISTENT SET hProc.
";
    // A persistent procedure never goes in a widget pool.
    assert_eq!(report(unpooled, hd005), vec![3]);
}
