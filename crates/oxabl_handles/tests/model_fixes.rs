//! Calls, `:HANDLE` identity, unmapped frees, `COPY-LOB`, `APPLY "CLOSE"`,
//! shadowed names and the work budget.

use oxabl_handles::{EscapeKind, FileHandles, Object, ObjectKind, OpaqueReason, Verdict};

fn analyze(source: &str) -> FileHandles {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}\n{source}", program.errors);
    oxabl_handles::analyze_file(&program.statements, source)
}

/// The objects of the file, in source order.
fn objects(handles: &FileHandles) -> Vec<&Object> {
    let mut all: Vec<&Object> = handles.objects().map(|(_, o)| o).collect();
    all.sort_by_key(|o| o.create.span.start);
    all
}

/// The only object of the file.
fn only(source: &str) -> Object {
    let handles = analyze(source);
    let all = objects(&handles);
    assert_eq!(all.len(), 1, "{}", handles.dump(source));
    all[0].clone()
}

fn reasons(o: &Object) -> Vec<OpaqueReason> {
    o.opaque.iter().map(|x| x.reason).collect()
}

fn escapes(o: &Object) -> Vec<EscapeKind> {
    o.escapes.iter().map(|x| x.kind).collect()
}

#[test]
fn input_argument_freed_by_a_procedure_of_the_file_is_an_escape() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
RUN cleanup (INPUT hq).

PROCEDURE cleanup:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  DELETE OBJECT ph.
END PROCEDURE.
",
    );
    assert_eq!(o.verdict, Verdict::Escapes);
    assert_eq!(escapes(&o), [EscapeKind::FreedByCallee]);
}

#[test]
fn input_argument_freed_by_a_function_of_the_file_is_an_escape() {
    let o = only(
        "\
FUNCTION killit RETURNS LOGICAL (INPUT ph AS HANDLE):
  DELETE OBJECT ph.
  RETURN TRUE.
END FUNCTION.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  killit(hq).
END PROCEDURE.
",
    );
    assert_eq!(o.verdict, Verdict::Escapes);
    assert_eq!(escapes(&o), [EscapeKind::FreedByCallee]);
}

#[test]
fn only_the_argument_in_the_position_the_callee_frees_is_handed_over() {
    let src = "\
DEFINE VARIABLE hKeep AS HANDLE NO-UNDO.
DEFINE VARIABLE hGone AS HANDLE NO-UNDO.
CREATE QUERY hKeep.
CREATE QUERY hGone.
RUN drop-two (INPUT hKeep, INPUT hGone).

PROCEDURE drop-two:
  DEFINE INPUT PARAMETER phKeep AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER phGone AS HANDLE NO-UNDO.
  DELETE OBJECT phGone.
END PROCEDURE.
";
    let handles = analyze(src);
    let all = objects(&handles);
    assert_eq!(all[0].verdict, Verdict::Leaks, "{}", handles.dump(src));
    assert_eq!(all[1].verdict, Verdict::Escapes);
}

#[test]
fn input_argument_of_a_procedure_that_keeps_it_is_still_a_leak() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.
RUN show (INPUT hq).

PROCEDURE show:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  MESSAGE ph:NAME.
END PROCEDURE.
",
    );
    assert_eq!(o.verdict, Verdict::Leaks);
}

#[test]
fn input_argument_of_a_call_the_file_does_not_define_is_opaque() {
    for call in [
        "RUN other.p (INPUT hq).",
        "RUN cleanup IN hLib (INPUT hq).",
        "RUN cleanup (INPUT hq).",
        "lOk = unknownFn(hq).",
        "lOk = oSvc:Register(hq).",
        "lOk = Pkg.Util:Register(hq).",
    ] {
        let src = format!(
            "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.
CREATE QUERY hq.
{call}
"
        );
        let o = only(&src);
        assert_eq!(o.verdict, Verdict::Unknown, "{call}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{call}");
    }
}

#[test]
fn known_borrows_stay_uses() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.
CREATE QUERY hq.
hq:SET-BUFFERS(hBuf).
hBuf:FIND-FIRST(STRING(hq)).
lOk = VALID-HANDLE(hq).
MESSAGE STRING(hq).
",
    );
    assert_eq!(o.kind, ObjectKind::Query);
    assert_eq!(o.verdict, Verdict::Leaks);
}

#[test]
fn handle_attribute_is_the_same_object() {
    let o = only(
        "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE h2 AS HANDLE NO-UNDO.
CREATE QUERY h.
h2 = h:HANDLE.
DELETE OBJECT h2.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
    let o = only(
        "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
CREATE QUERY h.
DELETE OBJECT h:HANDLE.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
}

#[test]
fn free_of_an_operand_that_cannot_be_mapped_is_opaque() {
    for del in [
        "DELETE OBJECT WIDGET-HANDLE(STRING(h)).",
        "DELETE OBJECT h:PARENT.",
        "DELETE WIDGET h:FRAME.",
    ] {
        let src = format!(
            "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
CREATE QUERY h.
{del}
"
        );
        let o = only(&src);
        assert_eq!(o.verdict, Verdict::Unknown, "{del}");
        assert_eq!(reasons(&o), [OpaqueReason::UnmappedFree], "{del}");
    }
}

#[test]
fn validity_and_error_tests_do_not_raise() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DO ON ERROR UNDO, THROW:
    CREATE QUERY hq.
    FINALLY:
        IF VALID-HANDLE(hq) THEN DELETE OBJECT hq.
    END FINALLY.
END.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.
DO ON ERROR UNDO, THROW:
    CREATE QUERY hq.
    IF NOT VALID-OBJECT(oObj) THEN RETURN.
    FINALLY:
        IF hq <> ? AND VALID-HANDLE(hq) THEN DELETE OBJECT hq.
    END FINALLY.
END.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
}

#[test]
fn error_status_test_does_not_raise() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
DELETE OBJECT hq.
",
    );
    assert!(o.leaks.iter().all(|l| l.normal), "{o:?}");
    assert!(!o.leaks_on_abnormal_path_only());
}

#[test]
fn copy_lob_to_a_memptr_allocates_it() {
    let o = only(
        "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
COPY-LOB FROM FILE \"x\" TO m.
",
    );
    assert_eq!(o.kind, ObjectKind::Memptr);
    assert_eq!(o.verdict, Verdict::Leaks);
    let o = only(
        "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
COPY-LOB FROM FILE \"x\" TO m.
SET-SIZE(m) = 0.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
}

#[test]
fn copy_lob_after_set_size_is_not_a_second_object() {
    let o = only(
        "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
SET-SIZE(m) = 4096.
COPY-LOB FROM FILE \"x\" TO m.
",
    );
    assert_eq!(o.verdict, Verdict::Leaks);
    let handles = analyze(
        "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
COPY-LOB FROM FILE \"x\" TO m.
COPY-LOB FROM FILE \"y\" TO m.
",
    );
    assert_eq!(objects(&handles).len(), 1);
}

#[test]
fn copy_lob_from_a_memptr_or_to_another_target_is_a_use() {
    let o = only(
        "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
SET-SIZE(m) = 4096.
COPY-LOB FROM m TO FILE \"out\".
",
    );
    assert_eq!(o.verdict, Verdict::Leaks);
    let handles = analyze(
        "\
DEFINE VARIABLE c AS LONGCHAR NO-UNDO.
COPY-LOB FROM FILE \"x\" TO c.
COPY-LOB FROM FILE \"x\" TO FILE \"y\".
",
    );
    assert!(objects(&handles).is_empty());
}

#[test]
fn apply_close_frees_only_a_persistent_procedure() {
    let o = only(
        "\
DEFINE VARIABLE hw AS HANDLE NO-UNDO.
CREATE WINDOW hw.
APPLY \"CLOSE\" TO hw.
",
    );
    assert_eq!(o.verdict, Verdict::Leaks);
    let o = only(
        "\
DEFINE VARIABLE hp AS HANDLE NO-UNDO.
RUN p.p PERSISTENT SET hp.
APPLY \"CLOSE\" TO hp.
",
    );
    assert_eq!(o.verdict, Verdict::Freed);
}

#[test]
fn a_local_of_the_same_name_is_not_the_variable_another_unit_frees() {
    let src = "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.

PROCEDURE other:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  DELETE OBJECT hq.
END PROCEDURE.
";
    let handles = analyze(src);
    let all = objects(&handles);
    assert_eq!(all[0].verdict, Verdict::Leaks, "{}", handles.dump(src));
    assert_eq!(all[1].verdict, Verdict::Freed);
}

#[test]
fn a_main_block_variable_another_unit_frees_is_owned_there() {
    let o = only(
        "\
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
CREATE QUERY hq.

PROCEDURE stop-it:
  DELETE OBJECT hq.
END PROCEDURE.
",
    );
    assert_eq!(o.verdict, Verdict::Escapes);
    assert_eq!(escapes(&o), [EscapeKind::FreedByOtherUnit]);
}

/// `n` procedures, each with an object and `k` conditional aliases of it:
/// about `2^k` walk states each.
fn heavy(n: usize, k: usize) -> String {
    let mut s = String::from("DEFINE VARIABLE x AS LOGICAL NO-UNDO.\n");
    for o in 0..n {
        s += &format!("PROCEDURE p{o}:\nDEFINE VARIABLE c AS HANDLE NO-UNDO.\n");
        for i in 0..k {
            s += &format!("DEFINE VARIABLE d{i} AS HANDLE NO-UNDO.\n");
        }
        s += "CREATE QUERY c.\n";
        for i in 0..k {
            s += &format!("IF x THEN d{i} = c.\n");
        }
        s += "END PROCEDURE.\n";
    }
    s
}

#[test]
fn a_file_has_a_work_budget() {
    let handles = analyze(&heavy(1, 13));
    assert_eq!(objects(&handles)[0].verdict, Verdict::Leaks);
    let src = heavy(40, 13);
    let handles = analyze(&src);
    let all = objects(&handles);
    assert_eq!(all.len(), 40);
    assert_eq!(all[0].verdict, Verdict::Leaks);
    let last = all[39];
    assert_eq!(last.verdict, Verdict::Unknown);
    assert_eq!(reasons(last), [OpaqueReason::TooComplex]);
}

#[test]
fn a_call_with_no_receiver_names_a_method_of_the_class() {
    let o = only(
        "\
CLASS Svc:
  METHOD PUBLIC VOID Run():
    DEFINE VARIABLE hq AS HANDLE NO-UNDO.
    CREATE QUERY hq.
    Show(hq).
  END METHOD.
  METHOD PRIVATE VOID Show(INPUT ph AS HANDLE):
    MESSAGE ph:NAME.
  END METHOD.
END CLASS.
",
    );
    assert_eq!(o.verdict, Verdict::Leaks);
}

#[test]
fn table_and_dataset_arguments_are_copies() {
    let handles = analyze(
        "\
DEFINE VARIABLE hds AS HANDLE NO-UNDO.
DEFINE VARIABLE htt AS HANDLE NO-UNDO.
CREATE DATASET hds.
CREATE TEMP-TABLE htt.
RUN x.p (INPUT DATASET-HANDLE hds, TABLE-HANDLE htt).
",
    );
    assert!(
        objects(&handles)
            .iter()
            .all(|o| o.verdict == Verdict::Leaks)
    );
}

#[test]
fn a_table_argument_by_reference_is_not_a_copy() {
    let o = only(
        "\
DEFINE VARIABLE hds AS HANDLE NO-UNDO.
CREATE DATASET hds.
RUN x.p (INPUT DATASET-HANDLE hds BY-REFERENCE).
",
    );
    assert_eq!(o.verdict, Verdict::Unknown);
    assert_eq!(reasons(&o), [OpaqueReason::Callee]);
}

#[test]
fn a_dynamic_call_still_hides_a_table_argument() {
    let o = only(
        "\
DEFINE VARIABLE hds AS HANDLE NO-UNDO.
DEFINE VARIABLE cProc AS CHARACTER NO-UNDO.
CREATE DATASET hds.
RUN VALUE(cProc) (INPUT DATASET-HANDLE hds BY-REFERENCE).
",
    );
    assert_eq!(o.verdict, Verdict::Unknown);
    assert_eq!(reasons(&o), [OpaqueReason::DynamicCall]);
}
