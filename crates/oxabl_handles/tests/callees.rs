//! What a callee does with a handle passed to it: prototypes, what a defined
//! callee does with its parameter, method receivers, `COPY-LOB` targets by
//! definition, and the statement named as the raise of a loop.

use oxabl_handles::{EscapeKind, FileHandles, Object, OpaqueReason, Verdict};

fn analyze(source: &str) -> FileHandles {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}\n{source}", program.errors);
    oxabl_handles::analyze_file(&program.statements, source)
}

/// The objects of the file, in source order.
fn objects(source: &str) -> Vec<Object> {
    let handles = analyze(source);
    let mut all: Vec<Object> = handles.objects().map(|(_, o)| o.clone()).collect();
    all.sort_by_key(|o| o.create.span.start);
    all
}

/// The only object of the file.
fn only(source: &str) -> Object {
    let all = objects(source);
    assert_eq!(all.len(), 1, "{}", analyze(source).dump(source));
    all[0].clone()
}

fn reasons(o: &Object) -> Vec<OpaqueReason> {
    o.opaque.iter().map(|x| x.reason).collect()
}

fn escapes(o: &Object) -> Vec<EscapeKind> {
    o.escapes.iter().map(|x| x.kind).collect()
}

/// A procedure `work` that creates a query and runs `call` on it, after
/// `defs`.
fn work(defs: &str, call: &str) -> String {
    format!(
        "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
{defs}
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.
  CREATE QUERY hq.
  {call}
END PROCEDURE.
"
    )
}

fn assert_callee_unknown(src: &str) {
    let o = only(src);
    assert_eq!(o.verdict, Verdict::Unknown, "{src}");
    assert_eq!(reasons(&o), [OpaqueReason::Callee], "{src}");
}

#[test]
fn a_procedure_prototype_is_not_a_callee_of_the_file() {
    for (defs, call) in [
        (
            "PROCEDURE killp IN SUPER:\n  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.\nEND PROCEDURE.",
            "RUN killp (INPUT hq).",
        ),
        (
            "PROCEDURE killp IN hLib:\n  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.\nEND PROCEDURE.",
            "RUN killp (INPUT hq).",
        ),
        (
            "PROCEDURE killx EXTERNAL \"lib.dll\":\n  DEFINE INPUT PARAMETER ph AS LONG NO-UNDO.\nEND PROCEDURE.",
            "RUN killx (INPUT hq).",
        ),
    ] {
        assert_callee_unknown(&work(defs, call));
    }
}

#[test]
fn a_function_prototype_in_another_procedure_is_not_a_callee_of_the_file() {
    for defs in [
        "FUNCTION killq RETURNS LOGICAL (INPUT ph AS HANDLE) IN hLib.",
        "FUNCTION killq RETURNS LOGICAL (INPUT ph AS HANDLE) IN SUPER.",
    ] {
        assert_callee_unknown(&work(defs, "lOk = killq(hq)."));
    }
}

#[test]
fn a_forward_function_is_the_callee_its_definition_is() {
    let src = |body: &str| {
        format!(
            "\
FUNCTION killq RETURNS LOGICAL (INPUT ph AS HANDLE) FORWARD.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  killq(hq).
END PROCEDURE.
FUNCTION killq RETURNS LOGICAL (INPUT ph AS HANDLE):
  {body}
  RETURN TRUE.
END FUNCTION.
"
        )
    };
    let freed = only(&src("DELETE OBJECT ph."));
    assert_eq!(escapes(&freed), [EscapeKind::FreedByCallee]);
    let used = only(&src("MESSAGE ph:NAME."));
    assert_eq!(used.verdict, Verdict::Leaks);
}

/// The query `work` passes to `keep`, whose body is `body`.
fn passed_to(body: &str) -> Object {
    let all = objects(&format!(
        "\
DEFINE VARIABLE ghq AS HANDLE NO-UNDO.
PROCEDURE keep:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER po AS HANDLE NO-UNDO.
  DEFINE VARIABLE hLocal AS HANDLE NO-UNDO.
  {body}
END PROCEDURE.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  DEFINE VARIABLE hOut AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  RUN keep (INPUT hq, OUTPUT hOut).
END PROCEDURE.
"
    ));
    let work = all.iter().find(|o| o.holder.as_deref() == Some("hq"));
    work.expect("the query of work").clone()
}

#[test]
fn a_callee_that_stores_its_parameter_outside_keeps_the_object() {
    for body in [
        "ghq = ph.",
        "po = ph.",
        "hLocal = ph. ghq = hLocal.",
        "PUBLISH \"took\" (ph).",
        "CREATE DATASET hLocal. hLocal:ADD-BUFFER(ph).",
    ] {
        let o = passed_to(body);
        assert_eq!(o.verdict, Verdict::Escapes, "{body}");
        assert_eq!(escapes(&o), [EscapeKind::KeptByCallee], "{body}");
    }
}

#[test]
fn a_callee_that_frees_a_copy_of_its_parameter_frees_the_object() {
    let o = passed_to("hLocal = ph. DELETE OBJECT hLocal.");
    assert_eq!(escapes(&o), [EscapeKind::FreedByCallee]);
}

#[test]
fn a_callee_that_hands_its_parameter_to_unseen_code_is_unknown() {
    for body in [
        "RUN lib/destroy.p (INPUT ph).",
        "RUN undefined-proc (INPUT ph).",
        "RUN VALUE(\"x\") (INPUT ph).",
        "WAIT-FOR CLOSE OF ph.",
    ] {
        let o = passed_to(body);
        assert_eq!(o.verdict, Verdict::Unknown, "{body}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{body}");
    }
}

#[test]
fn a_callee_that_only_uses_its_parameter_borrows_it() {
    for body in [
        "MESSAGE ph:NAME.",
        "hLocal = ph. MESSAGE hLocal:NAME.",
        "CREATE QUERY hLocal. hLocal:ADD-BUFFER(ph).",
        "",
    ] {
        assert_eq!(passed_to(body).verdict, Verdict::Leaks, "{body}");
    }
}

#[test]
fn a_callee_that_hands_its_parameter_to_another_callee_of_the_file_takes_that_fate() {
    let src = |inner: &str| {
        format!(
            "\
PROCEDURE inner:
  DEFINE INPUT PARAMETER pi AS HANDLE NO-UNDO.
  {inner}
END PROCEDURE.
PROCEDURE outer:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  RUN inner (INPUT ph).
END PROCEDURE.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  RUN outer (INPUT hq).
END PROCEDURE.
"
        )
    };
    assert_eq!(
        escapes(&only(&src("DELETE OBJECT pi."))),
        [EscapeKind::FreedByCallee]
    );
    assert_eq!(only(&src("MESSAGE pi:NAME.")).verdict, Verdict::Leaks);
    assert_eq!(
        reasons(&only(&src("RUN other.p (INPUT pi)."))),
        [OpaqueReason::Callee]
    );
}

#[test]
fn a_call_back_into_a_callee_being_read_adds_nothing() {
    let src = |body: &str| {
        format!(
            "\
PROCEDURE walk:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER pn AS INTEGER NO-UNDO.
  IF pn > 0 THEN RUN walk (INPUT ph, INPUT pn - 1).
  {body}
END PROCEDURE.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  RUN walk (INPUT hq, INPUT 3).
END PROCEDURE.
"
        )
    };
    assert_eq!(only(&src("")).verdict, Verdict::Leaks);
    assert_eq!(
        escapes(&only(&src("DELETE OBJECT ph."))),
        [EscapeKind::FreedByCallee]
    );
}

#[test]
fn a_handle_handed_on_past_the_depth_bound_is_unknown() {
    let o = only(
        "\
PROCEDURE p5:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  MESSAGE ph:NAME.
END PROCEDURE.
PROCEDURE p4:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  RUN p5 (INPUT ph).
END PROCEDURE.
PROCEDURE p3:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  RUN p4 (INPUT ph).
END PROCEDURE.
PROCEDURE p2:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  RUN p3 (INPUT ph).
END PROCEDURE.
PROCEDURE p1:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  RUN p2 (INPUT ph).
END PROCEDURE.
PROCEDURE work:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  RUN p1 (INPUT hq).
END PROCEDURE.
",
    );
    assert_eq!(reasons(&o), [OpaqueReason::Callee]);
}

/// A method `Work` of class `Svc` that creates a query and runs `call` on it.
fn method(call: &str) -> Object {
    only(&format!(
        "\
CLASS pkg.Svc:
  DEFINE PRIVATE VARIABLE oRepo AS Progress.Lang.Object NO-UNDO.
  DEFINE PRIVATE VARIABLE oPeer AS pkg.Svc NO-UNDO.
  DEFINE PRIVATE VARIABLE oShort AS Svc NO-UNDO.
  DEFINE PRIVATE VARIABLE hProc AS HANDLE NO-UNDO.
  METHOD PUBLIC VOID Work():
    DEFINE VARIABLE hq AS HANDLE NO-UNDO.
    CREATE QUERY hq.
    {call}
  END METHOD.
  METHOD PUBLIC VOID Keep(INPUT ph AS HANDLE):
  END METHOD.
  METHOD PUBLIC VOID Save(INPUT ph AS HANDLE):
  END METHOD.
END CLASS.
"
    ))
}

#[test]
fn a_method_of_an_object_of_another_class_is_unknown() {
    for call in [
        "CAST(oRepo, pkg.Repo):Save(hq).",
        "oRepo:Save(hq).",
        "oRepo:Load(hq).",
        "oRepo:Store-Query(hq).",
        "oRepo:Keep(hq).",
        "oPeer:Inner:Keep(hq).",
        "SUPER:Keep(hq).",
    ] {
        let o = method(call);
        assert_eq!(o.verdict, Verdict::Unknown, "{call}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{call}");
    }
}

#[test]
fn a_method_of_this_class_is_matched_by_name() {
    for call in [
        "Keep(hq).",
        "THIS-OBJECT:Keep(hq).",
        "oPeer:Keep(hq).",
        "oShort:Save(hq).",
    ] {
        assert_eq!(method(call).verdict, Verdict::Leaks, "{call}");
    }
}

#[test]
fn a_method_of_a_handle_is_built_in() {
    let src = |call: &str| {
        format!(
            "\
DEFINE VARIABLE hb AS HANDLE NO-UNDO.
DEFINE VARIABLE hq AS HANDLE NO-UNDO.
DEFINE VARIABLE w AS WIDGET-HANDLE NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE BUFFER bt FOR tt.
CREATE BUFFER hb FOR TABLE \"tt\".
{call}
PROCEDURE copy-into:
  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt.
  tt:BUFFER-COPY(ph).
END PROCEDURE.
"
        )
    };
    for call in [
        "hq:SET-BUFFERS(hb).",
        "w:MOVE-AFTER-TAB-ITEM(hb).",
        "bt:BUFFER-COPY(hb).",
        "FOCUS:MOVE-AFTER-TAB-ITEM(hb).",
        "hq:GET-BUFFER-HANDLE(1):BUFFER-COPY(hb).",
        "BUFFER tt:HANDLE:BUFFER-COPY(hb).",
        "BUFFER Customer:HANDLE:BUFFER-COPY(hb).",
        "RUN copy-into (INPUT hb).",
    ] {
        let all = objects(&src(call));
        assert_eq!(all.len(), 1, "{call}");
        assert_eq!(all[0].verdict, Verdict::Leaks, "{call}");
    }
    let all = objects(&src("hq:Register(hb)."));
    assert_eq!(reasons(&all[0]), [OpaqueReason::Callee]);
}

#[test]
fn a_copy_lob_target_is_a_memptr_by_its_own_definition() {
    let all = objects(
        "\
DEFINE VARIABLE gm AS MEMPTR NO-UNDO.
PROCEDURE a:
  DEFINE VARIABLE m AS MEMPTR NO-UNDO.
  SET-SIZE(m) = 0.
END PROCEDURE.
PROCEDURE b:
  DEFINE VARIABLE m AS LONGCHAR NO-UNDO.
  COPY-LOB FROM FILE \"x\" TO m.
END PROCEDURE.
PROCEDURE c:
  COPY-LOB FROM FILE \"x\" TO gm.
END PROCEDURE.
",
    );
    assert_eq!(all.len(), 1);
    assert_eq!(all[0].holder.as_deref(), Some("gm"));
}

#[test]
fn a_loop_with_plain_bounds_does_not_raise_in_its_header() {
    let src = "\
PROCEDURE p:
  DEFINE INPUT PARAMETER cFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m AS MEMPTR NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DO i = 1 TO 3:
    COPY-LOB FROM FILE cFile TO m.
  END.
  SET-SIZE(m) = 0.
END PROCEDURE.
";
    let o = only(src);
    let via = o.leaks[0].via.expect("an abnormal leak");
    let text = &src[via.span.start as usize..via.span.end as usize];
    assert!(text.starts_with("COPY-LOB"), "{text}");
}
