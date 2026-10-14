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
    ] {
        assert_callee_unknown(&work(defs, call));
    }
}

#[test]
fn a_shared_library_entry_point_borrows_its_input_arguments() {
    let dll = "\
PROCEDURE killx EXTERNAL \"lib.dll\":
  DEFINE INPUT PARAMETER pm AS MEMPTR NO-UNDO.
  DEFINE INPUT PARAMETER pn AS LONG NO-UNDO.
END PROCEDURE.
";
    let o = only(&work(dll, "RUN killx (INPUT hq, INPUT 1)."));
    assert_eq!(o.verdict, Verdict::Leaks);
    let memptr = format!(
        "\
{dll}
PROCEDURE work:
  DEFINE VARIABLE m AS MEMPTR NO-UNDO.
  SET-SIZE(m) = 16.
  RUN killx (INPUT m, INPUT 16).
END PROCEDURE.
"
    );
    let o = only(&memptr);
    assert_eq!(
        o.verdict,
        Verdict::Leaks,
        "{}",
        analyze(&memptr).dump(&memptr)
    );
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
  METHOD PRIVATE VOID Keep(INPUT ph AS HANDLE):
  END METHOD.
  METHOD PRIVATE VOID Save(INPUT ph AS HANDLE):
  END METHOD.
END CLASS.
"
    ))
}

/// A class `Svc`, declared by `class`, whose method `Work` creates a query
/// and runs `call` on it; `consume` declares a method `Consume`.
fn consumer(class: &str, consume: &str, call: &str) -> Object {
    only(&format!(
        "\
{class}:
  DEFINE PRIVATE VARIABLE oPeer AS Svc NO-UNDO.
  METHOD PUBLIC VOID Work():
    DEFINE VARIABLE hq AS HANDLE NO-UNDO.
    CREATE QUERY hq.
    {call}
  END METHOD.
  {consume}
END CLASS.
"
    ))
}

#[test]
fn an_abstract_method_is_not_a_callee_of_the_file() {
    for call in ["Consume(hq).", "THIS-OBJECT:Consume(hq)."] {
        let o = consumer(
            "CLASS Svc ABSTRACT",
            "METHOD PROTECTED ABSTRACT VOID Consume(INPUT ph AS HANDLE).",
            call,
        );
        assert_eq!(o.verdict, Verdict::Unknown, "{call}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{call}");
    }
}

#[test]
fn a_method_a_subclass_can_override_is_not_bound_to_its_body() {
    for access in ["PUBLIC", "PROTECTED"] {
        for call in [
            "Consume(hq).",
            "THIS-OBJECT:Consume(hq).",
            "oPeer:Consume(hq).",
        ] {
            let consume =
                format!("METHOD {access} VOID Consume(INPUT ph AS HANDLE):\n  END METHOD.");
            let o = consumer("CLASS Svc", &consume, call);
            assert_eq!(o.verdict, Verdict::Unknown, "{access} {call}");
            assert_eq!(reasons(&o), [OpaqueReason::Callee], "{access} {call}");
        }
    }
}

#[test]
fn a_private_static_or_final_class_method_is_bound_to_its_body() {
    for (class, access) in [
        ("CLASS Svc", "PRIVATE"),
        ("CLASS Svc", "PUBLIC STATIC"),
        ("CLASS Svc", "PROTECTED STATIC"),
        ("CLASS Svc FINAL", "PUBLIC"),
        ("CLASS Svc FINAL", "PROTECTED"),
    ] {
        for (body, freed) in [("", false), ("DELETE OBJECT ph.", true)] {
            let consume = format!(
                "METHOD {access} VOID Consume(INPUT ph AS HANDLE):\n    {body}\n  END METHOD."
            );
            let o = consumer(class, &consume, "Consume(hq).");
            if freed {
                assert_eq!(escapes(&o), [EscapeKind::FreedByCallee], "{class} {access}");
            } else {
                assert_eq!(o.verdict, Verdict::Leaks, "{class} {access}");
            }
        }
    }
}

#[test]
fn a_parameter_that_cannot_take_the_argument_is_not_the_callee() {
    // The call may bind to an inherited overload that takes the handle.
    for ty in ["CHARACTER", "INTEGER", "Progress.Lang.Object"] {
        let consume = format!("METHOD PRIVATE VOID Consume(INPUT pc AS {ty}):\n  END METHOD.");
        let o = consumer("CLASS Svc", &consume, "Consume(hq).");
        assert_eq!(o.verdict, Verdict::Unknown, "{ty}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{ty}");
    }
}

#[test]
fn a_parameter_that_takes_a_handle_or_a_memptr_is_read() {
    for ty in ["HANDLE", "WIDGET-HANDLE", "COM-HANDLE", "MEMPTR"] {
        let consume = format!("METHOD PRIVATE VOID Consume(INPUT ph AS {ty}):\n  END METHOD.");
        let o = consumer("CLASS Svc", &consume, "Consume(hq).");
        assert_eq!(o.verdict, Verdict::Leaks, "{ty}");
    }
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

#[test]
fn each_name_of_a_var_list_is_a_variable_of_its_unit() {
    let all = objects(
        "\
CLASS Svc:
  DEFINE PRIVATE VARIABLE hA AS HANDLE NO-UNDO.
  METHOD PUBLIC VOID Work():
    VAR HANDLE hq, hq2.
    VAR MEMPTR m1, m2.
    CREATE QUERY hq.
    CREATE QUERY hq2.
    hA = hq2.
    SET-SIZE(m2) = 8.
  END METHOD.
END CLASS.
",
    );
    let verdicts: Vec<_> = all
        .iter()
        .map(|o| (o.holder.clone().unwrap(), o.verdict, escapes(o)))
        .collect();
    assert_eq!(
        verdicts,
        [
            ("hq".into(), Verdict::Leaks, vec![]),
            ("hq2".into(), Verdict::Escapes, vec![EscapeKind::Member]),
            ("m2".into(), Verdict::Leaks, vec![]),
        ]
    );
}

#[test]
fn a_buffer_added_to_an_object_the_unit_did_not_create_is_unknown() {
    let all = objects(
        "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
PROCEDURE a:
  DEFINE INPUT PARAMETER phDs AS HANDLE NO-UNDO.
  DEFINE VARIABLE hb AS HANDLE NO-UNDO.
  CREATE BUFFER hb FOR TABLE \"tt\".
  phDs:ADD-BUFFER(hb).
END PROCEDURE.
DEFINE DATASET ds FOR tt.
PROCEDURE b:
  DEFINE VARIABLE hb2 AS HANDLE NO-UNDO.
  CREATE BUFFER hb2 FOR TABLE \"tt\".
  DATASET ds:ADD-BUFFER(hb2).
END PROCEDURE.
",
    );
    assert_eq!(all.len(), 2);
    for o in &all {
        assert_eq!(o.verdict, Verdict::Unknown, "{:?}", o.holder);
        assert_eq!(reasons(o), [OpaqueReason::AddBuffer], "{:?}", o.holder);
    }
    for body in ["po:ADD-BUFFER(ph).", "ghq:ADD-BUFFER(ph)."] {
        let o = passed_to(body);
        assert_eq!(o.verdict, Verdict::Unknown, "{body}");
        assert_eq!(reasons(&o), [OpaqueReason::Callee], "{body}");
    }
}

#[test]
fn many_callees_that_hand_a_handle_to_each_other_stay_fast() {
    const N: usize = 60;
    let mut src = String::new();
    for i in 0..N {
        src.push_str(&format!(
            "PROCEDURE p{i}:\n  DEFINE INPUT PARAMETER ph AS HANDLE NO-UNDO.\n"
        ));
        for j in (0..N).filter(|&j| j != i) {
            src.push_str(&format!("  RUN p{j} (INPUT ph).\n"));
        }
        src.push_str("END PROCEDURE.\n");
    }
    src.push_str("PROCEDURE work:\n  DEFINE VARIABLE hq AS HANDLE NO-UNDO.\n  CREATE QUERY hq.\n");
    for j in 0..N {
        src.push_str(&format!("  RUN p{j} (INPUT hq).\n"));
    }
    src.push_str("END PROCEDURE.\n");
    let start = std::time::Instant::now();
    let o = only(&src);
    let took = start.elapsed();
    assert_eq!(o.verdict, Verdict::Unknown);
    assert!(reasons(&o).iter().all(|&r| r == OpaqueReason::Callee));
    // Unbounded, this shape costs seconds at N = 60 even in a release build.
    assert!(took.as_millis() < 1500, "{took:?}");
}
