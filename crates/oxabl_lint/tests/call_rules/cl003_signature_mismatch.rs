//! CL003 `signature-mismatch`.

use oxabl_lint::signature_mismatch;

use super::{Setup, lines, messages, run_with};

const PROCS: &str = "\
PROCEDURE total-order:
    DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER dTotal AS DECIMAL NO-UNDO.
END PROCEDURE.
PROCEDURE both:
    DEFINE INPUT-OUTPUT PARAMETER cText AS CHARACTER NO-UNDO.
END PROCEDURE.
DEFINE VARIABLE iNum AS INTEGER NO-UNDO.
DEFINE VARIABLE dNum AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO.
";

fn check(calls: &str) -> Vec<oxabl_common::Diagnostic> {
    run_with(
        signature_mismatch::run,
        &format!("{PROCS}{calls}"),
        &Setup::default(),
    )
}

fn count_lines() -> usize {
    PROCS.lines().count()
}

#[test]
fn a_call_that_fits_is_clean() {
    assert!(
        check("RUN total-order (INPUT iNum, OUTPUT dNum).\nRUN both (INPUT-OUTPUT cStr).\n")
            .is_empty()
    );
    // A mode left out means `INPUT`.
    assert!(check("RUN total-order (iNum, OUTPUT dNum).\n").is_empty());
}

#[test]
fn too_few_or_too_many_arguments_is_one_finding_on_the_target() {
    let few = check("RUN total-order (INPUT iNum).\n");
    assert_eq!(few.len(), 1);
    assert!(
        few[0]
            .message
            .contains("passes 1 argument, but the procedure takes 2")
    );
    let span = few[0].span.span;
    let source = format!("{PROCS}RUN total-order (INPUT iNum).\n");
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "total-order"
    );

    let many = check("RUN both (INPUT-OUTPUT cStr, INPUT iNum).\n");
    assert_eq!(many.len(), 1);
    assert!(
        many[0]
            .message
            .contains("passes 2 arguments, but the procedure takes 1")
    );

    let none = check("RUN both.\n");
    assert!(none[0].message.contains("passes 0 arguments"));
}

#[test]
fn an_output_parameter_passed_without_output_is_reported() {
    let found = check("RUN total-order (INPUT iNum, dNum).\n");
    assert_eq!(found.len(), 1);
    assert!(found[0].message.contains("argument 2"));
    assert!(
        found[0]
            .message
            .contains("passed as `INPUT`, but the parameter is `OUTPUT`")
    );
}

#[test]
fn an_input_parameter_passed_as_output_is_reported() {
    let found = check("RUN total-order (OUTPUT iNum, OUTPUT dNum).\n");
    assert_eq!(found.len(), 1);
    assert!(found[0].message.contains("argument 1"));
    assert!(
        found[0]
            .message
            .contains("passed as `OUTPUT`, but the parameter is `INPUT`")
    );
}

#[test]
fn an_input_output_parameter_needs_input_output() {
    assert_eq!(check("RUN both (INPUT cStr).\n").len(), 1);
    assert_eq!(check("RUN both (OUTPUT cStr).\n").len(), 1);
    assert!(check("RUN both (INPUT-OUTPUT cStr).\n").is_empty());
}

#[test]
fn an_expression_passed_as_output_is_reported() {
    let found = check("RUN total-order (INPUT 1, OUTPUT dNum + 1).\n");
    assert_eq!(found.len(), 1);
    assert!(found[0].message.contains("is an expression"));
    let literal = check("RUN total-order (INPUT 1, OUTPUT 5).\n");
    assert_eq!(literal.len(), 1);
    // Fields, variables and array elements are writable.
    assert!(check("RUN total-order (INPUT 1, OUTPUT dNum).\n").is_empty());
}

#[test]
fn only_a_handle_clash_is_a_type_finding() {
    let source = "\
PROCEDURE p:
    DEFINE INPUT PARAMETER lOk AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER cOk AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iOk AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER hOut AS HANDLE NO-UNDO.
END PROCEDURE.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
";
    let run = |calls: &str| {
        run_with(
            signature_mismatch::run,
            &format!("{source}{calls}"),
            &Setup::default(),
        )
    };
    // A handle for a LOGICAL parameter.
    let found = run("RUN p (h, c, i, OUTPUT h).\n");
    assert_eq!(found.len(), 1);
    assert!(
        found[0]
            .message
            .contains("is HANDLE, but the parameter is LOGICAL")
    );
    // A handle value written back into an INTEGER variable converts; a LOGICAL
    // variable does not take it.
    assert!(run("RUN p (l, c, i, OUTPUT h).\n").is_empty());
    // Other pairs may convert when the call runs: no finding.
    assert!(run("RUN p (i, i, c, OUTPUT h).\n").is_empty());
    assert!(run("RUN p (c, l, h, OUTPUT h).\n").is_empty());
    // The unknown value fits anything.
    assert!(run("RUN p (?, ?, ?, OUTPUT h).\n").is_empty());
}

#[test]
fn an_argument_gets_at_most_one_finding() {
    // A wrong mode, and an expression written as `OUTPUT`: the mode is reported.
    let found = check("RUN total-order (OUTPUT 1, OUTPUT dNum).\n");
    assert_eq!(found.len(), 1);
    assert!(found[0].message.contains("passed as"));
}

#[test]
fn findings_are_anchored_on_the_argument() {
    let source = format!("{PROCS}RUN total-order (\n    INPUT iNum,\n    dNum).\n");
    let found = run_with(signature_mismatch::run, &source, &Setup::default());
    assert_eq!(lines(&source, &found), [count_lines() + 3]);
}

#[test]
fn unknown_types_never_make_a_finding() {
    let source = "\
PROCEDURE p:
    DEFINE INPUT PARAMETER a LIKE Customer.Name.
    DEFINE INPUT PARAMETER b AS INTEGER EXTENT 3 NO-UNDO.
    DEFINE INPUT PARAMETER c AS CLASS Foo.Bar NO-UNDO.
END PROCEDURE.
DEFINE VARIABLE iNum AS INTEGER NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO.
RUN p (INPUT lFlag, INPUT iNum, INPUT iNum).
";
    assert!(run_with(signature_mismatch::run, source, &Setup::default()).is_empty());
}

#[test]
fn table_buffer_and_dataset_arguments_are_matched_by_kind() {
    let source = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS INTEGER.
DEFINE DATASET ds FOR tt.
DEFINE VARIABLE iNum AS INTEGER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
PROCEDURE takes-table:
    DEFINE INPUT PARAMETER TABLE FOR tt.
END PROCEDURE.
PROCEDURE takes-handle:
    DEFINE INPUT PARAMETER TABLE-HANDLE th.
END PROCEDURE.
PROCEDURE takes-dataset:
    DEFINE INPUT PARAMETER DATASET FOR ds.
END PROCEDURE.
PROCEDURE takes-buffer:
    DEFINE PARAMETER BUFFER b FOR tt.
END PROCEDURE.
";
    let run = |calls: &str| {
        run_with(
            signature_mismatch::run,
            &format!("{source}{calls}"),
            &Setup::default(),
        )
    };
    // A TABLE and a TABLE-HANDLE fit each other.
    assert!(run("RUN takes-table (INPUT TABLE tt).\nRUN takes-handle (INPUT TABLE tt).\nRUN takes-handle (INPUT TABLE-HANDLE h).\nRUN takes-table (INPUT TABLE-HANDLE h).\n").is_empty());
    assert!(
        run("RUN takes-dataset (INPUT DATASET ds).\nRUN takes-dataset (INPUT DATASET-HANDLE h).\n")
            .is_empty()
    );
    assert!(run("RUN takes-buffer (BUFFER tt).\n").is_empty());
    // A plain variable where a table is expected, and the other way round.
    let found = run("RUN takes-table (INPUT iNum).\n");
    assert_eq!(found.len(), 1);
    assert!(
        found[0]
            .message
            .contains("is a plain variable or value, but the parameter is a `TABLE`")
    );
    assert_eq!(run("RUN takes-buffer (INPUT iNum).\n").len(), 1);
    assert_eq!(run("RUN takes-handle (INPUT h).\n").len(), 1);
    assert_eq!(run("RUN takes-dataset (INPUT TABLE tt).\n").len(), 1);
    assert_eq!(run("RUN takes-table (INPUT DATASET ds).\n").len(), 1);
    assert_eq!(
        messages(&run("RUN takes-table (OUTPUT TABLE tt).\n")),
        ["argument 1 of `RUN takes-table` is passed as `OUTPUT`, but the parameter is `INPUT`"]
    );
}

#[test]
fn only_a_call_that_names_a_whole_known_callee_is_checked() {
    let source = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
PROCEDURE twice:
    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.
END PROCEDURE.
PROCEDURE twice:
END PROCEDURE.
PROCEDURE once:
    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.
END PROCEDURE.
RUN twice.
RUN VALUE(c) (INPUT 1, INPUT 2).
RUN once IN h.
RUN once ASYNCHRONOUS.
RUN once ON SERVER h.
RUN once IN THIS-PROCEDURE.
";
    let found = run_with(signature_mismatch::run, source, &Setup::default());
    // Only the `IN THIS-PROCEDURE` call is checked: no argument for `a`.
    assert_eq!(lines(source, &found), [16]);
}

#[test]
fn a_procedure_with_an_include_is_not_checked() {
    let source = "\
PROCEDURE p:
    {params.i}
END PROCEDURE.
RUN p (INPUT 1, INPUT 2).
";
    assert!(run_with(signature_mismatch::run, source, &Setup::default()).is_empty());
}

#[test]
fn a_run_with_a_lost_include_is_not_checked() {
    let source = format!("{PROCS}RUN both.\n");
    let lost = Setup {
        elided_includes: &[0],
        ..Setup::default()
    };
    assert!(run_with(signature_mismatch::run, &source, &lost).is_empty());
    assert_eq!(check("RUN both.\n").len(), 1);
}

// ---------------------------------------------------------------------------
// Programs of other files
// ---------------------------------------------------------------------------

const CALLEE: &str = "\
DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER dTotal AS DECIMAL NO-UNDO.
";

const CALLER_DECLS: &str = "\
DEFINE VARIABLE iNum AS INTEGER NO-UNDO.
DEFINE VARIABLE dNum AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
";

fn external(calls: &str, files: &[(&str, &str)]) -> Vec<oxabl_common::Diagnostic> {
    let source = format!("{CALLER_DECLS}{calls}");
    let setup = Setup {
        files,
        ..Setup::default()
    };
    run_with(signature_mismatch::run, &source, &setup)
}

#[test]
fn an_external_program_is_checked_against_its_main_block_parameters() {
    let files = [("/src/total.p", CALLEE)];
    assert!(external("RUN total.p (INPUT iNum, OUTPUT dNum).\n", &files).is_empty());
    assert_eq!(external("RUN total.p (INPUT iNum).\n", &files).len(), 1);
    assert_eq!(
        external("RUN total.p (INPUT iNum, dNum).\n", &files).len(),
        1
    );
    assert_eq!(
        external("RUN total.p (INPUT-OUTPUT iNum, OUTPUT dNum).\n", &files).len(),
        1
    );
    // Without the extension, and with PERSISTENT.
    assert_eq!(external("RUN total (INPUT iNum).\n", &files).len(), 1);
    assert_eq!(
        external("RUN total.p PERSISTENT SET h (INPUT iNum).\n", &files).len(),
        1
    );
}

#[test]
fn a_program_with_no_parameters_takes_no_arguments() {
    let files = [("/src/plain.p", "MESSAGE 1.\n")];
    assert!(external("RUN plain.p.\n", &files).is_empty());
    assert_eq!(external("RUN plain.p (INPUT iNum).\n", &files).len(), 1);
}

#[test]
fn a_callee_that_is_not_known_gives_no_finding() {
    // Not on the search path.
    assert!(external("RUN missing.p (INPUT iNum).\n", &[]).is_empty());
    // A parse the file does not survive.
    let broken = [(
        "/src/broken.p",
        "DEFINE INPUT PARAMETER AS AS AS.\nIF THEN THEN.\n",
    )];
    assert!(external("RUN broken.p (INPUT iNum).\n", &broken).is_empty());
    // A class.
    let class = [("/src/cls.p", "CLASS a.b:\nEND CLASS.\n")];
    assert!(external("RUN cls.p (INPUT iNum).\n", &class).is_empty());
    // Several files.
    let setup = Setup {
        files: &[("/a/t.p", CALLEE), ("/b/t.p", "MESSAGE 1.\n")],
        paths: Some(&["/a", "/b"]),
        ..Setup::default()
    };
    assert!(run_with(signature_mismatch::run, "RUN t.p.\n", &setup).is_empty());
    // No index.
    let none = Setup {
        no_index: true,
        files: &[("/src/total.p", CALLEE)],
        ..Setup::default()
    };
    assert!(run_with(signature_mismatch::run, "RUN total.p (INPUT 1).\n", &none).is_empty());
}

#[test]
fn an_include_in_the_callee_is_read_through() {
    let files = [
        ("/src/total.p", "{params.i}\nMESSAGE 1.\n"),
        ("/src/params.i", CALLEE),
    ];
    assert!(external("RUN total.p (INPUT iNum, OUTPUT dNum).\n", &files).is_empty());
    // Mode and type are compared as for any callee.
    assert_eq!(
        external("RUN total.p (INPUT iNum, dNum).\n", &files).len(),
        1
    );
    // An include that is not there leaves the parameters unknown.
    let lost = [("/src/total.p", "{missing.i}\nMESSAGE 1.\n")];
    assert!(external("RUN total.p (INPUT iNum).\n", &lost).is_empty());
}

#[test]
fn a_count_against_an_include_supplied_list_needs_an_argument_from_an_include() {
    let files = [
        ("/src/total.p", "{params.i}\nMESSAGE 1.\n"),
        ("/src/params.i", CALLEE),
    ];
    // The call names no argument from an include: not judged on the count.
    assert!(external("RUN total.p (INPUT iNum).\n", &files).is_empty());
    // With an argument an include wrote, the count is judged. The region is
    // the `INPUT iNum` of the source below.
    let source = format!("{CALLER_DECLS}RUN total.p (INPUT iNum).\n");
    let at = source.find("INPUT iNum").unwrap() as u32;
    let regions = [oxabl_ast::Span {
        start: at,
        end: at + 10,
    }];
    let setup = Setup {
        files: &files,
        include_regions: &regions,
        ..Setup::default()
    };
    assert_eq!(run_with(signature_mismatch::run, &source, &setup).len(), 1);
    // A statement that an include holds whole is not a call with an include in it.
    let whole = [oxabl_ast::Span {
        start: at - 20,
        end: at + 30,
    }];
    let setup = Setup {
        files: &files,
        include_regions: &whole,
        ..Setup::default()
    };
    assert!(run_with(signature_mismatch::run, &source, &setup).is_empty());
    // A list the callee's own text shows is judged whatever the call holds.
    let own = [("/src/total.p", CALLEE)];
    assert_eq!(external("RUN total.p (INPUT iNum).\n", &own).len(), 1);
}

#[test]
fn a_procedure_in_a_persistent_program_is_checked_through_its_handle() {
    let lib = "\
PROCEDURE add-line:
    DEFINE INPUT PARAMETER iNum AS INTEGER NO-UNDO.
END PROCEDURE.
";
    let files = [("/src/lib.p", lib)];
    let call = |run: &str| external(&format!("RUN lib.p PERSISTENT SET h.\n{run}"), &files);
    assert!(call("RUN add-line IN h (INPUT iNum).\n").is_empty());
    assert_eq!(call("RUN add-line IN h.\n").len(), 1);
    assert_eq!(call("RUN add-line IN h (OUTPUT iNum).\n").len(), 1);
    // A handle the file sets in another way is Unknown.
    let reset = "RUN lib.p PERSISTENT SET h.\nh = SESSION:FIRST-PROCEDURE.\nRUN add-line IN h.\n";
    assert!(external(reset, &files).is_empty());
}

#[test]
fn a_bare_name_in_an_include_alone_is_not_checked() {
    let files = [("/src/total.p", CALLEE)];
    let setup = Setup {
        files: &files,
        fragment: true,
        ..Setup::default()
    };
    assert!(run_with(signature_mismatch::run, "RUN total (INPUT 1).\n", &setup).is_empty());
    // With the extension the callee does not depend on the includer.
    assert_eq!(
        run_with(signature_mismatch::run, "RUN total.p (INPUT 1).\n", &setup).len(),
        1
    );
}
