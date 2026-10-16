//! CL004 `unresolved-call`.

use oxabl_lint::{undefined_symbol, unresolved_call};

use super::{Setup, lines, run_with};

fn check(source: &str) -> Vec<oxabl_common::Diagnostic> {
    run_with(unresolved_call::run, source, &Setup::default())
}

#[test]
fn a_bare_name_that_nothing_supplies_is_reported() {
    let source = "RUN post-invoice.\n";
    let found = check(source);
    assert_eq!(lines(source, &found), [1]);
    assert!(found[0].message.contains("RUN post-invoice"));
    // Anchored on the target, not the whole statement.
    let span = found[0].span.span;
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "post-invoice"
    );
}

#[test]
fn a_program_on_the_search_path_is_not_reported() {
    let setup = Setup {
        files: &[("/src/post-invoice.p", "MESSAGE 1.\n")],
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, "RUN post-invoice.\n", &setup).is_empty());
    // The sibling: the same call with the file missing fires.
    assert_eq!(check("RUN post-invoice.\n").len(), 1);
}

#[test]
fn a_path_without_an_extension_is_reported_when_nothing_supplies_it() {
    let source = "RUN orders/post-invoice.\n";
    assert_eq!(check(source).len(), 1);
    let setup = Setup {
        files: &[("/src/orders/post-invoice.p", "MESSAGE 1.\n")],
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &setup).is_empty());
}

#[test]
fn an_internal_procedure_is_not_reported() {
    let source = "PROCEDURE post-invoice:\nEND PROCEDURE.\nRUN post-invoice.\nRUN POST-INVOICE.\n";
    assert!(check(source).is_empty());
}

#[test]
fn a_file_name_is_left_to_undefined_symbol() {
    let source = "RUN missing.p.\nRUN missing.\n";
    let cl004 = check(source);
    assert_eq!(lines(source, &cl004), [2]);
    // LINT0001 reports the first and not the second, so no `RUN` is reported twice.
    let lint0001 = run_with(undefined_symbol::run, source, &Setup::default());
    assert_eq!(lines(source, &lint0001), [1]);
}

#[test]
fn a_target_the_call_graph_cannot_name_is_not_reported() {
    let source = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
RUN VALUE(c).
RUN something IN h.
RUN SUPER.
RUN other IN THIS-PROCEDURE.
";
    // Only `IN THIS-PROCEDURE` names an internal procedure that is not there.
    let found = check(source);
    assert_eq!(lines(source, &found), [6]);
    assert!(found[0].message.contains("RUN other"));
}

#[test]
fn an_asynchronous_or_remote_run_is_not_reported() {
    let source = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
RUN remote-thing ASYNCHRONOUS SET h.
RUN other-thing ON SERVER h.
";
    assert!(check(source).is_empty());
}

#[test]
fn a_persistent_run_is_reported_like_a_plain_one() {
    let source = "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nRUN lib-thing PERSISTENT SET h.\n";
    assert_eq!(check(source).len(), 1);
}

#[test]
fn a_name_several_files_match_is_not_reported() {
    let setup = Setup {
        files: &[
            ("/a/thing.p", "MESSAGE 1.\n"),
            ("/b/thing.p", "MESSAGE 2.\n"),
        ],
        paths: Some(&["/a", "/b"]),
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, "RUN thing.\n", &setup).is_empty());
}

#[test]
fn nothing_is_reported_when_nothing_was_searched() {
    let source = "RUN post-invoice.\n";
    let no_index = Setup {
        no_index: true,
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &no_index).is_empty());
    let no_paths = Setup {
        paths: Some(&[]),
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &no_paths).is_empty());
}

#[test]
fn a_bare_name_in_an_include_alone_or_after_a_lost_include_is_not_reported() {
    let source = "RUN post-invoice.\n";
    let fragment = Setup {
        fragment: true,
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &fragment).is_empty());
    let lost = Setup {
        elided_includes: &[0],
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &lost).is_empty());
    // A path-qualified name does not depend on the includer.
    let qualified = Setup {
        fragment: true,
        ..Setup::default()
    };
    assert_eq!(
        run_with(
            unresolved_call::run,
            "RUN orders/post-invoice.\n",
            &qualified
        )
        .len(),
        1
    );
}

#[test]
fn a_bare_name_that_a_file_of_the_workspace_defines_is_not_reported() {
    // A super or persistent procedure supplies it at run time.
    let source = "RUN setup-globals.\n";
    let setup = Setup {
        files: &[(
            "/src/super.p",
            "PROCEDURE setup-globals:\nEND PROCEDURE.\nproc other-one:\nEND PROCEDURE.\n",
        )],
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, source, &setup).is_empty());
    assert!(run_with(unresolved_call::run, "RUN OTHER-ONE.\n", &setup).is_empty());
    // A name no file defines still fires with the same workspace.
    assert_eq!(
        run_with(unresolved_call::run, "RUN nobody.\n", &setup).len(),
        1
    );
}

#[test]
fn a_bare_name_is_not_reported_when_the_index_lists_no_files() {
    let setup = Setup {
        unseeded: true,
        ..Setup::default()
    };
    assert!(run_with(unresolved_call::run, "RUN post-invoice.\n", &setup).is_empty());
    // A path-qualified name is a file name and needs no list.
    assert_eq!(
        run_with(unresolved_call::run, "RUN orders/post-invoice.\n", &setup).len(),
        1
    );
}

#[test]
fn a_program_the_openedge_install_ships_is_not_reported() {
    assert!(check("RUN adecomm/_osprint.\n").is_empty());
    assert!(check("RUN ADEUIB/_uibmain.\n").is_empty());
    assert_eq!(check("RUN mine/_osprint.\n").len(), 1);
}

#[test]
fn an_operation_of_a_web_service_is_not_reported() {
    let source = "\
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE hPort AS HANDLE NO-UNDO.
RUN PortSoap SET hPort ON SERVER hServer.
RUN CancelThing (INPUT 1).
";
    assert!(check(source).is_empty());
    // The same call in a file with no port type is reported.
    assert_eq!(check("RUN CancelThing (INPUT 1).\n").len(), 1);
    // An AppServer program started with PERSISTENT is not a port type.
    let appserver = "\
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
RUN remote.p PERSISTENT SET hProc ON SERVER hServer.
RUN CancelThing (INPUT 1).
";
    assert_eq!(check(appserver).len(), 1);
}

#[test]
fn a_name_the_parser_read_short_is_not_reported() {
    // The parser stops this name at `dir/870`; the rest is part of the file name.
    assert!(check("RUN dir/870-recon-compare.p (INPUT 1).\n").is_empty());
    assert_eq!(check("RUN zf/recon-compare (INPUT 1).\n").len(), 1);
}
