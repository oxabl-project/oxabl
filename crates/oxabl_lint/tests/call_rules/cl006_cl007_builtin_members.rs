//! CL006 `member-call-mismatch` and CL007 `unknown-member`: calls and member
//! names of built-in types, judged from the catalog only where it is whole.

use oxabl_lint::{member_call_mismatch, unknown_member};

use super::{Setup, lines, messages, run_with};

const DECLS: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.\n\
DEFINE VARIABLE oCls AS Progress.Lang.Class NO-UNDO.\n\
DEFINE VARIABLE i64 AS INT64 NO-UNDO.\n\
DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n";

fn source(rows: &str) -> String {
    format!("{DECLS}{rows}")
}

fn cl006(rows: &str, setup: &Setup) -> Vec<String> {
    let src = source(rows);
    messages(&run_with(member_call_mismatch::run, &src, setup))
        .into_iter()
        .map(String::from)
        .collect()
}

fn cl007(rows: &str, setup: &Setup) -> Vec<String> {
    let src = source(rows);
    messages(&run_with(unknown_member::run, &src, setup))
        .into_iter()
        .map(String::from)
        .collect()
}

#[test]
fn a_call_that_no_overload_takes_is_reported() {
    let rows = "MESSAGE oErr:GetMessage(1, 2).\n\
                MESSAGE oErr:GetMessage(\"1\").\n\
                MESSAGE oErr:GetMessage(oJson).\n\
                oErr:AddMessage(\"x\").\n\
                oErr = NEW Progress.Lang.AppError(\"a\", 1, 2).\n\
                MESSAGE OpenEdge.Core.String:IsNullOrEmpty(lc, 1).\n";
    for setup in [
        Setup::default(),
        Setup {
            no_index: true,
            ..Setup::default()
        },
    ] {
        assert_eq!(
            cl006(rows, &setup),
            [
                "no overload of `Progress.Lang.AppError:GetMessage` takes 2 arguments",
                "no overload of `Progress.Lang.AppError:GetMessage` takes arguments of these types: `CHARACTER`",
                "no overload of `Progress.Lang.AppError:GetMessage` takes arguments of these types: `Progress.Json.ObjectModel.JsonObject`",
                "no overload of `Progress.Lang.AppError:AddMessage` takes 1 argument",
                "no constructor of `Progress.Lang.AppError` takes 3 arguments",
                "no overload of `OpenEdge.Core.String:IsNullOrEmpty` takes 2 arguments",
            ]
        );
    }
    let src = source(rows);
    let found = run_with(member_call_mismatch::run, &src, &Setup::default());
    assert_eq!(lines(&src, &found), [7, 8, 9, 10, 11, 12]);
    // The span is the whole call.
    let first = &found[0].span.span;
    assert_eq!(
        &src[first.start as usize..first.end as usize],
        "oErr:GetMessage(1, 2)"
    );
}

#[test]
fn a_call_some_overload_may_take_is_silent() {
    let rows = "MESSAGE oErr:GetMessage(1).\n\
                MESSAGE oErr:GetMessage(i64).\n\
                MESSAGE oErr:GetMessage(?).\n\
                MESSAGE oErr:GetMessage(1.5).\n\
                oErr = NEW Progress.Lang.AppError().\n\
                oErr = NEW Progress.Lang.AppError(\"a\", 1).\n\
                oErr:AddMessage(\"x\", 1).\n\
                MESSAGE oJson:Add(\"k\", oObj).\n\
                MESSAGE oErr:NumMessages().\n";
    assert!(cl006(rows, &Setup::default()).is_empty());
}

#[test]
fn facts_that_are_not_known_give_no_call_finding() {
    let rows = "MESSAGE oCls:Invoke(oObj, \"x\", 1, 2, 3, 4, 5, 6, 7, 8, 9).\n\
                MESSAGE oErr:GetMessage({missing-arg.i}).\n\
                MESSAGE oErr:GetMessage(1, {&extra}).\n";
    assert!(cl006(rows, &Setup::default()).is_empty());
    // An include analysed alone.
    let fragment = Setup {
        fragment: true,
        ..Setup::default()
    };
    assert!(cl006("MESSAGE oErr:GetMessage(1, 2).\n", &fragment).is_empty());
}

#[test]
fn a_class_that_inherits_a_built_in_class_is_not_judged() {
    let files = [(
        "/src/app/OrderError.cls",
        "CLASS app.OrderError INHERITS Progress.Lang.AppError:\nEND CLASS.\n",
    )];
    let rows = "DEFINE VARIABLE oOE AS app.OrderError NO-UNDO.\n\
                MESSAGE oOE:GetMessage(1, 2).\n\
                MESSAGE oOE:GetMesage(1).\n";
    let setup = Setup {
        files: &files,
        ..Setup::default()
    };
    assert!(cl006(rows, &setup).is_empty());
    assert!(cl007(rows, &setup).is_empty());
}

#[test]
fn a_name_a_complete_built_in_type_does_not_have_is_reported() {
    let rows = "MESSAGE oErr:GetMesage(1).\n\
                MESSAGE oErr:CallStak.\n\
                MESSAGE oObj:Lookup(\"x\").\n\
                MESSAGE Progress.Json.ObjectModel.JsonDataType:Strings.\n";
    for setup in [
        Setup::default(),
        Setup {
            no_index: true,
            ..Setup::default()
        },
    ] {
        assert_eq!(
            cl007(rows, &setup),
            [
                "built-in type `Progress.Lang.AppError` has no member `GetMesage`",
                "built-in type `Progress.Lang.AppError` has no member `CallStak`",
                "built-in type `Progress.Lang.Object` has no member `Lookup`",
                "built-in type `Progress.Json.ObjectModel.JsonDataType` has no member `Strings`",
            ]
        );
    }
    let src = source(rows);
    let found = run_with(unknown_member::run, &src, &Setup::default());
    let first = &found[0].span.span;
    assert_eq!(&src[first.start as usize..first.end as usize], "GetMesage");
}

#[test]
fn a_name_the_catalog_may_lack_is_silent() {
    let rows = "MESSAGE oErr:CallStack.\n\
                MESSAGE oErr:NumMessages().\n\
                MESSAGE oCls:NoSuch().\n\
                MESSAGE OpenEdge.Net.UriSchemeEnum:nosuch.\n\
                DEFINE VARIABLE oList AS Progress.Collections.List<OpenEdge.Core.String> NO-UNDO.\n\
                MESSAGE oList:NoSuch().\n";
    assert!(cl007(rows, &Setup::default()).is_empty());
    let fragment = Setup {
        fragment: true,
        ..Setup::default()
    };
    assert!(cl007("MESSAGE oErr:GetMesage(1).\n", &fragment).is_empty());
}
