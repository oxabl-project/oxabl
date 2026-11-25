//! The members of built-in types, as `type-mismatch` sees them: read from the
//! static catalog, with or without a workspace index.

mod support;

use support::*;

fn messages(found: &[oxabl_common::Diagnostic]) -> Vec<String> {
    found.iter().map(|d| d.message.clone()).collect()
}

const MISMATCH_CHAR_LOGICAL: &str = "type mismatch: cannot assign `CHARACTER` to `LOGICAL`";

#[test]
fn a_built_in_receiver_needs_no_index() {
    let source = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
                  DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
                  l = oErr:GetMessage(1).\n\
                  MESSAGE oErr l.\n";
    assert_eq!(
        messages(&lint0004_without_index(source)),
        [MISMATCH_CHAR_LOGICAL]
    );
    assert_eq!(
        messages(&lint0004_with_index(source, &[])),
        [MISMATCH_CHAR_LOGICAL]
    );
}

#[test]
fn a_class_that_inherits_a_built_in_class_types_its_inherited_calls_with_no_index() {
    let source = "CLASS app.Err INHERITS Progress.Lang.AppError:\n\
                  METHOD PUBLIC VOID Go():\n\
                  DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
                  l = GetMessage(1).\n\
                  l = THIS-OBJECT:GetMessage(1).\n\
                  MESSAGE l.\n\
                  END METHOD.\n\
                  END CLASS.\n";
    assert_eq!(
        messages(&lint0004_without_index(source)),
        [MISMATCH_CHAR_LOGICAL, MISMATCH_CHAR_LOGICAL]
    );
}

#[test]
fn a_damaged_level_leaves_methods_unknown_and_properties_typed() {
    let source = "DEFINE VARIABLE oCls AS Progress.Lang.Class NO-UNDO.\n\
                  DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
                  i = oCls:IsInterface().\n\
                  i = oCls:TypeName.\n\
                  MESSAGE oCls i.\n";
    assert_eq!(
        messages(&lint0004_without_index(source)),
        ["type mismatch: cannot assign `CHARACTER` to `INTEGER`"]
    );
}

#[test]
fn a_generic_type_gives_no_member_type() {
    let source = "DEFINE VARIABLE oList AS Progress.Collections.List<OpenEdge.Core.String> NO-UNDO.\n\
                  DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
                  l = oList:Count.\n\
                  MESSAGE oList l.\n";
    assert!(lint0004_without_index(source).is_empty());
}

#[test]
fn an_enum_member_has_the_type_of_its_enum() {
    let source = "DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
                  i = Progress.ApplicationServer.AdapterTypes:SOAP.\n\
                  i = OpenEdge.Net.UriSchemeEnum:http.\n\
                  MESSAGE i.\n";
    // The catalog does not list the members of `UriSchemeEnum`.
    assert_eq!(
        messages(&lint0004_without_index(source)),
        ["type mismatch: cannot assign `Progress.ApplicationServer.AdapterTypes` to `INTEGER`"]
    );
}

#[test]
fn a_workspace_enum_value_has_the_methods_of_progress_lang_enum() {
    let workspace = [(
        "/src/app/Shade.cls",
        "ENUM app.Shade:\n    DEFINE ENUM Light Dark.\nEND ENUM.\n",
    )];
    let source = "DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
                  DEFINE VARIABLE e AS Progress.Lang.Enum NO-UNDO.\n\
                  l = app.Shade:Dark:GetValue().\n\
                  e = app.Shade:Dark.\n\
                  MESSAGE l e.\n";
    assert_eq!(
        messages(&lint0004_with_index(source, &workspace)),
        ["type mismatch: cannot assign `INT64` to `LOGICAL`"]
    );
}
