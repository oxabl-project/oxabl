//! `unsafe-deserialization` (ABLSEC014).

use oxabl_lint::security::dynamic_code;
use oxabl_lint::security::unsafe_deserialization::{HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_deserialize_with_a_non_literal() {
    let src = "\
oObj = oSer:Deserialize(mData).
oSer:Deserialize(cText).
oObj = oSer:deserialize(mData, \"x\" + cType).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "Deserialize(mData)",
            "Deserialize(cText)",
            "deserialize(mData, \"x\" + cType)"
        ]
    );
}

#[test]
fn reports_the_serializer_classes_by_receiver_expression() {
    let src = "\
oObj = NEW Progress.IO.BinarySerializer():Deserialize(mData).
oObj = Progress.IO.JsonSerializer:Deserialize(lcJson).
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn a_literal_or_no_argument_is_not_reported() {
    let src = "\
oSer:Deserialize(\"{}\").
oSer:Deserialize().
oSer:Serialize(oObj).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_class_name_from_data_belongs_to_the_dynamic_code_rule() {
    let src = "oClass = Progress.Lang.Class:GetClass(cName).\n";
    assert!(check(run, src).is_empty());
    assert_eq!(check(dynamic_code::run, src).len(), 1);
}

#[test]
fn reports_in_blocks_and_class_members() {
    let src = "\
CLASS Acme.Store:
    METHOD PUBLIC Progress.Lang.Object Load(INPUT pmData AS MEMPTR):
        RETURN oSer:Deserialize(pmData).
    END METHOD.
END CLASS.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_constant() {
    let main = "\
&GLOBAL-DEFINE EMPTY \"{}\"
oSer:Deserialize({&EMPTY}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("load.i", "oObj = oSer:Deserialize(mData).\n")];
    assert_eq!(check_expanded(run, &files, "{load.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_pattern() {
    assert!(HELP.contains("fixed"), "{HELP}");
}
