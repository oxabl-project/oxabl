//! `xss-output` (ABLSEC006).

use oxabl_lint::security::xss_output::{HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_the_out_preprocessor_name_with_a_non_literal() {
    let src = "{&OUT} \"<p>\" cName \"</p>\".\n";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(flagged(src, &diags), ["{&OUT} \"<p>\" cName \"</p>\""]);
}

#[test]
fn out_with_only_literals_is_not_reported() {
    let src = "{&OUT} \"<p>\" \"hello\" SKIP.\n{&OUT} \"a\" + \"b\".\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn out_is_reported_after_the_web_include_expands_it() {
    let main = "\
&GLOBAL-DEFINE OUT PUT STREAM WebStream UNFORMATTED
{&OUT} cName.
{&OUT} \"static\" SKIP.
";
    let diags = check_expanded(run, &[], main);
    assert_eq!(diags.len(), 1, "{diags:?}");
}

#[test]
fn reports_a_written_out_put_to_the_web_stream() {
    let src = "\
PUT STREAM WebStream UNFORMATTED \"<b>\" + cName + \"</b>\".
PUT STREAM WebStream cName.
PUT STREAM webstream UNFORMATTED \"<b>\" cName SKIP.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn other_streams_and_literals_are_not_reported() {
    let src = "\
PUT STREAM WebStream UNFORMATTED \"<b>\" SKIP.
PUT STREAM sLog UNFORMATTED cName.
PUT UNFORMATTED cName.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_body_writes_through_a_response_writer() {
    let src = "\
oWriter:Write(cBody).
oResponse:Write(\"<p>\" + cName + \"</p>\").
THIS-OBJECT:oResp:Write(cBody).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "Write(cBody)",
            "Write(\"<p>\" + cName + \"</p>\")",
            "Write(cBody)"
        ]
    );
}

#[test]
fn a_literal_write_or_another_writer_is_not_reported() {
    let src = "\
oWriter:Write(\"<p>ok</p>\").
oLog:Write(cLine).
oStream:Write(cLine).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_the_entity_of_a_response() {
    let src = "\
oResponse:Entity = NEW OpenEdge.Core.String(cHtml).
oResponse:Entity = \"<p>\" + cName.
ASSIGN oResp:Entity = cBody.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn an_entity_of_literals_or_json_model_objects_is_not_reported() {
    let src = "\
oResponse:Entity = NEW OpenEdge.Core.String(\"<p>ok</p>\").
oResponse:Entity = NEW Progress.Json.ObjectModel.JsonObject().
oOther:Entity = cBody.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn json_built_by_concatenation_is_reported() {
    let src = "oWriter:Write('{\"name\":\"' + cName + '\"}').\n";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn reports_in_class_members_and_blocks() {
    let src = "\
CLASS Acme.Handler:
    METHOD PUBLIC VOID Render(INPUT pcName AS CHARACTER):
        DO ON ERROR UNDO, LEAVE:
            oResponse:Write(pcName).
        END.
    END METHOD.
END CLASS.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("page.i", "{&OUT} cName.\n")];
    let main = "&GLOBAL-DEFINE OUT PUT STREAM WebStream UNFORMATTED\n{page.i}\n";
    assert_eq!(check_expanded(run, &files, main).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_constant() {
    let main = "\
&GLOBAL-DEFINE TITLE \"<h1>Welcome</h1>\"
oWriter:Write({&TITLE}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn the_help_names_the_safe_patterns() {
    assert!(
        HELP.contains("HTML-ENCODE") || HELP.contains("encode"),
        "{HELP}"
    );
    assert!(HELP.contains("JsonObject"), "{HELP}");
}
