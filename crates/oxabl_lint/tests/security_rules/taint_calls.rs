//! Stage 2 across the calls of one file: a function or method returns
//! untrusted input, an internal procedure fills an `OUTPUT` parameter with it,
//! a caller passes it to a private routine, and a JSON object carries it from
//! `Add` to `Write`.

use super::taint::{run_with, taint};
use oxabl_common::Diagnostic;

fn check(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, taint())
}

/// The line of each finding, 1-based.
fn lines(src: &str, diags: &[Diagnostic]) -> Vec<usize> {
    diags
        .iter()
        .map(|d| src[..d.span.span.start as usize].matches('\n').count() + 1)
        .collect()
}

#[test]
fn a_function_returns_untrusted_input_to_its_caller() {
    let src = "\
FUNCTION get-filter RETURNS CHARACTER PRIVATE ():
    RETURN get-value(\"filter\").
END FUNCTION.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + get-filter()).
";
    let diags = check("ABLSEC001", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(
        diags[0].message.contains("web input `get-value`"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_function_that_cleans_its_argument_cleans_the_call() {
    let src = "\
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
FUNCTION quoted RETURNS CHARACTER PRIVATE (INPUT pc AS CHARACTER):
    RETURN QUOTER(pc).
END FUNCTION.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Name = \" + quoted(pcName)).
OS-COMMAND SILENT VALUE(\"echo \" + quoted(pcName)).
";
    assert!(check("ABLSEC001", src).is_empty());
    assert_eq!(check("ABLSEC003", src).len(), 1, "QUOTER does not clean a command");
}

#[test]
fn a_call_is_tainted_only_by_the_arguments_its_result_holds() {
    let src = "\
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
FUNCTION fixed-dir RETURNS CHARACTER (INPUT pc AS CHARACTER):
    RETURN \"/var/report\".
END FUNCTION.
FUNCTION under-tmp RETURNS CHARACTER (INPUT pc AS CHARACTER):
    RETURN \"/tmp/\" + pc.
END FUNCTION.
OUTPUT TO VALUE(fixed-dir(pcFile)).
OUTPUT TO VALUE(under-tmp(\"fixed.txt\")).
OUTPUT TO VALUE(under-tmp(pcFile)).
";
    let diags = check("ABLSEC005", src);
    assert_eq!(lines(src, &diags), [10], "{diags:?}");
}

#[test]
fn a_private_procedure_sees_what_its_callers_pass() {
    let src = "\
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
PROCEDURE compress PRIVATE:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    OS-COMMAND SILENT VALUE(\"gzip \" + pcName).
END PROCEDURE.
RUN compress (INPUT pcFile).
";
    let diags = check("ABLSEC003", src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(diags[0].message.contains("`pcName`"), "{}", diags[0].message);
    assert!(diags[0].message.contains("`pcFile`"), "{}", diags[0].message);

    let fixed = src.replace("RUN compress (INPUT pcFile).", "RUN compress (INPUT \"a.log\").");
    assert!(check("ABLSEC003", &fixed).is_empty());
}

#[test]
fn an_output_parameter_of_an_internal_procedure_carries_what_it_was_set_to() {
    let src = "\
DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSafe AS CHARACTER NO-UNDO.
RUN read-path (OUTPUT cPath).
RUN fixed-path (INPUT cPath, OUTPUT cSafe).
OUTPUT TO VALUE(cPath).
OUTPUT TO VALUE(cSafe).
PROCEDURE read-path PRIVATE:
    DEFINE OUTPUT PARAMETER pc AS CHARACTER NO-UNDO.
    pc = get-value(\"path\").
END PROCEDURE.
PROCEDURE fixed-path PRIVATE:
    DEFINE INPUT PARAMETER pcIn AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcOut AS CHARACTER NO-UNDO.
    pcOut = \"/var/out/report.txt\".
END PROCEDURE.
";
    let diags = check("ABLSEC005", src);
    assert_eq!(lines(src, &diags), [5], "{diags:?}");
}

#[test]
fn a_method_returns_untrusted_input_and_a_method_can_clean_it() {
    let src = "\
CLASS Finder:
    DEFINE PRIVATE VARIABLE hQ AS HANDLE NO-UNDO.
    METHOD PRIVATE CHARACTER ReadFilter():
        RETURN get-value(\"filter\").
    END METHOD.
    METHOD PRIVATE CHARACTER Quoted(INPUT pc AS CHARACTER):
        RETURN QUOTER(pc).
    END METHOD.
    METHOD PUBLIC VOID Open():
        hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Name = \" + ReadFilter()).
        hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Name = \" + THIS-OBJECT:Quoted(ReadFilter())).
    END METHOD.
END CLASS.
";
    let diags = check("ABLSEC001", src);
    assert_eq!(lines(src, &diags), [10], "{diags:?}");
}

#[test]
fn a_private_method_sees_what_other_methods_pass() {
    let src = "\
CLASS Exporter:
    METHOD PUBLIC VOID Save(INPUT pcName AS CHARACTER):
        THIS-OBJECT:WriteTo(pcName).
    END METHOD.
    METHOD PUBLIC VOID SaveFixed():
        WriteTo(\"fixed.txt\").
    END METHOD.
    METHOD PRIVATE VOID WriteTo(INPUT pcFile AS CHARACTER):
        OUTPUT TO VALUE(pcFile).
        OUTPUT CLOSE.
    END METHOD.
END CLASS.
";
    let diags = check("ABLSEC005", src);
    assert_eq!(lines(src, &diags), [9], "{diags:?}");
}

#[test]
fn a_chain_of_calls_carries_a_source_to_the_sink() {
    let src = "\
FUNCTION f3 RETURNS CHARACTER PRIVATE ():
    RETURN get-value(\"cmd\").
END FUNCTION.
FUNCTION f2 RETURNS CHARACTER PRIVATE ():
    RETURN \"run \" + f3().
END FUNCTION.
FUNCTION f1 RETURNS CHARACTER PRIVATE ():
    RETURN f2().
END FUNCTION.
OS-COMMAND SILENT VALUE(f1()).
";
    assert_eq!(check("ABLSEC003", src).len(), 1);
}

#[test]
fn a_recursive_function_ends() {
    let src = "\
FUNCTION walk RETURNS CHARACTER PRIVATE (INPUT pc AS CHARACTER, INPUT pi AS INTEGER):
    IF pi > 3 THEN RETURN pc.
    RETURN walk(pc + \"/x\", pi + 1).
END FUNCTION.
OS-COMMAND SILENT VALUE(walk(get-value(\"d\"), 0)).
OS-COMMAND SILENT VALUE(walk(\"/tmp\", 0)).
";
    let diags = check("ABLSEC003", src);
    assert_eq!(lines(src, &diags), [5], "{diags:?}");
}

const JSON: &str = "\
USING Progress.Json.ObjectModel.*.
DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
DEFINE VARIABLE oList AS JsonArray NO-UNDO.
DEFINE VARIABLE lcBody AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcList AS LONGCHAR NO-UNDO.
DEFINE VARIABLE oFixed AS JsonObject NO-UNDO.
DEFINE VARIABLE lcFixed AS LONGCHAR NO-UNDO.
oJson = NEW JsonObject().
oJson:Add(\"name\", get-value(\"name\")).
oJson:Write(lcBody).
PUT STREAM WebStream UNFORMATTED lcBody.
oList = NEW JsonArray().
oList:Add(oJson).
lcList = oList:GetJsonText().
oWriter:Write(lcList).
oFixed = NEW JsonObject().
oFixed:Set(\"status\", \"ok\").
oFixed:Write(lcFixed, TRUE).
PUT STREAM WebStream UNFORMATTED lcFixed.
";

#[test]
fn a_json_object_carries_what_is_added_to_it_to_its_text() {
    let diags = check("ABLSEC006", JSON);
    assert_eq!(lines(JSON, &diags), [11, 15], "{diags:?}");
    assert!(diags[0].message.contains("`lcBody`"), "{}", diags[0].message);
    assert!(
        diags[0].message.contains("web input `get-value`"),
        "{}",
        diags[0].message
    );
}

#[test]
fn json_write_with_an_output_keyword_and_set_carry_taint() {
    let src = "\
USING Progress.Json.ObjectModel.*.
DEFINE INPUT PARAMETER pcNote AS CHARACTER NO-UNDO.
DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
DEFINE VARIABLE lcBody AS LONGCHAR NO-UNDO.
oJson = NEW JsonObject().
oJson:Set(\"note\", pcNote).
oJson:Write(INPUT-OUTPUT lcBody).
PUT STREAM WebStream UNFORMATTED lcBody.
";
    assert_eq!(check("ABLSEC006", src).len(), 1);
}
