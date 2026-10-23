//! `information-leak` (ABLSEC013).

use oxabl_lint::security::information_leak::{HELP, run};

use super::{check, check_expanded};

const OUTPUT_PARAM: &str = "DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.\n";

fn in_catch(body: &str) -> String {
    format!(
        "{OUTPUT_PARAM}DO ON ERROR UNDO, LEAVE:\n    RUN work.p.\n    CATCH e AS Progress.Lang.Error:\n        {body}\n    END CATCH.\nEND.\n"
    )
}

#[test]
fn reports_error_detail_assigned_to_an_output_parameter() {
    for body in [
        "pcError = e:GetMessage(1).",
        "pcError = ERROR-STATUS:GET-MESSAGE(1).",
        "pcError = \"Failed: \" + e:CallStack.",
        "ASSIGN pcError = e:GetMessage(1).",
        "pcError = SUBSTITUTE(\"Failed: &1\", e:GetMessage(1)).",
    ] {
        assert_eq!(check(run, &in_catch(body)).len(), 1, "{body}");
    }
}

#[test]
fn reports_an_output_parameter_of_a_method() {
    let src = "\
CLASS Acme.Svc:
    METHOD PUBLIC VOID Run(OUTPUT pcError AS CHARACTER):
        DO ON ERROR UNDO, LEAVE:
            RUN work.p.
            CATCH e AS Progress.Lang.Error:
                pcError = e:GetMessage(1).
            END CATCH.
        END.
    END METHOD.
END CLASS.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_generic_message_or_a_local_variable_is_not_reported() {
    for body in [
        "pcError = \"The request failed\".",
        "cMsg = e:GetMessage(1).",
        "LOG-MANAGER:WRITE-MESSAGE(e:GetMessage(1)).",
    ] {
        assert!(check(run, &in_catch(body)).is_empty(), "{body}");
    }
}

#[test]
fn reports_error_detail_written_to_a_web_response() {
    for body in [
        "{&OUT} e:GetMessage(1).",
        "{&OUT} \"<p>\" ERROR-STATUS:GET-MESSAGE(1) \"</p>\".",
        "PUT STREAM WebStream UNFORMATTED e:CallStack.",
        "oResp:Write(e:GetMessage(1)).",
        "oResponse:Entity = NEW OpenEdge.Core.String(e:GetMessage(1)).",
    ] {
        assert_eq!(check(run, &in_catch(body)).len(), 1, "{body}");
    }
}

#[test]
fn an_unrelated_web_write_is_not_reported() {
    for body in [
        "{&OUT} \"<p>\" cMsg \"</p>\".",
        "oResp:Write(\"The request failed\").",
        "oResp:Write(cMsg).",
    ] {
        assert!(check(run, &in_catch(body)).is_empty(), "{body}");
    }
}

#[test]
fn get_message_of_an_unknown_object_is_not_reported() {
    let src =
        "DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.\npcError = oThing:GetMessage(1).\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_error_typed_variable_counts_as_a_caught_error() {
    let src = "\
DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.
pcError = oErr:GetMessage(1).
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn reports_password_like_names_passed_to_message() {
    let src = "\
MESSAGE \"login with \" + cPassword.
MESSAGE cUser pcPwd.
MESSAGE SUBSTITUTE(\"pw=&1\", cDbPassword).
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reports_password_like_names_passed_to_the_log_manager() {
    let src = "\
LOG-MANAGER:WRITE-MESSAGE(\"pw=\" + cPassword).
LOG-MANAGER:WRITE-MESSAGE(cSecret, \"AUTH\").
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn other_messages_are_not_reported() {
    let src = "\
MESSAGE \"Password changed\".
MESSAGE cUser.
MESSAGE cPasswordPrompt.
LOG-MANAGER:WRITE-MESSAGE(\"login ok for \" + cUser).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("log.i", "MESSAGE cPassword.\n")];
    assert_eq!(check_expanded(run, &files, "{log.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_pattern() {
    assert!(HELP.contains("generic"), "{HELP}");
    assert!(HELP.contains("log"), "{HELP}");
}
