//! `hardcoded-secret` (ABLSEC008).

use oxabl_lint::security::hardcoded_secret::{HELP, run, run_with};
use oxabl_lint::security::names::Heuristics;

use super::{check, check_expanded, flagged};

#[test]
fn reports_a_connect_with_a_quoted_password() {
    let src = "CONNECT -db sales -H host -S 1234 -U bob -P \"s3cret\".\n";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(flagged(src, &diags), ["-P \"s3cret\""]);
}

#[test]
fn reports_a_connect_with_a_bare_password() {
    assert_eq!(check(run, "CONNECT -db sales -U bob -P s3cret.\n").len(), 1);
}

#[test]
fn reports_a_password_inside_a_connect_string() {
    assert_eq!(
        check(run, "CONNECT VALUE(\"-db sales -U bob -P s3cret\").\n").len(),
        1
    );
}

#[test]
fn reports_a_password_in_a_connect_method_call() {
    assert_eq!(
        check(run, "hSrv:CONNECT(\"-AppService app -U bob -P s3cret\").\n").len(),
        1
    );
}

#[test]
fn a_connect_with_a_run_time_password_is_not_reported() {
    let src = "\
CONNECT -db sales -U bob -P VALUE(cPw).
CONNECT VALUE(\"-db sales -U bob -P \" + cPw).
CONNECT VALUE(cParams).
CONNECT -db sales -U bob.
hSrv:CONNECT(\"-AppService app -U bob -P \" + cPw).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn the_message_never_holds_the_secret() {
    let sources = [
        "CONNECT -db sales -U bob -P \"hunter2hunter2\".\n",
        "cPassword = \"hunter2hunter2\".\n",
        "cOut = ENCRYPT(cData, \"hunter2hunter2\").\n",
        "cX = \"oech1::hunter2hunter2\".\n",
    ];
    for src in sources {
        let diags = check(run, src);
        assert_eq!(diags.len(), 1, "{src}");
        let message = &diags[0].message;
        assert!(!message.contains("hunter2"), "{message}");
        assert!(message.contains("****"), "{message}");
    }
}

#[test]
fn reports_an_oech1_literal_anywhere() {
    let src = "\
cX = \"oech1::6a7b8c9d\".
DEFINE VARIABLE cY AS CHARACTER NO-UNDO INITIAL \"OECH1::abcdef\".
oCfg:Value = \"prefix oech1::abcdef\".
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn an_oech1_password_in_a_connect_is_reported_once() {
    let src = "\
CONNECT -db sales -U bob -P \"oech1::6a7b8c\".
CONNECT -db sales -U bob -P oech1::6a7b8c.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn oech1_inside_a_comment_is_not_reported() {
    assert!(check(run, "/* oech1::abc */\n// \"oech1::abc\"\nMESSAGE \"x\".\n").is_empty());
}

#[test]
fn reports_literal_key_material() {
    let src = "\
cOut = ENCRYPT(cData, \"k3y-k3y-k3y\").
cPlain = DECRYPT(cData, \"k3y-k3y-k3y\").
cPlain2 = DECRYPT(cData, \"k3y\" + \"k3y\", rIv).
rKey = GENERATE-PBE-KEY(\"passphrase\", rSalt).
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = \"literal-key\".
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 5, "{diags:?}");
}

#[test]
fn run_time_key_material_is_not_reported() {
    let src = "\
cOut = ENCRYPT(cData, cKey).
cPlain = DECRYPT(cData, GET-KEY()).
rKey = GENERATE-PBE-KEY(cPass, rSalt).
rKey2 = GENERATE-PBE-KEY(cPass).
cOut2 = ENCRYPT(\"literal data\", cKey).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_password_like_names_assigned_a_literal() {
    let src = "\
cPassword = \"hunter2\".
ASSIGN cDbPwd = \"x1y2\".
DEFINE VARIABLE cApiSecret AS CHARACTER NO-UNDO INITIAL \"abc123\".
oCfg:Password = \"hunter2\".
pcPasswd = \"a\" + \"b\".
";
    assert_eq!(check(run, src).len(), 5);
}

#[test]
fn other_names_and_other_values_are_not_reported() {
    let src = "\
cUser = \"admin\".
cPassenger = \"Bob\".
cPassword = cOther.
cPassword = \"\".
cPassword = ?.
cPasswordLabel = \"Password\".
lcPasswordPrompt = \"Enter your password\".
cKey = \"abc\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn the_name_lists_can_be_replaced() {
    let src = "cMagicPhrase = \"abra\".\ncPassword = \"hunter2\".\n";
    assert_eq!(flagged(src, &check(run, src)), ["cPassword = \"hunter2\""]);
    let custom = Heuristics {
        secret_words: vec!["phrase".to_string()],
        ..Heuristics::default()
    };
    let diags = check(|p, _, ctx| run_with(p, ctx, &custom), src);
    assert_eq!(flagged(src, &diags), ["cMagicPhrase = \"abra\""]);
}

#[test]
fn a_define_that_expands_to_a_secret_is_reported_at_the_assignment() {
    let main = "\
&GLOBAL-DEFINE PW \"hunter2\"
cPassword = {&PW}.
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("conn.i", "CONNECT -db sales -U bob -P \"s3cret\".\n")];
    assert_eq!(check_expanded(run, &files, "{conn.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_pattern() {
    assert!(
        HELP.contains("run time") || HELP.contains("runtime"),
        "{HELP}"
    );
}
