//! `client-identity` (ABLSEC010): a decision that reads who the client says
//! it is.

use oxabl_common::{Diagnostic, SecurityOptions, SecurityValidator};

use super::taint::run_with;

fn check_with(src: &str, options: SecurityOptions) -> Vec<Diagnostic> {
    run_with("ABLSEC010", src, options)
}

fn check(src: &str) -> Vec<Diagnostic> {
    check_with(src, SecurityOptions::default())
}

/// The 1-based line of each finding.
fn lines(src: &str, diags: &[Diagnostic]) -> Vec<usize> {
    diags
        .iter()
        .map(|d| src[..d.span.span.start as usize].matches('\n').count() + 1)
        .collect()
}

#[test]
fn a_role_from_the_request_decides_an_if() {
    let src = "\
IF get-value(\"role\") = \"admin\" THEN RUN purge-all.
IF get-value(\"page\") = \"2\" THEN RUN next-page.
";
    let diags = check(src);
    assert_eq!(lines(src, &diags), [1], "{diags:?}");
    let d = &diags[0];
    assert!(src[d.span.span.start as usize..].starts_with("IF"));
    assert!(d.message.contains("`role`"), "{}", d.message);
    assert!(d.help.as_deref().is_some_and(|h| h.contains("session")));
}

#[test]
fn a_user_held_in_a_variable_selects_records() {
    let src = "\
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = get-value(\"user\").
FOR EACH Order NO-LOCK WHERE Order.SalesRep = cUser:
    DISPLAY Order.OrderNum.
END.
FIND FIRST Customer NO-LOCK WHERE Customer.CustNum = INTEGER(get-field(\"company\")) NO-ERROR.
IF CAN-FIND(FIRST Customer WHERE Customer.SalesRep = cUser) THEN MESSAGE \"x\".
";
    let diags = check(src);
    assert_eq!(lines(src, &diags), [3, 6, 7], "{diags:?}");
    assert!(diags[0].message.contains("`cUser`"), "{}", diags[0].message);
}

#[test]
fn a_comparison_or_a_number_does_not_clean_an_identity() {
    let src = "\
DEFINE VARIABLE cRole AS CHARACTER NO-UNDO.
cRole = get-value(\"role\").
IF cRole <> \"admin\" AND cRole <> \"clerk\" THEN RETURN.
IF CAN-DO(\"admin,clerk\", cRole) THEN RUN grant.
CASE cRole:
    WHEN \"admin\" THEN RUN grant.
END CASE.
";
    assert_eq!(lines(src, &check(src)), [3, 4, 5]);
}

#[test]
fn request_objects_and_json_bodies_carry_identity_keys() {
    let src = "\
USING Progress.Json.ObjectModel.*.
DEFINE VARIABLE oBody AS JsonObject NO-UNDO.
DEFINE VARIABLE cTenant AS CHARACTER NO-UNDO.
IF poRequest:GetPathParameter(\"tenant\") <> cTenant THEN RETURN.
oBody = CAST(poRequest:Entity, JsonObject).
IF oBody:GetCharacter(\"userId\") = \"system\" THEN RUN elevate.
IF oBody:GetCharacter(\"comment\") = \"\" THEN RETURN.
";
    assert_eq!(lines(src, &check(src)), [4, 6]);
}

#[test]
fn the_identity_the_session_holds_is_not_the_clients() {
    let src = "\
IF USERID(\"sports\") = \"admin\" THEN RUN purge-all.
IF SESSION:CURRENT-REQUEST-INFO:ClientContextId = \"x\" THEN RUN purge-all.
MESSAGE get-value(\"user\").
";
    assert!(check(src).is_empty());
}

#[test]
fn identity_keys_and_sources_are_configurable() {
    let src = "\
IF get-value(\"branch\") = \"1\" THEN RUN a.
IF get-value(\"user\") = \"x\" THEN RUN b.
IF request-user() = \"admin\" THEN RUN c.
";
    let options = SecurityOptions {
        identity_keys: vec!["branch".into()],
        identity_sources: vec!["request-user".into()],
        ..SecurityOptions::default()
    };
    assert_eq!(lines(src, &check_with(src, options)), [1, 3]);
    assert_eq!(lines(src, &check(src)), [2]);
}

#[test]
fn a_validator_that_protects_identity_cleans_it() {
    let src = "\
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = get-value(\"user\").
IF NOT is-session-user(cUser) THEN RETURN.
IF cUser = \"admin\" THEN RUN grant.
";
    let options = SecurityOptions {
        validators: vec![SecurityValidator {
            name: "is-session-user".into(),
            protects: vec!["identity".into()],
        }],
        ..SecurityOptions::default()
    };
    assert!(check_with(src, options).is_empty());
    let other = SecurityOptions {
        validators: vec![SecurityValidator {
            name: "is-session-user".into(),
            protects: vec!["all".into()],
        }],
        ..SecurityOptions::default()
    };
    assert_eq!(
        lines(src, &check_with(src, other)),
        [3, 4],
        "`all` is every injection kind, not identity"
    );
}

#[test]
fn an_argument_of_an_authorisation_routine_is_a_decision() {
    let src = "\
RUN check-access (INPUT get-value(\"user\"), INPUT \"orders\").
RUN log-access (INPUT get-value(\"user\")).
";
    let mut options = SecurityOptions::default();
    options.reach.auth_routines = vec!["check-access".into()];
    let diags = check_with(src, options);
    assert_eq!(lines(src, &diags), [1], "{diags:?}");
    assert!(src[diags[0].span.span.start as usize..].starts_with("RUN"));
}

#[test]
fn a_function_of_the_file_that_returns_the_identity_is_followed() {
    let src = "\
FUNCTION request-role RETURNS CHARACTER PRIVATE ():
    RETURN get-value(\"role\").
END FUNCTION.
IF request-role() = \"admin\" THEN RUN grant.
";
    assert_eq!(lines(src, &check(src)), [4]);
}
