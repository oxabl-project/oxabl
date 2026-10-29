//! `client-identity` (ABLSEC010): a decision that reads who the client says
//! it is. One finding per routine and identity origin, in taint mode only.

use oxabl_common::{Diagnostic, SecurityMode, SecurityOptions, SecurityValidator};

use super::taint::{run_with, taint};

/// `options` in taint mode.
fn check_with(src: &str, options: SecurityOptions) -> Vec<Diagnostic> {
    run_with(
        "ABLSEC010",
        src,
        SecurityOptions {
            mode: SecurityMode::Taint,
            ..options
        },
    )
}

fn check(src: &str) -> Vec<Diagnostic> {
    run_with("ABLSEC010", src, taint())
}

/// The 1-based line of each related location of a finding.
fn related(src: &str, d: &Diagnostic) -> Vec<usize> {
    d.related()
        .iter()
        .map(|l| src[..l.span.span.start as usize].matches('\n').count() + 1)
        .collect()
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
    assert_eq!(lines(src, &diags), [3, 6], "{diags:?}");
    assert!(diags[0].message.contains("`cUser`"), "{}", diags[0].message);
    assert!(
        diags[0].message.contains("2 decisions"),
        "{}",
        diags[0].message
    );
    assert_eq!(related(src, &diags[0]), [7]);
    assert!(
        diags[1].message.contains("1 decision"),
        "{}",
        diags[1].message
    );
    assert!(diags[1].related().is_empty());
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
    let diags = check(src);
    assert_eq!(lines(src, &diags), [3]);
    assert_eq!(related(src, &diags[0]), [4, 5]);
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
    let diags = check_with(src, other);
    assert_eq!(
        lines(src, &diags),
        [3],
        "`all` is every injection kind, not identity"
    );
    assert_eq!(related(src, &diags[0]), [4]);
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

#[test]
fn many_decisions_on_one_origin_in_a_routine_are_one_finding() {
    let src = "\
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
cCompany = get-value(\"company\").
FIND FIRST Customer NO-LOCK WHERE Customer.Company = cCompany NO-ERROR.
FOR EACH Order NO-LOCK WHERE Order.Company = cCompany:
END.
IF cCompany = \"\" THEN RETURN.
FIND FIRST Item NO-LOCK WHERE Item.Company = cCompany NO-ERROR.
";
    let diags = check(src);
    assert_eq!(lines(src, &diags), [3], "{diags:?}");
    let d = &diags[0];
    assert!(src[d.span.span.start as usize..].starts_with("FIND"));
    assert!(
        d.message.contains("web input `get-value` for `company`"),
        "{}",
        d.message
    );
    assert!(d.message.contains("4 decisions"), "{}", d.message);
    assert_eq!(related(src, d), [4, 6, 7]);
}

#[test]
fn two_origins_in_a_routine_are_two_findings() {
    let src = "\
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRole AS CHARACTER NO-UNDO.
cUser = get-value(\"user\").
cRole = get-value(\"role\").
IF cUser = \"x\" THEN RETURN.
IF cRole = \"admin\" THEN RUN grant.
IF cUser = \"y\" THEN RETURN.
";
    let diags = check(src);
    assert_eq!(lines(src, &diags), [5, 6], "{diags:?}");
    assert_eq!(related(src, &diags[0]), [7]);
}

#[test]
fn one_origin_read_in_two_routines_is_a_finding_in_each() {
    let src = "\
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = get-value(\"user\").
IF cUser = \"x\" THEN RUN a.
PROCEDURE a:
    IF cUser = \"admin\" THEN RUN grant.
    IF cUser = \"root\" THEN RUN grant.
END PROCEDURE.
";
    let diags = check(src);
    assert_eq!(lines(src, &diags), [3, 5], "{diags:?}");
    assert_eq!(related(src, &diags[1]), [6]);
}

#[test]
fn the_related_decisions_are_capped_and_the_message_has_the_count() {
    let mut src =
        String::from("DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.\ncUser = get-value(\"user\").\n");
    for i in 0..25 {
        src.push_str(&format!("IF cUser = \"u{i}\" THEN RETURN.\n"));
    }
    let diags = check(&src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(diags[0].related().len(), 20);
    assert!(
        diags[0].message.contains("25 decisions"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_decision_added_below_the_first_does_not_move_the_finding() {
    let src = "\
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = get-value(\"user\").
IF cUser = \"x\" THEN RETURN.
";
    let more = format!("{src}IF cUser = \"y\" THEN RETURN.\n");
    let (a, b) = (check(src), check(&more));
    assert_eq!(a.len(), 1);
    assert_eq!(b.len(), 1);
    assert_eq!(a[0].span, b[0].span);
}

#[test]
fn the_rule_is_silent_in_sink_mode() {
    let src = "IF get-value(\"role\") = \"admin\" THEN RUN purge-all.\n";
    assert!(run_with("ABLSEC010", src, SecurityOptions::default()).is_empty());
    let mut per_rule = SecurityOptions::default();
    per_rule
        .modes
        .push(("client-identity".into(), SecurityMode::Taint));
    assert_eq!(run_with("ABLSEC010", src, per_rule).len(), 1);
    let off = SecurityOptions {
        modes: vec![("ablsec010".into(), SecurityMode::Sink)],
        ..taint()
    };
    assert!(run_with("ABLSEC010", src, off).is_empty());
}

/// A main block that reads the company from the request, with `n` variables
/// chained to each other, so that its solve costs more than a small budget;
/// a function that decides twice on the main block's company, one that
/// decides on a constant, and a procedure that decides on the company the
/// main block passes it.
fn heavy_company(n: usize) -> String {
    let mut src = String::from("DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.\n");
    for i in 0..n {
        src.push_str(&format!("DEFINE VARIABLE d{i} AS CHARACTER NO-UNDO.\n"));
    }
    src.push_str("cCompany = get-value(\"company\").\n");
    for i in 0..n {
        src.push_str(&format!("d{i} = d{} + cCompany.\n", (i + 1) % n));
    }
    src.push_str(
        "\
FUNCTION f RETURNS LOGICAL ():
  FIND FIRST Customer NO-LOCK WHERE Customer.Name = cCompany NO-ERROR.
  IF cCompany = \"x\" THEN RETURN TRUE.
  RETURN FALSE.
END FUNCTION.
FUNCTION g RETURNS LOGICAL ():
  DEFINE VARIABLE cLocal AS CHARACTER NO-UNDO.
  cLocal = \"x\".
  IF cLocal = \"y\" THEN RETURN TRUE.
  RETURN FALSE.
END FUNCTION.
PROCEDURE pick:
  DEFINE INPUT PARAMETER pcCompany AS CHARACTER NO-UNDO.
  FIND FIRST Customer NO-LOCK WHERE Customer.Name = pcCompany NO-ERROR.
END PROCEDURE.
IF cCompany = \"acme\" THEN RUN pick (cCompany).
",
    );
    src
}

#[test]
fn a_routine_the_budget_leaves_unjudged_gets_one_finding_that_says_so() {
    let src = heavy_company(100);
    let judged = check(&src);
    assert_eq!(judged.len(), 3, "{judged:?}");
    assert!(
        judged.iter().all(|d| d.message.contains("client supplied")),
        "{judged:?}"
    );
    let unjudged = check_with(
        &src,
        SecurityOptions {
            work_budget: Some(5_000),
            ..SecurityOptions::default()
        },
    );
    let at: Vec<&str> = unjudged
        .iter()
        .map(|d| {
            src[d.span.span.start as usize..]
                .split_whitespace()
                .next()
                .unwrap()
        })
        .collect();
    assert_eq!(at, ["FIND", "FIND", "IF"], "{unjudged:?}");
    for d in &unjudged {
        assert!(
            d.message
                .contains("authorization could not be judged: analysis budget exceeded"),
            "{}",
            d.message
        );
        assert!(
            d.help.as_deref().is_some_and(|h| h.contains("split it")),
            "{:?}",
            d.help
        );
    }
    assert!(
        unjudged[0].message.contains("2 decisions"),
        "{}",
        unjudged[0].message
    );
    assert!(
        unjudged[1].message.contains("1 decision"),
        "{}",
        unjudged[1].message
    );
    assert!(
        unjudged[2].message.contains("1 decision"),
        "{}",
        unjudged[2].message
    );
}

/// A function that reads the company from the request and returns it, with
/// `n` variables chained to each other, so that its solve costs more than a
/// small budget; a procedure that decides on its parameter; and a main block
/// that decides on the function's result and passes it to the procedure.
fn heavy_callee(n: usize) -> String {
    let mut src = String::from(
        "FUNCTION company RETURNS CHARACTER ():\n  \
           DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.\n",
    );
    for i in 0..n {
        src.push_str(&format!("  DEFINE VARIABLE d{i} AS CHARACTER NO-UNDO.\n"));
    }
    src.push_str("  cCompany = get-value(\"company\").\n");
    for i in 0..n {
        src.push_str(&format!("  d{i} = d{} + cCompany.\n", (i + 1) % n));
    }
    src.push_str(
        "  \
  RETURN cCompany.
END FUNCTION.
PROCEDURE pick:
  DEFINE INPUT PARAMETER pcCompany AS CHARACTER NO-UNDO.
  FIND FIRST Customer NO-LOCK WHERE Customer.Name = pcCompany NO-ERROR.
END PROCEDURE.
IF company() = \"acme\" THEN RUN pick (company()).
",
    );
    src
}

#[test]
fn a_call_of_a_routine_the_budget_leaves_unjudged_is_not_judged_either() {
    let src = heavy_callee(100);
    let judged = check(&src);
    assert_eq!(lines(&src, &judged).len(), 2, "{judged:?}");
    assert!(
        judged.iter().all(|d| d.message.contains("client supplied")),
        "{judged:?}"
    );
    let unjudged = check_with(
        &src,
        SecurityOptions {
            work_budget: Some(5_000),
            ..SecurityOptions::default()
        },
    );
    let at: Vec<&str> = unjudged
        .iter()
        .map(|d| {
            src[d.span.span.start as usize..]
                .split_whitespace()
                .next()
                .unwrap()
        })
        .collect();
    assert_eq!(at, ["FIND", "IF"], "{unjudged:?}");
    assert!(
        unjudged
            .iter()
            .all(|d| d.message.contains("could not be judged")),
        "{unjudged:?}"
    );
}

#[test]
fn a_file_whose_summaries_do_not_settle_in_the_rounds_is_not_judged() {
    // Each procedure carries the company one variable further, so each round
    // of summaries moves it one step: more steps than rounds.
    let n = 12;
    let mut src = String::new();
    for i in 0..=n {
        src.push_str(&format!("DEFINE VARIABLE g{i} AS CHARACTER NO-UNDO.\n"));
    }
    for i in 0..n {
        src.push_str(&format!(
            "PROCEDURE p{i}:\n  g{} = g{i}.\nEND PROCEDURE.\n",
            i + 1
        ));
    }
    src.push_str("g0 = get-value(\"company\").\n");
    for i in (0..n).rev() {
        src.push_str(&format!("RUN p{i}.\n"));
    }
    src.push_str("IF g0 = \"acme\" THEN MESSAGE \"a\".\n");
    src.push_str(&format!("IF g{n} = \"acme\" THEN MESSAGE \"b\".\n"));
    let diags = check(&src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    assert!(diags[0].message.contains("client supplied"), "{diags:?}");
    // No routine ran past its budget: the message and the help say why.
    assert!(
        diags[1].message.contains(
            "authorization could not be judged: the values of this file did not settle in the \
             analysis"
        ),
        "{diags:?}"
    );
    let help = diags[1].help.as_deref().unwrap_or_default();
    assert!(
        help.contains("more routines") && !help.contains("split"),
        "{help}"
    );
}
