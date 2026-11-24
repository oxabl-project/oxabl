//! `header-injection` (ABLSEC007).

use oxabl_lint::security::header_injection::{HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_set_header_and_cookie_calls_with_a_non_literal() {
    let src = "\
oResp:SetHeader(\"X-Name\", cValue).
oResp:SetCookie(\"sid\", cSid).
oResp:AddHeader(\"X-Trace\", \"id-\" + cId).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "SetHeader(\"X-Name\", cValue)",
            "SetCookie(\"sid\", cSid)",
            "AddHeader(\"X-Trace\", \"id-\" + cId)"
        ]
    );
}

#[test]
fn literal_headers_are_not_reported() {
    let src = "\
oResp:SetHeader(\"X-Frame-Options\", \"DENY\").
oResp:SetCookie(\"theme\", \"dark\").
oResp:SetHeader(NEW OpenEdge.Net.HTTP.HttpHeader(\"Cache-Control\", \"no-store\")).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_a_header_or_cookie_object_built_from_a_non_literal() {
    let src = "\
oLoc = NEW OpenEdge.Net.HTTP.HttpHeader(\"Location\", cUrl).
oCookie = NEW OpenEdge.Net.HTTP.Cookie(\"sid\", \"example.com\", \"/\", cSid).
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn a_header_object_passed_to_set_header_is_reported_once() {
    let src = "oResp:SetHeader(NEW OpenEdge.Net.HTTP.HttpHeader(\"Location\", cUrl)).\n";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn reports_the_header_builder_chain() {
    let src = "\
oH = OpenEdge.Net.HTTP.HttpHeaderBuilder:Build(\"Location\"):Value(cUrl):Header.
oH2 = HttpHeaderBuilder:Build(cName):Value(\"x\"):Header.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn reports_a_location_assignment() {
    let src = "\
oResp:Location = cUrl.
oResp:Location = \"/home?next=\" + cNext.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn a_literal_location_is_not_reported() {
    assert!(check(run, "oResp:Location = \"/home\".\n").is_empty());
}

#[test]
fn reports_the_webspeed_header_procedures() {
    let src = "\
RUN output-http-header (\"Set-Cookie\", cCookie).
RUN output-http-header IN web-utilities-hdl (\"Location\", cUrl).
RUN set-cookie (\"sid\", cSid, ?, ?, ?, ?, ?).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
}

#[test]
fn literal_webspeed_headers_are_not_reported() {
    let src = "\
RUN output-http-header (\"Content-Type\", \"text/html\").
RUN output-content-type (\"text/html\").
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_define_that_expands_to_a_literal_is_constant() {
    let main = "\
&GLOBAL-DEFINE HOME \"/home\"
oResp:Location = {&HOME}.
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("redir.i", "oResp:SetHeader(\"Location\", cUrl).\n")];
    assert_eq!(check_expanded(run, &files, "{redir.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_patterns() {
    assert!(HELP.contains("CR") && HELP.contains("LF"), "{HELP}");
    assert!(
        HELP.contains("allow-list") || HELP.contains("literal"),
        "{HELP}"
    );
}
