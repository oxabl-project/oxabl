//! `untrusted-xml` (ABLSEC015).

use oxabl_common::Severity;
use oxabl_lint::security::untrusted_xml::{DEFAULT_SEVERITY, HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_x_document_load_of_a_non_literal_source() {
    let src = "\
hDoc:LOAD(\"file\", cPath, FALSE).
hDoc:LOAD(\"longchar\", lcXml, FALSE).
hDoc:LOAD(\"memptr\", mXml, FALSE).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "LOAD(\"file\", cPath, FALSE)",
            "LOAD(\"longchar\", lcXml, FALSE)",
            "LOAD(\"memptr\", mXml, FALSE)"
        ]
    );
}

#[test]
fn a_literal_source_or_another_load_is_not_reported() {
    let src = "\
hDoc:LOAD(\"file\", \"/etc/app/config.xml\", FALSE).
hQuery:LOAD(cX).
oConfig:Load(\"file\").
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_each_sax_parse_start() {
    let src = "\
hReader:SAX-PARSE().
hReader:SAX-PARSE-FIRST().
hReader:SAX-PARSE() NO-ERROR.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn a_sax_parse_continuation_is_not_reported() {
    assert!(check(run, "hReader:SAX-PARSE-NEXT().\n").is_empty());
}

#[test]
fn findings_are_info() {
    assert_eq!(DEFAULT_SEVERITY, Severity::Info);
    let diags = check(run, "hReader:SAX-PARSE().\n");
    assert_eq!(diags[0].severity, Severity::Info);
}

#[test]
fn reports_in_blocks_and_expressions() {
    let src = "\
PROCEDURE parse:
    IF hDoc:LOAD(\"file\", cPath, FALSE) THEN MESSAGE \"ok\".
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("xml.i", "hDoc:LOAD(\"file\", cPath, FALSE).\n")];
    assert_eq!(check_expanded(run, &files, "{xml.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_pattern() {
    assert!(HELP.contains("DTD"), "{HELP}");
}
