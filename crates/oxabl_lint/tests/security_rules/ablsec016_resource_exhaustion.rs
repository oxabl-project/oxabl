//! `resource-exhaustion` (ABLSEC016).

use oxabl_common::Severity;
use oxabl_lint::security::resource_exhaustion::{DEFAULT_SEVERITY, HELP, run};

use super::{check, check_expanded, check_loose, flagged};

#[test]
fn reports_set_size_from_a_non_literal() {
    let src = "\
SET-SIZE(mBuf) = iLen.
SET-SIZE(mBuf) = iHeader + 4.
ASSIGN SET-SIZE(mBuf) = iLen.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(flagged(src, &diags)[0], "SET-SIZE(mBuf) = iLen");
}

#[test]
fn set_size_from_a_literal_is_not_reported() {
    let src = "SET-SIZE(mBuf) = 4096.\nSET-SIZE(mBuf) = 0.\nSET-SIZE(mBuf) = 1024 * 4.\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_a_socket_read_with_an_uncapped_size() {
    let src = "\
hSock:READ(mBuf, 1, iLen, READ-EXACT-NUM).
hSock:READ(mBuf, 1, GET-SIZE(mHeader), READ-EXACT-NUM).
";
    assert_eq!(check_loose(run, src).len(), 2);
}

#[test]
fn a_literal_or_capped_socket_read_is_not_reported() {
    let src = "\
hSock:READ(mBuf, 1, 4096, READ-EXACT-NUM).
hSock:READ(mBuf, 1, MINIMUM(iLen, 65536), READ-EXACT-NUM).
hSock:READ(mBuf, 1, MIN(iLen, 65536), READ-AVAILABLE).
";
    assert!(check_loose(run, src).is_empty());
}

#[test]
fn reports_a_fill_with_no_batch_size_in_the_file() {
    let src = "\
hDs:FILL().
DATASET dsOrders:FILL().
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn a_batch_size_in_the_file_caps_the_fill() {
    let src = "\
hBuf:BATCH-SIZE = 100.
hDs:FILL().
";
    assert!(check(run, src).is_empty());
    let src = "\
ASSIGN hBuf:BATCH-SIZE = iPage.
hDs:FILL().
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_batch_size_of_zero_is_no_cap() {
    let src = "\
hBuf:BATCH-SIZE = 0.
hDs:FILL().
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn findings_are_info() {
    assert_eq!(DEFAULT_SEVERITY, Severity::Info);
    let diags = check(run, "hDs:FILL().\n");
    assert_eq!(diags[0].severity, Severity::Info);
}

#[test]
fn a_define_that_expands_to_a_literal_is_constant() {
    let main = "\
&GLOBAL-DEFINE BUFSIZE 4096
SET-SIZE(mBuf) = {&BUFSIZE}.
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("buf.i", "SET-SIZE(mBuf) = iLen.\n")];
    assert_eq!(check_expanded(run, &files, "{buf.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_safe_patterns() {
    assert!(HELP.contains("cap"), "{HELP}");
    assert!(HELP.contains("BATCH-SIZE"), "{HELP}");
}
