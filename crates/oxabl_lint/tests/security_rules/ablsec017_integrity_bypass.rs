//! `integrity-bypass` (ABLSEC017).

use oxabl_lint::security::integrity_bypass::{HELP, run};

use super::{check, check_expanded, flagged};

#[test]
fn reports_disable_triggers() {
    let src = "\
DISABLE TRIGGERS FOR LOAD OF Customer.
DISABLE TRIGGERS FOR DUMP OF Customer.
disable triggers for load of Order ALLOW-REPLICATION.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags)[0],
        "DISABLE TRIGGERS FOR LOAD OF Customer"
    );
}

#[test]
fn other_disable_statements_are_not_reported() {
    let src = "DISABLE btnOk WITH FRAME f1.\nDISABLE ALL.\n";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_writes_to_the_user_table() {
    let src = "\
CREATE _User.
DELETE _User.
create _user.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reports_each_assignment_to_a_user_field() {
    let src = "\
_User._Userid = cName.
ASSIGN _User._Userid = \"bob\" _User._Password = ENCODE(\"x\").
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reports_writes_through_a_buffer_of_the_user_table() {
    let src = "\
DEFINE BUFFER bUser FOR _User.
CREATE bUser.
bUser._Userid = cName.
DELETE bUser.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reads_and_other_tables_are_not_reported() {
    let src = "\
FIND FIRST _User NO-LOCK NO-ERROR.
FOR EACH _User NO-LOCK: DISPLAY _User._Userid. END.
CREATE Customer.
DELETE Customer.
Customer.Name = cName.
DEFINE BUFFER bCust FOR Customer.
CREATE bCust.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("load.i", "DISABLE TRIGGERS FOR LOAD OF Customer.\n")];
    assert_eq!(check_expanded(run, &files, "{load.i}\n").len(), 1);
}

#[test]
fn the_help_names_the_allow_list() {
    assert!(
        HELP.contains("allow-list") || HELP.contains("override"),
        "{HELP}"
    );
}
