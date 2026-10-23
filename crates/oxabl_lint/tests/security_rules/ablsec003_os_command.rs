//! `os-command` (ABLSEC003).

use oxabl_lint::security::os_command::run;

use super::{check, check_expanded, flagged};

#[test]
fn reports_os_command_value() {
    let src = "\
OS-COMMAND SILENT VALUE(\"ls \" + c).
OS-COMMAND VALUE(cCmd).
OS-COMMAND NO-WAIT VALUE(cCmd).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(flagged(src, &diags), ["\"ls \" + c", "cCmd", "cCmd"]);
}

#[test]
fn reports_each_non_literal_operand() {
    let src = "OS-COMMAND SILENT VALUE(\"tar\") VALUE(cArg) VALUE(\"-x\") VALUE(\"a\" + c).";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["cArg", "\"a\" + c"]);
}

#[test]
fn reports_through_streams() {
    let src = "\
INPUT THROUGH VALUE(cCmd).
OUTPUT THROUGH VALUE(\"lpr \" + cPrinter).
INPUT-OUTPUT THROUGH VALUE(cCmd).
INPUT STREAM s THROUGH VALUE(cCmd).
OUTPUT STREAM s THROUGH VALUE(cCmd) APPEND.
";
    assert_eq!(check(run, src).len(), 5);
}

#[test]
fn reports_an_argument_after_a_literal_program() {
    let src = "INPUT THROUGH cat VALUE(pcFile) NO-ECHO.";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["pcFile"]);
}

#[test]
fn reports_unix_and_dos() {
    let src = "\
UNIX SILENT VALUE(\"rm \" + c).
UNIX VALUE(c).
DOS SILENT VALUE(c).
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn a_convert_clause_is_not_part_of_the_command() {
    let src = "INPUT THROUGH cat VALUE(\"/etc/motd\") CONVERT TARGET VALUE(cCp).";
    assert!(check(run, src).is_empty());
}

#[test]
fn fixed_commands_are_not_reported() {
    let src = "\
OS-COMMAND SILENT ls -l.
OS-COMMAND SILENT cmd-name.
OS-COMMAND SILENT VALUE(\"ls -l\").
OS-COMMAND SILENT VALUE(\"ls\" + \" -l\").
UNIX SILENT ls.
INPUT THROUGH ls -l.
INPUT THROUGH VALUE(\"date\" + \" +%Y\").
OUTPUT THROUGH lpr.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_allow_listed_choice_is_not_reported() {
    let src = "\
CASE pcAction:
    WHEN \"a\" THEN OS-COMMAND SILENT VALUE(\"backup-a.sh\").
    WHEN \"b\" THEN OS-COMMAND SILENT VALUE(\"backup-b.sh\").
END CASE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_in_nested_blocks() {
    let src = "\
PROCEDURE p:
    IF c > \"\" THEN DO:
        OS-COMMAND SILENT VALUE(c).
    END.
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_variable_is_reported() {
    let main = "\
&GLOBAL-DEFINE CMD cCmd
OS-COMMAND SILENT VALUE({&CMD}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_not() {
    let main = "\
&GLOBAL-DEFINE CMD \"ls -l\"
OS-COMMAND SILENT VALUE({&CMD}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("cmd.i", "OS-COMMAND SILENT VALUE(cCmd).\n")];
    assert_eq!(check_expanded(run, &files, "{cmd.i}\n").len(), 1);
}
