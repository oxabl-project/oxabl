//! `path-traversal` (ABLSEC005).

use oxabl_lint::security::path_traversal::run;

use super::{check, check_expanded, flagged};

#[test]
fn reports_stream_files_named_by_a_non_literal() {
    let src = "\
INPUT FROM VALUE(pcFile).
OUTPUT TO VALUE(cDir + \"/x.txt\").
OUTPUT STREAM s TO VALUE(c) APPEND.
INPUT STREAM s FROM VALUE(c) NO-ECHO.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 4, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "VALUE(pcFile)",
            "VALUE(cDir + \"/x.txt\")",
            "VALUE(c)",
            "VALUE(c)"
        ]
    );
}

#[test]
fn reports_os_dir() {
    let src = "INPUT FROM OS-DIR(pcDir).";
    assert_eq!(flagged(src, &check(run, src)), ["OS-DIR(pcDir)"]);
    assert!(check(run, "INPUT FROM OS-DIR(\"/tmp\") NO-ATTR-LIST.").is_empty());
}

#[test]
fn reports_os_file_statements() {
    let src = "\
OS-DELETE VALUE(c).
OS-DELETE VALUE(c) RECURSIVE.
OS-CREATE-DIR VALUE(c).
OS-RENAME VALUE(a) VALUE(\"b\").
OS-COPY VALUE(a) VALUE(b).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 6, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "VALUE(c)", "VALUE(c)", "VALUE(c)", "VALUE(a)", "VALUE(a)", "VALUE(b)"
        ]
    );
}

#[test]
fn reports_copy_lob_files() {
    let src = "\
COPY-LOB FROM cData TO FILE pcPath.
COPY-LOB FILE pcPath TO cData.
COPY-LOB FROM FILE \"x\" TO FILE cDir + \"/x\" APPEND.
";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        ["FILE pcPath", "FILE pcPath", "FILE cDir + \"/x\""]
    );
}

#[test]
fn reports_file_info_assignments() {
    let src = "\
FILE-INFO:FILE-NAME = pcPath.
FILE-INFORMATION:FILE-NAME = cDir + \"/\" + c.
ASSIGN FILE-INFO:FILE-NAME = c.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn literal_paths_are_not_reported() {
    let src = "\
INPUT FROM \"x.txt\".
INPUT FROM VALUE(\"/a/b.txt\").
OUTPUT TO VALUE(\"a\" + \".txt\").
OS-DELETE \"x.txt\".
OS-DELETE VALUE(\"/tmp/a\").
OS-CREATE-DIR \"x\".
OS-COPY \"a\" \"b\".
COPY-LOB FROM cData TO FILE \"/tmp/x\".
COPY-LOB FROM cData TO cLob.
FILE-INFO:FILE-NAME = \"/tmp\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn unquoted_names_are_literal_paths() {
    let src = "\
INPUT FROM data.txt.
OS-DELETE cFile.
OS-CREATE-DIR newdir.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn closing_a_stream_is_not_reported() {
    assert!(check(run, "INPUT CLOSE. OUTPUT CLOSE.").is_empty());
}

#[test]
fn a_file_name_on_another_object_is_not_reported() {
    assert!(check(run, "hDoc:FILE-NAME = c.").is_empty());
}

#[test]
fn a_define_that_expands_to_a_variable_is_reported() {
    let main = "\
&GLOBAL-DEFINE TARGET cPath
OUTPUT TO VALUE({&TARGET}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_not() {
    let main = "\
&GLOBAL-DEFINE TARGET \"/tmp/out.txt\"
OUTPUT TO VALUE({&TARGET}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("out.i", "OUTPUT TO VALUE(cPath).\n")];
    assert_eq!(check_expanded(run, &files, "{out.i}\n").len(), 1);
}
