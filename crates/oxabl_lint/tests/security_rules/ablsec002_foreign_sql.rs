//! `foreign-sql` (ABLSEC002).

use oxabl_lint::security::foreign_sql::run;

use super::{check, check_expanded, flagged};

#[test]
fn reports_send_sql_statement_built_by_concatenation() {
    let src = "RUN STORED-PROC send-sql-statement hP = PROC-HANDLE NO-ERROR (\"SELECT * FROM t WHERE n = '\" + c + \"'\").";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        ["\"SELECT * FROM t WHERE n = '\" + c + \"'\""]
    );
}

#[test]
fn reports_variable_and_input_forms_and_qualified_names() {
    let src = "\
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (cSql).
RUN STORED-PROC send-sql-statement NO-ERROR (INPUT cSql).
RUN STORED-PROC dbname.send-sql-statement hP = PROC-HANDLE NO-ERROR (cSql).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(flagged(src, &diags), ["cSql", "cSql", "cSql"]);
}

#[test]
fn procedure_name_is_matched_in_any_case() {
    let src = "RUN STORED-PROC Send-SQL-Statement hP = PROC-HANDLE (c).";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn reports_dotnet_command_constructors() {
    let src = "\
USING System.Data.SqlClient.*.
oCmd = NEW SqlCommand(\"SELECT * FROM t WHERE n = '\" + c + \"'\", oConn).
oCmd2 = NEW System.Data.Odbc.OdbcCommand(cSql).
oAd = NEW System.Data.SqlClient.SqlDataAdapter(cSql, oConn).
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
}

#[test]
fn reports_command_text_assignments() {
    let src = "\
oCmd:CommandText = \"SELECT * FROM t WHERE id = \" + c.
oCmd:CommandText = cSql.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn literal_sql_is_not_reported() {
    let src = "\
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (\"SELECT COUNT(*) FROM t\").
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (\"SELECT \" + \"1\").
oCmd = NEW System.Data.SqlClient.SqlCommand(\"SELECT 1\", oConn).
oCmd:CommandText = \"SELECT * FROM t WHERE n = @n\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn parameterised_commands_are_not_reported() {
    let src = "\
oCmd = NEW System.Data.SqlClient.SqlCommand(\"SELECT * FROM t WHERE n = @n\", oConn).
oCmd:Parameters:AddWithValue(\"@n\", c).
RUN STORED-PROC get-orders hP = PROC-HANDLE (INPUT c).
CLOSE STORED-PROC get-orders WHERE PROC-HANDLE = hP.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn other_classes_are_not_reported() {
    let src = "oErr = NEW Progress.Lang.AppError(cMsg, 1).";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_define_that_expands_to_a_variable_is_reported() {
    let main = "\
&GLOBAL-DEFINE SQLTEXT cSql
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE ({&SQLTEXT}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_not() {
    let main = "\
&GLOBAL-DEFINE SQLTEXT \"SELECT 1\"
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE ({&SQLTEXT}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [(
        "sql.i",
        "RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (cSql).\n",
    )];
    assert_eq!(check_expanded(run, &files, "{sql.i}\n").len(), 1);
}
