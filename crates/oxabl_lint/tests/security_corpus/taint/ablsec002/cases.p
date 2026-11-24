/* foreign-sql (ABLSEC002): stage 2 cases. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttArg NO-UNDO FIELD cValue AS CHARACTER.
DEFINE VARIABLE oCmd AS System.Data.SqlClient.SqlCommand NO-UNDO.
DEFINE VARIABLE cSql AS CHARACTER NO-UNDO.
DEFINE VARIABLE iId AS INTEGER NO-UNDO.

/* Tainted, directly. */
oCmd = NEW System.Data.SqlClient.SqlCommand("SELECT * FROM t WHERE name = '" + pcName + "'"). /* taint: real */

/* Tainted through an assignment chain. */
cSql = "SELECT * FROM t WHERE name = '" + pcName + "'".
oCmd:CommandText = cSql. /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttArg.
ttArg.cValue = pcName.
oCmd:CommandText = "SELECT * FROM t WHERE name = '" + ttArg.cValue + "'". /* taint: real */

/* Tainted through an unknown call. */
oCmd:CommandText = make-sql(pcName). /* taint: real */

/* Sanitised for the wrong sink: QUOTER protects an ABL query, not foreign SQL. */
oCmd:CommandText = "SELECT * FROM t WHERE name = " + QUOTER(pcName, "'"). /* taint: real */

/* Sanitised as a number. */
iId = INTEGER(pcName).
oCmd:CommandText = "SELECT * FROM t WHERE id = " + STRING(iId). /* taint: safe */

/* A constant held in a variable. */
cSql = "SELECT * FROM t".
oCmd:CommandText = cSql. /* taint: safe */
RUN STORED-PROC send-sql-statement (cSql). /* taint: safe */

/* An unknown call with constant arguments. */
oCmd:CommandText = make-sql("all"). /* taint: safe */

/* Tainted, through send-sql-statement. */
RUN STORED-PROC send-sql-statement ("SELECT * FROM t WHERE name = '" + pcName + "'"). /* taint: real */
