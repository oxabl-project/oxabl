/* expect: 3 */
USING System.Data.SqlClient.*.
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE oConn AS SqlConnection NO-UNDO.
DEFINE VARIABLE oCmd AS SqlCommand NO-UNDO.
DEFINE VARIABLE oAdapter AS SqlDataAdapter NO-UNDO.

oCmd = NEW SqlCommand("SELECT * FROM customers WHERE name = '" + pcName + "'", oConn).
oCmd = NEW System.Data.Odbc.OdbcCommand(pcName).
oAdapter = NEW SqlDataAdapter(pcName, oConn).
