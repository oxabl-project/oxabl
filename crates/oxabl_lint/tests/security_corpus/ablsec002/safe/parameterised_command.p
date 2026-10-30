DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE oConn AS System.Data.SqlClient.SqlConnection NO-UNDO.
DEFINE VARIABLE oCmd AS System.Data.SqlClient.SqlCommand NO-UNDO.

oCmd = NEW System.Data.SqlClient.SqlCommand("SELECT * FROM customers WHERE name = @name", oConn).
oCmd:CommandText = "SELECT * FROM customers WHERE name = @name AND active = 1".
oCmd:Parameters:AddWithValue("@name", pcName).
