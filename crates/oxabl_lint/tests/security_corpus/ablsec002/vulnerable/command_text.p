/* expect: 2 */
DEFINE INPUT PARAMETER pcId AS CHARACTER NO-UNDO.
DEFINE VARIABLE oCmd AS System.Data.SqlClient.SqlCommand NO-UNDO.

oCmd:CommandText = "SELECT * FROM orders WHERE id = " + pcId.
oCmd:CommandText = pcId.
