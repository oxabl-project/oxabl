/* expect: 1 */
/* QUOTER around a value does not make the string a literal in stage 1. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = " + QUOTER(pcName)).
