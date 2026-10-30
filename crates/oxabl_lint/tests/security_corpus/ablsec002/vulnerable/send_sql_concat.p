/* expect: 1 */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

RUN STORED-PROC send-sql-statement hProc = PROC-HANDLE NO-ERROR
    ("SELECT * FROM customers WHERE name = '" + pcName + "'").
CLOSE STORED-PROC send-sql-statement WHERE PROC-HANDLE = hProc.
