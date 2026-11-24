/* expect: 3 */
DEFINE VARIABLE cSql AS CHARACTER NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

RUN STORED-PROC send-sql-statement hProc = PROC-HANDLE (cSql).
RUN STORED-PROC send-sql-statement NO-ERROR (INPUT cSql).
RUN STORED-PROC erpdb.send-sql-statement hProc = PROC-HANDLE NO-ERROR (cSql).
