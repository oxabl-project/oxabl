DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE hOther AS HANDLE NO-UNDO.
DEFINE VARIABLE cProc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

/* A dynamic call may take it. */
CREATE QUERY hQuery.
RUN VALUE(cProc) (INPUT hQuery).

/* IN names where the function runs, not an argument: still a leak. */
RUN lib.p PERSISTENT SET hProc.
cResult = DYNAMIC-FUNCTION("getName" IN hProc).

/* A dynamic function argument. */
CREATE QUERY hOther.
cResult = DYNAMIC-FUNCTION("useIt", hOther).
