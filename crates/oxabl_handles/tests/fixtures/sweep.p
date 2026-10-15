DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE hWalk AS HANDLE NO-UNDO.
DEFINE VARIABLE hNext AS HANDLE NO-UNDO.

/* A sweep of the session's procedure list may delete it. */
RUN lib.p PERSISTENT SET hProc.
RUN do-work IN hProc.

hWalk = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hWalk):
    hNext = hWalk:NEXT-SIBLING.
    DELETE PROCEDURE hWalk NO-ERROR.
    hWalk = hNext.
END.
