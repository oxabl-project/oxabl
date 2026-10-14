DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE hKept AS HANDLE NO-UNDO.

RUN lib.p PERSISTENT SET hProc.
RUN do-work IN hProc.
DELETE PROCEDURE hProc.

RUN singleton.p PERSISTENT.

RUN leaky.p PERSISTENT SET hKept.
RUN do-work IN hKept.

/* By convention `destroy` deletes the procedure; not visible here. */
RUN driver.p PERSISTENT SET hKept.
RUN destroy IN hKept.
