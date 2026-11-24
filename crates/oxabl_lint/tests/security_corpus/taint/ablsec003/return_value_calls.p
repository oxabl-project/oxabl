/* RETURN-VALUE that the program did not set: what the caller's last RUN
   left when a unit starts, and what the code a call runs may leave. */
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */

DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.

PROCEDURE no-return PRIVATE:
END PROCEDURE.

PROCEDURE fixed PRIVATE:
    RETURN "ls".
END PROCEDURE.

FUNCTION run-other RETURNS LOGICAL PRIVATE ():
    RUN other.p.
    RETURN TRUE.
END FUNCTION.

RUN other.p (get-value("cmd")).
RUN no-return.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */

RUN fixed.
l = DYNAMIC-FUNCTION("check" IN hLib).
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */

RUN fixed.
l = run-other().
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */

RUN fixed.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: safe */
