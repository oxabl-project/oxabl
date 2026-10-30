/* An UNDO gives a variable that is not NO-UNDO back the value it had at the
   start of the block, so a clean write inside the block does not hide what
   the variable held before it. */
DEFINE VARIABLE cLeave AS CHARACTER.
DEFINE VARIABLE cStatement AS CHARACTER.
DEFINE VARIABLE cKept AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFixed AS CHARACTER.
DEFINE VARIABLE cMsg AS CHARACTER.
DEFINE VARIABLE n AS INTEGER NO-UNDO.

cLeave = get-value("cmd").
DO TRANSACTION ON ERROR UNDO, LEAVE:
    cLeave = "ls".
    n = INTEGER("x").
END.
OS-COMMAND VALUE(cLeave). /* taint: real */

cStatement = get-value("cmd").
DO TRANSACTION:
    cStatement = "ls".
    n = INTEGER("x") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, LEAVE.
END.
OS-COMMAND VALUE(cStatement). /* taint: real */

/* NO-UNDO: the UNDO leaves the clean value. */
cKept = get-value("cmd").
blk:
DO ON ERROR UNDO, LEAVE:
    cKept = "ls".
    IF n = 0 THEN UNDO blk, LEAVE blk.
END.
OS-COMMAND VALUE(cKept). /* taint: safe */

/* The value at the start of the block was clean too. */
cFixed = "pwd".
DO TRANSACTION ON ERROR UNDO, LEAVE:
    cFixed = "ls".
    n = INTEGER("x").
END.
OS-COMMAND VALUE(cFixed). /* taint: safe */

/* The RETURN action of the ON phrase reads the variable after the UNDO. */
PROCEDURE undo-return PRIVATE:
    DO TRANSACTION ON ERROR UNDO, RETURN cMsg:
        cMsg = "ls".
        n = INTEGER("x").
    END.
    RETURN.
END PROCEDURE.

cMsg = get-value("cmd").
RUN undo-return.
OS-COMMAND VALUE(RETURN-VALUE). /* taint: real */
