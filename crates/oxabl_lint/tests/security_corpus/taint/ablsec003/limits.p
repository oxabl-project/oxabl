/* os-command (ABLSEC003): what stage 2 gets wrong. */
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.

/* Reported: a project check that the options do not name as a validator. */
IF NOT is-safe-file-name(pcFile) THEN RETURN.
OS-COMMAND SILENT VALUE("gzip " + pcFile). /* taint: safe */

PROCEDURE compress PRIVATE:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    /* Missed: a private procedure that its own file calls with untrusted input. */
    OS-COMMAND SILENT VALUE("gzip " + pcName). /* taint: real */
END PROCEDURE.

RUN compress (INPUT pcFile).
