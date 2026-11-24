/* os-command (ABLSEC003): stage 2 cases. */
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttJob NO-UNDO FIELD cArg AS CHARACTER.
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDays AS INTEGER NO-UNDO.

/* Tainted, directly. */
OS-COMMAND SILENT VALUE("gzip " + pcFile). /* taint: real */

/* Tainted through an assignment chain. */
cCmd = "tar -czf /backup/a.tgz " + pcFile.
OS-COMMAND SILENT VALUE(cCmd). /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttJob.
ttJob.cArg = pcFile.
FOR EACH ttJob:
    UNIX SILENT VALUE("lpr " + ttJob.cArg). /* taint: real */
END.

/* Tainted through an unknown call. */
cCmd = shell-line(pcFile).
OS-COMMAND SILENT VALUE(cCmd). /* taint: real */

/* Sanitised for the wrong sink: QUOTER protects a query, not a shell. */
OS-COMMAND SILENT VALUE("rm " + QUOTER(pcFile)). /* taint: real */

/* Sanitised as a number. */
iDays = INTEGER(pcFile).
OS-COMMAND SILENT VALUE("purge-older-than " + STRING(iDays)). /* taint: safe */

/* A constant held in a variable. */
cCmd = "df -k".
OS-COMMAND SILENT VALUE(cCmd). /* taint: safe */

/* An unknown call with constant arguments. */
cCmd = shell-line("status").
OS-COMMAND SILENT VALUE(cCmd). /* taint: safe */

/* A CASE with literal branches. */
CASE pcFile:
    WHEN "daily" OR WHEN "weekly" THEN OS-COMMAND SILENT VALUE("backup-" + pcFile + ".sh"). /* taint: safe */
END CASE.

/* Tainted in a private procedure that the file runs with untrusted input. */
RUN compress (INPUT pcFile).
RUN compress-fixed (INPUT "app.log").

PROCEDURE compress PRIVATE:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    OS-COMMAND SILENT VALUE("gzip " + pcName). /* taint: real */
END PROCEDURE.

PROCEDURE compress-fixed PRIVATE:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    OS-COMMAND SILENT VALUE("gzip " + pcName). /* taint: safe */
END PROCEDURE.
