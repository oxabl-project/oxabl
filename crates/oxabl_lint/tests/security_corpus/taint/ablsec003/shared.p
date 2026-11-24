/* os-command (ABLSEC003): shared variables, and what a function writes. */
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE gcJob AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gcSpool AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gcArchive AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE ttJob NO-UNDO FIELD cName AS CHARACTER.

FUNCTION use-file RETURNS LOGICAL ():
    cCmd = "gzip " + pcFile.
    RETURN TRUE.
END FUNCTION.

/* Set by the program that runs this one. */
OS-COMMAND SILENT VALUE(gcJob). /* taint: real */
OS-COMMAND SILENT VALUE("lpr " + gcSpool). /* taint: real */

/* Set here, then by the program this one runs. */
gcArchive = "tar -czf /backup/a.tgz /data".
OS-COMMAND SILENT VALUE(gcArchive). /* taint: safe */
RUN archive-setup.p.
OS-COMMAND SILENT VALUE(gcArchive). /* taint: real */

/* Written by a function that the main block calls. */
cCmd = "df -k".
use-file().
OS-COMMAND SILENT VALUE(cCmd). /* taint: real */

/* Set here, then by a procedure of a persistent library and by a subscriber. */
gcArchive = "tar -czf /backup/a.tgz /data".
RUN archive-setup IN hLib.
OS-COMMAND SILENT VALUE(gcArchive). /* taint: real */
gcArchive = "tar -czf /backup/a.tgz /data".
PUBLISH "archive-setup".
OS-COMMAND SILENT VALUE(gcArchive). /* taint: real */

/* Written by a function that the statement of the sink calls first. */
cCmd = "df -k".
OS-COMMAND SILENT VALUE(STRING(use-file()) + " " + cCmd). /* taint: real */

/* Written by a function that a comparison, a NOT, a loop or a WHERE clause calls. */
cCmd = "df -k".
IF use-file() = TRUE THEN.
OS-COMMAND SILENT VALUE(cCmd). /* taint: real */
cCmd = "df -k".
IF NOT use-file() THEN.
OS-COMMAND SILENT VALUE(cCmd). /* taint: real */
cCmd = "df -k".
DO WHILE STRING(use-file()) <> "":
    OS-COMMAND SILENT VALUE(cCmd). /* taint: real */
    LEAVE.
END.
cCmd = "df -k".
FOR EACH ttJob WHERE ttJob.cName = STRING(use-file()):
    OS-COMMAND SILENT VALUE(cCmd). /* taint: real */
END.

/* Set here, then by a function of a library that a comparison calls. */
gcArchive = "tar -czf /backup/a.tgz /data".
IF DYNAMIC-FUNCTION("archive-mode" IN hLib) = "full" THEN.
OS-COMMAND SILENT VALUE(gcArchive). /* taint: real */

PROCEDURE archive-setup:
END PROCEDURE.
