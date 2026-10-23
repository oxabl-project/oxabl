/* path-traversal (ABLSEC005): stage 2 cases. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttFile NO-UNDO FIELD cPath AS CHARACTER.
DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE iId AS INTEGER NO-UNDO.

/* Tainted, directly. */
OUTPUT TO VALUE("/data/out/" + pcName). /* taint: real */

/* Tainted through an assignment chain. */
cPath = "/data/in/" + pcName.
cPath = cPath + ".csv".
INPUT FROM VALUE(cPath). /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttFile.
ttFile.cPath = pcName.
FOR EACH ttFile:
    OS-DELETE VALUE(ttFile.cPath). /* taint: real */
END.

/* Tainted through an unknown call. */
cPath = resolve-path(pcName).
OUTPUT TO VALUE(cPath). /* taint: real */

/* Sanitised for the wrong sink. */
OS-DELETE VALUE("/data/tmp/" + QUOTER(pcName)). /* taint: real */

/* Sanitised as a number. */
iId = INTEGER(pcName).
OUTPUT TO VALUE("/data/out/" + STRING(iId) + ".txt"). /* taint: safe */

/* A constant held in a variable. */
cPath = "/data/out/report.txt".
OUTPUT TO VALUE(cPath). /* taint: safe */

/* An unknown call with constant arguments. */
cPath = resolve-path("report.txt").
OUTPUT TO VALUE(cPath). /* taint: safe */

/* An environment variable is a source. */
OUTPUT TO VALUE(OS-GETENV("REPORT_DIR") + "/report.txt"). /* taint: real */

/* Tainted through the OUTPUT parameter of an internal procedure. */
RUN read-target (OUTPUT cPath).
OUTPUT TO VALUE(cPath). /* taint: real */

/* An internal procedure whose OUTPUT parameter is a constant. */
RUN fixed-target (INPUT pcName, OUTPUT cPath).
OUTPUT TO VALUE(cPath). /* taint: safe */

PROCEDURE read-target PRIVATE:
    DEFINE OUTPUT PARAMETER pc AS CHARACTER NO-UNDO.
    pc = get-value("target").
END PROCEDURE.

PROCEDURE fixed-target PRIVATE:
    DEFINE INPUT PARAMETER pcIn AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcOut AS CHARACTER NO-UNDO.
    pcOut = "/data/out/report.txt".
END PROCEDURE.
