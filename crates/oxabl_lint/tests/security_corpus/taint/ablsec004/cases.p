/* dynamic-code (ABLSEC004): stage 2 cases. */
DEFINE INPUT PARAMETER pcProg AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttStep NO-UNDO FIELD cProg AS CHARACTER.
DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

/* Tainted, directly. */
RUN VALUE(pcProg). /* taint: real */

/* Tainted through an assignment chain. */
cProg = "reports/" + pcProg.
cProg = cProg + ".p".
RUN VALUE(cProg). /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttStep.
ttStep.cProg = pcProg.
FOR EACH ttStep:
    RUN VALUE(ttStep.cProg). /* taint: real */
END.

/* Tainted through an unknown call. */
cProg = program-for(pcProg).
DYNAMIC-FUNCTION(cProg). /* taint: real */

/* Sanitised for the wrong sink. */
RUN VALUE(QUOTER(pcProg)). /* taint: real */

/* A constant held in a variable. */
cProg = "reports/daily.p".
RUN VALUE(cProg). /* taint: safe */

/* An unknown call with constant arguments. */
cProg = program-for("daily").
RUN VALUE(cProg). /* taint: safe */

/* An equality guard with literals. */
IF pcProg = "reports/daily.p" OR pcProg = "reports/weekly.p" THEN
    RUN VALUE(pcProg). /* taint: safe */

/* A CASE with literal branches. */
CASE pcProg:
    WHEN "daily" THEN RUN VALUE("reports/" + pcProg + ".p"). /* taint: safe */
    OTHERWISE RUN VALUE("reports/" + pcProg + ".p"). /* taint: real */
END CASE.
