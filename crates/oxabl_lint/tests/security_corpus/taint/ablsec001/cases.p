/* dynamic-query (ABLSEC001): stage 2 cases. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttFilter NO-UNDO FIELD cWhere AS CHARACTER.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNum AS INTEGER NO-UNDO.

/* Tainted, directly. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = '" + pcName + "'"). /* taint: real */

/* Tainted through an assignment chain. */
cPart = "Customer.Name = '" + pcName + "'".
cWhere = "FOR EACH Customer WHERE " + cPart.
hQ:QUERY-PREPARE(cWhere). /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttFilter.
ttFilter.cWhere = pcName.
FOR EACH ttFilter:
    hBuf:FIND-FIRST("WHERE Customer.Name = '" + ttFilter.cWhere + "'"). /* taint: real */
END.

/* Tainted through an unknown call. */
cWhere = build-where(pcName).
hQ:QUERY-PREPARE(cWhere). /* taint: real */

/* Sanitised for this sink. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = " + QUOTER(pcName)). /* taint: safe */

/* Sanitised as a number. */
iNum = INTEGER(pcName).
hQ:QUERY-PREPARE("FOR EACH Customer WHERE Customer.CustNum = " + STRING(iNum)). /* taint: safe */

/* A constant held in a variable. */
cWhere = "FOR EACH Customer WHERE Customer.Active".
hQ:QUERY-PREPARE(cWhere). /* taint: safe */

/* An unknown call with constant arguments. */
cWhere = build-where("open").
hQ:QUERY-PREPARE(cWhere). /* taint: safe */

/* Sanitised for the wrong sink. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = '" + html-encode(pcName) + "'"). /* taint: real */

/* An allow-list guard. */
IF LOOKUP(pcName, "Name,City") > 0 THEN
    hQ:QUERY-PREPARE("FOR EACH Customer BY Customer." + pcName). /* taint: safe */

/* Tainted through a function of the file that returns web input. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE " + get-filter()). /* taint: real */

/* A function of the file that quotes its argument. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = " + quoted(pcName)). /* taint: safe */

/* The name of a buffer that a second handle of a table gives: no input,
   whatever other handles of the table may have written. */
DEFINE VARIABLE hTrack AS HANDLE NO-UNDO.
DEFINE VARIABLE hTrackBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hTrackQuery AS HANDLE NO-UNDO.
CREATE TEMP-TABLE hTrack.
hTrack:ADD-NEW-FIELD("f", "character").
hTrack:TEMP-TABLE-PREPARE("track").
hTrackBuf = hTrack:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hTrackQuery.
hTrackQuery:SET-BUFFERS(hTrackBuf).
hTrackQuery:QUERY-PREPARE("FOR EACH " + hTrackBuf:NAME + " NO-LOCK"). /* taint: safe */

FUNCTION get-filter RETURNS CHARACTER PRIVATE ():
    RETURN get-value("filter").
END FUNCTION.

FUNCTION quoted RETURNS CHARACTER PRIVATE (INPUT pc AS CHARACTER):
    RETURN QUOTER(pc).
END FUNCTION.
