/* Totals for a made-up shop. */
// A second comment style.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 10.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.

ASSIGN cName = "widget".
cName = SUBSTRING(cName, 1, 3).
hBuf:QUERY-OPEN().
cName = hBuf:NAME.
IF iCount > 5 THEN DO:
  DISPLAY cName.
END.
