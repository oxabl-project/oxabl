/* Synthetic ABL — variable declarations and assignments. */
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO INITIAL "unset".
DEFINE VARIABLE dTotal AS DECIMAL NO-UNDO.

ASSIGN
    iCount = 0
    cName  = "start"
    dTotal = 0.0.

iCount = iCount + 1.
