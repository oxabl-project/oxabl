/* B7, T3: temp-table buffers scope like database buffers; their writes start
   no transaction. */
DEFINE TEMP-TABLE ttUndo
  FIELD Id AS INTEGER.
DEFINE TEMP-TABLE ttNoUndo NO-UNDO LIKE Customer.
DEFINE BUFFER bTT FOR TEMP-TABLE ttUndo.

REPEAT:
  CREATE ttUndo.
  ttUndo.Id = 1.
  LEAVE.
END.

FOR EACH ttNoUndo:
  Balance = 0.
END.

FIND FIRST bTT.
