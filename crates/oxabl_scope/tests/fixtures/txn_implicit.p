/* T2, T3: blocks that directly update the database. A plain DO hands its
   updates to the enclosing block. */
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FOR EACH Customer:
  Customer.Balance = 0.
END.

REPEAT:
  DO:
    CREATE Order.
  END.
  LEAVE.
END.

DO ON ERROR UNDO, LEAVE:
  FIND FIRST Item EXCLUSIVE-LOCK.
END.

DO i = 1 TO 2:
  FIND FIRST OrderLine NO-LOCK.
  UPDATE OrderLine.Qty.
END.

PROCEDURE upd:
  FIND FIRST Customer.
  DELETE Customer.
END PROCEDURE.

PROCEDURE copy:
  DEFINE BUFFER b1 FOR Customer.
  DEFINE BUFFER b2 FOR Customer.
  FIND FIRST b1 NO-LOCK.
  BUFFER-COPY b1 TO b2.
END PROCEDURE.
