/* The listing beside this file is synthetic: written by hand from the
   format in section 7.5 of strict-analysis-open-questions.md. */
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FOR EACH Customer NO-LOCK:
  DISPLAY Customer.Name.
END.

DO TRANSACTION:
  CREATE Order.
  Order.OrderNum = 1.
END.

REPEAT:
  FIND NEXT Item NO-LOCK.
  IF Item.Price > 0 THEN DO:
    i = i + 1.
  END.
END.

PROCEDURE upd:
  DEFINE BUFFER bLine FOR OrderLine.
  FIND FIRST bLine EXCLUSIVE-LOCK.
  bLine.Qty = 0.
END PROCEDURE.
