/* L3: a write counts when it may run after the read: later in the text, or
   anywhere in an iterating block of the scope that holds the read. A write
   in another routine of the scope is Unknown. */
DEFINE BUFFER bItem FOR Item.

REPEAT:
  IF AVAILABLE Customer THEN Customer.Balance = 0.
  FIND NEXT Customer.
END.

bItem.Price = 0.
DO WHILE TRUE:
  FIND NEXT bItem.
  LEAVE.
END.

PROCEDURE upd:
  Order.OrderStatus = "x".
END PROCEDURE.

PROCEDURE rd:
  FIND FIRST Order.
END PROCEDURE.

/* The reason is the other routine's own: a call that may write. */
PROCEDURE pass-it:
  RUN ext.p (BUFFER OrderLine).
END PROCEDURE.

PROCEDURE rd2:
  FIND FIRST OrderLine.
END PROCEDURE.
