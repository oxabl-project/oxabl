/* L1-L3, S9: lock defaults, explicit locks, temp-tables, CAN-FIND's private
   buffer, and a write after the read. */
DEFINE TEMP-TABLE ttX NO-UNDO FIELD k AS INTEGER.

FIND FIRST Customer.
FIND FIRST Order NO-LOCK.
IF CAN-FIND(FIRST OrderLine WHERE OrderLine.OrderNum = Order.OrderNum) THEN
  DISPLAY "yes".
IF CAN-FIND(FIRST Item WHERE ItemNum = 1 NO-LOCK) THEN
  DISPLAY "no".
FIND FIRST ttX.

REPEAT:
  FIND NEXT Customer SHARE-LOCK.
  Customer.Balance = Customer.Balance + 1.
END.

FOR EACH Item EXCLUSIVE-LOCK:
  DISPLAY Item.Price.
END.

PROCEDURE passes:
  DEFINE BUFFER bItem FOR Item.
  FIND FIRST bItem.
  RUN price.p (BUFFER bItem).
END PROCEDURE.
