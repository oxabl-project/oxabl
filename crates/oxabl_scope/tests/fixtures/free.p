/* S3, S4: free references scope a buffer to the nearest REPEAT, FOR or
   routine block. A plain DO, DO TRANSACTION and DO ON ERROR have no record
   scoping. */
REPEAT:
  FIND NEXT Customer NO-LOCK.
  DO:
    DISPLAY Customer.Name.
  END.
END.

DO TRANSACTION:
  CREATE Item.
  Item.ItemNum = 1.
END.

DO ON ERROR UNDO, LEAVE:
  FIND FIRST Order NO-LOCK.
END.

FOR EACH OrderLine NO-LOCK:
  FIND FIRST sports.Customer WHERE sports.Customer.CustNum = 1 NO-LOCK NO-ERROR.
  IF AVAILABLE Customer THEN DISPLAY Customer.Name.
END.
