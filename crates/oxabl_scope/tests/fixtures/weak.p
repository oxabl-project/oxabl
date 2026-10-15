/* S2, S6, S8: each FOR scopes its record-phrase buffers weakly; two FOR
   blocks in a row give two scopes; the OF in a header is a reference in the
   enclosing block. */
FOR EACH Customer NO-LOCK:
  DISPLAY Customer.Name.
END.

FOR EACH Customer NO-LOCK WHERE Customer.Balance > 0, EACH Order OF Customer NO-LOCK BY Order.OrderDate:
  DISPLAY Order.OrderNum.
END.

REPEAT:
  FIND NEXT Item NO-LOCK.
  FOR EACH OrderLine OF Item NO-LOCK:
    DISPLAY Qty.
  END.
END.
