/* S1: DO FOR and REPEAT FOR scope their buffers strongly; references inside,
   a FOR EACH of the same buffer included, belong to the strong scope. */
DO FOR Customer TRANSACTION:
  FIND FIRST Customer EXCLUSIVE-LOCK.
  Customer.Balance = 0.
  FOR EACH Customer NO-LOCK:
    DISPLAY Customer.Name.
  END.
END.

REPEAT FOR Order:
  FIND NEXT Order NO-LOCK.
  DISPLAY OrderNum.
END.
