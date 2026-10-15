/* S11: unqualified fields resolve to the statement's phrase, then the
   enclosing FOR blocks, then the buffers the file names. A variable wins. A
   name two buffers have is ambiguous. */
DEFINE BUFFER bCust FOR Customer.
DEFINE VARIABLE Comments AS CHARACTER NO-UNDO.

FIND FIRST Item WHERE ItemName = "x" NO-LOCK.
FOR EACH Order NO-LOCK:
  DISPLAY OrderDate.
  FOR EACH OrderLine OF Order NO-LOCK:
    DISPLAY Qty OrderStatus.
  END.
END.

REPEAT:
  FIND NEXT bCust NO-LOCK.
  Comments = "c".
END.

FIND FIRST Customer NO-LOCK.
DISPLAY Name.

/* A variable named like a buffer does not hide the buffer before a dot. */
DEFINE VARIABLE Item AS CHARACTER NO-UNDO.
REPEAT:
  FIND NEXT Item WHERE Item.ItemName = Item NO-LOCK.
END.
