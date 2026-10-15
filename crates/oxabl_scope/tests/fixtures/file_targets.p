/* T3, S3: a file name is no reference and no write: COPY-LOB ... TO FILE,
   and an OUTPUT or INPUT target written without VALUE. */
DEFINE VARIABLE lcText AS LONGCHAR NO-UNDO.

REPEAT:
  FIND NEXT Customer NO-LOCK.
  COPY-LOB FROM Customer.Comments TO FILE "x.txt".
  LEAVE.
END.

REPEAT:
  FIND NEXT Customer NO-LOCK.
  COPY-LOB FROM lcText TO OBJECT Customer.Comments.
  LEAVE.
END.

OUTPUT TO Item.d.
FOR EACH Item NO-LOCK:
  EXPORT Item.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE(Order.OrderNum).
