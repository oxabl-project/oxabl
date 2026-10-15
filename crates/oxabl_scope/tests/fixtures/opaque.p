/* T7, B8: what the model cannot see makes the answer Unknown. */
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
CREATE BUFFER hBuf FOR TABLE "Customer".

REPEAT:
  hBuf:BUFFER-CREATE().
  LEAVE.
END.

FOR EACH Order NO-LOCK:
  hBuf:BUFFER-FIELD("Name"):BUFFER-VALUE = Order.OrderNum.
END.

REPEAT:
  FIND NEXT Item NO-LOCK.
  RAW-TRANSFER Item TO Item.
END.

REPEAT:
  FIND NEXT Customer NO-LOCK.
  COPY-LOB FROM FILE "x.txt" TO Customer.Comments.
END.
