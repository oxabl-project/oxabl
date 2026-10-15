/* S7: a free reference whose scope would enclose a strong scope of the same
   buffer is a conflict: Unknown. */
DO FOR Customer:
  FIND FIRST Customer NO-LOCK.
END.
FIND FIRST Customer NO-LOCK.

FOR EACH Order NO-LOCK:
  FOR EACH Order NO-LOCK:
  END.
END.
