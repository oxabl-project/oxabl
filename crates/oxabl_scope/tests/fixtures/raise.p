/* S5: a free reference at procedure level raises the weak scopes of the same
   buffer to the procedure. Scopes in sibling REPEAT blocks stay apart. */
FOR EACH Customer NO-LOCK:
  DISPLAY Customer.Name.
END.

FIND FIRST Customer NO-LOCK.

REPEAT:
  FIND NEXT Order NO-LOCK.
END.

REPEAT:
  FIND NEXT Order NO-LOCK.
  FOR EACH Item NO-LOCK:
  END.
  FIND FIRST Item NO-LOCK.
END.
