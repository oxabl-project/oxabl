/* no-schema */
/* Without a schema, table names in buffer positions still give buffers, and
   an unqualified name may be a field: Unknown (no-schema). */
FOR EACH Customer NO-LOCK:
  DISPLAY Customer.Name.
END.

REPEAT:
  FIND NEXT Customer NO-LOCK.
  DISPLAY Balance.
END.
