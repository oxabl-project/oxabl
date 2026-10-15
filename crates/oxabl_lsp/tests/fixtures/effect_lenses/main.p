/* café: root positions must use the document, not expanded lines. */
{routines.i}
RUN load-data.

PROCEDURE load-data:
  FIND FIRST customer NO-LOCK NO-ERROR.
  CREATE order-line.
  RUN VALUE("dynamic.p").
  RUN missing.p.
END PROCEDURE.

PROCEDURE quiet:
  MESSAGE "ok".
END PROCEDURE.

FUNCTION answer RETURNS INTEGER ():
  FIND FIRST customer NO-LOCK NO-ERROR.
  RETURN 1.
END FUNCTION.

ON "F1" ANYWHERE DO:
  FIND FIRST customer NO-LOCK NO-ERROR.
END.
