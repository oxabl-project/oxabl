/* B2: a default buffer belongs to the procedure file. A free reference in an
   internal procedure raises it to the main block; a local DEFINE BUFFER keeps
   it inside the procedure. */
DEFINE BUFFER bOrder FOR Order.

FOR EACH Customer NO-LOCK:
  DISPLAY Customer.Name.
END.

PROCEDURE uses-default:
  FIND FIRST Customer NO-LOCK.
  FOR EACH Item NO-LOCK:
    DISPLAY Item.ItemName.
  END.
END PROCEDURE.

PROCEDURE uses-local:
  DEFINE BUFFER Customer FOR Customer.
  FIND FIRST Customer NO-LOCK.
  FIND FIRST bOrder NO-LOCK.
END PROCEDURE.

FUNCTION f RETURNS LOGICAL ():
  RETURN CAN-FIND(FIRST Item).
END FUNCTION.
