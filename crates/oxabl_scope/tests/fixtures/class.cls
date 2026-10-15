/* B3: in a class, a method's free reference to a default buffer scopes it to
   the class. A buffer the method defines stays in the method. */
CLASS Sales.Ledger:
  DEFINE TEMP-TABLE ttLine NO-UNDO
    FIELD LineNum AS INTEGER
    FIELD Amount AS DECIMAL.

  METHOD PUBLIC VOID Load():
    FOR EACH Customer NO-LOCK:
      CREATE ttLine.
      ttLine.LineNum = Customer.CustNum.
    END.
  END METHOD.

  METHOD PUBLIC DECIMAL Total():
    DEFINE BUFFER bLine FOR ttLine.
    DEFINE VARIABLE d AS DECIMAL NO-UNDO.
    FOR EACH bLine:
      d = d + Amount.
    END.
    FIND FIRST Customer NO-LOCK NO-ERROR.
    RETURN d.
  END METHOD.
END CLASS.
