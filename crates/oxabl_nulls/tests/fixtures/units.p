/* K7: each routine starts with nothing known. */
DEFINE VARIABLE c AS CHARACTER NO-UNDO.

FIND FIRST Customer NO-LOCK NO-ERROR.

PROCEDURE show:
    c = Customer.Name.                          /* the caller decides */
END PROCEDURE.

FUNCTION name-of RETURNS CHARACTER (n AS INTEGER):
    DEFINE BUFFER b FOR Customer.
    FIND b WHERE b.CustNum = n NO-LOCK NO-ERROR.
    RETURN b.Name.                              /* missing */
END FUNCTION.

PROCEDURE cleanup:
    DEFINE BUFFER b FOR Order.
    FIND FIRST b NO-LOCK NO-ERROR.
    IF c = "" THEN RETURN.
    DO ON ERROR UNDO, LEAVE:
        DISPLAY b.OrderNum.                     /* missing */
    END.
    FINALLY:
        c = STRING(b.Total).                    /* missing, once for both copies */
    END FINALLY.
END PROCEDURE.
