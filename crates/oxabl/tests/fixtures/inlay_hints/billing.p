/* A synthetic program: each database fact that ABL does not show. */
DEFINE VARIABLE total AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE ttnote NO-UNDO
    FIELD id AS INTEGER
    INDEX pk IS PRIMARY UNIQUE id.

FOR EACH customer WHERE customer.region = "north" NO-LOCK:
    total = total + 1.
END.
FIND FIRST customer WHERE customer.custnum = 1.
DISPLAY customer.name.

FIND FIRST ttnote WHERE ttnote.id = 1 NO-ERROR.

PROCEDURE post-invoices:
    FOR EACH invoice WHERE invoice.custnum = 5:
        invoice.amount = 0.
    END.
END PROCEDURE.

PROCEDURE count-invoices:
    FOR EACH invoice NO-LOCK:
        total = total + 1.
    END.
    IF CAN-FIND(FIRST invoice WHERE invoice.invnum = 7) THEN
        total = total + 1.
END PROCEDURE.
