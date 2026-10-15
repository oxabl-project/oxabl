/* N5, G3, K4: index walks and loops. */
DEFINE VARIABLE n AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

REPEAT:
    FIND NEXT Customer NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Customer THEN LEAVE.
    n = n + Customer.CustNum.
END.

FIND FIRST Order NO-LOCK NO-ERROR.
DO WHILE AVAILABLE Order:                      /* G3 */
    n = n + Order.Total.
    FIND NEXT Order NO-LOCK NO-ERROR.
END.
n = Order.OrderNum.                            /* missing: the loop ends when it is empty */

FIND FIRST Order NO-LOCK NO-ERROR.
DO i = 1 TO 10 WHILE AVAILABLE Order:          /* G3: TO also leaves */
    n = n + Order.Total.
    FIND NEXT Order NO-LOCK NO-ERROR.
END.

FIND FIRST Item NO-LOCK NO-ERROR.
FOR EACH Item NO-LOCK:                         /* K4: the FOR refills */
    n = n + Item.ItemNum.
END.
n = Item.ItemNum.

FIND FIRST Customer NO-LOCK NO-ERROR.
FOR EACH Order WHERE Order.CustNum = Customer.CustNum NO-LOCK:   /* missing */
    n = n + Order.Total.
END.

FIND FIRST Customer NO-LOCK NO-ERROR.
FOR EACH Order WHERE Order.OrderNum = INTEGER(RECID(Customer)) NO-LOCK:  /* K4: runs only with a record */
    n = Customer.CustNum.
END.
