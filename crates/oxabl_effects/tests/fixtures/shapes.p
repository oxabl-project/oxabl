/* Access shapes (section 8.3). */
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE QUERY qOrder FOR Order.

FIND FIRST Customer NO-LOCK NO-ERROR.
FOR EACH Customer NO-LOCK:
    FIND FIRST Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
    FOR EACH Order WHERE Order.CustNum = Customer.CustNum NO-LOCK:
        FIND Item WHERE Item.ItemNum = 1 NO-LOCK NO-ERROR.
    END.
    IF CAN-FIND(FIRST Item WHERE Item.ItemNum = 2) THEN.
END.
/* FOR FIRST is no loop. */
FOR FIRST Customer NO-LOCK:
    FIND FIRST Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
END.
/* A DO TO loop is no record loop. */
DO i = 1 TO 10:
    FIND Item WHERE Item.ItemNum = i NO-LOCK NO-ERROR.
END.
/* A query loop. */
OPEN QUERY qOrder FOR EACH Order NO-LOCK.
REPEAT:
    GET NEXT qOrder.
    IF NOT AVAILABLE Order THEN LEAVE.
    FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
END.
/* FIND NEXT drives a loop. */
FIND FIRST Item NO-LOCK NO-ERROR.
DO WHILE AVAILABLE Item:
    FIND FIRST OrderLine WHERE OrderLine.ItemNum = Item.ItemNum NO-LOCK NO-ERROR.
    FIND NEXT Item NO-LOCK NO-ERROR.
END.
/* A write per row. */
FOR EACH Order EXCLUSIVE-LOCK:
    Order.OrderStatus = "closed".
END.
