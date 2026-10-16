/* N1, N3, N6, N7, K1-K3: a failed FIND, a use, and the statements that refill. */
DEFINE VARIABLE n AS INTEGER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.

FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.
c = Customer.Name.                          /* missing: read */
c = Customer.Name.                          /* proven by the line above */

FIND Customer WHERE Customer.CustNum = 2 EXCLUSIVE-LOCK NO-ERROR.
ASSIGN Customer.Balance = 0.                /* missing: write */

FIND Order WHERE Order.OrderNum = 1 NO-LOCK NO-ERROR.
FIND Order WHERE Order.OrderNum = 2 NO-LOCK.
n = Order.CustNum.                          /* FIND without NO-ERROR refills */

FIND Item WHERE Item.ItemNum = 1 NO-ERROR.
CREATE Item.
Item.Price = 1.                             /* CREATE refills */

FIND Customer WHERE Customer.CustNum = 3 NO-LOCK NO-ERROR.
ASSIGN c = Customer.Name NO-ERROR.          /* N7: NO-ERROR on the use */
FIND Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.  /* N7 again */
n = Customer.CustNum.                       /* missing */

FIND FIRST Customer NO-LOCK.
FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
Customer.Balance = 1.                       /* N6: FIND CURRENT keeps the record */

FIND FIRST Customer NO-LOCK NO-ERROR.
FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
Customer.Balance = 1.                       /* missing: still after the first */

FIND FIRST Order WHERE Order.CustNum = n NO-ERROR.
FIND FIRST Order WHERE Order.CustNum = Order.OrderNum NO-ERROR.  /* own criteria */
n = Order.Total.                            /* missing, after the second only */

DO ON ERROR UNDO, LEAVE:
    FIND FIRST Customer NO-LOCK NO-ERROR.
    n = INTEGER(c).                         /* may raise */
    IF NOT AVAILABLE Customer THEN RETURN.
END.
c = Customer.Name.                          /* K8: the error path is not followed */
