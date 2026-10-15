/* G1-G8, N8-N10: the tests that guard a failed FIND. */
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE n AS INTEGER NO-UNDO.

FIND FIRST Customer NO-LOCK NO-ERROR.
IF AVAILABLE Customer THEN c = Customer.Name.   /* G1: then edge */
ELSE c = Customer.Name.                         /* missing: else edge */

FIND FIRST Customer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN.          /* G4: the arm leaves */
c = Customer.Name.

FIND FIRST Customer NO-LOCK NO-ERROR.
IF NOT AVAIL(Customer) THEN DO:
    MESSAGE "none".
    LEAVE.
END.
c = Customer.Name.

FIND FIRST Customer NO-LOCK NO-ERROR.
FIND FIRST Order NO-LOCK NO-ERROR.
IF AVAILABLE Customer AND AVAILABLE Order THEN  /* G2 */
    n = Customer.CustNum + Order.OrderNum.
IF NOT AVAILABLE Customer OR NOT AVAILABLE Order THEN RETURN.
n = Customer.CustNum + Order.OrderNum.

FIND FIRST Customer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN MESSAGE "none".  /* the arm rejoins */
c = Customer.Name.                              /* missing */

FIND FIRST Customer NO-LOCK NO-ERROR.
c = IF AVAILABLE Customer THEN Customer.Name ELSE "".   /* G8 */
IF AVAILABLE Customer AND Customer.Balance > 0 THEN.    /* G8, N10 */
c = Customer.Name.                              /* missing */

FIND FIRST Customer NO-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.              /* G5 */
c = Customer.Name.

FIND FIRST Customer NO-LOCK NO-ERROR.
ASSIGN n = INTEGER("1") NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.              /* G5: another NO-ERROR between */
c = Customer.Name.                              /* missing */

FIND FIRST Customer NO-LOCK NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
    FIND FIRST Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.              /* G5: a chain of NO-ERROR finds */
c = Customer.Name + STRING(Order.OrderNum).

FIND FIRST Customer NO-LOCK NO-ERROR.
IF CAN-FIND(FIRST Order WHERE Order.CustNum = 1) THEN  /* G6: no guard */
    c = Customer.Name.                          /* missing */

FIND FIRST Customer NO-LOCK NO-ERROR.
IF ROWID(Customer) = ? THEN RETURN.             /* G7: Unknown on both edges */
c = Customer.Name.

PROCEDURE shadowed:
    DEFINE VARIABLE Customer AS CHARACTER NO-UNDO.
    FIND FIRST Customer NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Customer THEN RETURN.      /* a variable hides the name */
    c = Customer.Name.
END PROCEDURE.

/* G9: a find whose criteria read the buffer proves it. */
FIND FIRST Order NO-LOCK NO-ERROR.
FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN.
c = STRING(Order.OrderNum).
FIND FIRST Order NO-LOCK NO-ERROR.
FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
c = Customer.Name.                              /* missing; a use proves it too */
c = STRING(Order.OrderNum).

/* Section 9.5: a branch of an undecided &IF may never be compiled. */
FIND FIRST Order NO-LOCK NO-ERROR.
&IF DEFINED(debug-it) &THEN
MESSAGE Order.OrderNum.
&ENDIF
