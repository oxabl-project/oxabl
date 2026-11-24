/* REST handlers for orders. */
{web/common.i}

PROCEDURE get-order:
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    iOrder = INTEGER(get-value("id")).
    RUN lib/orderdb.p (INPUT iOrder).
    IF get-value("company") <> "" THEN /* reach: real ABLSEC010 */
        RUN lib/filter.p (INPUT get-value("company")).
END PROCEDURE.

PROCEDURE delete-order:
    RUN auth/check-access.p (INPUT "orders.delete").
    FIND OrderLog EXCLUSIVE-LOCK WHERE OrderLog.OrderNum = INTEGER(get-value("id")) NO-ERROR.
    IF AVAILABLE OrderLog THEN
        DELETE OrderLog. /* reach: safe ABLSEC011 */
END PROCEDURE.
