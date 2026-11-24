/* order-report.p - prints open orders for one customer.
   Original sample program for the grammar snapshot tests. */

USING Progress.Lang.*.

{ common/defs.i &scope=report }

DEFINE INPUT PARAMETER ipCustNum AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opTotal AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrder.

DEFINE TEMP-TABLE ttOrder NO-UNDO
    FIELD orderNum AS INTEGER
    FIELD orderDate AS DATE
    FIELD amount AS DECIMAL
    INDEX idxOrder IS PRIMARY UNIQUE orderNum.

DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
VAR LOGICAL lFound = NO.

&SCOPED-DEFINE MAX-ROWS 100
&GLOBAL-DEFINE TITLE "Open ~
    orders"

FUNCTION fmtAmount RETURNS CHARACTER (INPUT dAmount AS DECIMAL):
    RETURN STRING(dAmount, "->>>,>>9.99").
END FUNCTION.

PROCEDURE addOrder:
    DEFINE INPUT PARAMETER ipNum AS INTEGER NO-UNDO.

    CREATE ttOrder.
    ASSIGN ttOrder.orderNum = ipNum
           ttOrder.orderDate = TODAY
           ttOrder.amount = 12.50.
END PROCEDURE.

main-blk:
DO ON ERROR UNDO, LEAVE:
    FOR EACH order NO-LOCK WHERE order.custNum = ipCustNum:
        IF order.shipped = ? OR order.total <= 0 THEN NEXT.
        iCount = iCount + 1.
        opTotal = opTotal + order.total.
        lFound = YES.
        IF iCount >= {&MAX-ROWS} THEN LEAVE main-blk.
    END.

    REPEAT:
        LEAVE.
    END.

    inner-blk:
    DO:
        NEXT inner-blk.
    END.

    IF NOT lFound THEN
        MESSAGE "No orders for" ipCustNum VIEW-AS ALERT-BOX WARNING.
    ELSE DO:
        cMessage = SUBSTRING(fmtAmount(opTotal), 1, 20) + "~n".
        RUN addOrder (INPUT iCount).
        RUN src/audit.p (INPUT cMessage, OUTPUT opTotal).
        RUN VALUE("lib/" + cMessage + ".p").
    END.

    CATCH eError AS Progress.Lang.Error:
        MESSAGE eError:GetMessage(1) SKIP "12/31/2026" 1.5 .5.
    END CATCH.
END.
