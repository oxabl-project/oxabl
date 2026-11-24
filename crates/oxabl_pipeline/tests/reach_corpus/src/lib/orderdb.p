/* Order storage, run by the order handlers. */
DEFINE INPUT PARAMETER piOrder AS INTEGER NO-UNDO.
CREATE OrderView. /* reach: real ABLSEC011 */
OrderView.OrderNum = piOrder.
