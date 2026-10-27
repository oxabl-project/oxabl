/* expect: 1 */
/* One company read from the request decides every lookup of the routine. */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
cCompany = get-value("company").
FIND FIRST Customer NO-LOCK WHERE Customer.Company = cCompany NO-ERROR.
FOR EACH Order NO-LOCK WHERE Order.Company = cCompany:
    DISPLAY Order.OrderNum.
END.
FIND FIRST Item NO-LOCK WHERE Item.Company = cCompany NO-ERROR.
IF cCompany = "" THEN
    RETURN.
