/* expect: 2 */
/* The company the caller asks for selects the records. */
DEFINE VARIABLE iCompany AS INTEGER NO-UNDO.
iCompany = INTEGER(get-field("company")).
FOR EACH Order NO-LOCK WHERE Order.Company = iCompany:
    DISPLAY Order.OrderNum.
END.
FIND FIRST Customer NO-LOCK WHERE Customer.Company = iCompany NO-ERROR.
