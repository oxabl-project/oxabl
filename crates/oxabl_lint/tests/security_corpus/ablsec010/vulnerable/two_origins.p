/* expect: 2 */
/* A user and a role, each read from the request, each decide. */
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRole AS CHARACTER NO-UNDO.
cUser = get-value("user").
cRole = get-field("role").
FOR EACH Order NO-LOCK WHERE Order.SalesRep = cUser:
    DISPLAY Order.OrderNum.
END.
IF cRole = "admin" THEN
    RUN purge-orders.
IF cUser = "system" THEN
    RUN elevate.
