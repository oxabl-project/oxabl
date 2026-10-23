/* The client's user name is echoed back, not used to decide. */
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
cUser = get-value("user").
MESSAGE "Hello" cUser.
