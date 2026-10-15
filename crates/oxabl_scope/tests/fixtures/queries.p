/* S10: OPEN QUERY and GET are free references where they stand; DEFINE
   QUERY is none. */
DEFINE QUERY qCust FOR Customer, Order.

REPEAT:
  OPEN QUERY qCust FOR EACH Customer NO-LOCK, EACH Order OF Customer NO-LOCK.
  GET FIRST qCust.
  GET NEXT qCust EXCLUSIVE-LOCK.
  LEAVE.
END.
