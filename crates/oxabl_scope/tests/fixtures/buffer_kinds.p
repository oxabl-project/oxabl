/* B4-B6, S12: defined, parameter, shared and new shared buffers. A
   definition alone, and LIKE, are no reference. */
DEFINE NEW SHARED BUFFER shCust FOR Customer.
DEFINE SHARED BUFFER shOrder FOR Order.
DEFINE BUFFER bUnused FOR Item.
DEFINE VARIABLE cName LIKE Customer.Name NO-UNDO.

FIND FIRST shOrder NO-LOCK.
REPEAT:
  FIND NEXT shCust NO-LOCK.
END.

PROCEDURE takes-buffer:
  DEFINE PARAMETER BUFFER pCust FOR Customer.
  pCust.Balance = 0.
END PROCEDURE.
