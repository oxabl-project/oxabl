/* T3: the screen form of ASSIGN (no `=`) updates the record. */
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE FRAME f Order.OrderStatus.

PROCEDURE plain:
  FIND FIRST Customer WHERE Customer.CustNum = 1.
  PROMPT-FOR Customer.Name.
  ASSIGN Customer.Name.
END PROCEDURE.

PROCEDURE in-frame:
  FIND FIRST Order WHERE Order.OrderNum = 1.
  ENABLE Order.OrderStatus WITH FRAME f.
  ASSIGN FRAME f Order.OrderStatus.
END PROCEDURE.

PROCEDURE input:
  FIND FIRST Item WHERE Item.ItemNum = 1.
  ASSIGN INPUT Item.Price.
END PROCEDURE.

PROCEDURE record:
  FIND FIRST OrderLine.
  ASSIGN OrderLine EXCEPT OrderLine.Qty.
END PROCEDURE.

PROCEDURE mixed:
  DEFINE BUFFER bCust FOR Customer.
  FIND FIRST bCust.
  ASSIGN x = 1 WHEN AVAILABLE bCust bCust.Balance.
END PROCEDURE.

PROCEDURE pairs-only:
  DEFINE BUFFER bItem FOR Item.
  FIND FIRST bItem NO-LOCK.
  ASSIGN x = bItem.ItemNum WHEN bItem.Price > 0.
END PROCEDURE.
