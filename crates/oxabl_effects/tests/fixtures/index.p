/* Index selection (section 8.2). */
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE Balance AS DECIMAL NO-UNDO.
DEFINE VARIABLE r AS ROWID NO-UNDO.

/* I3: every component of a unique index matched by equality. */
FIND Customer WHERE Customer.CustNum = 10 NO-LOCK NO-ERROR.
/* I4: unqualified fields; the most equality matches. */
FIND FIRST Customer WHERE SalesRep = "BBB" AND Name = cName NO-LOCK NO-ERROR.
/* I5: a range match on the first component. */
FIND FIRST Customer WHERE Customer.Name BEGINS "A" NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE Customer.Name > "M" NO-LOCK NO-ERROR.
/* I7: not bracketed, so the primary index, whole. */
FIND FIRST Customer WHERE Customer.Name <> "A" NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE Customer.Name MATCHES "*a*" NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE SUBSTRING(Customer.Name, 1, 2) = "AB" NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE Customer.Name = Customer.City NO-LOCK NO-ERROR.
/* A variable named like a field is the variable. */
FIND FIRST Customer WHERE Customer.Name = "x" AND Balance > 0 NO-LOCK NO-ERROR.
/* I8: no WHERE: the primary index, whole. */
FIND FIRST Customer NO-LOCK NO-ERROR.
/* I2: USE-INDEX, bracketed or not. */
FIND FIRST Customer WHERE Customer.Name = "A" USE-INDEX SalesRep NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE Customer.SalesRep = "A" USE-INDEX SalesRep NO-LOCK NO-ERROR.
/* I11: the key value form. */
FIND Customer 10 NO-LOCK.
/* I14: CONTAINS on a word index. */
FIND FIRST Customer WHERE Customer.Comments CONTAINS "late" NO-LOCK NO-ERROR.
/* I15: ROWID. */
FIND Customer WHERE ROWID(Customer) = r NO-LOCK NO-ERROR.
/* I9: OR in a FIND uses one index; in a FOR it may use several. */
FIND FIRST Customer WHERE Customer.Name = "A" OR Customer.CustNum = 1 NO-LOCK NO-ERROR.
FOR EACH Customer WHERE Customer.Name = "A" OR Customer.CustNum = 1 NO-LOCK:
END.
FOR EACH Customer WHERE (Customer.Name = "A" OR Customer.Comments = "x") AND Customer.Country = "NL" NO-LOCK:
END.
/* I6: a tie on equality broken by a range on the next component. */
FOR EACH Customer WHERE Customer.Country = "NL" AND Customer.City > "B" NO-LOCK:
END.
/* I6: sort matches pick the index when nothing brackets. */
FOR EACH Order NO-LOCK BY Order.OrderDate:
END.
FOR EACH Order WHERE Order.OrderStatus = "open" NO-LOCK BY Order.OrderDate:
END.
/* I10: OF. */
FOR EACH Customer NO-LOCK, EACH Order OF Customer NO-LOCK:
END.
FIND FIRST Order NO-LOCK NO-ERROR.
FIND Customer OF Order NO-LOCK NO-ERROR.
/* I3 TABLE-SCAN. */
FOR EACH Item NO-LOCK TABLE-SCAN:
END.
/* CAN-FIND. */
IF CAN-FIND(FIRST OrderLine WHERE OrderLine.ItemNum = 5) THEN.
IF CAN-FIND(Item WHERE Item.ItemNum = 5 NO-LOCK) THEN.
/* A join: the second phrase uses the first one's record. */
FOR EACH Order WHERE Order.OrderDate > TODAY - 7 NO-LOCK,
    EACH OrderLine WHERE OrderLine.OrderNum = Order.OrderNum NO-LOCK,
    FIRST Item WHERE Item.ItemNum = OrderLine.ItemNum NO-LOCK:
END.
/* USE-INDEX naming no index. */
FIND FIRST Item WHERE Item.ItemNum = 1 USE-INDEX Nope NO-LOCK NO-ERROR.
/* I16: a named buffer's criteria; the table name is the default buffer. */
DEFINE BUFFER bCust FOR Customer.
FIND FIRST bCust WHERE bCust.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
FIND FIRST bCust WHERE bCust.Name = bCust.City NO-LOCK NO-ERROR.
