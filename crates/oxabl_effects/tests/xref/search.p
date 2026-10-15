FIND Customer WHERE Customer.CustNum = 10 NO-LOCK NO-ERROR.
FIND FIRST Customer WHERE Customer.Name <> "A" NO-LOCK NO-ERROR.
FOR EACH Order WHERE Order.OrderStatus = "open" NO-LOCK:
END.
FOR EACH Customer WHERE Customer.Name = "A" OR Customer.CustNum = 1 NO-LOCK:
END.
FOR EACH Item NO-LOCK TABLE-SCAN:
END.
FIND FIRST Order WHERE Order.OrderDate > TODAY NO-LOCK NO-ERROR.
