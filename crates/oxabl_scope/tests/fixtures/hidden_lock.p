/* L5: text the file does not show after the record phrase may be the lock
   phrase, so the lock is Unknown; a visible lock phrase still counts. */
FIND FIRST Customer WHERE Customer.CustNum = 1 {&lock}.
FIND FIRST Order {&lock} NO-LOCK.
FOR EACH Item {&lock}:
END.
IF CAN-FIND(FIRST OrderLine {&lock}) THEN MESSAGE "x".
FIND FIRST Customer WHERE Customer.Name = "{&name}".
