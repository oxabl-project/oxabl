DEFINE VARIABLE i AS INTEGER NO-UNDO.
DO WHILE i < 10:
    i = i + 1.
END.
DO i = 1 TO 5:
    IF i = 3 THEN NEXT.
    IF i = 4 THEN LEAVE.
END.
REPEAT:
    i = i - 1.
    IF i < 0 THEN LEAVE.
END.
FOR FIRST Customer NO-LOCK:
    i = 0.
END.
FOR LAST Customer NO-LOCK:
END.
REPEAT TRANSACTION:
    i = 1.
END.
