DEFINE VARIABLE i AS INTEGER NO-UNDO.
outer:
FOR EACH Customer NO-LOCK:
    inner:
    DO i = 1 TO 3:
        IF i = 1 THEN NEXT outer.
        IF i = 2 THEN LEAVE outer.
        IF i = 3 THEN LEAVE inner.
        LEAVE nowhere.
    END.
END.
blk:
DO:
    LEAVE blk.
END.
