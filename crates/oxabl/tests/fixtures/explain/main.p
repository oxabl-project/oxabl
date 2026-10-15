{defs.i}
RUN read.p.
PROCEDURE scan:
    FOR EACH Customer NO-LOCK:
        FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK.
    END.
    RUN VALUE("unknown.p").
END PROCEDURE.
