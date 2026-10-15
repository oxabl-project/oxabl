/* T4, T5: a transaction block inside another is a sub-transaction. */
DO TRANSACTION:
  FOR EACH Customer:
    Customer.Balance = 1.
  END.
  RUN other.p.
END.

PROCEDURE p:
  REPEAT:
    CREATE Order.
    REPEAT:
      CREATE OrderLine.
      LEAVE.
    END.
    LEAVE.
  END.
END PROCEDURE.
