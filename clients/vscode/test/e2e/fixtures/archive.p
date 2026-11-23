/* Never opened by the outline scenario: Go to Symbol in Workspace finds it. */
DEFINE VARIABLE iKept AS INTEGER NO-UNDO.

PROCEDURE ArchiveOrders:
    iKept = iKept + 1.
END PROCEDURE.
