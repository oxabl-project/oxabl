/* Synthetic ABL — annotations before classes, members and procedures. */
@Suite.
CLASS Sample:
    DEFINE PRIVATE VARIABLE iCount AS INTEGER NO-UNDO.

      @Setup.
    METHOD PUBLIC VOID Init():
        iCount = 0.
    END METHOD.

    @Test (expected="Progress.Lang.AppError").
@strict.effects(reads="Customer", writes="Order").
    METHOD PUBLIC VOID Works():
        iCount = iCount + 1.
    END METHOD.
END CLASS.

@openapi.openedge.export FILE(type="REST", useReturnValue="false").
PROCEDURE doWork:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    MESSAGE pcName.
END PROCEDURE.
