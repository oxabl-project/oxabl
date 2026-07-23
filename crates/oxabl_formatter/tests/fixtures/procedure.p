/* Synthetic ABL — internal procedure with a bare END. */
DEFINE VARIABLE cGreeting AS CHARACTER NO-UNDO.

PROCEDURE buildGreeting:
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcResult AS CHARACTER NO-UNDO.

    pcResult = "Hello, " + pcName.
END.

RUN buildGreeting("world", OUTPUT cGreeting).
MESSAGE cGreeting.
