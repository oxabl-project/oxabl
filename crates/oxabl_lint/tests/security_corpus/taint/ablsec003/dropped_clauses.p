/* os-command (ABLSEC003): clauses that the parser does not put in the node of
   their statement. Each function writes the variable of its own case. */
DEFINE INPUT PARAMETER pcCmd AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKept AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlag AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhen AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJoin AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE cServed AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttJob NO-UNDO FIELD cArg AS CHARACTER.
DEFINE TEMP-TABLE ttStep NO-UNDO FIELD cArg AS CHARACTER.

FUNCTION readWhen RETURNS CHARACTER ():
    cWhen = get-value("cmd").
    RETURN "a".
END FUNCTION.

FUNCTION readJoin RETURNS CHARACTER ():
    cJoin = get-value("cmd").
    RETURN "a".
END FUNCTION.

FUNCTION readWhile RETURNS CHARACTER ():
    cWhile = get-value("cmd").
    RETURN "a".
END FUNCTION.

FUNCTION readSort RETURNS CHARACTER ():
    cSort = get-value("cmd").
    RETURN "a".
END FUNCTION.

/* "Default when empty": the parameter stays when it is not empty. */
cDefault = pcCmd.
ASSIGN cDefault = "ls" WHEN cDefault = "".
OS-COMMAND SILENT VALUE(cDefault). /* taint: real */

/* A pair that a request value decides: the old value may stay. */
cMode = get-value("cmd").
ASSIGN cMode = "ls" WHEN get-value("mode") = "safe".
OS-COMMAND SILENT VALUE(cMode). /* taint: real */

/* The condition decides whether the pair assigns; its value is not assigned. */
cKept = "df".
ASSIGN cKept = "ls" WHEN get-value("mode") = "safe".
OS-COMMAND SILENT VALUE(cKept). /* taint: safe */

/* A call in the condition of a pair runs. */
cWhen = "ls".
ASSIGN cFlag = "x" WHEN readWhen() <> "".
OS-COMMAND SILENT VALUE(cWhen). /* taint: real */

/* A call in the WHERE of a joined record phrase runs. */
cJoin = "ls".
FOR EACH ttJob NO-LOCK, EACH ttStep NO-LOCK WHERE ttStep.cArg = readJoin():
END.
OS-COMMAND SILENT VALUE(cJoin). /* taint: real */

/* A call in the WHILE of a FOR EACH runs. */
cWhile = "ls".
FOR EACH ttJob NO-LOCK WHILE readWhile() = "":
END.
OS-COMMAND SILENT VALUE(cWhile). /* taint: real */

/* A call in the BY of a FOR EACH runs. */
cSort = "ls".
FOR EACH ttJob NO-LOCK BY readSort():
END.
OS-COMMAND SILENT VALUE(cSort). /* taint: real */

/* The server a RUN runs on is part of the call, as a receiver is: what the
   call gives back holds what chose the server. */
hServer = WIDGET-HANDLE(get-value("server")).
RUN job.p ON SERVER hServer (OUTPUT cServed).
OS-COMMAND SILENT VALUE(cServed). /* taint: real */
