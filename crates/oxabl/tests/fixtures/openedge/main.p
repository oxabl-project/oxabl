{defs.i}
{ttrows.i}

DO counter = 1 TO {&LIMIT}:
    CREATE ttRow.
    ASSIGN ttRow.id = counter
           ttRow.caption = {&GREETING} + " " + STRING(counter).
END.

greeting = {&GREETING}.
FOR EACH ttRow NO-LOCK:
    PUT UNFORMATTED ttRow.caption SKIP.
END.
RUN helper.p (INPUT greeting).
