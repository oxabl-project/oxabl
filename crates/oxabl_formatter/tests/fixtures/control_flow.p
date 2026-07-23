/* Synthetic ABL — nested control flow. */
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO i = 1 TO 10:
    IF i MODULO 2 = 0 THEN
        MESSAGE "even".
    ELSE
        MESSAGE "odd".
END.

REPEAT:
    i = i - 1.
    IF i <= 0 THEN
        LEAVE.
END.
