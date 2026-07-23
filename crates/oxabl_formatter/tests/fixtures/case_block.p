/* Synthetic ABL — CASE with a trailing comment and a nested block. */
DEFINE VARIABLE iCode AS INTEGER NO-UNDO.

CASE iCode:
    WHEN 1 THEN
        MESSAGE "one".
    WHEN 2 THEN
        DO:
            MESSAGE "two". /* nested */
        END.
    OTHERWISE
        MESSAGE "other".
END CASE.
