DEFINE VARIABLE mA AS MEMPTR NO-UNDO.
DEFINE VARIABLE mB AS MEMPTR NO-UNDO.
DEFINE VARIABLE mC AS MEMPTR NO-UNDO.
DEFINE VARIABLE mOther AS MEMPTR NO-UNDO.

/* OUTPUT may fill the block or replace the pointer: unknown. */
SET-SIZE(mA) = 100.
RUN fill.p (INPUT mA, OUTPUT mA).

SET-SIZE(mB) = 100.
RUN fill.p (INPUT-OUTPUT mB).

/* Copying another pointer over it loses the block. */
SET-SIZE(mC) = 100.
mC = mOther.
