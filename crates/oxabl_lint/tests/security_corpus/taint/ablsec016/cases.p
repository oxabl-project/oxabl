/* resource-exhaustion (ABLSEC016): stage 2 cases. */
DEFINE INPUT PARAMETER pcSize AS CHARACTER NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE iSize AS INTEGER NO-UNDO.
DEFINE VARIABLE iFixed AS INTEGER NO-UNDO.

/* Tainted: a number conversion does not bound a size. */
iSize = INTEGER(pcSize).
SET-SIZE(m) = iSize. /* taint: real */

/* Tainted through an unknown call. */
SET-SIZE(m) = size-of(pcSize). /* taint: real */

/* A constant held in a variable. */
iFixed = 4096.
SET-SIZE(m) = iFixed. /* taint: safe */

/* A length the program chose. */
SET-SIZE(m) = LENGTH("header") + 1. /* taint: safe */
