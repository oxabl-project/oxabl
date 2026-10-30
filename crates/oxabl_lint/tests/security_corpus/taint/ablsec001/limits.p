/* dynamic-query (ABLSEC001): what stage 2 gets wrong. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE cAllowed AS CHARACTER NO-UNDO INITIAL "Name,City".

/* Reported: an allow-list held in a variable is not a literal list. */
IF LOOKUP(pcName, cAllowed) = 0 THEN RETURN.
hQ:QUERY-PREPARE("FOR EACH Customer BY Customer." + pcName). /* taint: safe */
