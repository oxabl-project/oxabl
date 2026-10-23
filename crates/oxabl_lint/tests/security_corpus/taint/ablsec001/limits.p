/* dynamic-query (ABLSEC001): what stage 2 gets wrong. */
DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE cAllowed AS CHARACTER NO-UNDO INITIAL "Name,City".

FUNCTION get-filter RETURNS CHARACTER ():
    RETURN get-value("filter").
END FUNCTION.

/* Missed: the value comes back from a function of the file, and the call has no tainted argument. */
hQ:QUERY-PREPARE("FOR EACH Customer WHERE " + get-filter()). /* taint: real */

/* Reported: an allow-list held in a variable is not a literal list. */
IF LOOKUP(pcName, cAllowed) = 0 THEN RETURN.
hQ:QUERY-PREPARE("FOR EACH Customer BY Customer." + pcName). /* taint: safe */
