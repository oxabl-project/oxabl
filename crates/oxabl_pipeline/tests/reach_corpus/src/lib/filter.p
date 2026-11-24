/* Builds a dynamic query from the caller's company. */
DEFINE INPUT PARAMETER pcCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
CREATE QUERY hQ.
hQ:QUERY-PREPARE("FOR EACH OrderLog WHERE OrderLog.Company = '" + pcCompany + "'").
hQ:QUERY-OPEN().
