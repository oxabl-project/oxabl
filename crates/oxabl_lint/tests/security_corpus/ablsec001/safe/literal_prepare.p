DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name BEGINS 'A'").
hQuery:QUERY-PREPARE("FOR EACH Customer " + "WHERE Customer.Balance > 0").
