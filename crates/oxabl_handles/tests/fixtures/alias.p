DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hCopy AS HANDLE NO-UNDO.
DEFINE VARIABLE hLost AS HANDLE NO-UNDO.

/* Freed through the alias. */
CREATE QUERY hQuery.
hCopy = hQuery.
DELETE OBJECT hCopy.

/* The alias keeps it while the first holder is overwritten. */
CREATE QUERY hLost.
ASSIGN hCopy = hLost
       hLost = ?.
DELETE OBJECT hCopy.
