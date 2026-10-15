DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO.

/* A pool created after the object does not hold it. */
CREATE QUERY hQuery.
IF lFlag THEN CREATE WIDGET-POOL.
