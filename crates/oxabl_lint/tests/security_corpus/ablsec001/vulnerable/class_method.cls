/* expect: 1 */
CLASS Acme.CustomerRepo:
    METHOD PUBLIC VOID Load(INPUT pcWhere AS CHARACTER):
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        CREATE QUERY hQuery.
        hQuery:QUERY-PREPARE("FOR EACH Customer WHERE " + pcWhere).
    END METHOD.
END CLASS.
