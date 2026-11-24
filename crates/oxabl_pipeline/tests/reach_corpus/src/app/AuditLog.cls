/* Writes the audit trail. */
CLASS app.AuditLog:
    METHOD PUBLIC VOID Record(INPUT pcWhat AS CHARACTER):
        CREATE AuditLine. /* reach: real ABLSEC011 */
        AuditLine.What = pcWhat.
    END METHOD.
END CLASS.
