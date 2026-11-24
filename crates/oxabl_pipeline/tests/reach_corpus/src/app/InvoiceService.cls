/* POST /invoices. */
USING app.*.
CLASS app.InvoiceService:
    METHOD PUBLIC VOID Create(INPUT pcCustomer AS CHARACTER):
        DEFINE VARIABLE oAudit AS AuditLog NO-UNDO.
        IF RequestContext:CurrentUser = "" THEN /* reach: real ABLSEC012 */
            RETURN.
        oAudit = NEW AuditLog().
        oAudit:Record(pcCustomer).
    END METHOD.
END CLASS.
