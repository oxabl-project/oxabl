/* /profile.w: remembers the caller's company, then shows the cache. */
DEFINE SHARED VARIABLE gcLastCompany AS CHARACTER NO-UNDO.
gcLastCompany = get-value("company").
IF get-value("user") = "admin" THEN /* reach: real ABLSEC010 */
    RUN lib/cache.p.
