/* POST /admin/purge: the guard include authorises it. */
{auth/guard.i}
FOR EACH OrderLog EXCLUSIVE-LOCK:
    DELETE OrderLog. /* reach: safe ABLSEC011 */
END.
RUN lib/ledger.p.
