/* Posts the ledger; run from the admin purge and from the export. */
RUN post-ledger. /* reach: real ABLSEC011 */

PROCEDURE post-ledger:
    CREATE LedgerLine. /* reach: real ABLSEC011 */
    LedgerLine.Posted = TODAY.
END PROCEDURE.
