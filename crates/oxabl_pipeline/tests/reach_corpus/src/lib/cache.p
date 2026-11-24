/* Keeps the last company and the theme for the agent's session. */
DEFINE NEW GLOBAL SHARED VARIABLE gcLastCompany AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gcTheme AS CHARACTER NO-UNDO.
FOR EACH OrderLog NO-LOCK WHERE OrderLog.Company = gcLastCompany: /* reach: real ABLSEC012 */
    DISPLAY OrderLog.OrderNum.
END.
gcTheme = "plain".
DISPLAY gcTheme. /* reach: safe ABLSEC012 */
