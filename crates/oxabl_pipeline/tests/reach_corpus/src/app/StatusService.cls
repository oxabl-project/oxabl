/* GET /status: the framework authorises the caller. */
USING app.*.
CLASS app.StatusService:
    METHOD PUBLIC CHARACTER Ping():
        RequestContext:CurrentUser = "status".
        CREATE StatusPing. /* reach: safe ABLSEC011 */
        RETURN "ok".
    END METHOD.
END CLASS.
