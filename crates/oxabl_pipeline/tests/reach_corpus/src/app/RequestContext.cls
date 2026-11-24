/* Who the agent served last: a static, so it outlives the request. */
CLASS app.RequestContext:
    DEFINE PUBLIC STATIC PROPERTY CurrentUser AS CHARACTER NO-UNDO GET. SET.
END CLASS.
