/* /app: runs the page the request names, from an allow-list. */
DEFINE VARIABLE cPage AS CHARACTER NO-UNDO.
cPage = get-value("page").
IF LOOKUP(cPage, "home,help") = 0 THEN
    RETURN.
RUN VALUE("web/pages/" + cPage + ".p").
