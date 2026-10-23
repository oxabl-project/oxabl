/* expect: 2 */
DEFINE INPUT PARAMETER pcUrl AS CHARACTER NO-UNDO.

RUN output-http-header ("Location", pcUrl).
RUN output-http-header IN web-utilities-hdl ("Set-Cookie", "sid=" + pcUrl).
