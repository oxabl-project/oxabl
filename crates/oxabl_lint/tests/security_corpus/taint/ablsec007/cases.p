/* header-injection (ABLSEC007): stage 2 cases. */
DEFINE TEMP-TABLE ttLink NO-UNDO FIELD cUrl AS CHARACTER.
DEFINE VARIABLE cUrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNext AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPage AS INTEGER NO-UNDO.

cNext = get-value("next").

/* Tainted, directly. */
oResp:SetHeader("Location", get-value("next")). /* taint: real */

/* Tainted through an assignment chain. */
cUrl = "/app/" + cNext.
oResp:SetHeader("Location", cUrl). /* taint: real */

/* Tainted through a temp-table field. */
CREATE ttLink.
ttLink.cUrl = cNext.
oResp:SetCookie("next", ttLink.cUrl). /* taint: real */

/* Tainted through an unknown call. */
cUrl = absolute-url(cNext).
oResp:SetHeader("Location", cUrl). /* taint: real */

/* Encoded, but no encoder protects a header: HTML-ENCODE keeps CR and LF,
   and URL-ENCODE keeps a redirect to another site. */
oResp:SetHeader("X-Next", html-encode(cNext)). /* taint: real */
oResp:SetHeader("Location", url-encode(cNext)). /* taint: real */

/* Sanitised for the wrong sink. */
oResp:SetHeader("X-Next", QUOTER(cNext)). /* taint: real */

/* Sanitised as a number. */
iPage = INTEGER(cNext).
oResp:SetHeader("X-Page", STRING(iPage)). /* taint: safe */

/* A constant held in a variable. */
cUrl = "/app/home".
oResp:SetHeader("Location", cUrl). /* taint: safe */

/* An allow-list guard. */
IF LOOKUP(cNext, "home,orders,account") = 0 THEN RETURN.
oResp:SetHeader("Location", "/app/" + cNext). /* taint: safe */
