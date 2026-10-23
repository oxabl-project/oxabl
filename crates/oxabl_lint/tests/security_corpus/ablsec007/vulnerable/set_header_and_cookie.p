/* expect: 3 */
DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.

oResp:SetHeader("X-Name", pcValue).
oResp:SetCookie("sid", pcValue).
oResp:AddHeader("X-Trace", "id-" + pcValue).
