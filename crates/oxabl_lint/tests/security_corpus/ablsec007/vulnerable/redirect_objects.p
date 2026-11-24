/* expect: 3 */
DEFINE INPUT PARAMETER pcUrl AS CHARACTER NO-UNDO.

oResp:Location = pcUrl.
oResp:SetHeader(NEW OpenEdge.Net.HTTP.HttpHeader("Location", pcUrl)).
oH = HttpHeaderBuilder:Build("Location"):Value(pcUrl):Header.
