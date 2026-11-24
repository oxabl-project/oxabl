oResp:SetHeader("X-Frame-Options", "DENY").
oResp:SetCookie("theme", "dark").
oResp:SetHeader(NEW OpenEdge.Net.HTTP.HttpHeader("Cache-Control", "no-store")).
