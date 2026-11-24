&GLOBAL-DEFINE OUT PUT STREAM WebStream UNFORMATTED
oResp:Write("<p>The request failed</p>").
{&OUT} "<p>Try again later</p>" SKIP.
