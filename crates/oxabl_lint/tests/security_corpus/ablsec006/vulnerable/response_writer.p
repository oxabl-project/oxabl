/* expect: 3 */
CLASS Acme.Page:
    METHOD PUBLIC VOID Render(INPUT oResponse AS OpenEdge.Web.IWebResponse, INPUT pcName AS CHARACTER):
        DEFINE VARIABLE oWriter AS OpenEdge.Web.WebResponseWriter NO-UNDO.
        oWriter:Write("<p>" + pcName + "</p>").
        oResponse:Entity = NEW OpenEdge.Core.String(pcName).
        THIS-OBJECT:oResp:Write(pcName).
    END METHOD.
END CLASS.
