/* dynamic-code (ABLSEC004): a node that a built-in method writes into its
   argument holds what the document holds. */
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hKid AS HANDLE NO-UNDO.
DEFINE VARIABLE lcXml AS LONGCHAR NO-UNDO.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hKid.
lcXml = get-value("xml").
hDoc:LOAD("longchar", lcXml, FALSE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).
hRoot:GET-CHILD(hKid, 1).
RUN VALUE(hKid:NAME + ".p"). /* taint: real */

/* A document of the program. */
DEFINE VARIABLE hFixed AS HANDLE NO-UNDO.
DEFINE VARIABLE hTop AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hFixed.
CREATE X-NODEREF hTop.
hFixed:LOAD("longchar", "<steps><daily/></steps>", FALSE).
hFixed:GET-DOCUMENT-ELEMENT(hTop).
RUN VALUE(hTop:NAME + ".p"). /* taint: safe */
