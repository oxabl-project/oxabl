/* untrusted-xml (ABLSEC015): stage 2 cases. */
DEFINE INPUT PARAMETER pcXml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE lcXml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

/* Tainted, directly. */
hDoc:LOAD("longchar", pcXml, FALSE). /* taint: real */

/* Tainted through a file read. */
COPY-LOB FROM FILE "/data/in/order.xml" TO lcXml.
hDoc:LOAD("longchar", lcXml, FALSE). /* taint: real */

/* A file name chosen by the program. */
cFile = "/app/config/settings.xml".
hDoc:LOAD("file", cFile, FALSE). /* taint: safe */
