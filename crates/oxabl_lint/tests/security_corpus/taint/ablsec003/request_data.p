/* os-command (ABLSEC003): request data that arrives as tables. */
DEFINE TEMP-TABLE ttJob NO-UNDO FIELD cArg AS CHARACTER.
DEFINE TEMP-TABLE ttCopy NO-UNDO FIELD cArg AS CHARACTER.
DEFINE TEMP-TABLE ttBody NO-UNDO FIELD cArg AS CHARACTER.
DEFINE TEMP-TABLE ttForm NO-UNDO FIELD cArg AS CHARACTER.
DEFINE TEMP-TABLE ttUpload NO-UNDO FIELD cArg AS CHARACTER.
DEFINE DATASET dsJob FOR ttJob.
DEFINE INPUT PARAMETER DATASET FOR dsJob.
DEFINE INPUT PARAMETER plcBody AS LONGCHAR NO-UNDO.
DEFINE VARIABLE hForm AS HANDLE NO-UNDO.

/* Tainted through an input dataset. */
FIND FIRST ttJob.
OS-COMMAND SILENT VALUE("lpr " + ttJob.cArg). /* taint: real */

/* Tainted through BUFFER-COPY. */
CREATE ttCopy.
BUFFER-COPY ttJob TO ttCopy.
OS-COMMAND SILENT VALUE("lpr " + ttCopy.cArg). /* taint: real */

/* Tainted through READ-JSON. */
TEMP-TABLE ttBody:READ-JSON("longchar", plcBody, "empty").
FIND FIRST ttBody.
OS-COMMAND SILENT VALUE("lpr " + ttBody.cArg). /* taint: real */

/* Tainted through READ-JSON on a handle of the table. */
hForm = TEMP-TABLE ttForm:HANDLE.
hForm:READ-JSON("longchar", plcBody, "empty").
FIND FIRST ttForm.
OS-COMMAND SILENT VALUE("lpr " + ttForm.cArg). /* taint: real */

/* Tainted through READ-JSON from a file. */
TEMP-TABLE ttUpload:READ-JSON("file", "upload.json", "empty").
FIND FIRST ttUpload.
OS-COMMAND SILENT VALUE("lpr " + ttUpload.cArg). /* taint: real */
