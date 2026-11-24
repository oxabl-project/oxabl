/* os-command (ABLSEC003): what stage 2 gets wrong. */
DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.

/* Reported: a project check that the options do not name as a validator. */
IF NOT is-safe-file-name(pcFile) THEN RETURN.
OS-COMMAND SILENT VALUE("gzip " + pcFile). /* taint: safe */

/* Reported: a second handle of an object. A write through the first after
   the copy would not reach the slot of the second, so a member read through
   a handle that is not the only one of its object is Unknown. */
DEFINE VARIABLE hOwn AS HANDLE NO-UNDO.
DEFINE VARIABLE hOther AS HANDLE NO-UNDO.
CREATE WINDOW hOwn.
hOwn:PRIVATE-DATA = "ls".
hOther = hOwn:HANDLE.
OS-COMMAND VALUE(hOther:PRIVATE-DATA). /* taint: safe */
