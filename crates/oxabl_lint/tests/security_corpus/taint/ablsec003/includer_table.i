/* os-command (ABLSEC003): an include file checked on its own. Its includer
   defines ttReq, maybe as an input table, so the include cannot judge its
   fields. */
DEFINE TEMP-TABLE ttLog NO-UNDO FIELD cLine AS CHARACTER.

FIND FIRST ttReq.
OS-COMMAND SILENT VALUE("lpr " + ttReq.cmd). /* taint: real */

CREATE ttLog.
ttLog.cLine = "uptime".
OS-COMMAND SILENT VALUE(ttLog.cLine). /* taint: safe */
