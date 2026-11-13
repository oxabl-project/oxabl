/* oxabl-worker protocol 1 */
/* The oxabl build worker. It reads OXW1 lines on stdin and answers on stdout
   with PUT UNFORMATTED ... SKIP. It connects no database and runs no other
   program than the compile procedure it writes. It writes no file except the
   compile outputs and that procedure, in the stage directory of the request.
   Start it as: _progres -b -p oxabl_worker.p */

DEFINE VARIABLE gcTab      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCwd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcStartPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLdb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSafe     AS CHARACTER NO-UNDO.
DEFINE VARIABLE giCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE giEntries  AS INTEGER   NO-UNDO.
DEFINE VARIABLE giTables   AS INTEGER   NO-UNDO.
DEFINE VARIABLE giPart     AS INTEGER   NO-UNDO.
DEFINE VARIABLE giParts    AS INTEGER   NO-UNDO.
DEFINE VARIABLE glQuit     AS LOGICAL   NO-UNDO.

/* The canonical text of the entry being built: gcBuf collects pieces and is
   moved into glcText before it can outgrow a CHARACTER. */
DEFINE VARIABLE glcText    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE gcBuf      AS CHARACTER NO-UNDO.
DEFINE VARIABLE giBuf      AS INTEGER   NO-UNDO.
DEFINE VARIABLE giLong     AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInRecord AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRecords  AS INTEGER   NO-UNDO.

/* One buffer and one query for each metaschema table the image reads, and
   the fields of each buffer, looked at once. A CHARACTER array may hold at
   most 32,000 bytes in all, so the fields go in a temp-table. */
DEFINE VARIABLE ghBuf      AS HANDLE    NO-UNDO EXTENT 8.
DEFINE VARIABLE ghQuery    AS HANDLE    NO-UNDO EXTENT 8.
DEFINE TEMP-TABLE ttMeta NO-UNDO
    FIELD iKind  AS INTEGER
    FIELD iPos   AS INTEGER
    FIELD hFld   AS HANDLE
    FIELD cLabel AS CHARACTER
    FIELD cArray AS CHARACTER
    FIELD iType  AS INTEGER
    FIELD iExt   AS INTEGER
    INDEX ixKind IS PRIMARY iKind iPos.
DEFINE VARIABLE gcDbRKey   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDbWKey   AS CHARACTER NO-UNDO.

DEFINE STREAM sIn.
DEFINE STREAM sFile.

/* The PROPATHs and the compile options the requests refer to by id, and the
   compile procedures written so far, by shape. */
DEFINE TEMP-TABLE ttPath NO-UNDO
    FIELD cId   AS CHARACTER
    FIELD cPath AS CHARACTER
    INDEX ixId IS UNIQUE PRIMARY cId.
DEFINE TEMP-TABLE ttOpt NO-UNDO
    FIELD cId   AS CHARACTER
    FIELD cHead AS CHARACTER
    FIELD cTail AS CHARACTER
    INDEX ixId IS UNIQUE PRIMARY cId.
DEFINE TEMP-TABLE ttGen NO-UNDO
    FIELD cKey AS CHARACTER
    INDEX ixKey IS UNIQUE PRIMARY cKey.
DEFINE TEMP-TABLE ttUnit NO-UNDO
    FIELD iSeq  AS INTEGER
    FIELD cName AS CHARACTER
    INDEX ixSeq IS UNIQUE PRIMARY iSeq.

/* Every row of a user table, field and index, by record id. */
DEFINE TEMP-TABLE ttPtr NO-UNDO
    FIELD cKey  AS CHARACTER
    FIELD cDesc AS CHARACTER
    INDEX ixKey IS UNIQUE PRIMARY cKey.

FUNCTION intText RETURNS CHARACTER (INPUT piValue AS INT64):
    RETURN STRING(piValue).
END FUNCTION.

FUNCTION wireBytes RETURNS CHARACTER (INPUT pcValue AS CHARACTER) FORWARD.

/* Percent-encodes the bytes of the value in the session code page. A value
   made only of the characters in gcSafe is already its own encoding; that is
   the common case, so it is checked first. TRIM removes leading and trailing
   characters of the set, so it leaves "" exactly when every character is in it. */
FUNCTION toWire RETURNS CHARACTER (INPUT pcValue AS CHARACTER):
    IF pcValue = ? THEN RETURN "".
    IF TRIM(pcValue, gcSafe) = "" THEN RETURN pcValue.
    RETURN wireBytes(pcValue).
END FUNCTION.

FUNCTION wireBytes RETURNS CHARACTER (INPUT pcValue AS CHARACTER):
    DEFINE VARIABLE mp    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE cOut  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLen  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iByte AS INTEGER   NO-UNDO.

    IF pcValue = ? THEN pcValue = "".
    iLen = LENGTH(pcValue, "RAW").
    IF iLen = 0 THEN RETURN "".
    SET-SIZE(mp) = iLen + 1.
    PUT-STRING(mp, 1) = pcValue.
    DO i = 1 TO iLen:
        iByte = GET-BYTE(mp, i).
        IF iByte >= 32 AND iByte <= 126 AND iByte <> 37 THEN
            cOut = cOut + CHR(iByte).
        ELSE
            cOut = cOut + "%"
                 + SUBSTRING("0123456789ABCDEF", INTEGER(TRUNCATE(iByte / 16, 0)) + 1, 1)
                 + SUBSTRING("0123456789ABCDEF", (iByte MODULO 16) + 1, 1).
    END.
    SET-SIZE(mp) = 0.
    RETURN cOut.
END FUNCTION.

/* Reverses toWire. oxabl writes ASCII only, so one character is one byte. */
FUNCTION fromWire RETURNS CHARACTER (INPUT pcValue AS CHARACTER):
    DEFINE VARIABLE mp    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE cOut  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLen  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE n     AS INTEGER   NO-UNDO INITIAL 0.
    DEFINE VARIABLE iByte AS INTEGER   NO-UNDO.

    iLen = LENGTH(pcValue, "RAW").
    IF iLen = 0 THEN RETURN "".
    SET-SIZE(mp) = iLen + 1.
    DO WHILE i <= iLen:
        IF SUBSTRING(pcValue, i, 1) = "%" THEN DO:
            iByte = (INDEX("0123456789ABCDEF", UPPER(SUBSTRING(pcValue, i + 1, 1))) - 1) * 16
                  + (INDEX("0123456789ABCDEF", UPPER(SUBSTRING(pcValue, i + 2, 1))) - 1).
            i = i + 3.
        END.
        ELSE DO:
            iByte = ASC(SUBSTRING(pcValue, i, 1)).
            i = i + 1.
        END.
        n = n + 1.
        PUT-BYTE(mp, n) = iByte.
    END.
    PUT-BYTE(mp, n + 1) = 0.
    cOut = GET-STRING(mp, 1).
    SET-SIZE(mp) = 0.
    RETURN cOut.
END FUNCTION.

FUNCTION ptrDesc RETURNS CHARACTER (INPUT pcKey AS CHARACTER):
    FIND FIRST ttPtr WHERE ttPtr.cKey = pcKey NO-ERROR.
    IF AVAILABLE ttPtr THEN RETURN ttPtr.cDesc.
    RETURN ?.
END FUNCTION.

/* The value of one buffer field as text: ? is unknown, RAW is hex, a RECID
   or ROWID is the row it points at when that row is in the image. */
FUNCTION fieldText RETURNS CHARACTER (INPUT phFld AS HANDLE, INPUT piIndex AS INTEGER):
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDesc AS CHARACTER NO-UNDO INITIAL ?.
    DEFINE VARIABLE rRaw  AS RAW       NO-UNDO.

    cType = phFld:DATA-TYPE.
    IF piIndex = 0 THEN DO:
        IF phFld:BUFFER-VALUE = ? THEN RETURN "?".
        IF cType = "RAW" THEN DO:
            rRaw = phFld:BUFFER-VALUE.
            cText = HEX-ENCODE(rRaw).
            RETURN cText.
        END.
        cText = STRING(phFld:BUFFER-VALUE).
    END.
    ELSE DO:
        IF phFld:BUFFER-VALUE(piIndex) = ? THEN RETURN "?".
        IF cType = "RAW" THEN DO:
            rRaw = phFld:BUFFER-VALUE(piIndex).
            cText = HEX-ENCODE(rRaw).
            RETURN cText.
        END.
        cText = STRING(phFld:BUFFER-VALUE(piIndex)).
    END.
    IF cType = "RECID" THEN cDesc = ptrDesc("r" + cText).
    ELSE IF cType = "ROWID" THEN cDesc = ptrDesc("w" + cText).
    IF cDesc <> ? THEN RETURN cDesc.
    RETURN cText.
END FUNCTION.

/* The text of the named field of the current record of a buffer. */
FUNCTION bv RETURNS CHARACTER (INPUT phBuf AS HANDLE, INPUT pcName AS CHARACTER):
    RETURN fieldText(phBuf:BUFFER-FIELD(pcName), 0).
END FUNCTION.

/* The key of the row that a RECID field points at. */
FUNCTION rowKey RETURNS CHARACTER (INPUT phBuf AS HANDLE, INPUT pcName AS CHARACTER):
    RETURN "r" + STRING(phBuf:BUFFER-FIELD(pcName):BUFFER-VALUE).
END FUNCTION.

/* Fields that differ between two databases loaded from one .df (time stamps,
   storage areas), or that hold large objects, stay out of the image. */
FUNCTION skipField RETURNS LOGICAL (INPUT phFld AS HANDLE):
    DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

    cName = LOWER(phFld:NAME).
    IF phFld:DATA-TYPE = "BLOB" OR phFld:DATA-TYPE = "CLOB" THEN RETURN TRUE.
    IF cName = "_last-change" OR cName = "_last-modified" OR cName = "_mod-sequence"
       OR cName = "_ianum" THEN RETURN TRUE.
    IF INDEX(cName, "area") > 0 THEN RETURN TRUE.
    RETURN FALSE.
END FUNCTION.

PROCEDURE sendLine:
    DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO.

    PUT UNFORMATTED "OXW1" + gcTab + pcLine SKIP.
END PROCEDURE.

PROCEDURE failWith:
    DEFINE INPUT PARAMETER pcText AS CHARACTER NO-UNDO.

    RUN sendLine("ERROR" + gcTab + toWire(pcText)).
    glQuit = TRUE.
END PROCEDURE.

PROCEDURE putEnv:
    DEFINE INPUT PARAMETER pcName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.

    giCount = giCount + 1.
    RUN sendLine("ENV" + gcTab + toWire(pcName) + gcTab + toWire(pcValue)).
END PROCEDURE.

PROCEDURE hello:
    DEFINE INPUT PARAMETER pcToken AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDlc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVersion AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cAliases AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j        AS INTEGER   NO-UNDO.

    giCount = 0.
    RUN sendLine("HELLO" + gcTab + "1" + gcTab + toWire(pcToken)).

    cDlc = OS-GETENV("DLC").
    IF cDlc = ? THEN cDlc = "".
    FILE-INFO:FILE-NAME = cDlc + "/version".
    IF cDlc <> "" AND FILE-INFO:FULL-PATHNAME <> ? THEN DO:
        INPUT STREAM sFile FROM VALUE(FILE-INFO:FULL-PATHNAME).
        IMPORT STREAM sFile UNFORMATTED cVersion.
        INPUT STREAM sFile CLOSE.
        cVersion = RIGHT-TRIM(cVersion).
    END.

    RUN putEnv("program", "1").
    RUN putEnv("dlc", cDlc).
    RUN putEnv("dlc_version", cVersion).
    RUN putEnv("proversion", PROVERSION).
    RUN putEnv("opsys", OPSYS).
    RUN putEnv("window_system", SESSION:WINDOW-SYSTEM).
    RUN putEnv("batch_mode", STRING(SESSION:BATCH-MODE, "yes/no")).
    RUN putEnv("process_architecture", intText(PROCESS-ARCHITECTURE)).
    RUN putEnv("cpinternal", SESSION:CPINTERNAL).
    RUN putEnv("cpstream", SESSION:CPSTREAM).
    RUN putEnv("cpcoll", SESSION:CPCOLL).
    RUN putEnv("cpcase", SESSION:CPCASE).
    RUN putEnv("date_format", SESSION:DATE-FORMAT).
    RUN putEnv("year_offset", intText(SESSION:YEAR-OFFSET)).
    RUN putEnv("numeric_decimal_point", SESSION:NUMERIC-DECIMAL-POINT).
    RUN putEnv("numeric_separator", SESSION:NUMERIC-SEPARATOR).
    RUN putEnv("startup_parameters", SESSION:STARTUP-PARAMETERS).
    RUN putEnv("propath", gcStartPath).
    RUN putEnv("cwd", gcCwd).

    DO i = 1 TO NUM-DBS:
        cAliases = "".
        DO j = 1 TO NUM-ALIASES:
            IF LDBNAME(ALIAS(j)) = LDBNAME(i) THEN
                cAliases = cAliases + (IF cAliases = "" THEN "" ELSE ",") + ALIAS(j).
        END.
        giCount = giCount + 1.
        RUN sendLine("DB" + gcTab + intText(i) + gcTab + toWire(LDBNAME(i)) + gcTab
                 + toWire(DBTYPE(i)) + gcTab + toWire(cAliases)).
    END.
    RUN sendLine("ENV-END" + gcTab + intText(giCount)).
END PROCEDURE.

/* Appends a CHARACTER to a LONGCHAR that holds piLength characters. The +
   operator would cap the result at 32,000 characters. */
PROCEDURE appendLong:
    DEFINE INPUT-OUTPUT PARAMETER pcTarget AS LONGCHAR  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER piLength AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcText   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcPiece AS LONGCHAR NO-UNDO.

    IF pcText = "" THEN RETURN.
    lcPiece = pcText.
    COPY-LOB FROM lcPiece TO pcTarget OVERLAY AT piLength + 1.
    piLength = piLength + LENGTH(pcText, "CHARACTER").
END PROCEDURE.

/* Adds text to the entry. */
PROCEDURE appendText:
    DEFINE INPUT PARAMETER pcText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iLen AS INTEGER NO-UNDO.

    iLen = LENGTH(pcText, "RAW").
    IF giBuf + iLen > 30000 THEN DO:
        RUN appendLong(INPUT-OUTPUT glcText, INPUT-OUTPUT giLong, gcBuf).
        gcBuf = "".
        giBuf = 0.
    END.
    gcBuf = gcBuf + pcText.
    giBuf = giBuf + iLen.
END PROCEDURE.

/* Adds one field of the current record: percent-encoded, after a TAB unless
   it is the first. A long value goes in slices of 2,000 characters, because
   its encoding can outgrow a CHARACTER. */
PROCEDURE putField:
    DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iLen AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPos AS INTEGER NO-UNDO INITIAL 1.

    IF giInRecord > 0 THEN RUN appendText(gcTab).
    giInRecord = giInRecord + 1.
    iLen = LENGTH(pcValue, "CHARACTER").
    IF iLen <= 2000 THEN DO:
        RUN appendText(toWire(pcValue)).
        RETURN.
    END.
    DO WHILE iPos <= iLen:
        RUN appendText(toWire(SUBSTRING(pcValue, iPos, 2000, "CHARACTER"))).
        iPos = iPos + 2000.
    END.
END PROCEDURE.

/* The text of one value of a field, appended to the entry. piIndex is 0 for a
   field with no extent. piType is giFldType. A value is ? when unknown, RAW is
   hex, and a RECID or ROWID is the row it points at when that row is known.
   A few metaschema CHARACTER fields hold more than 32,000 characters, which a
   CHARACTER variable cannot take, so those are read as a LONGCHAR and a long
   one goes in slices. */
PROCEDURE putElement:
    DEFINE INPUT PARAMETER phFld   AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER piIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piType  AS INTEGER NO-UNDO.

    DEFINE VARIABLE lcValue AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE rRaw    AS RAW       NO-UNDO.
    DEFINE VARIABLE cText   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDesc   AS CHARACTER NO-UNDO INITIAL ?.
    DEFINE VARIABLE iLen    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPos    AS INTEGER   NO-UNDO INITIAL 1.

    CASE piType:
        WHEN 1 THEN DO:
            IF piIndex = 0 THEN lcValue = phFld:BUFFER-VALUE.
            ELSE lcValue = phFld:BUFFER-VALUE(piIndex).
            IF lcValue = ? THEN DO:
                RUN appendText("?").
                RETURN.
            END.
            iLen = LENGTH(lcValue, "CHARACTER").
            IF iLen <= 2000 THEN DO:
                cText = lcValue.
                RUN appendText(toWire(cText)).
                RETURN.
            END.
            DO WHILE iPos <= iLen:
                cText = SUBSTRING(lcValue, iPos, 2000, "CHARACTER").
                RUN appendText(toWire(cText)).
                iPos = iPos + 2000.
            END.
        END.
        WHEN 2 THEN DO:
            IF piIndex = 0 THEN DO:
                IF phFld:BUFFER-VALUE = ? THEN cText = "?".
                ELSE DO:
                    rRaw = phFld:BUFFER-VALUE.
                    cText = HEX-ENCODE(rRaw).
                END.
            END.
            ELSE DO:
                IF phFld:BUFFER-VALUE(piIndex) = ? THEN cText = "?".
                ELSE DO:
                    rRaw = phFld:BUFFER-VALUE(piIndex).
                    cText = HEX-ENCODE(rRaw).
                END.
            END.
            RUN appendText(cText).
        END.
        OTHERWISE DO:
            IF piIndex = 0 THEN cText = STRING(phFld:BUFFER-VALUE).
            ELSE cText = STRING(phFld:BUFFER-VALUE(piIndex)).
            IF cText = ? THEN DO:
                RUN appendText("?").
                RETURN.
            END.
            IF piType = 3 THEN cDesc = ptrDesc("r" + cText).
            ELSE IF piType = 4 THEN cDesc = ptrDesc("w" + cText).
            IF cDesc <> ? THEN cText = cDesc.
            RUN appendText(toWire(cText)).
        END.
    END CASE.
END PROCEDURE.

/* One record: its type, its name fields (joined with CHR(1)), then one pair
   for each field of the buffer of kind piKind that the image keeps. Kind 0
   has no pairs. A pair is name=value; an array field is one pair,
   name[]=value1%00value2..., with %00 between the encoded values. Each field
   is encoded once, and %00 cannot occur in an encoded value.

   This is the hot loop: a large schema has millions of values. A value that
   is short and already safe goes straight into cRec, which is handed to the
   entry in one piece. Any other value (long, RAW, RECID, ROWID, or needing
   percent-encoding) goes through putElement, after cRec is handed over. */
PROCEDURE putRecord:
    DEFINE INPUT PARAMETER pcType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcNames AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER piKind  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRec  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hFld  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lErr  AS LOGICAL   NO-UNDO.

    giInRecord = 0.
    RUN putField(pcType).
    IF pcNames <> "" THEN
        DO i = 1 TO NUM-ENTRIES(pcNames, CHR(1)):
            RUN putField(ENTRY(i, pcNames, CHR(1))).
        END.
    IF piKind > 0 THEN
        FOR EACH ttMeta WHERE ttMeta.iKind = piKind:
            hFld = ttMeta.hFld.
            IF LENGTH(cRec, "RAW") > 20000 THEN DO:
                RUN appendText(cRec).
                cRec = "".
            END.
            IF ttMeta.iExt = 0 THEN DO:
                IF ttMeta.iType > 1 THEN DO:
                    RUN appendText(cRec + ttMeta.cLabel).
                    cRec = "".
                    RUN putElement(hFld, 0, ttMeta.iType).
                    NEXT.
                END.
                cText = ?.
                lErr = FALSE.
                IF ttMeta.iType = 1 THEN DO:
                    cText = hFld:BUFFER-VALUE NO-ERROR.
                    lErr = ERROR-STATUS:ERROR.
                END.
                ELSE cText = STRING(hFld:BUFFER-VALUE).
                IF lErr OR (cText <> ? AND (LENGTH(cText, "CHARACTER") > 2000
                                          OR TRIM(cText, gcSafe) <> "")) THEN DO:
                    RUN appendText(cRec + ttMeta.cLabel).
                    cRec = "".
                    RUN putElement(hFld, 0, ttMeta.iType).
                    NEXT.
                END.
                cRec = cRec + ttMeta.cLabel + (IF cText = ? THEN "?" ELSE cText).
            END.
            ELSE DO:
                RUN appendText(cRec + ttMeta.cArray).
                cRec = "".
                DO k = 1 TO ttMeta.iExt:
                    IF k > 1 THEN cRec = cRec + "%00".
                    IF ttMeta.iType > 1 THEN DO:
                        RUN appendText(cRec).
                        cRec = "".
                        RUN putElement(hFld, k, ttMeta.iType).
                        NEXT.
                    END.
                    cText = ?.
                    lErr = FALSE.
                    IF ttMeta.iType = 1 THEN DO:
                        cText = hFld:BUFFER-VALUE(k) NO-ERROR.
                        lErr = ERROR-STATUS:ERROR.
                    END.
                    ELSE cText = STRING(hFld:BUFFER-VALUE(k)).
                    IF lErr OR (cText <> ? AND (LENGTH(cText, "CHARACTER") > 2000
                                              OR TRIM(cText, gcSafe) <> "")) THEN DO:
                        RUN appendText(cRec).
                        cRec = "".
                        RUN putElement(hFld, k, ttMeta.iType).
                        NEXT.
                    END.
                    cRec = cRec + (IF cText = ? THEN "?" ELSE cText).
                    IF LENGTH(cRec, "RAW") > 20000 THEN DO:
                        RUN appendText(cRec).
                        cRec = "".
                    END.
                END.
            END.
        END.
    RUN appendText(cRec + CHR(10)).
    giRecords = giRecords + 1.
END PROCEDURE.

/* Ends the entry: SHA-256 of its text, then one SENT line. */
PROCEDURE finishEntry:
    DEFINE INPUT PARAMETER pcKind AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cSha AS CHARACTER NO-UNDO.

    RUN appendLong(INPUT-OUTPUT glcText, INPUT-OUTPUT giLong, gcBuf).
    cSha = LOWER(HEX-ENCODE(MESSAGE-DIGEST("SHA-256", glcText))).
    RUN sendLine("SENT" + gcTab + toWire(gcLdb) + gcTab + pcKind + gcTab + toWire(pcName)
                 + gcTab + intText(giRecords) + gcTab + cSha).
    giEntries = giEntries + 1.
    glcText = "".
    giLong = 0.
    gcBuf = "".
    giBuf = 0.
    giRecords = 0.
END PROCEDURE.

/* Records the current row of a buffer under its RECID and its ROWID. */
PROCEDURE addPtr:
    DEFINE INPUT PARAMETER phBuf  AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER pcDesc AS CHARACTER NO-UNDO.

    CREATE ttPtr.
    ASSIGN ttPtr.cKey = "r" + STRING(phBuf:RECID) ttPtr.cDesc = pcDesc.
    CREATE ttPtr.
    ASSIGN ttPtr.cKey = "w" + STRING(phBuf:ROWID) ttPtr.cDesc = pcDesc.
END PROCEDURE.

/* Forgets the rows of the previous entry; the database row stays known. */
PROCEDURE resetPointers:
    EMPTY TEMP-TABLE ttPtr.
    CREATE ttPtr.
    ASSIGN ttPtr.cKey = gcDbRKey ttPtr.cDesc = "db".
    CREATE ttPtr.
    ASSIGN ttPtr.cKey = gcDbWKey ttPtr.cDesc = "db".
END PROCEDURE.

/* Looks once at every field of the buffer of kind piKind and keeps the fields
   that the image keeps: handle, encoded labels, type (0 other, 1 CHARACTER,
   2 RAW, 3 RECID, 4 ROWID) and extent. */
PROCEDURE prepareKind:
    DEFINE INPUT PARAMETER piKind AS INTEGER NO-UNDO.

    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hFld  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

    DO i = 1 TO ghBuf[piKind]:NUM-FIELDS:
        hFld = ghBuf[piKind]:BUFFER-FIELD(i).
        IF skipField(hFld) THEN NEXT.
        cName = LOWER(hFld:NAME).
        CREATE ttMeta.
        ASSIGN ttMeta.iKind = piKind
               ttMeta.iPos = i
               ttMeta.hFld = hFld
               ttMeta.cLabel = gcTab + toWire(cName + "=")
               ttMeta.cArray = gcTab + toWire(cName + "[]=")
               ttMeta.iType = LOOKUP(hFld:DATA-TYPE, "CHARACTER,RAW,RECID,ROWID")
               ttMeta.iExt = hFld:EXTENT.
    END.
END PROCEDURE.

/* A buffer and a query for each metaschema table of the image, kinds 1 to 8:
   table, field, index, index field, table trigger, field trigger, sequence, db. */
PROCEDURE openKinds:
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttMeta.
    DO i = 1 TO 8:
        cTable = ENTRY(i, "_File,_Field,_Index,_Index-Field,_File-Trig,_Field-Trig,_Sequence,_Db").
        CREATE BUFFER ghBuf[i] FOR TABLE gcLdb + "." + cTable BUFFER-NAME "bK" + STRING(i).
        CREATE QUERY ghQuery[i].
        ghQuery[i]:SET-BUFFERS(ghBuf[i]).
        RUN prepareKind(i).
    END.
END PROCEDURE.

PROCEDURE closeKinds:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    DO i = 1 TO 8:
        IF VALID-HANDLE(ghQuery[i]) THEN DELETE OBJECT ghQuery[i].
        IF VALID-HANDLE(ghBuf[i]) THEN DELETE OBJECT ghBuf[i].
    END.
END PROCEDURE.

/* Runs the internal procedure pcHandler once for each row of the table of
   kind piKind that matches pcWhere, in the order of pcBy. The handler finds
   the row in ghBuf[piKind]. */
PROCEDURE scanKind:
    DEFINE INPUT PARAMETER piKind    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER pcWhere   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcBy      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcHandler AS CHARACTER NO-UNDO.

    ghQuery[piKind]:QUERY-PREPARE("FOR EACH bK" + STRING(piKind) + " " + pcWhere
                                  + " NO-LOCK " + pcBy).
    ghQuery[piKind]:QUERY-OPEN().
    REPEAT:
        ghQuery[piKind]:GET-NEXT(NO-LOCK).
        IF ghQuery[piKind]:QUERY-OFF-END THEN LEAVE.
        RUN VALUE(pcHandler) IN THIS-PROCEDURE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            RUN failWith(pcHandler + ": "
                         + (IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ERROR-STATUS:GET-MESSAGE(1) ELSE "error")).
            LEAVE.
        END.
        IF glQuit THEN LEAVE.
    END.
    ghQuery[piKind]:QUERY-CLOSE().
END PROCEDURE.

PROCEDURE ptrField:
    RUN addPtr(ghBuf[2], "field/" + bv(ghBuf[1], "_File-Name") + "/" + bv(ghBuf[2], "_Field-Name")).
END PROCEDURE.

PROCEDURE ptrIndex:
    RUN addPtr(ghBuf[3], "index/" + bv(ghBuf[1], "_File-Name") + "/" + bv(ghBuf[3], "_Index-Name")).
END PROCEDURE.

PROCEDURE textField:
    RUN putRecord("field", bv(ghBuf[1], "_File-Name") + CHR(1) + bv(ghBuf[2], "_Field-Name"), 2).
END PROCEDURE.

PROCEDURE textIndexField:
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.

    cField = ptrDesc(rowKey(ghBuf[4], "_Field-Recid")).
    IF cField BEGINS "field/" THEN
        RUN putRecord("index-field",
                      bv(ghBuf[1], "_File-Name") + CHR(1) + bv(ghBuf[3], "_Index-Name") + CHR(1)
                      + bv(ghBuf[4], "_Index-Seq") + CHR(1) + ENTRY(3, cField, "/"), 4).
END PROCEDURE.

PROCEDURE textIndex:
    RUN putRecord("index", bv(ghBuf[1], "_File-Name") + CHR(1) + bv(ghBuf[3], "_Index-Name"), 3).
    RUN scanKind(4, "WHERE bK4._Index-Recid = " + STRING(ghBuf[3]:RECID), "BY bK4._Index-Seq",
                 "textIndexField").
END PROCEDURE.

PROCEDURE textTableTrigger:
    RUN putRecord("table-trigger", bv(ghBuf[1], "_File-Name") + CHR(1) + bv(ghBuf[5], "_Event"), 5).
END PROCEDURE.

PROCEDURE textFieldTrigger:
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.

    cField = ptrDesc(rowKey(ghBuf[6], "_Field-Recid")).
    IF cField BEGINS "field/" THEN
        RUN putRecord("field-trigger",
                      bv(ghBuf[1], "_File-Name") + CHR(1) + ENTRY(3, cField, "/") + CHR(1)
                      + bv(ghBuf[6], "_Event"), 6).
END PROCEDURE.

/* One table, with its fields, indexes, index fields and triggers, is one
   entry. The tables are scanned by name, and the table at place i (from 0) is
   imaged by the worker whose part is i modulo the number of parts, so the
   workers share the tables without talking to each other. The first scans record which row each field and index is, so that
   a RECID field is written as the row it points at. */
PROCEDURE imageTable:
    DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.

    giTables = giTables + 1.
    IF (giTables - 1) MODULO giParts <> giPart THEN RETURN.
    cWhere = " = " + STRING(ghBuf[1]:RECID).
    RUN resetPointers.
    RUN addPtr(ghBuf[1], "table/" + bv(ghBuf[1], "_File-Name")).
    RUN scanKind(2, "WHERE bK2._File-Recid" + cWhere, "", "ptrField").
    RUN scanKind(3, "WHERE bK3._File-Recid" + cWhere, "", "ptrIndex").
    RUN putRecord("table", bv(ghBuf[1], "_File-Name"), 1).
    RUN scanKind(2, "WHERE bK2._File-Recid" + cWhere, "BY bK2._Field-Name", "textField").
    RUN scanKind(3, "WHERE bK3._File-Recid" + cWhere, "BY bK3._Index-Name", "textIndex").
    RUN scanKind(5, "WHERE bK5._File-Recid" + cWhere, "BY bK5._Event", "textTableTrigger").
    RUN scanKind(6, "WHERE bK6._File-Recid" + cWhere, "BY bK6._Field-Recid BY bK6._Event",
                 "textFieldTrigger").
    RUN finishEntry("table", bv(ghBuf[1], "_File-Name")).
END PROCEDURE.

PROCEDURE imageSequence:
    RUN resetPointers.
    RUN putRecord("sequence", bv(ghBuf[7], "_Seq-Name"), 7).
    RUN finishEntry("sequence", bv(ghBuf[7], "_Seq-Name")).
END PROCEDURE.

/* The database row is the `db` entry; only three of its fields are kept. */
PROCEDURE imageDb:
    DEFINE VARIABLE cNames AS CHARACTER NO-UNDO.

    gcDbRKey = "r" + STRING(ghBuf[8]:RECID).
    gcDbWKey = "w" + STRING(ghBuf[8]:ROWID).
    IF giPart <> 0 THEN RETURN.
    cNames = "_db-coll-name=" + bv(ghBuf[8], "_Db-coll-name") + CHR(1)
           + "_db-type=" + bv(ghBuf[8], "_Db-type") + CHR(1)
           + "_db-xl-name=" + bv(ghBuf[8], "_Db-xl-name").
    RUN putRecord("db", cNames, 0).
    RUN finishEntry("db", "").
END PROCEDURE.

/* One line of the `system` entry: the name and CRC of a table that is not a
   user table. R-code that reads such a table records its CRC. */
PROCEDURE imageSystemTable:
    RUN putRecord("system-table", bv(ghBuf[1], "_File-Name") + CHR(1)
                  + "_crc=" + bv(ghBuf[1], "_CRC"), 0).
END PROCEDURE.

/* The share of the schema image of one connected database that worker piPart
   of piParts images: one SENT line for each of its entries, then SCHEMA-END
   with the number of entries and the number of user tables in the whole
   database. Part 0 images the database record, the system tables (virtual
   system tables, the metaschema and the SQL catalog) and the sequences. */
PROCEDURE schemaImage:
    DEFINE INPUT PARAMETER pcLdb   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER piPart  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER piParts AS INTEGER   NO-UNDO.

    IF NOT CONNECTED(pcLdb) THEN DO:
        RUN failWith("database " + pcLdb + " is not connected").
        RETURN.
    END.
    IF piParts < 1 OR piPart < 0 OR piPart >= piParts THEN DO:
        RUN failWith("bad share " + intText(piPart) + " of " + intText(piParts)).
        RETURN.
    END.
    gcLdb = pcLdb.
    giPart = piPart.
    giParts = piParts.
    giEntries = 0.
    giTables = 0.
    glcText = "".
    giLong = 0.
    gcBuf = "".
    giBuf = 0.
    giRecords = 0.
    RUN openKinds.
    RUN scanKind(8, "", "", "imageDb").
    RUN scanKind(1, "WHERE bK1._Tbl-Type = 'T'", "BY bK1._File-Name", "imageTable").
    IF piPart = 0 THEN DO:
        RUN scanKind(1, "WHERE bK1._Tbl-Type <> 'T'", "BY bK1._File-Name BY bK1._Owner",
                     "imageSystemTable").
        IF NOT glQuit THEN RUN finishEntry("system", "").
        RUN scanKind(7, "", "BY bK7._Seq-Name", "imageSequence").
    END.
    RUN closeKinds.
    IF NOT glQuit THEN
        RUN sendLine("SCHEMA-END" + gcTab + toWire(pcLdb) + gcTab + intText(giEntries)
                     + gcTab + intText(giTables)).
END PROCEDURE.

/* A cheap description of a connected database: its GUID and the stamp of its
   last schema change. The stamp has a resolution of one second and not every
   schema change moves it, so it only tells workers apart. */
PROCEDURE fingerprint:
    DEFINE INPUT PARAMETER pcLdb AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hBuf   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cGuid  AS CHARACTER NO-UNDO INITIAL "?".
    DEFINE VARIABLE cStamp AS CHARACTER NO-UNDO INITIAL "?".

    IF NOT CONNECTED(pcLdb) THEN DO:
        RUN failWith("database " + pcLdb + " is not connected").
        RETURN.
    END.
    CREATE BUFFER hBuf FOR TABLE pcLdb + "._Db" BUFFER-NAME "bFp".
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuf).
    hQuery:QUERY-PREPARE("FOR EACH bFp NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST(NO-LOCK).
    IF NOT hQuery:QUERY-OFF-END THEN cGuid = bv(hBuf, "_Db-guid").
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    DELETE OBJECT hBuf.
    CREATE BUFFER hBuf FOR TABLE pcLdb + "._DbStatus" BUFFER-NAME "bFs".
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuf).
    hQuery:QUERY-PREPARE("FOR EACH bFs NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST(NO-LOCK).
    IF NOT hQuery:QUERY-OFF-END THEN cStamp = bv(hBuf, "_DbStatus-CacheStamp").
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    DELETE OBJECT hBuf.
    RUN sendLine("FPRINT" + gcTab + toWire(pcLdb) + gcTab + toWire(cGuid) + gcTab + toWire(cStamp)).
END PROCEDURE.

/* Finds what is at a worker-side path: file with its size and SHA-256, dir or none. */
PROCEDURE hashPath:
    DEFINE INPUT  PARAMETER pcPath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcKind AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcSize AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcSha  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDlc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mData AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE iSize AS INT64     NO-UNDO.

    ASSIGN pcKind = "none" pcSize = "-" pcSha = "-".
    cDlc = OS-GETENV("DLC").
    IF cDlc = ? THEN cDlc = "".
    cPath = pcPath.
    IF cPath = "$DLC" THEN cPath = cDlc.
    ELSE IF cPath BEGINS "$DLC/" THEN cPath = RIGHT-TRIM(cDlc, "/") + "/" + SUBSTRING(cPath, 6).
    /* An absolute name, so that FILE-INFO does not search the PROPATH. */
    IF NOT cPath BEGINS "/" THEN cPath = gcCwd + "/" + cPath.

    FILE-INFO:FILE-NAME = cPath.
    IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
    IF INDEX(FILE-INFO:FILE-TYPE, "D") > 0 THEN DO:
        pcKind = "dir".
        RETURN.
    END.
    IF INDEX(FILE-INFO:FILE-TYPE, "F") = 0 THEN RETURN.

    iSize = FILE-INFO:FILE-SIZE.
    pcKind = "file".
    pcSize = intText(iSize).
    IF iSize = 0 THEN
        pcSha = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".
    ELSE DO:
        cPath = FILE-INFO:FULL-PATHNAME.
        COPY-LOB FROM FILE cPath TO mData.
        pcSha = LOWER(HEX-ENCODE(MESSAGE-DIGEST("SHA-256", mData))).
        SET-SIZE(mData) = 0.
    END.
END PROCEDURE.

PROCEDURE hashOne:
    DEFINE INPUT PARAMETER pcId   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcPath AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cKind AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSize AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSha  AS CHARACTER NO-UNDO.

    RUN hashPath(pcPath, OUTPUT cKind, OUTPUT cSize, OUTPUT cSha).
    RUN sendLine("HASHED" + gcTab + toWire(pcId) + gcTab + cKind + gcTab + cSize + gcTab + cSha).
END PROCEDURE.

/* HASHSET: reads the PATH lines, then answers with the SHA-256 of the lines
   "<path> TAB <digest, dir or none> LF". */
PROCEDURE hashSet:
    DEFINE INPUT PARAMETER pcId    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER piCount AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lcList AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iList  AS INTEGER   NO-UNDO INITIAL 0.
    DEFINE VARIABLE mList  AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE cLine  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKind  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSize  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSha   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWord  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

    DO i = 1 TO piCount:
        IMPORT STREAM sIn UNFORMATTED cLine.
        cLine = RIGHT-TRIM(cLine, CHR(13)).
        IF ENTRY(1, cLine, gcTab) <> "OXW1" OR ENTRY(2, cLine, gcTab) <> "PATH" THEN DO:
            RUN failWith("expected PATH").
            RETURN.
        END.
        cPath = fromWire(ENTRY(4, cLine, gcTab)).
        RUN hashPath(cPath, OUTPUT cKind, OUTPUT cSize, OUTPUT cSha).
        cWord = IF cKind = "file" THEN cSha ELSE cKind.
        RUN appendLong(INPUT-OUTPUT lcList, INPUT-OUTPUT iList, cPath + gcTab + cWord + CHR(10)).
    END.
    IF piCount = 0 THEN
        cSha = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".
    ELSE DO:
        COPY-LOB FROM lcList TO mList.
        cSha = LOWER(HEX-ENCODE(MESSAGE-DIGEST("SHA-256", mList))).
        SET-SIZE(mList) = 0.
    END.
    RUN sendLine("HASHSET-DONE" + gcTab + toWire(pcId) + gcTab + cSha).
END PROCEDURE.

/* The name of a unit without its extension: the path of its r-code. */
FUNCTION stemOf RETURNS CHARACTER (INPUT pcName AS CHARACTER):
    DEFINE VARIABLE iSlash AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDot   AS INTEGER NO-UNDO.

    iSlash = R-INDEX(pcName, "/").
    iDot = R-INDEX(pcName, ".").
    IF iDot > iSlash AND iDot > 1 THEN RETURN SUBSTRING(pcName, 1, iDot - 1).
    RETURN pcName.
END FUNCTION.

FUNCTION dirOf RETURNS CHARACTER (INPUT pcPath AS CHARACTER):
    DEFINE VARIABLE iSlash AS INTEGER NO-UNDO.

    iSlash = R-INDEX(pcPath, "/").
    IF iSlash > 1 THEN RETURN SUBSTRING(pcPath, 1, iSlash - 1).
    RETURN "".
END FUNCTION.

/* Creates every missing level of a directory: SAVE INTO makes the package
   directories of a class but not the directory of a procedure. */
PROCEDURE ensureDir:
    DEFINE INPUT PARAMETER pcDir AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPart  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoFar AS CHARACTER NO-UNDO.

    IF pcDir = "" THEN RETURN.
    DO i = 1 TO NUM-ENTRIES(pcDir, "/"):
        cPart = ENTRY(i, pcDir, "/").
        cSoFar = cSoFar + (IF i > 1 THEN "/" ELSE "") + cPart.
        IF cPart <> "" AND cPart <> "." THEN OS-CREATE-DIR VALUE(cSoFar).
    END.
END PROCEDURE.

/* PROPATH id entry...: remembers a PROPATH under its id. An entry that starts
   with $DLC names the installation, so it is written out with the DLC of this
   process. */
PROCEDURE definePropath:
    DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cWantId   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDlc   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

    cDlc = OS-GETENV("DLC").
    IF cDlc = ? THEN cDlc = "".
    cWantId = fromWire(ENTRY(3, pcLine, gcTab)).
    DO i = 4 TO NUM-ENTRIES(pcLine, gcTab):
        cEntry = fromWire(ENTRY(i, pcLine, gcTab)).
        IF cEntry = "$DLC" OR cEntry BEGINS "$DLC/" THEN
            cEntry = RIGHT-TRIM(cDlc, "/") + SUBSTRING(cEntry, 5).
        cList = cList + (IF cList = "" THEN "" ELSE ",") + cEntry.
    END.
    FIND ttPath WHERE ttPath.cId = cWantId NO-ERROR.
    IF NOT AVAILABLE ttPath THEN CREATE ttPath.
    ASSIGN ttPath.cId = cWantId
           ttPath.cPath = cList.
    RUN sendLine("OK" + gcTab + toWire(cWantId)).
END PROCEDURE.

/* OPTIONS id name=value...: remembers the compile options under their id as
   the two halves of the COMPILE phrase. oxabl sends only names made of
   letters, digits and "-_:,.", so a value needs no quoting. */
PROCEDURE defineOptions:
    DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cWantId    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPair  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeadText  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLang  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFlags AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpts  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAt    AS INTEGER   NO-UNDO.

    cWantId = fromWire(ENTRY(3, pcLine, gcTab)).
    DO i = 4 TO NUM-ENTRIES(pcLine, gcTab):
        cPair = fromWire(ENTRY(i, pcLine, gcTab)).
        iAt = INDEX(cPair, "=").
        cName = SUBSTRING(cPair, 1, iAt - 1).
        cValue = SUBSTRING(cPair, iAt + 1).
        CASE cName:
            WHEN "attr-space" THEN DO:
                IF cValue = "true" THEN cHeadText = " ATTR-SPACE".
            END.
            WHEN "languages" THEN DO:
                IF cValue <> "" THEN cLang = ' LANGUAGES ("' + REPLACE(cValue, ":", '","') + '")'.
            END.
            WHEN "min-size" THEN DO:
                IF cValue = "true" THEN cFlags = cFlags + " MIN-SIZE".
            END.
            WHEN "generate-md5" THEN DO:
                IF cValue = "true" THEN cFlags = cFlags + " GENERATE-MD5".
            END.
            WHEN "stream-io" THEN DO:
                IF cValue = "true" THEN cFlags = cFlags + " STREAM-IO".
            END.
            WHEN "v6frame" THEN DO:
                IF cValue = "true" THEN cFlags = cFlags + " V6FRAME".
            END.
            WHEN "options" THEN DO:
                IF cValue <> "" THEN cOpts = ' OPTIONS "' + cValue + '"'.
            END.
        END CASE.
    END.
    FIND ttOpt WHERE ttOpt.cId = cWantId NO-ERROR.
    IF NOT AVAILABLE ttOpt THEN CREATE ttOpt.
    ASSIGN ttOpt.cId = cWantId
           ttOpt.cHead = cHeadText
           ttOpt.cTail = cLang + cFlags + cOpts.
    RUN sendLine("OK" + gcTab + toWire(cWantId)).
END PROCEDURE.

/* Writes the procedure that holds one shape of COMPILE statement. The
   statement takes the output files as run-time values, but its phrases
   (XREF-XML, MIN-SIZE, ...) are keywords, so each shape is its own source. */
PROCEDURE writeCompiler:
    DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcXref AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER plDebug AS LOGICAL  NO-UNDO.
    DEFINE INPUT PARAMETER plList AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER pcHead AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcTail AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cStatement AS CHARACTER NO-UNDO.

    cStatement = "COMPILE VALUE(pcName)" + pcHead + " SAVE INTO VALUE(pcInto)".
    IF plList THEN cStatement = cStatement + " LISTING VALUE(pcLst)".
    IF pcXref = "xref-xml" OR pcXref = "private" THEN cStatement = cStatement + " XREF-XML VALUE(pcXref)".
    IF pcXref = "xref" THEN cStatement = cStatement + " XREF VALUE(pcXref)".
    IF plDebug THEN cStatement = cStatement + " DEBUG-LIST VALUE(pcDbg)".
    cStatement = cStatement + pcTail + " NO-ERROR.".

    RUN ensureDir(dirOf(pcFile)).
    OUTPUT STREAM sFile TO VALUE(pcFile).
    PUT STREAM sFile UNFORMATTED "DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE INPUT PARAMETER pcInto AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE INPUT PARAMETER pcXref AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE INPUT PARAMETER pcDbg AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE INPUT PARAMETER pcLst AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE OUTPUT PARAMETER pcErr AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED "DEFINE VARIABLE i AS INTEGER NO-UNDO." SKIP.
    PUT STREAM sFile UNFORMATTED cStatement SKIP.
    PUT STREAM sFile UNFORMATTED "IF ERROR-STATUS:ERROR THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:" SKIP.
    PUT STREAM sFile UNFORMATTED "    pcErr = pcErr + (IF pcErr = '' THEN '' ELSE ' ') + ERROR-STATUS:GET-MESSAGE(i)." SKIP.
    PUT STREAM sFile UNFORMATTED "END." SKIP.
    OUTPUT STREAM sFile CLOSE.
END PROCEDURE.

/* Compiles one unit into the stage and answers with its messages and DONE.
   A failure before the compile counts as a failed unit with a message of
   number 0. */
PROCEDURE compileUnit:
    DEFINE INPUT PARAMETER pcReq   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcStage AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcOpt   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcXref  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER plDebug AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER plList  AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cStem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShape      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGen      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXrefFile AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cDbgFile  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cLstFile  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cCompErr  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE lOk       AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE iMsg      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMs       AS INT64     NO-UNDO.
    DEFINE VARIABLE cType     AS CHARACTER NO-UNDO.

    ETIME(TRUE).
    cStem = stemOf(pcName).
    FIND ttOpt WHERE ttOpt.cId = pcOpt NO-ERROR.
    IF NOT AVAILABLE ttOpt THEN DO:
        RUN failWith("unknown OPTIONS " + pcOpt).
        RETURN.
    END.
    cShape = pcOpt + "-" + pcXref + "-" + STRING(plDebug, "1/0") + STRING(plList, "1/0").
    cGen = pcStage + "/gen/c-" + cShape + ".p".
    FIND ttGen WHERE ttGen.cKey = cShape NO-ERROR.
    IF NOT AVAILABLE ttGen THEN DO:
        RUN writeCompiler(cGen, pcXref, plDebug, plList, ttOpt.cHead, ttOpt.cTail).
        CREATE ttGen.
        ttGen.cKey = cShape.
    END.

    RUN ensureDir(pcStage + "/r/" + dirOf(cStem)).
    IF pcXref = "xref-xml" THEN cXrefFile = pcStage + "/xref/" + pcName + ".xref.xml".
    IF pcXref = "private" THEN cXrefFile = pcStage + "/verify/" + pcName + ".xref.xml".
    IF pcXref = "xref" THEN cXrefFile = pcStage + "/xref/" + pcName + ".xref".
    IF cXrefFile <> "" THEN RUN ensureDir(dirOf(cXrefFile)).
    IF plDebug THEN DO:
        cDbgFile = pcStage + "/dbg/" + pcName + ".dbg".
        RUN ensureDir(dirOf(cDbgFile)).
    END.
    IF plList THEN DO:
        cLstFile = pcStage + "/lst/" + pcName + ".lst".
        RUN ensureDir(dirOf(cLstFile)).
    END.

    RUN VALUE(gcCwd + "/" + cGen)
        (INPUT pcName, INPUT pcStage + "/r", INPUT cXrefFile, INPUT cDbgFile, INPUT cLstFile,
         OUTPUT cCompErr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        lOk = FALSE.
        RUN sendLine("MSG" + gcTab + toWire(pcReq) + gcTab + toWire(pcName) + gcTab + "error"
                     + gcTab + "0" + gcTab + toWire(pcName) + gcTab + "0" + gcTab + "0" + gcTab
                     + toWire(IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ERROR-STATUS:GET-MESSAGE(1)
                              ELSE "the compile procedure failed")).
    END.
    ELSE DO:
        IF COMPILER:ERROR THEN lOk = FALSE.
        IF cCompErr <> "" THEN DO:
            lOk = FALSE.
            RUN sendLine("MSG" + gcTab + toWire(pcReq) + gcTab + toWire(pcName) + gcTab + "error"
                         + gcTab + "0" + gcTab + toWire(pcName) + gcTab + "0" + gcTab + "0" + gcTab
                         + toWire(cCompErr)).
        END.
        DO iMsg = 1 TO COMPILER:NUM-MESSAGES:
            cType = IF COMPILER:GET-MESSAGE-TYPE(iMsg) = 2 THEN "warning" ELSE "error".
            RUN sendLine("MSG" + gcTab + toWire(pcReq) + gcTab + toWire(pcName) + gcTab + cType
                         + gcTab + intText(COMPILER:GET-NUMBER(iMsg))
                         + gcTab + toWire(COMPILER:GET-FILE-NAME(iMsg))
                         + gcTab + intText(COMPILER:GET-ROW(iMsg))
                         + gcTab + intText(COMPILER:GET-COLUMN(iMsg))
                         + gcTab + toWire(COMPILER:GET-MESSAGE(iMsg))).
        END.
    END.
    iMs = ETIME.
    RUN sendLine("DONE" + gcTab + toWire(pcReq) + gcTab + toWire(pcName) + gcTab
                 + (IF lOk THEN "ok" ELSE "fail") + gcTab + intText(iMs)).
END PROCEDURE.

/* COMPILE req propath options stage xref debug-list listing count, then count
   UNIT lines. Each unit gets its MSG lines and a DONE; END closes the request. */
PROCEDURE compileRequest:
    DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReq   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProp  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpt   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXref  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDebug AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lList  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRead  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

    cReq = fromWire(ENTRY(3, pcLine, gcTab)).
    cProp = fromWire(ENTRY(4, pcLine, gcTab)).
    cOpt = fromWire(ENTRY(5, pcLine, gcTab)).
    cStage = fromWire(ENTRY(6, pcLine, gcTab)).
    cXref = fromWire(ENTRY(7, pcLine, gcTab)).
    lDebug = (fromWire(ENTRY(8, pcLine, gcTab)) = "1").
    lList = (fromWire(ENTRY(9, pcLine, gcTab)) = "1").
    iCount = INTEGER(fromWire(ENTRY(10, pcLine, gcTab))).

    EMPTY TEMP-TABLE ttUnit.
    DO i = 1 TO iCount:
        IMPORT STREAM sIn UNFORMATTED cRead.
        cRead = RIGHT-TRIM(cRead, CHR(13)).
        IF ENTRY(1, cRead, gcTab) <> "OXW1" OR ENTRY(2, cRead, gcTab) <> "UNIT" THEN DO:
            RUN failWith("expected UNIT").
            RETURN.
        END.
        CREATE ttUnit.
        ASSIGN ttUnit.iSeq = i
               ttUnit.cName = fromWire(ENTRY(4, cRead, gcTab)).
    END.
    FIND ttPath WHERE ttPath.cId = cProp NO-ERROR.
    IF NOT AVAILABLE ttPath THEN DO:
        RUN failWith("unknown PROPATH " + cProp).
        RETURN.
    END.
    PROPATH = ttPath.cPath.
    FOR EACH ttUnit BY ttUnit.iSeq:
        RUN compileUnit(cReq, ttUnit.cName, cStage, cOpt, cXref, lDebug, lList).
        IF glQuit THEN RETURN.
    END.
    RUN sendLine("END" + gcTab + toWire(cReq)).
END PROCEDURE.

/* Handles one request line. */
PROCEDURE dispatch:
    DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.

    IF NUM-ENTRIES(pcLine, gcTab) < 2 OR ENTRY(1, pcLine, gcTab) <> "OXW1" THEN DO:
        RUN failWith("not a protocol line").
        RETURN.
    END.
    cType = ENTRY(2, pcLine, gcTab).
    CASE cType:
        WHEN "HELLO" THEN DO:
            IF fromWire(ENTRY(3, pcLine, gcTab)) <> "1" THEN
                RUN failWith("unsupported protocol version " + fromWire(ENTRY(3, pcLine, gcTab))).
            ELSE
                RUN hello(fromWire(ENTRY(4, pcLine, gcTab))).
        END.
        WHEN "SCHEMA" THEN
            RUN schemaImage(fromWire(ENTRY(3, pcLine, gcTab)),
                            INTEGER(fromWire(ENTRY(4, pcLine, gcTab))),
                            INTEGER(fromWire(ENTRY(5, pcLine, gcTab)))).
        WHEN "FINGERPRINT" THEN RUN fingerprint(fromWire(ENTRY(3, pcLine, gcTab))).
        WHEN "HASH" THEN RUN hashOne(fromWire(ENTRY(3, pcLine, gcTab)), fromWire(ENTRY(4, pcLine, gcTab))).
        WHEN "HASHSET" THEN RUN hashSet(fromWire(ENTRY(3, pcLine, gcTab)), INTEGER(fromWire(ENTRY(4, pcLine, gcTab)))).
        WHEN "PROPATH" THEN RUN definePropath(pcLine).
        WHEN "OPTIONS" THEN RUN defineOptions(pcLine).
        WHEN "COMPILE" THEN RUN compileRequest(pcLine).
        WHEN "PING" THEN RUN sendLine("PONG" + gcTab + toWire(fromWire(ENTRY(3, pcLine, gcTab)))).
        WHEN "QUIT" THEN DO:
            RUN sendLine("BYE").
            glQuit = TRUE.
        END.
        OTHERWISE RUN failWith("unsupported request " + cType).
    END CASE.
END PROCEDURE.

/* Main block. End of input ends the loop, so a worker whose parent died
   quits at its next read. */
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE iSafe AS INTEGER NO-UNDO.
DO iSafe = 32 TO 126:
    IF iSafe <> 37 THEN gcSafe = gcSafe + CHR(iSafe).
END.
gcTab = CHR(9).
gcStartPath = PROPATH.
FILE-INFO:FILE-NAME = ".".
gcCwd = FILE-INFO:FULL-PATHNAME.
COMPILER:MULTI-COMPILE = TRUE.

INPUT STREAM sIn FROM VALUE("/dev/stdin").

mainLoop:
REPEAT ON ENDKEY UNDO mainLoop, LEAVE mainLoop
       ON ERROR UNDO mainLoop, LEAVE mainLoop:
    IMPORT STREAM sIn UNFORMATTED cLine.
    cLine = RIGHT-TRIM(cLine, CHR(13)).
    RUN dispatch(cLine) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        RUN failWith(IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ERROR-STATUS:GET-MESSAGE(1) ELSE "error").
    END.
    IF glQuit THEN LEAVE mainLoop.
END.

QUIT.
