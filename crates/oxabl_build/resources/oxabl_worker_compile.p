/* The compile part of the oxabl build worker. It holds everything that
   decides how a unit compiles: the session setup, the PROPATH, the compile
   options, the procedure that holds each shape of COMPILE statement and the
   COMPILE itself. The compile key hashes this text and not the driver part,
   so a change here changes every key. It reads no protocol line: the driver
   part decodes the requests and calls the procedures below. */

DEFINE VARIABLE gcCwd AS CHARACTER NO-UNDO.
DEFINE STREAM sGen.

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

/* Sets up the session for the compiles of this process. */
PROCEDURE setupCompile:
    FILE-INFO:FILE-NAME = ".".
    gcCwd = FILE-INFO:FULL-PATHNAME.
    COMPILER:MULTI-COMPILE = TRUE.
END PROCEDURE.

/* Remembers a PROPATH under its id. pcList holds the entries, separated by a
   tab. An entry that starts with $DLC names the installation, so it is
   written out with the DLC of this process. */
PROCEDURE definePropath:
    DEFINE INPUT PARAMETER pcWantId AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcList   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDlc   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

    cDlc = OS-GETENV("DLC").
    IF cDlc = ? THEN cDlc = "".
    IF pcList <> "" THEN
    DO i = 1 TO NUM-ENTRIES(pcList, CHR(9)):
        cEntry = ENTRY(i, pcList, CHR(9)).
        IF cEntry = "$DLC" OR cEntry BEGINS "$DLC/" THEN
            cEntry = RIGHT-TRIM(cDlc, "/") + SUBSTRING(cEntry, 5).
        cList = cList + (IF cList = "" THEN "" ELSE ",") + cEntry.
    END.
    FIND ttPath WHERE ttPath.cId = pcWantId NO-ERROR.
    IF NOT AVAILABLE ttPath THEN CREATE ttPath.
    ASSIGN ttPath.cId = pcWantId
           ttPath.cPath = cList.
END PROCEDURE.

/* Makes the PROPATH of that id the PROPATH of the session. */
PROCEDURE usePropath:
    DEFINE INPUT  PARAMETER pcId    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER plFound AS LOGICAL   NO-UNDO.

    FIND ttPath WHERE ttPath.cId = pcId NO-ERROR.
    plFound = AVAILABLE ttPath.
    IF plFound THEN PROPATH = ttPath.cPath.
END PROCEDURE.

/* Remembers the compile options under their id as the two halves of the
   COMPILE phrase. pcPairs holds the name=value pairs, separated by a tab.
   oxabl sends only names made of letters, digits and "-_:,.", so a value needs
   no quoting. */
PROCEDURE defineOptions:
    DEFINE INPUT PARAMETER pcWantId AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcPairs  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPair  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeadText  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLang  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFlags AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpts  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAt    AS INTEGER   NO-UNDO.

    IF pcPairs <> "" THEN
    DO i = 1 TO NUM-ENTRIES(pcPairs, CHR(9)):
        cPair = ENTRY(i, pcPairs, CHR(9)).
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
    FIND ttOpt WHERE ttOpt.cId = pcWantId NO-ERROR.
    IF NOT AVAILABLE ttOpt THEN CREATE ttOpt.
    ASSIGN ttOpt.cId = pcWantId
           ttOpt.cHead = cHeadText
           ttOpt.cTail = cLang + cFlags + cOpts.
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
    IF pcXref = "xref-xml" THEN cStatement = cStatement + " XREF-XML VALUE(pcXref)".
    IF pcXref = "xref" OR pcXref = "private" THEN cStatement = cStatement + " XREF VALUE(pcXref)".
    IF plDebug THEN cStatement = cStatement + " DEBUG-LIST VALUE(pcDbg)".
    cStatement = cStatement + pcTail + " NO-ERROR.".

    RUN ensureDir(dirOf(pcFile)).
    OUTPUT STREAM sGen TO VALUE(pcFile).
    PUT STREAM sGen UNFORMATTED "DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE INPUT PARAMETER pcInto AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE INPUT PARAMETER pcXref AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE INPUT PARAMETER pcDbg AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE INPUT PARAMETER pcLst AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE OUTPUT PARAMETER pcErr AS CHARACTER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED "DEFINE VARIABLE i AS INTEGER NO-UNDO." SKIP.
    PUT STREAM sGen UNFORMATTED cStatement SKIP.
    PUT STREAM sGen UNFORMATTED "IF ERROR-STATUS:ERROR THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:" SKIP.
    PUT STREAM sGen UNFORMATTED "    pcErr = pcErr + (IF pcErr = '' THEN '' ELSE ' ') + ERROR-STATUS:GET-MESSAGE(i)." SKIP.
    PUT STREAM sGen UNFORMATTED "END." SKIP.
    OUTPUT STREAM sGen CLOSE.
END PROCEDURE.

/* Compiles one unit into the stage. plKnown is FALSE when the options id is
   not defined. plRunFailed is TRUE when the compile procedure itself failed
   to run, with its first message in pcRunText. pcCompErr holds what the
   procedure collected from ERROR-STATUS. The messages of the compiler stay
   in COMPILER for the caller. */
PROCEDURE compileOne:
    DEFINE INPUT  PARAMETER pcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcStage     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcOpt       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pcXref      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER plDebug     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER plList      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER plKnown     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER plRunFailed AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER pcRunText   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcCompErr   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cStem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShape    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGen      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXrefFile AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cDbgFile  AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE cLstFile  AS CHARACTER NO-UNDO INITIAL "".

    ASSIGN plKnown = TRUE
           plRunFailed = FALSE
           pcRunText = ""
           pcCompErr = "".
    cStem = stemOf(pcName).
    FIND ttOpt WHERE ttOpt.cId = pcOpt NO-ERROR.
    IF NOT AVAILABLE ttOpt THEN DO:
        plKnown = FALSE.
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
    IF pcXref = "private" THEN cXrefFile = pcStage + "/verify/" + pcName + ".xref".
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
         OUTPUT pcCompErr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        plRunFailed = TRUE.
        pcRunText = IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ERROR-STATUS:GET-MESSAGE(1)
                    ELSE "the compile procedure failed".
    END.
END PROCEDURE.
