/*------------------------------------------------------------------------
    File    : grammar-tour.p
    Purpose : One program that shows how the grammar colours ABL. It mixes
              the shapes that often confuse a highlighter: /* nested
              comments /* in nested comments */ */, "quotes" and
              'apostrophes' inside comments, and blocks inside blocks.
    Notes   : It's a sample. It touches no database: only a temp-table.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE APP-NAME  "Grammar tour"  /* a comment after a value */
&SCOPED-DEFINE VERSION   1.2
&GLOBAL-DEFINE MAX-LINES 3
&SCOPED-DEFINE DEBUG     no

DEFINE TEMP-TABLE ttLine NO-UNDO
    FIELD lineNo   AS INTEGER   LABEL "Line"
    FIELD itemCode AS CHARACTER FORMAT "x(12)"
    FIELD qty      AS DECIMAL   DECIMALS 2 INITIAL 0
    FIELD shipped  AS LOGICAL   INITIAL FALSE
    INDEX pkLine IS PRIMARY UNIQUE lineNo.

DEFINE VARIABLE cTitle   AS CHARACTER NO-UNDO INITIAL {&APP-NAME}.
DEFINE VARIABLE cNote    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWords   AS CHARACTER NO-UNDO EXTENT 3
    INITIAL ["one", "two", "three"].
DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAttempt AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBig     AS INT64     NO-UNDO INITIAL 9000000000.
DEFINE VARIABLE dTotal   AS DECIMAL   NO-UNDO INITIAL -12.5.
DEFINE VARIABLE dRate    AS DECIMAL   NO-UNDO INITIAL .075.
DEFINE VARIABLE dtStart  AS DATE      NO-UNDO INITIAL 12/31/2025.
DEFINE VARIABLE end-date AS DATE      NO-UNDO. /* "end" inside a name */
DEFINE VARIABLE do-again AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE hBuffer  AS HANDLE    NO-UNDO.
DEF VAR cShort AS CHAR NO-UNDO. // DEF, VAR and CHAR are abbreviations

DEFINE /* a comment between keywords */ VARIABLE iInline /* and another */
    AS INTEGER NO-UNDO.

FUNCTION lineLabel RETURNS CHARACTER (INPUT ipLine AS INTEGER) FORWARD.

/* *****************************  Comments  *************************** */

/**/
/***/
/* a comment that ends in stars **/
/* level 1 /* level 2 /* level 3 */ back in 2 */ back in 1 */ iCount = 1.
/* a block comment that holds // and is still one block comment */
// a line comment that holds /* and is still one line comment
/* a comment with an apostrophe: don't stop here,
   and a lone " quote over
   three lines */ cShort = "after the comment".
/* a tilde ~ in a comment escapes nothing */

/* ******************************  Strings  *************************** */

cNote = "It's ~"quoted~" twice: ""like this""".
cNote = cNote + ' and ''single'' too'.
cNote = cNote + "/* not a comment */ // not a line comment".
cNote = cNote + "an end marker */ is only text".
cNote = cNote + "http://example.com/path". // a real comment after a URL
cNote = cNote + "tab~tnewline~nends in a tilde~~".
cNote = cNote + "one string
over two lines".
cNote = cNote + "version {&VERSION}":U.
cNote = cNote + SUBSTITUTE("&1 of &2", 1, {&MAX-LINES}).
cTitle = "Total:":R20 + STRING(dTotal, "->>,>>9.99").
end-date = dtStart + 30.

/* ****************************  Main Block  ************************** */

main-block:
DO ON ERROR UNDO, LEAVE:
    RUN fillLines (INPUT {&MAX-LINES}).

    line-loop:
    FOR EACH ttLine NO-LOCK
        WHERE ttLine.qty >= 0
        BY ttLine.lineNo DESCENDING:

        IF ttLine.qty > 1000 THEN
            UNDO, THROW NEW AppError("Too many items", 42).

        IF ttLine.shipped THEN DO:
            /* a DO in an IF in a FOR EACH in a labelled DO */
            IF ttLine.qty > 100 THEN
                LEAVE line-loop.
            ELSE IF ttLine.qty = 0 THEN DO:
                NEXT line-loop.
            END.
        END.
        ELSE DO:
            CASE ttLine.lineNo MODULO 3:
                WHEN 0 THEN
                    cNote = "fizz".
                WHEN 1 OR WHEN 2 THEN DO:
                    retry-loop:
                    REPEAT iAttempt = 1 TO 3:
                        IF iAttempt >= 2 AND NOT do-again THEN
                            LEAVE retry-loop.
                        iInline = iInline + iAttempt.
                    END. /* REPEAT */
                END.
                OTHERWISE
                    cNote = ?.
            END CASE.
        END.

        dTotal = dTotal + ttLine.qty * (1 + dRate).
    END. // FOR EACH

    DO iCount = 1 TO EXTENT(cWords) WHILE do-again:
        IF cWords[iCount] BEGINS "t" OR cWords[iCount] MATCHES "*e" THEN
            cNote = cWords[iCount].
    END.

    CATCH eError AS Error:
        MESSAGE eError:GetMessage(1) VIEW-AS ALERT-BOX ERROR.
    END CATCH.

    FINALLY:
        hBuffer = BUFFER ttLine:HANDLE.
        MESSAGE cTitle SKIP lineLabel(iCount) SKIP hBuffer:NAME
            SKIP SESSION:DATE-FORMAT SKIP end-date SKIP cShort SKIP iBig.
    END FINALLY.
END. /* main-block */

/* ************************  Procedures and functions  **************** */

PROCEDURE fillLines PRIVATE:
    /*------------------------------------------------------------------
      Purpose: Creates ipCount lines. Every second line is shipped.
    ------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE iLine AS INTEGER NO-UNDO.

    DO iLine = 1 TO ipCount TRANSACTION:
        CREATE ttLine.
        ASSIGN
            ttLine.lineNo   = iLine
            ttLine.itemCode = SUBSTITUTE("ITEM-&1", STRING(iLine, "999"))
            ttLine.qty      = iLine * 2.5
            ttLine.shipped  = (iLine MODULO 2 = 0).
    END.
END PROCEDURE.

FUNCTION lineLabel RETURNS CHARACTER (INPUT ipLine AS INTEGER):
    &IF DEFINED(DEBUG) > 0 AND "{&DEBUG}" = "yes" &THEN
    RETURN SUBSTITUTE("line &1 of &2", ipLine, {&MAX-LINES}).
    &ELSEIF {&MAX-LINES} > 10 &THEN
    RETURN "line " + STRING(ipLine, ">>9").
    &ELSE
    RETURN "line " + STRING(ipLine). /* {&DEBUG} is no */
    &ENDIF
END FUNCTION.
