/* A long procedure: enough code that its r-code is much larger than the bytes that vary between compiles. */

DEFINE VARIABLE v00 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v01 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v02 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v03 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v04 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v05 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v06 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v07 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v08 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v09 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v10 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v11 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v12 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v13 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v14 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v15 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v16 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v17 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v18 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v19 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v20 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v21 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v22 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v23 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v24 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v25 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v26 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v27 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v28 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v29 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v30 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v31 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v32 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v33 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v34 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v35 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v36 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v37 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v38 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v39 AS CHARACTER NO-UNDO.
DEFINE VARIABLE acc AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

PROCEDURE step00:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 0:
        piece = piece + STRING(j * 3) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 40 THEN piece = SUBSTRING(piece, 2).
        acc = acc + j * 1.
    END.
    v00 = piece.
    IF acc MODULO 2 = 0 THEN
        PUT UNFORMATTED "step00 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step00 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step01:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 1:
        piece = piece + STRING(j * 4) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 41 THEN piece = SUBSTRING(piece, 3).
        acc = acc + j * 2.
    END.
    v01 = piece.
    IF acc MODULO 3 = 0 THEN
        PUT UNFORMATTED "step01 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step01 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step02:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 2:
        piece = piece + STRING(j * 5) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 42 THEN piece = SUBSTRING(piece, 4).
        acc = acc + j * 3.
    END.
    v02 = piece.
    IF acc MODULO 4 = 0 THEN
        PUT UNFORMATTED "step02 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step02 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step03:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 3:
        piece = piece + STRING(j * 6) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 43 THEN piece = SUBSTRING(piece, 5).
        acc = acc + j * 4.
    END.
    v03 = piece.
    IF acc MODULO 5 = 0 THEN
        PUT UNFORMATTED "step03 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step03 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step04:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 4:
        piece = piece + STRING(j * 7) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 44 THEN piece = SUBSTRING(piece, 6).
        acc = acc + j * 5.
    END.
    v04 = piece.
    IF acc MODULO 6 = 0 THEN
        PUT UNFORMATTED "step04 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step04 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step05:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 5:
        piece = piece + STRING(j * 8) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 45 THEN piece = SUBSTRING(piece, 7).
        acc = acc + j * 6.
    END.
    v05 = piece.
    IF acc MODULO 7 = 0 THEN
        PUT UNFORMATTED "step05 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step05 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step06:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 6:
        piece = piece + STRING(j * 9) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 46 THEN piece = SUBSTRING(piece, 8).
        acc = acc + j * 7.
    END.
    v06 = piece.
    IF acc MODULO 8 = 0 THEN
        PUT UNFORMATTED "step06 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step06 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step07:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 7:
        piece = piece + STRING(j * 10) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 47 THEN piece = SUBSTRING(piece, 9).
        acc = acc + j * 8.
    END.
    v07 = piece.
    IF acc MODULO 9 = 0 THEN
        PUT UNFORMATTED "step07 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step07 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step08:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 8:
        piece = piece + STRING(j * 11) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 48 THEN piece = SUBSTRING(piece, 10).
        acc = acc + j * 9.
    END.
    v08 = piece.
    IF acc MODULO 10 = 0 THEN
        PUT UNFORMATTED "step08 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step08 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step09:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 9:
        piece = piece + STRING(j * 12) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 49 THEN piece = SUBSTRING(piece, 11).
        acc = acc + j * 10.
    END.
    v09 = piece.
    IF acc MODULO 11 = 0 THEN
        PUT UNFORMATTED "step09 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step09 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step10:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 10:
        piece = piece + STRING(j * 13) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 50 THEN piece = SUBSTRING(piece, 12).
        acc = acc + j * 11.
    END.
    v10 = piece.
    IF acc MODULO 12 = 0 THEN
        PUT UNFORMATTED "step10 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step10 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step11:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 11:
        piece = piece + STRING(j * 14) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 51 THEN piece = SUBSTRING(piece, 13).
        acc = acc + j * 12.
    END.
    v11 = piece.
    IF acc MODULO 13 = 0 THEN
        PUT UNFORMATTED "step11 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step11 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step12:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 12:
        piece = piece + STRING(j * 15) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 52 THEN piece = SUBSTRING(piece, 14).
        acc = acc + j * 13.
    END.
    v12 = piece.
    IF acc MODULO 14 = 0 THEN
        PUT UNFORMATTED "step12 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step12 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step13:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 13:
        piece = piece + STRING(j * 16) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 53 THEN piece = SUBSTRING(piece, 15).
        acc = acc + j * 14.
    END.
    v13 = piece.
    IF acc MODULO 15 = 0 THEN
        PUT UNFORMATTED "step13 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step13 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step14:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 14:
        piece = piece + STRING(j * 17) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 54 THEN piece = SUBSTRING(piece, 16).
        acc = acc + j * 15.
    END.
    v14 = piece.
    IF acc MODULO 16 = 0 THEN
        PUT UNFORMATTED "step14 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step14 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step15:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 15:
        piece = piece + STRING(j * 18) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 55 THEN piece = SUBSTRING(piece, 17).
        acc = acc + j * 16.
    END.
    v15 = piece.
    IF acc MODULO 17 = 0 THEN
        PUT UNFORMATTED "step15 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step15 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step16:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 16:
        piece = piece + STRING(j * 19) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 56 THEN piece = SUBSTRING(piece, 18).
        acc = acc + j * 17.
    END.
    v16 = piece.
    IF acc MODULO 18 = 0 THEN
        PUT UNFORMATTED "step16 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step16 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step17:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 17:
        piece = piece + STRING(j * 20) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 57 THEN piece = SUBSTRING(piece, 19).
        acc = acc + j * 18.
    END.
    v17 = piece.
    IF acc MODULO 19 = 0 THEN
        PUT UNFORMATTED "step17 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step17 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step18:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 18:
        piece = piece + STRING(j * 21) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 58 THEN piece = SUBSTRING(piece, 20).
        acc = acc + j * 19.
    END.
    v18 = piece.
    IF acc MODULO 20 = 0 THEN
        PUT UNFORMATTED "step18 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step18 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step19:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 19:
        piece = piece + STRING(j * 22) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 59 THEN piece = SUBSTRING(piece, 21).
        acc = acc + j * 20.
    END.
    v19 = piece.
    IF acc MODULO 21 = 0 THEN
        PUT UNFORMATTED "step19 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step19 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step20:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 20:
        piece = piece + STRING(j * 23) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 60 THEN piece = SUBSTRING(piece, 22).
        acc = acc + j * 21.
    END.
    v20 = piece.
    IF acc MODULO 22 = 0 THEN
        PUT UNFORMATTED "step20 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step20 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step21:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 21:
        piece = piece + STRING(j * 24) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 61 THEN piece = SUBSTRING(piece, 23).
        acc = acc + j * 22.
    END.
    v21 = piece.
    IF acc MODULO 23 = 0 THEN
        PUT UNFORMATTED "step21 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step21 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step22:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 22:
        piece = piece + STRING(j * 25) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 62 THEN piece = SUBSTRING(piece, 24).
        acc = acc + j * 23.
    END.
    v22 = piece.
    IF acc MODULO 24 = 0 THEN
        PUT UNFORMATTED "step22 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step22 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step23:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 23:
        piece = piece + STRING(j * 26) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 63 THEN piece = SUBSTRING(piece, 25).
        acc = acc + j * 24.
    END.
    v23 = piece.
    IF acc MODULO 25 = 0 THEN
        PUT UNFORMATTED "step23 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step23 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step24:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 24:
        piece = piece + STRING(j * 27) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 64 THEN piece = SUBSTRING(piece, 26).
        acc = acc + j * 25.
    END.
    v24 = piece.
    IF acc MODULO 26 = 0 THEN
        PUT UNFORMATTED "step24 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step24 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step25:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 25:
        piece = piece + STRING(j * 28) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 65 THEN piece = SUBSTRING(piece, 27).
        acc = acc + j * 26.
    END.
    v25 = piece.
    IF acc MODULO 27 = 0 THEN
        PUT UNFORMATTED "step25 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step25 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step26:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 26:
        piece = piece + STRING(j * 29) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 66 THEN piece = SUBSTRING(piece, 28).
        acc = acc + j * 27.
    END.
    v26 = piece.
    IF acc MODULO 28 = 0 THEN
        PUT UNFORMATTED "step26 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step26 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step27:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 27:
        piece = piece + STRING(j * 30) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 67 THEN piece = SUBSTRING(piece, 29).
        acc = acc + j * 28.
    END.
    v27 = piece.
    IF acc MODULO 29 = 0 THEN
        PUT UNFORMATTED "step27 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step27 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step28:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 28:
        piece = piece + STRING(j * 31) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 68 THEN piece = SUBSTRING(piece, 30).
        acc = acc + j * 29.
    END.
    v28 = piece.
    IF acc MODULO 30 = 0 THEN
        PUT UNFORMATTED "step28 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step28 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step29:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 29:
        piece = piece + STRING(j * 32) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 69 THEN piece = SUBSTRING(piece, 31).
        acc = acc + j * 30.
    END.
    v29 = piece.
    IF acc MODULO 31 = 0 THEN
        PUT UNFORMATTED "step29 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step29 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step30:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 30:
        piece = piece + STRING(j * 33) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 70 THEN piece = SUBSTRING(piece, 32).
        acc = acc + j * 31.
    END.
    v30 = piece.
    IF acc MODULO 32 = 0 THEN
        PUT UNFORMATTED "step30 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step30 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step31:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 31:
        piece = piece + STRING(j * 34) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 71 THEN piece = SUBSTRING(piece, 33).
        acc = acc + j * 32.
    END.
    v31 = piece.
    IF acc MODULO 33 = 0 THEN
        PUT UNFORMATTED "step31 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step31 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step32:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 32:
        piece = piece + STRING(j * 35) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 72 THEN piece = SUBSTRING(piece, 34).
        acc = acc + j * 33.
    END.
    v32 = piece.
    IF acc MODULO 34 = 0 THEN
        PUT UNFORMATTED "step32 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step32 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step33:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 33:
        piece = piece + STRING(j * 36) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 73 THEN piece = SUBSTRING(piece, 35).
        acc = acc + j * 34.
    END.
    v33 = piece.
    IF acc MODULO 35 = 0 THEN
        PUT UNFORMATTED "step33 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step33 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step34:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 34:
        piece = piece + STRING(j * 37) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 74 THEN piece = SUBSTRING(piece, 36).
        acc = acc + j * 35.
    END.
    v34 = piece.
    IF acc MODULO 36 = 0 THEN
        PUT UNFORMATTED "step34 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step34 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step35:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 35:
        piece = piece + STRING(j * 38) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 75 THEN piece = SUBSTRING(piece, 37).
        acc = acc + j * 36.
    END.
    v35 = piece.
    IF acc MODULO 37 = 0 THEN
        PUT UNFORMATTED "step35 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step35 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step36:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 36:
        piece = piece + STRING(j * 39) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 76 THEN piece = SUBSTRING(piece, 38).
        acc = acc + j * 37.
    END.
    v36 = piece.
    IF acc MODULO 38 = 0 THEN
        PUT UNFORMATTED "step36 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step36 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step37:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 37:
        piece = piece + STRING(j * 40) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 77 THEN piece = SUBSTRING(piece, 39).
        acc = acc + j * 38.
    END.
    v37 = piece.
    IF acc MODULO 39 = 0 THEN
        PUT UNFORMATTED "step37 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step37 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step38:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 38:
        piece = piece + STRING(j * 41) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 78 THEN piece = SUBSTRING(piece, 40).
        acc = acc + j * 39.
    END.
    v38 = piece.
    IF acc MODULO 40 = 0 THEN
        PUT UNFORMATTED "step38 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step38 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step39:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 39:
        piece = piece + STRING(j * 42) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 79 THEN piece = SUBSTRING(piece, 41).
        acc = acc + j * 40.
    END.
    v39 = piece.
    IF acc MODULO 41 = 0 THEN
        PUT UNFORMATTED "step39 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step39 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step40:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 40:
        piece = piece + STRING(j * 43) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 80 THEN piece = SUBSTRING(piece, 42).
        acc = acc + j * 41.
    END.
    v00 = piece.
    IF acc MODULO 42 = 0 THEN
        PUT UNFORMATTED "step40 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step40 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step41:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 41:
        piece = piece + STRING(j * 44) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 81 THEN piece = SUBSTRING(piece, 43).
        acc = acc + j * 42.
    END.
    v01 = piece.
    IF acc MODULO 43 = 0 THEN
        PUT UNFORMATTED "step41 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step41 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step42:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 42:
        piece = piece + STRING(j * 45) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 82 THEN piece = SUBSTRING(piece, 44).
        acc = acc + j * 43.
    END.
    v02 = piece.
    IF acc MODULO 44 = 0 THEN
        PUT UNFORMATTED "step42 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step42 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step43:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 43:
        piece = piece + STRING(j * 46) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 83 THEN piece = SUBSTRING(piece, 45).
        acc = acc + j * 44.
    END.
    v03 = piece.
    IF acc MODULO 45 = 0 THEN
        PUT UNFORMATTED "step43 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step43 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step44:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 44:
        piece = piece + STRING(j * 47) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 84 THEN piece = SUBSTRING(piece, 46).
        acc = acc + j * 45.
    END.
    v04 = piece.
    IF acc MODULO 46 = 0 THEN
        PUT UNFORMATTED "step44 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step44 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step45:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 45:
        piece = piece + STRING(j * 48) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 85 THEN piece = SUBSTRING(piece, 47).
        acc = acc + j * 46.
    END.
    v05 = piece.
    IF acc MODULO 47 = 0 THEN
        PUT UNFORMATTED "step45 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step45 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step46:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 46:
        piece = piece + STRING(j * 49) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 86 THEN piece = SUBSTRING(piece, 48).
        acc = acc + j * 47.
    END.
    v06 = piece.
    IF acc MODULO 48 = 0 THEN
        PUT UNFORMATTED "step46 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step46 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step47:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 47:
        piece = piece + STRING(j * 50) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 87 THEN piece = SUBSTRING(piece, 49).
        acc = acc + j * 48.
    END.
    v07 = piece.
    IF acc MODULO 49 = 0 THEN
        PUT UNFORMATTED "step47 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step47 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step48:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 48:
        piece = piece + STRING(j * 51) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 88 THEN piece = SUBSTRING(piece, 50).
        acc = acc + j * 49.
    END.
    v08 = piece.
    IF acc MODULO 50 = 0 THEN
        PUT UNFORMATTED "step48 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step48 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step49:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 49:
        piece = piece + STRING(j * 52) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 89 THEN piece = SUBSTRING(piece, 51).
        acc = acc + j * 50.
    END.
    v09 = piece.
    IF acc MODULO 51 = 0 THEN
        PUT UNFORMATTED "step49 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step49 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step50:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 50:
        piece = piece + STRING(j * 53) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 90 THEN piece = SUBSTRING(piece, 52).
        acc = acc + j * 51.
    END.
    v10 = piece.
    IF acc MODULO 52 = 0 THEN
        PUT UNFORMATTED "step50 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step50 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step51:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 51:
        piece = piece + STRING(j * 54) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 91 THEN piece = SUBSTRING(piece, 53).
        acc = acc + j * 52.
    END.
    v11 = piece.
    IF acc MODULO 53 = 0 THEN
        PUT UNFORMATTED "step51 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step51 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step52:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 52:
        piece = piece + STRING(j * 55) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 92 THEN piece = SUBSTRING(piece, 54).
        acc = acc + j * 53.
    END.
    v12 = piece.
    IF acc MODULO 54 = 0 THEN
        PUT UNFORMATTED "step52 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step52 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step53:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 53:
        piece = piece + STRING(j * 56) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 93 THEN piece = SUBSTRING(piece, 55).
        acc = acc + j * 54.
    END.
    v13 = piece.
    IF acc MODULO 55 = 0 THEN
        PUT UNFORMATTED "step53 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step53 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step54:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 54:
        piece = piece + STRING(j * 57) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 94 THEN piece = SUBSTRING(piece, 56).
        acc = acc + j * 55.
    END.
    v14 = piece.
    IF acc MODULO 56 = 0 THEN
        PUT UNFORMATTED "step54 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step54 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step55:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 55:
        piece = piece + STRING(j * 58) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 95 THEN piece = SUBSTRING(piece, 57).
        acc = acc + j * 56.
    END.
    v15 = piece.
    IF acc MODULO 57 = 0 THEN
        PUT UNFORMATTED "step55 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step55 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step56:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 56:
        piece = piece + STRING(j * 59) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 96 THEN piece = SUBSTRING(piece, 58).
        acc = acc + j * 57.
    END.
    v16 = piece.
    IF acc MODULO 58 = 0 THEN
        PUT UNFORMATTED "step56 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step56 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step57:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 57:
        piece = piece + STRING(j * 60) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 97 THEN piece = SUBSTRING(piece, 59).
        acc = acc + j * 58.
    END.
    v17 = piece.
    IF acc MODULO 59 = 0 THEN
        PUT UNFORMATTED "step57 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step57 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step58:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 58:
        piece = piece + STRING(j * 61) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 98 THEN piece = SUBSTRING(piece, 60).
        acc = acc + j * 59.
    END.
    v18 = piece.
    IF acc MODULO 60 = 0 THEN
        PUT UNFORMATTED "step58 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step58 odd " acc SKIP.
END PROCEDURE.

PROCEDURE step59:
    DEFINE INPUT PARAMETER amount AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE piece AS CHARACTER NO-UNDO.
    DO j = 1 TO amount + 59:
        piece = piece + STRING(j * 62) + "-" + SUBSTRING("abcdefghij", (j MODULO 10) + 1, 1).
        IF LENGTH(piece) > 99 THEN piece = SUBSTRING(piece, 61).
        acc = acc + j * 60.
    END.
    v19 = piece.
    IF acc MODULO 61 = 0 THEN
        PUT UNFORMATTED "step59 even " acc SKIP.
    ELSE
        PUT UNFORMATTED "step59 odd " acc SKIP.
END PROCEDURE.

DO i = 1 TO 3:
    RUN step00 (INPUT i).
    RUN step01 (INPUT i).
    RUN step02 (INPUT i).
    RUN step03 (INPUT i).
    RUN step04 (INPUT i).
    RUN step05 (INPUT i).
    RUN step06 (INPUT i).
    RUN step07 (INPUT i).
    RUN step08 (INPUT i).
    RUN step09 (INPUT i).
    RUN step10 (INPUT i).
    RUN step11 (INPUT i).
    RUN step12 (INPUT i).
    RUN step13 (INPUT i).
    RUN step14 (INPUT i).
    RUN step15 (INPUT i).
    RUN step16 (INPUT i).
    RUN step17 (INPUT i).
    RUN step18 (INPUT i).
    RUN step19 (INPUT i).
    RUN step20 (INPUT i).
    RUN step21 (INPUT i).
    RUN step22 (INPUT i).
    RUN step23 (INPUT i).
    RUN step24 (INPUT i).
    RUN step25 (INPUT i).
    RUN step26 (INPUT i).
    RUN step27 (INPUT i).
    RUN step28 (INPUT i).
    RUN step29 (INPUT i).
    RUN step30 (INPUT i).
    RUN step31 (INPUT i).
    RUN step32 (INPUT i).
    RUN step33 (INPUT i).
    RUN step34 (INPUT i).
    RUN step35 (INPUT i).
    RUN step36 (INPUT i).
    RUN step37 (INPUT i).
    RUN step38 (INPUT i).
    RUN step39 (INPUT i).
    RUN step40 (INPUT i).
    RUN step41 (INPUT i).
    RUN step42 (INPUT i).
    RUN step43 (INPUT i).
    RUN step44 (INPUT i).
    RUN step45 (INPUT i).
    RUN step46 (INPUT i).
    RUN step47 (INPUT i).
    RUN step48 (INPUT i).
    RUN step49 (INPUT i).
    RUN step50 (INPUT i).
    RUN step51 (INPUT i).
    RUN step52 (INPUT i).
    RUN step53 (INPUT i).
    RUN step54 (INPUT i).
    RUN step55 (INPUT i).
    RUN step56 (INPUT i).
    RUN step57 (INPUT i).
    RUN step58 (INPUT i).
    RUN step59 (INPUT i).
END.
