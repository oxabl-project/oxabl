/* expect: 1 */
PROCEDURE parse:
    IF hDoc:LOAD("file", cPath, FALSE) THEN MESSAGE "ok".
END PROCEDURE.
