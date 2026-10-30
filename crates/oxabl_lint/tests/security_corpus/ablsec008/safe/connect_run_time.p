DEFINE VARIABLE cPw AS CHARACTER NO-UNDO.

CONNECT -db sales -H dbhost -S 20000 -U bob -P VALUE(cPw).
CONNECT VALUE("-db audit -U bob -P " + cPw).
CONNECT VALUE(cParams).
