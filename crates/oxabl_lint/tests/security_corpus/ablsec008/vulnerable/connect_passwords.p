/* expect: 3 */
CONNECT -db sales -H dbhost -S 20000 -U bob -P "s3cret".
CONNECT -db audit -U bob -P s3cret2.
CONNECT VALUE("-db hr -U bob -P s3cret3").
