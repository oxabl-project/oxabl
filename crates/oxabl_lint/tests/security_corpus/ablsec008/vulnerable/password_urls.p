/* expect: 2 */
/* URLs that carry a password. */
DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
cPassword = "ftp://backup:s3cret@files.example.com/".
cPassword = "https://example.com/api?pwd=s3cret".
