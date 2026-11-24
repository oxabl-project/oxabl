/* expect: 4 */
CREATE _User.
ASSIGN _User._Userid = "bob" _User._Password = ENCODE("x").
DELETE _User.
