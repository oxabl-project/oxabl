/* expect: 3 */
DEFINE BUFFER bUser FOR _User.

CREATE bUser.
bUser._Userid = cName.
DELETE bUser.
