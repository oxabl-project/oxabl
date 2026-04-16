/* Pipeline include nested: deepest level of the include chain. */

&scoped-define NESTED-LOADED yes
&scoped-define NESTED-COUNTER 0
&scoped-define NESTED-NAME "deep-include"

define variable lv-nested-name as character no-undo initial {&NESTED-NAME}.

&if {&NESTED-COUNTER} = 0 &then
  define variable lv-nested-zero as logical no-undo initial true.
&else
  define variable lv-nested-zero as logical no-undo initial false.
&endif
