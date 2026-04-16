/* Pipeline include B: shared helpers; references include A's defines. */

&scoped-define INCLUDE-B-LOADED yes

&if defined(INCLUDE-A-LOADED) &then
  define variable lv-b-after-a as logical no-undo initial true.
&else
  define variable lv-b-after-a as logical no-undo initial false.
&endif

define variable lv-b-prefix as character no-undo initial {&BATCH-PREFIX}.
define variable lv-b-suffix as character no-undo initial {&BATCH-SUFFIX}.

procedure log-event:
    define input parameter ip-message as character no-undo.
    message ip-message.
end procedure.
