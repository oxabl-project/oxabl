/* Pipeline include A: medium include that itself pulls in another. */

&scoped-define INCLUDE-A-LOADED yes
&scoped-define BATCH-PREFIX "BATCH-"
&scoped-define BATCH-SUFFIX "-END"

{bench_preprocessor_pipeline_nested.i}

define variable lv-batch-prefix as character no-undo initial {&BATCH-PREFIX}.
define variable lv-batch-suffix as character no-undo initial {&BATCH-SUFFIX}.

&if defined(USE-DEBUG) &then
  define variable lv-a-debug as logical no-undo initial true.
&endif
