&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wMain
&Scoped-define FRAME-NAME fMain

/* Original sample window for the grammar snapshot tests. */

CREATE WIDGET-POOL.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMain
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipTitle AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */
&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(30)":U
     LABEL "Name"
     VIEW-AS FILL-IN
     SIZE 30 BY 1 NO-UNDO.

DEFINE BUTTON btnOk
     LABEL "OK"
     SIZE 15 BY 1.14.

DEFINE FRAME fMain
     fiName AT ROW 2 COL 12 COLON-ALIGNED
     btnOk AT ROW 4 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 6 .
&ANALYZE-RESUME

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMain ASSIGN
         HIDDEN             = YES
         TITLE              = "Sample"
         HEIGHT             = 6
         WIDTH              = 80
         VISIBLE            = NO.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk wMain
ON CHOOSE OF btnOk IN FRAME fMain /* OK */
DO:
  cName = fiName:SCREEN-VALUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wMain
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Sets the title.
------------------------------------------------------------------------------*/
  {&WINDOW-NAME}:TITLE = ipTitle.
  RUN SUPER.
END PROCEDURE.
&ANALYZE-RESUME
