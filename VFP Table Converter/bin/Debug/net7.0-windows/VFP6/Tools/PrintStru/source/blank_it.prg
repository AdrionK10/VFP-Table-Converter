*
FUNCTION BLANK_IT
 LPARAMETERS x_Var, l_Space_fill
 LOCAL n_Params
 n_Params = PARAMETERS()
 IF n_Params>0
      DO CASE
           CASE TYPE("X_VAR")="C"
                l_Space_fill = IIF((n_Params>1 .AND. TYPE("L_SPACE_FILL")= ;
                               "L"), l_Space_fill, .F.)
                x_Var = IIF((l_Space_fill=.T.), SPACE(LEN(x_Var)), "")
           CASE (TYPE("X_VAR")="N" .OR. TYPE("X_VAR")="F")
                x_Var = 0
           CASE TYPE("X_VAR")="D"
                x_Var = {}
           CASE TYPE("X_VAR")="T"
                x_Var = CTOT("")
           CASE TYPE("X_VAR")="M"
                x_Var = ""
           CASE TYPE("X_VAR")="Y"
                x_Var = $0.0000
           CASE TYPE("X_VAR")="L"
                x_Var = .F.
      ENDCASE
 ENDIF
 RETURN (x_Var)
ENDFUNC
*
