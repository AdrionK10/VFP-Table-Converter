*
FUNCTION ISARRAY
 PARAMETER c_Var_name
 PRIVATE c_Var_name
 PRIVATE l_Result
 l_Result = .T.
 IF (PARAMETERS()=1 .AND. TYPE("C_VAR_NAME")="C" .AND. NOT EMPTY(c_Var_name))
      IF TYPE(c_Var_name+"(1)")="U"
           l_Result = .F.
      ENDIF
 ELSE
      l_Result = .F.
 ENDIF
 RETURN (l_Result)
ENDFUNC
*
