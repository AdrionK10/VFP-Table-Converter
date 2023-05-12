*
FUNCTION FILLED
 PARAMETER c_String, c_Fillchrs, l_Details
 PRIVATE c_String, c_Fillchrs
 PRIVATE n_Occrnces
 PRIVATE l_Details, l_Result
 l_Details = IIF((TYPE("L_DETAILS")="L"), l_Details, .F.)
 IF (TYPE("C_STRING")="C" .AND. TYPE("C_FILLCHRS")="C" .AND. NOT  ;
    EMPTY(c_String))
      n_Occrnces = OCCURS(c_Fillchrs, c_String)
      l_Result = IIF(((n_Occrnces*LEN(c_Fillchrs))=LEN(c_String)), .T., .F.)
      l_Result = IIF((l_Details=.T.), IIF((l_Result=.T.), n_Occrnces, 0),  ;
                 l_Result)
 ELSE
      l_Result = IIF((l_Details=.T.), 0, .F.)
 ENDIF
 RETURN (l_Result)
ENDFUNC
*
