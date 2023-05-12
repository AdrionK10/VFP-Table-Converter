*
FUNCTION GET_LINE
 LPARAMETERS c_String, n_Line_len, n_Whchline, l_Chkparam, l_For_loop
 LOCAL n_Params, n_Lines
 LOCAL l_Proceed
 LOCAL prEv_memow
 n_Params = PARAMETERS()
 l_Chkparam = IIF((n_Params<4 .OR. TYPE("L_CHKPARAM")<>"L"), .T., l_Chkparam)
 l_For_loop = IIF((TYPE("L_FOR_LOOP")<>"L"), .F., l_For_loop)
 IF l_Chkparam=.T.
      l_Proceed = IIF((TYPE("C_STRING")<>"C"), .F., .T.)
      l_Proceed = IIF(is_intgr(n_Line_len,8,1024), l_Proceed, .F.)
      l_Proceed = IIF(is_intgr(n_Whchline,1), l_Proceed, .F.)
 ELSE
      l_Proceed = .T.
 ENDIF
 IF l_Proceed=.T.
      prEv_memow = SET("MEMOWIDTH")
      SET MEMOWIDTH TO n_Line_len
      n_Lines = MEMLINES(c_String)
      c_String = IIF((n_Whchline>n_Lines), IIF((l_For_loop=.T.), CHR(255),  ;
                 ""), MLINE(c_String, n_Whchline))
      SET MEMOWIDTH TO prEv_memow
 ELSE
      c_String = ""
 ENDIF
 RETURN (c_String)
ENDFUNC
*
