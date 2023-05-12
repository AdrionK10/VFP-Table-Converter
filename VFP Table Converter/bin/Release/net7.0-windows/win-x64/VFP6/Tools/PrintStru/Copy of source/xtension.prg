*
FUNCTION XTENSION
 PARAMETER c_Filename, l_Period
 PRIVATE c_Filename, c_Xtension
 PRIVATE l_Period
 PRIVATE n_Period
 c_Filename = IIF((TYPE("C_FILENAME")<>"C"), "", UPPER(ALLTRIM(c_Filename)))
 l_Period = IIF((TYPE("L_PERIOD")<>"L"), .F., l_Period)
 c_Xtension = ""
 IF NOT EMPTY(c_Filename)
      n_Period = RAT(".", c_Filename)
      IF (n_Period>0 .AND. n_Period=MAX(n_Period, RAT("\", c_Filename),  ;
         RAT(":", c_Filename)))
           c_Xtension = IIF((l_Period=.T.), SUBSTR(c_Filename, n_Period),  ;
                        SUBSTR(c_Filename, (n_Period+1)))
           c_Xtension = IIF((c_Xtension=="."), "", c_Xtension)
      ENDIF
 ENDIF
 RETURN (c_Xtension)
ENDFUNC
*
