*
FUNCTION DECIMALS
 LPARAMETERS n_Number
 LOCAL c_Number
 LOCAL n_Number, n_Decimals, n_Significant_digits, n_Max_decimals
 LOCAL i
 IF TYPE("N_NUMBER")="N"
      n_Number = ABS(n_Number)
      FOR i = 1 TO 12
           IF n_Number<10^i
                n_Significant_digits = i
                EXIT
           ELSE
                IF i=12
                     n_Significant_digits = 13
                ENDIF
           ENDIF
      ENDFOR
      n_Max_decimals = MIN(15, (15-n_Significant_digits))
      c_Number = STR(n_Number, 50, n_Max_decimals)
      IF (VAL(LTRIM(c_Number))<>n_Number .OR. n_Number>8789999999999)
           n_Decimals = -1
      ELSE
           FOR i = 50 TO (50-n_Max_decimals) STEP -1
                IF NOT (SUBSTR(c_Number, i, 1)=="0")
                     n_Decimals = i-(50-n_Max_decimals)
                     EXIT
                ENDIF
           ENDFOR
      ENDIF
 ELSE
      IF TYPE("N_NUMBER")="Y"
           n_Decimals = 4
      ELSE
           n_Decimals = -1
      ENDIF
 ENDIF
 RETURN (n_Decimals)
ENDFUNC
*
