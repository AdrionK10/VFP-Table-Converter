*
FUNCTION TO_CHAR
 PARAMETER x_Value, n_Out_len, n_Decimals, c_Justify, c_Date_fmt
 PRIVATE c_Justify, c_Result, c_Date_fmt
 PRIVATE n_Out_len, n_Decimals, n_Params, n_Temp
 PRIVATE x_Value
 PRIVATE prEv_exact, prEv_date, prEv_error
 n_Params = PARAMETERS()
 c_Result = ""
 prEv_exact = SET("EXACT")
 prEv_date = SET("DATE")
 SET EXACT ON
 IF n_Params>0
      DO CASE
           CASE n_Params=1
                n_Out_len = 0
                n_Decimals = deCimals(x_Value)
                c_Justify = "C"
                c_Date_fmt = ""
           CASE n_Params=2
                n_Out_len = IIF((TYPE("N_OUT_LEN")<>"N"), 0, n_Out_len)
                n_Out_len = IIF((n_Out_len<0 .OR. n_Out_len<> ;
                            INT(n_Out_len)), 0, n_Out_len)
                n_Decimals = deCimals(x_Value)
                c_Justify = "C"
                c_Date_fmt = ""
           CASE n_Params=3
                n_Out_len = IIF((TYPE("N_OUT_LEN")<>"N"), 0, n_Out_len)
                n_Out_len = IIF((n_Out_len<0 .OR. n_Out_len<> ;
                            INT(n_Out_len)), 0, n_Out_len)
                n_Decimals = IIF((TYPE("N_DECIMALS")<>"N"), 0, n_Decimals)
                n_Decimals = IIF((n_Decimals<0 .OR. n_Decimals<> ;
                             INT(n_Decimals) .OR. n_Decimals>18),  ;
                             deCimals(x_Value), n_Decimals)
                c_Justify = "C"
                c_Date_fmt = ""
           CASE n_Params=4
                n_Out_len = IIF((TYPE("N_OUT_LEN")<>"N"), 0, n_Out_len)
                n_Out_len = IIF((n_Out_len<0 .OR. n_Out_len<> ;
                            INT(n_Out_len)), 0, n_Out_len)
                n_Decimals = IIF((TYPE("N_DECIMALS")<>"N"), 0, n_Decimals)
                n_Decimals = IIF((n_Decimals<0 .OR. n_Decimals<> ;
                             INT(n_Decimals) .OR. n_Decimals>18),  ;
                             deCimals(x_Value), n_Decimals)
                c_Justify = IIF((TYPE("C_JUSTIFY")<>"C"), "C",  ;
                            UPPER(ALLTRIM(c_Justify)))
                c_Justify = IIF(INLIST(c_Justify, "C", "R", "L"),  ;
                            c_Justify, "C")
                c_Date_fmt = ""
           CASE n_Params=5
                n_Out_len = IIF((TYPE("N_OUT_LEN")<>"N"), 0, n_Out_len)
                n_Out_len = IIF((n_Out_len<0 .OR. n_Out_len<> ;
                            INT(n_Out_len)), 0, n_Out_len)
                n_Decimals = IIF((TYPE("N_DECIMALS")<>"N"), 0, n_Decimals)
                n_Decimals = IIF((n_Decimals<0 .OR. n_Decimals<> ;
                             INT(n_Decimals) .OR. n_Decimals>18),  ;
                             deCimals(x_Value), n_Decimals)
                c_Justify = IIF((TYPE("C_JUSTIFY")<>"C"), "C",  ;
                            UPPER(ALLTRIM(c_Justify)))
                c_Justify = IIF(INLIST(c_Justify, "C", "R", "L"),  ;
                            c_Justify, "C")
                c_Date_fmt = IIF((TYPE("C_DATE_FMT")<>"C"), "", c_Date_fmt)
      ENDCASE
      DO CASE
           CASE TYPE("X_VALUE")="N"
                n_Temp = LEN(LTRIM(STR(x_Value)))
                c_Result = LTRIM(STR(x_Value, (n_Temp+1+n_Decimals),  ;
                           n_Decimals))
           CASE TYPE("X_VALUE")="Y"
                n_Temp = LEN(LTRIM(STR(x_Value)))
                c_Result = SET("CURRENCY", 1)+LTRIM(STR(MTON(x_Value),  ;
                           (n_Temp+1+n_Decimals), n_Decimals))
           CASE TYPE("X_VALUE")="L"
                c_Result = IIF((x_Value=.T.), ".T.", ".F.")
           CASE TYPE("X_VALUE")="C"
                c_Result = x_Value
           CASE TYPE("X_VALUE")="D"
                IF NOT EMPTY(c_Date_fmt)
                     prEv_error = ON("ERROR")
                     ON ERROR *
                     SET DATE TO (c_Date_fmt)
                ENDIF
                c_Result = DTOC(x_Value)
                IF NOT EMPTY(c_Date_fmt)
                     ON ERROR  &PREV_ERROR
                     SET DATE TO (prEv_date)
                ENDIF
           CASE TYPE("X_VALUE")="T"
                IF NOT EMPTY(c_Date_fmt)
                     prEv_error = ON("ERROR")
                     ON ERROR *
                     SET DATE TO (c_Date_fmt)
                ENDIF
                c_Result = TTOC(x_Value)
                IF NOT EMPTY(c_Date_fmt)
                     ON ERROR  &PREV_ERROR
                     SET DATE TO (prEv_date)
                ENDIF
      ENDCASE
 ENDIF
 IF (n_Params>0 .AND. n_Out_len<>0)
      DO CASE
           CASE c_Justify="L"
                c_Result = PADR(c_Result, n_Out_len)
           CASE c_Justify="R"
                c_Result = PADL(c_Result, n_Out_len)
           OTHERWISE
                c_Result = PADC(c_Result, n_Out_len)
      ENDCASE
 ENDIF
 SET EXACT	 &PREV_EXACT
 RETURN (c_Result)
ENDFUNC
*
