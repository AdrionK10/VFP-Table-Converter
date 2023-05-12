***
*** is_free_table.fxp
***
*
*
FUNCTION IS_FREE_TABLE
 LPARAMETERS c_Filename, l_Logical
 LOCAL n_Params, n_Error, n_Databases, n_Dbcs_now, n_Matches
 LOCAL c_Result
 LOCAL prEv_error, prEv_select, prEv_dbc
 LOCAL i
 PRIVATE a_Databases, a_Dbcs_now
 DIMENSION a_Databases(1, 2), a_Dbcs_now(1, 2)
 n_Params = PARAMETERS()
 c_Result = ""
 l_Logical = IIF((TYPE("L_LOGICAL")="L"), l_Logical, .F.)
 IF NOT (n_Params<1 .OR. NOT (TYPE("C_FILENAME")=="C") .OR. EMPTY(c_Filename))
      a_Databases = ""
      a_Dbcs_now = ""
      n_Databases = ADATABASES(a_Databases)
      c_Filename = ALLTRIM(UPPER(c_Filename))
      n_Error = 0
      prEv_error = ON("ERROR")
      ON ERROR N_ERROR = ERROR()
      USE SHARED (c_Filename) AGAIN ALIAS "Is_Free_Table_Check" IN 0
      ON ERROR  &PREV_ERROR
      IF n_Error=0
           prEv_select = SELECT(0)
           SELECT "Is_Free_Table_Check"
           c_Result = CURSORGETPROP("Database")
           USE
           SELECT (prEv_select)
           n_Dbcs_now = ADATABASES(a_Dbcs_now)
           IF (NOT EMPTY(c_Result) .AND. (n_Dbcs_now>n_Databases))
                prEv_dbc = DBC()
                FOR i = 1 TO n_Dbcs_now
                     n_Matches = a_Search("A_DATABASES",a_Dbcs_now(i,1), ;
                                 "COLUMN",1)
                     IF n_Matches=0
                          SET DATABASE TO (a_Dbcs_now(i,1))
                          CLOSE DATABASES
                          IF NOT EMPTY(prEv_dbc)
                               SET DATABASE TO (prEv_dbc)
                          ELSE
                               SET DATABASE TO
                          ENDIF
                          EXIT
                     ENDIF
                ENDFOR
           ENDIF
      ELSE
           c_Result = "****"
      ENDIF
 ELSE
      c_Result = "****"
 ENDIF
 RETURN (IIF((l_Logical=.F.), c_Result, IIF(EMPTY(c_Result), .T., .F.)))
ENDFUNC
*
