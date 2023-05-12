***
*** a_tables_in_dbc.fxp
***
*
*
FUNCTION A_TABLES_IN_DBC
 PARAMETER c_Database, a_Tables, l_Proper
 LOCAL c_Temp_dir, c_Temp_file, c_Text_line
 LOCAL n_Params, n_Tables, n_Handle, n_First_space
 LOCAL l_Continue, l_Already_open, l_Already_checked
 LOCAL prEv_error, prEv_dbc
 PRIVATE c_Database
 PRIVATE l_Proper
 PRIVATE a_Tables
 n_Params = PARAMETERS()
 IF (n_Params>1 .AND. TYPE("C_DATABASE")=="C" .AND. isArray("A_TABLES")  ;
    .AND. NOT EMPTY(c_Database))
      prEv_error = ON("ERROR")
      prEv_dbc = DBC()
      l_Proper = IIF((TYPE("L_PROPER")=="L"), l_Proper, .F.)
      l_Continue = .T.
      IF NOT DBUSED(c_Database)
           ON ERROR L_CONTINUE = .F.
           OPEN DATABASE (c_Database) EXCLUSIVE NOUPDATE
           ON ERROR  &PREV_ERROR
           l_Already_open = .F.
      ELSE
           SET DATABASE TO (c_Database)
           l_Already_open = .T.
      ENDIF
      IF l_Continue=.T.
           c_Temp_dir = teMp_files_dir(.T.)
           c_Temp_file = c_Temp_dir+"_LSM_DBTABLES_"+unIque_name(.T.)
           DELETE FILE (c_Temp_file)
           LIST TABLE TO FILE (c_Temp_file) NOCONSOLE
           IF l_Already_open=.F.
                CLOSE DATABASES
           ENDIF
           IF NOT EMPTY(prEv_dbc)
                SET DATABASE TO (prEv_dbc)
           ENDIF
           c_Temp_file = c_Temp_file+".TXT"
           n_Handle = FOPEN(c_Temp_file, 0)
           IF n_Handle>0
                l_Already_checked = .F.
                n_Tables = 0
                DO WHILE NOT FEOF(n_Handle)
                     c_Text_line = UPPER(FGETS(n_Handle, 8192))
                     IF NOT EMPTY(c_Text_line)
                          IF l_Already_checked=.F.
                               IF (("NAME"$c_Text_line) .AND. ("SOURCE"$ ;
                                  c_Text_line))
                                    l_Already_checked = .T.
                               ENDIF
                          ELSE
                               n_Tables = n_Tables+1
                               c_Text_line = reDuce(ALLTRIM(c_Text_line))
                               n_First_space = AT(" ", c_Text_line)
                               DIMENSION a_Tables(n_Tables, 2)
                               a_Tables(n_Tables, 1) = LEFT(c_Text_line,  ;
                                       (n_First_space-1))
                               a_Tables(n_Tables, 2) = SUBSTR(c_Text_line,  ;
                                       (n_First_space+1))
                               IF l_Proper=.T.
                                    a_Tables(n_Tables, 1) =  ;
                                     LEFT(a_Tables(n_Tables,1), 1)+ ;
                                     LOWER(SUBSTR(a_Tables(n_Tables,1), 2))
                                    a_Tables(n_Tables, 2) =  ;
                                     LEFT(a_Tables(n_Tables,2), 1)+ ;
                                     LOWER(SUBSTR(a_Tables(n_Tables,2), 2))
                               ENDIF
                          ENDIF
                     ENDIF
                ENDDO
                = FCLOSE(n_Handle)
                = ASORT(a_Tables, 1)
           ELSE
                n_Tables = -2
           ENDIF
           DELETE FILE (c_Temp_file)
      ELSE
           n_Tables = -3
      ENDIF
 ELSE
      n_Tables = -1
 ENDIF
 RETURN (n_Tables)
ENDFUNC
*
