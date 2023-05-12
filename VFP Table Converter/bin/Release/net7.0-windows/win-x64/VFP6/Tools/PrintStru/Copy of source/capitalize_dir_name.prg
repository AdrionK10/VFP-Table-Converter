***
*** capitalize_dir_name.fxp
***
*
*
FUNCTION CAPITALIZE_DIR_NAME
 LPARAMETERS c_Dir_name, l_Strict
 LOCAL c_Massaged_name, c_Char, c_First_char
 LOCAL l_Space, l_Backslash, l_At_start, l_Capitalize
 LOCAL prEv_exact
 LOCAL i
 IF TYPE("C_DIR_NAME")<>"C" .OR. EMPTY(c_Dir_name)
      c_Massaged_name = ""
 ELSE
      c_Dir_name = LOWER(c_Dir_name)
      l_Strict = IIF((TYPE("L_STRICT")="L"), l_Strict, .F.)
      IF l_Strict=.T.
           c_Massaged_name = UPPER(LEFT(c_Dir_name, 1))
           c_First_char = c_Massaged_name
           l_Space = IIF((c_First_char==" "), .T., .F.)
           l_Backslash = IIF((c_First_char=="\"), .T., .F.)
           l_At_start = .T.
           FOR i = 2 TO LEN(c_Dir_name)
                c_Char = SUBSTR(c_Dir_name, i, 1)
                IF c_Char=="\"
                     l_Backslash = .T.
                     c_Massaged_name = c_Massaged_name+c_Char
                     l_At_start = .F.
                ELSE
                     IF c_Char==" "
                          l_Space = .T.
                          c_Massaged_name = c_Massaged_name+c_Char
                     ELSE
                          IF (l_Backslash=.T. .OR. l_Space=.T.)
                               IF l_Backslash=l_Space
                                    c_Massaged_name = c_Massaged_name+c_Char
                               ELSE
                                    IF (l_Space=.T. .AND. c_First_char== ;
                                       " " .AND. l_At_start=.T.)
                                         c_Massaged_name =  ;
                                          c_Massaged_name+c_Char
                                    ELSE
                                         c_Massaged_name =  ;
                                          c_Massaged_name+UPPER(c_Char)
                                    ENDIF
                               ENDIF
                               l_Backslash = .F.
                               l_Space = .F.
                               l_At_start = .F.
                          ELSE
                               c_Massaged_name = c_Massaged_name+c_Char
                          ENDIF
                     ENDIF
                ENDIF
           ENDFOR
      ELSE
           l_Capitalize = .T.
           c_Massaged_name = ""
           prEv_exact = SET("EXACT")
           SET EXACT ON
           FOR i = 1 TO LEN(c_Dir_name)
                c_Char = SUBSTR(c_Dir_name, i, 1)
                IF INLIST(c_Char, "\", "-", " ", "_", ":")
                     c_Massaged_name = c_Massaged_name+c_Char
                     l_Capitalize = .T.
                ELSE
                     IF l_Capitalize=.T.
                          c_Massaged_name = c_Massaged_name+UPPER(c_Char)
                          l_Capitalize = .F.
                     ELSE
                          c_Massaged_name = c_Massaged_name+c_Char
                     ENDIF
                ENDIF
           ENDFOR
           SET EXACT  &PREV_EXACT
      ENDIF
 ENDIF
 RETURN (c_Massaged_name)
ENDFUNC
*
