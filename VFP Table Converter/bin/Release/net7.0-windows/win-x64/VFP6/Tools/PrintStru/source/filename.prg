*
FUNCTION FILENAME
 PARAMETER c_Filename, c_Option, l_Bckslash
 PRIVATE c_Filename, c_Option
 PRIVATE n_Slashes, n_Length, n_Colons, n_Params
 PRIVATE l_Bckslash
 PRIVATE prEv_exact
 n_Params = PARAMETERS()
 c_Filename = IIF((TYPE("C_FILENAME")<>"C"), "", UPPER(ALLTRIM(c_Filename)))
 c_Option = IIF((TYPE("C_OPTION")<>"C"), "", UPPER(ALLTRIM(c_Option)))
 l_Bckslash = IIF((n_Params>2 .AND. TYPE("L_BCKSLASH")="L"), l_Bckslash, .F.)
 prEv_exact = SET("EXACT")
 SET EXACT ON
 IF (NOT EMPTY(c_Filename) .AND. (c_Option=="F" .OR. c_Option=="P"))
      n_Length = LEN(c_Filename)
      n_Slashes = RAT("\", c_Filename)
      n_Colons = RAT(":", c_Filename)
      IF (n_Slashes+n_Colons)>0
           IF c_Option=="F"
                c_Filename = RIGHT(c_Filename, (n_Length-MAX(n_Slashes,  ;
                             n_Colons)))
                IF RIGHT(c_Filename, 1)=="."
                     c_Filename = LEFT(c_Filename, (LEN(c_Filename)-1))
                ENDIF
           ELSE
                c_Filename = LEFT(c_Filename, MAX(n_Slashes, n_Colons))
                IF LEN(c_Filename)=2
                     IF l_Bckslash=.T.
                          IF (RIGHT(c_Filename, 1)==":" .AND.  ;
                             ISALPHA(c_Filename))
                               c_Filename = c_Filename+"\"
                          ENDIF
                     ENDIF
                ENDIF
           ENDIF
      ELSE
           IF c_Option=="P"
                c_Filename = IIF(INLIST(c_Filename, "..", "."), c_Filename, "")
           ELSE
                IF RIGHT(c_Filename, 1)=="."
                     c_Filename = IIF(fiLled(c_Filename,"."), "",  ;
                                  LEFT(c_Filename, (LEN(c_Filename)-1)))
                ENDIF
           ENDIF
      ENDIF
 ENDIF
 SET EXACT	&PREV_EXACT
 RETURN (c_Filename)
ENDFUNC
*
