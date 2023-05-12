*
FUNCTION ADIR_NEW
 PARAMETER c_Array, c_Tmplates, c_F_types, n_Sortordr, l_Showpath, l_Debug
 PRIVATE c_Array, c_Tmplates, c_F_types, c_Template, c_Error
 PRIVATE c_Path
 PRIVATE n_Params, n_Matches, n_Sortordr, n_Error, n_Tmplates
 PRIVATE n_Start, n_End, n_Count
 PRIVATE l_Debug, l_Showpath
 PRIVATE a_Temp, a_Temp_2, a_Temp_3
 PRIVATE i
 n_Params = PARAMETERS()
 n_Error = 0
 c_Error = ""
 l_Debug = IIF((n_Params>5 .AND. TYPE("L_DEBUG")="L"), l_Debug, .F.)
 IF n_Params>1
      IF (isArray(c_Array) .AND. TYPE("C_TMPLATES")="C" .AND. NOT  ;
         EMPTY(c_Tmplates))
           c_Tmplates = CHR(0)+UPPER(c_Tmplates)
           c_F_types = IIF((TYPE("C_F_TYPES")<>"C"), "",  ;
                       UPPER(ALLTRIM(c_F_types)))
           n_Sortordr = IIF((is_intgr(n_Sortordr) .AND.  ;
                        BETWEEN(n_Sortordr, -6, 6)), n_Sortordr, 0)
           l_Showpath = IIF((n_Params>4 .AND. TYPE("L_SHOWPATH")="L"),  ;
                        l_Showpath, .F.)
           n_Tmplates = OCCURS(CHR(0), c_Tmplates)
           c_Tmplates = c_Tmplates+CHR(0)
           n_Matches = 0
           a_Temp_3 = 0
           FOR i = 1 TO n_Tmplates
                n_Start = AT(CHR(0), c_Tmplates, i)+1
                n_End = AT(CHR(0), c_Tmplates, (i+1))
                c_Template = SUBSTR(c_Tmplates, n_Start, (n_End-n_Start))
                c_Path = fiLename(c_Template,"P")
                IF EMPTY(c_Path)
                     c_Path = cuRr_dir(.T.)
                ELSE
                     c_Path = exIsts(c_Path,"",.T.)
                     IF NOT EMPTY(c_Path)
                          c_Path = SUBSTR(c_Path, 47)
                          c_Path = IIF((RIGHT(c_Path, 1)=="\"), c_Path,  ;
                                   (c_Path+"\"))
                     ENDIF
                ENDIF
                IF (i=1 .OR. n_Matches=0)
                     n_Matches = ADIR(a_Temp, c_Template, c_F_types)
                     DO __Adirnew3
                ELSE
                     n_Count = ADIR(a_Temp_2, c_Template, c_F_types)
                     IF n_Count>0
                          DO __Adirnew4
                          n_Matches = n_Matches+n_Count
                          DO a_Tag WITH c_Error, a_Temp, a_Temp_2, a_Temp_3
                          DIMENSION a_Temp(ALEN(a_Temp_3, 1), 5)
                          = ACOPY(a_Temp_3, a_Temp)
                     ENDIF
                ENDIF
           ENDFOR
           IF n_Matches>0
                IF NOT INLIST(n_Sortordr, -6, 6)
                     IF n_Tmplates=1
                          IF INLIST(n_Sortordr, 0, 1)
                               = ASORT(a_Temp)
                          ELSE
                               DO __Adirnew
                          ENDIF
                     ELSE
                          IF INLIST(n_Sortordr, 0, 1)
                               DO a_Unique WITH c_Error, a_Temp, 1, "ASCENDING"
                          ELSE
                               DO a_Unique WITH c_Error, a_Temp, 1, "ASCENDING"
                               DO __Adirnew
                          ENDIF
                          n_Matches = ALEN(a_Temp, 1)
                     ENDIF
                ELSE
                     n_Matches = n_Matches-a_Clean(@a_Temp,1)
                ENDIF
                DIMENSION  &C_ARRAY(ALEN(A_TEMP, 1), 5)
                = ACOPY (A_TEMP, &C_ARRAY)
           ENDIF
      ELSE
           n_Error = -2
      ENDIF
 ELSE
      n_Error = -1
 ENDIF
 IF n_Error<>0
      n_Matches = IIF((l_Debug=.T.), n_Error, 0)
 ENDIF
 RETURN (n_Matches)
ENDFUNC
*
PROCEDURE __ADIRNEW
 PRIVATE n_Start, n_Penultim, n_Target
 PRIVATE a_Misc
 PRIVATE j, k
 DIMENSION a_Misc(5)
 n_Start = 0
 n_Target = 0
 = ASORT(a_Temp, ABS(n_Sortordr), -1, IIF((SIGN(n_Sortordr)>0), 0, 1))
 IF ABS(n_Sortordr)=3
      IF SIGN(n_Sortordr)>0
           n_Penultim = ALEN(a_Temp, 1)-1
           FOR j = 1 TO n_Penultim
                IF a_Temp(j,3)=a_Temp(j+1,3)
                     FOR k = (j+1) TO (n_Penultim+1)
                          IF a_Temp(j,3)=a_Temp(k,3)
                               IF a_Temp(j,4)>a_Temp(k,4)
                                    DO __Adirnew2
                               ENDIF
                          ELSE
                               EXIT
                          ENDIF
                     ENDFOR
                ENDIF
           ENDFOR
      ELSE
           n_Penultim = ALEN(a_Temp, 1)-1
           FOR j = 1 TO n_Penultim
                IF a_Temp(j,3)=a_Temp(j+1,3)
                     FOR k = (j+1) TO (n_Penultim+1)
                          IF a_Temp(j,3)=a_Temp(k,3)
                               IF a_Temp(j,4)<a_Temp(k,4)
                                    DO __Adirnew2
                               ENDIF
                          ELSE
                               EXIT
                          ENDIF
                     ENDFOR
                ENDIF
           ENDFOR
      ENDIF
 ENDIF
 RETURN
ENDPROC
*
PROCEDURE __ADIRNEW2
 n_Start = AELEMENT(a_Temp, j, 1)
 n_Target = AELEMENT(a_Temp, k, 1)
 = ACOPY(a_Temp, a_Misc, n_Start, 5)
 a_Temp(j, 1) = a_Temp(k,1)
 a_Temp(j, 2) = a_Temp(k,2)
 a_Temp(j, 3) = a_Temp(k,3)
 a_Temp(j, 4) = a_Temp(k,4)
 a_Temp(j, 5) = a_Temp(k,5)
 = ACOPY(a_Misc, a_Temp, 1, 5, n_Target)
 RETURN
ENDPROC
*
PROCEDURE __ADIRNEW3
 PRIVATE i
 IF (n_Matches>0 .AND. l_Showpath=.T. .AND. NOT EMPTY(c_Path))
      FOR i = 1 TO n_Matches
           a_Temp(i, 1) = c_Path+a_Temp(i,1)
      ENDFOR
 ENDIF
 RETURN
ENDPROC
*
PROCEDURE __ADIRNEW4
 PRIVATE i
 IF (n_Count>0 .AND. l_Showpath=.T. .AND. NOT EMPTY(c_Path))
      FOR i = 1 TO n_Count
           a_Temp_2(i, 1) = c_Path+a_Temp_2(i,1)
      ENDFOR
 ENDIF
 RETURN
ENDPROC
*
