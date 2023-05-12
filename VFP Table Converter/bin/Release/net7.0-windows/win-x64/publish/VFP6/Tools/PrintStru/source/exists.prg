*
FUNCTION EXISTS
 PARAMETER c_Filename, c_Use_path, l_Details
 PRIVATE c_Filename, c_Temp, c_Use_path, c_Origname, c_Dir_name
 PRIVATE l_Details, l_Proceed, l_Result, l_Goodpath
 PRIVATE n_Matches
 PRIVATE a_Files
 PRIVATE prEv_centu, prEv_exact, prEv_path, prEv_date, prEv_compa
 PRIVATE prEv_defau, prEv_error
 prEv_exact = SET("EXACT")
 prEv_path = SET("PATH")
 prEv_compa = SET("COMPATIBLE")
 SET EXACT ON
 SET COMPATIBLE FOXPLUS
 c_Filename = IIF((TYPE("C_FILENAME")<>"C"), "", UPPER(ALLTRIM(c_Filename)))
 c_Origname = c_Filename
 c_Filename = IIF((RIGHT(c_Filename, 1)=="\"), LEFT(c_Filename,  ;
              (LEN(c_Filename)-1)), c_Filename)
 c_Use_path = IIF((TYPE("C_USE_PATH")<>"C"), "",  ;
              IIF(INLIST(UPPER(c_Use_path), "F", "D", "FD", "DF"),  ;
              UPPER(c_Use_path), ""))
 l_Details = IIF((TYPE("L_DETAILS")<>"L"), .F., l_Details)
 l_Proceed = IIF(EMPTY(c_Origname), .F., .T.)
 IF l_Proceed=.T.
      c_Dir_name = goTo_dir(c_Filename,.F.)
      DO CASE
           CASE NOT EMPTY(c_Dir_name)
                IF l_Details=.F.
                     l_Result = .T.
                ELSE
                     IF RIGHT(c_Dir_name, 2)==":\"
                          l_Result = "D  ...."+SPACE(14)+"0"+SPACE(24)+ ;
                                     c_Dir_name
                     ELSE
                          n_Matches = ADIR(a_Files, c_Dir_name, "DHS")
                          l_Result = _lSm_exis2()+c_Dir_name
                     ENDIF
                ENDIF
           CASE l_Details=.F.
                l_Result = IIF((ADIR(a_Files, c_Filename, "DHS")>0), .T., .F.)
                IF (l_Result=.F. .AND. NOT EMPTY(c_Use_path))
                     c_Temp = fiLename(c_Filename,"F")
                     IF c_Temp==c_Filename
                          DO _lSm_exists
                          l_Result = FILE(c_Filename)
                          SET PATH TO (prEv_path)
                     ENDIF
                ENDIF
           CASE l_Details=.T.
                n_Matches = ADIR(a_Files, c_Filename, "DHS")
                IF (n_Matches=0 .AND. NOT EMPTY(c_Use_path))
                     c_Temp = fiLename(c_Filename,"F")
                     IF c_Temp==c_Filename
                          DO _lSm_exists
                          IF FILE(c_Filename)=.T.
                               c_Filename = FULLPATH(c_Filename)
                               n_Matches = ADIR(a_Files, c_Filename, "DHS")
                          ENDIF
                          SET PATH TO (prEv_path)
                     ENDIF
                ELSE
                     IF n_Matches>0
                          c_Filename = FULLPATH(c_Filename)
                     ENDIF
                ENDIF
                IF n_Matches=0
                     l_Result = ""
                ELSE
                     l_Result = _lSm_exis2()+c_Filename
                ENDIF
      ENDCASE
 ELSE
      l_Result = IIF((l_Details=.F.), .F., "")
 ENDIF
 SET EXACT		&PREV_EXACT
 SET COMPATIBLE  &PREV_COMPA
 RETURN (l_Result)
ENDFUNC
*
PROCEDURE _LSM_EXISTS
 PRIVATE c_Temp
 DO CASE
      CASE c_Use_path=="D"
           SET PATH TO (GETENV("PATH"))
      CASE c_Use_path=="FD"
           c_Temp = IIF((RIGHT(prEv_path, 1)$";,"), "", ";")
           SET PATH TO (prEv_path+c_Temp+GETENV("PATH"))
      CASE c_Use_path=="DF"
           c_Temp = IIF((RIGHT(GETENV("PATH"), 1)$";,"), "", ";")
           SET PATH TO (GETENV("PATH")+c_Temp+prEv_path)
 ENDCASE
 RETURN
ENDPROC
*
FUNCTION _LSM_EXIS2
 PRIVATE prEv_centu, prEv_date
 prEv_centu = SET("CENTURY")
 prEv_date = SET("DATE")
 SET CENTURY ON
 SET DATE TO AMERICAN
 l_Result = IIF((RIGHT(a_Files(1,5), 1)="D"), "D", "F")+"  "
 l_Result = l_Result+LEFT(a_Files(1,5), 4)+"  "+STR(a_Files(1,2), 13)+ ;
            "  "+DTOC(a_Files(1,3))+"  "+a_Files(1,4)+"  "
 SET DATE TO (prEv_date)
 SET CENTURY  &PREV_CENTU
 RETURN (l_Result)
ENDFUNC
*
FUNCTION _LSM_EXIS3
 PARAMETER c_Temp_dir
 PRIVATE c_Temp_dir
 PRIVATE l_Goodpath
 PRIVATE prEv_defaul, prEv_error
 l_Goodpath = .T.
 prEv_defau = cuRr_dir(.T.)
 prEv_error = ON("ERROR")
 ON ERROR L_GOODPATH = .F.
 SET DEFAULT TO  &C_TEMP_DIR
 c_Filename = IIF((l_Goodpath=.T.), cuRr_dir(), "")
 ON ERROR	&PREV_ERROR
 SET DEFAULT TO  &PREV_DEFAU
 RETURN (c_Filename)
ENDFUNC
*
