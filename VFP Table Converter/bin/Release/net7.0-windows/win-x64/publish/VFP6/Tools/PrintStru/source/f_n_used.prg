*
FUNCTION F_N_USED
 PARAMETER c_Pathname, c_Temp_dir
 PRIVATE c_Pathname, c_Details, c_Handle, c_Fullname, c_Temp_dir
 PRIVATE c_Chars_in, c_Temp, c_Temp_2
 PRIVATE n_Params, n_Handle, n_Hand_pos, n_Pos_pos, n_Read_pos
 PRIVATE n_Writepos
 PRIVATE l_Proceed
 PRIVATE prEv_safty, prEv_error
 n_Params = PARAMETERS()
 IF n_Params>0
      l_Proceed = .T.
      c_Details = exIsts(c_Pathname,"",.T.)
      IF NOT EMPTY(c_Details)
           c_Fullname = SUBSTR(c_Details, 47)
           c_Temp_dir = IIF((n_Params>1 .AND. TYPE("C_TEMP_DIR")="C"  ;
                        .AND. NOT EMPTY(c_Temp_dir)), ALLTRIM(c_Temp_dir),  ;
                        cuRr_dir(.T.))
           c_Temp_dir = IIF((RIGHT(c_Temp_dir, 1)=="\"), c_Temp_dir,  ;
                        (c_Temp_dir+"\"))
           prEv_safty = SET("SAFETY")
           prEv_error = ON("ERROR")
           SET SAFETY OFF
           ON ERROR L_PROCEED = .F.
           c_Tempfile = SYS(3)
           c_Tempfile = "FUSED"+LEFT(c_Tempfile, 3)+"."+SUBSTR(c_Tempfile,  ;
                        4, 3)
           c_Tempfile = c_Temp_dir+c_Tempfile
           LIST STATUS TO FILE (c_Tempfile) NOCONSOLE
           SET SAFETY	&PREV_SAFTY
           ON ERROR	&PREV_ERROR
           IF l_Proceed=.T.
                c_Handle = "  0"
                n_Handle = FOPEN(c_Tempfile, 0)
                IF n_Handle>-1
                     DO WHILE NOT FEOF(n_Handle)
                          c_Chars_in = UPPER(FGETS(n_Handle, 62000))
                          IF INLIST(FERROR(), 0, 31)
                               IF "HANDLE="$c_Chars_in
                                    c_Chars_in = reDuce(c_Chars_in)
                                    c_This_one = UPPER(LEFT(c_Chars_in,  ;
                                     (AT(" ", c_Chars_in)-1)))
                                    IF c_This_one=c_Fullname
                                         n_Hand_pos = AT("HANDLE=", c_Chars_in)
                                         n_Pos_pos = AT("POS=", c_Chars_in)
                                         n_Read_pos = AT("READ=", c_Chars_in)
                                         n_Writepos = AT("WRITE=", c_Chars_in)
                                         c_Handle = SUBSTR(c_Chars_in,  ;
                                          (n_Hand_pos+7), (n_Pos_pos- ;
                                          n_Hand_pos-7))
                                         c_Handle = PADL(RTRIM(c_Handle),  ;
                                          3)+"  "
                                         c_Temp = SUBSTR(c_Chars_in,  ;
                                          (n_Pos_pos+4), (n_Read_pos- ;
                                          n_Pos_pos-4))
                                         c_Handle = c_Handle+ ;
                                          PADL(RTRIM(c_Temp), 10)+"  "
                                         c_Temp = SUBSTR(c_Chars_in,  ;
                                          (n_Read_pos+5), (n_Writepos- ;
                                          n_Read_pos-5))
                                         c_Temp_2 = SUBSTR(c_Chars_in,  ;
                                          (n_Writepos+6), 3)
                                         c_Temp = UPPER(ALLTRIM(c_Temp))
                                         c_Temp_2 = UPPER(ALLTRIM(c_Temp_2))
                                         IF c_Temp=="YES" .AND. c_Temp_2=="YES"
                                              c_Handle = c_Handle+"R/W  "
                                         ELSE
                                              IF c_Temp=="YES"
                                                   c_Handle = c_Handle+"READ "
                                              ELSE
                                                   c_Handle = c_Handle+"WRITE"
                                              ENDIF
                                         ENDIF
                                         c_Handle = c_Handle+"  "+c_Fullname
                                         EXIT
                                    ENDIF
                               ENDIF
                          ELSE
                               IF NOT FEOF(n_Handle)
                                    c_Handle =  ;
                                     " -5  Network/Disk failure while processing "+ ;
                                     "temporary file  "+CHR(34)+ ;
                                     c_Tempfile+CHR(34)+"."
                               ENDIF
                               EXIT
                          ENDIF
                     ENDDO
                     = FCLOSE(n_Handle)
                     DELETE FILE (c_Tempfile)
                ELSE
                     c_Handle = " -4  Error opening temporary file  "+ ;
                                CHR(34)+c_Tempfile+CHR(34)+"  / "+STR(FERROR())
                ENDIF
           ELSE
                c_Handle = " -3  Error creating  "+CHR(34)+c_Tempfile+ ;
                           CHR(34)+"  temporary file."
           ENDIF
      ELSE
           c_Handle = " -2  Filename parameter is not of type CHARACTER, or pathname "+ ;
                      "supplied does not exist."
      ENDIF
 ELSE
      c_Handle = " -1  No filename was specified."
 ENDIF
 RETURN (c_Handle)
ENDFUNC
*
