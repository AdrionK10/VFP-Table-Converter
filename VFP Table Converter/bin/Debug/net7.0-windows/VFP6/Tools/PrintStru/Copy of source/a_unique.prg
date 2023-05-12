*
PROCEDURE A_UNIQUE
 PARAMETER c_Error, a_Array, n_Sort_col, c_Srt_ordr, a_Total_up, l_Xtra_col
 PRIVATE n_Elements, n_Columns, n_Rows, n_Cells, n_Params
 PRIVATE n_Fields, n_Sortcol, n_Srt_ordr, n_Temp
 PRIVATE c_Order
 PRIVATE a_Temp, a_Totals, a_New_col
 PRIVATE l_All_flds, l_New_col
 PRIVATE x_Temp
 PRIVATE prEv_error, prEv_exact
 PRIVATE i, i2
 n_Params = PARAMETERS()
 c_Error = ""
 l_All_flds = .F.
 prEv_error = ON("ERROR")
 prEv_exact = SET("EXACT")
 ON ERROR C_ERROR  =  STR(ERROR(), 7) + "  " + MESSAGE()
 SET EXACT ON
 IF isArray("A_ARRAY")
      n_Elements = ALEN(a_Array, 0)
      n_Rows = ALEN(a_Array, 1)
      n_Columns = ALEN(a_Array, 2)
      n_Sortcol = IIF((TYPE("N_SORT_COL")<>"N"), 0, n_Sort_col)
      c_Order = IIF((TYPE("C_SRT_ORDR")<>"C"), "ASCENDING",  ;
                UPPER(ALLTRIM(c_Srt_ordr)))
      c_Order = IIF(INLIST(c_Order, "ASCENDING", "DESCENDING"), c_Order,  ;
                "ASCENDING")
      n_Srt_ordr = IIF((c_Order=="ASCENDING"), 0, 1)
      l_New_col = IIF((n_Params>5), IIF((TYPE("L_XTRA_COL")<>"L"), .F.,  ;
                  l_Xtra_col), .F.)
      IF isArray("A_TOTAL_UP")
           n_Fields = ALEN(a_Total_up, 0)
           FOR i2 = 1 TO n_Fields
                x_Temp = a_Total_up(i2)
                IF TYPE("X_TEMP")<>"N"
                     n_Fields = 0
                     EXIT
                ELSE
                     IF x_Temp=-1
                          l_All_flds = .T.
                          n_Fields = 1
                     ELSE
                          IF ((n_Columns=0 .AND. x_Temp=0) .OR.  ;
                             (n_Columns<>0 .AND. (NOT BETWEEN(x_Temp, 1,  ;
                             n_Columns))) .OR. (x_Temp<>INT(x_Temp)) .OR.  ;
                             (x_Temp=n_Sortcol))
                               n_Fields = 0
                               EXIT
                          ENDIF
                     ENDIF
                ENDIF
           ENDFOR
      ELSE
           n_Fields = 0
      ENDIF
      IF n_Elements=n_Rows
           DIMENSION a_Temp(n_Rows)
           = ASORT(a_Array, 1, -1, n_Srt_ordr)
           IF n_Fields>0
                DIMENSION a_Totals(n_Rows)
                a_Totals = 0
           ENDIF
           n_Cells = 0
           n_Temp = 0
           IF n_Fields>0
                FOR i = 1 TO (n_Rows-1)
                     IF NOT (a_Array(i+1)==a_Array(i))
                          n_Cells = n_Cells+1
                          n_Temp = n_Temp+1
                          a_Temp(n_Cells) = a_Array(i)
                          a_Totals(n_Cells) = n_Temp
                          n_Temp = 0
                     ELSE
                          n_Temp = n_Temp+1
                     ENDIF
                ENDFOR
           ELSE
                FOR i = 1 TO (n_Rows-1)
                     IF NOT (a_Array(i+1)==a_Array(i))
                          n_Cells = n_Cells+1
                          a_Temp(n_Cells) = a_Array(i)
                     ENDIF
                ENDFOR
           ENDIF
           IF (n_Cells=0 .OR. (NOT (a_Array(n_Rows)==a_Temp(n_Cells))))
                n_Cells = n_Cells+1
                a_Temp(n_Cells) = a_Array(n_Rows)
                IF n_Fields>0
                     n_Temp = n_Temp+1
                     a_Totals(n_Cells) = n_Temp
                ENDIF
           ENDIF
           IF EMPTY(c_Error)
                IF n_Fields>0
                     DIMENSION a_Array(n_Cells, 2)
                     FOR i = 1 TO n_Cells
                          a_Array(i, 1) = a_Temp(i)
                          a_Array(i, 2) = a_Totals(i)
                     ENDFOR
                ELSE
                     DIMENSION a_Temp(n_Cells)
                     = ACOPY(a_Temp, a_Array)
                     DIMENSION a_Array(n_Cells)
                ENDIF
           ENDIF
      ELSE
           IF (n_Sortcol=INT(n_Sortcol) .AND. BETWEEN(n_Sortcol, 1, n_Columns))
                DIMENSION a_Temp(n_Rows, n_Columns)
                = ASORT(a_Array, n_Sortcol, -1, n_Srt_ordr)
                IF n_Fields>0
                     IF l_All_flds=.T.
                          DIMENSION a_Total_up(n_Columns-1)
                          n_Fields = n_Columns-1
                          n_Temp = 0
                          FOR i = 1 TO n_Columns
                               IF i<>n_Sortcol
                                    n_Temp = n_Temp+1
                                    a_Total_up(n_Temp) = i
                               ENDIF
                          ENDFOR
                     ENDIF
                     DIMENSION a_Totals(n_Fields)
                     a_Totals = 0
                ENDIF
                IF l_New_col=.T.
                     DIMENSION a_New_col(n_Rows)
                ENDIF
                n_Cells = 0
                n_Temp = 0
                IF n_Fields>0
                     FOR i = 1 TO (n_Rows-1)
                          IF NOT (a_Array((i+1),n_Sortcol)==a_Array(i, ;
                             n_Sortcol))
                               FOR i2 = 1 TO n_Fields
                                    a_Totals(i2) = a_Totals(i2)+a_Array(i, ;
                                     a_Total_up(i2))
                                    a_Array(i, a_Total_up(i2)) = a_Totals(i2)
                               ENDFOR
                               n_Cells = n_Cells+1
                               = ACOPY(a_Array, a_Temp, AELEMENT(a_Array,  ;
                                 i, 1), n_Columns, AELEMENT(a_Temp,  ;
                                 n_Cells, 1))
                               a_Totals = 0
                               IF l_New_col=.T.
                                    n_Temp = n_Temp+1
                                    a_New_col(n_Cells) = n_Temp
                                    n_Temp = 0
                               ENDIF
                          ELSE
                               n_Temp = n_Temp+1
                               FOR i2 = 1 TO n_Fields
                                    a_Totals(i2) = a_Totals(i2)+a_Array(i, ;
                                     a_Total_up(i2))
                               ENDFOR
                          ENDIF
                     ENDFOR
                ELSE
                     FOR i = 1 TO (n_Rows-1)
                          IF NOT (a_Array((i+1),n_Sortcol)==a_Array(i, ;
                             n_Sortcol))
                               n_Cells = n_Cells+1
                               = ACOPY(a_Array, a_Temp, AELEMENT(a_Array,  ;
                                 i, 1), n_Columns, AELEMENT(a_Temp,  ;
                                 n_Cells, 1))
                               IF l_New_col=.T.
                                    n_Temp = n_Temp+1
                                    a_New_col(n_Cells) = n_Temp
                                    n_Temp = 0
                               ENDIF
                          ELSE
                               n_Temp = n_Temp+1
                          ENDIF
                     ENDFOR
                ENDIF
                IF (n_Cells=0 .OR. NOT (a_Array(n_Rows,n_Sortcol)== ;
                   a_Temp(n_Cells,n_Sortcol)))
                     FOR i2 = 1 TO n_Fields
                          a_Totals(i2) = a_Totals(i2)+a_Array(n_Rows, ;
                                  a_Total_up(i2))
                          a_Array(n_Rows, a_Total_up(i2)) = a_Totals(i2)
                     ENDFOR
                     n_Cells = n_Cells+1
                     = ACOPY(a_Array, a_Temp, AELEMENT(a_Array, n_Rows,  ;
                       1), n_Columns, AELEMENT(a_Temp, n_Cells, 1))
                     IF l_New_col=.T.
                          n_Temp = n_Temp+1
                          a_New_col(n_Cells) = n_Temp
                     ENDIF
                ENDIF
                IF EMPTY(c_Error)
                     IF l_New_col=.T.
                          DIMENSION a_Temp(n_Cells, n_Columns)
                          DIMENSION a_Array(n_Cells, (n_Columns+1))
                          FOR i = 1 TO n_Cells
                               = ACOPY(a_Temp, a_Array, AELEMENT(a_Temp,  ;
                                 i, 1), n_Columns, AELEMENT(a_Array, i, 1))
                               a_Array(i, n_Columns+1) = a_New_col(i)
                          ENDFOR
                     ELSE
                          DIMENSION a_Temp(n_Cells, n_Columns)
                          = ACOPY(a_Temp, a_Array)
                          DIMENSION a_Array(n_Cells, n_Columns)
                     ENDIF
                ENDIF
           ELSE
                IF n_Sortcol<>INT(n_Sortcol)
                     c_Error = STR(-3, 7)+"  Sort column specifier, "+ ;
                               to_char(n_Sort_col)+", is not an "+ ;
                               "integer number."
                ELSE
                     c_Error = STR(-4, 7)+"  Sort column specifier, "+ ;
                               to_char(n_Sort_col)+", is not a valid "+ ;
                               "column specifier for the supplied array."
                ENDIF
           ENDIF
      ENDIF
 ELSE
      IF n_Params<2
           c_Error = STR(-2, 7)+"  Missing array argument."
      ELSE
           c_Error = STR(-1, 7)+"  Variable to sort is not of type  ARRAY."
      ENDIF
 ENDIF
 ON ERROR   &PREV_ERROR
 SET EXACT  &PREV_EXACT
 RETURN
ENDPROC
*
