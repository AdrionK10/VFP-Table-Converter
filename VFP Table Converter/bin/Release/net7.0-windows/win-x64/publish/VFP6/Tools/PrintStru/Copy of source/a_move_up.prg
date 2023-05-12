***
*** a_move_up.fxp
***
*
*
FUNCTION A_MOVE_UP
 PARAMETER a_Array, n_Col, l_Empty, x_Discard_value, l_Null_strings
 PRIVATE a_Array, n_Col, x_Discard_value, l_Null_strings
 PRIVATE c_Column_type
 PRIVATE l_Proceed, l_Check_for_empty, l_Empty
 PRIVATE l_Space_fill
 PRIVATE n_Params, n_Columns, n_Non_empty_rows
 PRIVATE n_Empty_back_rows, n_Rows
 PRIVATE n_To_do, n_Consecutive, n_Start_row
 PRIVATE i, j
 EXTERNAL ARRAY a_Array
 n_Params = PARAMETERS()
 l_Proceed = .T.
 n_Non_empty_rows = -1
 IF ((n_Params<3) .OR. NOT isArray("A_ARRAY") .OR. NOT is_intgr(n_Col,0)  ;
    .OR. (TYPE("L_EMPTY")<>"L"))
      l_Proceed = .F.
 ENDIF
 IF l_Proceed=.T.
      n_Columns = ALEN(a_Array, 2)
      n_Columns = IIF((n_Columns=0), 1, n_Columns)
      IF BETWEEN(n_Col, 1, n_Columns)
           n_Rows = ALEN(a_Array, 1)
           l_Check_for_empty = l_Empty
           l_Null_strings = IIF((TYPE("L_NULL_STRINGS")="L"),  ;
                            l_Null_strings, .F.)
           l_Space_fill = NOT l_Null_strings
           c_Column_type = TYPE("X_DISCARD_VALUE")
           n_Empty_back_rows = 0
           IF l_Check_for_empty=.T.
                IF n_Columns=1
                     FOR i = n_Rows TO 1 STEP -1
                          IF EMPTY(a_Array(i))
                               n_Empty_back_rows = n_Empty_back_rows+1
                          ELSE
                               EXIT
                          ENDIF
                     ENDFOR
                ELSE
                     FOR i = n_Rows TO 1 STEP -1
                          IF EMPTY(a_Array(i,n_Col))
                               n_Empty_back_rows = n_Empty_back_rows+1
                          ELSE
                               EXIT
                          ENDIF
                     ENDFOR
                ENDIF
           ELSE
                IF n_Columns=1
                     FOR i = n_Rows TO 1 STEP -1
                          IF NOT (TYPE("A_ARRAY(I)")==c_Column_type)
                               RETURN -2
                          ELSE
                               IF a_Array(i)==x_Discard_value
                                    n_Empty_back_rows = n_Empty_back_rows+1
                               ELSE
                                    EXIT
                               ENDIF
                          ENDIF
                     ENDFOR
                ELSE
                     FOR i = n_Rows TO 1 STEP -1
                          IF NOT (TYPE("A_ARRAY(I, N_COL)")==c_Column_type)
                               RETURN -2
                          ELSE
                               IF a_Array(i,n_Col)==x_Discard_value
                                    n_Empty_back_rows = n_Empty_back_rows+1
                               ELSE
                                    EXIT
                               ENDIF
                          ENDIF
                     ENDFOR
                ENDIF
           ENDIF
           n_To_do = n_Rows-n_Empty_back_rows
           n_Consecutive = 0
           n_Non_empty_rows = 0
           FOR i = 1 TO n_To_do
                IF l_Check_for_empty=.T.
                     IF n_Columns=1
                          IF EMPTY(a_Array(i))
                               n_Start_row = IIF((n_Consecutive=0), i,  ;
                                n_Start_row)
                               n_Consecutive = n_Consecutive+1
                          ELSE
                               DO _lSm_amoveup_copy_one_up
                               n_Non_empty_rows = n_Non_empty_rows+1
                          ENDIF
                     ELSE
                          IF EMPTY(a_Array(i,n_Col))
                               n_Start_row = IIF((n_Consecutive=0), i,  ;
                                n_Start_row)
                               n_Consecutive = n_Consecutive+1
                          ELSE
                               DO _lSm_amoveup_copy_one_up
                               n_Non_empty_rows = n_Non_empty_rows+1
                          ENDIF
                     ENDIF
                ELSE
                     IF n_Columns=1
                          IF NOT (TYPE("A_ARRAY(I)")==c_Column_type)
                               RETURN -2
                          ELSE
                               IF a_Array(i)==x_Discard_value
                                    n_Start_row = IIF((n_Consecutive=0),  ;
                                     i, n_Start_row)
                                    n_Consecutive = n_Consecutive+1
                               ELSE
                                    DO _lSm_amoveup_copy_one_up
                                    n_Non_empty_rows = n_Non_empty_rows+1
                               ENDIF
                          ENDIF
                     ELSE
                          IF NOT (TYPE("A_ARRAY(I, N_COL)")==c_Column_type)
                               RETURN -2
                          ELSE
                               IF a_Array(i,n_Col)==x_Discard_value
                                    n_Start_row = IIF((n_Consecutive=0),  ;
                                     i, n_Start_row)
                                    n_Consecutive = n_Consecutive+1
                               ELSE
                                    DO _lSm_amoveup_copy_one_up
                                    n_Non_empty_rows = n_Non_empty_rows+1
                               ENDIF
                          ENDIF
                     ENDIF
                ENDIF
           ENDFOR
           IF n_Consecutive>0
                IF n_Columns=1
                     FOR i = n_Start_row TO n_Rows
                          a_Array(i) = blAnk_it(a_Array(i),l_Space_fill)
                     ENDFOR
                ELSE
                     FOR i = n_Start_row TO n_Rows
                          FOR j = 1 TO n_Columns
                               a_Array(i, j) = blAnk_it(a_Array(i,j), ;
                                      l_Space_fill)
                          ENDFOR
                     ENDFOR
                ENDIF
           ELSE
                IF n_Empty_back_rows>0
                     IF n_Columns=1
                          FOR i = (n_Rows-n_Empty_back_rows+1) TO n_Rows
                               a_Array(i) = blAnk_it(a_Array(i),l_Space_fill)
                          ENDFOR
                     ELSE
                          FOR i = (n_Rows-n_Empty_back_rows+1) TO n_Rows
                               FOR j = 1 TO n_Columns
                                    a_Array(i, j) = blAnk_it(a_Array(i,j), ;
                                     l_Space_fill)
                               ENDFOR
                          ENDFOR
                     ENDIF
                ENDIF
           ENDIF
      ENDIF
 ENDIF
 RETURN (n_Non_empty_rows)
ENDFUNC
*
PROCEDURE _LSM_AMOVEUP_COPY_ONE_UP
 IF n_Consecutive>0
      IF n_Columns=1
           a_Array(n_Start_row) = a_Array(i)
      ELSE
           FOR j = 1 TO n_Columns
                a_Array(n_Start_row, j) = a_Array(i,j)
           ENDFOR
      ENDIF
      n_Start_row = n_Start_row+1
 ENDIF
 RETURN
ENDPROC
*
