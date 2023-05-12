*
FUNCTION A_SEARCH
 PARAMETER c_Arraynam, x_Value, c_Position, n_Position, n_Occurnce, l_Exact
 PRIVATE c_Position, c_Arraynam
 PRIVATE n_Column, n_Params, n_Position, n_Columns, n_Rows
 PRIVATE n_Element, n_Occurnce, n_Axis, n_Othraxis, n_Row_col
 PRIVATE l_Rows, l_Exact
 PRIVATE x_Value
 PRIVATE prEv_exact
 PRIVATE i
 n_Params = PARAMETERS()
 n_Row_col = 0
 prEv_exact = SET("EXACT")
 SET EXACT ON
 IF n_Params>=4
      IF isArray(c_Arraynam)
           N_ROWS		=  ALEN(&C_ARRAYNAM, 1)
           N_COLUMNS	=  ALEN(&C_ARRAYNAM, 2)
           n_Columns = IIF((n_Columns=0), 1, n_Columns)
           c_Position = IIF((TYPE("C_POSITION")<>"C"), "",  ;
                        UPPER(ALLTRIM(c_Position)))
           c_Position = IIF(INLIST(c_Position, "ROW", "COLUMN"),  ;
                        c_Position, "")
           n_Position = IIF((TYPE("N_POSITION")<>"N"), 0, n_Position)
           IF NOT EMPTY(c_Position)
                IF n_Position=INT(n_Position)
                     l_Rows = IIF((c_Position=="ROW"), .T., .F.)
                     IF ((l_Rows=.T. .AND. BETWEEN(n_Position, 1,  ;
                        n_Rows)) .OR. (l_Rows=.F. .AND.  ;
                        BETWEEN(n_Position, 1, n_Columns)))
                          n_Occurnce = IIF((TYPE("N_OCCURNCE")<>"N"), 1,  ;
                                       n_Occurnce)
                          n_Occurnce = IIF((n_Occurnce<>INT(n_Occurnce)),  ;
                                       1, n_Occurnce)
                          n_Element = 0
                          n_Axis = IIF((l_Rows=.T.), 1, 2)
                          n_Othraxis = IIF((l_Rows=.T.), 2, 1)
                          l_Exact = IIF((n_Params>=6 .AND.  ;
                                    TYPE("L_EXACT")="L"), l_Exact, .T.)
                          IF l_Exact=.F.
                               SET EXACT OFF
                          ENDIF
                          FOR i = 1 TO n_Occurnce
                               n_Row_col = 0
                               n_Element = n_Element+1
                               DO WHILE .T.
                                    N_ELEMENT  =  ASCAN(&C_ARRAYNAM, X_VALUE, N_ELEMENT)
                                    IF n_Element=0
                                         i = n_Occurnce+1
                                         EXIT
                                    ELSE
                                         DO CASE
                                              CASE (l_Rows=.T. .AND.  ;
                                               n_Columns=1)
                                                   n_Row_col = n_Element
                                              CASE (l_Rows=.F. .AND.  ;
                                               n_Columns=1)
                                                   n_Row_col = 1
                                              OTHERWISE
                                                   N_ROW_COL  =  ASUBSCRIPT(&C_ARRAYNAM, N_ELEMENT,  N_AXIS)
                                         ENDCASE
                                         IF n_Row_col=n_Position
                                              DO CASE
                                                   CASE (l_Rows=.T. .AND.  ;
                                                    n_Columns=1)
                                                        n_Row_col = 1
                                                   CASE (l_Rows=.F. .AND.  ;
                                                    n_Columns=1)
                                                        n_Row_col = n_Element
                                                   OTHERWISE
                                                        N_ROW_COL  =  ASUBSCRIPT(&C_ARRAYNAM,  N_ELEMENT, N_OTHRAXIS)
                                              ENDCASE
                                              EXIT
                                         ELSE
                                              IF (l_Rows=.T. .AND.  ;
                                               n_Row_col>n_Position)
                                                   n_Row_col = 0
                                                   i = n_Occurnce+1
                                                   EXIT
                                              ELSE
                                                   n_Row_col = 0
                                                   n_Element = n_Element+1
                                              ENDIF
                                         ENDIF
                                    ENDIF
                               ENDDO
                          ENDFOR
                     ELSE
                          n_Row_col = -5
                     ENDIF
                ELSE
                     n_Row_col = -4
                ENDIF
           ELSE
                n_Row_col = -3
           ENDIF
      ELSE
           n_Row_col = -2
      ENDIF
 ELSE
      n_Row_col = -1
 ENDIF
 SET EXACT  &PREV_EXACT
 RETURN (n_Row_col)
ENDFUNC
*
